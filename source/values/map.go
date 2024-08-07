package values

import (
	"math/rand"

	"src.elv.sh/pkg/persistent/vector"
)

type Map struct {
	root *mapNode
}

type mapNode struct {
	key         Value
	value       Value
	weight      uint64
	left, right *mapNode
}

func newNode(key, value Value) *mapNode {
	return &mapNode{
		key:    key,
		value:  value,
		weight: rand.Uint64(),
	}
}

func (node *mapNode) shallowClone() *mapNode {
	return &mapNode{
		key:    node.key,
		value:  node.value,
		weight: node.weight,
	}
}

type MapPair struct {
	Key Value
	Val Value
}

// Range calls f sequentially in ascending key order for all entries in the map.
func (pm *Map) Range(f func(key, value Value)) {
	pm.root.forEach(func(k, v Value) {
		f(k, v)
	})
}

func (pm *Map) Len() int {
	return pm.root.len()
}

func (pm *Map) AsVector() vector.Vector {
	return pm.root.getVector()
}

func (node *mapNode) getVector() vector.Vector {
	if node == nil {
		return vector.Empty
	}
	lhs := node.left.getVector()
	el := node.key
	rhs := node.right.getVector()
	new := lhs.Conj(el)
	for i := 0; i < rhs.Len(); i++ {
		v, _ := rhs.Index(i)
		new = new.Conj(v)
	}
	return new
}

func (pm *Map) AsSlice() []MapPair {
	return pm.root.asSlice()
}

func (node *mapNode) asSlice() []MapPair {
	if node == nil {
		return []MapPair{}
	}
	lhs := node.left.asSlice()
	el := MapPair{node.key, node.value}
	rhs := node.right.asSlice()
	lhs = append(lhs, el)
	lhs = append(lhs, rhs...)
	return lhs
}

// It is assumed that we know the map is keyed by strings.
func (pm *Map) ToEnv() (map[string]uint32, []Value) {
	return pm.root.toEnv(make(map[string]uint32), []Value{})
}
func (node *mapNode) toEnv(env map[string]uint32, mem []Value) (map[string]uint32, []Value) {
	if node == nil {
		return env, mem
	}
	env, mem = node.left.toEnv(env, mem)
	k := node.key
	v := node.value
	env[k.V.(string)] = uint32(len(mem))
	mem = append(mem, v)
	env, mem = node.left.toEnv(env, mem)
	return env, mem
}

func (node *mapNode) forEach(f func(key, value Value)) {
	if node == nil {
		return
	}
	node.left.forEach(f)
	f(node.key, node.value)
	node.right.forEach(f)
}

func (node *mapNode) len() int {
	if node == nil {
		return 0
	}
	return node.left.len() + 1 + node.right.len()
}

// Get returns the map value associated with the specified key.
// The ok result indicates whether an entry was found in the map.
func (pm *Map) Get(key Value) (Value, bool) {
	node := pm.root
	for node != nil {
		if key.compare(node.key) {
			node = node.left
		} else if node.key.compare(key) {
			node = node.right
		} else {
			return node.value, true
		}
	}
	var zero Value
	return zero, false
}

// SetAll updates the map with key/value pairs from the other map, overwriting existing keys.
// It is equivalent to calling Set for each entry in the other map but is more efficient.
func (pm *Map) SetAll(other *Map) {
	root := pm.root
	pm.root = union(root, other.root, true)
}

func (pm Map) Set(key, value Value) *Map {
	first := pm.root
	second := newNode(key, value)
	pm.root = union(first, second, true)
	return &pm
}

// union returns a new tree which is a union of first and second one.
// If overwrite is set to true, second one would override a value for any duplicate keys.
//
// union(first:-0, second:-0) (result:+1)
// Union borrows both subtrees without affecting their refcount and returns a
// new reference that the caller is expected to call decref.
func union(first, second *mapNode, overwrite bool) *mapNode {
	if first == nil {
		return second
	}
	if second == nil {
		return first
	}

	if first.weight < second.weight {
		second, first, overwrite = first, second, !overwrite
	}

	left, mid, right := split(second, first.key, false)
	var result *mapNode
	if overwrite && mid != nil {
		result = mid.shallowClone()
	} else {
		result = first.shallowClone()
	}
	result.weight = first.weight
	result.left = union(first.left, left, overwrite)
	result.right = union(first.right, right, overwrite)
	return result
}

// split the tree midway by the key into three different ones.
// Return three new trees: left with all nodes with smaller than key, mid with
// the node matching the key, right with all nodes larger than key.
// If there are no nodes in one of trees, return nil instead of it.
// If requireMid is set (such as during deletion), then all return arguments
// are nil if mid is not found.
//
// split(n:-0) (left:+1, mid:+1, right:+1)
// Split borrows n without affecting its refcount, and returns three
// new references that the caller is expected to call decref.
func split(n *mapNode, key Value, requireMid bool) (left, mid, right *mapNode) {
	if n == nil {
		return nil, nil, nil
	}

	if n.key.compare(key) {
		left, mid, right := split(n.right, key, requireMid)
		if requireMid && mid == nil {
			return nil, nil, nil
		}
		newN := n.shallowClone()
		newN.left = n.left
		newN.right = left
		return newN, mid, right
	} else if key.compare(n.key) {
		left, mid, right := split(n.left, key, requireMid)
		if requireMid && mid == nil {
			return nil, nil, nil
		}
		newN := n.shallowClone()
		newN.left = right
		newN.right = n.right
		return left, mid, newN
	}
	mid = n.shallowClone()
	return n.left, mid, n.right
}

// Delete deletes the value for a key.
func (pm Map) Delete(key Value) *Map {
	root := pm.root
	left, mid, right := split(root, key, true)
	if mid == nil {
		return &pm
	}
	pm.root = merge(left, right)
	return &pm
}

// merge two trees while preserving the weight invariant.
// All nodes in left must have smaller keys than any node in right.
//
// merge(left:-0, right:-0) (result:+1)
// Merge borrows its arguments without affecting their refcount
// and returns a new reference that the caller is expected to call decref.
func merge(left, right *mapNode) *mapNode {
	switch {
	case left == nil:
		return right
	case right == nil:
		return left
	case left.weight > right.weight:
		root := left.shallowClone()
		root.left = left.left
		root.right = merge(left.right, right)
		return root
	default:
		root := right.shallowClone()
		root.left = merge(left, right.left)
		root.right = right.right
		return root
	}
}
