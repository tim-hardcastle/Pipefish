package values

import (
	"math/rand"
)

type Set struct {
	root *setNode
}

type setNode struct {
	element     Value
	weight      uint64
	left, right *setNode
}

func (pm Set) Add(element Value) Set {
	first := pm.root
	second := newSetNode(element)
	pm.root = setUnion(first, second, true)
	return pm
}

// Delete deletes the value for a element.
func (pm Set) Delete(element Value) {
	root := pm.root
	left, mid, right := setSplit(root, element, true)
	if mid == nil {
		return
	}
	pm.root = setMerge(left, right)
}

func (pm Set) Contains(element Value) bool {
	node := pm.root
	for node != nil {
		if element.compare(node.element) {
			node = node.left
		} else if node.element.compare(element) {
			node = node.right
		} else {
			return true
		}
	}
	return false
}

// Range calls f sequentially in ascending element order for all entries in the set.
func (pm Set) Range(f func(element Value)) {
	pm.root.forEach(func(k Value) {
		f(k)
	})
}

func (pm Set) Len() int {
	return pm.root.len()
}

func (pm Set) Union(other *Set) {
	root := pm.root
	pm.root = setUnion(root, other.root, true)
}

func newSetNode(element Value) *setNode {
	return &setNode{
		element: element,
		weight:  rand.Uint64(),
	}
}

func (node *setNode) shallowClone() *setNode {
	return &setNode{
		element: node.element,
		weight:  node.weight,
	}
}

func (node *setNode) forEach(f func(element Value)) {
	if node == nil {
		return
	}
	node.left.forEach(f)
	f(node.element)
	node.right.forEach(f)
}

func (node *setNode) len() int {
	if node == nil {
		return 0
	}
	return node.left.len() + 1 + node.right.len()
}

func setUnion(first, second *setNode, overwrite bool) *setNode {
	if first == nil {
		return second
	}
	if second == nil {
		return first
	}

	if first.weight < second.weight {
		second, first, overwrite = first, second, !overwrite
	}

	left, mid, right := setSplit(second, first.element, false)
	var result *setNode
	if overwrite && mid != nil {
		result = mid.shallowClone()
	} else {
		result = first.shallowClone()
	}
	result.weight = first.weight
	result.left = setUnion(first.left, left, overwrite)
	result.right = setUnion(first.right, right, overwrite)
	return result
}

// setSplit the tree midway by the element into three different ones.
// Return three new trees: left with all nodes with smaller than element, mid with
// the node matching the element, right with all nodes larger than element.
// If there are no nodes in one of trees, return nil instead of it.
// If requireMid is set (such as during deletion), then all return arguments
// are nil if mid is not found.
//
// setSplit(n:-0) (left:+1, mid:+1, right:+1)
// Split borrows n without affecting its refcount, and returns three
// new references that the caller is expected to call decref.
func setSplit(n *setNode, element Value, requireMid bool) (left, mid, right *setNode) {
	if n == nil {
		return nil, nil, nil
	}

	if n.element.compare(element) {
		left, mid, right := setSplit(n.right, element, requireMid)
		if requireMid && mid == nil {
			return nil, nil, nil
		}
		newN := n.shallowClone()
		newN.left = n.left
		newN.right = left
		return newN, mid, right
	} else if element.compare(n.element) {
		left, mid, right := setSplit(n.left, element, requireMid)
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

func setMerge(left, right *setNode) *setNode {
	switch {
	case left == nil:
		return right
	case right == nil:
		return left
	case left.weight > right.weight:
		root := left.shallowClone()
		root.left = left.left
		root.right = setMerge(left.right, right)
		return root
	default:
		root := right.shallowClone()
		root.left = setMerge(left, right.left)
		root.right = right.right
		return root
	}
}
