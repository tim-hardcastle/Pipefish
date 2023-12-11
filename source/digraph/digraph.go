package digraph

// We have a digraph given in the form of a map associating each node with the set
// of nodes it points to. We want to list all the nodes of the graph in such a way that
// no node X ever precedes a node Y in the list if there is a route from X to Y, or, if this
// is not possible given the map, our function should notice this and say so.

// (This is possible only if the graph is acyclic, i.e. a forest.)

// We can do this like this:

// while there are leaf nodes in the graph :
// add all the leaf nodes to the end of the list
// remove the leaf nodes from the graph
// if there are still nodes in the graph :
// complain that the graph is cyclic
// otherwise :
// return the list

// Proof : if the graph is a forest, this process must terminate with an empty
// graph, by induction or by "well duh". The way in which the nodes are added to the
// graph guarantees that they will fulfill the constraint. But if the graph
// contains a cycle then when we run out of leaf nodes the graph is non-empty,
// since no member of the cycle is a leaf node.

// In this implementation I will return as parameters the list and a cycle from
// the digraph: if the cycle is of length zero then the list is valid.

import (
	"charm/source/set"
	"fmt"
)

type Digraph[E comparable] map[E]set.Set[E]

func (D *Digraph[E]) String() string {
	result := "{\n"
	for k, v := range *D {
		result += fmt.Sprintf("%v : %v", k, v.String())
	}
	result += "}\n"
	return result
}

func Ordering[E comparable](D Digraph[E]) ([]E, []E) {
	result := []E{}
	for leafnodes := D.StripLeafnodes(); len(leafnodes) > 0; leafnodes = D.StripLeafnodes() {
		result = append(result, leafnodes.ToSlice()...)
	}
	return result, extractCycle(&D)
}

// This is just for the Ordering function to use: *IF* the digraph at the end of the
// Ordering function is non-empty, then it consists of a bunch of cycles, and we can
// choose one of them to return as proof of this fact.
func extractCycle[E comparable](D *Digraph[E]) []E {
	start, ok := D.GetArbitraryNode()
	if !ok {
		return []E{}
	}
	result := []E{start}
	for next, ok := ((*D)[start]).GetArbitraryElement(); true; next, ok = ((*D)[next]).GetArbitraryElement() {
		if !ok {
			panic("extractCycle has found a leaf node, this is bad.")
		}
		if i := Index(result, next); i != -1 {
			result = result[i:]
			break
		}
		result = append(result, next)
	}

	return result
}

// In a digraph D, if we have x in D[y] for some y but x itself is undefined, something has gone
// wrong. (x would NOT represent a leaf node, which would be represented by D[x] being {}.)
// In the case of Charm, the particular thing gone wrong will be that something has been defined
// in terms of a constant or variable which doesn't exist.
func (D *Digraph[E]) Check() (bool, E) {
	nodes := *(D.SetOfNodes())
	for v := range nodes {
		for w := range (*D)[v] {
			if !nodes.Contains(w) {
				return false, w
			}
		}
	}
	var x E
	return true, x
}

func (D *Digraph[E]) SetOfNodes() *set.Set[E] {
	result := set.Set[E]{}
	for x := range *D {
		result.Add(x)
	}
	return &result
}

func (D *Digraph[E]) GetArbitraryNode() (E, bool) {
	var result E
	var ok bool
	for k := range *D { // There should be a less clumsy way to do this but ...
		result = k
		ok = true
		break
	}
	return result, ok
}

// This checks to see if a node already has an entry before adding it to the digraph.
func (D *Digraph[E]) AddSafe(node E, neighbors []E) bool {
	if !D.SetOfNodes().Contains(node) {
		D.Add(node, neighbors)
		return true
	}
	return false
}

// This adds an arrow with transitive closure to a digraph, on the assumption that it is
// already transitively closed.
func (D *Digraph[E]) AddTransitiveArrow(a, b E) {
	if !D.SetOfNodes().Contains(a) {
		(*D)[a] = set.Set[E]{b: struct{}{}}
	}
	if !D.SetOfNodes().Contains(b) {
		(*D)[b] = set.Set[E]{}
	}
	(*D)[a].Add(b)
	(*D)[a].AddSet((*D)[b])
	for e := range *(D.ArrowsTo(a)) {
		(*D)[e].Add(b)
		(*D)[e].AddSet((*D)[b])
	}
}

func (D *Digraph[E]) ArrowsTo(e E) *set.Set[E] {
	result := set.Set[E]{}
	for k, V := range *D {
		if V.Contains(e) {
			result.Add(k)
		}
	}
	return &result
}

func (D *Digraph[E]) Add(node E, neighbors []E) {
	s := *set.MakeFromSlice(neighbors)
	(*D)[node] = s
}

func (D *Digraph[E]) StripLeafnodes() set.Set[E] {
	result := set.Set[E]{}
	for k, v := range *D {
		if v.IsEmpty() {
			result.Add(k)
		}
	}
	for k := range result {
		delete(*D, k)
	}
	for _, V := range *D {
		for e := range result {
			delete(V, e)
		}
	}
	return result
}

func Index[E comparable](slice []E, element E) int {
	result := -1
	for k, v := range slice {
		if v == element {
			result = k
			break
		}
	}
	return result
}

func (T *Digraph[E]) PointsTo(candidate, target E) bool {
	return (*T)[candidate].Contains(target)
}
