package dtypes

import (
	"fmt"
)

type Digraph[E comparable] map[E]Set[E]

func (D Digraph[E]) String() string {
	result := "{\n"
	for k, v := range D {
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
	return result, extractCycle(D)
}

// This is just for the Ordering function to use: *IF* the digraph at the end of the
// Ordering function is non-empty, then it consists of a bunch of cycles, and we can
// choose one of them to return as proof of this fact.
func extractCycle[E comparable](D Digraph[E]) []E {
	start, ok := D.GetArbitraryNode()
	if !ok {
		return nil
	}
	result := []E{start}
	for next, ok := ((D)[start]).GetArbitraryElement(); true; next, ok = ((D)[next]).GetArbitraryElement() {
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

type data[E comparable] struct {
	graph  Digraph[E]
	nodes  []node
	stack  []E
	index  map[E]int
	output [][]E
}

type node struct {
	lowlink int
	stacked bool
}

// This partitions the graph into strongly-connected components
func (graph Digraph[E]) Tarjan() [][]E {
	g := &data[E]{
		graph: graph,
		nodes: make([]node, 0, len(graph)),
		index: make(map[E]int, len(graph)),
	}
	for v := range g.graph {
		if _, ok := g.index[v]; !ok {
			g.getStronglyConnectedComponent(v)
		}
	}
	return g.output
}

func (data *data[E]) getStronglyConnectedComponent(v E) *node {
	index := len(data.nodes)
	data.index[v] = index
	data.stack = append(data.stack, v)
	data.nodes = append(data.nodes, node{lowlink: index, stacked: true})
	node := &data.nodes[index]

	for w := range data.graph[v] {
		i, seen := data.index[w]
		if !seen {
			n := data.getStronglyConnectedComponent(w)
			if n.lowlink < node.lowlink {
				node.lowlink = n.lowlink
			}
		} else if data.nodes[i].stacked {
			if i < node.lowlink {
				node.lowlink = i
			}
		}
	}

	if node.lowlink == index {
		var vertices []E
		i := len(data.stack) - 1
		for {
			w := data.stack[i]
			stackIndex := data.index[w]
			data.nodes[stackIndex].stacked = false
			vertices = append(vertices, w)
			if stackIndex == index {
				break
			}
			i--
		}
		data.stack = data.stack[:i]
		data.output = append(data.output, vertices)
	}

	return node
}

func (D Digraph[E]) SetOfNodes() *Set[E] {
	result := Set[E]{}
	for x := range D {
		result.Add(x)
	}
	return &result
}

func (D Digraph[E]) GetArbitraryNode() (E, bool) {
	var result E
	var ok bool
	for k := range D { // There should be a less clumsy way to do this but ...
		result = k
		ok = true
		break
	}
	return result, ok
}

// This checks to see if a node already has an entry before adding it to the digraph.
func (D Digraph[E]) AddSafe(node E, neighbors []E) bool {
	if !D.SetOfNodes().Contains(node) {
		D.Add(node, neighbors)
		return true
	}
	return false
}

// This adds an arrow with transitive closure to a digraph, on the assumption that it is
// already transitively closed.
func (D Digraph[E]) AddTransitiveArrow(a, b E) {
	if !D.SetOfNodes().Contains(a) {
		(D)[a] = Set[E]{b: struct{}{}}
	}
	if !D.SetOfNodes().Contains(b) {
		(D)[b] = Set[E]{}
	}
	(D)[a].Add(b)
	(D)[a].AddSet((D)[b])
	for e := range *(D.ArrowsTo(a)) {
		(D)[e].Add(b)
		(D)[e].AddSet((D)[b])
	}
}

func (D Digraph[E]) ArrowsTo(e E) *Set[E] {
	result := Set[E]{}
	for k, V := range D {
		if V.Contains(e) {
			result.Add(k)
		}
	}
	return &result
}

func (D *Digraph[E]) Add(node E, neighbors []E) {
	s := MakeFromSlice(neighbors)
	(*D)[node] = s
}

func (D *Digraph[E]) StripLeafnodes() Set[E] {
	result := Set[E]{}
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
