package parser

// So the problem is this. If we have bit of bling which is the same as an operator (as is 
// perfectly reasonable especially as I have 'for' and 'from' as prefixes as part of the core
// language) then if we have something which might be a function or a value (e.g. a type 
// operator/expression or a variable of type 'func') then it will by default assume that it's
// followed by the prefix and is "positionally functional".

// The way round this is for the parser to know from the point where it starts parsing a 
// function which bling might be upcoming, and to treat that presumptively as bling.
// (Parentheses would of course distinguish the case where it's not bling but that would
// be vary rare in any case.)

// TODO --- this could replace pretty much all of the ways we presently handle bling in the 
// parser.

type BlingManager struct {
	navigators []*blingNavigator // As functions compose, we need a stack of navigators to keep track of where we are. 
}

func newBlingManager() *BlingManager {
	newBm := &BlingManager{[]*blingNavigator{}}
	return newBm
}



func (bm *BlingManager) startFunction(s string, tree blingTree) {
	bm.navigators = append(bm.navigators, tree.newBlingNavigator(s))
}

func (bm *BlingManager) stopFunction() {
	bm.navigators = bm.navigators[0:len(bm.navigators)-1]
}

func (bm *BlingManager) canBling(s string) bool {
	if len(bm.navigators) == 0 {
		return false
	}
	return bm.navigators[len(bm.navigators)-1].canBling(s)
}



func (bm *BlingManager) doBling(s string) {
	bm.navigators[len(bm.navigators)-1].doBling(s)
}


// Go doesn't allow recursive definitions, so ...
type blingTree map[string]any

type blingNavigator struct {
	position blingTree
}

func newBlingTree() blingTree {
	bt := make(blingTree, 0)
	bt.AddBling([]string{"="})
	return bt
}

func (b blingTree) AddBling(bling []string) {
	if len(bling) == 0 {
		return
	}
	head := bling[0]
	_, ok := b[head] 
	if !ok {
		b[head] = make(blingTree)
	}
	b[head].(blingTree).AddBling(bling[1:])
}

func (b blingTree) String() string {
	return b.recursiveString("")
}

func (b blingTree) recursiveString(s string) string {
	result := ""
	for k, v := range b {
		result = result + s + "'" + k + "'" + "\n" + v.(blingTree).recursiveString(s + "    ")
	}
	return result
}

func (b blingTree) newBlingNavigator(s string) *blingNavigator {
	result, ok := b[s]
	if !ok { // TODO --- this is a temporary hack around type declarations and so on again and can be sorted out by separating them from the ordinary parsing.
		return &blingNavigator{position: make(blingTree)}
	}
	return &blingNavigator{position: result.(blingTree)}
}

func (bn *blingNavigator) canBling(s string) bool {
	_, ok := (bn.position)[s]
	return ok
}

func (bn *blingNavigator) doBling(s string) {
	bn.position, _ = (bn.position)[s].(blingTree)
}
