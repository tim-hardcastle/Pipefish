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

type IdentifierPosition int 

const (
	BLING IdentifierPosition = iota 
)

type BlingData struct {
	Bling string
	Pos   IdentifierPosition
}

type BlingManager struct {
	navigators []*blingNavigator // As functions compose, we need a stack of navigators to keep track of where we are. 
}

func newBlingManager() *BlingManager {
	newBm := &BlingManager{[]*blingNavigator{}}
	return newBm
}



func (bm *BlingManager) startFunction(s string, tree blingTree) {
	//println("Starting function", s)
	bm.navigators = append(bm.navigators, tree.newBlingNavigator(s))
}

func (bm *BlingManager) stopFunction() {
	//println("stopping")
	bm.navigators = bm.navigators[0:len(bm.navigators)-1]
}

func (bm *BlingManager) canBling(s string, pos IdentifierPosition) bool {
	if len(bm.navigators) == 0 {
		//println("canBling: no navigator")
		return false
	}
	result := bm.navigators[len(bm.navigators)-1].canBling(s, pos)
	//println("canBling", s, false)
	return result
}

func (bm *BlingManager) canEndfix(s string) bool {
	if len(bm.navigators) == 0 {
		return false
	}
	result := bm.navigators[len(bm.navigators)-1].canEndfix(s)
	return result
}

func (bm *BlingManager) doBling(s string) {
	bm.navigators[len(bm.navigators)-1].doBling(s)
}


// Go doesn't allow recursive definitions, so ...
type blingTree map[BlingData]any

type blingNavigator struct {
	position blingTree
}

func newBlingTree() blingTree {
	bt := make(blingTree, 0)
	bt.AddBling([]BlingData{{"=", BLING}})
	return bt
}

func (b blingTree) AddBling(bling []BlingData) {
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
		result = result + s + "'" + k.Bling + "'" + "\n" + v.(blingTree).recursiveString(s + "    ")
	}
	return result
}

func (b blingTree) newBlingNavigator(s string) *blingNavigator {
	result, ok := b[BlingData{s, BLING}]
	if !ok { // TODO --- this is a temporary hack around type declarations and so on again and can be sorted out by separating them from the ordinary parsing.
		return &blingNavigator{position: make(blingTree)}
	}
	return &blingNavigator{position: result.(blingTree)}
}

func (bn *blingNavigator) canBling(s string, poss ... IdentifierPosition) bool {
	for _, pos := range poss {
	_, ok := (bn.position)[BlingData{s, pos}]
		if ok {
			return true
		}
	}
	return false
}

func (bn *blingNavigator) canEndfix(s string) bool {
	t, ok := (bn.position)[BlingData{s, BLING}]
	if ok {
		return len(t.(blingTree)) == 0
	}
	return false
}

func (bn *blingNavigator) doBling(s string, poss ... IdentifierPosition) {
	for _, pos := range poss {
		newPosition, ok := (bn.position)[BlingData{s, pos}].(blingTree)
		if ok {
			bn.position = newPosition
			return
		}
	}
}
