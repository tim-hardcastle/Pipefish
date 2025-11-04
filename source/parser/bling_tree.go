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

type IdentifierPosition int

const (
	PREFIX IdentifierPosition = iota
	INFIX
	SUFFIX
	UNFIX
	FOREFIX
	MIDFIX
	ENDFIX
)

var ANY_BLING = []IdentifierPosition{FOREFIX, MIDFIX, ENDFIX}

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

func (bm *BlingManager) startFunction(s string, pos IdentifierPosition, tree blingTree) {
	bm.navigators = append(bm.navigators, tree.newBlingNavigator(s, pos))
}

func (bm *BlingManager) stopFunction() {
	bm.navigators = bm.navigators[0 : len(bm.navigators)-1]
}

func (bm *BlingManager) canBling(s string, pos ...IdentifierPosition) bool {
	if len(bm.navigators) == 0 {
		return false
	}
	return bm.navigators[len(bm.navigators)-1].canBling(s, pos...)
}

func (bm *BlingManager) didBling(s string, pos ...IdentifierPosition) bool {
	if len(bm.navigators) == 0 {
		return false
	}
	return bm.navigators[len(bm.navigators)-1].didBling(s, pos...)
}

func (bm *BlingManager) canEndfix(s string) bool {
	if len(bm.navigators) == 0 {
		return false
	}
	result := bm.navigators[len(bm.navigators)-1].canEndfix(s)
	return result
}

func (bm *BlingManager) doBling(s string, pos ...IdentifierPosition) {
	bm.navigators[len(bm.navigators)-1].doBling(s, pos...)
}

// Go doesn't allow recursive definitions, so ...
type blingTree map[BlingData]any

type blingNavigator struct {
	position blingTree
	wasBling BlingData
}

func newBlingTree() blingTree {
	bt := make(blingTree, 0)
	bt.AddBling([]BlingData{{"=", INFIX}})
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
		result = result + s + "'" + k.Bling + "'" + "\n" + v.(blingTree).recursiveString(s+"    ")
	}
	return result
}

func (b blingTree) newBlingNavigator(s string, pos IdentifierPosition) *blingNavigator {
	result, ok := b[BlingData{s, pos}]
	if !ok { // TODO --- this is a temporary hack around type declarations and so on again and can be sorted out by separating them from the ordinary parsing.
		return &blingNavigator{position: make(blingTree)}
	}
	return &blingNavigator{position: result.(blingTree), wasBling: BlingData{s, pos}}
}

func (bn *blingNavigator) canBling(s string, poss ...IdentifierPosition) bool {
	for _, pos := range poss {
		_, ok := (bn.position)[BlingData{s, pos}]
		if ok {
			return true
		}
	}
	return false
}

func (bn *blingNavigator) didBling(s string, poss ...IdentifierPosition) bool {
	for _, pos := range poss {
		if bn.wasBling.Bling == s && bn.wasBling.Pos == pos {
			return true
		}
	}
	return false
}

func (bn *blingNavigator) canEndfix(s string) bool {
	_, ok := (bn.position)[BlingData{s, ENDFIX}]
	return ok
}

func (bn *blingNavigator) doBling(s string, poss ...IdentifierPosition) {
	for _, pos := range poss {
		newPosition, ok := (bn.position)[BlingData{s, pos}].(blingTree)
		if ok {
			bn.wasBling = BlingData{s, pos}
			bn.position = newPosition
			return
		}
	}
}
