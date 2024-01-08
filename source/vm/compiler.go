package vm

import (
	"charm/source/ast"
	"charm/source/parser"
	"charm/source/set"
	"charm/source/token"
)

const SHOW_BYTECODE = false
const SHOW_COMPILE = true
const SHOW_RUN = true

type enumOrdinates struct {
	enum    simpleType
	element int
}

type thunk struct {
	mLoc uint32
	cLoc uint32
}

type fnTreeNode struct {
	fn     *cpFunc
	branch []*typeNodePair
}

type typeNodePair struct {
	typeName string
	fnNode   *fnTreeNode
}

type Compiler struct {
	p                  *parser.Parser
	vm                 *Vm
	enums              map[string]enumOrdinates
	gconsts            *environment
	gvars              *environment
	fns                []*cpFunc
	thunkList          []thunk
	typeNameToTypeList map[string]alternateType

	tupleType uint32 // Location of a constant saying {TYPE, <type number of tuples>}
}

type cpFunc struct {
	callTo  uint32
	loReg   uint32
	hiReg   uint32
	outReg  uint32
	types   alternateType
	builtin string // A non-empty string in case it is a builtin.
}

const DUMMY = 4294967295

func NewCompiler(p *parser.Parser) *Compiler {
	return &Compiler{
		p:         p,
		vm:        blankVm(),
		enums:     make(map[string]enumOrdinates),
		gconsts:   newEnvironment(),
		gvars:     newEnvironment(),
		thunkList: []thunk{},
		fns:       []*cpFunc{},
		typeNameToTypeList: map[string]alternateType{
			"int":      {INT},
			"string":   {STRING},
			"bool":     {BOOL},
			"float64":  {FLOAT},
			"error":    {ERROR},
			"type":     {TYPE},
			"int?":     {NULL, INT},
			"string?":  {NULL, STRING},
			"bool?":    {NULL, BOOL},
			"float64?": {NULL, FLOAT},
			"type?":    {NULL, TYPE},
			"null":     {NULL},
			"single":   {INT, BOOL, STRING, FLOAT, TYPE},
			"single?":  {NULL, INT, BOOL, STRING, FLOAT, TYPE},
		},
	}
}

func (cp *Compiler) Run() {
	cp.vm.Run(0)
}

func (cp *Compiler) GetParser() *parser.Parser {
	return cp.p
}

func (cp *Compiler) Do(line string) string {
	mT := cp.vm.memTop()
	cT := cp.vm.codeTop()
	node := cp.p.ParseLine("REPL input", line)
	if cp.p.ErrorsExist() {
		return ""
	}
	cp.compileNode(cp.vm, node, cp.gvars)
	if cp.p.ErrorsExist() {
		return ""
	}
	cp.emit(cp.vm, ret)
	if SHOW_BYTECODE {
		print("\nBytecode:\n\n")
		for i := cT; i < cp.vm.codeTop(); i++ {
			println(cp.vm.describeCode(i))
		}
	}
	cp.vm.Run(cT)
	result := cp.vm.mem[cp.vm.that()]
	cp.vm.mem = cp.vm.mem[:mT]
	cp.vm.code = cp.vm.code[:cT]
	return cp.vm.literal(result)
}

func (cp *Compiler) Compile(source, sourcecode string) {
	if SHOW_COMPILE {
		print("\nCompiling\n\n")
	}
	cp.vm = blankVm()
	node := cp.p.ParseLine(source, sourcecode)
	cp.compileNode(cp.vm, node, cp.gvars)
	cp.emit(cp.vm, ret)
}

func (cp *Compiler) reserve(vm *Vm, t simpleType, v any) uint32 {
	vm.mem = append(vm.mem, Value{T: t, V: v})
	return uint32(len(vm.mem) - 1)
}

func (cp *Compiler) addVariable(vm *Vm, env *environment, name string, acc varAccess, types alternateType) {
	env.data[name] = variable{mLoc: vm.that(), access: acc, types: types}
}

func (cp *Compiler) compileNode(vm *Vm, node ast.Node, env *environment) (alternateType, bool) {
	rtnTypes, rtnConst := alternateType{}, true
	mT := vm.memTop()
	cT := vm.codeTop()
	switch node := node.(type) {
	case *ast.IntegerLiteral:
		cp.reserve(vm, INT, node.Value)
		rtnTypes, rtnConst = simpleList(INT), true
		break
	case *ast.StringLiteral:
		cp.reserve(vm, STRING, node.Value)
		rtnTypes, rtnConst = simpleList(STRING), true
		break
	case *ast.BooleanLiteral:
		cp.reserve(vm, BOOL, node.Value)
		rtnTypes, rtnConst = simpleList(BOOL), true
		break
	case *ast.FloatLiteral:
		cp.reserve(vm, FLOAT, node.Value)
		rtnTypes, rtnConst = simpleList(FLOAT), true
		break
	case *ast.TypeLiteral:
		cp.reserve(vm, TYPE, cp.typeNameToTypeList[node.Value][0])
		rtnTypes, rtnConst = simpleList(TYPE), true
		break
	case *ast.InfixExpression:
		if cp.p.Infixes.Contains(node.Operator) {
			rtnTypes, rtnConst = cp.createFunctionCall(vm, node, env)
			break
		}
		if node.Operator == "," {
			rtnTypes, rtnConst = cp.emitComma(vm, node, env)
			break
		}
		if node.Operator == "==" {
			rtnTypes, rtnConst = cp.emitEquals(vm, node, env)
			break
		}
		if node.Operator == "!=" {
			rtnTypes, rtnConst = cp.emitEquals(vm, node, env)
			cp.put(vm, notb, vm.that())
			break
		}
		cp.p.Throw("comp/infix", node.Token)
		rtnTypes, rtnConst = simpleList(ERROR), true
		break
	case *ast.LazyInfixExpression:
		if node.Operator == "or" {
			lTypes, lcst := cp.compileNode(vm, node.Left, env)
			if !lTypes.contains(BOOL) {
				cp.p.Throw("comp/or/bool/left", node.Token)
				rtnTypes, rtnConst = simpleList(ERROR), true
				break
			}
			leftRg := vm.that()
			cp.emit(vm, qtru, leftRg, vm.next()+2)
			backtrack := vm.next()
			cp.emit(vm, jmp, DUMMY)
			rTypes, rcst := cp.compileNode(vm, node.Right, env)
			if !rTypes.contains(BOOL) {
				cp.p.Throw("comp/or/bool/right", node.Token)
				rtnTypes, rtnConst = simpleList(ERROR), true
				break
			}
			rightRg := vm.that()
			vm.code[backtrack].args[0] = vm.next()
			cp.put(vm, orb, leftRg, rightRg)
			rtnTypes, rtnConst = simpleList(BOOL), lcst && rcst
			break
		}
		if node.Operator == "and" {
			lTypes, lcst := cp.compileNode(vm, node.Left, env)
			if !lTypes.contains(BOOL) {
				cp.p.Throw("comp/and/bool/left", node.Token)
				rtnTypes, rtnConst = simpleList(ERROR), true
				break
			}
			leftRg := vm.that()
			backtrack := vm.next()
			cp.emit(vm, qtru, leftRg, DUMMY)
			rTypes, rcst := cp.compileNode(vm, node.Right, env)
			if !rTypes.contains(BOOL) {
				cp.p.Throw("comp/and/bool/right", node.Token)
				rtnTypes, rtnConst = simpleList(ERROR), true
				break
			}
			rightRg := vm.that()
			vm.code[backtrack].args[1] = vm.next()
			cp.put(vm, andb, leftRg, rightRg)
			rtnTypes, rtnConst = simpleList(BOOL), lcst && rcst
			break
		}
		if node.Operator == ":" {
			if node.Left.GetToken().Type == token.ELSE {
				rtnTypes, rtnConst = cp.compileNode(vm, node.Right, env)
			}
			lTypes, lcst := cp.compileNode(vm, node.Left, env)
			if !lTypes.contains(BOOL) {
				cp.p.Throw("comp/cond/bool", node.Token)
				rtnTypes, rtnConst = simpleList(ERROR), true
				break
			}
			leftRg := vm.that()
			backtrack := vm.next()
			cp.emit(vm, qtru, leftRg, DUMMY)
			rTypes, rcst := cp.compileNode(vm, node.Right, env)
			cp.put(vm, asgm, vm.that())
			cp.emit(vm, jmp, vm.next()+2)
			vm.code[backtrack].args[1] = vm.next()
			cp.reput(vm, asgm, C_U_OBJ)
			rtnTypes, rtnConst = rTypes.union(simpleList(UNSAT)), lcst && rcst
			break
		}
		if node.Operator == ";" {
			lTypes, lcst := cp.compileNode(vm, node.Left, env)
			// We deal with the case where the newline is separating local constant definitions
			// in the 'given' block.
			if lTypes.only(CREATED_LOCAL_CONSTANT) {
				cp.compileNode(vm, node.Right, env)
				rtnTypes, rtnConst = simpleList(CREATED_LOCAL_CONSTANT), true
				break
			}
			leftRg := vm.that()
			backtrack := vm.next()
			cp.emit(vm, qtyp, leftRg, uint32(UNSAT), DUMMY)
			rTypes, rcst := cp.compileNode(vm, node.Right, env)
			rightRg := vm.that()
			cp.put(vm, asgm, rightRg)
			cp.emit(vm, jmp, vm.next()+2)
			vm.code[backtrack].args[2] = vm.next()
			cp.reput(vm, asgm, leftRg)
			if !(lTypes.contains(UNSAT) && rTypes.contains(UNSAT)) {
				rtnTypes, rtnConst = lTypes.union(rTypes).without(UNSAT), lcst && rcst
				break
			}
			rtnTypes, rtnConst = lTypes.union(rTypes), lcst && rcst
			break
		}
	case *ast.Identifier:
		v, ok := env.getVar(node.Value)
		if ok {
			if v.access == LOCAL_CONSTANT_THUNK {
				cp.emit(vm, untk, v.mLoc)
			}
			cp.put(vm, asgm, v.mLoc)
			rtnTypes = v.types
			rtnConst = ALL_CONST_ACCESS.Contains(v.access)
			break
		}
		cp.p.Throw("comp/ident/known", node.Token)
		rtnTypes, rtnConst = simpleList(ERROR), true
		break
	case *ast.AssignmentExpression:
		if node.Token.Type == token.GVN_ASSIGN {
			// TODO --- need to do this better after we implement tuples
			if node.Left.GetToken().Type != token.IDENT {
				cp.p.Throw("comp/assign/ident", node.Left.GetToken())
				rtnTypes, rtnConst = simpleList(ERROR), true
				break
			}
			thunkStart := vm.next()
			types, _ := cp.compileNode(vm, node.Right, env)
			cp.emit(vm, ret)
			cp.addVariable(vm, env, node.Left.(*ast.Identifier).Value, LOCAL_CONSTANT_THUNK, types)
			cp.thunkList = append(cp.thunkList, thunk{vm.that(), thunkStart})
			rtnTypes, rtnConst = simpleList(CREATED_LOCAL_CONSTANT), true
		}
		cp.p.Throw("comp/assign", node.Token)
		rtnTypes, rtnConst = simpleList(ERROR), true
		break
	case *ast.PrefixExpression:
		if node.Operator == "not" {
			allTypes, cst := cp.compileNode(vm, node.Args[0], env)
			if allTypes.only(BOOL) {
				cp.put(vm, notb, vm.that())
				rtnTypes, rtnConst = simpleList(BOOL), cst
				break
			}
			if !allTypes.contains(BOOL) {
				cp.p.Throw("comp/not/bool", node.Token)
				rtnTypes, rtnConst = simpleList(ERROR), true
				break
			}
		}
		if cp.p.Prefixes.Contains(node.Operator) || cp.p.Functions.Contains(node.Operator) {
			rtnTypes, rtnConst = cp.createFunctionCall(vm, node, env)
			break
		}
		cp.p.Throw("comp/prefix/known", node.Token)
		rtnTypes, rtnConst = simpleList(ERROR), true
		break
	case *ast.SuffixExpression:
		if cp.p.Suffixes.Contains(node.Operator) {
			rtnTypes, rtnConst = cp.createFunctionCall(vm, node, env)
			break
		}
		cp.p.Throw("comp/suffix", node.Token)
		rtnTypes, rtnConst = simpleList(ERROR), true
		break
	default:
		panic("Unimplemented node type.")
	}
	if rtnConst && vm.codeTop() > cT {
		if SHOW_COMPILE {
			println("Expression is constant. Folding.")
		}
		cp.emit(vm, ret)
		vm.Run(cT)
		result := vm.mem[vm.that()]
		vm.mem = vm.mem[:mT]
		vm.code = vm.code[:cT]
		cp.reserve(vm, result.T, result.V)
	}
	return rtnTypes, rtnConst
}

// This needs its own very special logic because the type it returns has to be composed in a different way from all the other operators.
func (cp *Compiler) emitComma(vm *Vm, node *ast.InfixExpression, env *environment) (alternateType, bool) {
	lTypes, lcst := cp.compileNode(vm, node.Args[0], env)
	if lTypes.only(ERROR) {
		cp.p.Throw("comp/tuple/err/a", node.Token)
	}
	left := vm.that()
	rTypes, rcst := cp.compileNode(vm, node.Args[2], env)
	if rTypes.only(ERROR) {
		cp.p.Throw("comp/tuple/err/b", node.Token)
	}
	right := vm.that()
	var leftBacktrack, rightBacktrack uint32
	if lTypes.contains(ERROR) {
		cp.emit(vm, qtyp, left, uint32(ERROR), vm.codeTop()+2)
		leftBacktrack = vm.codeTop()
		cp.put(vm, asgm, DUMMY, left)
		if rTypes.contains(ERROR) {
			cp.emit(vm, jmp, vm.codeTop()+5)
		} else {
			cp.emit(vm, jmp, vm.codeTop()+2)
		}
	}
	if rTypes.contains(ERROR) {
		cp.emit(vm, qtyp, right, uint32(ERROR), vm.codeTop()+2)
		rightBacktrack = vm.codeTop()
		cp.put(vm, asgm, right)
		cp.emit(vm, jmp, vm.codeTop()+2)
	}
	leftMustBeSingle, leftMustBeTuple := lTypes.mustBeSingleOrTuple()
	rightMustBeSingle, rightMustBeTuple := rTypes.mustBeSingleOrTuple()
	switch {
	case leftMustBeSingle && rightMustBeSingle:
		cp.put(vm, cc11, left, right)
	case leftMustBeSingle && rightMustBeTuple:
		cp.put(vm, cc1T, left, right)
	case leftMustBeTuple && rightMustBeSingle:
		cp.put(vm, ccT1, left, right)
	case leftMustBeTuple && rightMustBeTuple:
		cp.put(vm, ccTT, left, right)
	default:
		cp.put(vm, ccxx, left, right) // We can after all let the operation dispatch for us.
	}
	if lTypes.contains(ERROR) {
		vm.code[leftBacktrack].args[0] = vm.that()
	}
	if rTypes.contains(ERROR) {
		vm.code[rightBacktrack].args[0] = vm.that()
	}
	lT := lTypes.reduce()
	rT := rTypes.reduce()
	cst := lcst && rcst
	switch lT := lT.(type) {
	case finiteTupleType:
		switch rT := rT.(type) {
		case finiteTupleType:
			return alternateType{append(lT, rT...)}, cst
		case typedTupleType:
			return alternateType{typedTupleType{rT.t.union(getAllTypes(lT))}}, cst
		case simpleType:
			return alternateType{finiteTupleType{append(lT, rT)}}, cst
		case alternateType:
			return alternateType{finiteTupleType{append(lT, rT)}}, cst // TODO --- check if this works.
		default:
			panic("We shouldn't be here!")
		}
	case typedTupleType:
		switch rT := rT.(type) {
		case finiteTupleType:
			return alternateType{typedTupleType{lT.t.union(getAllTypes(rT))}}, cst
		case typedTupleType:
			return alternateType{typedTupleType{lT.t.union(rT.t)}}, cst
		case simpleType:
			return alternateType{typedTupleType{lT.t.union(simpleList(rT))}}, cst
		case alternateType:
			return alternateType{typedTupleType{lT.t.union(getAllTypes(rT))}}, cst
		default:
			panic("We shouldn't be here!")
		}
	case simpleType:
		switch rT := rT.(type) {
		case finiteTupleType:
			return alternateType{append(finiteTupleType{lT}, rT...)}, cst
		case typedTupleType:
			return alternateType{typedTupleType{rT.t.union(simpleList(lT))}}, cst
		case simpleType:
			return alternateType{finiteTupleType{lT, rT}}, cst
		case alternateType:
			return alternateType{finiteTupleType{lT, rT}}, cst
		default:
			panic("We shouldn't be here!")
		}
	case alternateType:
		switch rT := rT.(type) {
		case finiteTupleType:
			return alternateType{append(finiteTupleType{lT}, rT...)}, cst
		case typedTupleType:
			return alternateType{typedTupleType{rT.t.union(lT)}}, cst
		case simpleType:
			return alternateType{finiteTupleType{lT, rT}}, cst
		case alternateType:
			return append(lT, rT...), cst
		default:
			panic("We shouldn't be here!")
		}
	default:
		panic("We shouldn't be here!")
	}
}

func getAllTypes(ts typeScheme) alternateType {
	result := alternateType{}
	switch ts := ts.(type) {
	case alternateType:
		for _, v := range ts {
			result = result.union(getAllTypes(v))
		}
	case typedTupleType:
		result = ts.t
	case finiteTupleType:
		for _, v := range ts {
			result = result.union(getAllTypes(v))
		}
	case simpleType:
		result = simpleList(ts)
	default:
		panic("We shouldn't be here!")
	}
	return result
}

func (ts alternateType) reduce() typeScheme { // Turns alternative types with only on option into their contents.
	if len(ts) == 1 {
		return ts[0]
	}
	return ts
}

func (t alternateType) mustBeSingleOrTuple() (bool, bool) {
	s, T := true, true
	for _, v := range t {
		switch v.(type) {
		case simpleType:
			T = false
		default:
			s = false
		}
	}
	return s, T
}

func (cp *Compiler) createFunctionCall(vm *Vm, node ast.Callable, env *environment) (alternateType, bool) {
	args := node.GetArgs()
	b := &bindle{tok: node.GetToken(),
		treePosition: cp.p.FunctionTreeMap[node.GetToken().Literal],
		outLoc:       cp.reserve(vm, ERROR, DUMMY),
		env:          env,
		valLocs:      make([]uint32, len(args)),
		types:        make(finiteTupleType, len(args)),
	}
	var cstI bool
	cst := true
	for i, arg := range args {
		switch arg := arg.(type) {
		case *ast.Bling:
			b.types[i] = alternateType{blingType{arg.Value}}
		default:
			b.types[i], cstI = cp.compileNode(vm, arg, env)
			cst = cst && cstI
			b.valLocs[i] = vm.that()
		}
	}
	returnTypes := cp.generateNewArgument(vm, b)
	cp.put(vm, asgm, b.outLoc)
	if returnTypes.only(ERROR) {
		cp.p.Throw("comp/call", b.tok)
	}
	return returnTypes, cst
}

type bindle struct {
	treePosition *ast.FnTreeNode // Our position on the function tree.
	branchNo     int             // The number of the branch in the function tree.
	argNo        int             // The number of the argument we're looking at.
	index        int             // The index we're looking at in the argument we're looking at.
	lengths      set.Set[int]    // The possible arities of the values of the argument we're looking at.
	maxLength    int             // The maximum of the 'lengths' set, or -1 if the set contains this.
	targetList   alternateType   // The possible types associated with this tree position.
	doneList     alternateType   // The types we've looked at up to and including those of the current branchNo.
	valLocs      []uint32        // The locations of the values evaluated from the arguments.
	types        finiteTupleType // The types of the values.
	outLoc       uint32          // Where we're going to put the output.
	env          *environment    // Associates variable names with memory locations
	tupleTime    bool            // Once we've taken a tuple path, we can discard values 'til we reach bling or run out.
	tok          token.Token     // For generating errors.
}

func (cp *Compiler) generateNewArgument(vm *Vm, b *bindle) alternateType {
	// Case (1) : we've used up all our arguments. In this case we should look in the function tree for a function call.
	if b.argNo >= len(b.types) {
		return cp.seekFunctionCall(vm, b)
	}
	// Case (2) : the argument is bling.
	if len(b.types[b.argNo].(alternateType)) == 1 {
		switch bl := (b.types[b.argNo].(alternateType)[0]).(type) {
		case blingType:
			return cp.seekBling(vm, b, bl.tag)
		}
	}
	// Case (3) : we're in tuple time.
	if b.tupleTime {
		newBindle := *b
		newBindle.argNo++
		return cp.generateNewArgument(vm, &newBindle)
	}
	// Case (4) : We aren't yet at the end of the list of arguments.
	newBindle := *b
	newBindle.index = 0
	return cp.generateFromTopBranchDown(vm, b)
}

func (cp *Compiler) generateFromTopBranchDown(vm *Vm, b *bindle) alternateType {
	newBindle := *b
	newBindle.branchNo = 0
	newBindle.targetList = typesAtIndex(b.types[b.argNo], b.index)
	newBindle.doneList = make(alternateType, 0, len(b.targetList))
	if newBindle.index == 0 {
		newBindle.lengths = lengths(newBindle.targetList)
		newBindle.maxLength = maxLengthsOrMinusOne(newBindle.lengths)
	}
	return cp.generateBranch(vm, &newBindle)
}

// We look at the current branch and see if its type can account for some, all, or none of the possibilities in the targetList.
// If the answer is "all", we can recurse on the next argument.
// If "none", then we can recurse on the next branch down.
// If "some", then we must generate a conditional where it recurses on the next argument for the types accepted by the branch
// and on the next branch for the unaccepted types.
// It may also be the run-off-the-end branch number, in which case we can generate an error.
func (cp *Compiler) generateBranch(vm *Vm, b *bindle) alternateType {
	typeError := cp.reserve(vm, ERROR, DUMMY)
	if b.tupleTime || b.branchNo < len(b.treePosition.Branch) && b.treePosition.Branch[b.branchNo].TypeName == "tuple" { // We can move on to the next argument.
		newBindle := *b
		newBindle.treePosition = b.treePosition.Branch[b.branchNo].Node
		newBindle.tupleTime = true
		newBindle.argNo++
		return cp.generateNewArgument(vm, &newBindle)
	}
	if b.branchNo >= len(b.treePosition.Branch) { // We've tried all the alternatives and have some left over.
		cp.emit(vm, asgm, b.outLoc, typeError)
		return simpleList(ERROR)
	}
	branch := b.treePosition.Branch[b.branchNo]
	acceptedTypes := cp.typeNameToTypeList[branch.TypeName]
	overlap := acceptedTypes.intersect(b.targetList)
	if len(overlap) == 0 { // We drew a blank.
		return cp.generateNextBranchDown(vm, b)
	}
	// If we've got this far, the current branch accepts at least some of our types. Now we need to do conditionals based on
	// whether this is some or all. But to generate the conditional we also need to know whether we might be looking at a mix of
	// single values and of 0th elements of tuples.
	newBindle := *b
	newBindle.doneList = newBindle.doneList.union(overlap)
	acceptedSingleTypes := make(alternateType, 0, len(overlap))
	if newBindle.index == 0 {
		for _, t := range overlap {
			switch t := t.(type) {
			case simpleType:
				acceptedSingleTypes = append(acceptedSingleTypes, t)
			}
		}
	}
	// So now the length of acceptedSingleTypes tells us whether some, none, or all of the ways to follow the branch involve single values,
	// whereas the length of doneList tells us whether we need to recurse on the next branch or not.

	needsOtherBranch := len(newBindle.doneList) != len(newBindle.targetList)
	branchBacktrack := vm.codeTop()
	if needsOtherBranch {
		// Then we need to generate a conditional. Which one exactly depends on whether we're looking at a single, a tuple, or both.
		switch len(acceptedSingleTypes) {
		case 0:
			cp.put(vm, idxT, b.valLocs[b.argNo], uint32(b.index))
			cp.emitTypeComparison(vm, branch.TypeName, vm.that(), DUMMY)
		case len(overlap):
			cp.emitTypeComparison(vm, branch.TypeName, b.valLocs[b.argNo], DUMMY)
		default:
			cp.emit(vm, qsnQ, b.valLocs[b.argNo], vm.codeTop()+3)
			cp.emitTypeComparison(vm, branch.TypeName, b.valLocs[b.argNo], DUMMY)
			cp.emit(vm, jmp, vm.codeTop()+3)
			cp.put(vm, idxT, b.valLocs[b.argNo], uint32(b.index))
			cp.emitTypeComparison(vm, branch.TypeName, vm.that(), DUMMY)
		}
	}
	// Now we're in the 'if' part of the 'if-else'. We can recurse along the branch.
	// If we know whether we're looking at a single or a tuple, we can erase this and act accordingly, otherwise we generate a conditional.
	var typesFromGoingAcross, typesFromGoingDown alternateType
	switch len(acceptedSingleTypes) {
	case 0:
		typesFromGoingAcross = cp.generateMoveAlongBranchViaTupleElement(vm, &newBindle)
	case len(overlap):
		typesFromGoingAcross = cp.generateMoveAlongBranchViaSingleValue(vm, &newBindle)
	default:
		backtrack := vm.codeTop()
		cp.emit(vm, qsnQ, b.valLocs[b.argNo], DUMMY)
		typesFromSingles := cp.generateMoveAlongBranchViaSingleValue(vm, &newBindle)
		cp.emit(vm, jmp, DUMMY)
		vm.code[backtrack].makeLastArg(vm.codeTop())
		backtrack = vm.codeTop()
		typesFromTuples := cp.generateMoveAlongBranchViaTupleElement(vm, &newBindle)
		vm.code[backtrack].makeLastArg(vm.codeTop())
		typesFromGoingAcross = typesFromSingles.union(typesFromTuples)
	}
	// And now we need to do the 'else' branch if there is one.
	if needsOtherBranch {
		elseBacktrack := vm.codeTop()
		cp.emit(vm, jmp, DUMMY) // The last part of the 'if' branch: jumps over the 'else'.
		// We need to backtrack on whatever conditional we generated.
		switch len(acceptedSingleTypes) {
		case 0:
			vm.code[branchBacktrack+1].makeLastArg(vm.codeTop())
		case len(overlap):
			vm.code[branchBacktrack].makeLastArg(vm.codeTop())
		default:
			vm.code[branchBacktrack+1].makeLastArg(vm.codeTop())
			vm.code[branchBacktrack+4].makeLastArg(vm.codeTop())
		}
		// We recurse on the next branch down.
		typesFromGoingDown = cp.generateNextBranchDown(vm, &newBindle)
		vm.code[elseBacktrack].makeLastArg(vm.codeTop())
	}
	return typesFromGoingAcross.union(typesFromGoingDown)
}

var TYPE_COMPARISONS = map[string]*operation{
	"int":     {qtyp, []uint32{DUMMY, uint32(INT), DUMMY}},
	"string":  {qtyp, []uint32{DUMMY, uint32(STRING), DUMMY}},
	"bool":    {qtyp, []uint32{DUMMY, uint32(BOOL), DUMMY}},
	"float64": {qtyp, []uint32{DUMMY, uint32(FLOAT), DUMMY}},
	"null":    {qtyp, []uint32{DUMMY, uint32(NULL), DUMMY}},
	"single":  {qsng, []uint32{DUMMY, DUMMY}},
	"single?": {qsnQ, []uint32{DUMMY, DUMMY}},
}

func (cp *Compiler) emitTypeComparison(vm *Vm, typeAsString string, mem, loc uint32) {
	op, ok := TYPE_COMPARISONS[typeAsString]
	if ok {
		newArgs := make([]uint32, len(op.args))
		copy(newArgs, op.args)
		newOp := &operation{op.opcode, newArgs}
		newOp.args[0] = mem
		newOp.makeLastArg(loc)
		cp.emit(vm, newOp.opcode, newArgs...)
		return
	}
	panic("Unknown type: " + typeAsString)
}

func (cp *Compiler) generateMoveAlongBranchViaTupleElement(vm *Vm, b *bindle) alternateType {
	// We may definitely have run off the end of all the potential tuples.
	if b.index+1 == b.maxLength {
		newBindle := *b
		newBindle.argNo++
		return cp.generateNewArgument(vm, &newBindle)
	}
	newBindle := *b
	newBindle.index++
	newBindle.treePosition = newBindle.treePosition.Branch[newBindle.branchNo].Node
	// We may have to generate an if-then-else to do a length check on the tuple.
	var typesFromNextArgument alternateType
	needsConditional := b.maxLength == -1 || // Then there's a non-finite tuple
		b.lengths.Contains(newBindle.index) // Then we may have run off the end of a finite tuple.
	backtrack1 := vm.codeTop()
	var backtrack2 uint32
	if needsConditional {
		cp.emit(vm, qlnT, b.valLocs[newBindle.argNo], uint32(newBindle.index), DUMMY)
		newArgumentBindle := newBindle
		newArgumentBindle.argNo++
		typesFromNextArgument = cp.generateNewArgument(vm, &newArgumentBindle)
		backtrack2 = vm.codeTop()
		cp.emit(vm, jmp, DUMMY)
		vm.code[backtrack1].args[2] = vm.codeTop()
	}

	typesFromContinuingInTuple := cp.generateFromTopBranchDown(vm, &newBindle)

	if needsConditional {
		vm.code[backtrack2].args[0] = backtrack2
	}

	return typesFromContinuingInTuple.union(typesFromNextArgument)
}

func (cp *Compiler) generateMoveAlongBranchViaSingleValue(vm *Vm, b *bindle) alternateType {
	newBindle := *b
	newBindle.treePosition = b.treePosition.Branch[b.branchNo].Node
	newBindle.argNo++
	return cp.generateNewArgument(vm, &newBindle)
}

func (cp *Compiler) generateNextBranchDown(vm *Vm, b *bindle) alternateType {
	newBindle := *b
	newBindle.branchNo++
	return cp.generateBranch(vm, &newBindle)
}

func (cp *Compiler) seekFunctionCall(vm *Vm, b *bindle) alternateType {
	for _, branch := range b.treePosition.Branch { // TODO --- this is a pretty vile hack; it would make sense for it to always be at the top.}
		if branch.Node.Fn != nil {
			fNo := branch.Node.Fn.Number
			functionAndType, ok := BUILTINS[cp.fns[fNo].builtin]
			if ok {
				if cp.fns[fNo].builtin == "tuple_of_single?" {
					functionAndType.t = alternateType{finiteTupleType{b.types[0]}}
				}
				if cp.fns[fNo].builtin == "tuple_of_tuple" {
					// TODO --- hack this.
				}
				functionAndType.f(cp, vm, b.outLoc, b.valLocs)
				return functionAndType.t
			}
			cp.emitFunctionCall(vm, fNo, b.valLocs)
			cp.emit(vm, asgm, b.outLoc, cp.fns[fNo].outReg) // Because the different implementations of the function will have their own out register.
			return cp.fns[fNo].types                        // TODO : There is no reason why this should be so.
		}
	}
	cp.emit(vm, asgm, b.outLoc, cp.reserve(vm, ERROR, DUMMY))
	return simpleList(ERROR)
}

func (cp *Compiler) seekBling(vm *Vm, b *bindle, bling string) alternateType {
	for i, branch := range b.treePosition.Branch {
		if branch.TypeName == bling {
			newBindle := *b
			newBindle.branchNo = i
			newBindle.tupleTime = false
			return cp.generateMoveAlongBranchViaSingleValue(vm, &newBindle)
		}
	}
	cp.p.Throw("comp/eq/err/a", b.tok) // TODO -- the bindle should pass all the original args or at least their tokens for better error messages.
	return simpleList(ERROR)
}

// This supplies us with a Vm which shares its memory with the target vm in the compiler, but which has its own code store. The
// contents of the store after compiling to the temporary target can be added to the main vm using the .add method below.
func (cp *Compiler) tempVm() *Vm {
	vm := *cp.vm
	vm.code = []*operation{}
	return &vm
}

func (cp *Compiler) addCode(vm *Vm) {
	cp.vm.add(vm)
}

// We have two different ways of emiting an opcode: 'emit' does it the regular way, 'put' ensures that
// the destination is the next free memory address.
func (cp *Compiler) emit(vm *Vm, opcode opcode, args ...uint32) {
	vm.code = append(vm.code, makeOp(opcode, args...))
	if SHOW_COMPILE {
		println(describe(vm.code[len(vm.code)-1]))
	}
}

func (cp *Compiler) put(vm *Vm, opcode opcode, args ...uint32) {
	args = append([]uint32{vm.memTop()}, args...)
	cp.emit(vm, opcode, args...)
	vm.mem = append(vm.mem, Value{})
}

// Reput puts the value in the last memory address to be used.
func (cp *Compiler) reput(vm *Vm, opcode opcode, args ...uint32) {
	args = append([]uint32{vm.that()}, args...)
	cp.emit(vm, opcode, args...)
}

func (cp *Compiler) emitFunctionCall(vm *Vm, funcNumber uint32, valLocs []uint32) {
	args := append([]uint32{cp.fns[funcNumber].callTo, cp.fns[funcNumber].loReg, cp.fns[funcNumber].hiReg}, valLocs...)
	cp.emit(vm, call, args...)
}

func (cp *Compiler) emitEquals(vm *Vm, node *ast.InfixExpression, env *environment) (alternateType, bool) {
	lTypes, lcst := cp.compileNode(vm, node.Args[0], env)
	if lTypes.only(ERROR) {
		cp.p.Throw("comp/eq/err/a", node.Token)
		return simpleList(ERROR), true
	}
	leftRg := vm.that()
	rTypes, rcst := cp.compileNode(vm, node.Args[2], env)
	if rTypes.only(ERROR) {
		cp.p.Throw("comp/eq/err/b", node.Token)
		return simpleList(ERROR), true
	}
	rightRg := vm.that()
	oL := lTypes.intersect(rTypes)
	if oL.only(ERROR) {
		cp.p.Throw("comp/eq/err/c", node.Token)
		return simpleList(ERROR), true
	}
	if len(oL) == 0 {
		cp.p.Throw("comp/eq/types", node.Token)
		return simpleList(ERROR), true
	}
	if len(oL) == 1 && len(lTypes) == 1 && len(rTypes) == 1 {
		switch el := oL[0].(type) {
		case simpleType:
			switch el {
			case INT:
				cp.put(vm, equi, leftRg, rightRg)
			case STRING:
				cp.put(vm, equs, leftRg, rightRg)
			case BOOL:
				cp.put(vm, equb, leftRg, rightRg)
			case FLOAT:
				cp.put(vm, equf, leftRg, rightRg)
			default:
				panic("Unimplemented comparison type.")
			}
			return simpleList(BOOL), lcst && rcst
		default:
			panic("Unimplemented comparison type.")
		}
	} else {
		panic("Haven't implemented this bit because of having no way to test it at this point.")
	}
}
