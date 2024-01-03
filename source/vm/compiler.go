package vm

import (
	"charm/source/ast"
	"charm/source/parser"
	"charm/source/set"
	"charm/source/token"
)

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
	functionForest     map[string]*fnTreeNode
	typeNameToTypeList map[string]alternateType
}

type cpFunc struct {
	callTo  uint32
	loReg   uint32
	hiReg   uint32
	outReg  uint32
	types   alternateType
	builtin string // A non-empty string in case it is a builtin.
}

func (cp *Compiler) memTop() uint32 {
	return uint32(len(cp.vm.mem))
}

func (cp *Compiler) that() uint32 {
	return uint32(len(cp.vm.mem) - 1)
}

func (cp *Compiler) codeTop() uint32 {
	return uint32(len(cp.vm.code))
}

func (cp *Compiler) next() uint32 {
	return uint32(len(cp.vm.code))
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
			"int":     {INT},
			"string":  {STRING},
			"bool":    {BOOL},
			"float64": {FLOAT},
			"error":   {ERROR},
			"single":  {INT, BOOL, STRING, FLOAT},
		},
	}
}

func (cp *Compiler) Run() {
	cp.vm.Run(0)
}

func (cp *Compiler) GetParser() *parser.Parser {
	return cp.p
}

const SHOW_BYTECODE = false

func (cp *Compiler) Do(line string) string {
	mT := cp.memTop()
	cT := cp.codeTop()
	node := cp.p.ParseLine("REPL input", line)
	if cp.p.ErrorsExist() {
		return ""
	}
	cp.compileNode(node, cp.gvars)
	if cp.p.ErrorsExist() {
		return ""
	}
	cp.emit(ret)
	if SHOW_BYTECODE {
		print("\nBytecode:\n\n")
		for i := cT; i < cp.codeTop(); i++ {
			println(cp.vm.describeCode(i))
		}
	}
	cp.vm.Run(cT)
	result := cp.vm.mem[cp.that()]
	cp.vm.mem = cp.vm.mem[:mT]
	cp.vm.code = cp.vm.code[:cT]
	return result.describe()
}

func (cp *Compiler) Compile(source, sourcecode string) {
	if SHOW_COMPILE {
		print("\nCompiling\n\n")
	}
	cp.vm = blankVm()
	node := cp.p.ParseLine(source, sourcecode)
	cp.compileNode(node, cp.gvars)
	cp.emit(ret)
}

func (cp *Compiler) reserve(t simpleType, v any) uint32 {
	cp.vm.mem = append(cp.vm.mem, Value{T: t, V: v})
	return uint32(len(cp.vm.mem) - 1)
}

func (cp *Compiler) addVariable(env *environment, name string, acc varAccess, types alternateType) {
	env.data[name] = variable{mLoc: cp.that(), access: acc, types: types}
}

func (cp *Compiler) compileNode(node ast.Node, env *environment) alternateType {
	switch node := node.(type) {
	case *ast.IntegerLiteral:
		cp.reserve(INT, node.Value)
		return simpleList(INT)
	case *ast.StringLiteral:
		cp.reserve(STRING, node.Value)
		return simpleList(STRING)
	case *ast.BooleanLiteral:
		cp.reserve(BOOL, node.Value)
		return simpleList(BOOL)
	case *ast.FloatLiteral:
		cp.reserve(FLOAT, node.Value)
		return simpleList(FLOAT)
	case *ast.InfixExpression:
		if cp.p.Infixes.Contains(node.Operator) {
			return cp.createInfixCall(node, env)
		}
		if node.Operator == "==" {
			return cp.emitEquals(node, env)
		}
		if node.Operator == "!=" {
			types := cp.emitEquals(node, env)
			cp.put(notb, cp.that())
			return types
		}
		cp.p.Throw("comp/infix", node.Token)
		return simpleList(ERROR)
	case *ast.LazyInfixExpression:
		if node.Operator == "or" {
			lTypes := cp.compileNode(node.Left, env)
			if !lTypes.contains(BOOL) {
				cp.p.Throw("comp/or/bool/left", node.Token)
				return simpleList(ERROR)
			}
			leftRg := cp.that()
			cp.emit(qtru, leftRg, cp.next()+2)
			backtrack := cp.next()
			cp.emit(jmp, DUMMY)
			rTypes := cp.compileNode(node.Right, env)
			if !rTypes.contains(BOOL) {
				cp.p.Throw("comp/or/bool/right", node.Token)
				return simpleList(ERROR)
			}
			rightRg := cp.that()
			cp.vm.code[backtrack].args[0] = cp.next()
			cp.put(orb, leftRg, rightRg)
			return simpleList(BOOL)
		}
		if node.Operator == "and" {
			lTypes := cp.compileNode(node.Left, env)
			if !lTypes.contains(BOOL) {
				cp.p.Throw("comp/and/bool/left", node.Token)
				return simpleList(ERROR)
			}
			leftRg := cp.that()
			backtrack := cp.next()
			cp.emit(qtru, leftRg, DUMMY)
			rTypes := cp.compileNode(node.Right, env)
			if !rTypes.contains(BOOL) {
				cp.p.Throw("comp/and/bool/right", node.Token)
				return simpleList(ERROR)
			}
			rightRg := cp.that()
			cp.vm.code[backtrack].args[1] = cp.next()
			cp.put(andb, leftRg, rightRg)
			return simpleList(BOOL)
		}
		if node.Operator == ":" {
			if node.Left.GetToken().Type == token.ELSE {
				return cp.compileNode(node.Right, env)
			}
			lTypes := cp.compileNode(node.Left, env)
			if !lTypes.contains(BOOL) {
				cp.p.Throw("comp/cond/bool", node.Token)
				return simpleList(ERROR)
			}
			leftRg := cp.that()
			backtrack := cp.next()
			cp.emit(qtru, leftRg, DUMMY)
			rTypes := cp.compileNode(node.Right, env)
			cp.put(asgm, cp.that())
			cp.emit(jmp, cp.next()+2)
			cp.vm.code[backtrack].args[1] = cp.next()
			cp.reput(asgm, C_U_OBJ)
			return rTypes.union(simpleList(UNSAT))
		}
		if node.Operator == ";" {
			lTypes := cp.compileNode(node.Left, env)
			// We deal with the case where the newline is separating local constant definitions
			// in the 'given' block.
			if lTypes.only(CREATED_LOCAL_CONSTANT) {
				cp.compileNode(node.Right, env)
				return simpleList(CREATED_LOCAL_CONSTANT)
			}
			leftRg := cp.that()
			backtrack := cp.next()
			cp.emit(qtyp, leftRg, uint32(UNSAT), DUMMY)
			rTypes := cp.compileNode(node.Right, env)
			rightRg := cp.that()
			cp.put(asgm, rightRg)
			cp.emit(jmp, cp.next()+2)
			cp.vm.code[backtrack].args[2] = cp.next()
			cp.reput(asgm, leftRg)
			if !(lTypes.contains(UNSAT) && rTypes.contains(UNSAT)) {
				return lTypes.union(rTypes).without(UNSAT)
			}
			return lTypes.union(rTypes)
		}
		panic("Unimplemented lazy infix.")
	case *ast.Identifier:
		v, ok := env.getVar(node.Value)
		if ok {
			if v.access == LOCAL_CONSTANT_THUNK {
				cp.emit(untk, v.mLoc)
			}
			cp.put(asgm, v.mLoc)
			return v.types
		}
		cp.p.Throw("comp/ident/known", node.Token)
		return simpleList(ERROR)
	case *ast.AssignmentExpression:
		if node.Token.Type == token.GVN_ASSIGN {
			// TODO --- need to do this better after we implement tuples
			if node.Left.GetToken().Type != token.IDENT {
				cp.p.Throw("comp/assign/ident", node.Left.GetToken())
				return simpleList(ERROR)
			}
			thunkStart := cp.next()
			types := cp.compileNode(node.Right, env)
			cp.emit(ret)
			cp.addVariable(env, node.Left.(*ast.Identifier).Value, LOCAL_CONSTANT_THUNK, types)
			cp.thunkList = append(cp.thunkList, thunk{cp.that(), thunkStart})
		}
		cp.p.Throw("comp/assign", node.Token)
		return simpleList(ERROR)
	case *ast.PrefixExpression:
		if node.Operator == "not" {
			allTypes := cp.compileNode(node.Args[0], env)
			if allTypes.only(BOOL) {
				cp.put(notb, cp.that())
				return simpleList(BOOL)
			}
			if !allTypes.contains(BOOL) {
				cp.p.Throw("comp/not/bool", node.Token)
				return simpleList(ERROR)
			}
			panic("Haven't implemented this bit because of having no way to test it at this point.")
		}
		if cp.p.Prefixes.Contains(node.Operator) || cp.p.Functions.Contains(node.Operator) {
			return cp.createFunctionCall(node, env)
		}
		cp.p.Throw("comp/prefix/known", node.Token)
		return simpleList(ERROR)
	default:
		panic("Unimplemented node type.")
	}
}

func (cp *Compiler) createInfixCall(node *ast.InfixExpression, env *environment) alternateType {
	b := &bindle{tok: node.Token,
		treePosition: cp.p.FunctionTreeMap[node.Operator],
		outLoc:       cp.reserve(ERROR, DUMMY),
		env:          env,
		valLocs:      make([]uint32, len(node.Args)),
		types:        make(finiteTupleType, len(node.Args)),
	}
	for i, arg := range node.Args {
		switch arg := arg.(type) {
		case *ast.Bling:
			b.types[i] = alternateType{blingType{arg.Value}}
		default:
			b.types[i] = cp.compileNode(arg, env)
			b.valLocs[i] = cp.that()
		}
	}
	returnTypes := cp.generateNewArgument(b)
	cp.put(asgm, b.outLoc)
	if returnTypes.only(ERROR) {
		cp.p.Throw("comp/call", b.tok)
	}
	return returnTypes
}

func (cp *Compiler) createFunctionCall(node *ast.PrefixExpression, env *environment) alternateType {
	b := &bindle{tok: node.Token,
		treePosition: cp.p.FunctionTreeMap[node.Operator],
		outLoc:       cp.reserve(ERROR, DUMMY),
		env:          env,
		valLocs:      make([]uint32, len(node.Args)),
		types:        make(finiteTupleType, len(node.Args)),
	}
	for i, arg := range node.Args {
		switch arg := arg.(type) {
		case *ast.Bling:
			b.types[i] = alternateType{blingType{arg.Value}}
		default:
			b.types[i] = cp.compileNode(arg, env)
			b.valLocs[i] = cp.that()
		}
	}
	returnTypes := cp.generateNewArgument(b)
	cp.put(asgm, b.outLoc)
	if returnTypes.only(ERROR) {
		cp.p.Throw("comp/call", b.tok)
	}
	return returnTypes
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

func (cp *Compiler) generateNewArgument(b *bindle) alternateType {
	// Case (1) : we've used up all our arguments. In this case we should look in the function tree for a function call.
	if b.argNo >= len(b.types) {
		return cp.seekFunctionCall(b)
	}
	// Case (2) : the argument is bling.
	if len(b.types[b.argNo].(alternateType)) == 1 {
		switch bl := (b.types[b.argNo].(alternateType)[0]).(type) {
		case blingType:
			return cp.seekBling(b, bl.tag)
		}
	}
	// Case (3) : We aren't yet at the end of the list of arguments.
	newBindle := *b
	newBindle.index = 0
	return cp.generateFromTopBranchDown(b)
}

func (cp *Compiler) generateFromTopBranchDown(b *bindle) alternateType {
	newBindle := *b
	newBindle.branchNo = 0
	newBindle.targetList = typesAtIndex(b.types[b.argNo], b.index)
	newBindle.doneList = make(alternateType, 0, len(b.targetList))
	if newBindle.index == 0 {
		newBindle.lengths = lengths(newBindle.targetList)
		newBindle.maxLength = maxLengthsOrMinusOne(newBindle.lengths)
	}
	return cp.generateBranch(&newBindle)
}

// We look at the current branch and see if its type can account for some, all, or none of the possibilities in the targetList.
// If the answer is "all", we can recurse on the next argument.
// If "none", then we can recurse on the next branch down.
// If "some", then we must generate a conditional where it recurses on the next argument for the types accepted by the branch
// and on the next branch for the unaccepted types.
// It may also be the run-off-the-end branch number, in which case we can generate an error.
func (cp *Compiler) generateBranch(b *bindle) alternateType {
	typeError := cp.reserve(ERROR, DUMMY)
	if b.tupleTime { // We can move on to the next argument.
		newBindle := *b
		newBindle.argNo++
		return cp.generateNewArgument(&newBindle)
	}
	if b.branchNo >= len(b.treePosition.Branch) { // We've tried all the alternatives and have some left over.
		cp.emit(asgm, b.outLoc, typeError)
		return simpleList(ERROR)
	}
	branch := b.treePosition.Branch[b.branchNo]
	acceptedTypes := cp.typeNameToTypeList[branch.TypeName]
	overlap := acceptedTypes.intersect(b.targetList)
	if len(overlap) == 0 { // We drew a blank.
		return cp.generateNextBranchDown(b)
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
	branchBacktrack := cp.codeTop()
	if needsOtherBranch {
		// Then we need to generate a conditional. Which one exactly depends on whether we're looking at a single, a tuple, or both.
		switch len(acceptedSingleTypes) {
		case 0:
			cp.put(idxT, b.valLocs[b.argNo], uint32(b.index))
			cp.emitTypeComparison(branch.TypeName, cp.that(), DUMMY)
		case len(overlap):
			cp.emitTypeComparison(branch.TypeName, b.valLocs[b.argNo], DUMMY)
		default:
			cp.emit(qsnQ, b.valLocs[b.argNo], cp.codeTop()+3)
			cp.emitTypeComparison(branch.TypeName, b.valLocs[b.argNo], DUMMY)
			cp.emit(jmp, cp.codeTop()+3)
			cp.put(idxT, b.valLocs[b.argNo], uint32(b.index))
			cp.emitTypeComparison(branch.TypeName, cp.that(), DUMMY)
		}
	}
	// Now we're in the 'if' part of the 'if-else'. We can recurse along the branch.
	// If we know whether we're looking at a single or a tuple, we can erase this and act accordingly, otherwise we generate a conditional.
	var typesFromGoingAcross, typesFromGoingDown alternateType
	switch len(acceptedSingleTypes) {
	case 0:
		typesFromGoingAcross = cp.generateMoveAlongBranchViaTupleElement(&newBindle)
	case len(overlap):
		typesFromGoingAcross = cp.generateMoveAlongBranchViaSingleValue(&newBindle)
	default:
		backtrack := cp.codeTop()
		cp.emit(qsnQ, b.valLocs[b.argNo], DUMMY)
		typesFromSingles := cp.generateMoveAlongBranchViaSingleValue(&newBindle)
		cp.emit(jmp, DUMMY)
		cp.vm.code[backtrack].args[2] = cp.codeTop()
		backtrack = cp.codeTop()
		typesFromTuples := cp.generateMoveAlongBranchViaTupleElement(&newBindle)
		cp.vm.code[backtrack].args[1] = cp.codeTop()
		typesFromGoingAcross = typesFromSingles.union(typesFromTuples)
	}
	// And now we need to do the 'else' branch if there is one.
	if needsOtherBranch {
		elseBacktrack := cp.codeTop()
		cp.emit(jmp, DUMMY) // The last part of the 'if' branch: jumps over the 'else'.
		// We need to backtrack on whatever conditional we generated.
		switch len(acceptedSingleTypes) {
		case 0:
			cp.vm.code[branchBacktrack+1].makeLastArg(cp.codeTop())
		case len(overlap):
			cp.vm.code[branchBacktrack].makeLastArg(cp.codeTop())
		default:
			cp.vm.code[branchBacktrack+1].makeLastArg(cp.codeTop())
			cp.vm.code[branchBacktrack+4].makeLastArg(cp.codeTop())
		}
		// We recurse on the next branch down.
		typesFromGoingDown = cp.generateNextBranchDown(&newBindle)
		cp.vm.code[elseBacktrack].makeLastArg(cp.codeTop())
	}

	return typesFromGoingAcross.union(typesFromGoingDown)
}

var TYPE_COMPARISONS = map[string]operation{
	"int":     {qtyp, []uint32{DUMMY, uint32(INT), DUMMY}},
	"string":  {qtyp, []uint32{DUMMY, uint32(STRING), DUMMY}},
	"bool":    {qtyp, []uint32{DUMMY, uint32(BOOL), DUMMY}},
	"float64": {qtyp, []uint32{DUMMY, uint32(FLOAT), DUMMY}},
	"null":    {qtyp, []uint32{DUMMY, uint32(NULL), DUMMY}},
	"single":  {qsng, []uint32{DUMMY, DUMMY}},
	"single?": {qsnQ, []uint32{DUMMY, DUMMY}},
}

func (cp *Compiler) emitTypeComparison(typeAsString string, mem, loc uint32) {
	op, ok := TYPE_COMPARISONS[typeAsString]
	if ok {
		op.args[0] = mem
		op.makeLastArg(loc)
		cp.emit(op.opcode, op.args...)
		return
	}
	panic("Unknown type: " + typeAsString)
}

func (cp *Compiler) generateMoveAlongBranchViaTupleElement(b *bindle) alternateType {
	// We may definitely have run off the end of all the potential tuples.
	if b.index+1 == b.maxLength {
		newBindle := *b
		newBindle.argNo++
		return cp.generateNewArgument(&newBindle)
	}
	newBindle := *b
	newBindle.index++
	newBindle.treePosition = newBindle.treePosition.Branch[newBindle.branchNo].Node
	// We may have to generate an if-then-else to do a length check on the tuple.
	var typesFromNextArgument alternateType
	needsConditional := b.maxLength == -1 || // Then there's a non-finite tuple
		b.lengths.Contains(newBindle.index) // Then we may have run off the end of a finite tuple.
	backtrack1 := cp.codeTop()
	var backtrack2 uint32
	if needsConditional {
		cp.emit(qlnT, b.valLocs[newBindle.argNo], uint32(newBindle.index), DUMMY)
		newArgumentBindle := newBindle
		newArgumentBindle.argNo++
		typesFromNextArgument = cp.generateNewArgument(&newArgumentBindle)
		backtrack2 = cp.codeTop()
		cp.emit(jmp, DUMMY)
		cp.vm.code[backtrack1].args[2] = cp.codeTop()
	}

	typesFromContinuingInTuple := cp.generateFromTopBranchDown(&newBindle)

	if needsConditional {
		cp.vm.code[backtrack2].args[0] = backtrack2
	}

	return typesFromContinuingInTuple.union(typesFromNextArgument)
}

func (cp *Compiler) generateMoveAlongBranchViaSingleValue(b *bindle) alternateType {
	newBindle := *b
	newBindle.treePosition = b.treePosition.Branch[b.branchNo].Node
	newBindle.argNo++
	return cp.generateNewArgument(&newBindle)
}

func (cp *Compiler) generateNextBranchDown(b *bindle) alternateType {
	newBindle := *b
	newBindle.branchNo++
	return cp.generateBranch(&newBindle)
}

func (cp *Compiler) seekFunctionCall(b *bindle) alternateType {
	for _, branch := range b.treePosition.Branch { // TODO --- this is a pretty vile hack; it would make sense for it to always be at the top.}
		if branch.Node.Fn != nil {
			fNo := branch.Node.Fn.Number
			functionAndType, ok := BUILTINS[cp.fns[fNo].builtin]
			if ok {
				functionAndType.f(cp, b.outLoc, b.valLocs)
				return functionAndType.t
			}
			cp.emitFunctionCall(fNo, b.valLocs)
			cp.emit(asgm, b.outLoc, cp.fns[fNo].outReg) // Because the different implementations of the function will have their own out register.
			return cp.fns[fNo].types                    // TODO : There is no reason why this should be so.
		}
	}
	cp.emit(asgm, b.outLoc, cp.reserve(ERROR, DUMMY))
	return simpleList(ERROR)
}

func (cp *Compiler) seekBling(b *bindle, bling string) alternateType {
	for i, branch := range b.treePosition.Branch {
		if branch.TypeName == bling {
			newBindle := *b
			newBindle.branchNo = i
			return cp.generateMoveAlongBranchViaSingleValue(&newBindle)
		}
	}
	cp.p.Throw("comp/eq/err/a", b.tok) // TODO -- the bindle should pass all the original args or at least their tokens for better error messages.
	return simpleList(ERROR)
}

const SHOW_COMPILE = true

// We have two different ways of emiting an opcode: 'emit' does it the regular way, 'put' ensures that
// the destination is the next free memory address.
func (cp *Compiler) emit(opcode opcode, args ...uint32) {
	cp.vm.code = append(cp.vm.code, makeOp(opcode, args...))
	if SHOW_COMPILE {
		println(describe(cp.vm.code[len(cp.vm.code)-1]))
	}
}

func (cp *Compiler) put(opcode opcode, args ...uint32) {
	args = append([]uint32{cp.memTop()}, args...)
	cp.emit(opcode, args...)
	cp.vm.mem = append(cp.vm.mem, Value{})
}

// Reput puts the value in the last memory address to be used.
func (cp *Compiler) reput(opcode opcode, args ...uint32) {
	args = append([]uint32{cp.that()}, args...)
	cp.emit(opcode, args...)
}

func (cp *Compiler) emitFunctionCall(funcNumber uint32, valLocs []uint32) {
	args := append([]uint32{cp.fns[funcNumber].callTo, cp.fns[funcNumber].loReg, cp.fns[funcNumber].hiReg}, valLocs...)
	cp.emit(call, args...)
}

func (cp *Compiler) emitEquals(node *ast.InfixExpression, env *environment) alternateType {
	lTypes := cp.compileNode(node.Args[0], env)
	if lTypes.only(ERROR) {
		cp.p.Throw("comp/eq/err/a", node.Token)
		return simpleList(ERROR)
	}
	leftRg := cp.that()
	rTypes := cp.compileNode(node.Args[2], env)
	if rTypes.only(ERROR) {
		cp.p.Throw("comp/eq/err/b", node.Token)
		return simpleList(ERROR)
	}
	rightRg := cp.that()
	oL := lTypes.intersect(rTypes)
	if oL.only(ERROR) {
		cp.p.Throw("comp/eq/err/c", node.Token)
		return simpleList(ERROR)
	}
	if len(oL) == 0 {
		cp.p.Throw("comp/eq/types", node.Token)
		return simpleList(ERROR)
	}
	if len(oL) == 1 && len(lTypes) == 1 && len(rTypes) == 1 {
		switch el := oL[0].(type) {
		case simpleType:
			switch el {
			case INT:
				cp.put(equi, leftRg, rightRg)
			case STRING:
				cp.put(equs, leftRg, rightRg)
			case BOOL:
				cp.put(equb, leftRg, rightRg)
			case FLOAT:
				cp.put(equf, leftRg, rightRg)
			default:
				panic("Unimplemented comparison type.")
			}
			return simpleList(BOOL)
		default:
			panic("Unimplemented comparison type.")
		}
	} else {
		panic("Haven't implemented this bit because of having no way to test it at this point.")
	}
}
