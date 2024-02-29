package vm

import (
	"pipefish/source/ast"
	"pipefish/source/object"
	"pipefish/source/parser"
	"pipefish/source/set"
	"pipefish/source/token"
)

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

type Compiler struct {
	p                  *parser.Parser
	vm                 *Vm
	enums              map[string]enumOrdinates
	gconsts            *environment
	gvars              *environment
	fns                []*cpFunc
	typeNameToTypeList map[string]alternateType

	tupleType uint32 // Location of a constant saying {TYPE, <type number of tuples>}

	// Very temporary state. Arguably shouldn't be here.
	thunkList []thunk
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

func (cp *Compiler) Do(line string) Value {
	mT := cp.vm.memTop()
	cT := cp.vm.codeTop()
	node := cp.p.ParseLine("REPL input", line)
	if cp.p.ErrorsExist() {
		return Value{T: ERROR}
	}
	cp.compileNode(cp.vm, node, cp.gvars)
	if cp.p.ErrorsExist() {
		return Value{T: ERROR}
	}
	cp.emit(cp.vm, ret)
	cp.vm.Run(cT)
	result := cp.vm.mem[cp.vm.that()]
	cp.vm.mem = cp.vm.mem[:mT]
	cp.vm.code = cp.vm.code[:cT]
	return result
}

func (cp *Compiler) Describe(v Value) string {
	return cp.vm.literal(v)
}

func (cp *Compiler) Compile(source, sourcecode string) {
	cp.vm = blankVm()
	node := cp.p.ParseLine(source, sourcecode)
	cp.compileNode(cp.vm, node, cp.gvars)
	cp.emit(cp.vm, ret)
}

func (cp *Compiler) reserve(vm *Vm, t simpleType, v any) uint32 {
	vm.mem = append(vm.mem, Value{T: t, V: v})
	return uint32(len(vm.mem) - 1)
}

func (cp *Compiler) reserveError(vm *Vm, ec string, tok *token.Token, args []any) uint32 {
	vm.mem = append(vm.mem, Value{T: ERROR, V: &object.Error{ErrorId: ec, Token: tok, Args: append([]any{vm}, args...), Trace: make([]*token.Token, 0, 10)}})
	return uint32(len(vm.mem) - 1)
}

func (cp *Compiler) reserveToken(vm *Vm, tok *token.Token) uint32 {
	vm.tokens = append(vm.tokens, tok)
	return uint32(len(vm.tokens) - 1)
}

func (cp *Compiler) reserveLambdaFactory(vm *Vm, env *environment, fnNode *ast.FuncExpression, tok *token.Token) (uint32, bool) {
	LF := &lambdaFactory{model: &lambda{}}
	LF.model.vm = blankVm()
	LF.model.vm.code = []*operation{}
	newEnv := newEnvironment()
	sig := fnNode.Sig
	// First, we're going to find all (if any) variables/constants declared outside of the function: anything we're closing over.
	// We do this by a process of elimination: find the variables declared in the function parameters and the given block,
	// then subtract them from the identifiers in the main body of the function.

	// We get the function parameters.
	params := set.Set[string]{}
	for _, pair := range sig {
		params.Add(pair.VarName)
	}
	// We find all the identifiers that we declare in the 'given' block.
	locals := ast.GetLhsOfAssignments(fnNode.Given)
	// Find all the variable names in the body.
	bodyNames := ast.GetVariableNames(fnNode.Body)
	externals := bodyNames.SubtractSet(params).SubtractSet(locals) // I.e. the "externals" things, if any, that we're closing over.

	// Copy the externals to the environment.
	for k := range externals {
		v, ok := env.getVar(k)
		if !ok {
			cp.p.Throw("comp/body/known", tok)
		}
		cp.reserve(LF.model.vm, INT, DUMMY) // It doesn't matter what we put in here 'cos we copy the values any time we call the LambdaFactory.
		cp.addVariable(LF.model.vm, newEnv, k, v.access, v.types)
		// At the same time, the lambda factory need to know where they are in the calling Vm.
		LF.extMem = append(LF.extMem, v.mLoc)
	}

	LF.model.extTop = LF.model.vm.memTop()

	// Add the function parameters.
	for _, pair := range sig { // It doesn't matter what we put in here either, because we're going to have to copy the values any time we call the function.
		cp.reserve(LF.model.vm, INT, DUMMY)
		cp.addVariable(LF.model.vm, newEnv, pair.VarName, FUNCTION_ARGUMENT, cp.typeNameToTypeList[pair.VarType])
	}

	LF.model.prmTop = LF.model.vm.memTop()

	// Compile the locals.

	if fnNode.Given != nil {
		cp.thunkList = []thunk{}
		cp.compileNode(LF.model.vm, fnNode.Given, newEnv)
		for _, pair := range cp.thunkList {
			cp.emit(LF.model.vm, thnk, pair.mLoc, pair.cLoc)
		}
	}

	// Function starts here.

	LF.model.locToCall = LF.model.vm.codeTop()

	// We have to typecheck inside the lambda, because the calling site doesn't know which function it's calling.

	// TODO !!!

	// Now we can emit the main body of the function.

	cp.compileNode(LF.model.vm, fnNode.Body, newEnv)
	LF.model.dest = LF.model.vm.that()
	LF.size = LF.model.vm.codeTop()
	cp.emit(LF.model.vm, ret)

	// We have made our lambda factory!

	vm.lambdaFactories = append(vm.lambdaFactories, LF)
	return uint32(len(vm.lambdaFactories) - 1), externals.IsEmpty() // A lambda which doesn't close over anything is a constant.
}

func (cp *Compiler) addVariable(vm *Vm, env *environment, name string, acc varAccess, types alternateType) {
	env.data[name] = variable{mLoc: vm.that(), access: acc, types: types}
}

func (cp *Compiler) compileNode(vm *Vm, node ast.Node, env *environment) (alternateType, bool) {
	rtnTypes, rtnConst := alternateType{}, true
	mT := vm.memTop()
	cT := vm.codeTop()
	switch node := node.(type) {
	case *ast.AssignmentExpression:
		if node.Token.Type == token.GVN_ASSIGN {
			// TODO --- need to do this better after we implement tuples
			if node.Left.GetToken().Type != token.IDENT {
				cp.p.Throw("comp/assign/ident", node.Left.GetToken())
				rtnTypes, rtnConst = singleType(ERROR), true
				break
			}
			thunkStart := vm.next()
			types, cst := cp.compileNode(vm, node.Right, env)
			cp.emit(vm, ret)
			if cst {
				cp.addVariable(vm, env, node.Left.(*ast.Identifier).Value, LOCAL_TRUE_CONSTANT, types)
				rtnTypes, rtnConst = singleType(CREATED_LOCAL_CONSTANT), true
				break
			}
			cp.addVariable(vm, env, node.Left.(*ast.Identifier).Value, LOCAL_CONSTANT_THUNK, types)
			cp.thunkList = append(cp.thunkList, thunk{vm.that(), thunkStart})
			rtnTypes, rtnConst = singleType(CREATED_LOCAL_CONSTANT), false
			break
		}
		cp.p.Throw("comp/assign", node.GetToken())
		rtnTypes, rtnConst = singleType(ERROR), true
		break
	case *ast.ApplicationExpression:
		// I doubt that this and related forms such as GroupedExpression are still serving a function in the parser.
		panic("Tim, you were wrong")
	case *ast.BooleanLiteral:
		cp.reserve(vm, BOOL, node.Value)
		rtnTypes, rtnConst = singleType(BOOL), true
		break
	case *ast.EmptyTuple:
		cp.reserve(vm, TUPLE, []Value{})
		rtnTypes, rtnConst = alternateType{finiteTupleType{}}, true
		break
	case *ast.FloatLiteral:
		cp.reserve(vm, FLOAT, node.Value)
		rtnTypes, rtnConst = singleType(FLOAT), true
		break
	case *ast.FuncExpression:
		facNo, isConst := cp.reserveLambdaFactory(vm, env, node, node.GetToken())
		cp.put(vm, mkfn, facNo)
		rtnTypes, rtnConst = singleType(FUNC), isConst
		break
	case *ast.Identifier:
		v, ok := env.getVar(node.Value)
		if !ok {
			cp.p.Throw("comp/ident/known", node.GetToken())
			return singleType(ERROR), true
		}
		if v.access == LOCAL_CONSTANT_THUNK {
			cp.emit(vm, untk, v.mLoc)
		}
		if v.access == REFERENCE_VARIABLE {
			cp.put(vm, dref, v.mLoc)
			rtnTypes = cp.typeNameToTypeList["single?"]
		} else {
			cp.put(vm, asgm, v.mLoc)
			rtnTypes = v.types
		}
		rtnConst = ALL_CONST_ACCESS.Contains(v.access)
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
		cp.p.Throw("comp/infix", node.GetToken())
		rtnTypes, rtnConst = singleType(ERROR), true
		break
	case *ast.IntegerLiteral:
		cp.reserve(vm, INT, node.Value)
		rtnTypes, rtnConst = singleType(INT), true
		break
	case *ast.LazyInfixExpression:
		if node.Operator == "or" {
			lTypes, lcst := cp.compileNode(vm, node.Left, env)
			if !lTypes.contains(BOOL) {
				cp.p.Throw("comp/or/bool/left", node.GetToken())
				rtnTypes, rtnConst = singleType(ERROR), true
				break
			}
			leftRg := vm.that()
			cp.emit(vm, qtru, leftRg, vm.next()+2)
			backtrack := vm.next()
			cp.emit(vm, jmp, DUMMY)
			rTypes, rcst := cp.compileNode(vm, node.Right, env)
			if !rTypes.contains(BOOL) {
				cp.p.Throw("comp/or/bool/right", node.GetToken())
				rtnTypes, rtnConst = singleType(ERROR), true
				break
			}
			rightRg := vm.that()
			vm.code[backtrack].args[0] = vm.next()
			cp.put(vm, orb, leftRg, rightRg)
			rtnTypes, rtnConst = singleType(BOOL), lcst && rcst
			break
		}
		if node.Operator == "and" {
			lTypes, lcst := cp.compileNode(vm, node.Left, env)
			if !lTypes.contains(BOOL) {
				cp.p.Throw("comp/and/bool/left", node.GetToken())
				rtnTypes, rtnConst = singleType(ERROR), true
				break
			}
			leftRg := vm.that()
			backtrack := vm.next()
			cp.emit(vm, qtru, leftRg, DUMMY)
			rTypes, rcst := cp.compileNode(vm, node.Right, env)
			if !rTypes.contains(BOOL) {
				cp.p.Throw("comp/and/bool/right", node.GetToken())
				rtnTypes, rtnConst = singleType(ERROR), true
				break
			}
			rightRg := vm.that()
			vm.code[backtrack].args[1] = vm.next()
			cp.put(vm, andb, leftRg, rightRg)
			rtnTypes, rtnConst = singleType(BOOL), lcst && rcst
			break
		}
		if node.Operator == ":" {
			if node.Left.GetToken().Type == token.ELSE {
				rtnTypes, rtnConst = cp.compileNode(vm, node.Right, env)
				break
			}
			lTypes, lcst := cp.compileNode(vm, node.Left, env)
			if !lTypes.contains(BOOL) {
				cp.p.Throw("comp/cond/bool", node.GetToken())
				rtnTypes, rtnConst = singleType(ERROR), true
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
			rtnTypes, rtnConst = rTypes.union(singleType(UNSAT)), lcst && rcst
			break
		}
		if node.Operator == ";" {
			lTypes, lcst := cp.compileNode(vm, node.Left, env)
			// We deal with the case where the newline is separating local constant definitions
			// in the 'given' block.
			if lTypes.only(CREATED_LOCAL_CONSTANT) {
				_, cst := cp.compileNode(vm, node.Right, env)
				rtnTypes, rtnConst = singleType(CREATED_LOCAL_CONSTANT), lcst && cst
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
	case *ast.ListExpression:
		// allTypes, rtnConst := cp.compileNode(vm, node.List, env)
		// rtnTypes = typesToListTypes(allTypes)
		// lengths := lengths(allTypes)
		// if len(lengths) == 1 && lengths.Contains(1) {
		// 	cp.put(vm, Lfr1, vm.that())
		// 	break
		// }
		// if !lengths.Contains(1) {
		// 	cp.put(vm, LfrT, vm.that())
		// }
		break
	case *ast.PrefixExpression:
		if node.Operator == "not" {
			allTypes, cst := cp.compileNode(vm, node.Args[0], env)
			if allTypes.only(BOOL) {
				cp.put(vm, notb, vm.that())
				rtnTypes, rtnConst = singleType(BOOL), cst
				break
			}
			if !allTypes.contains(BOOL) {
				cp.p.Throw("comp/not/bool", node.GetToken())
				rtnTypes, rtnConst = singleType(ERROR), true
				break
			}
		}
		v, ok := env.getVar(node.Operator)
		if ok && v.types.contains(FUNC) {
			operands := []uint32{v.mLoc}
			for _, arg := range node.Args {
				cp.compileNode(vm, arg, env)
				operands = append(operands, vm.that())
			}
			if cp.p.ErrorsExist() {
				rtnTypes, rtnConst = singleType(ERROR), true
				break
			}
			if v.types.only(FUNC) { // Then no type checking for v.
				cp.put(vm, dofn, operands...)
			}
			rtnTypes = ANY_TYPE
			break
		}
		if cp.p.Prefixes.Contains(node.Operator) || cp.p.Functions.Contains(node.Operator) {
			rtnTypes, rtnConst = cp.createFunctionCall(vm, node, env)
			break
		}
		cp.p.Throw("comp/prefix/known", node.GetToken())
		rtnTypes, rtnConst = singleType(ERROR), true
		break
	case *ast.StreamingExpression: // I.e. -> >> and -> and ?> .
		lhsTypes, lhsConst := cp.compileNode(vm, node.Left, env)
		if cp.p.ErrorsExist() {
			rtnTypes, rtnConst = singleType(ERROR), true
			break
		}
		var whatAccess varAccess
		if lhsConst {
			whatAccess = VERY_LOCAL_CONSTANT
		} else {
			whatAccess = VERY_LOCAL_VARIABLE
		}
		envWithThat := &environment{data: map[string]variable{"that": {mLoc: vm.that(), access: whatAccess, types: lhsTypes}}, ext: env}
		// And that's about all the streaming operators really do have in common under the hood, so let's do a switch on the operators.
		var rhsConst bool
		switch node.Operator {
		case "->":
			rtnTypes, rhsConst = cp.compilePipe(vm, lhsTypes, node.Right, envWithThat)
		case ">>":
			rtnTypes, rhsConst = cp.compileMapping(vm, lhsTypes, node.Right, envWithThat)
		default:
			rtnTypes, rhsConst = cp.compileFilter(vm, lhsTypes, node.Right, envWithThat)
		}
		rtnConst = lhsConst && rhsConst
		break
	case *ast.StringLiteral:
		cp.reserve(vm, STRING, node.Value)
		rtnTypes, rtnConst = singleType(STRING), true
		break
	case *ast.SuffixExpression:
		if cp.p.Suffixes.Contains(node.Operator) {
			rtnTypes, rtnConst = cp.createFunctionCall(vm, node, env)
			break
		}
		cp.p.Throw("comp/suffix", node.GetToken())
		rtnTypes, rtnConst = singleType(ERROR), true
		break
	case *ast.TypeLiteral:
		cp.reserve(vm, TYPE, cp.typeNameToTypeList[node.Value][0])
		rtnTypes, rtnConst = singleType(TYPE), true
		break
	case *ast.UnfixExpression:
		if cp.p.Unfixes.Contains(node.Operator) {
			rtnTypes, rtnConst = cp.createFunctionCall(vm, node, env)
			break
		}
		cp.p.Throw("comp/unfix", node.GetToken())
		rtnTypes, rtnConst = singleType(ERROR), true
		break

		break
	default:
		panic("Unimplemented node type.")
	}
	if rtnConst && vm.codeTop() > cT {
		cp.emit(vm, ret)
		if SHOW_COMPILE {
			println("Expression is constant. Folding.")
		}
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
		cp.p.Throw("comp/tuple/err/a", node.GetToken())
	}
	left := vm.that()
	rTypes, rcst := cp.compileNode(vm, node.Args[2], env)
	if rTypes.only(ERROR) {
		cp.p.Throw("comp/tuple/err/b", node.GetToken())
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
			return alternateType{typedTupleType{lT.t.union(singleType(rT))}}, cst
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
			return alternateType{typedTupleType{rT.t.union(singleType(lT))}}, cst
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
		result = singleType(ts)
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
	if len(args) == 1 {
		switch args[0].(type) {
		case *ast.EmptyTuple:
			args = []ast.Node{}
		}
	}
	b := &bindle{tok: node.GetToken(),
		treePosition: cp.p.FunctionGroupMap[node.GetToken().Literal].Tree,
		outLoc:       cp.reserveError(vm, "vm/oopsie", node.GetToken(), []any{}),
		env:          env,
		valLocs:      make([]uint32, len(args)),
		types:        make(finiteTupleType, len(args)),
	}
	backtrackList := make([]uint32, len(args))
	var traceTokenReserved bool
	var cstI bool
	cst := true
	for i, arg := range args {
		backtrackList[i] = DUMMY
		if i < cp.p.FunctionGroupMap[node.GetToken().Literal].RefCount { // It might be a reference variable
			if arg.GetToken().Type != token.IDENT {
				cp.p.Throw("comp/ref/ident", node.GetToken())
				return singleType(ERROR), true
			}
			v, ok := env.getVar(arg.GetToken().Literal)
			if !ok {
				cp.p.Throw("comp/ref/var", node.GetToken())
				return singleType(ERROR), true
			}
			b.types[i] = cp.typeNameToTypeList["single?"]
			cst = false
			if v.access == REFERENCE_VARIABLE {
				cp.put(vm, asgm, v.mLoc)
				b.valLocs[i] = vm.that()
			} else {
				cp.reserve(vm, REF, v.mLoc)
				b.valLocs[i] = vm.that()
			}
			continue
		}
		switch arg := arg.(type) { // It might be bling.
		case *ast.Bling:
			b.types[i] = alternateType{blingType{arg.Value}}
		default: // Otherwise we emit code to evaluate it.
			b.types[i], cstI = cp.compileNode(vm, arg, env)
			cst = cst && cstI
			b.valLocs[i] = vm.that()
			if b.types[i].(alternateType).only(ERROR) {
				cp.p.Throw("comp/arg/error", node.GetToken())
				return singleType(ERROR), true
			}
			if b.types[i].(alternateType).contains(ERROR) {
				cp.emit(vm, qtyp, vm.that(), uint32(ERROR), vm.codeTop()+2)
				backtrackList[i] = vm.codeTop()
				cp.emit(vm, asgm, DUMMY, vm.that(), vm.thatToken())
				cp.emit(vm, ret)
			}
		}
	}
	// Having gotten the arguments, we create the function call itself.
	returnTypes := cp.generateNewArgument(vm, b) // This is our path into the recursion that will in fact generate the whole function call.

	cp.put(vm, asgm, b.outLoc)
	if returnTypes.only(ERROR) && node.GetToken().Literal != "error" {
		cp.p.Throw("comp/call", b.tok)
	}
	for _, v := range backtrackList {
		if v != DUMMY {
			vm.code[v].args[0] = vm.that()
		}
	}
	if returnTypes.contains(ERROR) {
		if !traceTokenReserved {
			cp.reserveToken(vm, b.tok)
			traceTokenReserved = true
		}
		cp.emit(vm, qtyp, vm.that(), uint32(ERROR), vm.codeTop()+3)
		cp.emit(vm, adtk, vm.that(), vm.thatToken())
		cp.emit(vm, ret)
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
	tok          *token.Token    // For generating errors.
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
	// Case (3) : we have a reference.
	if b.treePosition.Branch[b.branchNo].TypeName == "ref" {
		newBindle := *b
		newBindle.treePosition = b.treePosition.Branch[b.branchNo].Node
		newBindle.argNo++
		return cp.generateNewArgument(vm, &newBindle)
	}
	// Case (4) : we're in tuple time.
	if b.tupleTime {
		newBindle := *b
		newBindle.argNo++
		return cp.generateNewArgument(vm, &newBindle)
	}
	// Case (5) : We aren't yet at the end of the list of arguments.
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
	if b.tupleTime || b.branchNo < len(b.treePosition.Branch) && b.treePosition.Branch[b.branchNo].TypeName == "tuple" { // We can move on to the next argument.
		newBindle := *b
		newBindle.treePosition = b.treePosition.Branch[b.branchNo].Node
		newBindle.tupleTime = true
		newBindle.argNo++
		return cp.generateNewArgument(vm, &newBindle)
	}
	if b.branchNo >= len(b.treePosition.Branch) { // We've tried all the alternatives and have some left over.
		cp.reserveError(vm, "vm/types/a", b.tok, []any{})
		cp.emit(vm, asgm, b.outLoc, vm.that())
		return singleType(ERROR)
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
				functionAndType.f(cp, vm, b.tok, b.outLoc, b.valLocs)
				return functionAndType.t
			}
			cp.emitFunctionCall(vm, fNo, b.valLocs)
			cp.emit(vm, asgm, b.outLoc, cp.fns[fNo].outReg) // Because the different implementations of the function will have their own out register.
			return cp.fns[fNo].types                        // TODO : Is there a reason why this should be so?
		}
	}
	cp.reserveError(vm, "vm/types/b", b.tok, []any{}) // TODO : the bindle can accumulate the types to allow us to generates this error properly.
	cp.emit(vm, asgm, b.outLoc, vm.that())
	return singleType(ERROR)
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
	return singleType(ERROR)
}

// We have two different ways of emiting an opcode: 'emit' does it the regular way, 'put' ensures that
// the destination is the next free memory address.
func (cp *Compiler) emit(vm *Vm, opcode opcode, args ...uint32) {
	vm.code = append(vm.code, makeOp(opcode, args...))
	if SHOW_COMPILE {
		println(vm, vm.describeCode(vm.codeTop()-1))
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
		cp.p.Throw("comp/eq/err/a", node.GetToken())
		return singleType(ERROR), true
	}
	leftRg := vm.that()
	rTypes, rcst := cp.compileNode(vm, node.Args[2], env)
	if rTypes.only(ERROR) {
		cp.p.Throw("comp/eq/err/b", node.GetToken())
		return singleType(ERROR), true
	}
	rightRg := vm.that()
	oL := lTypes.intersect(rTypes)
	if oL.only(ERROR) {
		cp.p.Throw("comp/eq/err/c", node.GetToken())
		return singleType(ERROR), true
	}
	if len(oL) == 0 {
		cp.p.Throw("comp/eq/types", node.GetToken())
		return singleType(ERROR), true
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
			return singleType(BOOL), lcst && rcst
		default:
			panic("Unimplemented comparison type.")
		}
	} else {
		panic("Haven't implemented this bit because of having no way to test it at this point.")
	}
}

// The various 'streaming operators'. TODO, find different name.

func (cp *Compiler) compilePipe(vm *Vm, lhsTypes alternateType, rhs ast.Node, env *environment) (alternateType, bool) {
	// If we have a single identifier, we wish it to contain a function ...
	if rhs.GetToken().Type == token.IDENT {
		v, ok := env.getVar(rhs.GetToken().Literal)
		if ok && v.types.contains(FUNC) {
			if v.types.only(FUNC) {
				cp.put(vm, dofn, v.mLoc, vm.that())
				return ANY_TYPE, ALL_CONST_ACCESS.Contains(v.access)
			} else {
				// Emit some error handling.

			}
		}
	}
	// Otherwise we are in the presence of a 'that' expression. We can simply compile and return.
	return cp.compileNode(vm, rhs, env)
}

func (cp *Compiler) compileMapping(vm *Vm, lhsTypes alternateType, rhs ast.Node, env *environment) (alternateType, bool) {
	return alternateType{}, false
}

func (cp *Compiler) compileFilter(vm *Vm, lhsTypes alternateType, rhs ast.Node, env *environment) (alternateType, bool) {
	return alternateType{}, false
}
