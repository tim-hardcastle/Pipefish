package vm

import (
	"pipefish/source/ast"
	"pipefish/source/object"
	"pipefish/source/parser"
	"pipefish/source/set"
	"pipefish/source/token"
	"pipefish/source/values"
)

const SHOW_COMPILE = true
const SHOW_RUN = true

type enumOrdinates struct {
	enum    values.ValueType
	element int
}

type thunk struct {
	mLoc uint32
	cLoc uint32
}

type Compiler struct {
	p                  *parser.Parser
	mc                 *Vm
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
		mc:        blankVm(),
		enums:     make(map[string]enumOrdinates),
		gconsts:   newEnvironment(),
		gvars:     newEnvironment(),
		thunkList: []thunk{},
		fns:       []*cpFunc{},
		typeNameToTypeList: map[string]alternateType{
			"int":      altType(values.INT),
			"string":   altType(values.STRING),
			"bool":     altType(values.BOOL),
			"float64":  altType(values.FLOAT),
			"error":    altType(values.ERROR),
			"type":     altType(values.TYPE),
			"int?":     altType(values.NULL, values.INT),
			"string?":  altType(values.NULL, values.STRING),
			"bool?":    altType(values.NULL, values.BOOL),
			"float64?": altType(values.NULL, values.FLOAT),
			"type?":    altType(values.NULL, values.TYPE),
			"null":     altType(values.NULL),
			"single":   altType(values.INT, values.BOOL, values.STRING, values.FLOAT, values.TYPE, values.FUNC, values.LIST),
			"single?":  altType(values.NULL, values.INT, values.BOOL, values.STRING, values.FLOAT, values.TYPE, values.FUNC, values.LIST),
		},
	}
}

func (cp *Compiler) Run() {
	cp.mc.Run(0)
}

func (cp *Compiler) GetParser() *parser.Parser {
	return cp.p
}

func (cp *Compiler) Do(line string) values.Value {
	mT := cp.mc.memTop()
	cT := cp.mc.codeTop()
	node := cp.p.ParseLine("REPL input", line)
	if cp.p.ErrorsExist() {
		return values.Value{T: values.ERROR}
	}
	cp.compileNode(cp.mc, node, cp.gvars)
	if cp.p.ErrorsExist() {
		return values.Value{T: values.ERROR}
	}
	cp.emit(cp.mc, Ret)
	cp.mc.Run(cT)
	result := cp.mc.mem[cp.mc.that()]
	cp.mc.mem = cp.mc.mem[:mT]
	cp.mc.code = cp.mc.code[:cT]
	return result
}

func (cp *Compiler) Describe(v values.Value) string {
	return cp.mc.literal(v)
}

func (cp *Compiler) Compile(source, sourcecode string) {
	cp.mc = blankVm()
	node := cp.p.ParseLine(source, sourcecode)
	cp.compileNode(cp.mc, node, cp.gvars)
	cp.emit(cp.mc, Ret)
}

func (cp *Compiler) reserve(mc *Vm, t values.ValueType, v any) uint32 {
	mc.mem = append(mc.mem, values.Value{T: t, V: v})
	return uint32(len(mc.mem) - 1)
}

func (cp *Compiler) reserveError(mc *Vm, ec string, tok *token.Token, args []any) uint32 {
	mc.mem = append(mc.mem, values.Value{T: values.ERROR, V: &object.Error{ErrorId: ec, Token: tok, Args: append([]any{mc}, args...), Trace: make([]*token.Token, 0, 10)}})
	return uint32(len(mc.mem) - 1)
}

func (cp *Compiler) reserveToken(mc *Vm, tok *token.Token) uint32 {
	mc.tokens = append(mc.tokens, tok)
	return uint32(len(mc.tokens) - 1)
}

func (cp *Compiler) reserveLambdaFactory(mc *Vm, env *environment, fnNode *ast.FuncExpression, tok *token.Token) (uint32, bool) {
	LF := &lambdaFactory{model: &lambda{}}
	LF.model.mc = blankVm()
	LF.model.mc.code = []*operation{}
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
		cp.reserve(LF.model.mc, 0, DUMMY) // It doesn't matter what we put in here 'cos we copy the values any time we call the LambdaFactory.
		cp.addVariable(LF.model.mc, newEnv, k, v.access, v.types)
		// At the same time, the lambda factory need to know where they are in the calling Vm.
		LF.extMem = append(LF.extMem, v.mLoc)
	}

	LF.model.extTop = LF.model.mc.memTop()

	// Add the function parameters.
	for _, pair := range sig { // It doesn't matter what we put in here either, because we're going to have to copy the values any time we call the function.
		cp.reserve(LF.model.mc, 0, DUMMY)
		cp.addVariable(LF.model.mc, newEnv, pair.VarName, FUNCTION_ARGUMENT, cp.typeNameToTypeList[pair.VarType])
	}

	LF.model.prmTop = LF.model.mc.memTop()

	// Compile the locals.

	if fnNode.Given != nil {
		cp.thunkList = []thunk{}
		cp.compileNode(LF.model.mc, fnNode.Given, newEnv)
		for _, pair := range cp.thunkList {
			cp.emit(LF.model.mc, Thnk, pair.mLoc, pair.cLoc)
		}
	}

	// Function starts here.

	LF.model.locToCall = LF.model.mc.codeTop()

	// We have to typecheck inside the lambda, because the calling site doesn't know which function it's calling.

	// TODO !!!

	// Now we can emit the main body of the function.

	cp.compileNode(LF.model.mc, fnNode.Body, newEnv)
	LF.model.dest = LF.model.mc.that()
	LF.size = LF.model.mc.codeTop()
	cp.emit(LF.model.mc, Ret)

	// We have made our lambda factory!

	mc.lambdaFactories = append(mc.lambdaFactories, LF)
	return uint32(len(mc.lambdaFactories) - 1), externals.IsEmpty() // A lambda which doesn't close over anything is a constant.
}

func (cp *Compiler) addVariable(mc *Vm, env *environment, name string, acc varAccess, types alternateType) {
	env.data[name] = variable{mLoc: mc.that(), access: acc, types: types}
}

func (cp *Compiler) compileNode(mc *Vm, node ast.Node, env *environment) (alternateType, bool) {
	rtnTypes, rtnConst := alternateType{}, true
	mT := mc.memTop()
	cT := mc.codeTop()
	switch node := node.(type) {
	case *ast.AssignmentExpression:
		if node.Token.Type == token.GVN_ASSIGN {
			// TODO --- need to do this better after we implement tuples
			if node.Left.GetToken().Type != token.IDENT {
				cp.p.Throw("comp/assign/ident", node.Left.GetToken())
				rtnTypes, rtnConst = altType(values.ERROR), true
				break
			}
			thunkStart := mc.next()
			types, cst := cp.compileNode(mc, node.Right, env)
			cp.emit(mc, Ret)
			if cst {
				cp.addVariable(mc, env, node.Left.(*ast.Identifier).Value, LOCAL_TRUE_CONSTANT, types)
				rtnTypes, rtnConst = altType(values.CREATED_LOCAL_CONSTANT), true
				break
			}
			cp.addVariable(mc, env, node.Left.(*ast.Identifier).Value, LOCAL_CONSTANT_THUNK, types)
			cp.thunkList = append(cp.thunkList, thunk{mc.that(), thunkStart})
			rtnTypes, rtnConst = altType(values.CREATED_LOCAL_CONSTANT), false
			break
		}
		cp.p.Throw("comp/assign", node.GetToken())
		rtnTypes, rtnConst = altType(values.ERROR), true
		break
	case *ast.ApplicationExpression:
		// I doubt that this and related forms such as GroupedExpression are still serving a function in the parser.
		panic("Tim, you were wrong")
	case *ast.BooleanLiteral:
		cp.reserve(mc, values.BOOL, node.Value)
		rtnTypes, rtnConst = altType(values.BOOL), true
		break
	case *ast.EmptyTuple:
		cp.reserve(mc, values.TUPLE, []values.Value{})
		rtnTypes, rtnConst = alternateType{finiteTupleType{}}, true
		break
	case *ast.FloatLiteral:
		cp.reserve(mc, values.FLOAT, node.Value)
		rtnTypes, rtnConst = altType(values.FLOAT), true
		break
	case *ast.FuncExpression:
		facNo, isConst := cp.reserveLambdaFactory(mc, env, node, node.GetToken())
		cp.put(mc, Mkfn, facNo)
		rtnTypes, rtnConst = altType(values.FUNC), isConst
		break
	case *ast.Identifier:
		v, ok := env.getVar(node.Value)
		if !ok {
			cp.p.Throw("comp/ident/known", node.GetToken())
			return altType(values.ERROR), true
		}
		if v.access == LOCAL_CONSTANT_THUNK {
			cp.emit(mc, Untk, v.mLoc)
		}
		if v.access == REFERENCE_VARIABLE {
			cp.put(mc, Dref, v.mLoc)
			rtnTypes = cp.typeNameToTypeList["single?"]
		} else {
			cp.put(mc, Asgm, v.mLoc)
			rtnTypes = v.types
		}
		rtnConst = ALL_CONST_ACCESS.Contains(v.access)
		break
	case *ast.InfixExpression:
		if cp.p.Infixes.Contains(node.Operator) {
			rtnTypes, rtnConst = cp.createFunctionCall(mc, node, env)
			break
		}
		if node.Operator == "," {
			rtnTypes, rtnConst = cp.emitComma(mc, node, env)
			break
		}
		if node.Operator == "==" {
			rtnTypes, rtnConst = cp.emitEquals(mc, node, env)
			break
		}
		if node.Operator == "!=" {
			rtnTypes, rtnConst = cp.emitEquals(mc, node, env)
			cp.put(mc, Notb, mc.that())
			break
		}
		cp.p.Throw("comp/infix", node.GetToken())
		rtnTypes, rtnConst = altType(values.ERROR), true
		break
	case *ast.IntegerLiteral:
		cp.reserve(mc, values.INT, node.Value)
		rtnTypes, rtnConst = altType(values.INT), true
		break
	case *ast.LazyInfixExpression:
		if node.Operator == "or" {
			lTypes, lcst := cp.compileNode(mc, node.Left, env)
			if !lTypes.contains(tp(values.BOOL)) {
				cp.p.Throw("comp/or/bool/left", node.GetToken())
				rtnTypes, rtnConst = altType(values.ERROR), true
				break
			}
			leftRg := mc.that()
			cp.emit(mc, Qtru, leftRg, mc.next()+2)
			backtrack := mc.next()
			cp.emit(mc, Jmp, DUMMY)
			rTypes, rcst := cp.compileNode(mc, node.Right, env)
			if !rTypes.contains(tp(values.BOOL)) {
				cp.p.Throw("comp/or/bool/right", node.GetToken())
				rtnTypes, rtnConst = altType(values.ERROR), true
				break
			}
			rightRg := mc.that()
			mc.code[backtrack].args[0] = mc.next()
			cp.put(mc, Orb, leftRg, rightRg)
			rtnTypes, rtnConst = altType(values.BOOL), lcst && rcst
			break
		}
		if node.Operator == "and" {
			lTypes, lcst := cp.compileNode(mc, node.Left, env)
			if !lTypes.contains(tp(values.BOOL)) {
				cp.p.Throw("comp/and/bool/left", node.GetToken())
				rtnTypes, rtnConst = altType(values.ERROR), true
				break
			}
			leftRg := mc.that()
			backtrack := mc.next()
			cp.emit(mc, Qtru, leftRg, DUMMY)
			rTypes, rcst := cp.compileNode(mc, node.Right, env)
			if !rTypes.contains(tp(values.BOOL)) {
				cp.p.Throw("comp/and/bool/right", node.GetToken())
				rtnTypes, rtnConst = altType(values.ERROR), true
				break
			}
			rightRg := mc.that()
			mc.code[backtrack].args[1] = mc.next()
			cp.put(mc, Andb, leftRg, rightRg)
			rtnTypes, rtnConst = altType(values.BOOL), lcst && rcst
			break
		}
		if node.Operator == ":" {
			if node.Left.GetToken().Type == token.ELSE {
				rtnTypes, rtnConst = cp.compileNode(mc, node.Right, env)
				break
			}
			lTypes, lcst := cp.compileNode(mc, node.Left, env)
			if !lTypes.contains(tp(values.BOOL)) {
				cp.p.Throw("comp/cond/bool", node.GetToken())
				rtnTypes, rtnConst = altType(values.ERROR), true
				break
			}
			leftRg := mc.that()
			backtrack := mc.next()
			cp.emit(mc, Qtru, leftRg, DUMMY)
			rTypes, rcst := cp.compileNode(mc, node.Right, env)
			cp.put(mc, Asgm, mc.that())
			cp.emit(mc, Jmp, mc.next()+2)
			mc.code[backtrack].args[1] = mc.next()
			cp.reput(mc, Asgm, values.C_U_OBJ)
			rtnTypes, rtnConst = rTypes.union(altType(values.UNSAT)), lcst && rcst
			break
		}
		if node.Operator == ";" {
			lTypes, lcst := cp.compileNode(mc, node.Left, env)
			// We deal with the case where the newline is separating local constant definitions
			// in the 'given' block.
			if lTypes.only(tp(values.CREATED_LOCAL_CONSTANT)) {
				_, cst := cp.compileNode(mc, node.Right, env)
				rtnTypes, rtnConst = altType(values.CREATED_LOCAL_CONSTANT), lcst && cst
				break
			}
			leftRg := mc.that()
			backtrack := mc.next()
			cp.emit(mc, Qtyp, leftRg, uint32(values.UNSAT), DUMMY)
			rTypes, rcst := cp.compileNode(mc, node.Right, env)
			rightRg := mc.that()
			cp.put(mc, Asgm, rightRg)
			cp.emit(mc, Jmp, mc.next()+2)
			mc.code[backtrack].args[2] = mc.next()
			cp.reput(mc, Asgm, leftRg)
			if !(lTypes.contains(tp(values.UNSAT)) && rTypes.contains(tp(values.UNSAT))) {
				rtnTypes, rtnConst = lTypes.union(rTypes).without(tp(values.UNSAT)), lcst && rcst
				break
			}
			rtnTypes, rtnConst = lTypes.union(rTypes), lcst && rcst
			break
		}
	case *ast.ListExpression:
		// allTypes, rtnConst := cp.compileNode(mc, node.List, env)
		// rtnTypes = typesToListTypes(allTypes)
		// lengths := lengths(allTypes)
		// if len(lengths) == 1 && lengths.Contains(1) {
		// 	cp.put(mc, Lfr1, mc.that())
		// 	break
		// }
		// if !lengths.Contains(1) {
		// 	cp.put(mc, LfrT, mc.that())
		// }
		break
	case *ast.PrefixExpression:
		if node.Operator == "not" {
			allTypes, cst := cp.compileNode(mc, node.Args[0], env)
			if allTypes.only(tp(values.BOOL)) {
				cp.put(mc, Notb, mc.that())
				rtnTypes, rtnConst = altType(values.BOOL), cst
				break
			}
			if !allTypes.contains(tp(values.BOOL)) {
				cp.p.Throw("comp/not/bool", node.GetToken())
				rtnTypes, rtnConst = altType(values.ERROR), true
				break
			}
		}
		v, ok := env.getVar(node.Operator)
		if ok && v.types.contains(tp(values.FUNC)) {
			operands := []uint32{v.mLoc}
			for _, arg := range node.Args {
				cp.compileNode(mc, arg, env)
				operands = append(operands, mc.that())
			}
			if cp.p.ErrorsExist() {
				rtnTypes, rtnConst = altType(values.ERROR), true
				break
			}
			if v.types.only(tp(values.FUNC)) { // Then no type checking for v.
				cp.put(mc, Dofn, operands...)
			}
			rtnTypes = ANY_TYPE
			break
		}
		if cp.p.Prefixes.Contains(node.Operator) || cp.p.Functions.Contains(node.Operator) {
			rtnTypes, rtnConst = cp.createFunctionCall(mc, node, env)
			break
		}
		cp.p.Throw("comp/prefix/known", node.GetToken())
		rtnTypes, rtnConst = altType(values.ERROR), true
		break
	case *ast.StreamingExpression: // I.e. -> >> and -> and ?> .
		lhsTypes, lhsConst := cp.compileNode(mc, node.Left, env)
		if cp.p.ErrorsExist() {
			rtnTypes, rtnConst = altType(values.ERROR), true
			break
		}
		var whatAccess varAccess
		if lhsConst {
			whatAccess = VERY_LOCAL_CONSTANT
		} else {
			whatAccess = VERY_LOCAL_VARIABLE
		}
		envWithThat := &environment{data: map[string]variable{"that": {mLoc: mc.that(), access: whatAccess, types: lhsTypes}}, ext: env}
		// And that's about all the streaming operators really do have in common under the hood, so let's do a switch on the operators.
		var rhsConst bool
		switch node.Operator {
		case "->":
			rtnTypes, rhsConst = cp.compilePipe(mc, lhsTypes, node.Right, envWithThat)
		case ">>":
			rtnTypes, rhsConst = cp.compileMapping(mc, lhsTypes, node.Right, envWithThat)
		default:
			rtnTypes, rhsConst = cp.compileFilter(mc, lhsTypes, node.Right, envWithThat)
		}
		rtnConst = lhsConst && rhsConst
		break
	case *ast.StringLiteral:
		cp.reserve(mc, values.STRING, node.Value)
		rtnTypes, rtnConst = altType(values.STRING), true
		break
	case *ast.SuffixExpression:
		if cp.p.Suffixes.Contains(node.Operator) {
			rtnTypes, rtnConst = cp.createFunctionCall(mc, node, env)
			break
		}
		cp.p.Throw("comp/suffix", node.GetToken())
		rtnTypes, rtnConst = altType(values.ERROR), true
		break
	case *ast.TypeLiteral:
		cp.reserve(mc, values.TYPE, cp.typeNameToTypeList[node.Value][0])
		rtnTypes, rtnConst = altType(values.TYPE), true
		break
	case *ast.UnfixExpression:
		if cp.p.Unfixes.Contains(node.Operator) {
			rtnTypes, rtnConst = cp.createFunctionCall(mc, node, env)
			break
		}
		cp.p.Throw("comp/unfix", node.GetToken())
		rtnTypes, rtnConst = altType(values.ERROR), true
		break

		break
	default:
		panic("Unimplemented node type.")
	}
	if rtnConst && mc.codeTop() > cT {
		cp.emit(mc, Ret)
		if SHOW_COMPILE {
			println("Expression is constant. Folding.")
		}
		mc.Run(cT)
		result := mc.mem[mc.that()]
		mc.mem = mc.mem[:mT]
		mc.code = mc.code[:cT]
		cp.reserve(mc, result.T, result.V)
	}
	return rtnTypes, rtnConst
}

// This needs its own very special logic because the type it returns has to be composed in a different way from all the other operators.
func (cp *Compiler) emitComma(mc *Vm, node *ast.InfixExpression, env *environment) (alternateType, bool) {
	lTypes, lcst := cp.compileNode(mc, node.Args[0], env)
	if lTypes.only(tp(values.ERROR)) {
		cp.p.Throw("comp/tuple/err/a", node.GetToken())
	}
	left := mc.that()
	rTypes, rcst := cp.compileNode(mc, node.Args[2], env)
	if rTypes.only(tp(values.ERROR)) {
		cp.p.Throw("comp/tuple/err/b", node.GetToken())
	}
	right := mc.that()
	var leftBacktrack, rightBacktrack uint32
	if lTypes.contains(tp(values.ERROR)) {
		cp.emit(mc, Qtyp, left, uint32(tp(values.ERROR)), mc.codeTop()+2)
		leftBacktrack = mc.codeTop()
		cp.put(mc, Asgm, DUMMY, left)
		if rTypes.contains(tp(values.ERROR)) {
			cp.emit(mc, Jmp, mc.codeTop()+5)
		} else {
			cp.emit(mc, Jmp, mc.codeTop()+2)
		}
	}
	if rTypes.contains(tp(values.ERROR)) {
		cp.emit(mc, Qtyp, right, uint32(tp(values.ERROR)), mc.codeTop()+2)
		rightBacktrack = mc.codeTop()
		cp.put(mc, Asgm, right)
		cp.emit(mc, Jmp, mc.codeTop()+2)
	}
	leftMustBeSingle, leftMustBeTuple := lTypes.mustBeSingleOrTuple()
	rightMustBeSingle, rightMustBeTuple := rTypes.mustBeSingleOrTuple()
	switch {
	case leftMustBeSingle && rightMustBeSingle:
		cp.put(mc, Cc11, left, right)
	case leftMustBeSingle && rightMustBeTuple:
		cp.put(mc, Cc1T, left, right)
	case leftMustBeTuple && rightMustBeSingle:
		cp.put(mc, CcT1, left, right)
	case leftMustBeTuple && rightMustBeTuple:
		cp.put(mc, CcTT, left, right)
	default:
		cp.put(mc, Ccxx, left, right) // We can after all let the operation dispatch for us.
	}
	if lTypes.contains(tp(values.ERROR)) {
		mc.code[leftBacktrack].args[0] = mc.that()
	}
	if rTypes.contains(tp(values.ERROR)) {
		mc.code[rightBacktrack].args[0] = mc.that()
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
			return alternateType{typedTupleType{lT.t.union(alternateType{rT})}}, cst
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
			return alternateType{typedTupleType{rT.t.union(alternateType{lT})}}, cst
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
		result = alternateType{ts}
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

func (cp *Compiler) createFunctionCall(mc *Vm, node ast.Callable, env *environment) (alternateType, bool) {
	args := node.GetArgs()
	if len(args) == 1 {
		switch args[0].(type) {
		case *ast.EmptyTuple:
			args = []ast.Node{}
		}
	}
	b := &bindle{tok: node.GetToken(),
		treePosition: cp.p.FunctionGroupMap[node.GetToken().Literal].Tree,
		outLoc:       cp.reserveError(mc, "mc/oopsie", node.GetToken(), []any{}),
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
				return altType(values.ERROR), true
			}
			v, ok := env.getVar(arg.GetToken().Literal)
			if !ok {
				cp.p.Throw("comp/ref/var", node.GetToken())
				return altType(values.ERROR), true
			}
			b.types[i] = cp.typeNameToTypeList["single?"]
			cst = false
			if v.access == REFERENCE_VARIABLE {
				cp.put(mc, Asgm, v.mLoc)
				b.valLocs[i] = mc.that()
			} else {
				cp.reserve(mc, values.REF, v.mLoc)
				b.valLocs[i] = mc.that()
			}
			continue
		}
		switch arg := arg.(type) { // It might be bling.
		case *ast.Bling:
			b.types[i] = alternateType{blingType{arg.Value}}
		default: // Otherwise we emit code to evaluate it.
			b.types[i], cstI = cp.compileNode(mc, arg, env)
			cst = cst && cstI
			b.valLocs[i] = mc.that()
			if b.types[i].(alternateType).only(tp(values.ERROR)) {
				cp.p.Throw("comp/arg/error", node.GetToken())
				return altType(values.ERROR), true
			}
			if b.types[i].(alternateType).contains(tp(values.ERROR)) {
				cp.emit(mc, Qtyp, mc.that(), uint32(tp(values.ERROR)), mc.codeTop()+2)
				backtrackList[i] = mc.codeTop()
				cp.emit(mc, Asgm, DUMMY, mc.that(), mc.thatToken())
				cp.emit(mc, Ret)
			}
		}
	}
	// Having gotten the arguments, we create the function call itself.
	returnTypes := cp.generateNewArgument(mc, b) // This is our path into the recursion that will in fact generate the whole function call.

	cp.put(mc, Asgm, b.outLoc)
	if returnTypes.only(tp(values.ERROR)) && node.GetToken().Literal != "error" {
		cp.p.Throw("comp/call", b.tok)
	}
	for _, v := range backtrackList {
		if v != DUMMY {
			mc.code[v].args[0] = mc.that()
		}
	}
	if returnTypes.contains(tp(values.ERROR)) {
		if !traceTokenReserved {
			cp.reserveToken(mc, b.tok)
			traceTokenReserved = true
		}
		cp.emit(mc, Qtyp, mc.that(), uint32(values.ERROR), mc.codeTop()+3)
		cp.emit(mc, Adtk, mc.that(), mc.thatToken())
		cp.emit(mc, Ret)
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

func (cp *Compiler) generateNewArgument(mc *Vm, b *bindle) alternateType {
	// Case (1) : we've used up all our arguments. In this case we should look in the function tree for a function call.
	if b.argNo >= len(b.types) {
		return cp.seekFunctionCall(mc, b)
	}
	// Case (2) : the argument is bling.
	if len(b.types[b.argNo].(alternateType)) == 1 {
		switch bl := (b.types[b.argNo].(alternateType)[0]).(type) {
		case blingType:
			return cp.seekBling(mc, b, bl.tag)
		}
	}
	// Case (3) : we have a reference.
	if b.treePosition.Branch[b.branchNo].TypeName == "ref" {
		newBindle := *b
		newBindle.treePosition = b.treePosition.Branch[b.branchNo].Node
		newBindle.argNo++
		return cp.generateNewArgument(mc, &newBindle)
	}
	// Case (4) : we're in tuple time.
	if b.tupleTime {
		newBindle := *b
		newBindle.argNo++
		return cp.generateNewArgument(mc, &newBindle)
	}
	// Case (5) : We aren't yet at the end of the list of arguments.
	newBindle := *b
	newBindle.index = 0
	return cp.generateFromTopBranchDown(mc, b)
}

func (cp *Compiler) generateFromTopBranchDown(mc *Vm, b *bindle) alternateType {
	newBindle := *b
	newBindle.branchNo = 0
	newBindle.targetList = typesAtIndex(b.types[b.argNo], b.index)
	newBindle.doneList = make(alternateType, 0, len(b.targetList))
	if newBindle.index == 0 {
		newBindle.lengths = lengths(newBindle.targetList)
		newBindle.maxLength = maxLengthsOrMinusOne(newBindle.lengths)
	}
	return cp.generateBranch(mc, &newBindle)
}

// We look at the current branch and see if its type can account for some, all, or none of the possibilities in the targetList.
// If the answer is "all", we can recurse on the next argument.
// If "none", then we can recurse on the next branch down.
// If "some", then we must generate a conditional where it recurses on the next argument for the types accepted by the branch
// and on the next branch for the unaccepted types.
// It may also be the run-off-the-end branch number, in which case we can generate an error.
func (cp *Compiler) generateBranch(mc *Vm, b *bindle) alternateType {
	if b.tupleTime || b.branchNo < len(b.treePosition.Branch) && b.treePosition.Branch[b.branchNo].TypeName == "tuple" { // We can move on to the next argument.
		newBindle := *b
		newBindle.treePosition = b.treePosition.Branch[b.branchNo].Node
		newBindle.tupleTime = true
		newBindle.argNo++
		return cp.generateNewArgument(mc, &newBindle)
	}
	if b.branchNo >= len(b.treePosition.Branch) { // We've tried all the alternatives and have some left over.
		cp.reserveError(mc, "mc/types/a", b.tok, []any{})
		cp.emit(mc, Asgm, b.outLoc, mc.that())
		return altType(values.ERROR)
	}
	branch := b.treePosition.Branch[b.branchNo]
	acceptedTypes := cp.typeNameToTypeList[branch.TypeName]
	overlap := acceptedTypes.intersect(b.targetList)
	if len(overlap) == 0 { // We drew a blank.
		return cp.generateNextBranchDown(mc, b)
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
	branchBacktrack := mc.codeTop()
	if needsOtherBranch {
		// Then we need to generate a conditional. Which one exactly depends on whether we're looking at a single, a tuple, or both.
		switch len(acceptedSingleTypes) {
		case 0:
			cp.put(mc, IdxT, b.valLocs[b.argNo], uint32(b.index))
			cp.emitTypeComparison(mc, branch.TypeName, mc.that(), DUMMY)
		case len(overlap):
			cp.emitTypeComparison(mc, branch.TypeName, b.valLocs[b.argNo], DUMMY)
		default:
			cp.emit(mc, QsnQ, b.valLocs[b.argNo], mc.codeTop()+3)
			cp.emitTypeComparison(mc, branch.TypeName, b.valLocs[b.argNo], DUMMY)
			cp.emit(mc, Jmp, mc.codeTop()+3)
			cp.put(mc, IdxT, b.valLocs[b.argNo], uint32(b.index))
			cp.emitTypeComparison(mc, branch.TypeName, mc.that(), DUMMY)
		}
	}
	// Now we're in the 'if' part of the 'if-else'. We can recurse along the branch.
	// If we know whether we're looking at a single or a tuple, we can erase this and act accordingly, otherwise we generate a conditional.
	var typesFromGoingAcross, typesFromGoingDown alternateType
	switch len(acceptedSingleTypes) {
	case 0:
		typesFromGoingAcross = cp.generateMoveAlongBranchViaTupleElement(mc, &newBindle)
	case len(overlap):
		typesFromGoingAcross = cp.generateMoveAlongBranchViaSingleValue(mc, &newBindle)
	default:
		backtrack := mc.codeTop()
		cp.emit(mc, QsnQ, b.valLocs[b.argNo], DUMMY)
		typesFromSingles := cp.generateMoveAlongBranchViaSingleValue(mc, &newBindle)
		cp.emit(mc, Jmp, DUMMY)
		mc.code[backtrack].makeLastArg(mc.codeTop())
		backtrack = mc.codeTop()
		typesFromTuples := cp.generateMoveAlongBranchViaTupleElement(mc, &newBindle)
		mc.code[backtrack].makeLastArg(mc.codeTop())
		typesFromGoingAcross = typesFromSingles.union(typesFromTuples)
	}
	// And now we need to do the 'else' branch if there is one.
	if needsOtherBranch {
		elseBacktrack := mc.codeTop()
		cp.emit(mc, Jmp, DUMMY) // The last part of the 'if' branch: jumps over the 'else'.
		// We need to backtrack on whatever conditional we generated.
		switch len(acceptedSingleTypes) {
		case 0:
			mc.code[branchBacktrack+1].makeLastArg(mc.codeTop())
		case len(overlap):
			mc.code[branchBacktrack].makeLastArg(mc.codeTop())
		default:
			mc.code[branchBacktrack+1].makeLastArg(mc.codeTop())
			mc.code[branchBacktrack+4].makeLastArg(mc.codeTop())
		}
		// We recurse on the next branch down.
		typesFromGoingDown = cp.generateNextBranchDown(mc, &newBindle)
		mc.code[elseBacktrack].makeLastArg(mc.codeTop())
	}
	return typesFromGoingAcross.union(typesFromGoingDown)
}

var TYPE_COMPARISONS = map[string]*operation{
	"int":     {Qtyp, []uint32{DUMMY, uint32(values.INT), DUMMY}},
	"string":  {Qtyp, []uint32{DUMMY, uint32(values.STRING), DUMMY}},
	"bool":    {Qtyp, []uint32{DUMMY, uint32(values.BOOL), DUMMY}},
	"float64": {Qtyp, []uint32{DUMMY, uint32(values.FLOAT), DUMMY}},
	"null":    {Qtyp, []uint32{DUMMY, uint32(values.NULL), DUMMY}},
	"list":    {Qtyp, []uint32{DUMMY, uint32(values.LIST), DUMMY}},
	"func":    {Qtyp, []uint32{DUMMY, uint32(values.FUNC), DUMMY}},
	"single":  {Qsng, []uint32{DUMMY, DUMMY}},
	"single?": {QsnQ, []uint32{DUMMY, DUMMY}},
}

func (cp *Compiler) emitTypeComparison(mc *Vm, typeAsString string, mem, loc uint32) {
	op, ok := TYPE_COMPARISONS[typeAsString]
	if ok {
		newArgs := make([]uint32, len(op.args))
		copy(newArgs, op.args)
		newOp := &operation{op.opcode, newArgs}
		newOp.args[0] = mem
		newOp.makeLastArg(loc)
		cp.emit(mc, newOp.opcode, newArgs...)
		return
	}
	panic("Unknown type: " + typeAsString)
}

func (cp *Compiler) generateMoveAlongBranchViaTupleElement(mc *Vm, b *bindle) alternateType {
	// We may definitely have run off the end of all the potential tuples.
	if b.index+1 == b.maxLength {
		newBindle := *b
		newBindle.argNo++
		return cp.generateNewArgument(mc, &newBindle)
	}
	newBindle := *b
	newBindle.index++
	newBindle.treePosition = newBindle.treePosition.Branch[newBindle.branchNo].Node
	// We may have to generate an if-then-else to do a length check on the tuple.
	var typesFromNextArgument alternateType
	needsConditional := b.maxLength == -1 || // Then there's a non-finite tuple
		b.lengths.Contains(newBindle.index) // Then we may have run off the end of a finite tuple.
	backtrack1 := mc.codeTop()
	var backtrack2 uint32
	if needsConditional {
		cp.emit(mc, QlnT, b.valLocs[newBindle.argNo], uint32(newBindle.index), DUMMY)
		newArgumentBindle := newBindle
		newArgumentBindle.argNo++
		typesFromNextArgument = cp.generateNewArgument(mc, &newArgumentBindle)
		backtrack2 = mc.codeTop()
		cp.emit(mc, Jmp, DUMMY)
		mc.code[backtrack1].args[2] = mc.codeTop()
	}

	typesFromContinuingInTuple := cp.generateFromTopBranchDown(mc, &newBindle)

	if needsConditional {
		mc.code[backtrack2].args[0] = backtrack2
	}

	return typesFromContinuingInTuple.union(typesFromNextArgument)
}

func (cp *Compiler) generateMoveAlongBranchViaSingleValue(mc *Vm, b *bindle) alternateType {
	newBindle := *b
	newBindle.treePosition = b.treePosition.Branch[b.branchNo].Node
	newBindle.argNo++
	return cp.generateNewArgument(mc, &newBindle)
}

func (cp *Compiler) generateNextBranchDown(mc *Vm, b *bindle) alternateType {
	newBindle := *b
	newBindle.branchNo++
	return cp.generateBranch(mc, &newBindle)
}

func (cp *Compiler) seekFunctionCall(mc *Vm, b *bindle) alternateType {
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
				functionAndType.f(cp, mc, b.tok, b.outLoc, b.valLocs)
				return functionAndType.t
			}
			cp.emitFunctionCall(mc, fNo, b.valLocs)
			cp.emit(mc, Asgm, b.outLoc, cp.fns[fNo].outReg) // Because the different implementations of the function will have their own out register.
			return cp.fns[fNo].types                        // TODO : Is there a reason why this should be so?
		}
	}
	cp.reserveError(mc, "mc/types/b", b.tok, []any{}) // TODO : the bindle can accumulate the types to allow us to generates this error properly.
	cp.emit(mc, Asgm, b.outLoc, mc.that())
	return altType(values.ERROR)
}

func (cp *Compiler) seekBling(mc *Vm, b *bindle, bling string) alternateType {
	for i, branch := range b.treePosition.Branch {
		if branch.TypeName == bling {
			newBindle := *b
			newBindle.branchNo = i
			newBindle.tupleTime = false
			return cp.generateMoveAlongBranchViaSingleValue(mc, &newBindle)
		}
	}
	cp.p.Throw("comp/eq/err/a", b.tok) // TODO -- the bindle should pass all the original args or at least their tokens for better error messages.
	return altType(values.ERROR)
}

// We have two different ways of emiting an opcode: 'emit' does it the regular way, 'put' ensures that
// the destination is the next free memory address.
func (cp *Compiler) emit(mc *Vm, opcode opcode, args ...uint32) {
	mc.code = append(mc.code, makeOp(opcode, args...))
	if SHOW_COMPILE {
		println(mc, mc.describeCode(mc.codeTop()-1))
	}
}

func (cp *Compiler) put(mc *Vm, opcode opcode, args ...uint32) {
	args = append([]uint32{mc.memTop()}, args...)
	cp.emit(mc, opcode, args...)
	mc.mem = append(mc.mem, values.Value{})
}

// Reput puts the value in the last memory address to be used.
func (cp *Compiler) reput(mc *Vm, opcode opcode, args ...uint32) {
	args = append([]uint32{mc.that()}, args...)
	cp.emit(mc, opcode, args...)
}

func (cp *Compiler) emitFunctionCall(mc *Vm, funcNumber uint32, valLocs []uint32) {
	args := append([]uint32{cp.fns[funcNumber].callTo, cp.fns[funcNumber].loReg, cp.fns[funcNumber].hiReg}, valLocs...)
	cp.emit(mc, Call, args...)
}

func (cp *Compiler) emitEquals(mc *Vm, node *ast.InfixExpression, env *environment) (alternateType, bool) {
	lTypes, lcst := cp.compileNode(mc, node.Args[0], env)
	if lTypes.only(tp(values.ERROR)) {
		cp.p.Throw("comp/eq/err/a", node.GetToken())
		return altType(values.ERROR), true
	}
	leftRg := mc.that()
	rTypes, rcst := cp.compileNode(mc, node.Args[2], env)
	if rTypes.only(tp(values.ERROR)) {
		cp.p.Throw("comp/eq/err/b", node.GetToken())
		return altType(values.ERROR), true
	}
	rightRg := mc.that()
	oL := lTypes.intersect(rTypes)
	if oL.only(tp(values.ERROR)) {
		cp.p.Throw("comp/eq/err/c", node.GetToken())
		return altType(values.ERROR), true
	}
	if len(oL) == 0 {
		cp.p.Throw("comp/eq/types", node.GetToken())
		return altType(values.ERROR), true
	}
	if len(oL) == 1 && len(lTypes) == 1 && len(rTypes) == 1 {
		switch el := oL[0].(type) {
		case simpleType:
			switch el {
			case tp(values.INT):
				cp.put(mc, Equi, leftRg, rightRg)
			case tp(values.STRING):
				cp.put(mc, Equs, leftRg, rightRg)
			case tp(values.BOOL):
				cp.put(mc, Equb, leftRg, rightRg)
			case tp(values.FLOAT):
				cp.put(mc, Equf, leftRg, rightRg)
			default:
				panic("Unimplemented comparison type.")
			}
			return altType(values.BOOL), lcst && rcst
		default:
			panic("Unimplemented comparison type.")
		}
	} else {
		panic("Haven't implemented this bit because of having no way to test it at this point.")
	}
}

// The various 'streaming operators'. TODO, find different name.

func (cp *Compiler) compilePipe(mc *Vm, lhsTypes alternateType, rhs ast.Node, env *environment) (alternateType, bool) {
	// If we have a single identifier, we wish it to contain a function ...
	if rhs.GetToken().Type == token.IDENT {
		v, ok := env.getVar(rhs.GetToken().Literal)
		if ok && v.types.contains(tp(values.FUNC)) {
			if v.types.only(tp(values.FUNC)) {
				cp.put(mc, Dofn, v.mLoc, mc.that())
				return ANY_TYPE, ALL_CONST_ACCESS.Contains(v.access)
			} else {
				// Emit some error handling.

			}
		}
	}
	// Otherwise we are in the presence of a 'that' expression. We can simply compile and return.
	return cp.compileNode(mc, rhs, env)
}

func (cp *Compiler) compileMapping(mc *Vm, lhsTypes alternateType, rhs ast.Node, env *environment) (alternateType, bool) {
	return alternateType{}, false
}

func (cp *Compiler) compileFilter(mc *Vm, lhsTypes alternateType, rhs ast.Node, env *environment) (alternateType, bool) {
	return alternateType{}, false
}
