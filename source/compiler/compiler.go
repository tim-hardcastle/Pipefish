package compiler

import (
	"pipefish/source/ast"
	"pipefish/source/object"
	"pipefish/source/parser"
	"pipefish/source/set"
	"pipefish/source/token"
	"pipefish/source/values"
	"pipefish/source/vm"
)

const SHOW_COMPILE = true
const SHOW_RUN = true

type thunk struct {
	mLoc uint32
	cLoc uint32
}

type Compiler struct {
	p                  *parser.Parser
	mc                 *vm.Vm
	enumElements       map[string]uint32
	fieldLabels        map[string]uint32
	structNumbers      map[string]values.ValueType
	gconsts            *environment
	gvars              *environment
	fns                []*cpFunc
	typeNameToTypeList map[string]alternateType

	tupleType uint32 // Location of a constant saying {TYPE, <type number of tuples>}

	// Very temporary state. Arguably shouldn't be here.
	thunkList []thunk
}

type cpFunc struct {
	callTo   uint32
	loReg    uint32
	hiReg    uint32
	outReg   uint32
	tupleReg uint32
	types    alternateType
	builtin  string // A non-empty string in case it is a builtin.
}

const DUMMY = 4294967295

func NewCompiler(p *parser.Parser) *Compiler {
	return &Compiler{
		p:             p,
		mc:            vm.BlankVm(),
		enumElements:  make(map[string]uint32),
		fieldLabels:   make(map[string]uint32),
		structNumbers: make(map[string]values.ValueType),
		gconsts:       newEnvironment(),
		gvars:         newEnvironment(),
		thunkList:     []thunk{},
		fns:           []*cpFunc{},
		typeNameToTypeList: map[string]alternateType{
			"int":      altType(values.INT),
			"string":   altType(values.STRING),
			"bool":     altType(values.BOOL),
			"float64":  altType(values.FLOAT),
			"error":    altType(values.ERROR),
			"type":     altType(values.TYPE),
			"pair":     altType(values.PAIR),
			"list":     altType(values.LIST),
			"map":      altType(values.MAP),
			"set":      altType(values.SET),
			"label":    altType(values.LABEL),
			"int?":     altType(values.NULL, values.INT),
			"string?":  altType(values.NULL, values.STRING),
			"bool?":    altType(values.NULL, values.BOOL),
			"float64?": altType(values.NULL, values.FLOAT),
			"type?":    altType(values.NULL, values.TYPE),
			"pair?":    altType(values.NULL, values.PAIR),
			"list?":    altType(values.NULL, values.LIST),
			"map?":     altType(values.NULL, values.MAP),
			"set?":     altType(values.NULL, values.SET),
			"label?":   altType(values.NULL, values.LABEL),
			"null":     altType(values.NULL),
			"single":   altType(values.INT, values.BOOL, values.STRING, values.FLOAT, values.TYPE, values.FUNC, values.PAIR, values.LIST, values.MAP, values.SET, values.LABEL),
			"single?":  altType(values.NULL, values.INT, values.BOOL, values.STRING, values.FLOAT, values.TYPE, values.FUNC, values.PAIR, values.LIST, values.MAP, values.SET, values.LABEL),
			"struct":   altType(),
			"struct?":  altType(values.NULL),
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
	mT := cp.mc.MemTop()
	cT := cp.mc.CodeTop()
	tT := cp.mc.TokenTop()
	lT := cp.mc.LfTop()
	node := cp.p.ParseLine("REPL input", line)
	if cp.p.ErrorsExist() {
		return values.Value{T: values.ERROR}
	}
	cp.compileNode(cp.mc, node, cp.gvars)
	if cp.p.ErrorsExist() {
		return values.Value{T: values.ERROR}
	}
	cp.emit(cp.mc, vm.Ret)
	cp.mc.Run(cT)
	result := cp.mc.Mem[cp.mc.That()]
	cp.mc.Mem = cp.mc.Mem[:mT]
	cp.mc.Code = cp.mc.Code[:cT]
	cp.mc.Tokens = cp.mc.Tokens[:tT]
	cp.mc.LambdaFactories = cp.mc.LambdaFactories[:lT]
	return result
}

func (cp *Compiler) Describe(v values.Value) string {
	return cp.mc.Literal(v)
}

func (cp *Compiler) Compile(source, sourcecode string) {
	cp.mc = vm.BlankVm()
	node := cp.p.ParseLine(source, sourcecode)
	cp.compileNode(cp.mc, node, cp.gvars)
	cp.emit(cp.mc, vm.Ret)
}

func (cp *Compiler) reserve(mc *vm.Vm, t values.ValueType, v any) uint32 {
	mc.Mem = append(mc.Mem, values.Value{T: t, V: v})
	return uint32(len(mc.Mem) - 1)
}

func (cp *Compiler) reserveError(mc *vm.Vm, ec string, tok *token.Token, args []any) uint32 {
	mc.Mem = append(mc.Mem, values.Value{T: values.ERROR, V: &object.Error{ErrorId: ec, Token: tok, Args: append([]any{mc}, args...), Trace: make([]*token.Token, 0, 10)}})
	return uint32(len(mc.Mem) - 1)
}

func (cp *Compiler) reserveToken(mc *vm.Vm, tok *token.Token) uint32 {
	mc.Tokens = append(mc.Tokens, tok)
	return uint32(len(mc.Tokens) - 1)
}

func (cp *Compiler) reserveLambdaFactory(mc *vm.Vm, env *environment, fnNode *ast.FuncExpression, tok *token.Token) (uint32, bool) {
	LF := &vm.LambdaFactory{Model: &vm.Lambda{}}
	LF.Model.Mc = vm.BlankVm()
	LF.Model.Mc.Code = []*vm.Operation{}
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
		cp.reserve(LF.Model.Mc, 0, DUMMY) // It doesn't matter what we put in here 'cos we copy the values any time we call the LambdaFactory.
		cp.addVariable(LF.Model.Mc, newEnv, k, v.access, v.types)
		// At the same time, the lambda factory need to know where they are in the calling vm.Vm.
		LF.ExtMem = append(LF.ExtMem, v.mLoc)
	}

	LF.Model.ExtTop = LF.Model.Mc.MemTop()

	// Add the function parameters.
	for _, pair := range sig { // It doesn't matter what we put in here either, because we're going to have to copy the values any time we call the function.
		cp.reserve(LF.Model.Mc, 0, DUMMY)
		cp.addVariable(LF.Model.Mc, newEnv, pair.VarName, FUNCTION_ARGUMENT, cp.typeNameToTypeList[pair.VarType])
	}

	LF.Model.PrmTop = LF.Model.Mc.MemTop()

	// Compile the locals.

	if fnNode.Given != nil {
		cp.thunkList = []thunk{}
		cp.compileNode(LF.Model.Mc, fnNode.Given, newEnv)
		for _, pair := range cp.thunkList {
			cp.emit(LF.Model.Mc, vm.Thnk, pair.mLoc, pair.cLoc)
		}
	}

	// Function starts here.

	LF.Model.LocToCall = LF.Model.Mc.CodeTop()

	// We have to typecheck inside the lambda, because the calling site doesn't know which function it's calling.

	// TODO !!!

	// Now we can emit the main body of the function.

	cp.compileNode(LF.Model.Mc, fnNode.Body, newEnv)
	LF.Model.Dest = LF.Model.Mc.That()
	LF.Size = LF.Model.Mc.CodeTop()
	cp.emit(LF.Model.Mc, vm.Ret)

	// We have made our lambda factory!

	mc.LambdaFactories = append(mc.LambdaFactories, LF)
	return uint32(len(mc.LambdaFactories) - 1), externals.IsEmpty() // A lambda which doesn't close over anything is a constant.
}

func (cp *Compiler) addVariable(mc *vm.Vm, env *environment, name string, acc varAccess, types alternateType) {
	env.data[name] = variable{mLoc: mc.That(), access: acc, types: types}
}

func (cp *Compiler) compileNode(mc *vm.Vm, node ast.Node, env *environment) (alternateType, bool) {
	rtnTypes, rtnConst := alternateType{}, true
	mT := mc.MemTop()
	cT := mc.CodeTop()
	tT := cp.mc.TokenTop()
	lT := cp.mc.LfTop()
	switch node := node.(type) {
	case *ast.AssignmentExpression:
		if node.Token.Type == token.GVN_ASSIGN {
			// TODO --- need to do this better after we implement tuples
			if node.Left.GetToken().Type != token.IDENT {
				cp.p.Throw("comp/assign/ident", node.Left.GetToken())
				rtnTypes, rtnConst = altType(values.ERROR), true
				break
			}
			thunkStart := mc.Next()
			types, cst := cp.compileNode(mc, node.Right, env)
			cp.emit(mc, vm.Ret)
			if cst {
				cp.addVariable(mc, env, node.Left.(*ast.Identifier).Value, LOCAL_TRUE_CONSTANT, types)
				rtnTypes, rtnConst = altType(values.CREATED_LOCAL_CONSTANT), true
				break
			}
			cp.addVariable(mc, env, node.Left.(*ast.Identifier).Value, LOCAL_CONSTANT_THUNK, types)
			cp.thunkList = append(cp.thunkList, thunk{mc.That(), thunkStart})
			rtnTypes, rtnConst = altType(values.CREATED_LOCAL_CONSTANT), false
			break
		}
		cp.p.Throw("comp/assign", node.GetToken())
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
		cp.put(mc, vm.Mkfn, facNo)
		rtnTypes, rtnConst = altType(values.FUNC), isConst
		break
	case *ast.Identifier:
		enumElement, ok := cp.enumElements[node.Value]
		if ok {
			cp.put(mc, vm.Asgm, enumElement)
			rtnTypes, rtnConst = altType(mc.Mem[enumElement].T), true
			break
		}
		labelNumber, ok := cp.fieldLabels[node.Value]
		if ok {
			cp.put(mc, vm.Asgm, labelNumber)
			rtnTypes, rtnConst = altType(values.LABEL), true
			break
		}
		v, ok := env.getVar(node.Value)
		if !ok {
			cp.p.Throw("comp/ident/known", node.GetToken())
			break
		}
		if v.access == LOCAL_CONSTANT_THUNK {
			cp.emit(mc, vm.Untk, v.mLoc)
		}
		if v.access == REFERENCE_VARIABLE {
			cp.put(mc, vm.Dref, v.mLoc)
			rtnTypes = cp.typeNameToTypeList["single?"]
		} else {
			cp.put(mc, vm.Asgm, v.mLoc)
			rtnTypes = v.types
		}
		rtnConst = ALL_CONST_ACCESS.Contains(v.access)
		break
	case *ast.IndexExpression:
		containerType, ctrConst := cp.compileNode(mc, node.Left, env)
		container := mc.That()
		indexType, idxConst := cp.compileNode(mc, node.Index, env)
		index := mc.That()
		rtnConst = ctrConst && idxConst
		// Things we can index:
		// Lists, by integers; or a pair for a slice.
		// Tuples, ditto.
		// Strings, ditto.
		// Pairs, by integers.
		// Names of enum types, by integers. Query, add slice too?
		// Maps, by any value we can Compare with another value.
		// Structs, by a label, preferably an appropriate one.

		if containerType.isOnly(values.LIST) {
			if indexType.isOnly(values.INT) {
				boundsError := cp.reserveError(mc, "vm/list/index", &node.Token, []any{})
				cp.put(mc, vm.IdxL, container, index, boundsError)
				break
			}
			if indexType.isOnly(values.PAIR) {
				boundsError := cp.reserveError(mc, "vm/list/slice", &node.Token, []any{})
				cp.put(mc, vm.SliL, container, index, boundsError)
				break
			}
			if indexType.isNoneOf(values.INT, values.PAIR) {
				cp.p.Throw("comp/list/index", node.GetToken())
				break
			}
		}
		if containerType.isOnly(values.STRING) {
			if indexType.isOnly(values.INT) {
				boundsError := cp.reserveError(mc, "vm/string/index", &node.Token, []any{})
				cp.put(mc, vm.Idxs, container, index, boundsError)
				break
			}
			if indexType.isOnly(values.PAIR) {
				boundsError := cp.reserveError(mc, "vm/string/slice", &node.Token, []any{})
				cp.put(mc, vm.Slis, container, index, boundsError)
				break
			}
			if indexType.isNoneOf(values.INT, values.PAIR) {
				cp.p.Throw("comp/string/index", node.GetToken())
				break
			}
		}
		if containerType.containsOnlyTuples() {
			if indexType.isOnly(values.INT) {
				boundsError := cp.reserveError(mc, "vm/tuple/index", &node.Token, []any{})
				cp.put(mc, vm.IdxT, container, index, boundsError)
				break
			}
			if indexType.isOnly(values.PAIR) {
				boundsError := cp.reserveError(mc, "vm/tuple/slice", &node.Token, []any{})
				cp.put(mc, vm.SliT, container, index, boundsError)
				break
			}
			if indexType.isNoneOf(values.INT, values.PAIR) {
				cp.p.Throw("comp/tuple/index", node.GetToken())
				break
			}
		}
		if containerType.isOnly(values.PAIR) {
			if indexType.isOnly(values.INT) {
				boundsError := cp.reserveError(mc, "vm/pair/index", &node.Token, []any{})
				cp.put(mc, vm.Idxp, container, index, boundsError)
				break
			}
			if indexType.isNoneOf(values.INT) {
				cp.p.Throw("comp/pair/index", node.GetToken())
				break
			}
		}
		if containerType.isOnly(values.TYPE) {
			if indexType.isOnly(values.INT) {
				enumError := cp.reserveError(mc, "vm/type/enum", &node.Token, []any{})
				boundsError := cp.reserveError(mc, "vm/type/index", &node.Token, []any{})
				cp.put(mc, vm.Idxt, container, index, enumError, boundsError)
				break
			}
			if indexType.isNoneOf(values.INT) {
				cp.p.Throw("comp/type/index", node.GetToken())
				break
			}
		}
		structType, ok := containerType.isOnlyStruct(int(mc.Ub_enums))
		if ok {
			if indexType.isOnly(values.LABEL) {
				if idxConst { // Then we can find the field number of the struct at compile time and throw away the computed label.
					structNumber := structType - mc.Ub_enums
					indexNumber := mc.Mem[index].V.(int)
					fieldNumber := mc.StructResolve.Resolve(int(structNumber), indexNumber)
					if fieldNumber == -1 {
						cp.p.Throw("comp/struct/index", node.GetToken())
						break
					}
					cp.put(mc, vm.IxZn, container, uint32(fieldNumber))
					break
				}
				boundsError := cp.reserveError(mc, "vm/struct/index", &node.Token, []any{})
				cp.put(mc, vm.IxZl, container, index, boundsError)
				break
			}
			if indexType.isNoneOf(values.LABEL) {
				cp.p.Throw("comp/struct/index", node.GetToken())
				break
			}
		}
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
			cp.put(mc, vm.Notb, mc.That())
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
			if !lTypes.contains(values.BOOL) {
				cp.p.Throw("comp/or/bool/left", node.GetToken())
				break
			}
			leftRg := mc.That()
			cp.emit(mc, vm.Qtru, leftRg, mc.Next()+2)
			backtrack := mc.Next()
			cp.emit(mc, vm.Jmp, DUMMY)
			rTypes, rcst := cp.compileNode(mc, node.Right, env)
			if !rTypes.contains(values.BOOL) {
				cp.p.Throw("comp/or/bool/right", node.GetToken())
				break
			}
			rightRg := mc.That()
			mc.Code[backtrack].Args[0] = mc.Next()
			cp.put(mc, vm.Orb, leftRg, rightRg)
			rtnTypes, rtnConst = altType(values.BOOL), lcst && rcst
			break
		}
		if node.Operator == "and" {
			lTypes, lcst := cp.compileNode(mc, node.Left, env)
			if !lTypes.contains(values.BOOL) {
				cp.p.Throw("comp/and/bool/left", node.GetToken())
				break
			}
			leftRg := mc.That()
			backtrack := mc.Next()
			cp.emit(mc, vm.Qtru, leftRg, DUMMY)
			rTypes, rcst := cp.compileNode(mc, node.Right, env)
			if !rTypes.contains(values.BOOL) {
				cp.p.Throw("comp/and/bool/right", node.GetToken())
				break
			}
			rightRg := mc.That()
			mc.Code[backtrack].Args[1] = mc.Next()
			cp.put(mc, vm.Andb, leftRg, rightRg)
			rtnTypes, rtnConst = altType(values.BOOL), lcst && rcst
			break
		}
		if node.Operator == ":" {
			if node.Left.GetToken().Type == token.ELSE {
				rtnTypes, rtnConst = cp.compileNode(mc, node.Right, env)
				break
			}
			lTypes, lcst := cp.compileNode(mc, node.Left, env)
			if !lTypes.contains(values.BOOL) {
				cp.p.Throw("comp/cond/bool", node.GetToken())
				break
			}
			leftRg := mc.That()
			backtrack := mc.Next()
			cp.emit(mc, vm.Qtru, leftRg, DUMMY)
			rTypes, rcst := cp.compileNode(mc, node.Right, env)
			cp.put(mc, vm.Asgm, mc.That())
			cp.emit(mc, vm.Jmp, mc.Next()+2)
			mc.Code[backtrack].Args[1] = mc.Next()
			cp.reput(mc, vm.Asgm, values.C_U_OBJ)
			rtnTypes, rtnConst = rTypes.union(altType(values.UNSAT)), lcst && rcst
			break
		}
		if node.Operator == ";" {
			lTypes, lcst := cp.compileNode(mc, node.Left, env)
			// We deal with the case where the newline is separating local constant definitions
			// in the 'given' block.
			if lTypes.isOnly(values.CREATED_LOCAL_CONSTANT) {
				_, cst := cp.compileNode(mc, node.Right, env)
				rtnTypes, rtnConst = altType(values.CREATED_LOCAL_CONSTANT), lcst && cst
				break
			}
			leftRg := mc.That()
			backtrack := mc.Next()
			cp.emit(mc, vm.Qtyp, leftRg, uint32(values.UNSAT), DUMMY)
			rTypes, rcst := cp.compileNode(mc, node.Right, env)
			rightRg := mc.That()
			cp.put(mc, vm.Asgm, rightRg)
			cp.emit(mc, vm.Jmp, mc.Next()+2)
			mc.Code[backtrack].Args[2] = mc.Next()
			cp.reput(mc, vm.Asgm, leftRg)
			if !(lTypes.contains(values.UNSAT) && rTypes.contains(values.UNSAT)) {
				rtnTypes, rtnConst = lTypes.union(rTypes).without(tp(values.UNSAT)), lcst && rcst
				break
			}
			rtnTypes, rtnConst = lTypes.union(rTypes), lcst && rcst
			break
		}
	case *ast.ListExpression:
		var containedTypes alternateType
		containedTypes, rtnConst = cp.compileNode(mc, node.List, env)
		backTrackTo, failed := cp.emitErrorBoilerplate(mc, containedTypes, "comp/list/err", node.GetToken(), false)
		if failed {
			break
		}
		cp.put(mc, vm.List, mc.That())
		mc.Code[backTrackTo].Args[0] = mc.That()
		rtnTypes = altType(values.LIST)
		break
	case *ast.PrefixExpression:
		if node.Operator == "not" {
			allTypes, cst := cp.compileNode(mc, node.Args[0], env)
			if allTypes.isOnly(values.BOOL) {
				cp.put(mc, vm.Notb, mc.That())
				rtnTypes, rtnConst = altType(values.BOOL), cst
				break
			}
			if !allTypes.contains(values.BOOL) {
				cp.p.Throw("comp/not/bool", node.GetToken())
				break
			}
		}
		v, ok := env.getVar(node.Operator)
		if ok && v.types.contains(values.FUNC) {
			operands := []uint32{v.mLoc}
			for _, arg := range node.Args {
				cp.compileNode(mc, arg, env)
				operands = append(operands, mc.That())
			}
			if cp.p.ErrorsExist() {
				rtnTypes, rtnConst = altType(values.ERROR), true
				break
			}
			if v.types.isOnly(values.FUNC) { // Then no type checking for v.
				cp.put(mc, vm.Dofn, operands...)
			}
			rtnTypes = ANY_TYPE
			break
		}
		if cp.p.Prefixes.Contains(node.Operator) || cp.p.Functions.Contains(node.Operator) {
			rtnTypes, rtnConst = cp.createFunctionCall(mc, node, env)
			break
		}
		cp.p.Throw("comp/prefix/known", node.GetToken())
		break
	case *ast.StreamingExpression: // I.e. -> >> and -> and ?> .
		lhsTypes, lhsConst := cp.compileNode(mc, node.Left, env)
		if cp.p.ErrorsExist() {
			rtnTypes, rtnConst = altType(values.ERROR), true
			break
		}
		// And that's about all the streaming operators really do have in common under the hood, so let's do a switch on the operators.
		var rhsConst bool
		switch node.Operator {
		case "->":
			rtnTypes, rhsConst = cp.compilePipe(mc, lhsTypes, lhsConst, node.Right, env)
		case ">>":
			rtnTypes, rhsConst = cp.compileMappingOrFilter(mc, lhsTypes, lhsConst, node.Right, env, false)
		default:
			rtnTypes, rhsConst = cp.compileMappingOrFilter(mc, lhsTypes, lhsConst, node.Right, env, true)
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
		cp.reserve(mc, values.TYPE, values.ValueType(cp.typeNameToTypeList[node.Value][0].(simpleType)))
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
	default:
		panic("Unimplemented node type.")
	}
	if cp.p.ErrorsExist() {
		return altType(values.ERROR), false
	}
	if rtnConst && mc.CodeTop() > cT {
		cp.emit(mc, vm.Ret)
		mc.Run(cT)
		result := mc.Mem[mc.That()]
		mc.Mem = mc.Mem[:mT]
		mc.Code = mc.Code[:cT]
		cp.mc.Tokens = cp.mc.Tokens[:tT]
		cp.mc.LambdaFactories = cp.mc.LambdaFactories[:lT]
		cp.reserve(mc, result.T, result.V)
	}
	return rtnTypes, rtnConst
}

// This needs its own very special logic because the type it returns has to be composed in a different way from all the other operators.
func (cp *Compiler) emitComma(mc *vm.Vm, node *ast.InfixExpression, env *environment) (alternateType, bool) {
	lTypes, lcst := cp.compileNode(mc, node.Args[0], env)
	if lTypes.isOnly(values.ERROR) {
		cp.p.Throw("comp/tuple/err/a", node.GetToken())
	}
	left := mc.That()
	rTypes, rcst := cp.compileNode(mc, node.Args[2], env)
	if rTypes.isOnly(values.ERROR) {
		cp.p.Throw("comp/tuple/err/b", node.GetToken())
	}
	right := mc.That()
	var leftBacktrack, rightBacktrack uint32
	if lTypes.contains(values.ERROR) {
		cp.emit(mc, vm.Qtyp, left, uint32(tp(values.ERROR)), mc.CodeTop()+2)
		leftBacktrack = mc.CodeTop()
		cp.put(mc, vm.Asgm, DUMMY, left)
		if rTypes.contains(values.ERROR) {
			cp.emit(mc, vm.Jmp, mc.CodeTop()+5)
		} else {
			cp.emit(mc, vm.Jmp, mc.CodeTop()+2)
		}
	}
	if rTypes.contains(values.ERROR) {
		cp.emit(mc, vm.Qtyp, right, uint32(tp(values.ERROR)), mc.CodeTop()+2)
		rightBacktrack = mc.CodeTop()
		cp.put(mc, vm.Asgm, right)
		cp.emit(mc, vm.Jmp, mc.CodeTop()+2)
	}
	leftMustBeSingle, leftMustBeTuple := lTypes.mustBeSingleOrTuple()
	rightMustBeSingle, rightMustBeTuple := rTypes.mustBeSingleOrTuple()
	switch {
	case leftMustBeSingle && rightMustBeSingle:
		cp.put(mc, vm.Cc11, left, right)
	case leftMustBeSingle && rightMustBeTuple:
		cp.put(mc, vm.Cc1T, left, right)
	case leftMustBeTuple && rightMustBeSingle:
		cp.put(mc, vm.CcT1, left, right)
	case leftMustBeTuple && rightMustBeTuple:
		cp.put(mc, vm.CcTT, left, right)
	default:
		cp.put(mc, vm.Ccxx, left, right) // We can after all let the operation dispatch for us.
	}
	if lTypes.contains(values.ERROR) {
		mc.Code[leftBacktrack].Args[0] = mc.That()
	}
	if rTypes.contains(values.ERROR) {
		mc.Code[rightBacktrack].Args[0] = mc.That()
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

func (cp *Compiler) emitErrorBoilerplate(mc *vm.Vm, types alternateType, errCode string, tok *token.Token, appendToken bool) (uint32, bool) {
	failed := false
	var backtrackTo uint32
	if types.isOnly(values.ERROR) {
		cp.p.Throw("comp/list/err", tok)
		failed = true
	}
	if types.contains(values.ERROR) {
		cp.emit(mc, vm.Qtyp, mc.That(), uint32(values.ERROR), mc.CodeTop()+2)
		backtrackTo = mc.CodeTop()
		if appendToken {
			cp.reserveToken(mc, tok)
			cp.emit(mc, vm.Adtk, DUMMY, mc.That(), mc.ThatToken())
		} else {
			cp.emit(mc, vm.Asgm, DUMMY, mc.That())
		}
		cp.emit(mc, vm.Ret)
	}
	return backtrackTo, failed
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

func (cp *Compiler) createFunctionCall(mc *vm.Vm, node ast.Callable, env *environment) (alternateType, bool) {
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
				cp.put(mc, vm.Asgm, v.mLoc)
				b.valLocs[i] = mc.That()
			} else {
				cp.reserve(mc, values.REF, v.mLoc)
				b.valLocs[i] = mc.That()
			}
			continue
		}
		switch arg := arg.(type) { // It might be bling.
		case *ast.Bling:
			b.types[i] = alternateType{blingType{arg.Value}}
			b.valLocs[i] = values.C_BLING
		default: // Otherwise we emit code to evaluate it.
			b.types[i], cstI = cp.compileNode(mc, arg, env)
			cst = cst && cstI
			b.valLocs[i] = mc.That()
			if b.types[i].(alternateType).isOnly(values.ERROR) {
				cp.p.Throw("comp/arg/error", node.GetToken())
				return altType(values.ERROR), true
			}
			if b.types[i].(alternateType).contains(values.ERROR) {
				cp.emit(mc, vm.Qtyp, mc.That(), uint32(tp(values.ERROR)), mc.CodeTop()+2)
				backtrackList[i] = mc.CodeTop()
				cp.emit(mc, vm.Asgm, DUMMY, mc.That(), mc.ThatToken())
				cp.emit(mc, vm.Ret)
			}
		}
	}
	// Having gotten the arguments, we create the function call itself.
	returnTypes := cp.generateNewArgument(mc, b) // This is our path into the recursion that will in fact generate the whole function call.

	cp.put(mc, vm.Asgm, b.outLoc)
	if returnTypes.isOnly(values.ERROR) && node.GetToken().Literal != "error" {
		cp.p.Throw("comp/call", b.tok)
	}
	for _, v := range backtrackList {
		if v != DUMMY {
			mc.Code[v].Args[0] = mc.That()
		}
	}
	if returnTypes.contains(values.ERROR) {
		if !traceTokenReserved {
			cp.reserveToken(mc, b.tok)
			traceTokenReserved = true
		}
		cp.emit(mc, vm.Qtyp, mc.That(), uint32(values.ERROR), mc.CodeTop()+3)
		cp.emit(mc, vm.Adtk, mc.That(), mc.That(), mc.ThatToken())
		cp.emit(mc, vm.Ret)
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

func (cp *Compiler) generateNewArgument(mc *vm.Vm, b *bindle) alternateType {
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
	// Case (3) : we're in tuple time.
	if b.tupleTime {
		newBindle := *b
		newBindle.argNo++
		return cp.generateNewArgument(mc, &newBindle)
	}
	// Case (4) : we have a reference.
	if b.treePosition.Branch[b.branchNo].TypeName == "ref" {
		newBindle := *b
		newBindle.treePosition = b.treePosition.Branch[b.branchNo].Node
		newBindle.argNo++
		return cp.generateNewArgument(mc, &newBindle)
	}
	// Case (5) : We aren't yet at the end of the list of arguments.
	newBindle := *b
	newBindle.index = 0
	return cp.generateFromTopBranchDown(mc, b)
}

func (cp *Compiler) generateFromTopBranchDown(mc *vm.Vm, b *bindle) alternateType {
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
func (cp *Compiler) generateBranch(mc *vm.Vm, b *bindle) alternateType {
	if b.tupleTime || b.branchNo < len(b.treePosition.Branch) && b.treePosition.Branch[b.branchNo].TypeName == "tuple" { // We can move on to the next argument.
		newBindle := *b
		newBindle.treePosition = b.treePosition.Branch[b.branchNo].Node
		newBindle.tupleTime = true
		newBindle.argNo++
		return cp.generateNewArgument(mc, &newBindle)
	}
	if b.branchNo >= len(b.treePosition.Branch) { // We've tried all the alternatives and have some left over.
		cp.reserveError(mc, "mc/types/a", b.tok, []any{})
		cp.emit(mc, vm.Asgm, b.outLoc, mc.That())
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
	branchBacktrack := mc.CodeTop()
	if needsOtherBranch {
		// Then we need to generate a conditional. Which one exactly depends on whether we're looking at a single, a tuple, or both.
		switch len(acceptedSingleTypes) {
		case 0:
			cp.put(mc, vm.IxTn, b.valLocs[b.argNo], uint32(b.index))
			cp.emitTypeComparison(mc, branch.TypeName, mc.That(), DUMMY)
		case len(overlap):
			cp.emitTypeComparison(mc, branch.TypeName, b.valLocs[b.argNo], DUMMY)
		default:
			cp.emit(mc, vm.QsnQ, b.valLocs[b.argNo], mc.CodeTop()+3)
			cp.emitTypeComparison(mc, branch.TypeName, b.valLocs[b.argNo], DUMMY)
			cp.emit(mc, vm.Jmp, mc.CodeTop()+3)
			cp.put(mc, vm.IxTn, b.valLocs[b.argNo], uint32(b.index))
			cp.emitTypeComparison(mc, branch.TypeName, mc.That(), DUMMY)
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
		backtrack := mc.CodeTop()
		cp.emit(mc, vm.QsnQ, b.valLocs[b.argNo], DUMMY)
		typesFromSingles := cp.generateMoveAlongBranchViaSingleValue(mc, &newBindle)
		cp.emit(mc, vm.Jmp, DUMMY)
		mc.Code[backtrack].MakeLastArg(mc.CodeTop())
		backtrack = mc.CodeTop()
		typesFromTuples := cp.generateMoveAlongBranchViaTupleElement(mc, &newBindle)
		mc.Code[backtrack].MakeLastArg(mc.CodeTop())
		typesFromGoingAcross = typesFromSingles.union(typesFromTuples)
	}
	// And now we need to do the 'else' branch if there is one.
	if needsOtherBranch {
		elseBacktrack := mc.CodeTop()
		cp.emit(mc, vm.Jmp, DUMMY) // The last part of the 'if' branch: jumps over the 'else'.
		// We need to backtrack on whatever conditional we generated.
		switch len(acceptedSingleTypes) {
		case 0:
			mc.Code[branchBacktrack+1].MakeLastArg(mc.CodeTop())
		case len(overlap):
			mc.Code[branchBacktrack].MakeLastArg(mc.CodeTop())
		default:
			mc.Code[branchBacktrack+1].MakeLastArg(mc.CodeTop())
			mc.Code[branchBacktrack+4].MakeLastArg(mc.CodeTop())
		}
		// We recurse on the next branch down.
		typesFromGoingDown = cp.generateNextBranchDown(mc, &newBindle)
		mc.Code[elseBacktrack].MakeLastArg(mc.CodeTop())
	}
	return typesFromGoingAcross.union(typesFromGoingDown)
}

var TYPE_COMPARISONS = map[string]*vm.Operation{
	"int":     {vm.Qtyp, []uint32{DUMMY, uint32(values.INT), DUMMY}},
	"string":  {vm.Qtyp, []uint32{DUMMY, uint32(values.STRING), DUMMY}},
	"bool":    {vm.Qtyp, []uint32{DUMMY, uint32(values.BOOL), DUMMY}},
	"float64": {vm.Qtyp, []uint32{DUMMY, uint32(values.FLOAT), DUMMY}},
	"null":    {vm.Qtyp, []uint32{DUMMY, uint32(values.NULL), DUMMY}},
	"list":    {vm.Qtyp, []uint32{DUMMY, uint32(values.LIST), DUMMY}},
	"set":     {vm.Qtyp, []uint32{DUMMY, uint32(values.SET), DUMMY}},
	"func":    {vm.Qtyp, []uint32{DUMMY, uint32(values.FUNC), DUMMY}},
	"single":  {vm.Qsng, []uint32{DUMMY, DUMMY}},
	"single?": {vm.QsnQ, []uint32{DUMMY, DUMMY}},
}

func (cp *Compiler) emitTypeComparison(mc *vm.Vm, typeAsString string, mem, loc uint32) {
	op, ok := TYPE_COMPARISONS[typeAsString]
	if ok {
		newArgs := make([]uint32, len(op.Args))
		copy(newArgs, op.Args)
		newOp := &vm.Operation{op.Opcode, newArgs}
		newOp.Args[0] = mem
		newOp.MakeLastArg(loc)
		cp.emit(mc, newOp.Opcode, newArgs...)
		return
	}
	panic("Unknown type: " + typeAsString)
}

func (cp *Compiler) generateMoveAlongBranchViaTupleElement(mc *vm.Vm, b *bindle) alternateType {
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
	backtrack1 := mc.CodeTop()
	var backtrack2 uint32
	if needsConditional {
		cp.emit(mc, vm.QlnT, b.valLocs[newBindle.argNo], uint32(newBindle.index), DUMMY)
		newArgumentBindle := newBindle
		newArgumentBindle.argNo++
		typesFromNextArgument = cp.generateNewArgument(mc, &newArgumentBindle)
		backtrack2 = mc.CodeTop()
		cp.emit(mc, vm.Jmp, DUMMY)
		mc.Code[backtrack1].Args[2] = mc.CodeTop()
	}

	typesFromContinuingInTuple := cp.generateFromTopBranchDown(mc, &newBindle)

	if needsConditional {
		mc.Code[backtrack2].Args[0] = backtrack2
	}

	return typesFromContinuingInTuple.union(typesFromNextArgument)
}

func (cp *Compiler) generateMoveAlongBranchViaSingleValue(mc *vm.Vm, b *bindle) alternateType {
	newBindle := *b
	newBindle.treePosition = b.treePosition.Branch[b.branchNo].Node
	newBindle.argNo++
	return cp.generateNewArgument(mc, &newBindle)
}

func (cp *Compiler) generateNextBranchDown(mc *vm.Vm, b *bindle) alternateType {
	newBindle := *b
	newBindle.branchNo++
	return cp.generateBranch(mc, &newBindle)
}

func (cp *Compiler) seekFunctionCall(mc *vm.Vm, b *bindle) alternateType {
	for _, branch := range b.treePosition.Branch { // TODO --- this is a pretty vile hack; it would make sense for it to always be at the top.}
		if branch.Node.Fn != nil {
			fNo := branch.Node.Fn.Number
			builtinTag := cp.fns[fNo].builtin
			functionAndType, ok := BUILTINS[builtinTag]
			if ok {
				if builtinTag == "tuple_of_single?" {
					functionAndType.t = alternateType{finiteTupleType{b.types[0]}}
				}
				if builtinTag == "tuple_of_tuple" {
					functionAndType.t = b.doneList
				}
				if builtinTag == "tuplify_list" {
					functionAndType.t = alternateType{typedTupleType{cp.typeNameToTypeList["single?"]}}
				}
				functionAndType.f(cp, mc, b.tok, b.outLoc, b.valLocs)
				return functionAndType.t
			}
			structNumber, ok := cp.structNumbers[builtinTag]
			if ok {
				args := append([]uint32{b.outLoc, uint32(structNumber)}, b.valLocs...)
				cp.emit(mc, vm.Strc, args...)
				return altType(structNumber)
			}
			cp.emitFunctionCall(mc, fNo, b.valLocs)
			cp.emit(mc, vm.Asgm, b.outLoc, cp.fns[fNo].outReg) // Because the different implementations of the function will have their own out register.
			return cp.fns[fNo].types                           // TODO : Is there a reason why this should be so?
		}
	}
	cp.reserveError(mc, "mc/types/b", b.tok, []any{}) // TODO : the bindle can accumulate the types to allow us to generates this error properly.
	cp.emit(mc, vm.Asgm, b.outLoc, mc.That())
	return altType(values.ERROR)
}

func (cp *Compiler) seekBling(mc *vm.Vm, b *bindle, bling string) alternateType {
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
func (cp *Compiler) emit(mc *vm.Vm, opcode vm.Opcode, args ...uint32) {
	mc.Code = append(mc.Code, vm.MakeOp(opcode, args...))
	if SHOW_COMPILE {
		println(mc, mc.DescribeCode(mc.CodeTop()-1))
	}
}

func (cp *Compiler) put(mc *vm.Vm, opcode vm.Opcode, args ...uint32) {
	args = append([]uint32{mc.MemTop()}, args...)
	cp.emit(mc, opcode, args...)
	mc.Mem = append(mc.Mem, values.Value{})
}

// Reput puts the value in the last memory address to be used.
func (cp *Compiler) reput(mc *vm.Vm, opcode vm.Opcode, args ...uint32) {
	args = append([]uint32{mc.That()}, args...)
	cp.emit(mc, opcode, args...)
}

func (cp *Compiler) emitFunctionCall(mc *vm.Vm, funcNumber uint32, valLocs []uint32) {
	args := append([]uint32{cp.fns[funcNumber].callTo, cp.fns[funcNumber].loReg, cp.fns[funcNumber].hiReg}, valLocs...)
	if cp.fns[funcNumber].tupleReg == DUMMY { // We specialize on whether we have to capture tuples.
		cp.emit(mc, vm.Call, args...)
	} else {
		cp.emit(mc, vm.CalT, args...)
	}
}

func (cp *Compiler) emitEquals(mc *vm.Vm, node *ast.InfixExpression, env *environment) (alternateType, bool) {
	lTypes, lcst := cp.compileNode(mc, node.Args[0], env)
	if lTypes.isOnly(values.ERROR) {
		cp.p.Throw("comp/eq/err/a", node.GetToken())
		return altType(values.ERROR), true
	}
	leftRg := mc.That()
	rTypes, rcst := cp.compileNode(mc, node.Args[2], env)
	if rTypes.isOnly(values.ERROR) {
		cp.p.Throw("comp/eq/err/b", node.GetToken())
		return altType(values.ERROR), true
	}
	rightRg := mc.That()
	oL := lTypes.intersect(rTypes)
	if oL.isOnly(values.ERROR) {
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
				cp.put(mc, vm.Equi, leftRg, rightRg)
			case tp(values.STRING):
				cp.put(mc, vm.Equs, leftRg, rightRg)
			case tp(values.BOOL):
				cp.put(mc, vm.Equb, leftRg, rightRg)
			case tp(values.FLOAT):
				cp.put(mc, vm.Equf, leftRg, rightRg)
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

func (cp *Compiler) compilePipe(mc *vm.Vm, lhsTypes alternateType, lhsConst bool, rhs ast.Node, env *environment) (alternateType, bool) {
	var envWithThat *environment
	var isAttemptedFunc bool
	var v *variable
	backtrack := uint32(DUMMY)
	lhs := mc.That()
	// If we have a single identifier, we wish it to contain a function ...
	switch rhs := rhs.(type) {
	case *ast.Identifier:
		v, ok := env.getVar(rhs.Value)
		if ok {
			cp.p.Throw("comp/pipe/ident", rhs.GetToken())
			return altType(values.ERROR), true
		}
		isAttemptedFunc = true
		if !v.types.contains(values.FUNC) {
			if rhs.GetToken().Literal == "that" { // Yeah it's a stupid corner case but the stupid user has a right to it.
				isAttemptedFunc = false
			} else {
				cp.p.Throw("comp/pipe/func", rhs.GetToken())
				return altType(values.ERROR), true
			}
		}
		if !v.types.isOnly(values.FUNC) {
			cp.reserveError(mc, "vm/pipe/func", rhs.GetToken(), []any{})
			cp.emit(mc, vm.Qtyp, v.mLoc, uint32(values.FUNC), mc.CodeTop()+3)
			backtrack = mc.CodeTop()
			cp.emit(mc, vm.Asgm, DUMMY, mc.That())
			cp.emit(mc, vm.Jmp, DUMMY)
		}
	default:
		var whatAccess varAccess
		if lhsConst {
			whatAccess = VERY_LOCAL_CONSTANT
		} else {
			whatAccess = VERY_LOCAL_VARIABLE
		}
		envWithThat = &environment{data: map[string]variable{"that": {mLoc: mc.That(), access: whatAccess, types: lhsTypes}}, ext: env}
	}
	if isAttemptedFunc {
		cp.put(mc, vm.Dofn, v.mLoc, lhs)
	} else {
		return cp.compileNode(mc, rhs, envWithThat)
	}
	if backtrack != uint32(DUMMY) {
		mc.Code[backtrack].Args[0] = mc.That()
		mc.Code[backtrack+1].Args[0] = mc.CodeTop()
	}
	return ANY_TYPE, ALL_CONST_ACCESS.Contains(v.access)
}

func (cp *Compiler) compileMappingOrFilter(mc *vm.Vm, lhsTypes alternateType, lhsConst bool, rhs ast.Node, env *environment, isFilter bool) (alternateType, bool) {
	var isConst bool
	var isAttemptedFunc bool
	var v *variable
	inputElement := uint32(DUMMY)
	backtrack := uint32(DUMMY)
	resultBacktrack := uint32(DUMMY)
	boolBacktrack := uint32(DUMMY)
	var types alternateType
	sourceList := mc.That()
	envWithThat := &environment{}
	thatLoc := uint32(DUMMY)
	// If we have a single identifier, we wish it to contain a function ...
	switch rhs := rhs.(type) {
	case *ast.Identifier:
		if rhs.GetToken().Literal != "that" {
			v, ok := env.getVar(rhs.Value)
			if !ok {
				cp.p.Throw("comp/mf/ident", rhs.GetToken())
				return altType(values.ERROR), true
			}
			isAttemptedFunc = true
			isConst = ALL_CONST_ACCESS.Contains(v.access)
			if !v.types.contains(values.FUNC) {
				cp.p.Throw("comp/mf/func", rhs.GetToken())
			}
			if !v.types.isOnly(values.FUNC) {
				cp.reserveError(mc, "vm/mf/func", rhs.GetToken(), []any{})
				cp.emit(mc, vm.Qtyp, v.mLoc, uint32(values.FUNC), mc.CodeTop()+3)
				backtrack = mc.CodeTop()
				cp.emit(mc, vm.Asgm, DUMMY, mc.That())
				cp.emit(mc, vm.Jmp, DUMMY)
			}
		}
	}
	if !isAttemptedFunc {
		thatLoc = cp.reserve(mc, values.UNDEFINED_VALUE, DUMMY)
		envWithThat = &environment{data: map[string]variable{"that": {mLoc: mc.That(), access: VERY_LOCAL_VARIABLE, types: cp.typeNameToTypeList["single?"]}}, ext: env}
	}
	counter := cp.reserve(mc, values.INT, 0)
	accumulator := cp.reserve(mc, values.TUPLE, []values.Value{})
	cp.put(mc, vm.LenL, sourceList)
	length := mc.That()

	loopStart := mc.CodeTop()
	cp.put(mc, vm.Gthi, length, counter)
	loopBacktrack := mc.CodeTop()
	cp.emit(mc, vm.Qtru, mc.That(), DUMMY)
	if isAttemptedFunc {
		cp.put(mc, vm.IdxL, sourceList, counter, DUMMY)
		inputElement = mc.That()
		cp.put(mc, vm.Dofn, v.mLoc, mc.That())
		types = altType(values.ERROR).union(cp.typeNameToTypeList["single?"]) // Very much TODO. Normally the function is constant and so we know its return types.
	} else {
		cp.emit(mc, vm.IdxL, thatLoc, sourceList, counter, DUMMY)
		inputElement = thatLoc
		types, isConst = cp.compileNode(mc, rhs, envWithThat)
	}
	resultElement := mc.That()
	if types.contains(values.ERROR) {
		cp.emit(mc, vm.Qtyp, resultElement, uint32(values.ERROR), mc.CodeTop()+3)
		resultBacktrack = mc.CodeTop()
		cp.emit(mc, vm.Asgm, DUMMY, resultElement)
		cp.emit(mc, vm.Jmp, DUMMY)
	}
	if isFilter {
		if !types.contains(values.BOOL) {
			cp.p.Throw("comp/filter/bool", rhs.GetToken())
		}
		if !types.isOnly(values.BOOL) {
			cp.reserveError(mc, "vm/filter/bool", rhs.GetToken(), []any{})
			cp.emit(mc, vm.Qtyp, resultElement, uint32(values.BOOL), mc.CodeTop()+2)
			cp.emit(mc, vm.Jmp, mc.CodeTop()+3)
			boolBacktrack = mc.CodeTop()
			cp.emit(mc, vm.Asgm, DUMMY, mc.That())
			cp.emit(mc, vm.Jmp, DUMMY)
		}
		cp.emit(mc, vm.Qtru, resultElement, mc.CodeTop()+2)
		cp.emit(mc, vm.CcT1, accumulator, accumulator, inputElement)
	} else {
		cp.emit(mc, vm.CcT1, accumulator, accumulator, resultElement)
	}
	cp.emit(mc, vm.Addi, counter, counter, values.C_ONE)
	cp.emit(mc, vm.Jmp, loopStart)

	mc.Code[loopBacktrack].Args[1] = mc.CodeTop()
	cp.put(mc, vm.List, accumulator)

	if backtrack != uint32(DUMMY) {
		mc.Code[backtrack].Args[0] = mc.That()
		mc.Code[backtrack+1].Args[0] = mc.CodeTop()
	}
	if resultBacktrack != uint32(DUMMY) {
		mc.Code[resultBacktrack].Args[0] = mc.That()
		mc.Code[resultBacktrack+1].Args[0] = mc.CodeTop()
	}
	if boolBacktrack != uint32(DUMMY) {
		mc.Code[boolBacktrack].Args[0] = mc.That()
		mc.Code[boolBacktrack+1].Args[0] = mc.CodeTop()
	}

	if types.contains(values.ERROR) {
		return altType(values.ERROR, values.LIST), isConst
	}
	return altType(values.LIST), isConst
}
