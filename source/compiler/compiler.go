package compiler

import (
	"pipefish/source/ast"
	"pipefish/source/object"
	"pipefish/source/parser"
	"pipefish/source/set"
	"pipefish/source/text"
	"pipefish/source/token"
	"pipefish/source/values"
	"pipefish/source/vm"
	"strconv"
)

const SHOW_COMPILE = false

type thunk struct {
	mLoc uint32
	cLoc uint32
}

type Compiler struct {
	p                  *parser.Parser
	enumElements       map[string]uint32
	fieldLabels        map[string]uint32
	structNumbers      map[string]values.ValueType
	gconsts            *environment
	gvars              *environment
	fns                []*cpFunc
	typeNameToTypeList map[string]alternateType
	libraries          map[string]*Compiler

	tupleType uint32 // Location of a constant saying {TYPE, <type number of tuples>}

	// Temporary state.
	thunkList []thunk
	ifStack   []uint32
	goNumber  uint32 // Keeps track of how many Go functions we've created.
}

type cpFunc struct { // The compiler's representation of a function after the function has been compiled.
	callTo   uint32
	loReg    uint32
	hiReg    uint32
	outReg   uint32
	tupleReg uint32
	types    alternateType
	builtin  string // A non-empty string if it's a builtin, saying which one.
	private  bool
	command  bool
	goNumber uint32
	hasGo    bool
}

type Access int

const ( // We use this to keep track of what we're doing so we don't e.g. call a command from a function, or let a command see the globals without a `global` keyword, etc.
	REPL Access = iota
	CMD
	DEF
	INIT
	LAMBDA
	NAMESPACE
)

const DUMMY = 4294967295

func NewCompiler(p *parser.Parser) *Compiler {
	return &Compiler{
		p:             p,
		enumElements:  make(map[string]uint32),
		fieldLabels:   make(map[string]uint32),
		structNumbers: make(map[string]values.ValueType),
		gconsts:       newEnvironment(),
		gvars:         newEnvironment(),
		thunkList:     []thunk{},
		fns:           []*cpFunc{},
		libraries:     make(map[string]*Compiler),
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

func (cp *Compiler) Run(mc *vm.Vm) {
	mc.Run(0)
}

func (cp *Compiler) GetParser() *parser.Parser {
	return cp.p
}

func (cp *Compiler) Do(mc *vm.Vm, line string) values.Value {
	mT := mc.MemTop()
	cT := mc.CodeTop()
	tT := mc.TokenTop()
	lT := mc.LfTop()
	node := cp.p.ParseLine("REPL input", line)
	if cp.p.ErrorsExist() {
		return values.Value{T: values.ERROR}
	}
	cp.compileNode(mc, node, cp.gvars, REPL)
	if cp.p.ErrorsExist() {
		return values.Value{T: values.ERROR}
	}
	cp.emit(mc, vm.Ret)
	mc.Run(cT)
	result := mc.Mem[mc.That()]
	mc.Mem = mc.Mem[:mT]
	mc.Code = mc.Code[:cT]
	mc.Tokens = mc.Tokens[:tT]
	mc.LambdaFactories = mc.LambdaFactories[:lT]
	return result
}

func (cp *Compiler) Describe(mc *vm.Vm, v values.Value) string {
	return mc.Literal(v)
}

func (cp *Compiler) Compile(mc *vm.Vm, source, sourcecode string) {
	node := cp.p.ParseLine(source, sourcecode)
	cp.compileNode(mc, node, cp.gvars, REPL)
	cp.emit(mc, vm.Ret)
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
	LF.Model.Mc = vm.BlankVm(mc.Db)
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
	captures := bodyNames.SubtractSet(params).SubtractSet(locals)
	for k := range captures {
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
		cp.compileNode(LF.Model.Mc, fnNode.Given, newEnv, LAMBDA)
		for _, pair := range cp.thunkList {
			cp.emit(LF.Model.Mc, vm.Thnk, pair.mLoc, pair.cLoc)
		}
	}

	// Function starts here.

	LF.Model.LocToCall = LF.Model.Mc.CodeTop()

	// We have to typecheck inside the lambda, because the calling site doesn't know which function it's calling.

	// TODO !!!

	// Now we can emit the main body of the function.

	cp.compileNode(LF.Model.Mc, fnNode.Body, newEnv, LAMBDA)
	LF.Model.Dest = LF.Model.Mc.That()
	LF.Size = LF.Model.Mc.CodeTop()
	cp.emit(LF.Model.Mc, vm.Ret)

	// We have made our lambda factory!

	mc.LambdaFactories = append(mc.LambdaFactories, LF)
	return uint32(len(mc.LambdaFactories) - 1), captures.IsEmpty() // A lambda which doesn't close over anything is a constant.
}

func (cp *Compiler) addVariable(mc *vm.Vm, env *environment, name string, acc varAccess, types alternateType) {
	env.data[name] = variable{mLoc: mc.That(), access: acc, types: types}
}

func (cp *Compiler) vmIf(mc *vm.Vm, oc vm.Opcode, args ...uint32) {
	cp.ifStack = append(cp.ifStack, mc.CodeTop())
	cp.emit(mc, oc, (append(args, DUMMY))...)
}

func (cp *Compiler) vmEndIf(mc *vm.Vm) {
	cLoc := cp.ifStack[len(cp.ifStack)-1]
	cp.ifStack = cp.ifStack[:len(cp.ifStack)-1]
	mc.Code[cLoc].MakeLastArg(mc.CodeTop())
}

type bkGoto int

func (cp *Compiler) vmGoTo(mc *vm.Vm) bkGoto {
	cp.emit(mc, vm.Jmp, DUMMY)
	return bkGoto(mc.CodeTop() - 1)
}

type bkEarlyReturn int

func (cp *Compiler) vmEarlyReturn(mc *vm.Vm, mLoc uint32) bkEarlyReturn {
	cp.emit(mc, vm.Asgm, DUMMY, mLoc)
	cp.emit(mc, vm.Jmp, DUMMY)
	return bkEarlyReturn(mc.CodeTop() - 2)
}

func (cp *Compiler) vmConditionalEarlyReturn(mc *vm.Vm, oc vm.Opcode, args ...uint32) bkEarlyReturn {
	mLoc := args[len(args)-1]
	cp.emit(mc, oc, append(args[:len(args)-1], mc.CodeTop()+3)...)
	return cp.vmEarlyReturn(mc, mLoc)
}

func (cp *Compiler) vmComeFrom(mc *vm.Vm, items ...any) {
	for _, item := range items {
		switch item := item.(type) {
		case bkGoto:
			if uint32(item) == DUMMY {
				continue
			}
			mc.Code[uint32(item)].MakeLastArg(mc.CodeTop())
		case bkEarlyReturn:
			if uint32(item) == DUMMY {
				continue
			}
			mc.Code[uint32(item)].Args[0] = mc.That()
			mc.Code[uint32(item)+1].MakeLastArg(mc.CodeTop())
		default:
			panic("Can't ComeFrom that!")
		}
	}
}

// The heart of the compiler. It starts by taking a snapshot of the vm. It then does a big switch on the node type
// and compiles accordingly. It then performs some sanity checks and, if the compiled expression is constant,
// evaluates it and uses the snapshot to roll back the vm.
func (cp *Compiler) compileNode(mc *vm.Vm, node ast.Node, env *environment, ac Access) (alternateType, bool) {
	rtnTypes, rtnConst := alternateType{}, true
	mT := mc.MemTop()
	cT := mc.CodeTop()
	tT := mc.TokenTop()
	lT := mc.LfTop()
NodeTypeSwitch:
	switch node := node.(type) {
	case *ast.ApplicationExpression:
		// I doubt that this and related forms such as GroupedExpression are still serving a function in the parser.
		panic("Tim, you were wrong.")
	case *ast.AssignmentExpression:
		// TODO --- need to do this better after we implement tuples
		if node.Left.GetToken().Type != token.IDENT {
			cp.p.Throw("comp/assign/ident", node.Left.GetToken())
			break NodeTypeSwitch
		}
		name := node.Left.(*ast.Identifier).Value
		switch node.Token.Type {
		case token.GVN_ASSIGN:
			thunkStart := mc.Next()
			types, cst := cp.compileNode(mc, node.Right, env, ac)
			cp.emit(mc, vm.Ret)
			if cst {
				cp.addVariable(mc, env, name, LOCAL_TRUE_CONSTANT, types)
				rtnTypes, rtnConst = altType(values.CREATED_LOCAL_CONSTANT), true
				break NodeTypeSwitch
			}
			cp.addVariable(mc, env, name, LOCAL_CONSTANT_THUNK, types)
			cp.thunkList = append(cp.thunkList, thunk{mc.That(), thunkStart})
			rtnTypes, rtnConst = altType(values.CREATED_LOCAL_CONSTANT), false
			break NodeTypeSwitch
		case token.CMD_ASSIGN:
			rhsIsError := bkEarlyReturn(DUMMY)
			rTypes, _ := cp.compileNode(mc, node.Right, env, ac)
			if rTypes.contains(values.ERROR) {
				rhsIsError = cp.vmConditionalEarlyReturn(mc, vm.Qtyp, mc.That(), uint32(values.ERROR), mc.That())
				rtnTypes = altType(values.SUCCESSFUL_VALUE, values.ERROR)
			} else {
				rtnTypes = altType(values.SUCCESSFUL_VALUE)
			}
			rtnConst = false // The initialization/mutation in the assignment makes it variable whatever the RHS is.
			v, ok := env.getVar(name)
			if !ok { // Then we create a local variable.
				cp.reserve(mc, values.UNDEFINED_VALUE, DUMMY)
				cp.addVariable(mc, env, name, LOCAL_VARIABLE, rTypes.without(simpleType(values.ERROR)))
				cp.emit(mc, vm.Asgm, mc.That(), mc.That()-1)
				cp.put(mc, vm.Asgm, values.C_OK)
				break NodeTypeSwitch
			} // Otherwise we update the variable we've got.
			// TODO --- type checking after refactoring type representation.
			cp.emit(mc, vm.Asgm, v.mLoc, mc.That())
			cp.put(mc, vm.Asgm, values.C_OK)
			cp.vmComeFrom(mc, rhsIsError)
			break NodeTypeSwitch
		case token.ASSIGN: // If this hasn't been turned into some other kind of _ASSIGN then we're in the REPL.
			rhsIsError := bkEarlyReturn(DUMMY)
			rTypes, _ := cp.compileNode(mc, node.Right, env, ac)
			if rTypes.contains(values.ERROR) {
				rhsIsError = cp.vmConditionalEarlyReturn(mc, vm.Qtyp, mc.That(), uint32(values.ERROR), mc.That())
				rtnTypes = altType(values.SUCCESSFUL_VALUE, values.ERROR)
			} else {
				rtnTypes = altType(values.SUCCESSFUL_VALUE)
			}
			rtnConst = false // The initialization/mutation in the assignment makes it variable whatever the RHS is.
			v, ok := env.getVar(name)
			if !ok {
				cp.p.Throw("comp/var/exist", node.GetToken(), name)
				break NodeTypeSwitch
			}
			if v.access != GLOBAL_VARIABLE_PUBLIC {
				cp.p.Throw("comp/var/public", node.GetToken(), name)
				break NodeTypeSwitch
			}
			// TODO --- type checking after refactoring type representation.
			cp.emit(mc, vm.Asgm, v.mLoc, mc.That())
			cp.put(mc, vm.Asgm, values.C_OK)
			cp.vmComeFrom(mc, rhsIsError)
			break NodeTypeSwitch
		default: // Of switch on ast.Assignment.
			cp.p.Throw("comp/assign", node.GetToken())
			break NodeTypeSwitch
		}
	case *ast.Bling:
		panic("There is no reason to compile this.")
	case *ast.BooleanLiteral:
		cp.reserve(mc, values.BOOL, node.Value)
		rtnTypes, rtnConst = altType(values.BOOL), true
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
		resolvingCompiler := cp.getResolvingCompiler(node, node.Namespace)
		enumElement, ok := resolvingCompiler.enumElements[node.Value]
		if ok {
			cp.put(mc, vm.Asgm, enumElement)
			rtnTypes, rtnConst = altType(mc.Mem[enumElement].T), true
			break
		}
		labelNumber, ok := resolvingCompiler.fieldLabels[node.Value]
		if ok {
			cp.put(mc, vm.Asgm, labelNumber)
			rtnTypes, rtnConst = altType(values.LABEL), true
			break
		}
		var v *variable
		println("node is", node.Value, len(node.Namespace))
		if resolvingCompiler != cp {
			v, ok = resolvingCompiler.gconsts.getVar(node.Value)
		} else {
			v, ok = env.getVar(node.Value)
		}
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
		containerType, ctrConst := cp.compileNode(mc, node.Left, env, ac)
		container := mc.That()
		indexType, idxConst := cp.compileNode(mc, node.Index, env, ac)
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
		resolvingCompiler := cp.getResolvingCompiler(node, node.Namespace)
		if resolvingCompiler.p.Infixes.Contains(node.Operator) {
			rtnTypes, rtnConst = resolvingCompiler.createFunctionCall(mc, node, env, ac)
			break
		}
		if node.Operator == "," {
			rtnTypes, rtnConst = cp.emitComma(mc, node, env, ac)
			break
		}
		if node.Operator == "==" {
			rtnTypes, rtnConst = cp.emitEquals(mc, node, env, ac)
			break
		}
		if node.Operator == "!=" {
			rtnTypes, rtnConst = cp.emitEquals(mc, node, env, ac)
			cp.put(mc, vm.Notb, mc.That())
			break
		}
		cp.p.Throw("comp/infix", node.GetToken())
		break
	case *ast.IntegerLiteral:
		cp.reserve(mc, values.INT, node.Value)
		rtnTypes, rtnConst = altType(values.INT), true
		break
	case *ast.LazyInfixExpression:
		if node.Operator == "or" {
			lTypes, lcst := cp.compileNode(mc, node.Left, env, ac)
			if !lTypes.contains(values.BOOL) {
				cp.p.Throw("comp/or/bool/left", node.GetToken())
				break
			}
			leftRg := mc.That()
			cp.emit(mc, vm.Qtru, leftRg, mc.Next()+2)
			skipElse := cp.vmGoTo(mc)
			rTypes, rcst := cp.compileNode(mc, node.Right, env, ac)
			if !rTypes.contains(values.BOOL) {
				cp.p.Throw("comp/or/bool/right", node.GetToken())
				break
			}
			rightRg := mc.That()
			cp.vmComeFrom(mc, skipElse)
			cp.put(mc, vm.Orb, leftRg, rightRg)
			rtnTypes, rtnConst = altType(values.BOOL), lcst && rcst
			break
		}
		if node.Operator == "and" {
			lTypes, lcst := cp.compileNode(mc, node.Left, env, ac)
			if !lTypes.contains(values.BOOL) {
				cp.p.Throw("comp/and/bool/left", node.GetToken())
				break
			}
			leftRg := mc.That()
			cp.vmIf(mc, vm.Qtru, leftRg)
			rTypes, rcst := cp.compileNode(mc, node.Right, env, ac)
			if !rTypes.contains(values.BOOL) {
				cp.p.Throw("comp/and/bool/right", node.GetToken())
				break
			}
			rightRg := mc.That()
			cp.vmEndIf(mc)
			cp.put(mc, vm.Andb, leftRg, rightRg)
			rtnTypes, rtnConst = altType(values.BOOL), lcst && rcst
			break
		}
		if node.Operator == ":" {
			if node.Left.GetToken().Type == token.ELSE {
				rtnTypes, rtnConst = cp.compileNode(mc, node.Right, env, ac)
				break
			}
			lTypes, lcst := cp.compileNode(mc, node.Left, env, ac)
			if !lTypes.contains(values.BOOL) {
				cp.p.Throw("comp/cond/bool", node.GetToken())
				break
			}
			// TODO --- what if it's not *only* bool?
			leftRg := mc.That()
			cp.vmIf(mc, vm.Qtru, leftRg)
			rTypes, rcst := cp.compileNode(mc, node.Right, env, ac)
			ifCondition := cp.vmEarlyReturn(mc, mc.That())
			cp.vmEndIf(mc)
			cp.put(mc, vm.Asgm, values.C_U_OBJ)
			cp.vmComeFrom(mc, ifCondition)
			rtnTypes, rtnConst = rTypes.union(altType(values.UNSAT)), lcst && rcst
			break
		}
		if node.Operator == ";" {
			lTypes, lcst := cp.compileNode(mc, node.Left, env, ac)
			leftRg := mc.That()
			// We deal with the case where the newline is separating local constant definitions
			// in the 'given' block.
			if lTypes.isOnly(values.CREATED_LOCAL_CONSTANT) {
				_, cst := cp.compileNode(mc, node.Right, env, ac)
				rtnTypes, rtnConst = altType(values.CREATED_LOCAL_CONSTANT), lcst && cst
				break
			}
			cmdRet := lTypes.isLegalCmdReturn()
			if (cmdRet && lTypes.isOnly(values.BREAK)) || (!cmdRet && !lTypes.contains(values.UNSAT)) {
				cp.p.Throw("comp/unreachable", node.GetToken())
				break
			}
			var rTypes alternateType
			var rcst bool
			if cmdRet { // It could be error, break, OK, or an unsatisfied conditional.
				ifBreak := bkEarlyReturn(DUMMY)
				ifError := bkEarlyReturn(DUMMY)
				ifCouldBeUnsatButIsnt := bkEarlyReturn(DUMMY)
				if lTypes.contains(values.BREAK) {
					ifBreak = cp.vmConditionalEarlyReturn(mc, vm.Qtyp, leftRg, uint32(values.BREAK), values.C_BREAK)
				}
				if lTypes.contains(values.ERROR) {
					ifError = cp.vmConditionalEarlyReturn(mc, vm.Qtyp, leftRg, uint32(values.ERROR), leftRg)
				}
				if lTypes.contains(values.UNSAT) { // Then it is an else-less conditional or a try, and it it isn't UNSAT then we should skip the right node.
					ifCouldBeUnsatButIsnt = cp.vmConditionalEarlyReturn(mc, vm.Qntp, leftRg, uint32(values.UNSAT), leftRg)
				}
				rTypes, _ = cp.compileNode(mc, node.Right, env, ac) // In a cmd we wish rConst to remain false to avoid folding.
				cp.vmComeFrom(mc, ifBreak, ifError, ifCouldBeUnsatButIsnt)
				rtnTypes, rtnConst = lTypes.union(rTypes), lcst && rcst
				break
			} else { // Otherwise it's functional.
				cp.vmIf(mc, vm.Qtyp, leftRg, uint32(values.UNSAT))
				rTypes, rcst = cp.compileNode(mc, node.Right, env, ac)
				lhsIsUnsat := cp.vmEarlyReturn(mc, mc.That())
				cp.vmEndIf(mc)
				cp.put(mc, vm.Asgm, leftRg)
				cp.vmComeFrom(mc, lhsIsUnsat)
				if !(lTypes.contains(values.UNSAT) && rTypes.contains(values.UNSAT)) {
					rtnTypes, rtnConst = lTypes.union(rTypes).without(tp(values.UNSAT)), lcst && rcst
					break
				}
			}
		}
	case *ast.ListExpression:
		var containedTypes alternateType
		containedTypes, rtnConst = cp.compileNode(mc, node.List, env, ac)
		backTrackTo, failed := cp.emitErrorBoilerplate(mc, containedTypes, "comp/list/err", node.GetToken(), false)
		if failed {
			break
		}
		cp.put(mc, vm.List, mc.That())
		mc.Code[backTrackTo].Args[0] = mc.That()
		rtnTypes = altType(values.LIST)
		break
	case *ast.LogExpression:
		rtnConst = false // Since a log expression has a side-effect, it can't be folded even if it's constant.
		initStr := cp.reserve(mc, values.STRING, "Log at line "+text.YELLOW+strconv.Itoa(node.GetToken().Line)+text.RESET+":\n    ")
		output := cp.reserve(mc, values.STRING, "")
		cp.vmIf(mc, vm.Qlog)
		cp.emit(mc, vm.Logn)
		cp.emit(mc, vm.Asgm, output, initStr)
		logMayHaveError := cp.compileLog(mc, node, env, ac)
		cp.emit(mc, vm.Log, output)
		cp.emit(mc, vm.Logy)
		ifRuntimeError := bkEarlyReturn(DUMMY)
		if logMayHaveError {
			ifRuntimeError = cp.vmConditionalEarlyReturn(mc, vm.Qtyp, output, uint32(values.ERROR), output)
		}
		cp.vmEndIf(mc)
		// Syntactically a log expression is attached to a normal expression, which we must now compile.
		switch node.GetToken().Type {
		case token.IFLOG:
			ifNode := &ast.LazyInfixExpression{Operator: ":", Token: *node.GetToken(), Left: node.Left, Right: node.Right}
			rtnTypes, _ = cp.compileNode(mc, ifNode, env, ac)
		case token.PRELOG:
			rtnTypes, _ = cp.compileNode(mc, node.Right, env, ac)
		default: // I.e. token.LOG.
			rtnTypes, _ = cp.compileNode(mc, node.Left, env, ac)
		}
		cp.vmComeFrom(mc, ifRuntimeError)
		break
	case *ast.LoopExpression:
		rtnConst = false
		loopStart := mc.CodeTop()
		bodyTypes, _ := cp.compileNode(mc, node.Code, env, ac)
		if cp.p.ErrorsExist() {
			break
		}
		result := mc.That()
		if !bodyTypes.isLegalReturnFromLoopBody() {
			cp.p.Throw("comp/loop/body", node.GetToken())
			break
		}
		if bodyTypes.isNoneOf(values.BREAK, values.ERROR) {
			cp.p.Throw("comp/loop/infinite", node.GetToken())
			break
		}
		cp.emit(mc, vm.Qntp, result, uint32(values.SUCCESSFUL_VALUE), loopStart)
		if bodyTypes.isOnly(values.ERROR) {
			rtnTypes = altType(values.ERROR)
			break
		}
		if bodyTypes.isOnly(values.BREAK) {
			cp.put(mc, vm.Asgm, values.C_OK)
			rtnTypes = altType(values.SUCCESSFUL_VALUE)
			break
		}
		ifError := cp.vmConditionalEarlyReturn(mc, vm.Qtyp, result, uint32(values.ERROR), result)
		cp.put(mc, vm.Asgm, values.C_OK)
		cp.vmComeFrom(mc, ifError)
		rtnTypes = altType(values.SUCCESSFUL_VALUE, values.ERROR)
		break
	case *ast.Nothing: // TODO: there is no reason why both this and the ast.Nothing type should exist.
		cp.put(mc, vm.Asgm, values.C_EMPTY_TUPLE)
		rtnTypes, rtnConst = alternateType{finiteTupleType{}}, true
	case *ast.PipingExpression: // I.e. -> >> and -> and ?> .
		lhsTypes, lhsConst := cp.compileNode(mc, node.Left, env, ac)
		if cp.p.ErrorsExist() {
			break
		}
		// And that's about all the streaming operators really do have in common under the hood, so let's do a switch on the operators.
		var rhsConst bool
		switch node.Operator {
		case "->":
			rtnTypes, rhsConst = cp.compilePipe(mc, lhsTypes, lhsConst, node.Right, env, ac)
		case ">>":
			rtnTypes, rhsConst = cp.compileMappingOrFilter(mc, lhsTypes, lhsConst, node.Right, env, false, ac)
		default:
			rtnTypes, rhsConst = cp.compileMappingOrFilter(mc, lhsTypes, lhsConst, node.Right, env, true, ac)
		}
		rtnConst = lhsConst && rhsConst
		break
	case *ast.PrefixExpression:
		if node.Operator == "not" {
			allTypes, cst := cp.compileNode(mc, node.Args[0], env, ac)
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
		if node.Operator == "global" { // This is in effect a compiler directive, it doesn't need to emit any code besides `ok`, it just mutates the environment.
			for _, v := range node.Args {
				switch arg := v.(type) {
				case *ast.Identifier:
					variable, ok := cp.gvars.getVar(arg.Value)
					if !ok {
						cp.p.Throw("comp/global/global", arg.GetToken())
						break NodeTypeSwitch
					}
					env.data[arg.Value] = *variable
				default:
					cp.p.Throw("comp/global/ident", arg.GetToken())
					break NodeTypeSwitch
				}
			}
			rtnTypes, rtnConst = altType(values.SUCCESSFUL_VALUE), false
			break
		}
		resolvingCompiler := cp.getResolvingCompiler(node, node.Namespace)
		var (
			v  *variable
			ok bool
		)
		if resolvingCompiler != cp {
			v, ok = resolvingCompiler.gvars.getVar(node.Operator)
		} else {
			v, ok = env.getVar(node.Operator)
		}
		if ok && v.types.contains(values.FUNC) {
			operands := []uint32{v.mLoc}
			for _, arg := range node.Args {
				cp.compileNode(mc, arg, env, ac)
				operands = append(operands, mc.That())
			}
			if cp.p.ErrorsExist() {

				break
			}
			if v.types.isOnly(values.FUNC) { // Then no type checking for v.
				cp.put(mc, vm.Dofn, operands...)
			}
			rtnTypes = ANY_TYPE
			break
		}
		if resolvingCompiler.p.Prefixes.Contains(node.Operator) || resolvingCompiler.p.Functions.Contains(node.Operator) {
			rtnTypes, rtnConst = resolvingCompiler.createFunctionCall(mc, node, env, ac)
			break
		}
		cp.p.Throw("comp/prefix/known", node.GetToken())
		break
	case *ast.SetExpression:
		panic("This has been deprecated and should be removed.") // TODO.
	case *ast.StringLiteral:
		cp.reserve(mc, values.STRING, node.Value)
		rtnTypes, rtnConst = altType(values.STRING), true
		break
	case *ast.StructExpression:
		panic("This is used only in the vmmaker and should never be compiled.")
	case *ast.SuffixExpression:
		resolvingCompiler := cp.getResolvingCompiler(node, node.Namespace)
		if resolvingCompiler.p.Suffixes.Contains(node.Operator) {
			rtnTypes, rtnConst = resolvingCompiler.createFunctionCall(mc, node, env, ac)
			break
		}
		cp.p.Throw("comp/suffix", node.GetToken())
		break
	case *ast.TryExpression:
		ident := node.VarName
		v, exists := env.getVar(ident)
		if exists && (v.access == GLOBAL_CONSTANT_PRIVATE || v.access == GLOBAL_CONSTANT_PUBLIC ||
			v.access == GLOBAL_VARIABLE_PRIVATE || v.access == GLOBAL_VARIABLE_PUBLIC || v.access == LOCAL_CONSTANT_THUNK) {
			cp.p.Throw("comp/try/var", node.GetToken())
			break
		}
		var err uint32
		if !exists {
			err = cp.reserve(mc, values.NULL, nil)
			cp.addVariable(mc, env, ident, LOCAL_VARIABLE, altType(values.NULL, values.ERROR))
		} else {
			err = v.mLoc
		}
		tryTypes, _ := cp.compileNode(mc, node.Right, env, ac)
		if tryTypes.isNoneOf(values.ERROR, values.SUCCESSFUL_VALUE) {
			cp.p.Throw("comp/try/return", node.GetToken())
			break
		}
		cp.emit(mc, vm.Qtyp, mc.That(), uint32(values.ERROR), mc.CodeTop()+4)
		cp.emit(mc, vm.Asgm, err, mc.That())
		cp.emit(mc, vm.Asgm, mc.That(), values.C_U_OBJ)
		cp.emit(mc, vm.Qtyp, mc.That(), uint32(values.ERROR), mc.CodeTop()+2)
		cp.emit(mc, vm.Asgm, mc.That(), values.C_OK)
		rtnTypes, rtnConst = altType(values.UNSAT, values.SUCCESSFUL_VALUE), false
		break
	case *ast.TypeLiteral:
		resolvingCompiler := cp.getResolvingCompiler(node, node.Namespace)
		cp.reserve(mc, values.TYPE, values.ValueType(resolvingCompiler.typeNameToTypeList[node.Value][0].(simpleType)))
		rtnTypes, rtnConst = altType(values.TYPE), true
		break
	case *ast.UnfixExpression:
		resolvingCompiler := cp.getResolvingCompiler(node, node.Namespace)
		if resolvingCompiler.p.Unfixes.Contains(node.Operator) {
			rtnTypes, rtnConst = resolvingCompiler.createFunctionCall(mc, node, env, ac)
			break
		}
		cp.p.Throw("comp/unfix", node.GetToken()) // TODO --- can errors like this even arise or must they be caught in the parser?
		break
	default:
		panic("Unimplemented node type.")
	}
	if !rtnTypes.isLegalCmdReturn() && !rtnTypes.isLegalDefReturn() {
		cp.p.Throw("comp/sanity/a", node.GetToken())
	}
	if ac == DEF && !rtnTypes.isLegalDefReturn() {
		cp.p.Throw("comp/fcis", node.GetToken())
	}
	if cp.p.ErrorsExist() {
		return altType(values.COMPILE_TIME_ERROR), false // False because we don't want any code folding to happen as that could remove information about the error.
	}
	if rtnConst && mc.CodeTop() > cT {
		cp.emit(mc, vm.Ret)
		mc.Run(cT)
		result := mc.Mem[mc.That()]
		if result.T == values.TUPLE {
			tType := finiteTupleType{}
			for _, v := range result.V.([]values.Value) {
				tType = append(tType, simpleType(v.T))
			}
			rtnTypes = alternateType{tType}
		} else {
			rtnTypes = altType(result.T)
		}
		mc.Mem = mc.Mem[:mT]
		mc.Code = mc.Code[:cT]
		mc.Tokens = mc.Tokens[:tT]
		mc.LambdaFactories = mc.LambdaFactories[:lT]
		cp.reserve(mc, result.T, result.V)
	}
	return rtnTypes, rtnConst
}

// This needs its own very special logic because the type it returns has to be composed in a different way from all the other operators.
func (cp *Compiler) emitComma(mc *vm.Vm, node *ast.InfixExpression, env *environment, ac Access) (alternateType, bool) {
	lTypes, lcst := cp.compileNode(mc, node.Args[0], env, ac)
	if lTypes.isOnly(values.ERROR) {
		cp.p.Throw("comp/tuple/err/a", node.GetToken())
	}
	left := mc.That()
	rTypes, rcst := cp.compileNode(mc, node.Args[2], env, ac)
	if rTypes.isOnly(values.ERROR) {
		cp.p.Throw("comp/tuple/err/b", node.GetToken())
	}
	right := mc.That()
	leftIsError := bkEarlyReturn(DUMMY)
	rightIsError := bkEarlyReturn(DUMMY)
	if lTypes.contains(values.ERROR) {
		cp.vmConditionalEarlyReturn(mc, vm.Qtyp, left, uint32(tp(values.ERROR)), left)
	}
	if rTypes.contains(values.ERROR) {
		cp.vmConditionalEarlyReturn(mc, vm.Qtyp, right, uint32(tp(values.ERROR)), right)
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
	cp.vmComeFrom(mc, leftIsError, rightIsError)
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

// Finds the appropriate compiler for a given namespace.
func (cp *Compiler) getResolvingCompiler(node ast.Node, namespace []string) *Compiler {
	lC := cp
	var ok bool
	for _, name := range namespace {
		lC, ok = lC.libraries[name]
		if !ok {
			cp.p.Throw("comp/namespace/exist", node.GetToken(), name)
		}
	}
	return lC
}

// TODO --- this can be replaced with other generalizations.
func (cp *Compiler) emitErrorBoilerplate(mc *vm.Vm, types alternateType, errCode string, tok *token.Token, appendToken bool) (uint32, bool) {
	failed := false
	var backtrackTo uint32
	if types.isOnly(values.ERROR) {
		cp.p.Throw("comp/list/err", tok)
		failed = true
	}
	if types.contains(values.ERROR) {
		cp.emit(mc, vm.Qtyp, mc.That(), uint32(values.ERROR), mc.CodeTop()+3)
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

func (cp *Compiler) createFunctionCall(mc *vm.Vm, node ast.Callable, env *environment, ac Access) (alternateType, bool) {
	args := node.GetArgs()
	if len(args) == 1 {
		switch args[0].(type) {
		case *ast.Nothing:
			args = []ast.Node{}
		}
	}
	b := &bindle{tok: node.GetToken(),
		treePosition: cp.p.FunctionGroupMap[node.GetToken().Literal].Tree,
		outLoc:       cp.reserveError(mc, "mc/oopsie", node.GetToken(), []any{}),
		env:          env,
		valLocs:      make([]uint32, len(args)),
		types:        make(finiteTupleType, len(args)),
		access:       ac,
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
			b.types[i], cstI = cp.compileNode(mc, arg, env, ac)
			cst = cst && cstI
			b.valLocs[i] = mc.That()
			if b.types[i].(alternateType).isOnly(values.ERROR) {
				cp.p.Throw("comp/arg/error", node.GetToken())
				return altType(values.ERROR), true
			}
			if b.types[i].(alternateType).contains(values.ERROR) {
				cp.emit(mc, vm.Qtyp, mc.That(), uint32(tp(values.ERROR)), mc.CodeTop()+3)
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
	access       Access
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
			cp.emit(mc, vm.Qsnq, b.valLocs[b.argNo], mc.CodeTop()+3)
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
		cp.vmIf(mc, vm.Qsnq, b.valLocs[b.argNo])
		typesFromSingles := cp.generateMoveAlongBranchViaSingleValue(mc, &newBindle)
		skipElse := cp.vmGoTo(mc)
		cp.vmEndIf(mc)
		typesFromTuples := cp.generateMoveAlongBranchViaTupleElement(mc, &newBindle)
		cp.vmComeFrom(mc, skipElse)
		typesFromGoingAcross = typesFromSingles.union(typesFromTuples)
	}
	// And now we need to do the 'else' branch if there is one.
	if needsOtherBranch {
		skipElse := cp.vmGoTo(mc)
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
		cp.vmComeFrom(mc, skipElse)
	}
	return typesFromGoingAcross.union(typesFromGoingDown)
}

var TYPE_COMPARISONS = map[string]*vm.Operation{
	"int":     {vm.Qtyp, []uint32{DUMMY, uint32(values.INT), DUMMY}},
	"string":  {vm.Qtyp, []uint32{DUMMY, uint32(values.STRING), DUMMY}},
	"bool":    {vm.Qtyp, []uint32{DUMMY, uint32(values.BOOL), DUMMY}},
	"float64": {vm.Qtyp, []uint32{DUMMY, uint32(values.FLOAT), DUMMY}},
	"null":    {vm.Qtyp, []uint32{DUMMY, uint32(values.NULL), DUMMY}},
	"label":   {vm.Qtyp, []uint32{DUMMY, uint32(values.LABEL), DUMMY}},
	"list":    {vm.Qtyp, []uint32{DUMMY, uint32(values.LIST), DUMMY}},
	"set":     {vm.Qtyp, []uint32{DUMMY, uint32(values.SET), DUMMY}},
	"map":     {vm.Qtyp, []uint32{DUMMY, uint32(values.PAIR), DUMMY}},
	"func":    {vm.Qtyp, []uint32{DUMMY, uint32(values.FUNC), DUMMY}},
	"single":  {vm.Qsng, []uint32{DUMMY, DUMMY}},
	"single?": {vm.Qsnq, []uint32{DUMMY, DUMMY}},
	"struct":  {vm.Qstr, []uint32{DUMMY, DUMMY}},
	"struct?": {vm.Qstq, []uint32{DUMMY, DUMMY}},
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
	var skipElse bkGoto
	if needsConditional {
		cp.vmIf(mc, vm.QlnT, b.valLocs[newBindle.argNo], uint32(newBindle.index))
		newArgumentBindle := newBindle
		newArgumentBindle.argNo++
		typesFromNextArgument = cp.generateNewArgument(mc, &newArgumentBindle)
		skipElse = cp.vmGoTo(mc)
		cp.vmEndIf(mc)
	}

	typesFromContinuingInTuple := cp.generateFromTopBranchDown(mc, &newBindle)

	if needsConditional {
		cp.vmComeFrom(mc, skipElse)
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
			F := cp.fns[fNo]
			// Before we do anything else, let's control for access. The REPL shouldn't be able to access private
			// commands or functions, and functions shouldn't be able to access commands.
			if b.access == REPL && F.private {
				cp.p.Throw("comp/private", b.tok)
			}
			if b.access == DEF && F.command {
				cp.p.Throw("comp/command", b.tok)
			}
			// Deal with the case where the function is a builtin.
			builtinTag := F.builtin
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
				if builtinTag == "type_with" {
					functionAndType.t = alternateType{cp.typeNameToTypeList["struct"]}.union(altType(values.ERROR))
				}
				if builtinTag == "struct_with" {
					functionAndType.t = alternateType{cp.typeNameToTypeList["struct"]}.union(altType(values.ERROR))
				}
				functionAndType.f(cp, mc, b.tok, b.outLoc, b.valLocs)
				return functionAndType.t
			}
			// It might be a short-form constructor.
			structNumber, ok := cp.structNumbers[builtinTag]
			if ok {
				args := append([]uint32{b.outLoc, uint32(structNumber)}, b.valLocs...)
				cp.emit(mc, vm.Strc, args...)
				return altType(structNumber)
			}
			// It could have a Golang body.
			if F.hasGo {
				args := append([]uint32{b.outLoc, F.goNumber}, b.valLocs...)
				cp.emit(mc, vm.Gofn, args...)
				if len(branch.Node.Fn.Rets) == 0 {
					return ANY_TYPE
				}
				if len(branch.Node.Fn.Rets) == 1 {
					return cp.typeNameToTypeList[branch.Node.Fn.Rets[0].VarType]
				}
				// Otherwise it's a tuple.
				tt := make(alternateType, 0, len(branch.Node.Fn.Rets))
				for _, v := range branch.Node.Fn.Rets {
					tt = append(tt, cp.typeNameToTypeList[v.VarType])
				}
				return alternateType{finiteTupleType{tt}}
			}
			// Otherwise it's a regular old function call, which we do like this:
			cp.emitFunctionCall(mc, fNo, b.valLocs)
			cp.emit(mc, vm.Asgm, b.outLoc, F.outReg) // Because the different implementations of the function will have their own out register.
			return F.types                           // TODO : Is there a reason why this should be so?
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

func (cp *Compiler) emitFunctionCall(mc *vm.Vm, funcNumber uint32, valLocs []uint32) {
	args := append([]uint32{cp.fns[funcNumber].callTo, cp.fns[funcNumber].loReg, cp.fns[funcNumber].hiReg}, valLocs...)
	if cp.fns[funcNumber].tupleReg == DUMMY { // We specialize on whether we have to capture tuples.
		cp.emit(mc, vm.Call, args...)
	} else {
		cp.emit(mc, vm.CalT, args...)
	}
}

func (cp *Compiler) emitEquals(mc *vm.Vm, node *ast.InfixExpression, env *environment, ac Access) (alternateType, bool) {
	lTypes, lcst := cp.compileNode(mc, node.Args[0], env, ac)
	if lTypes.isOnly(values.ERROR) {
		cp.p.Throw("comp/eq/err/a", node.GetToken())
		return altType(values.ERROR), true
	}
	leftRg := mc.That()
	rTypes, rcst := cp.compileNode(mc, node.Args[2], env, ac)
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

func (cp *Compiler) compileLog(mc *vm.Vm, node *ast.LogExpression, env *environment, ac Access) bool {
	output := mc.That()
	logStr := node.Value
	if logStr == "" {

	}
	strList := []string{}
	var (
		word string
		exp  bool
	)
	for _, c := range logStr {
		if c == '|' {
			if exp {
				strList = append(strList, word+"|")
				word = ""
				exp = false
			} else {
				strList = append(strList, word)
				word = "|"
				exp = true
			}
		} else {
			word = word + string(c)
		}
	}
	if exp {
		cp.p.Throw("comp/log/close", &node.Token)
		return false
	}
	strList = append(strList, word)
	// So at this point we have a strList consisting of things which either do or don't need parsing and compiling,
	// depending on whether they are or aren't bracketed by | symbols.
	// If they don't need compiling they can just be concatenated to the output.
	errorReturns := []bkEarlyReturn{}
	for _, str := range strList {
		if str == "" {
			continue
		}
		if str[0] == '|' { // Then we must parse and compile.
			parsedAst := cp.p.ParseLine("code snippet", str[1:len(str)-1])
			sTypes, _ := cp.compileNode(mc, parsedAst, env, ac)
			thingToAdd := mc.That()
			if sTypes.contains(values.ERROR) {
				errorReturns = append(errorReturns,
					cp.vmConditionalEarlyReturn(mc, vm.Qtyp, mc.That(), uint32(values.ERROR), mc.That()))
			}
			cp.put(mc, vm.Strx, thingToAdd)
			cp.emit(mc, vm.Adds, output, output, mc.That())
			continue
		}
		// Otherwise, we just add it on as a string.
		cp.reserve(mc, values.STRING, str)
		cp.emit(mc, vm.Adds, output, output, mc.That())
	}
	for _, rtn := range errorReturns {
		cp.vmComeFrom(mc, rtn)
	}
	return len(errorReturns) > 0
}

// The various 'piping operators'.
func (cp *Compiler) compilePipe(mc *vm.Vm, lhsTypes alternateType, lhsConst bool, rhs ast.Node, env *environment, ac Access) (alternateType, bool) {
	var envWithThat *environment
	var isAttemptedFunc bool
	var v *variable
	typeIsNotFunc := bkEarlyReturn(DUMMY)
	var rtnTypes alternateType
	var rtnConst bool
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
			cp.emit(mc, vm.Qntp, v.mLoc, uint32(values.FUNC), mc.CodeTop()+3)
			typeIsNotFunc = cp.vmEarlyReturn(mc, mc.That())
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
		rtnTypes, rtnConst = ANY_TYPE, ALL_CONST_ACCESS.Contains(v.access)
	} else {
		rtnTypes, rtnConst = cp.compileNode(mc, rhs, envWithThat, ac)
	}
	cp.vmComeFrom(mc, typeIsNotFunc)
	return rtnTypes, rtnConst
}

func (cp *Compiler) compileMappingOrFilter(mc *vm.Vm, lhsTypes alternateType, lhsConst bool, rhs ast.Node, env *environment, isFilter bool, ac Access) (alternateType, bool) {
	var isConst bool
	var isAttemptedFunc bool
	var v *variable
	inputElement := uint32(DUMMY)
	typeIsNotFunc := bkEarlyReturn(DUMMY)
	resultIsError := bkEarlyReturn(DUMMY)
	resultIsNotBool := bkEarlyReturn(DUMMY)
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
				cp.emit(mc, vm.Qntp, v.mLoc, uint32(values.FUNC), mc.CodeTop()+3)
				typeIsNotFunc = cp.vmEarlyReturn(mc, mc.That())
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
	cp.vmIf(mc, vm.Qtru, mc.That())
	if isAttemptedFunc {
		cp.put(mc, vm.IdxL, sourceList, counter, DUMMY)
		inputElement = mc.That()
		cp.put(mc, vm.Dofn, v.mLoc, mc.That())
		types = altType(values.ERROR).union(cp.typeNameToTypeList["single?"]) // Very much TODO. Normally the function is constant and so we know its return types.
	} else {
		cp.emit(mc, vm.IdxL, thatLoc, sourceList, counter, DUMMY)
		inputElement = thatLoc
		types, isConst = cp.compileNode(mc, rhs, envWithThat, ac)
	}
	resultElement := mc.That()
	if types.contains(values.ERROR) {
		cp.emit(mc, vm.Qtyp, resultElement, uint32(values.ERROR), mc.CodeTop()+3)
		resultIsError = cp.vmEarlyReturn(mc, mc.That())
	}
	if isFilter {
		if !types.contains(values.BOOL) {
			cp.p.Throw("comp/filter/bool", rhs.GetToken())
		}
		if !types.isOnly(values.BOOL) {
			cp.reserveError(mc, "vm/filter/bool", rhs.GetToken(), []any{})
			cp.emit(mc, vm.Qntp, resultElement, uint32(values.BOOL), mc.CodeTop()+2)
			resultIsNotBool = cp.vmEarlyReturn(mc, mc.That())
		}
		cp.emit(mc, vm.Qtru, resultElement, mc.CodeTop()+2)
		cp.emit(mc, vm.CcT1, accumulator, accumulator, inputElement)
	} else {
		cp.emit(mc, vm.CcT1, accumulator, accumulator, resultElement)
	}
	cp.emit(mc, vm.Addi, counter, counter, values.C_ONE)
	cp.emit(mc, vm.Jmp, loopStart)
	cp.vmEndIf(mc)
	cp.put(mc, vm.List, accumulator)
	cp.vmComeFrom(mc, typeIsNotFunc, resultIsError, resultIsNotBool)

	if types.contains(values.ERROR) {
		return altType(values.ERROR, values.LIST), isConst
	}
	return altType(values.LIST), isConst
}
