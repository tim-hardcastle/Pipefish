package service

import (
	"os"
	"pipefish/source/ast"
	"pipefish/source/dtypes"
	"pipefish/source/parser"
	"pipefish/source/report"
	"pipefish/source/settings"
	"pipefish/source/text"
	"pipefish/source/token"
	"pipefish/source/values"
	"strings"

	"fmt"
	"strconv"
)

type Thunk struct {
	MLoc uint32
	CLoc uint32
}

type Compiler struct {
	// Permanent state, i.e. it is unchanged after initialization.
	vm                       *Vm // The vm we're compiling to.
	P                        *parser.Parser
	EnumElements             map[string]uint32
	FieldLabelsInMem         map[string]uint32 // We have these so that we can introduce a label by putting Asgm location of label and then transitively squishing.
	LabelIsPrivate           []bool
	StructNameToTypeNumber   map[string]values.ValueType
	GlobalConsts             *Environment
	GlobalVars               *Environment
	Fns                      []*CpFunc
	TypeNameToTypeList       map[string]AlternateType
	AnyTypeScheme            AlternateType         // Sometimes, like if someone doesn't specify a return type from their Go function, then the compiler needs to be able to say "whatevs".
	Services                 map[string]*VmService // Both true internal services, and stubs that call the externals.
	CallHandlerNumbersByName map[string]uint32     // Map from the names of external services to their index as stored in the vm.
	Timestamp                int64
	ScriptFilepath           string

	TupleType uint32 // Location of a constant saying {TYPE, <type number of tuples>}, so that 'type (x tuple)' in the builtins has something to return. Query, why not just define 'type (x tuple) : tuple' ?

	// Temporary state.
	ThunkList   []Thunk
	ifStack     []uint32
	showCompile bool
}

type CpFunc struct { // The compiler's representation of a function after the function has been compiled.
	CallTo   uint32
	LoReg    uint32
	HiReg    uint32
	OutReg   uint32
	TupleReg uint32
	Types    AlternateType
	Builtin  string   // A non-empty string if it's a builtin, saying which one.
	Xcall    *XBindle // Information for making an external call, if non-nil.
	Private  bool     // True if it's private.
	Command  bool     // True if it's a command.
	GoNumber uint32
	HasGo    bool
}

type XBindle struct { // The types have already been decoded and put into the types of the owning CpFunc.
	ExternalServiceOrdinal uint32
	FunctionName           string
	Position               uint32
}

// The access that the compiler has at any given point in the compilation. Are we compiling code in a function, a command, a REPL?
type cpAccess int

const ( // We use this to keep track of what we're doing so we don't e.g. call a command from a function, or let a command see the globals without a `global` keyword, etc.
	REPL   cpAccess = iota // Call from the REPL, or an external service. TODO --- distinguish them for clarity?
	CMD                    // We're in a command.
	DEF                    // We're in a function.
	INIT                   // We're initializing the global variables.
	LAMBDA                 // We're in a lambda function.
)

const DUMMY = 4294967295

func NewCompiler(p *parser.Parser) *Compiler {
	newC := &Compiler{
		P:                        p,
		EnumElements:             make(map[string]uint32),
		FieldLabelsInMem:         make(map[string]uint32),
		StructNameToTypeNumber:   make(map[string]values.ValueType),
		GlobalConsts:             NewEnvironment(),
		GlobalVars:               NewEnvironment(),
		ThunkList:                []Thunk{},
		Fns:                      []*CpFunc{},
		Services:                 make(map[string]*VmService),
		CallHandlerNumbersByName: make(map[string]uint32), // A map from the identifier of the external service to its ordinal in the vm's externalServices list.
		TypeNameToTypeList: map[string]AlternateType{
			"ok":       AltType(values.SUCCESSFUL_VALUE),
			"int":      AltType(values.INT),
			"string":   AltType(values.STRING),
			"bool":     AltType(values.BOOL),
			"float":    AltType(values.FLOAT),
			"error":    AltType(values.ERROR),
			"type":     AltType(values.TYPE),
			"pair":     AltType(values.PAIR),
			"list":     AltType(values.LIST),
			"map":      AltType(values.MAP),
			"set":      AltType(values.SET),
			"label":    AltType(values.LABEL),
			"func":     AltType(values.FUNC),
			"int?":     AltType(values.NULL, values.INT),
			"string?":  AltType(values.NULL, values.STRING),
			"bool?":    AltType(values.NULL, values.BOOL),
			"float64?": AltType(values.NULL, values.FLOAT),
			"type?":    AltType(values.NULL, values.TYPE),
			"pair?":    AltType(values.NULL, values.PAIR),
			"list?":    AltType(values.NULL, values.LIST),
			"map?":     AltType(values.NULL, values.MAP),
			"set?":     AltType(values.NULL, values.SET),
			"label?":   AltType(values.NULL, values.LABEL),
			"func?":    AltType(values.NULL, values.FUNC),
			"null":     AltType(values.NULL),
			"single":   AltType(values.INT, values.BOOL, values.STRING, values.FLOAT, values.TYPE, values.FUNC, values.PAIR, values.LIST, values.MAP, values.SET, values.LABEL),
			"single?":  AltType(values.NULL, values.INT, values.BOOL, values.STRING, values.FLOAT, values.TYPE, values.FUNC, values.PAIR, values.LIST, values.MAP, values.SET, values.LABEL),
			"struct":   AltType(),
			"struct?":  AltType(values.NULL),
			"snippet":  AltType(),
			"snippet?": AltType(values.NULL),
		},
	}
	copy(newC.AnyTypeScheme, newC.TypeNameToTypeList["single?"])
	newC.AnyTypeScheme = newC.AnyTypeScheme.Union(AltType(values.ERROR))
	newC.AnyTypeScheme = append(newC.AnyTypeScheme, TypedTupleType{newC.TypeNameToTypeList["single?"]})
	return newC
}

func (cp *Compiler) NeedsUpdate() (bool, error) {
	if len(cp.ScriptFilepath) >= 5 && cp.ScriptFilepath[0:5] == "http:" {
		return false, nil
	}
	file, err := os.Stat(cp.ScriptFilepath)
	if err != nil {
		return false, err
	}
	currentTimeStamp := file.ModTime().UnixMilli()
	if cp.Timestamp != currentTimeStamp {
		return true, nil
	}
	for _, imp := range cp.Services {
		impNeedsUpdate, impError := imp.Cp.NeedsUpdate()
		if impNeedsUpdate || impError != nil {
			return impNeedsUpdate, impError
		}
	}
	return false, nil
}

func (cp *Compiler) GetParser() *parser.Parser {
	return cp.P
}

func (mc *Vm) isPrivate(a values.AbstractType) bool {
	for _, w := range a.Types {
		if mc.concreteTypes[w].isPrivate() {
			return true
		}
	}
	return false
}

func (cp *Compiler) Do(line string) values.Value {
	state := cp.getState()
	cT := cp.CodeTop()
	node := cp.P.ParseLine("REPL input", line)
	if settings.SHOW_PARSER {
		fmt.Println("Parsed line:", node.String())
	}
	if cp.P.ErrorsExist() {
		return values.Value{T: values.ERROR}
	}
	cp.CompileNode(node, cp.GlobalVars, REPL)
	if cp.P.ErrorsExist() {
		return values.Value{T: values.ERROR}
	}
	cp.Emit(Ret)
	cp.vm.Run(cT)
	result := cp.vm.Mem[cp.That()]
	cp.rollback(state)
	return result
}

func (cp *Compiler) Describe(v values.Value) string {
	return cp.vm.Literal(v)
}

func (cp *Compiler) Reserve(t values.ValueType, v any) uint32 {
	cp.vm.Mem = append(cp.vm.Mem, values.Value{T: t, V: v})
	return uint32(len(cp.vm.Mem) - 1)
}

func (cp *Compiler) reserveError(ec string, tok *token.Token, args ...any) uint32 {
	cp.vm.Mem = append(cp.vm.Mem, values.Value{T: values.ERROR, V: &report.Error{ErrorId: ec, Token: tok, Args: args, Trace: make([]*token.Token, 0, 10)}})
	return cp.That()
}

func (cp *Compiler) reserveToken(tok *token.Token) uint32 {
	cp.vm.Tokens = append(cp.vm.Tokens, tok)
	return cp.ThatToken()
}

type compiledSnippetKind int

const (
	UNCOMPILED_SNIPPET compiledSnippetKind = iota
	SQL_SNIPPET
	HTML_SNIPPET
)

func (cp *Compiler) reserveSnippetFactory(t string, env *Environment, fnNode *ast.SuffixExpression, ac cpAccess) uint32 {
	sEnv := NewEnvironment() // The source environment is used to build the env field of the snippet. NOTE: if we never reference this field, as we often won't, we can remove this as an optimization.
	sEnv = flattenEnv(env, sEnv)
	snF := &SnippetFactory{snippetType: cp.StructNameToTypeNumber[t], sourceString: fnNode.Token.Literal, sourceEnv: sEnv}
	csk := UNCOMPILED_SNIPPET
	switch {
	case t == "SQL":
		csk = SQL_SNIPPET
	case t == "HTML":
		csk = HTML_SNIPPET
	}
	varLocsStart := cp.MemTop()
	if csk != UNCOMPILED_SNIPPET { // Then it's an external snippet, or HTML, or SQL, and we should compile some code.
		cEnv := NewEnvironment() // The compliation environment is used to compile against.
		sourceLocs := []uint32{}
		for k, v := range sEnv.data {
			where := cp.Reserve(values.UNDEFINED_VALUE, nil)
			w := v
			w.mLoc = where
			sourceLocs = append(sourceLocs, v.mLoc)
			cEnv.data[k] = w
		}
		// We can now compile against the cEnv.
		snF.bindle = cp.compileInjectableSnippet(fnNode.GetToken(), cEnv, csk, snF.sourceString, ac)
		snF.bindle.varLocsStart = varLocsStart
		snF.bindle.sourceLocs = sourceLocs
		snF.bindle.compiledSnippetKind = csk
	} // End of handling special snippets.
	cp.vm.SnippetFactories = append(cp.vm.SnippetFactories, snF)
	return uint32(len(cp.vm.SnippetFactories) - 1)
}

func flattenEnv(env *Environment, target *Environment) *Environment {
	// TODO --- variables captured should be restricted by access.
	if env.Ext != nil {
		flattenEnv(env.Ext, target)
	}
	for k, v := range env.data {
		target.data[k] = v
	}
	return target
}

func (cp *Compiler) compileLambda(env *Environment, fnNode *ast.FuncExpression, tok *token.Token) bool {

	LF := &LambdaFactory{Model: &Lambda{}}
	newEnv := NewEnvironment()
	sig := fnNode.Sig
	skipLambdaCode := cp.vmGoTo()
	LF.Model.capturesStart = cp.MemTop()

	// We get the function parameters. These shadow anything we might otherwise capture.
	params := dtypes.Set[string]{}
	for _, pair := range sig {
		params.Add(pair.VarName)
	}
	// We find all the identifiers that we declare in the 'given' block.
	locals, rhs := ast.GetVariablesFromLhsAndRhsOfAssignments(fnNode.Given)
	// Find all the variable names in the body.
	bodyNames := ast.GetVariableNames(fnNode.Body)
	rhs.AddSet(bodyNames)
	captures := rhs.SubtractSet(params).SubtractSet(locals)
	for k := range captures {
		v, ok := env.getVar(k)
		if !ok {
			cp.P.Throw("comp/body/known", tok)
		}
		cp.Reserve(values.UNDEFINED_VALUE, nil) // It doesn't matter what we put in here 'cos we copy the values any time we call the LambdaFactory.
		cp.AddVariable(newEnv, k, v.access, v.types)
		// At the same time, the lambda factory need to know where they are in the calling vm.Vm.
		LF.CaptureLocations = append(LF.CaptureLocations, v.mLoc)
	}

	LF.Model.capturesEnd = cp.MemTop()

	// Add the function parameters.
	for _, pair := range sig { // It doesn't matter what we put in here either, because we're going to have to copy the values any time we call the function.
		cp.Reserve(0, DUMMY)
		cp.AddVariable(newEnv, pair.VarName, FUNCTION_ARGUMENT, cp.TypeNameToTypeList[pair.VarType])
	}

	LF.Model.parametersEnd = cp.MemTop()

	// Compile the locals.

	if fnNode.Given != nil {
		saveThunkList := cp.ThunkList // TODO --- this is bad coding and I know it. The whole ThunkList thing is deeply sus. Replace with a stack?
		cp.ThunkList = []Thunk{}
		cp.CompileNode(fnNode.Given, newEnv, LAMBDA)
		for _, pair := range cp.ThunkList {
			cp.Emit(Thnk, pair.MLoc, pair.CLoc)
		}
		cp.ThunkList = saveThunkList
	}

	// Function starts here.

	LF.Model.addressToCall = cp.CodeTop()

	// We have to typecheck inside the lambda, because the calling site doesn't know which function it's calling.

	// TODO !!!

	// Now we can emit the main body of the function.

	cp.CompileNode(fnNode.Body, newEnv, LAMBDA)
	LF.Model.resultLocation = cp.That()
	cp.Emit(Ret)
	cp.vmComeFrom(skipLambdaCode)

	// We have made our lambda factory! But do we need it? If there are no captures, then the function is a constant, and we
	// can just reserve it in memory.

	if captures.IsEmpty() {
		cp.Reserve(values.FUNC, *LF.Model)
		return true
	}
	cp.vm.LambdaFactories = append(cp.vm.LambdaFactories, LF)
	cp.put(Mkfn, uint32(len(cp.vm.LambdaFactories)-1))
	return false
}

func (cp *Compiler) AddVariable(env *Environment, name string, acc varAccess, types AlternateType) {
	env.data[name] = variable{mLoc: cp.That(), access: acc, types: types}
}

func (cp *Compiler) vmIf(oc Opcode, args ...uint32) {
	cp.ifStack = append(cp.ifStack, cp.CodeTop())
	cp.Emit(oc, (append(args, DUMMY))...)
}

func (cp *Compiler) vmEndIf() {
	cLoc := cp.ifStack[len(cp.ifStack)-1]
	cp.ifStack = cp.ifStack[:len(cp.ifStack)-1]
	cp.vm.Code[cLoc].MakeLastArg(cp.CodeTop())
}

type bkGoto int

func (cp *Compiler) vmGoTo() bkGoto {
	cp.Emit(Jmp, DUMMY)
	return bkGoto(cp.CodeTop() - 1)
}

type bkEarlyReturn int

func (cp *Compiler) vmEarlyReturn(mLoc uint32) bkEarlyReturn {
	cp.Emit(Asgm, DUMMY, mLoc)
	cp.Emit(Jmp, DUMMY)
	return bkEarlyReturn(cp.CodeTop() - 2)
}

func (cp *Compiler) vmConditionalEarlyReturn(oc Opcode, args ...uint32) bkEarlyReturn {
	mLoc := args[len(args)-1]
	cp.Emit(oc, append(args[:len(args)-1], cp.CodeTop()+3)...)
	return cp.vmEarlyReturn(mLoc)
}

func (cp *Compiler) vmComeFrom(items ...any) {
	for _, item := range items {
		switch item := item.(type) {
		case bkGoto:
			if uint32(item) == DUMMY {
				continue
			}
			cp.vm.Code[uint32(item)].MakeLastArg(cp.CodeTop())
		case bkEarlyReturn:
			if uint32(item) == DUMMY {
				continue
			}
			cp.vm.Code[uint32(item)].Args[0] = cp.That()
			cp.vm.Code[uint32(item)+1].MakeLastArg(cp.CodeTop())
		default:
			panic("Can't ComeFrom that!")
		}
	}
}

// The heart of the compiler. It starts by taking a snapshot of the vm. It then does a big switch on the node type
// and compiles accordingly. It then performs some sanity checks and, if the compiled expression is constant,
// evaluates it and uses the snapshot to roll back the vm.
func (cp *Compiler) CompileNode(node ast.Node, env *Environment, ac cpAccess) (AlternateType, bool) {
	cp.showCompile = settings.SHOW_COMPILER && !(settings.IGNORE_BOILERPLATE && settings.ThingsToIgnore.Contains(node.GetToken().Source))
	rtnTypes, rtnConst := AlternateType{}, true
	state := cp.getState()
	cT := cp.CodeTop()
NodeTypeSwitch:
	switch node := node.(type) {
	case *ast.AssignmentExpression:
		// TODO --- need to do this better after we implement tuples
		if node.Left.GetToken().Type != token.IDENT {
			cp.P.Throw("comp/assign/ident", node.Left.GetToken())
			break NodeTypeSwitch
		}
		name := node.Left.(*ast.Identifier).Value
		switch node.Token.Type {
		case token.GVN_ASSIGN:
			thunkStart := cp.Next()
			types, cst := cp.CompileNode(node.Right, env, ac)
			cp.Emit(Ret)
			if cst {
				cp.AddVariable(env, name, LOCAL_TRUE_CONSTANT, types)
				rtnTypes, rtnConst = AltType(values.CREATED_LOCAL_CONSTANT), true
				break NodeTypeSwitch
			}
			cp.AddVariable(env, name, LOCAL_CONSTANT_THUNK, types)
			cp.ThunkList = append(cp.ThunkList, Thunk{cp.That(), thunkStart})
			rtnTypes, rtnConst = AltType(values.CREATED_LOCAL_CONSTANT), false
			break NodeTypeSwitch
		case token.CMD_ASSIGN:
			rhsIsError := bkEarlyReturn(DUMMY)
			rTypes, _ := cp.CompileNode(node.Right, env, ac)
			if rTypes.Contains(values.ERROR) {
				rhsIsError = cp.vmConditionalEarlyReturn(Qtyp, cp.That(), uint32(values.ERROR), cp.That())
				rtnTypes = AltType(values.SUCCESSFUL_VALUE, values.ERROR)
			} else {
				rtnTypes = AltType(values.SUCCESSFUL_VALUE)
			}
			rtnConst = false // The initialization/mutation in the assignment makes it variable whatever the RHS is.
			v, ok := env.getVar(name)
			if !ok { // Then we create a local variable.
				cp.Reserve(values.UNDEFINED_VALUE, DUMMY)
				cp.AddVariable(env, name, LOCAL_VARIABLE, rTypes.without(simpleType(values.ERROR)))
				cp.Emit(Asgm, cp.That(), cp.That()-1)
				cp.put(Asgm, values.C_OK)
				break NodeTypeSwitch
			} // Otherwise we update the variable we've got.
			// TODO --- type checking after refactoring type representation.
			if v.access == REFERENCE_VARIABLE {
				cp.Emit(Aref, v.mLoc, cp.That())
				cp.vmComeFrom(rhsIsError)
				break NodeTypeSwitch
			}
			cp.Emit(Asgm, v.mLoc, cp.That())
			cp.put(Asgm, values.C_OK)
			cp.vmComeFrom(rhsIsError)
			break NodeTypeSwitch
		case token.ASSIGN: // If this hasn't been turned into some other kind of _ASSIGN then we're in the REPL.
			rhsIsError := bkEarlyReturn(DUMMY)
			rTypes, _ := cp.CompileNode(node.Right, env, ac)
			if rTypes.Contains(values.ERROR) {
				rhsIsError = cp.vmConditionalEarlyReturn(Qtyp, cp.That(), uint32(values.ERROR), cp.That())
				rtnTypes = AltType(values.SUCCESSFUL_VALUE, values.ERROR)
			} else {
				rtnTypes = AltType(values.SUCCESSFUL_VALUE)
			}
			rtnConst = false // The initialization/mutation in the assignment makes it variable whatever the RHS is.
			v, ok := env.getVar(name)
			if !ok {
				cp.P.Throw("comp/var/exist", node.GetToken(), name)
				break NodeTypeSwitch
			}
			if !((v.access == GLOBAL_VARIABLE_PUBLIC) || (v.access == GLOBAL_VARIABLE_PRIVATE && ac != REPL)) {
				cp.P.Throw("comp/var/var", node.GetToken(), name)
				break NodeTypeSwitch
			}
			// TODO --- type checking after refactoring type representation.
			cp.Emit(Asgm, v.mLoc, cp.That())
			cp.put(Asgm, values.C_OK)
			cp.vmComeFrom(rhsIsError)
			break NodeTypeSwitch
		default: // Of switch on ast.Assignment.
			cp.P.Throw("comp/assign", node.GetToken())
			break NodeTypeSwitch
		}
	case *ast.Bling:
		cp.P.Throw("comp/bling/wut", node.GetToken())
		break
	case *ast.BooleanLiteral:
		cp.Reserve(values.BOOL, node.Value)
		rtnTypes, rtnConst = AltType(values.BOOL), true
		break
	case *ast.FloatLiteral:
		cp.Reserve(values.FLOAT, node.Value)
		rtnTypes, rtnConst = AltType(values.FLOAT), true
		break
	case *ast.FuncExpression:
		rtnConst = cp.compileLambda(env, node, node.GetToken())
		rtnTypes = AltType(values.FUNC) // In the case where the function is a constant (i.e. has no captures), the compileLambda function will emit an assignment rather than a lambda factory.)
		break                           // Things that return functions and snippets are not folded, even if they are constant.
	case *ast.Identifier:
		resolvingCompiler := cp.getResolvingCompiler(node, node.Namespace, ac)
		cp.P.GetErrorsFrom(resolvingCompiler.P)
		enumElement, ok := resolvingCompiler.EnumElements[node.Value]
		if ok {
			if cp.vm.concreteTypes[cp.vm.Mem[enumElement].T].isPrivate() {
				cp.P.Throw("comp/private/enum", node.GetToken(), cp.vm.DescribeType(cp.vm.Mem[enumElement].T))
				break
			}
			cp.put(Asgm, enumElement)
			rtnTypes, rtnConst = AltType(cp.vm.Mem[enumElement].T), true
			break
		}
		labelNumberLocation, ok := resolvingCompiler.FieldLabelsInMem[node.Value]
		if ok {
			if (cp != resolvingCompiler || ac == REPL) && resolvingCompiler.LabelIsPrivate[cp.vm.Mem[labelNumberLocation].V.(int)] {
				cp.P.Throw("comp/private/label", node.GetToken())
				break
			}
			cp.put(Asgm, labelNumberLocation)
			rtnTypes, rtnConst = AltType(values.LABEL), true
			break
		}
		var v *variable
		if resolvingCompiler != cp {
			v, ok = resolvingCompiler.GlobalConsts.getVar(node.Value)
		} else {
			v, ok = env.getVar(node.Value)
		}
		if !ok {
			cp.P.Throw("comp/ident/known", node.GetToken())
			break
		}
		if (v.access == GLOBAL_CONSTANT_PRIVATE || v.access == GLOBAL_VARIABLE_PRIVATE) && ac == REPL {
			cp.P.Throw("comp/ident/private", node.GetToken())
			break
		}
		if v.access == LOCAL_CONSTANT_THUNK {
			cp.Emit(Untk, v.mLoc)
		}
		if v.access == REFERENCE_VARIABLE {
			cp.put(Dref, v.mLoc)
			rtnTypes = cp.TypeNameToTypeList["single?"]
		} else {
			cp.put(Asgm, v.mLoc)
			rtnTypes = v.types
		}
		rtnConst = ALL_CONST_ACCESS.Contains(v.access)
		break
	case *ast.IndexExpression:
		containerType, ctrConst := cp.CompileNode(node.Left, env, ac)
		container := cp.That()
		indexType, idxConst := cp.CompileNode(node.Index, env, ac)
		index := cp.That()
		rtnConst = ctrConst && idxConst
		errTok := cp.reserveToken(node.GetToken())
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
				cp.put(IdxL, container, index, errTok)
				break
			}
			if indexType.isOnly(values.PAIR) {
				cp.put(SliL, container, index, errTok)
				break
			}
			if indexType.isNoneOf(values.INT, values.PAIR) {
				cp.P.Throw("comp/index/list", node.GetToken())
				break
			}
			rtnTypes = cp.TypeNameToTypeList["single?"].Union(AltType(values.ERROR))
		}
		if containerType.isOnly(values.STRING) {
			if indexType.isOnly(values.INT) {
				cp.put(Idxs, container, index, errTok)
				break
			}
			if indexType.isOnly(values.PAIR) {
				cp.put(Slis, container, index, errTok)
				break
			}
			if indexType.isNoneOf(values.INT, values.PAIR) {
				cp.P.Throw("comp/index/string", node.GetToken())
				break
			}
			rtnTypes = AltType(values.ERROR, values.STRING)
		}
		if containerType.containsOnlyTuples() {
			if indexType.isOnly(values.INT) {
				cp.put(IdxT, container, index, errTok)
				break
			}
			if indexType.isOnly(values.PAIR) {
				cp.put(SliT, container, index, errTok)
				break
			}
			if indexType.isNoneOf(values.INT, values.PAIR) {
				cp.P.Throw("comp/index/tuple", node.GetToken())
				break
			}
			rtnTypes = cp.TypeNameToTypeList["single?"].Union(AltType(values.ERROR))
		}
		if containerType.isOnly(values.PAIR) {
			if indexType.isOnly(values.INT) {
				cp.put(Idxp, container, index, errTok)
				break
			}
			if indexType.isNoneOf(values.INT) {
				cp.P.Throw("comp/index/pair", node.GetToken())
				break
			}
			rtnTypes = cp.TypeNameToTypeList["single?"].Union(AltType(values.ERROR))
		}
		if containerType.isOnly(values.TYPE) {
			if indexType.isOnly(values.INT) {
				cp.put(Idxt, container, index, errTok)
				break
			}
			if indexType.isNoneOf(values.INT) {
				cp.P.Throw("comp/index/type", node.GetToken())
				break
			}
			if ctrConst {
				rtnTypes = AltType(values.ERROR, cp.vm.Mem[container].T)
			} else {
				allEnums := make(AlternateType, 0, 1+cp.vm.Ub_enums-values.FIRST_DEFINED_TYPE) // TODO --- yu only need to calculate this once.
				allEnums = append(allEnums, simpleType(values.ERROR))
				for i := values.FIRST_DEFINED_TYPE; i < cp.vm.Ub_enums; i++ {
					allEnums = append(allEnums, simpleType(i))
				}
				rtnTypes = allEnums
			}
		}
		structT, ok := containerType.isOnlyStruct(int(cp.vm.Ub_enums))
		if ok {
			structOrdinal := structT - cp.vm.Ub_enums
			if indexType.isOnly(values.LABEL) {
				if idxConst { // Then we can find the field number of the struct at compile time and throw away the computed label.
					indexNumber := cp.vm.Mem[index].V.(int)
					labelName := cp.vm.Labels[indexNumber]
					fieldNumber := cp.vm.StructResolve.Resolve(int(structOrdinal), indexNumber)
					if fieldNumber == -1 {
						cp.P.Throw("comp/index/struct/a", node.GetToken(), labelName, cp.vm.DescribeType(structT))
						break
					}
					cp.put(IxZn, container, uint32(fieldNumber))
					rtnTypes = cp.vm.concreteTypes[structT].(structType).alternateStructFields[fieldNumber]
					break
				}
				cp.put(IxZl, container, index, errTok)
				rtnTypes = AltType()
				for _, t := range cp.vm.concreteTypes[structT].(structType).alternateStructFields {
					rtnTypes = rtnTypes.Union(t)
				}
				rtnTypes = rtnTypes.Union(AltType(values.ERROR))
				break
			}
			if indexType.isNoneOf(values.LABEL) {
				cp.P.Throw("comp/index/struct/b", node.GetToken())
				break
			}
		}
		if containerType.isOnlyAssortedStructs(int(cp.vm.Ub_enums)) {
			if indexType.isOnly(values.LABEL) {
				if idxConst { // Then we can find the field number of the struct at compile time and throw away the computed label.
					labelIsPossible := false
					labelIsCertain := true
					rtnTypes = AltType()
					for _, structTypeAsSimpleType := range containerType {
						structT := values.ValueType(structTypeAsSimpleType.(simpleType))
						structNumber := structT - cp.vm.Ub_enums
						indexNumber := cp.vm.Mem[index].V.(int)
						fieldNumber := cp.vm.StructResolve.Resolve(int(structNumber), indexNumber)
						if fieldNumber != -1 {
							labelIsPossible = true
							rtnTypes = rtnTypes.Union(cp.vm.concreteTypes[structT].(structType).alternateStructFields[fieldNumber])
						} else {
							labelIsCertain = false
						}
					}
					if !labelIsPossible {
						cp.P.Throw("comp/index/struct/c", node.GetToken())
						break
					}
					if !labelIsCertain {
						rtnTypes = rtnTypes.Union(AltType(values.ERROR))
					}
					cp.put(IxZl, container, index, errTok)
					break
				}
				cp.put(IxZl, container, index, errTok)
				break
			}
		}
		// If we can't infer anything else about the types we can emit a catchall indexing operation.
		cp.put(IxXx, container, index, errTok)
		if containerType.Contains(values.TUPLE) {
			rtnTypes = cp.AnyTypeScheme
		} else {
			rtnTypes = cp.TypeNameToTypeList["single?"]
		}
	case *ast.InfixExpression:
		resolvingCompiler := cp.getResolvingCompiler(node, node.Namespace, ac)
		if resolvingCompiler.P.Infixes.Contains(node.Operator) {
			rtnTypes, rtnConst = resolvingCompiler.createFunctionCall(resolvingCompiler, node, env, ac, len(node.Namespace) > 0)
			cp.P.GetErrorsFrom(resolvingCompiler.P)
			break
		}
		if node.Operator == "," {
			rtnTypes, rtnConst = cp.emitComma(node, env, ac)
			break
		}
		if node.Operator == "==" {
			rtnTypes, rtnConst = cp.emitEquals(node, env, ac)
			break
		}
		if node.Operator == "!=" {
			rtnTypes, rtnConst = cp.emitEquals(node, env, ac)
			cp.put(Notb, cp.That())
			break
		}
		cp.P.Throw("comp/known/infix", node.GetToken())
		break
	case *ast.IntegerLiteral:
		cp.Reserve(values.INT, node.Value)
		rtnTypes, rtnConst = AltType(values.INT), true
		break
	case *ast.LazyInfixExpression:
		if node.Operator == "or" {
			lTypes, lcst := cp.CompileNode(node.Left, env, ac)
			if !lTypes.Contains(values.BOOL) {
				cp.P.Throw("comp/bool/or/left", node.GetToken())
				break
			}
			leftRg := cp.That()
			cp.Emit(Qtru, leftRg, cp.Next()+2)
			skipElse := cp.vmGoTo()
			rTypes, rcst := cp.CompileNode(node.Right, env, ac)
			if !rTypes.Contains(values.BOOL) {
				cp.P.Throw("comp/bool/or/right", node.GetToken())
				break
			}
			rightRg := cp.That()
			cp.vmComeFrom(skipElse)
			cp.put(Orb, leftRg, rightRg)
			rtnTypes, rtnConst = AltType(values.BOOL), lcst && rcst
			break
		}
		if node.Operator == "and" {
			lTypes, lcst := cp.CompileNode(node.Left, env, ac)
			if !lTypes.Contains(values.BOOL) {
				cp.P.Throw("comp/bool/and/left", node.GetToken())
				break
			}
			leftRg := cp.That()
			cp.vmIf(Qtru, leftRg)
			rTypes, rcst := cp.CompileNode(node.Right, env, ac)
			if !rTypes.Contains(values.BOOL) {
				cp.P.Throw("comp/bool/and/right", node.GetToken())
				break
			}
			rightRg := cp.That()
			cp.vmEndIf()
			cp.put(Andb, leftRg, rightRg)
			rtnTypes, rtnConst = AltType(values.BOOL), lcst && rcst
			break
		}
		if node.Operator == ":" {
			if node.Left.GetToken().Type == token.ELSE {
				rtnTypes, rtnConst = cp.CompileNode(node.Right, env, ac)
				break
			}
			lTypes, lcst := cp.CompileNode(node.Left, env, ac)
			if !lTypes.Contains(values.BOOL) {
				cp.P.Throw("comp/bool/cond", node.GetToken())
				break
			}
			// TODO --- what if it's not *only* bool?
			leftRg := cp.That()
			cp.vmIf(Qtru, leftRg)
			rTypes, rcst := cp.CompileNode(node.Right, env, ac)
			ifCondition := cp.vmEarlyReturn(cp.That())
			cp.vmEndIf()
			cp.put(Asgm, values.C_U_OBJ)
			cp.vmComeFrom(ifCondition)
			rtnTypes, rtnConst = rTypes.Union(AltType(values.UNSAT)), lcst && rcst
			break
		}
		if node.Operator == ";" {
			lTypes, lcst := cp.CompileNode(node.Left, env, ac)
			leftRg := cp.That()
			// We deal with the case where the newline is separating local constant definitions
			// in the 'given' block.
			if lTypes.isOnly(values.CREATED_LOCAL_CONSTANT) {
				_, cst := cp.CompileNode(node.Right, env, ac)
				rtnTypes, rtnConst = AltType(values.CREATED_LOCAL_CONSTANT), lcst && cst
				break
			}
			// We may be executing a command.
			cmdRet := lTypes.IsLegalCmdReturn()
			if (cmdRet && lTypes.isOnly(values.BREAK)) || (!cmdRet && !lTypes.Contains(values.UNSAT)) {
				// TODO --- implement warnings.
				// cp.p.Throw("comp/unreachable", node.GetToken())
				// break
			}
			var rTypes AlternateType
			var rcst bool
			if cmdRet { // It could be error, break, OK, or an unsatisfied conditional.
				ifBreak := bkEarlyReturn(DUMMY)
				ifError := bkEarlyReturn(DUMMY)
				ifCouldBeUnsatButIsnt := bkEarlyReturn(DUMMY)
				if lTypes.Contains(values.BREAK) {
					ifBreak = cp.vmConditionalEarlyReturn(Qtyp, leftRg, uint32(values.BREAK), values.C_BREAK)
				}
				if lTypes.Contains(values.ERROR) {
					ifError = cp.vmConditionalEarlyReturn(Qtyp, leftRg, uint32(values.ERROR), leftRg)
				}
				if lTypes.Contains(values.UNSAT) { // Then it is an else-less conditional or a try, and it it isn't UNSAT then we should skip the right node.
					ifCouldBeUnsatButIsnt = cp.vmConditionalEarlyReturn(Qntp, leftRg, uint32(values.UNSAT), leftRg)
				}
				rTypes, _ = cp.CompileNode(node.Right, env, ac) // In a cmd we wish rConst to remain false to avoid folding.
				cp.vmComeFrom(ifBreak, ifError, ifCouldBeUnsatButIsnt)
				rtnTypes, rtnConst = lTypes.Union(rTypes), lcst && rcst

				break
			} else { // Otherwise it's functional.
				cp.vmIf(Qsat, leftRg)
				lhsIsSat := cp.vmEarlyReturn(leftRg)
				cp.vmEndIf()
				rTypes, rcst = cp.CompileNode(node.Right, env, ac)
				cp.put(Asgm, cp.That())
				cp.vmComeFrom(lhsIsSat)
				rtnConst = lcst && rcst
				if !(lTypes.Contains(values.UNSAT) && rTypes.Contains(values.UNSAT)) {
					rtnTypes = lTypes.Union(rTypes).without(tp(values.UNSAT))
				} else {
					rtnTypes = lTypes.Union(rTypes)
				}
				break
			}
		}
	case *ast.ListExpression:
		var containedTypes AlternateType
		containedTypes, rtnConst = cp.CompileNode(node.List, env, ac)
		backTrackTo, failed := cp.emitErrorBoilerplate(containedTypes, "comp/list/err", node.GetToken(), false)
		if failed {
			break
		}
		cp.put(List, cp.That())
		cp.vm.Code[backTrackTo].Args[0] = cp.That()
		rtnTypes = AltType(values.LIST)
		break
	case *ast.LogExpression:
		rtnConst = false // Since a log expression has a side-effect, it can't be folded even if it's constant.
		initStr := cp.Reserve(values.STRING, "Log at line "+text.YELLOW+strconv.Itoa(node.GetToken().Line)+text.RESET+":\n    ")
		output := cp.Reserve(values.STRING, "")
		cp.vmIf(Qlog)
		cp.Emit(Logn)
		cp.Emit(Asgm, output, initStr)
		logMayHaveError := cp.compileLog(node, env, ac)
		cp.Emit(Log, output)
		cp.Emit(Logy)
		ifRuntimeError := bkEarlyReturn(DUMMY)
		if logMayHaveError {
			ifRuntimeError = cp.vmConditionalEarlyReturn(Qtyp, output, uint32(values.ERROR), output)
		}
		cp.vmEndIf()
		// Syntactically a log expression is attached to a normal expression, which we must now compile.
		switch node.GetToken().Type {
		case token.IFLOG:
			ifNode := &ast.LazyInfixExpression{Operator: ":", Token: *node.GetToken(), Left: node.Left, Right: node.Right}
			rtnTypes, _ = cp.CompileNode(ifNode, env, ac)
		case token.PRELOG:
			rtnTypes, _ = cp.CompileNode(node.Right, env, ac)
		default: // I.e. token.LOG.
			rtnTypes, _ = cp.CompileNode(node.Left, env, ac)
		}
		cp.vmComeFrom(ifRuntimeError)
		break
	case *ast.LoopExpression:
		rtnConst = false
		loopStart := cp.CodeTop()
		bodyTypes, _ := cp.CompileNode(node.Code, env, ac)
		if cp.P.ErrorsExist() {
			break
		}
		result := cp.That()
		if !bodyTypes.isLegalReturnFromLoopBody() {
			cp.P.Throw("comp/loop/body", node.GetToken())
			break
		}
		if bodyTypes.isNoneOf(values.BREAK, values.ERROR) {
			cp.P.Throw("comp/loop/infinite", node.GetToken())
			break
		}
		cp.Emit(Qntp, result, uint32(values.SUCCESSFUL_VALUE), loopStart)
		if bodyTypes.isOnly(values.ERROR) {
			rtnTypes = AltType(values.ERROR)
			break
		}
		if bodyTypes.isOnly(values.BREAK) {
			cp.put(Asgm, values.C_OK)
			rtnTypes = AltType(values.SUCCESSFUL_VALUE)
			break
		}
		ifError := cp.vmConditionalEarlyReturn(Qtyp, result, uint32(values.ERROR), result)
		cp.put(Asgm, values.C_OK)
		cp.vmComeFrom(ifError)
		rtnTypes = AltType(values.SUCCESSFUL_VALUE, values.ERROR)
		break
	case *ast.Nothing:
		cp.put(Asgm, values.C_EMPTY_TUPLE)
		rtnTypes, rtnConst = AlternateType{finiteTupleType{}}, true
	case *ast.PipingExpression: // I.e. -> >> and -> and ?> .
		lhsTypes, lhsConst := cp.CompileNode(node.Left, env, ac)
		if cp.P.ErrorsExist() {
			break
		}
		// And that's about all the streaming operators really do have in common under the hood, so let's do a switch on the operators.
		var rhsConst bool
		switch node.Operator {
		case "->":
			rtnTypes, rhsConst = cp.compilePipe(lhsTypes, lhsConst, node.Right, env, ac)
		case ">>":
			rtnTypes, rhsConst = cp.compileMappingOrFilter(lhsTypes, lhsConst, node.Right, env, false, ac)
		default:
			rtnTypes, rhsConst = cp.compileMappingOrFilter(lhsTypes, lhsConst, node.Right, env, true, ac)
		}
		rtnConst = lhsConst && rhsConst
		break
	case *ast.PrefixExpression: // Note that the vmmaker will have caught xcall and builtin expressions already.
		if node.Token.Type == token.NOT {
			allTypes, cst := cp.CompileNode(node.Args[0], env, ac)
			if allTypes.isOnly(values.BOOL) {
				cp.put(Notb, cp.That())
				rtnTypes, rtnConst = AltType(values.BOOL), cst
				break
			}
			if !allTypes.Contains(values.BOOL) {
				cp.P.Throw("comp/bool/not", node.GetToken())
				break
			}
		}
		if node.Token.Type == token.GLOBAL { // This is in effect a compiler directive, it doesn't need to emit any code besides `ok`, it just mutates the environment.
			for _, v := range node.Args {
				switch arg := v.(type) {
				case *ast.Identifier:
					variable, ok := cp.GlobalVars.getVar(arg.Value)
					if !ok {
						cp.P.Throw("comp/global/global", arg.GetToken())
						break NodeTypeSwitch
					}
					env.data[arg.Value] = *variable
				default:
					cp.P.Throw("comp/global/ident", arg.GetToken())
					break NodeTypeSwitch
				}
			}
			rtnTypes, rtnConst = AltType(values.SUCCESSFUL_VALUE), false
			break
		}
		resolvingCompiler := cp.getResolvingCompiler(node, node.Namespace, ac)
		if cp.P.ErrorsExist() {
			break
		}
		var (
			v  *variable
			ok bool
		)
		if resolvingCompiler != cp {
			v, ok = resolvingCompiler.GlobalConsts.getVar(node.Operator)
		} else {
			v, ok = env.getVar(node.Operator)
		}
		if ok && v.types.Contains(values.FUNC) {
			if v.access == LOCAL_CONSTANT_THUNK {
				cp.Emit(Untk, v.mLoc)
			}
			operands := []uint32{v.mLoc}
			for _, arg := range node.Args {
				cp.CompileNode(arg, env, ac)
				operands = append(operands, cp.That())
			}
			if cp.P.ErrorsExist() {
				break
			}
			if v.types.isOnly(values.FUNC) { // Then no type checking for v.
				cp.put(Dofn, operands...)
			} // TODO --- what if not?
			rtnConst = false
			rtnTypes = cp.AnyTypeScheme
			break
		}
		if resolvingCompiler.P.Prefixes.Contains(node.Operator) || resolvingCompiler.P.Functions.Contains(node.Operator) {
			rtnTypes, rtnConst = resolvingCompiler.createFunctionCall(resolvingCompiler, node, env, ac, len(node.Namespace) > 0)
			cp.P.GetErrorsFrom(resolvingCompiler.P)
			break
		}
		cp.P.Throw("comp/known/prefix", node.GetToken())
		break
	case *ast.StringLiteral:
		cp.Reserve(values.STRING, node.Value)
		rtnTypes, rtnConst = AltType(values.STRING), true
		break
	case *ast.StructExpression:
		panic("This is used only in the vmmaker and should never be compiled.")
	case *ast.SuffixExpression:
		resolvingCompiler := cp.getResolvingCompiler(node, node.Namespace, ac)
		if node.GetToken().Type == token.EMDASH {
			switch t := node.Args[0].(type) {
			case *ast.TypeLiteral:
				skipOverCompiledSnippet := cp.vmGoTo()
				snF := cp.reserveSnippetFactory(t.Value, env, node, ac)
				cp.vmComeFrom(skipOverCompiledSnippet)
				cp.put(MkSn, snF)
				rtnTypes, rtnConst = AltType(cp.StructNameToTypeNumber[t.Value]), false
				break NodeTypeSwitch
			default:
				cp.P.Throw("comp/snippet/type", node.Args[0].GetToken()) // There is no reason why this should be a first-class value, that would just be confusing. Hence the error.
				break NodeTypeSwitch
			}
		}
		if resolvingCompiler.P.Suffixes.Contains(node.Operator) {
			rtnTypes, rtnConst = resolvingCompiler.createFunctionCall(resolvingCompiler, node, env, ac, len(node.Namespace) > 0)
			cp.P.GetErrorsFrom(resolvingCompiler.P)
			break
		}
		cp.P.Throw("comp/known/suffix", node.GetToken())
		break
	case *ast.TryExpression:
		ident := node.VarName
		v, exists := env.getVar(ident)
		if exists && (v.access == GLOBAL_CONSTANT_PRIVATE || v.access == GLOBAL_CONSTANT_PUBLIC ||
			v.access == GLOBAL_VARIABLE_PRIVATE || v.access == GLOBAL_VARIABLE_PUBLIC || v.access == LOCAL_CONSTANT_THUNK) {
			cp.P.Throw("comp/try/var", node.GetToken())
			break
		}
		var err uint32
		if !exists {
			err = cp.Reserve(values.NULL, nil)
			cp.AddVariable(env, ident, LOCAL_VARIABLE, AltType(values.NULL, values.ERROR))
		} else {
			err = v.mLoc
		}
		tryTypes, _ := cp.CompileNode(node.Right, env, ac)
		if tryTypes.isNoneOf(values.ERROR, values.SUCCESSFUL_VALUE) {
			cp.P.Throw("comp/try/return", node.GetToken())
			break
		}
		cp.Emit(Qtyp, cp.That(), uint32(values.ERROR), cp.CodeTop()+4)
		cp.Emit(Asgm, err, cp.That())
		cp.Emit(Asgm, cp.That(), values.C_U_OBJ)
		cp.Emit(Qtyp, cp.That(), uint32(values.ERROR), cp.CodeTop()+2)
		cp.Emit(Asgm, cp.That(), values.C_OK)
		rtnTypes, rtnConst = AltType(values.UNSAT, values.SUCCESSFUL_VALUE), false
		break
	case *ast.TypeLiteral:
		resolvingCompiler := cp.getResolvingCompiler(node, node.Namespace, ac)
		typeName := node.Value
		switch { // We special-case it a bit because otherwise a string would look like a varchar(0).
		case typeName == "string":
			cp.Reserve(values.TYPE, values.AbstractType{[]values.ValueType{values.STRING}, DUMMY})
		case typeName == "string?":
			cp.Reserve(values.TYPE, values.AbstractType{[]values.ValueType{values.NULL, values.STRING}, DUMMY})
		default:
			abType := resolvingCompiler.TypeNameToTypeList[typeName].ToAbstractType()
			if (ac == REPL || resolvingCompiler != cp) && cp.vm.isPrivate(abType) {
				cp.P.Throw("comp/private/type", node.GetToken())
			}
			cp.Reserve(values.TYPE, abType)
		}
		rtnTypes, rtnConst = AltType(values.TYPE), true
		break
	case *ast.UnfixExpression:
		resolvingCompiler := cp.getResolvingCompiler(node, node.Namespace, ac)
		if resolvingCompiler.P.Unfixes.Contains(node.Operator) {
			rtnTypes, rtnConst = resolvingCompiler.createFunctionCall(resolvingCompiler, node, env, ac, len(node.Namespace) > 0)
			cp.P.GetErrorsFrom(resolvingCompiler.P)
			break
		}
		cp.P.Throw("comp/known/unfix", node.GetToken()) // TODO --- can errors like this even arise or must they be caught in the parser?
		break
	default:
		panic("Unimplemented node type.")
	}
	if !rtnTypes.IsLegalCmdReturn() && !rtnTypes.IsLegalDefReturn() && !rtnTypes.Contains(values.COMPILE_TIME_ERROR) {
		cp.P.Throw("comp/sanity", node.GetToken())
	}
	if ac == DEF && !rtnTypes.IsLegalDefReturn() {
		cp.P.Throw("comp/fcis", node.GetToken())
	}
	if cp.P.ErrorsExist() {
		return AltType(values.COMPILE_TIME_ERROR), false // False because we don't want any code folding to happen as that could remove information about the error.
	}
	if rtnConst && (!rtnTypes.hasSideEffects()) && cp.CodeTop() > cT {
		cp.Emit(Ret)
		cp.vm.Run(cT)
		result := cp.vm.Mem[cp.That()]
		if result.T == values.TUPLE {
			tType := finiteTupleType{}
			for _, v := range result.V.([]values.Value) {
				tType = append(tType, simpleType(v.T))
			}
			rtnTypes = AlternateType{tType}
		} else {
			rtnTypes = AltType(result.T)
		}
		if !rtnTypes.containsAnyOf(cp.vm.codeGeneratingTypes.ToSlice()...) {
			cp.rollback(state)
			cp.Reserve(result.T, result.V)
		}
	}
	return rtnTypes, rtnConst
}

// This needs its own very special logic because the type it returns has to be composed in a different way from all the other operators.
func (cp *Compiler) emitComma(node *ast.InfixExpression, env *Environment, ac cpAccess) (AlternateType, bool) {
	lTypes, lcst := cp.CompileNode(node.Args[0], env, ac)
	if lTypes.isOnly(values.ERROR) {
		cp.P.Throw("comp/tuple/err/a", node.GetToken())
	}
	left := cp.That()
	rTypes, rcst := cp.CompileNode(node.Args[2], env, ac)
	if rTypes.isOnly(values.ERROR) {
		cp.P.Throw("comp/tuple/err/b", node.GetToken())
	}
	right := cp.That()
	leftIsError := bkEarlyReturn(DUMMY)
	rightIsError := bkEarlyReturn(DUMMY)
	if lTypes.Contains(values.ERROR) {
		cp.vmConditionalEarlyReturn(Qtyp, left, uint32(tp(values.ERROR)), left)
	}
	if rTypes.Contains(values.ERROR) {
		cp.vmConditionalEarlyReturn(Qtyp, right, uint32(tp(values.ERROR)), right)
	}
	leftMustBeSingle, leftMustBeTuple := lTypes.mustBeSingleOrTuple()
	rightMustBeSingle, rightMustBeTuple := rTypes.mustBeSingleOrTuple()
	switch {
	case leftMustBeSingle && rightMustBeSingle:
		cp.put(Cc11, left, right)
	case leftMustBeSingle && rightMustBeTuple:
		cp.put(Cc1T, left, right)
	case leftMustBeTuple && rightMustBeSingle:
		cp.put(CcT1, left, right)
	case leftMustBeTuple && rightMustBeTuple:
		cp.put(CcTT, left, right)
	default:
		cp.put(Ccxx, left, right) // We can after all let the operation dispatch for us.
	}
	cp.vmComeFrom(leftIsError, rightIsError)
	lT := lTypes.reduce()
	rT := rTypes.reduce()
	cst := lcst && rcst
	switch lT := lT.(type) {
	case finiteTupleType:
		switch rT := rT.(type) {
		case finiteTupleType:
			return AlternateType{append(lT, rT...)}, cst
		case TypedTupleType:
			return AlternateType{TypedTupleType{rT.T.Union(getAllTypes(lT))}}, cst
		case simpleType:
			return AlternateType{finiteTupleType{append(lT, rT)}}, cst
		case AlternateType:
			return AlternateType{finiteTupleType{append(lT, rT)}}, cst // TODO --- check if this works.
		default:
			panic("We shouldn't be here!")
		}
	case TypedTupleType:
		switch rT := rT.(type) {
		case finiteTupleType:
			return AlternateType{TypedTupleType{lT.T.Union(getAllTypes(rT))}}, cst
		case TypedTupleType:
			return AlternateType{TypedTupleType{lT.T.Union(rT.T)}}, cst
		case simpleType:
			return AlternateType{TypedTupleType{lT.T.Union(AlternateType{rT})}}, cst
		case AlternateType:
			return AlternateType{TypedTupleType{lT.T.Union(getAllTypes(rT))}}, cst
		default:
			panic("We shouldn't be here!")
		}
	case simpleType:
		switch rT := rT.(type) {
		case finiteTupleType:
			return AlternateType{append(finiteTupleType{lT}, rT...)}, cst
		case TypedTupleType:
			return AlternateType{TypedTupleType{rT.T.Union(AlternateType{lT})}}, cst
		case simpleType:
			return AlternateType{finiteTupleType{lT, rT}}, cst
		case AlternateType:
			return AlternateType{finiteTupleType{lT, rT}}, cst
		default:
			panic("We shouldn't be here!")
		}
	case AlternateType:
		switch rT := rT.(type) {
		case finiteTupleType:
			return AlternateType{append(finiteTupleType{lT}, rT...)}, cst
		case TypedTupleType:
			return AlternateType{TypedTupleType{rT.T.Union(lT)}}, cst
		case simpleType:
			return AlternateType{finiteTupleType{lT, rT}}, cst
		case AlternateType:
			return append(lT, rT...), cst
		default:
			panic("We shouldn't be here!")
		}
	default:
		panic("We shouldn't be here!")
	}
}

// Finds the appropriate compiler for a given namespace.
func (cp *Compiler) getResolvingCompiler(node ast.Node, namespace []string, ac cpAccess) *Compiler {
	lC := cp
	for _, name := range namespace {
		srv, ok := lC.Services[name]
		if !ok {
			cp.P.Throw("comp/namespace/exist", node.GetToken(), name)
			return nil
		}
		lC = srv.Cp
		if lC.P.Private && (ac == REPL || len(namespace) > 1) {
			cp.P.Throw("comp/namespace/private", node.GetToken(), name)
			return nil
		}
	}
	return lC
}

// TODO --- this can be replaced with other generalizations.
func (cp *Compiler) emitErrorBoilerplate(types AlternateType, errCode string, tok *token.Token, appendToken bool) (uint32, bool) {
	failed := false
	var backtrackTo uint32
	if types.isOnly(values.ERROR) {
		cp.P.Throw("comp/list/err", tok)
		failed = true
	}
	if types.Contains(values.ERROR) {
		cp.Emit(Qtyp, cp.That(), uint32(values.ERROR), cp.CodeTop()+3)
		backtrackTo = cp.CodeTop()
		if appendToken {
			cp.Emit(Adtk, DUMMY, cp.That(), cp.reserveToken(tok))
		} else {
			cp.Emit(Asgm, DUMMY, cp.That())
		}
		cp.Emit(Ret)
	}
	return backtrackTo, failed
}

func getAllTypes(ts typeScheme) AlternateType {
	result := AlternateType{}
	switch ts := ts.(type) {
	case AlternateType:
		for _, v := range ts {
			result = result.Union(getAllTypes(v))
		}
	case TypedTupleType:
		result = ts.T
	case finiteTupleType:
		for _, v := range ts {
			result = result.Union(getAllTypes(v))
		}
	case simpleType:
		result = AlternateType{ts}
	default:
		panic("We shouldn't be here!")
	}
	return result
}

func (ts AlternateType) reduce() typeScheme { // Turns alternative types with only on option into their contents.
	if len(ts) == 1 {
		return ts[0]
	}
	return ts
}

func (t AlternateType) mustBeSingleOrTuple() (bool, bool) {
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

// The compiler in the method receiver is where we look up the function name (the "resolving compiler").
// The arguments need to be compiled in their own namespace by the argCompiler, unless they're bling in which case we
// use them to look up the function.
func (cp *Compiler) createFunctionCall(argCompiler *Compiler, node ast.Callable, env *Environment, ac cpAccess, libcall bool) (AlternateType, bool) {
	args := node.GetArgs()
	if len(args) == 1 {
		switch args[0].(type) {
		case *ast.Nothing:
			args = []ast.Node{}
		}
	}
	b := &bindle{tok: node.GetToken(),
		treePosition: cp.P.FunctionGroupMap[node.GetToken().Literal].Tree,
		outLoc:       cp.reserveError("vm/oopsie", node.GetToken()),
		env:          env,
		valLocs:      make([]uint32, len(args)),
		types:        make(finiteTupleType, len(args)),
		access:       ac,
		libcall:      libcall,
	}
	backtrackList := make([]uint32, len(args))
	var cstI bool
	cst := true
	for i, arg := range args {
		backtrackList[i] = DUMMY
		if i < cp.P.FunctionGroupMap[node.GetToken().Literal].RefCount { // It might be a reference variable
			if arg.GetToken().Type != token.IDENT {
				cp.P.Throw("comp/ref/ident", arg.GetToken())
				return AltType(values.COMPILE_TIME_ERROR), false
			}
			var v *variable
			v, ok := env.getVar(arg.GetToken().Literal)
			if !ok {
				if ac == REPL {
					cp.P.Throw("comp/ref/var", arg.GetToken())
					return AltType(values.COMPILE_TIME_ERROR), false
				} else { // We must be in a command. We can create a local variable.
					cp.Reserve(values.UNDEFINED_VALUE, nil)
					newVar := variable{cp.That(), LOCAL_VARIABLE, cp.TypeNameToTypeList["single?"]}
					env.data[arg.GetToken().Literal] = newVar
					v = &newVar
				}
			}
			b.types[i] = cp.TypeNameToTypeList["single?"]
			cst = false
			if v.access == REFERENCE_VARIABLE { // If the variable we're passing is already a reference variable, then we don't re-wrap it.
				cp.put(Asgm, v.mLoc)
				b.valLocs[i] = cp.That()
			} else {
				cp.Reserve(values.REF, v.mLoc)
				b.valLocs[i] = cp.That()
			}
			continue
		}
		switch arg := arg.(type) { // It might be bling.
		case *ast.Bling:
			b.types[i] = AlternateType{blingType{arg.Value}}
			cp.Reserve(values.BLING, arg.Value)
			b.valLocs[i] = cp.That()
		default: // Otherwise we emit code to evaluate it.
			b.types[i], cstI = argCompiler.CompileNode(arg, env, ac)
			if b.types[i].(AlternateType).Contains(values.COMPILE_TIME_ERROR) {
				return AltType(values.COMPILE_TIME_ERROR), false
			}
			cst = cst && cstI
			b.valLocs[i] = cp.That()
			if b.types[i].(AlternateType).isOnly(values.ERROR) {
				cp.P.Throw("comp/error/arg", arg.GetToken())
				return AltType(values.COMPILE_TIME_ERROR), false
			}
			if b.types[i].(AlternateType).Contains(values.ERROR) {
				cp.Emit(Qtyp, cp.That(), uint32(tp(values.ERROR)), cp.CodeTop()+4)
				backtrackList[i] = cp.CodeTop()
				cp.Emit(Asgm, DUMMY, cp.That())
				cp.Emit(Adtk, cp.That(), cp.That(), cp.reserveToken(arg.GetToken()))
				cp.Emit(Ret)
			}
		}
	}
	b.cst = cst
	// Having gotten the arguments, we create the function call itself.
	returnTypes := cp.generateNewArgument(b) // This is our path into the recursion that will in fact generate the whole function call.

	cp.put(Asgm, b.outLoc)
	if returnTypes.isOnly(values.ERROR) && node.GetToken().Literal != "error" {
		cp.P.Throw("comp/error/return", b.tok)
	}
	for _, v := range backtrackList {
		if v != DUMMY {
			cp.vm.Code[v].Args[0] = cp.That()
		}
	}
	if returnTypes.Contains(values.ERROR) {
		cp.Emit(Qtyp, cp.That(), uint32(values.ERROR), cp.CodeTop()+3)
		cp.Emit(Adtk, cp.That(), cp.That(), cp.reserveToken(b.tok))
		cp.Emit(Ret)
	}
	return returnTypes, cst
}

type bindle struct {
	treePosition *ast.FnTreeNode // Our position on the function tree.
	branchNo     int             // The number of the branch in the function tree.
	argNo        int             // The number of the argument we're looking at.
	index        int             // The index we're looking at in the argument we're looking at.
	lengths      dtypes.Set[int] // The possible arities of the values of the argument we're looking at.
	maxLength    int             // The maximum of the 'lengths' set, or -1 if the set contains this.
	targetList   AlternateType   // The possible types associated with this tree position.
	doneList     AlternateType   // The types we've looked at up to and including those of the current branchNo.
	valLocs      []uint32        // The locations of the values evaluated from the arguments.
	types        finiteTupleType // The types of the values.
	outLoc       uint32          // Where we're going to put the output.
	env          *Environment    // Associates variable names with memory locations
	tupleTime    bool            // Once we've taken a tuple path, we can discard values 'til we reach bling or run out.
	tok          *token.Token    // For generating errors.
	access       cpAccess        // Whether the function call is coming from the REPL, the cmd section, etc.
	cst          bool            // Whether the arguments are constant.
	libcall      bool            // Are we in a namespace?
}

func (cp *Compiler) generateNewArgument(b *bindle) AlternateType {
	// Case (1) : we've used up all our arguments. In this case we should look in the function tree for a function call.
	if b.argNo >= len(b.types) {
		return cp.seekFunctionCall(b)
	}
	// Case (2) : the argument is bling.
	if len(b.types[b.argNo].(AlternateType)) == 1 {
		switch bl := (b.types[b.argNo].(AlternateType)[0]).(type) {
		case blingType:
			return cp.seekBling(b, bl.tag)
		}
	}
	// Case (3) : we're in tuple time.
	if b.tupleTime {
		newBindle := *b
		newBindle.argNo++
		return cp.generateNewArgument(&newBindle)
	}
	// Case (4) : we have a reference.
	if b.treePosition.Branch[b.branchNo].TypeName == "ref" {
		newBindle := *b
		newBindle.treePosition = b.treePosition.Branch[b.branchNo].Node
		newBindle.argNo++
		return cp.generateNewArgument(&newBindle)
	}
	// Case (5) : We aren't yet at the end of the list of arguments.
	newBindle := *b
	newBindle.index = 0
	return cp.generateFromTopBranchDown(b)
}

func (cp *Compiler) generateFromTopBranchDown(b *bindle) AlternateType {
	newBindle := *b
	newBindle.branchNo = 0
	newBindle.targetList = typesAtIndex(b.types[b.argNo], b.index)
	newBindle.doneList = make(AlternateType, 0, len(b.targetList))
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
func (cp *Compiler) generateBranch(b *bindle) AlternateType {
	if b.tupleTime || b.branchNo < len(b.treePosition.Branch) && b.treePosition.Branch[b.branchNo].TypeName == "tuple" { // We can move on to the next argument.
		newBindle := *b
		newBindle.treePosition = b.treePosition.Branch[b.branchNo].Node
		newBindle.tupleTime = true
		newBindle.argNo++
		return cp.generateNewArgument(&newBindle)
	}
	if b.branchNo >= len(b.treePosition.Branch) { // We've tried all the alternatives and have some left over.
		cp.reserveError("vm/types/a", b.tok)
		for _, loc := range b.valLocs {
			cp.vm.Mem[cp.That()].V.(*report.Error).Args = append(cp.vm.Mem[cp.That()].V.(*report.Error).Args, loc)
		}
		cp.Emit(UntE, cp.That())
		cp.Emit(Asgm, b.outLoc, cp.That())
		return AltType(values.ERROR)
	}
	branch := b.treePosition.Branch[b.branchNo]
	var acceptedTypes AlternateType
	typeName := branch.TypeName
	isVarchar := len(typeName) >= 8 && typeName[0:8] == "varchar("
	if isVarchar {
		if typeName[len(typeName)-1] == '?' {
			acceptedTypes = cp.TypeNameToTypeList["string?"]
		} else {
			acceptedTypes = cp.TypeNameToTypeList["string"]
		}
	} else {
		acceptedTypes = cp.TypeNameToTypeList[branch.TypeName]
	}
	overlap := acceptedTypes.intersect(b.targetList)
	if len(overlap) == 0 { // We drew a blank.
		return cp.generateNextBranchDown(b)
	}
	// If we've got this far, the current branch accepts at least some of our types. Now we need to do conditionals based on
	// whether this is some or all. But to generate the conditional we also need to know whether we might be looking at a mix of
	// single values and of 0th elements of tuples.
	newBindle := *b
	newBindle.doneList = newBindle.doneList.Union(overlap)
	acceptedSingleTypes := make(AlternateType, 0, len(overlap))
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

	// We may have found a match because any string is a match for a varchar at this point. In that case we do need to do a type check on the length and
	// conditionally continue to the next branch. We can kludge this by taking STRING out of the doneList of the bindle.
	if isVarchar {
		newBindle.doneList = newBindle.doneList.without(simpleType(values.STRING))
	}

	needsOtherBranch := len(newBindle.doneList) != len(newBindle.targetList)
	branchBacktrack := cp.CodeTop()
	if needsOtherBranch {
		// Then we need to generate a conditional. Which one exactly depends on whether we're looking at a single, a tuple, or both.
		switch len(acceptedSingleTypes) {
		case 0:
			cp.put(IxTn, b.valLocs[b.argNo], uint32(b.index))
			cp.emitTypeComparison(branch.TypeName, cp.That(), DUMMY)
		case len(overlap):
			cp.emitTypeComparison(branch.TypeName, b.valLocs[b.argNo], DUMMY)
		default:
			cp.Emit(Qsnq, b.valLocs[b.argNo], cp.CodeTop()+3)
			cp.emitTypeComparison(branch.TypeName, b.valLocs[b.argNo], DUMMY)
			cp.Emit(Jmp, cp.CodeTop()+3)
			cp.put(IxTn, b.valLocs[b.argNo], uint32(b.index))
			cp.emitTypeComparison(branch.TypeName, cp.That(), DUMMY)
		}
	}
	// Now we're in the 'if' part of the condition we just generated, if we did. So either we definitely had
	// a type match, or we're inside a conditional that has checked for one.

	// Now we can recurse along the branch.
	// If we know whether we're looking at a single or a tuple, we can erase this and act accordingly, otherwise we generate a conditional.
	var typesFromGoingAcross, typesFromGoingDown AlternateType
	switch len(acceptedSingleTypes) {
	case 0:
		typesFromGoingAcross = cp.generateMoveAlongBranchViaTupleElement(&newBindle)
	case len(overlap):
		typesFromGoingAcross = cp.generateMoveAlongBranchViaSingleValue(&newBindle)
	default:
		cp.vmIf(Qsnq, b.valLocs[b.argNo])
		typesFromSingles := cp.generateMoveAlongBranchViaSingleValue(&newBindle)
		skipElse := cp.vmGoTo()
		cp.vmEndIf()
		typesFromTuples := cp.generateMoveAlongBranchViaTupleElement(&newBindle)
		cp.vmComeFrom(skipElse)
		typesFromGoingAcross = typesFromSingles.Union(typesFromTuples)
	}
	// And now we need to do the 'else' branch if there is one.
	if needsOtherBranch {
		skipElse := cp.vmGoTo()
		// We need to backtrack on whatever conditional we generated.
		switch len(acceptedSingleTypes) {
		case 0:
			cp.vm.Code[branchBacktrack+1].MakeLastArg(cp.CodeTop())
		case len(overlap):
			cp.vm.Code[branchBacktrack].MakeLastArg(cp.CodeTop())
		default:
			cp.vm.Code[branchBacktrack+1].MakeLastArg(cp.CodeTop())
			cp.vm.Code[branchBacktrack+4].MakeLastArg(cp.CodeTop())
		}
		// We recurse on the next branch down.
		typesFromGoingDown = cp.generateNextBranchDown(&newBindle)
		cp.vmComeFrom(skipElse)
	}
	return typesFromGoingAcross.Union(typesFromGoingDown)
}

var TYPE_COMPARISONS = map[string]Opcode{
	"snippet":  Qspt,
	"snippet?": Qspq,
	"single":   Qsng,
	"single?":  Qsnq,
	"struct":   Qstr,
	"struct?":  Qstq,
}

func (cp *Compiler) emitTypeComparison(typeAsString string, mem, loc uint32) {
	// We may have a 'varchar'.
	if len(typeAsString) >= 8 && typeAsString[0:8] == "varchar(" {
		if typeAsString[len(typeAsString)-1] == '?' {
			vChar, _ := strconv.Atoi(typeAsString[8 : len(typeAsString)-2])
			cp.Emit(Qvcq, mem, uint32(vChar), loc)
			return
		} else {
			vChar, _ := strconv.Atoi(typeAsString[8 : len(typeAsString)-1])
			cp.Emit(Qvch, mem, uint32(vChar), loc)
			return
		}
	}
	// It may be a plain old concrete type.
	ty := cp.TypeNameToTypeList[typeAsString]
	if len(ty) == 1 {
		cp.Emit(Qtyp, mem, uint32(ty[0].(simpleType)), loc)
		return
	}
	// It may be one of the built-in abstract types, 'struct', 'snippet', etc.
	op, ok := TYPE_COMPARISONS[typeAsString]
	if ok {
		cp.Emit(op, mem, loc)
		return
	}
	// It may be a user-defined abstract type.
	var abType values.AbstractType
	for _, aT := range cp.vm.AbstractTypes { // TODO --- the lookup here and in the VM could be much faster, this by a map, that by a slice of booleans.
		if aT.Name == typeAsString {
			abType = aT.AT
			break
		}
	}
	if abType.Types != nil {
		args := []uint32{DUMMY, abType.Varchar}
		for _, t := range abType.Types {
			args = append(args, uint32(t))
		}
		args = append(args, DUMMY)
		cp.Emit(Qabt, args...)
	}
	panic("Unknown type: " + typeAsString)
}

func (cp *Compiler) generateMoveAlongBranchViaTupleElement(b *bindle) AlternateType {
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
	var typesFromNextArgument AlternateType
	needsConditional := b.maxLength == -1 || // Then there's a non-finite tuple
		b.lengths.Contains(newBindle.index) // Then we may have run off the end of a finite tuple.
	var skipElse bkGoto
	if needsConditional {
		cp.vmIf(QlnT, b.valLocs[newBindle.argNo], uint32(newBindle.index))
		newArgumentBindle := newBindle
		newArgumentBindle.argNo++
		typesFromNextArgument = cp.generateNewArgument(&newArgumentBindle)
		skipElse = cp.vmGoTo()
		cp.vmEndIf()
	}

	typesFromContinuingInTuple := cp.generateFromTopBranchDown(&newBindle)

	if needsConditional {
		cp.vmComeFrom(skipElse)
	}

	return typesFromContinuingInTuple.Union(typesFromNextArgument)
}

func (cp *Compiler) generateMoveAlongBranchViaSingleValue(b *bindle) AlternateType {
	newBindle := *b
	newBindle.treePosition = b.treePosition.Branch[b.branchNo].Node
	newBindle.argNo++
	newBindle.branchNo = 0
	return cp.generateNewArgument(&newBindle)
}

func (cp *Compiler) generateNextBranchDown(b *bindle) AlternateType {
	newBindle := *b
	newBindle.branchNo++
	return cp.generateBranch(&newBindle)
}

func (cp *Compiler) seekFunctionCall(b *bindle) AlternateType {
	for _, branch := range b.treePosition.Branch {
		if branch.Node.Fn != nil {
			fNo := branch.Node.Fn.Number
			F := cp.Fns[fNo]
			// Before we do anything else, let's control for access. The REPL shouldn't be able to access private
			// commands or functions, and functions shouldn't be able to access commands.
			if (b.access == REPL || b.libcall) && F.Private {
				cp.P.Throw("comp/private", b.tok)
				return AltType(values.COMPILE_TIME_ERROR)
			}
			if b.access == DEF && F.Command {
				cp.P.Throw("comp/command", b.tok)
				return AltType(values.COMPILE_TIME_ERROR)
			}
			// Deal with the case where the function is a builtin.
			builtinTag := F.Builtin
			functionAndType, ok := BUILTINS[builtinTag]
			if ok {
				switch builtinTag { // Then for these we need to special-case their return types.
				case "tuplify_list", "get_from_sql":
					functionAndType.T = cp.AnyTypeScheme
				case "tuple_of_single?":
					functionAndType.T = AlternateType{finiteTupleType{b.types[0]}}
				case "tuple_of_tuple":
					functionAndType.T = b.doneList
				case "type_with":
					functionAndType.T = AlternateType{cp.TypeNameToTypeList["struct"]}.Union(AltType(values.ERROR))
				case "struct_with":
					functionAndType.T = AlternateType{cp.TypeNameToTypeList["struct"]}.Union(AltType(values.ERROR))
				}
				functionAndType.f(cp, b.tok, b.outLoc, b.valLocs)
				return functionAndType.T
			}
			// It might be a short-form constructor.
			structNumber, ok := cp.StructNameToTypeNumber[builtinTag]
			if ok {
				args := append([]uint32{b.outLoc, uint32(structNumber)}, b.valLocs...)
				cp.Emit(Strc, args...)
				return AltType(structNumber)
			}
			// It could have a Golang body.
			if F.HasGo {
				args := append([]uint32{b.outLoc, F.GoNumber}, b.valLocs...)
				cp.Emit(Gofn, args...)
				if len(branch.Node.Fn.Rets) == 0 {
					if F.Command {
						return AltType(values.SUCCESSFUL_VALUE, values.ERROR)
					} else {
						return cp.AnyTypeScheme
					}
				}
				if len(branch.Node.Fn.Rets) == 1 {
					return cp.TypeNameToTypeList[branch.Node.Fn.Rets[0].VarType]
				}
				// Otherwise it's a tuple.
				tt := make(AlternateType, 0, len(branch.Node.Fn.Rets))
				for _, v := range branch.Node.Fn.Rets {
					tt = append(tt, cp.TypeNameToTypeList[v.VarType])
				}
				return AlternateType{finiteTupleType{tt}}
			}
			// It could be a call to an external service.
			if F.Xcall != nil {
				var remainingNamespace string
				vmArgs := make([]uint32, 0, len(b.valLocs)+5)
				vmArgs = append(vmArgs, b.outLoc, F.Xcall.ExternalServiceOrdinal, F.Xcall.Position)
				cp.Reserve(values.STRING, remainingNamespace)
				vmArgs = append(vmArgs, cp.That())
				cp.Reserve(values.STRING, F.Xcall.FunctionName)
				vmArgs = append(vmArgs, cp.That())
				vmArgs = append(vmArgs, b.valLocs...)
				cp.Emit(Extn, vmArgs...)
				return F.Types
			}
			// Otherwise it's a regular old function call, which we do like this:
			cp.emitFunctionCall(fNo, b.valLocs)
			cp.Emit(Asgm, b.outLoc, F.OutReg) // Because the different implementations of the function will have their own out register.
			return F.Types                    // TODO : Is there a reason why this should be so?
		}
	}
	cp.reserveError("vm/types/b", b.tok)
	for _, loc := range b.valLocs {
		cp.vm.Mem[cp.That()].V.(*report.Error).Args = append(cp.vm.Mem[cp.That()].V.(*report.Error).Args, loc)
	}
	cp.Emit(UntE, cp.That())
	cp.Emit(Asgm, b.outLoc, cp.That())
	return AltType(values.ERROR)
}

func (cp *Compiler) seekBling(b *bindle, bling string) AlternateType {
	for i, branch := range b.treePosition.Branch {
		if branch.TypeName == bling {
			newBindle := *b
			newBindle.branchNo = i
			newBindle.tupleTime = false
			return cp.generateMoveAlongBranchViaSingleValue(&newBindle)
		}
	}
	return AltType(values.ERROR)
}

// We have two different ways of emiting an opcode: 'Emit' does it the regular way, 'put' ensures that
// the destination is the next free memory address.
func (cp *Compiler) Emit(opcode Opcode, args ...uint32) {
	cp.vm.Code = append(cp.vm.Code, MakeOp(opcode, args...))
	if cp.showCompile {
		println(cp.vm.DescribeCode(cp.CodeTop() - 1))
	}
}

func (cp *Compiler) put(opcode Opcode, args ...uint32) {
	args = append([]uint32{cp.MemTop()}, args...)
	cp.Emit(opcode, args...)
	cp.vm.Mem = append(cp.vm.Mem, values.Value{})
}

func (cp *Compiler) emitFunctionCall(funcNumber uint32, valLocs []uint32) {
	args := append([]uint32{cp.Fns[funcNumber].CallTo, cp.Fns[funcNumber].LoReg, cp.Fns[funcNumber].HiReg}, valLocs...)
	if cp.Fns[funcNumber].TupleReg == DUMMY { // We specialize on whether we have to capture tuples.
		cp.Emit(Call, args...)
	} else {
		cp.Emit(CalT, args...)
	}
}

func (cp *Compiler) emitEquals(node *ast.InfixExpression, env *Environment, ac cpAccess) (AlternateType, bool) {
	lTypes, lcst := cp.CompileNode(node.Args[0], env, ac)
	if lTypes.isOnly(values.ERROR) {
		cp.P.Throw("comp/error/eq/a", node.GetToken())
		return AltType(values.ERROR), true
	}
	leftRg := cp.That()
	rTypes, rcst := cp.CompileNode(node.Args[2], env, ac)
	if rTypes.isOnly(values.ERROR) {
		cp.P.Throw("comp/error/eq/b", node.GetToken())
		return AltType(values.ERROR), true
	}
	rightRg := cp.That()
	oL := lTypes.intersect(rTypes)
	if oL.isOnly(values.ERROR) {
		cp.P.Throw("comp/error/eq/c", node.GetToken())
		return AltType(values.ERROR), true
	}
	if len(oL) == 0 {
		cp.P.Throw("comp/eq/types", node.GetToken())
		return AltType(values.ERROR), true
	}
	if len(oL) == 1 && len(lTypes) == 1 && len(rTypes) == 1 {
		switch el := oL[0].(type) { // TODO --- we can do as much of this stuff as actually makes things performant before handing it over to Eqxx
		case simpleType:
			switch el {
			case tp(values.INT):
				cp.put(Equi, leftRg, rightRg)
				return AltType(values.BOOL), lcst && rcst
			case tp(values.STRING):
				cp.put(Equs, leftRg, rightRg)
				return AltType(values.BOOL), lcst && rcst
			case tp(values.BOOL):
				cp.put(Equb, leftRg, rightRg)
				return AltType(values.BOOL), lcst && rcst
			case tp(values.FLOAT):
				cp.put(Equf, leftRg, rightRg)
				return AltType(values.BOOL), lcst && rcst
			case tp(values.TYPE):
				cp.put(Equt, leftRg, rightRg)
				return AltType(values.BOOL), lcst && rcst
			}
		}
	}
	cp.put(Eqxx, leftRg, rightRg, cp.reserveToken(node.GetToken()))
	return AltType(values.ERROR, values.BOOL), lcst && rcst
}

const (
	PREFIX uint32 = iota
	INFIX
	SUFFIX
	UNFIX
)

func (cp *Compiler) compileLog(node *ast.LogExpression, env *Environment, ac cpAccess) bool {
	output := cp.That()
	logStr := node.Value
	if logStr == "" {

	}
	strList, unclosed := text.GetTextWithBarsAsList(logStr)
	if unclosed {
		cp.P.Throw("comp/log/close", &node.Token)
		return false
	}
	// So at this point we have a strList consisting of things which either do or don't need parsing and compiling,
	// depending on whether they are or aren't bracketed by | symbols.
	// If they don't need compiling they can just be concatenated to the output.
	errorReturns := []bkEarlyReturn{}
	for _, str := range strList {
		if str == "" {
			continue
		}
		if str[0] == '|' { // Then we must parse and compile.
			parsedAst := cp.P.ParseLine("code snippet", str[1:len(str)-1])
			sTypes, _ := cp.CompileNode(parsedAst, env, ac)
			thingToAdd := cp.That()
			if sTypes.Contains(values.ERROR) {
				errorReturns = append(errorReturns,
					cp.vmConditionalEarlyReturn(Qtyp, cp.That(), uint32(values.ERROR), cp.That()))
			}
			cp.put(Strx, thingToAdd)
			cp.Emit(Adds, output, output, cp.That())
			continue
		}
		// Otherwise, we just add it on as a string.
		cp.Reserve(values.STRING, str)
		cp.Emit(Adds, output, output, cp.That())
	}
	for _, rtn := range errorReturns {
		cp.vmComeFrom(rtn)
	}
	return len(errorReturns) > 0
}

// The various 'piping operators'.
func (cp *Compiler) compilePipe(lhsTypes AlternateType, lhsConst bool, rhs ast.Node, env *Environment, ac cpAccess) (AlternateType, bool) {
	var envWithThat *Environment
	var isAttemptedFunc bool
	var v *variable
	typeIsNotFunc := bkEarlyReturn(DUMMY)
	var rtnTypes AlternateType
	var rtnConst bool
	lhs := cp.That()
	// If we have a single identifier, we wish it to contain a function ...
	switch rhs := rhs.(type) {
	case *ast.Identifier:
		v, ok := env.getVar(rhs.Value)
		if ok {
			cp.P.Throw("comp/pipe/pipe/ident", rhs.GetToken())
			return AltType(values.ERROR), true
		}
		isAttemptedFunc = true
		if !v.types.Contains(values.FUNC) {
			if rhs.GetToken().Literal == "that" { // Yeah it's a stupid corner case but the stupid user has a right to it.
				isAttemptedFunc = false
			} else {
				cp.P.Throw("comp/pipe/pipe/func", rhs.GetToken())
				return AltType(values.ERROR), true
			}
		}
		if !v.types.isOnly(values.FUNC) {
			cp.reserveError("vm/pipe/pipe/func", rhs.GetToken())
			cp.Emit(Qntp, v.mLoc, uint32(values.FUNC), cp.CodeTop()+3)
			typeIsNotFunc = cp.vmEarlyReturn(cp.That())
		}
	default:
		var whatAccess varAccess
		if lhsConst {
			whatAccess = VERY_LOCAL_CONSTANT
		} else {
			whatAccess = VERY_LOCAL_VARIABLE
		}
		envWithThat = &Environment{data: map[string]variable{"that": {mLoc: cp.That(), access: whatAccess, types: lhsTypes}}, Ext: env}
	}
	if isAttemptedFunc {
		cp.put(Dofn, v.mLoc, lhs)
		rtnTypes, rtnConst = cp.AnyTypeScheme, ALL_CONST_ACCESS.Contains(v.access)
	} else {
		rtnTypes, rtnConst = cp.CompileNode(rhs, envWithThat, ac)
	}
	cp.vmComeFrom(typeIsNotFunc)
	return rtnTypes, rtnConst
}

func (cp *Compiler) compileMappingOrFilter(lhsTypes AlternateType, lhsConst bool, rhs ast.Node, env *Environment, isFilter bool, ac cpAccess) (AlternateType, bool) {
	var isConst bool
	var isAttemptedFunc bool
	var v *variable
	inputElement := uint32(DUMMY)
	typeIsNotFunc := bkEarlyReturn(DUMMY)
	resultIsError := bkEarlyReturn(DUMMY)
	resultIsNotBool := bkEarlyReturn(DUMMY)
	var types AlternateType
	sourceList := cp.That()
	envWithThat := &Environment{}
	thatLoc := uint32(DUMMY)
	// If we have a single identifier, we wish it to contain a function ...
	switch rhs := rhs.(type) {
	case *ast.Identifier:
		if rhs.GetToken().Literal != "that" {
			v, ok := env.getVar(rhs.Value)
			if !ok {
				cp.P.Throw("comp/pipe/mf/ident", rhs.GetToken())
				return AltType(values.ERROR), true
			}
			isAttemptedFunc = true
			isConst = ALL_CONST_ACCESS.Contains(v.access)
			if !v.types.Contains(values.FUNC) {
				cp.P.Throw("comp/pipe/mf/func", rhs.GetToken())
			}
			if !v.types.isOnly(values.FUNC) {
				cp.reserveError("vm/pipe/mf/func", rhs.GetToken())
				cp.Emit(Qntp, v.mLoc, uint32(values.FUNC), cp.CodeTop()+3)
				typeIsNotFunc = cp.vmEarlyReturn(cp.That())
			}
		}
	}
	if !isAttemptedFunc {
		thatLoc = cp.Reserve(values.UNDEFINED_VALUE, DUMMY)
		envWithThat = &Environment{data: map[string]variable{"that": {mLoc: cp.That(), access: VERY_LOCAL_VARIABLE, types: cp.TypeNameToTypeList["single?"]}}, Ext: env}
	}
	counter := cp.Reserve(values.INT, 0)
	accumulator := cp.Reserve(values.TUPLE, []values.Value{})
	cp.put(LenL, sourceList)
	length := cp.That()

	loopStart := cp.CodeTop()
	cp.put(Gthi, length, counter)
	cp.vmIf(Qtru, cp.That())
	if isAttemptedFunc {
		cp.put(IdxL, sourceList, counter, DUMMY)
		inputElement = cp.That()
		cp.put(Dofn, v.mLoc, cp.That())
		types = AltType(values.ERROR).Union(cp.TypeNameToTypeList["single?"]) // Very much TODO. Normally the function is constant and so we know its return types.
	} else {
		cp.Emit(IdxL, thatLoc, sourceList, counter, DUMMY)
		inputElement = thatLoc
		types, isConst = cp.CompileNode(rhs, envWithThat, ac)
	}
	resultElement := cp.That()
	if types.Contains(values.ERROR) {
		cp.Emit(Qtyp, resultElement, uint32(values.ERROR), cp.CodeTop()+3)
		resultIsError = cp.vmEarlyReturn(cp.That())
	}
	if isFilter {
		if !types.Contains(values.BOOL) {
			cp.P.Throw("comp/pipe/filter/bool", rhs.GetToken())
		}
		if !types.isOnly(values.BOOL) {
			cp.reserveError("vm/pipe/filter/bool", rhs.GetToken())
			cp.Emit(Qntp, resultElement, uint32(values.BOOL), cp.CodeTop()+2)
			resultIsNotBool = cp.vmEarlyReturn(cp.That())
		}
		cp.Emit(Qtru, resultElement, cp.CodeTop()+2)
		cp.Emit(CcT1, accumulator, accumulator, inputElement)
	} else {
		cp.Emit(CcT1, accumulator, accumulator, resultElement)
	}
	cp.Emit(Addi, counter, counter, values.C_ONE)
	cp.Emit(Jmp, loopStart)
	cp.vmEndIf()
	cp.put(List, accumulator)
	cp.vmComeFrom(typeIsNotFunc, resultIsError, resultIsNotBool)

	if types.Contains(values.ERROR) {
		return AltType(values.ERROR, values.LIST), isConst
	}
	return AltType(values.LIST), isConst
}

func (cp *Compiler) compileExternalSnippet(tok *token.Token, newEnv *Environment, sText string, ac cpAccess) *SnippetBindle {
	bindle := SnippetBindle{}
	bits, ok := text.GetTextWithBarsAsList(sText)
	if !ok {
		cp.P.Throw("comp/snippet/form/a", tok)
		return &bindle
	}
	bindle.codeLoc = cp.CodeTop()
	result := cp.Reserve(values.STRING, "")
	bindle.objectStringLoc = cp.That()
	for _, bit := range bits {
		if bit[0] == '|' {
			code := bit[1 : len(bit)-1]
			node := cp.P.ParseLine(tok.Source, code)
			cp.CompileNode(node, newEnv, ac)
			cp.put(Litx, cp.That())
		} else {
			cp.Reserve(values.STRING, bit)
		}
		cp.Emit(Adds, result, result, cp.That())
	}
	cp.Emit(Ret)
	return &bindle
}

func (cp *Compiler) compileInjectableSnippet(tok *token.Token, newEnv *Environment, csk compiledSnippetKind, sText string, ac cpAccess) *SnippetBindle {
	bindle := SnippetBindle{}
	bits, ok := text.GetTextWithBarsAsList(sText)
	if !ok {
		cp.P.Throw("comp/snippet/form/b", tok)
		return &bindle
	}
	var buf strings.Builder
	bindle.codeLoc = cp.CodeTop()
	c := 0
	for _, bit := range bits {
		if len(bit) == 0 {
			continue
		}
		if bit[0] == '|' {
			node := cp.P.ParseLine(tok.Source, bit[1:len(bit)-1])
			types, cst := cp.CompileNode(node, newEnv, ac)
			val := cp.That()
			if types.isOnly(values.TYPE) && cst && csk == SQL_SNIPPET {
				typeNumbers := cp.vm.Mem[cp.That()].V.(values.AbstractType).Types
				if len(typeNumbers) == 1 && typeNumbers[0] > cp.vm.Ub_enums {
					sig, ok := cp.vm.getSqlSig(typeNumbers[0])
					if !ok {
						cp.P.Throw("comp/snippet/sig", tok, cp.vm.DescribeType(typeNumbers[0]))
					}
					buf.WriteString(sig)
					continue // ... the for loop.
				}
			}
			// If it's a tuple of fixed length, we can split it and inject the values separately.
			// If it's of indeterminate length then we need to throw an error.
			numberOfInjectionSites := 1 // Default, if the type is single.
			if types.Contains(values.TUPLE) {
				lengths := lengths(types)
				if lengths.Contains(-1) || len(lengths) > 1 { // ... then we can't infer the length and must throw an error.
					cp.P.Throw("comp/snippet/tuple", tok)
				}
				numberOfInjectionSites, _ = lengths.GetArbitraryElement()
				for i := 0; i < numberOfInjectionSites; i++ {
					cp.put(IxTn, val, uint32(i))
					bindle.valueLocs = append(bindle.valueLocs, cp.That())
				}
			} else { // We have a single element so we add it to the injectable values.
				bindle.valueLocs = append(bindle.valueLocs, val)
			}
			sep := ""
			for i := 0; i < numberOfInjectionSites; i++ {
				buf.WriteString(sep)
				switch csk {
				case SQL_SNIPPET:
					buf.WriteString("$")
					c++
					buf.WriteString(strconv.Itoa(c)) // The injection sites in SQL go $1 , $2 , $3 ...
				case HTML_SNIPPET:

					buf.WriteString("{{index .Data ")
					buf.WriteString(strconv.Itoa(c)) // The injection sites in HTML go {{index .Data 0}} , {{index .Data 1}} ...
					buf.WriteString("}}")
					c++
				}
				sep = ", "
			}
		} else {
			buf.WriteString(bit)
		}
	}
	cp.Reserve(values.STRING, buf.String())
	bindle.objectStringLoc = cp.That()
	cp.Emit(Ret)
	return &bindle
}

func (cp *Compiler) MemTop() uint32 {
	return uint32(len(cp.vm.Mem))
}

func (cp *Compiler) That() uint32 {
	return uint32(len(cp.vm.Mem) - 1)
}

func (cp *Compiler) ThatToken() uint32 {
	return uint32(len(cp.vm.Tokens) - 1)
}

func (cp *Compiler) CodeTop() uint32 {
	return uint32(len(cp.vm.Code))
}

func (cp *Compiler) TokenTop() uint32 {
	return uint32(len(cp.vm.Tokens))
}

func (cp *Compiler) LfTop() uint32 {
	return uint32(len(cp.vm.LambdaFactories))
}

func (cp *Compiler) Next() uint32 {
	return uint32(len(cp.vm.Code))
}

// This captures the record.
func (cp *Compiler) getState() vmState {
	return vmState{len(cp.vm.Mem), len(cp.vm.Code), len(cp.vm.Tokens), len(cp.vm.LambdaFactories), len(cp.vm.SnippetFactories)}
}

// And this rolls back the machine.
func (cp *Compiler) rollback(vms vmState) {
	cp.vm.Code = cp.vm.Code[:vms.code]
	cp.vm.Mem = cp.vm.Mem[:vms.mem]
	cp.vm.Tokens = cp.vm.Tokens[:vms.tokens]
	cp.vm.LambdaFactories = cp.vm.LambdaFactories[:vms.lambdaFactories]
	cp.vm.SnippetFactories = cp.vm.SnippetFactories[:vms.snippetFactories]
}
