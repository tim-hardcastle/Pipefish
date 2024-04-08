package compiler

import (
	"os"
	"pipefish/source/ast"
	"pipefish/source/initializer"
	"pipefish/source/parser"
	"pipefish/source/relexer"
	"pipefish/source/signature"
	"pipefish/source/token"
	"pipefish/source/values"
	"pipefish/source/vm"

	"database/sql"
)

// Just as the initializer directs the tokenizer and the parser in the construction of the parsed code
// chunks from the tokens, so the vmMaker directs the initializer and compiler in the construction of the mc.

type VmMaker struct {
	cp             *Compiler
	uP             *initializer.Initializer
	scriptFilepath string
}

func NewVmMaker(scriptFilepath, sourcecode string, mc *vm.Vm) *VmMaker {
	uP := initializer.NewInitializer(scriptFilepath, sourcecode, mc.Db)
	vmm := &VmMaker{
		cp: NewCompiler(uP.Parser),
		uP: uP,
	}
	vmm.scriptFilepath = scriptFilepath
	vmm.uP.GetSource(scriptFilepath)
	return vmm
}

// The base case: we start off with a blank vm.
func StartService(scriptFilepath, sourcecode string, db *sql.DB) (*VmService, *initializer.Initializer) {
	mc := vm.BlankVm(db)
	cp, uP := initalizeEverything(mc, scriptFilepath, sourcecode) // We pass back the uP bcause it contains the sources.
	return &VmService{Mc: mc, Cp: cp, ScriptFilepath: scriptFilepath}, uP
}

// Then we can recurse over this, passing it the same vm every time.
// This returns a compiler and mutates the vm.
func initalizeEverything(mc *vm.Vm, scriptFilepath, sourcecode string) (*Compiler, *initializer.Initializer) {
	vmm := NewVmMaker(scriptFilepath, sourcecode, mc)
	vmm.Make(mc, scriptFilepath, sourcecode)
	return vmm.cp, vmm.uP
}

func (vmm *VmMaker) Make(mc *vm.Vm, scriptFilepath, sourcecode string) {

	vmm.uP.AddToNameSpace([]string{"rsc/pipefish/builtins.pf", "rsc/pipefish/world.pf"})
	vmm.uP.SetRelexer(*relexer.New(scriptFilepath, sourcecode))
	vmm.uP.MakeParserAndTokenizedProgram()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.uP.ParseImports() // That is, parse the import declarations. The files being imported are imported by the method with the long name below.
	if vmm.uP.ErrorsExist() {
		return
	}
	unnamespacedImports := vmm.uP.InitializeNamespacedImportsAndReturnUnnamespacedImports(vmm.cp.p.RootService, vmm.cp.p.NamespacePath)

	if vmm.uP.ErrorsExist() {
		return
	}
	vmm.uP.AddToNameSpace(unnamespacedImports)

	vmm.compileImports(mc)
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.createEnums(mc)
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.uP.MakeLanguagesAndContacts()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.uP.ParseTypeDefs()
	if vmm.uP.ErrorsExist() {
		return
	}

	// We make the struct names and labels, but not the constructors, which come later.
	vmm.createStructs(mc)
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.createLanguagesAndContacts(mc)
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.addAbstractTypesToVm(mc)

	vmm.uP.ParseEverything()
	if vmm.uP.ErrorsExist() {
		return
	}

	// An intermediate step that groups the functions by name and orders them by specificity in a "function table"
	vmm.uP.MakeFunctions(vmm.scriptFilepath)
	if vmm.uP.ErrorsExist() {
		return
	}

	// Now we turn this into a different data structure, a "function tree" with its branches labeled
	// with types. Following it tells us which version of an overloaded function to use.
	vmm.uP.MakeFunctionTrees()
	if vmm.uP.ErrorsExist() {
		return
	}

	// We add in constructors for the structs, languages, and contacts.
	vmm.makeConstructors(mc)

	// And we compile the functions in what is mainly a couple of loops wrapping around the aptly-named
	// compileFunction method.
	vmm.compileFunctions(mc, functionDeclaration, privateFunctionDeclaration)
	if vmm.uP.ErrorsExist() {
		return
	}
	// NOTE: There's some unDRYness here --- e.g. we use .ExtractPartsOfFunction twice --- but that can
	// be disposed of when we strip out the evaluator.

	// Finally we can evaluate the constants and variables, which needs the functions to be compiled
	// first because the RHS of the assignment can be any expression.
	// NOTE: is this even going to work any more? You also need to use the types of the variables/consts.
	// So it all needs to be thrown into a dependency digraph and sorted.
	vmm.evaluateConstantsAndVariables(mc)
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.compileFunctions(mc, commandDeclaration, privateCommandDeclaration)
	if vmm.uP.ErrorsExist() {
		return
	}
}

func (vmm *VmMaker) compileFunctions(mc *vm.Vm, args ...declarationType) {
	for _, j := range args {
		for i := 0; i < len(vmm.cp.p.ParsedDeclarations[j]); i++ {
			vmm.compileFunction(mc, vmm.cp.p.ParsedDeclarations[j][i], vmm.cp.gconsts, j)
		}
	}
}

func (vmm *VmMaker) compileImports(mc *vm.Vm) {
	for namespace, lib := range vmm.cp.p.NamespaceBranch {
		sourcecode, _ := os.ReadFile(lib.ScriptFilepath)
		newCp, _ := initalizeEverything(mc, lib.ScriptFilepath, string(sourcecode))
		vmm.cp.Services[namespace] = &VmService{Cp: newCp, Mc: mc, ScriptFilepath: lib.ScriptFilepath}
	}
}

// TODO This duplicates the type in the initializer and is therefore terrible. Eventually they will be the same file.
type declarationType int

const (
	importDeclaration          declarationType = iota
	enumDeclaration                            //
	typeDeclaration                            //
	languageDeclaration                        //
	contactDeclaration                         // The fact that these things come
	constantDeclaration                        // in this order is used in the code
	variableDeclaration                        // and should not be changed without
	functionDeclaration                        // a great deal of forethought.
	privateFunctionDeclaration                 //
	commandDeclaration                         //
	privateCommandDeclaration                  //
	golangDeclaration                          // Pure golang in a block; the Charm functions with golang bodies don't go here.

)

// On the one hand, the VM must know the names of the enums and their elements so it can describe them.
// Otoh, the compiler needs to know how to turn enum literals into values.
func (vmm *VmMaker) createEnums(mc *vm.Vm) {
	for chunk, tokens := range vmm.uP.Parser.TokenizedDeclarations[enumDeclaration] {
		tokens.ToStart()
		tok1 := tokens.NextToken()
		tok2 := tokens.NextToken()
		if !(tok1.Type == token.IDENT && tok2.Type == token.DEF_ASSIGN) {
			vmm.uP.Throw("init/enum/lhs", tok1)
		}

		if parser.TypeExists(tok1.Literal, vmm.uP.Parser.TypeSystem) {
			vmm.uP.Throw("init/enum/type", tok1)
		}

		mc.TypeNames = append(mc.TypeNames, tok1.Literal)
		vmm.uP.Parser.Suffixes.Add(tok1.Literal)
		vmm.uP.Parser.TypeSystem.AddTransitiveArrow(tok1.Literal, "enum")
		typeNo := values.LB_ENUMS + values.ValueType(chunk)
		vmm.cp.typeNameToTypeList["single"] = vmm.cp.typeNameToTypeList["single"].union(altType(typeNo))
		vmm.cp.typeNameToTypeList["single?"] = vmm.cp.typeNameToTypeList["single?"].union(altType(typeNo))
		vmm.cp.typeNameToTypeList[tok1.Literal] = altType(typeNo)
		vmm.cp.typeNameToTypeList[tok1.Literal+"?"] = altType(values.NULL, typeNo)
		mc.Enums = append(mc.Enums, []string{})
		mc.Ub_enums++

		tokens.NextToken() // This says "enum" or we wouldn't be here.
		for tok := tokens.NextToken(); tok.Type != token.EOF; {
			if tok.Type != token.IDENT {
				vmm.uP.Throw("init/enum/ident", tok)
			}
			_, alreadyExists := vmm.cp.enumElements[tok.Literal]
			if alreadyExists { // Enums in the same namespace can't have overlapping elements or we wouldn't know their type.
				vmm.uP.Throw("init/enum/element", tok)
			}

			vmm.cp.enumElements[tok.Literal] = vmm.cp.reserve(mc, values.ValueType(chunk)+values.LB_ENUMS, len(mc.Enums[chunk]))
			mc.Enums[chunk] = append(mc.Enums[chunk], tok.Literal)

			tok = tokens.NextToken()
			if tok.Type != token.COMMA && tok.Type != token.WEAK_COMMA && tok.Type != token.EOF {
				vmm.uP.Throw("init/enum/comma", tok)
			}
			tok = tokens.NextToken()
		}
	}
}

func (vmm *VmMaker) createStructs(mc *vm.Vm) {
	for _, node := range vmm.uP.Parser.ParsedDeclarations[typeDeclaration] {
		lhs := node.(*ast.AssignmentExpression).Left
		if lhs.GetToken().Type != token.IDENT {
			vmm.uP.Throw("init/enum/lhs", *lhs.GetToken())
		}
		name := lhs.GetToken().Literal

		_, alreadyExists := vmm.cp.StructNumbers[name]
		if alreadyExists {
			vmm.uP.Throw("init/struct/type", *lhs.GetToken())
		}

		// We make the type itself exist.

		typeNo := values.ValueType(len(mc.TypeNames))
		mc.TypeNames = append(mc.TypeNames, name)
		vmm.cp.typeNameToTypeList["single"] = vmm.cp.typeNameToTypeList["single"].union(altType(typeNo))
		vmm.cp.typeNameToTypeList["single?"] = vmm.cp.typeNameToTypeList["single?"].union(altType(typeNo))
		vmm.cp.typeNameToTypeList["struct"] = vmm.cp.typeNameToTypeList["struct"].union(altType(typeNo))
		vmm.cp.typeNameToTypeList["struct?"] = vmm.cp.typeNameToTypeList["struct?"].union(altType(typeNo))
		vmm.cp.typeNameToTypeList[name] = altType(typeNo)
		vmm.cp.typeNameToTypeList[name+"?"] = altType(values.NULL, typeNo)
		vmm.cp.StructNumbers[name] = typeNo

		// The parser needs to know about it too.

		vmm.uP.Parser.Functions.Add(name)
		sig := node.(*ast.AssignmentExpression).Right.(*ast.StructExpression).Sig
		vmm.cp.p.FunctionTable.Add(vmm.cp.p.TypeSystem, name, ast.Function{Sig: sig, Body: &ast.BuiltInExpression{Name: name}}) // TODO --- give them their own ast type?

		// We make the labels exist.

		labelsForStruct := make([]int, 0, len(sig))
		for _, labelNameAndType := range sig {
			labelName := labelNameAndType.VarName
			labelLocation, alreadyExists := vmm.cp.fieldLabels[labelName]
			if alreadyExists { // Structs can of course have overlapping fields but we don't want to declare them twice..
				labelsForStruct = append(labelsForStruct, mc.Mem[labelLocation].V.(int))
			} else {
				vmm.cp.fieldLabels[labelName] = vmm.cp.reserve(mc, values.LABEL, len(mc.Labels))
				labelsForStruct = append(labelsForStruct, len(mc.Labels))
				mc.Labels = append(mc.Labels, labelName)
			}
		}
		mc.StructLabels = append(mc.StructLabels, labelsForStruct)
		mc.StructResolve = mc.StructResolve.Add(int(typeNo-mc.Ub_enums), labelsForStruct)
	}
}

func (vmm *VmMaker) createLanguagesAndContacts(mc *vm.Vm) {
	for _, name := range vmm.cp.p.Languages {
		vmm.addLanguageOrContact(mc, name)
	}
	for name := range vmm.cp.p.Contacts {
		vmm.addLanguageOrContact(mc, name)
	}
}

func (vmm *VmMaker) addLanguageOrContact(mc *vm.Vm, name string) {

	sig := signature.Signature{signature.NameTypePair{VarName: "text", VarType: "string"}, signature.NameTypePair{VarName: "env", VarType: "map"}}

	typeNo := values.ValueType(len(mc.TypeNames))
	mc.TypeNames = append(mc.TypeNames, name)
	vmm.cp.typeNameToTypeList["single"] = vmm.cp.typeNameToTypeList["single"].union(altType(typeNo))
	vmm.cp.typeNameToTypeList["single?"] = vmm.cp.typeNameToTypeList["single?"].union(altType(typeNo))
	vmm.cp.typeNameToTypeList["struct"] = vmm.cp.typeNameToTypeList["struct"].union(altType(typeNo))
	vmm.cp.typeNameToTypeList["struct?"] = vmm.cp.typeNameToTypeList["struct?"].union(altType(typeNo))
	vmm.cp.typeNameToTypeList["snippet"] = vmm.cp.typeNameToTypeList["snippet"].union(altType(typeNo))
	vmm.cp.typeNameToTypeList["snippet?"] = vmm.cp.typeNameToTypeList["snippet?"].union(altType(typeNo))
	vmm.cp.typeNameToTypeList[name] = altType(typeNo)
	vmm.cp.typeNameToTypeList[name+"?"] = altType(values.NULL, typeNo)
	vmm.cp.StructNumbers[name] = typeNo

	// The parser needs to know about it too.

	vmm.uP.Parser.Functions.Add(name)
	vmm.cp.p.FunctionTable.Add(vmm.cp.p.TypeSystem, name, ast.Function{Sig: sig, Body: &ast.BuiltInExpression{Name: name}})

	// And the vm.
	vmm.addStructLabelsToMc(mc, name, typeNo, sig)
}

func (vmm *VmMaker) addStructLabelsToMc(mc *vm.Vm, name string, typeNo values.ValueType, sig signature.Signature) {
	labelsForStruct := make([]int, 0, len(sig))
	for _, labelNameAndType := range sig {
		labelName := labelNameAndType.VarName
		labelLocation, alreadyExists := vmm.cp.fieldLabels[labelName]
		if alreadyExists { // Structs can of course have overlapping fields but we don't want to declare them twice..
			labelsForStruct = append(labelsForStruct, mc.Mem[labelLocation].V.(int))
		} else {
			vmm.cp.fieldLabels[labelName] = vmm.cp.reserve(mc, values.LABEL, len(mc.Labels))
			labelsForStruct = append(labelsForStruct, len(mc.Labels))
			mc.Labels = append(mc.Labels, labelName)
		}
	}
	mc.StructLabels = append(mc.StructLabels, labelsForStruct)
	mc.StructResolve = mc.StructResolve.Add(int(typeNo-mc.Ub_enums), labelsForStruct)
}

func (vmm *VmMaker) makeConstructors(mc *vm.Vm) {
	for _, node := range vmm.uP.Parser.ParsedDeclarations[typeDeclaration] {
		name := node.(*ast.AssignmentExpression).Left.GetToken().Literal // We know this and the next line are safe because we already checked in createStructs
		sig := node.(*ast.AssignmentExpression).Right.(*ast.StructExpression).Sig
		vmm.cp.fns = append(vmm.cp.fns, vmm.compileConstructor(mc, name, sig))
	}
	sig := signature.Signature{signature.NameTypePair{VarName: "text", VarType: "string"}, signature.NameTypePair{VarName: "env", VarType: "map"}}
	for _, name := range vmm.cp.p.Languages {
		vmm.cp.fns = append(vmm.cp.fns, vmm.compileConstructor(mc, name, sig))
	}
	for name := range vmm.cp.p.Contacts {
		vmm.cp.fns = append(vmm.cp.fns, vmm.compileConstructor(mc, name, sig))
	}
}

func (vmm *VmMaker) compileConstructor(mc *vm.Vm, name string, sig signature.Signature) *cpFunc {
	typeNo := vmm.cp.StructNumbers[name]
	cpF := &cpFunc{types: altType(typeNo), builtin: name}
	fnenv := newEnvironment() // Note that we don't use this for anything, we just need some environment to pass to addVariables.
	cpF.loReg = mc.MemTop()
	for _, pair := range sig {
		vmm.cp.addVariable(mc, fnenv, pair.VarName, FUNCTION_ARGUMENT, vmm.cp.typeNameToTypeList[pair.VarType])
	}
	cpF.hiReg = mc.MemTop()
	return cpF
}

// The Vm doesn't *use* abstract types, but they are what values of type TYPE contain, and so it needs to be able to describe them.
func (vmm *VmMaker) addAbstractTypesToVm(mc *vm.Vm) {
	nativeAbstractTypes := []string{"single", "struct", "snippet"}
	for _, t := range nativeAbstractTypes {
		mc.AbstractTypes = append(mc.AbstractTypes, values.NameAbstractTypePair{t, vmm.cp.typeNameToTypeList[t].ToAbstractType()})
	}
}

func (vmm *VmMaker) compileFunction(mc *vm.Vm, node ast.Node, outerEnv *environment, dec declarationType) *cpFunc {
	cpF := cpFunc{}
	var ac Access
	if dec == functionDeclaration || dec == privateFunctionDeclaration {
		ac = DEF
	} else {
		ac = CMD
		cpF.command = true
	}
	if dec == privateFunctionDeclaration || dec == privateCommandDeclaration {
		cpF.private = true
	}
	functionName, sig, _, body, given, tupleList := vmm.uP.Parser.ExtractPartsOfFunction(node)
	if body.GetToken().Type == token.PRELOG && body.GetToken().Literal == "" {
		body.(*ast.LogExpression).Value = parser.DescribeFunctionCall(functionName, &sig)
	}
	if vmm.uP.Parser.ErrorsExist() {
		return nil
	}
	if body.GetToken().Type == token.BUILTIN {
		cpF.builtin = body.(*ast.BuiltInExpression).Name
	}
	fnenv := newEnvironment()
	fnenv.ext = outerEnv
	cpF.loReg = mc.MemTop()
	for _, pair := range sig {
		vmm.cp.reserve(mc, values.UNDEFINED_VALUE, DUMMY)
		if pair.VarType == "ref" {
			vmm.cp.addVariable(mc, fnenv, pair.VarName, REFERENCE_VARIABLE, vmm.cp.typeNameToTypeList[pair.VarType])
			continue
		}
		vmm.cp.addVariable(mc, fnenv, pair.VarName, FUNCTION_ARGUMENT, vmm.cp.typeNameToTypeList[pair.VarType])
	}
	cpF.hiReg = mc.MemTop()
	cpF.callTo = mc.CodeTop()
	if len(tupleList) > 0 {
		cpF.tupleReg = vmm.cp.reserve(mc, values.INT_ARRAY, tupleList)
	} else {
		cpF.tupleReg = DUMMY
	}
	switch body.GetToken().Type {
	case token.BUILTIN:
		name := body.(*ast.BuiltInExpression).Name
		types, ok := BUILTINS[name]
		if ok {
			cpF.types = types.t
		} else {
			structNo, ok := vmm.cp.StructNumbers[name]
			if ok {
				cpF.types = altType(structNo)
			}
		}
	case token.GOLANG:
		cpF.goNumber = uint32(len(mc.GoFns))
		cpF.hasGo = true
		mc.GoFns = append(mc.GoFns, vm.GoFn{body.(*ast.GolangExpression).ObjectCode, body.(*ast.GolangExpression).Raw})
	default:
		if given != nil {
			vmm.cp.thunkList = []thunk{}
			vmm.cp.compileNode(mc, given, fnenv, ac)
			cpF.callTo = mc.CodeTop()
			for _, pair := range vmm.cp.thunkList {
				vmm.cp.emit(mc, vm.Thnk, pair.mLoc, pair.cLoc)
			}
		}
		cpF.types, _ = vmm.cp.compileNode(mc, body, fnenv, ac) // TODO --- could we in fact do anything useful if we knew it was a constant?
		vmm.cp.emit(mc, vm.Ret)
		cpF.outReg = mc.That()
	}
	vmm.cp.fns = append(vmm.cp.fns, &cpF)
	if ac == DEF && !cpF.types.isLegalDefReturn() {
		vmm.cp.p.Throw("comp/return/def/a", node.GetToken())
	}
	if ac == CMD && !cpF.types.isLegalCmdReturn() {
		vmm.cp.p.Throw("comp/return/cmd/a", node.GetToken())
	}
	return &cpF
}

func (vmm *VmMaker) evaluateConstantsAndVariables(mc *vm.Vm) {
	vmm.cp.gvars.ext = vmm.cp.gconsts
	vmm.cp.reserve(mc, values.NULL, nil)
	vmm.cp.addVariable(mc, vmm.cp.gconsts, "NULL", GLOBAL_CONSTANT_PUBLIC, altType(values.NULL))
	vmm.cp.reserve(mc, values.SUCCESSFUL_VALUE, nil)
	vmm.cp.addVariable(mc, vmm.cp.gconsts, "ok", GLOBAL_CONSTANT_PUBLIC, altType(values.SUCCESSFUL_VALUE))
	vmm.cp.reserve(mc, values.BREAK, nil)
	vmm.cp.addVariable(mc, vmm.cp.gconsts, "break", GLOBAL_CONSTANT_PUBLIC, altType(values.BREAK))
	vmm.cp.tupleType = vmm.cp.reserve(mc, values.TYPE, values.AbstractType{values.TUPLE})
	for declarations := int(constantDeclaration); declarations <= int(variableDeclaration); declarations++ {
		assignmentOrder := vmm.uP.ReturnOrderOfAssignments(declarations)
		for _, v := range assignmentOrder {
			dec := vmm.uP.Parser.ParsedDeclarations[declarations][v]
			lhs := dec.(*ast.AssignmentExpression).Left
			rhs := dec.(*ast.AssignmentExpression).Right
			if lhs.GetToken().Type != token.IDENT { // TODO --- use assignment signature once tuples are working.
				vmm.uP.Throw("vmm/assign/ident", *dec.GetToken())
			}
			vname := lhs.(*ast.Identifier).Value
			runFrom := mc.CodeTop()
			inferedType, _ := vmm.cp.compileNode(mc, rhs, vmm.cp.gvars, INIT)
			if vmm.uP.ErrorsExist() {
				return
			}
			vmm.cp.emit(mc, vm.Ret)
			mc.Run(runFrom)
			if declarations == int(constantDeclaration) {
				vmm.cp.addVariable(mc, vmm.cp.gconsts, vname, GLOBAL_CONSTANT_PUBLIC, inferedType)
			} else {
				vmm.cp.addVariable(mc, vmm.cp.gvars, vname, GLOBAL_VARIABLE_PUBLIC, inferedType)
			}
			mc.Code = mc.Code[:runFrom]
		}
	}
}
