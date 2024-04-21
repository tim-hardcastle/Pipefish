package compiler

import (
	"os"
	"pipefish/source/ast"
	"pipefish/source/lexer"
	"pipefish/source/parser"
	"pipefish/source/service"
	"pipefish/source/settings"
	"pipefish/source/text"
	"pipefish/source/token"
	"pipefish/source/values"

	"database/sql"
)

// Just as the initializer directs the tokenizer and the parser in the construction of the parsed code
// chunks from the tokens, so the vmMaker directs the initializer and compiler in the construction of the mc.

type VmMaker struct {
	cp             *service.Compiler
	uP             *Initializer
	scriptFilepath string
	goToPf         map[string]func(any) (uint32, []any, bool)
	pfToGo         map[string]func(uint32, []any) any
}

func NewVmMaker(scriptFilepath, sourcecode string, mc *service.Vm) *VmMaker {
	uP := NewInitializer(scriptFilepath, sourcecode, mc.Db)
	vmm := &VmMaker{
		cp: service.NewCompiler(uP.Parser),
		uP: uP,
	}
	vmm.scriptFilepath = scriptFilepath
	vmm.uP.GetSource(scriptFilepath)
	return vmm
}

// The base case: we start off with a blank vm.
func StartService(scriptFilepath, sourcecode string, db *sql.DB) (*service.VmService, *Initializer) {
	mc := service.BlankVm(db)
	cp, uP := initializeEverything(mc, scriptFilepath) // We pass back the uP bcause it contains the sources and/or errors (in the parser).
	return &service.VmService{Mc: mc, Cp: cp, ScriptFilepath: scriptFilepath}, uP
}

// Then we can recurse over this, passing it the same vm every time.
// This returns a compiler and initializer and mutates the vm.
// We want the initializer back in case there are errors --- it wll contain the cource code and the errors in the store in its parser.
func initializeEverything(mc *service.Vm, scriptFilepath string) (*service.Compiler, *Initializer) {
	sourcecode := ""
	if scriptFilepath != "" { // In which case we're making a blank VM.
		sourcebytes, err := os.ReadFile(scriptFilepath)
		sourcecode = string(sourcebytes) + "\n"
		if err != nil {
			uP := NewInitializer(scriptFilepath, sourcecode, nil)
			uP.Throw("vmm/source", token.Token{Source: "linking"}, scriptFilepath)
			return nil, uP
		}
	}
	vmm := NewVmMaker(scriptFilepath, sourcecode, mc)
	vmm.Make(mc, scriptFilepath, sourcecode)
	return vmm.cp, vmm.uP
}

func (vmm *VmMaker) InitializeNamespacedImportsAndReturnUnnamespacedImports(mc *service.Vm) []string {
	uP := vmm.uP
	unnamespacedImports := []string{}
	for _, imp := range uP.Parser.ParsedDeclarations[importDeclaration] {
		scriptFilepath := ""
		namespace := ""
		switch imp := (imp).(type) {
		case *ast.StringLiteral:
			scriptFilepath = imp.Value
			namespace = text.ExtractFileName(scriptFilepath)
		case *ast.InfixExpression:
			if imp.GetToken().Literal != "::" {
				uP.Throw("init/import/infix", imp.Token)
			}
			lhs := imp.Args[0]
			rhs := imp.Args[2]
			switch rhs := rhs.(type) {
			case *ast.StringLiteral:
				scriptFilepath = rhs.Value
				switch lhs := lhs.(type) {
				case *ast.Identifier:
					if lhs.Value != "NULL" {
						namespace = lhs.Value
					} else {
						namespace = ""
					}
				default:
					uP.Throw("init/import/ident", *lhs.GetToken())
				}
			default:
				uP.Throw("init/import/string", *lhs.GetToken())
			}
		case *ast.GolangExpression:
			uP.Parser.GoImports[imp.Token.Source] = append(uP.Parser.GoImports[imp.Token.Source], imp.Token.Literal)
			continue
		default:
			uP.Throw("init/import/pair", *imp.GetToken())
		}
		if namespace == "" {
			unnamespacedImports = append(unnamespacedImports, scriptFilepath)
		}
		newCp, newUP := initializeEverything(mc, scriptFilepath)
		newUP.GetSource(scriptFilepath)
		for k, v := range newUP.Sources {
			uP.Sources[k] = v
		}
		file, _ := os.Stat(scriptFilepath)
		timestamp := file.ModTime().UnixMilli()
		if newUP.ErrorsExist() {
			uP.Parser.Errors = append(uP.Parser.Errors, newUP.Parser.Errors...)
			vmm.cp.Services[namespace] = &service.VmService{mc, newCp, scriptFilepath, timestamp, true, false}
		} else {
			vmm.cp.Services[namespace] = &service.VmService{mc, newCp, scriptFilepath, timestamp, false, false}
		}
	}
	return unnamespacedImports
}

func (vmm *VmMaker) Make(mc *service.Vm, scriptFilepath, sourcecode string) {
	if !settings.OMIT_BUILTINS {
		vmm.uP.AddToNameSpace(settings.MandatoryImports)
	}
	if settings.USE_TEST {
		vmm.uP.AddToNameSpace([]string{"rsc/pipefish/test.pf"})
	}
	vmm.uP.SetRelexer(*lexer.NewRelexer(scriptFilepath, sourcecode))
	vmm.uP.MakeParserAndTokenizedProgram()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.uP.ParseImports() // That is, parse the import declarations. The files being imported are imported by the method with the long name below.
	if vmm.uP.ErrorsExist() {
		return
	}
	unnamespacedImports := vmm.InitializeNamespacedImportsAndReturnUnnamespacedImports(mc)

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

	vmm.createAbstractTypes(mc)

	vmm.createLanguagesAndContacts(mc)
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.addAbstractTypesToVm(mc)

	vmm.uP.ParseEverything()
	if vmm.uP.ErrorsExist() {
		return
	}

	// An intermediate step that groups the functions by name and orders them by specificity in a "function table".
	// We return a GoHandler for the next step.
	goHandler := vmm.uP.MakeFunctions(vmm.scriptFilepath)

	if settings.FUNCTION_TO_PEEK != "" {
		for _, f := range vmm.uP.Parser.FunctionTable[settings.FUNCTION_TO_PEEK] {
			println(f.Sig.String())
		}
	}

	if vmm.uP.ErrorsExist() {
		return
	}

	// We build the Go files, if any.
	vmm.MakeGoMods(mc, goHandler)

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

func (vmm *VmMaker) MakeGoMods(mc *service.Vm, goHandler *service.GoHandler) {
	uP := vmm.uP
	for source, _ := range goHandler.Modules {
		goHandler.TypeDeclarations[source] = vmm.cp.MakeTypeDeclarationsForGo(mc, goHandler, source)
		if uP.Parser.ErrorsExist() {
			return
		}
	}
	goHandler.BuildGoMods()
	if uP.Parser.ErrorsExist() {
		return
	}
	vmm.goToPf = map[string]func(any) (uint32, []any, bool){}
	vmm.pfToGo = map[string]func(uint32, []any) any{}
	for source, _ := range goHandler.Modules {
		fnSymbol, _ := goHandler.Plugins[source].Lookup("ConvertGoStructHalfwayToPipefish")
		vmm.goToPf[source] = fnSymbol.(func(any) (uint32, []any, bool))
		fnSymbol, _ = goHandler.Plugins[source].Lookup("ConvertPipefishStructToGoStruct")
		vmm.pfToGo[source] = fnSymbol.(func(uint32, []any) any)
	}
	for functionName, fns := range uP.Parser.FunctionTable { // TODO --- why are we doing it like this?
		for _, v := range fns {
			if v.Body.GetToken().Type == token.GOLANG {
				v.Body.(*ast.GolangExpression).ObjectCode = goHandler.GetFn(text.Flatten(functionName), v.Body.GetToken())
			}
		}
	}
	goHandler.CleanUp()
}

func (vmm *VmMaker) compileFunctions(mc *service.Vm, args ...declarationType) {
	for _, j := range args {
		for i := 0; i < len(vmm.cp.P.ParsedDeclarations[j]); i++ {
			vmm.compileFunction(mc, vmm.cp.P.ParsedDeclarations[j][i], vmm.cp.GlobalConsts, j)
		}
	}
}

func (vmm *VmMaker) compileImports(mc *service.Vm) {
	for namespace, lib := range vmm.cp.P.NamespaceBranch {
		newCp, _ := initializeEverything(mc, lib.ScriptFilepath)
		vmm.cp.Services[namespace] = &service.VmService{Cp: newCp, Mc: mc, ScriptFilepath: lib.ScriptFilepath}
	}
}

// On the one hand, the VM must know the names of the enums and their elements so it can describe them.
// Otoh, the compiler needs to know how to turn enum literals into values.
func (vmm *VmMaker) createEnums(mc *service.Vm) {
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
		vmm.cp.TypeNameToTypeList["single"] = vmm.cp.TypeNameToTypeList["single"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList["single?"] = vmm.cp.TypeNameToTypeList["single?"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList[tok1.Literal] = altType(typeNo)
		vmm.cp.TypeNameToTypeList[tok1.Literal+"?"] = altType(values.NULL, typeNo)
		mc.Enums = append(mc.Enums, []string{})
		vmm.cp.AnyTypeScheme = vmm.cp.AnyTypeScheme.Union(altType(typeNo))
		// We are now going to assume that the last element of anyType is a service.TypedTupleType and add the new enum type accordingly.
		lastType := vmm.cp.AnyTypeScheme[len(vmm.cp.AnyTypeScheme)-1].(service.TypedTupleType)
		vmm.cp.AnyTypeScheme[len(vmm.cp.AnyTypeScheme)-1] = service.TypedTupleType{(lastType.T).Union(altType(typeNo))}

		mc.Ub_enums++

		tokens.NextToken() // This says "enum" or we wouldn't be here.
		for tok := tokens.NextToken(); tok.Type != token.EOF; {
			if tok.Type != token.IDENT {
				vmm.uP.Throw("init/enum/ident", tok)
			}
			_, alreadyExists := vmm.cp.EnumElements[tok.Literal]
			if alreadyExists { // Enums in the same namespace can't have overlapping elements or we wouldn't know their type.
				vmm.uP.Throw("init/enum/element", tok)
			}

			vmm.cp.EnumElements[tok.Literal] = vmm.cp.Reserve(mc, values.ValueType(chunk)+values.LB_ENUMS, len(mc.Enums[chunk]))
			mc.Enums[chunk] = append(mc.Enums[chunk], tok.Literal)

			tok = tokens.NextToken()
			if tok.Type != token.COMMA && tok.Type != token.WEAK_COMMA && tok.Type != token.EOF {
				vmm.uP.Throw("init/enum/comma", tok)
			}
			tok = tokens.NextToken()
		}
	}
}

func (vmm *VmMaker) createStructs(mc *service.Vm) {
	for _, node := range vmm.uP.Parser.ParsedDeclarations[structDeclaration] {
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
		vmm.cp.TypeNameToTypeList["single"] = vmm.cp.TypeNameToTypeList["single"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList["single?"] = vmm.cp.TypeNameToTypeList["single?"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList["struct"] = vmm.cp.TypeNameToTypeList["struct"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList["struct?"] = vmm.cp.TypeNameToTypeList["struct?"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList[name] = altType(typeNo)
		vmm.cp.TypeNameToTypeList[name+"?"] = altType(values.NULL, typeNo)
		vmm.cp.StructNumbers[name] = typeNo
		vmm.cp.AnyTypeScheme = vmm.cp.AnyTypeScheme.Union(altType(typeNo))
		// We are now going to assume that the last element of anyType is a service.TypedTupleType and add the new struct type accordingly.
		lastType := vmm.cp.AnyTypeScheme[len(vmm.cp.AnyTypeScheme)-1].(service.TypedTupleType)
		vmm.cp.AnyTypeScheme[len(vmm.cp.AnyTypeScheme)-1] = service.TypedTupleType{(lastType.T).Union(altType(typeNo))}

		// The parser needs to know about it too.
		vmm.uP.Parser.Functions.Add(name)
		sig := node.(*ast.AssignmentExpression).Right.(*ast.StructExpression).Sig
		vmm.cp.P.FunctionTable.Add(vmm.cp.P.TypeSystem, name, ast.Function{Sig: sig, Body: &ast.BuiltInExpression{Name: name}}) // TODO --- give them their own ast type?

		// We make the labels exist.

		labelsForStruct := make([]int, 0, len(sig))
		typesForStruct := make([]service.AlternateType, 0, len(sig))
		typesForStructForVm := make([]values.AbstractType, 0, len(sig))
		for _, labelNameAndType := range sig {
			labelName := labelNameAndType.VarName
			labelLocation, alreadyExists := vmm.cp.FieldLabelsInMem[labelName]
			if alreadyExists { // Structs can of course have overlapping fields but we don't want to declare them twice..
				labelsForStruct = append(labelsForStruct, mc.Mem[labelLocation].V.(int))
			} else {
				vmm.cp.FieldLabelsInMem[labelName] = vmm.cp.Reserve(mc, values.LABEL, len(mc.Labels))
				labelsForStruct = append(labelsForStruct, len(mc.Labels))
				mc.Labels = append(mc.Labels, labelName)
			}
			typesForStruct = append(typesForStruct, vmm.cp.TypeNameToTypeList[labelNameAndType.VarType])
		}
		for _, t := range typesForStruct {
			typesForStructForVm = append(typesForStructForVm, t.ToAbstractType())
		}
		mc.StructFields = append(mc.StructFields, typesForStructForVm)
		vmm.cp.FieldTypes = append(vmm.cp.FieldTypes, typesForStruct)
		mc.StructLabels = append(mc.StructLabels, labelsForStruct)
		mc.StructResolve = mc.StructResolve.Add(int(typeNo-mc.Ub_enums), labelsForStruct)
	}
}

func (vmm *VmMaker) createAbstractTypes(mc *service.Vm) {
	for _, tcc := range vmm.uP.Parser.TokenizedDeclarations[abstractDeclaration] {
		tcc.ToStart()
		nameTok := tcc.NextToken()
		if nameTok.Type != token.IDENT {
			vmm.uP.Throw("init/type/ident", nameTok)
		}
		newTypename := nameTok.Literal
		eqTok := tcc.NextToken()
		if eqTok.Type != token.DEF_ASSIGN {
			vmm.uP.Throw("init/type/assign", eqTok)
		}
		typeNames := []string{}
		for {
			typeTok := tcc.NextToken()
			divTok := tcc.NextToken()
			if typeTok.Type != token.IDENT {
				vmm.uP.Throw("init/type/form/a", typeTok)
				break
			}
			if divTok.Type != token.EOF && !(divTok.Type == token.IDENT && divTok.Literal == "/") {
				vmm.uP.Throw("init/type/form/b", typeTok)
				break
			}
			tname := typeTok.Literal
			if !vmm.cp.P.TypeSystem.SetOfNodes().Contains(tname) {
				vmm.uP.Throw("init/type/known", typeTok)
				break
			}
			vmm.cp.P.TypeSystem.AddTransitiveArrow(tname, newTypename)
			typeNames = append(typeNames, tname)
			vmm.cp.TypeNameToTypeList[newTypename] = vmm.cp.TypeNameToTypeList[newTypename].Union(vmm.cp.TypeNameToTypeList[tname])
			if divTok.Type == token.EOF {
				break
			}
		}
		mc.AbstractTypes = append(mc.AbstractTypes, values.NameAbstractTypePair{newTypename, vmm.cp.TypeNameToTypeList[newTypename].ToAbstractType()})
		vmm.uP.Parser.Suffixes.Add(newTypename)
		if !vmm.cp.TypeNameToTypeList[newTypename].Contains(values.NULL) {
			for _, tname := range typeNames {
				vmm.cp.P.TypeSystem.AddTransitiveArrow(tname, newTypename+"?")
				vmm.cp.TypeNameToTypeList[newTypename+"?"] = vmm.cp.TypeNameToTypeList[newTypename+"?"].Union(vmm.cp.TypeNameToTypeList[tname])
			}
			vmm.cp.TypeNameToTypeList[newTypename+"?"] = vmm.cp.TypeNameToTypeList[newTypename+"?"].Union(altType(values.NULL))
			vmm.uP.Parser.Suffixes.Add(newTypename + "?")
			vmm.cp.P.TypeSystem.AddTransitiveArrow("null", newTypename+"?")
		}
	}
}

func (vmm *VmMaker) createLanguagesAndContacts(mc *service.Vm) {
	mc.Lb_snippets = values.ValueType(len(mc.TypeNames))
	for _, name := range vmm.cp.P.Languages {
		vmm.addLanguageOrContact(mc, name, false)
	}
	mc.Ub_langs = values.ValueType(len(mc.TypeNames))
	for name := range vmm.cp.P.Contacts {
		vmm.addLanguageOrContact(mc, name, true)
	}
}

func (vmm *VmMaker) addLanguageOrContact(mc *service.Vm, name string, isContact bool) {

	sig := ast.Signature{ast.NameTypePair{VarName: "text", VarType: "string"}, ast.NameTypePair{VarName: "env", VarType: "map"}}

	typeNo := values.ValueType(len(mc.TypeNames))
	mc.TypeNames = append(mc.TypeNames, name)
	vmm.cp.TypeNameToTypeList["single"] = vmm.cp.TypeNameToTypeList["single"].Union(altType(typeNo))
	vmm.cp.TypeNameToTypeList["single?"] = vmm.cp.TypeNameToTypeList["single?"].Union(altType(typeNo))
	vmm.cp.TypeNameToTypeList["struct"] = vmm.cp.TypeNameToTypeList["struct"].Union(altType(typeNo))
	vmm.cp.TypeNameToTypeList["struct?"] = vmm.cp.TypeNameToTypeList["struct?"].Union(altType(typeNo))
	vmm.cp.TypeNameToTypeList["snippet"] = vmm.cp.TypeNameToTypeList["snippet"].Union(altType(typeNo))
	vmm.cp.TypeNameToTypeList["snippet?"] = vmm.cp.TypeNameToTypeList["snippet?"].Union(altType(typeNo))
	vmm.cp.TypeNameToTypeList["contact"] = vmm.cp.TypeNameToTypeList["contact"].Union(altType(typeNo))
	vmm.cp.TypeNameToTypeList["contact?"] = vmm.cp.TypeNameToTypeList["contact?"].Union(altType(typeNo))
	vmm.cp.TypeNameToTypeList[name] = altType(typeNo)
	vmm.cp.TypeNameToTypeList[name+"?"] = altType(values.NULL, typeNo)
	vmm.cp.StructNumbers[name] = typeNo

	// The parser needs to know about it too.

	vmm.uP.Parser.Functions.Add(name)
	vmm.cp.P.FunctionTable.Add(vmm.cp.P.TypeSystem, name, ast.Function{Sig: sig, Body: &ast.BuiltInExpression{Name: name}})

	// And the vm.
	vmm.addStructLabelsToMc(mc, name, typeNo, sig)
}

func (vmm *VmMaker) addStructLabelsToMc(mc *service.Vm, name string, typeNo values.ValueType, sig ast.Signature) {
	labelsForStruct := make([]int, 0, len(sig))
	for _, labelNameAndType := range sig {
		labelName := labelNameAndType.VarName
		labelLocation, alreadyExists := vmm.cp.FieldLabelsInMem[labelName]
		if alreadyExists { // Structs can of course have overlapping fields but we don't want to declare them twice..
			labelsForStruct = append(labelsForStruct, mc.Mem[labelLocation].V.(int))
		} else {
			vmm.cp.FieldLabelsInMem[labelName] = vmm.cp.Reserve(mc, values.LABEL, len(mc.Labels))
			labelsForStruct = append(labelsForStruct, len(mc.Labels))
			mc.Labels = append(mc.Labels, labelName)
		}
	}
	mc.StructLabels = append(mc.StructLabels, labelsForStruct)
	mc.StructResolve = mc.StructResolve.Add(int(typeNo-mc.Ub_enums), labelsForStruct)
}

func (vmm *VmMaker) makeConstructors(mc *service.Vm) {
	for _, node := range vmm.uP.Parser.ParsedDeclarations[structDeclaration] {
		name := node.(*ast.AssignmentExpression).Left.GetToken().Literal // We know this and the next line are safe because we already checked in createStructs
		sig := node.(*ast.AssignmentExpression).Right.(*ast.StructExpression).Sig
		vmm.cp.Fns = append(vmm.cp.Fns, vmm.compileConstructor(mc, name, sig))
	}
	sig := ast.Signature{ast.NameTypePair{VarName: "text", VarType: "string"}, ast.NameTypePair{VarName: "env", VarType: "map"}}
	for _, name := range vmm.cp.P.Languages {
		vmm.cp.Fns = append(vmm.cp.Fns, vmm.compileConstructor(mc, name, sig))
	}
	for name := range vmm.cp.P.Contacts {
		vmm.cp.Fns = append(vmm.cp.Fns, vmm.compileConstructor(mc, name, sig))
	}
}

func (vmm *VmMaker) compileConstructor(mc *service.Vm, name string, sig ast.Signature) *service.CpFunc {
	typeNo := vmm.cp.StructNumbers[name]
	cpF := &service.CpFunc{Types: altType(typeNo), Builtin: name}
	fnenv := service.NewEnvironment() // Note that we don't use this for anything, we just need some environment to pass to addVariables.
	cpF.LoReg = mc.MemTop()
	for _, pair := range sig {
		vmm.cp.AddVariable(mc, fnenv, pair.VarName, service.FUNCTION_ARGUMENT, vmm.cp.TypeNameToTypeList[pair.VarType])
	}
	cpF.HiReg = mc.MemTop()
	return cpF
}

// The Vm doesn't *use* abstract types, but they are what values of type TYPE contain, and so it needs to be able to describe them.
func (vmm *VmMaker) addAbstractTypesToVm(mc *service.Vm) {
	nativeAbstractTypes := []string{"single", "struct", "snippet"}
	for _, t := range nativeAbstractTypes {
		mc.AbstractTypes = append(mc.AbstractTypes, values.NameAbstractTypePair{t, vmm.cp.TypeNameToTypeList[t].ToAbstractType()})
	}
}

func (vmm *VmMaker) compileFunction(mc *service.Vm, node ast.Node, outerEnv *service.Environment, dec declarationType) *service.CpFunc {
	cpF := service.CpFunc{}
	var ac service.Access
	if dec == functionDeclaration || dec == privateFunctionDeclaration {
		ac = service.DEF
	} else {
		ac = service.CMD
		cpF.Command = true
	}
	if dec == privateFunctionDeclaration || dec == privateCommandDeclaration {
		cpF.Private = true
	}
	functionName, sig, _, body, given, tupleList := vmm.uP.Parser.ExtractPartsOfFunction(node)
	if body.GetToken().Type == token.PRELOG && body.GetToken().Literal == "" {
		body.(*ast.LogExpression).Value = parser.DescribeFunctionCall(functionName, &sig)
	}
	if vmm.uP.Parser.ErrorsExist() {
		return nil
	}
	if body.GetToken().Type == token.BUILTIN {
		cpF.Builtin = body.(*ast.BuiltInExpression).Name
	}
	fnenv := service.NewEnvironment()
	fnenv.Ext = outerEnv
	cpF.LoReg = mc.MemTop()
	for _, pair := range sig {
		vmm.cp.Reserve(mc, values.UNDEFINED_VALUE, service.DUMMY)
		if pair.VarType == "ref" {
			vmm.cp.AddVariable(mc, fnenv, pair.VarName, service.REFERENCE_VARIABLE, vmm.cp.TypeNameToTypeList[pair.VarType])
			continue
		}
		vmm.cp.AddVariable(mc, fnenv, pair.VarName, service.FUNCTION_ARGUMENT, vmm.cp.TypeNameToTypeList[pair.VarType])
	}
	cpF.HiReg = mc.MemTop()
	cpF.CallTo = mc.CodeTop()
	if len(tupleList) > 0 {
		cpF.TupleReg = vmm.cp.Reserve(mc, values.INT_ARRAY, tupleList)
	} else {
		cpF.TupleReg = service.DUMMY
	}
	switch body.GetToken().Type {
	case token.BUILTIN:
		name := body.(*ast.BuiltInExpression).Name
		types, ok := service.BUILTINS[name]
		if ok {
			cpF.Types = types.T
		} else {
			structNo, ok := vmm.cp.StructNumbers[name]
			if ok {
				cpF.Types = altType(structNo)
			}
		}
	case token.GOLANG:
		cpF.GoNumber = uint32(len(mc.GoFns))
		cpF.HasGo = true
		mc.GoFns = append(mc.GoFns, service.GoFn{body.(*ast.GolangExpression).ObjectCode,
			vmm.goToPf[body.GetToken().Source], vmm.pfToGo[body.GetToken().Source], body.(*ast.GolangExpression).Raw})
	default:
		if given != nil {
			vmm.cp.ThunkList = []service.Thunk{}
			vmm.cp.CompileNode(mc, given, fnenv, ac)
			cpF.CallTo = mc.CodeTop()
			for _, pair := range vmm.cp.ThunkList {
				vmm.cp.Emit(mc, service.Thnk, pair.MLoc, pair.CLoc)
			}
		}
		cpF.Types, _ = vmm.cp.CompileNode(mc, body, fnenv, ac) // TODO --- could we in fact do anything useful if we knew it was a constant?
		vmm.cp.Emit(mc, service.Ret)
		cpF.OutReg = mc.That()
	}
	vmm.cp.Fns = append(vmm.cp.Fns, &cpF)
	if ac == service.DEF && !cpF.Types.IsLegalDefReturn() {
		vmm.cp.P.Throw("comp/return/def/a", node.GetToken())
	}
	if ac == service.CMD && !cpF.Types.IsLegalCmdReturn() {
		vmm.cp.P.Throw("comp/return/cmd/a", node.GetToken())
	}
	return &cpF
}

func (vmm *VmMaker) evaluateConstantsAndVariables(mc *service.Vm) {
	vmm.cp.GlobalVars.Ext = vmm.cp.GlobalConsts
	vmm.cp.Reserve(mc, values.NULL, nil)
	vmm.cp.AddVariable(mc, vmm.cp.GlobalConsts, "NULL", service.GLOBAL_CONSTANT_PUBLIC, altType(values.NULL))
	vmm.cp.Reserve(mc, values.SUCCESSFUL_VALUE, nil)
	vmm.cp.AddVariable(mc, vmm.cp.GlobalConsts, "ok", service.GLOBAL_CONSTANT_PUBLIC, altType(values.SUCCESSFUL_VALUE))
	vmm.cp.Reserve(mc, values.BREAK, nil)
	vmm.cp.AddVariable(mc, vmm.cp.GlobalConsts, "break", service.GLOBAL_CONSTANT_PUBLIC, altType(values.BREAK))
	vmm.cp.TupleType = vmm.cp.Reserve(mc, values.TYPE, values.AbstractType{values.TUPLE})
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
			inferedType, _ := vmm.cp.CompileNode(mc, rhs, vmm.cp.GlobalVars, service.INIT)
			if vmm.uP.ErrorsExist() {
				return
			}
			vmm.cp.Emit(mc, service.Ret)
			mc.Run(runFrom)
			if declarations == int(constantDeclaration) {
				vmm.cp.AddVariable(mc, vmm.cp.GlobalConsts, vname, service.GLOBAL_CONSTANT_PUBLIC, inferedType)
			} else {
				vmm.cp.AddVariable(mc, vmm.cp.GlobalVars, vname, service.GLOBAL_VARIABLE_PUBLIC, inferedType)
			}
			mc.Code = mc.Code[:runFrom]
		}
	}
}

func altType(t ...values.ValueType) service.AlternateType {
	return service.AltType(t...)
}
