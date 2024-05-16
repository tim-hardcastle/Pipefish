package service

import (
	"os"
	"pipefish/source/ast"
	"pipefish/source/lexer"
	"pipefish/source/parser"
	"pipefish/source/settings"
	"pipefish/source/text"
	"pipefish/source/token"
	"pipefish/source/values"
	"strings"

	"database/sql"

	"github.com/lmorg/readline"
)

// Just as the initializer directs the tokenizer and the parser in the construction of the parsed code
// chunks from the tokens, so the vmMaker directs the initializer and compiler in the construction of the mc.

type VmMaker struct {
	cp             *Compiler
	uP             *Initializer
	scriptFilepath string
	goToPf         map[string]func(any) (uint32, []any, bool)
	pfToGo         map[string]func(uint32, []any) any
}

// The base case: we start off with a blank vm.
func StartService(scriptFilepath, sourcecode string, db *sql.DB, hubServices map[string]*VmService) (*VmService, *Initializer) {
	mc := BlankVm(db, hubServices)
	cp, uP := initializeFromFilepath(mc, scriptFilepath) // We pass back the uP bcause it contains the sources and/or errors (in the parser).
	result := &VmService{Mc: mc, Cp: cp}
	mc.OwnService = result
	return result, uP
}

// Then we can recurse over this, passing it the same vm every time.
// This returns a compiler and initializer and mutates the vm.
// We want the initializer back in case there are errors --- it will contain the source code and the errors in the store in its parser.
func initializeFromFilepath(mc *Vm, scriptFilepath string) (*Compiler, *Initializer) {
	sourcecode := ""
	if scriptFilepath != "" { // In which case we're making a blank VM.
		sourcebytes, err := os.ReadFile(scriptFilepath)
		sourcecode = string(sourcebytes) + "\n"
		if err != nil {
			uP := NewInitializer(scriptFilepath, sourcecode)
			uP.Throw("vmm/source/a", token.Token{Source: "linking"}, scriptFilepath)
			return nil, uP
		}
	}
	return initializeFromSourcecode(mc, scriptFilepath, sourcecode)
}

func initializeFromSourcecode(mc *Vm, scriptFilepath, sourcecode string) (*Compiler, *Initializer) {
	vmm := newVmMaker(scriptFilepath, sourcecode, mc)
	vmm.makeAll(mc, scriptFilepath, sourcecode)
	vmm.cp.ScriptFilepath = scriptFilepath
	if !(scriptFilepath == "" || (len(scriptFilepath) >= 5 && scriptFilepath[0:5] == "http:")) {
		file, err := os.Stat(scriptFilepath)
		if err != nil {
			uP := NewInitializer(scriptFilepath, sourcecode)
			uP.Throw("vmm/source/b", token.Token{Source: "linking"}, scriptFilepath)
			return nil, uP
		}
		vmm.cp.Timestamp = file.ModTime().UnixMilli()
	}
	return vmm.cp, vmm.uP
}

func newVmMaker(scriptFilepath, sourcecode string, mc *Vm) *VmMaker {
	uP := NewInitializer(scriptFilepath, sourcecode)
	vmm := &VmMaker{
		cp: NewCompiler(uP.Parser),
		uP: uP,
	}
	vmm.scriptFilepath = scriptFilepath
	vmm.cp.ScriptFilepath = scriptFilepath
	vmm.uP.GetSource(scriptFilepath)
	return vmm
}

func (vmm *VmMaker) makeAll(mc *Vm, scriptFilepath, sourcecode string) {
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

	vmm.uP.ParseImportsAndExternals() // That is, parse the import declarations. The files being imported are imported by the method with the long name below.
	if vmm.uP.ErrorsExist() {
		return
	}

	unnamespacedImports := vmm.InitializeNamespacedImportsAndReturnUnnamespacedImports(mc)
	if vmm.uP.ErrorsExist() {
		return
	}
	vmm.uP.AddToNameSpace(unnamespacedImports)

	vmm.initializeExternals(mc)
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.createEnums(mc)
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.uP.MakeSnippets()
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

	vmm.createSnippetTypes(mc)
	if vmm.uP.ErrorsExist() {
		return
	}

	// We want to ensure that no public type (whether a struct or abstract type) contains a private type.
	vmm.checkTypesForConsistency(mc)
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

	// We add in constructors for the structs, languages, and externals.
	vmm.makeConstructors(mc)

	// And we compile the functions in what is mainly a couple of loops wrapping around the aptly-named
	// compileFunction method.
	vmm.compileFunctions(mc, functionDeclaration)
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

	vmm.compileFunctions(mc, commandDeclaration)
	if vmm.uP.ErrorsExist() {
		return
	}
}

func (vmm *VmMaker) InitializeNamespacedImportsAndReturnUnnamespacedImports(mc *Vm) []string {
	uP := vmm.uP
	unnamespacedImports := []string{}
	for _, imp := range uP.Parser.ParsedDeclarations[importDeclaration] {
		namespace := ""
		scriptFilepath := ""
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
			namespace, scriptFilepath = uP.getPartsOfImportOrExternalDeclaration(imp)
		}
		if namespace == "" {
			unnamespacedImports = append(unnamespacedImports, scriptFilepath)
		}
		newCp, newUP := initializeFromFilepath(mc, scriptFilepath)
		newUP.GetSource(scriptFilepath)
		for k, v := range newUP.Sources {
			uP.Sources[k] = v
		}
		if newUP.ErrorsExist() {
			uP.Parser.Errors = append(uP.Parser.Errors, newUP.Parser.Errors...)
			vmm.cp.Services[namespace] = &VmService{mc, newCp, true, false}
		} else {
			vmm.cp.Services[namespace] = &VmService{mc, newCp, false, false}
			vmm.cp.P.NamespaceBranch[namespace] = &parser.ParserData{newCp.P, scriptFilepath}
		}
	}
	return unnamespacedImports
}

func (vmm *VmMaker) MakeGoMods(mc *Vm, goHandler *GoHandler) {
	uP := vmm.uP
	for source := range goHandler.Modules {
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
	for source := range goHandler.Modules {
		fnSymbol, _ := goHandler.Plugins[source].Lookup("ConvertGoStructHalfwayToPipefish")
		vmm.goToPf[source] = fnSymbol.(func(any) (uint32, []any, bool))
		fnSymbol, _ = goHandler.Plugins[source].Lookup("ConvertPipefishStructToGoStruct")
		vmm.pfToGo[source] = fnSymbol.(func(uint32, []any) any)
	}
	for functionName, fns := range uP.Parser.FunctionTable { // TODO --- why are we doing it like this?
		for _, v := range fns {
			if v.Body.GetToken().Type == token.GOCODE {
				v.Body.(*ast.GolangExpression).ObjectCode = goHandler.GetFn(text.Flatten(functionName), v.Body.GetToken())
			}
		}
	}
	goHandler.CleanUp()
}

func (vmm *VmMaker) compileFunctions(mc *Vm, args ...declarationType) {
	for _, j := range args {
		for i := 0; i < len(vmm.cp.P.ParsedDeclarations[j]); i++ {
			vmm.compileFunction(mc, vmm.cp.P.ParsedDeclarations[j][i], vmm.uP.isPrivate(int(j), i), vmm.cp.GlobalConsts, j)
		}
	}
}

// There are three possibilities. Either we have a namespace without a path, in which case we're looking for
// a service with that name already running on the hub. Or we have a namespace and a filename, in which case
// we're looking for a service with that name running on the hub, checking that it has the same filename,
// updating it if necessary, and if it doesn't exist, trying to launch it.
//
// Note that getPartsOfImportOrExternalDeclaration will guess the default service name from the file name if
// one is not supplied, so there is no need to do it here.
//
// The third case is that we have a namespace and a path to a website. In that case, we need to find out whether
// there is in fact a Pipefish service, or at least something emulating one, on the other end.
//
// Either way, we then need to extract a stub of the external service's public functions, types, etc.
//
// Details of the external services are kept in the vm, because it will have to make the external calls.
func (vmm *VmMaker) initializeExternals(mc *Vm) {
	for _, declaration := range vmm.cp.P.ParsedDeclarations[externalDeclaration] {
		name, path := vmm.uP.getPartsOfImportOrExternalDeclaration(declaration)
		if path == "" { // Then this will work only if there's already an instance of a service of that name running on the hub.
			service, ok := mc.HubServices[name]
			if !ok {
				vmm.uP.Throw("init/external/exist", *declaration.GetToken())
				continue
			}
			vmm.addExternalOnSameHub(mc, service.Cp.ScriptFilepath, name)
			continue
		}
		if len(path) >= 5 && path[0:5] == "http:" {
			pos := strings.LastIndex(path, "/")
			if pos == -1 {
				vmm.uP.Throw("init/external/path/a", *declaration.GetToken())
				continue
			}
			hostpath := path[0:pos]
			serviceName := path[pos+1:]
			pos = strings.LastIndex(hostpath, "/")
			if pos == -1 {
				vmm.uP.Throw("init/external/path/b", *declaration.GetToken())
				continue
			}
			hostname := hostpath[pos+1:]
			// TODO --- there are doubtless reasons why I shouldn't do this with println and rline but I am too tired to remember what they are.
			rline := readline.NewInstance()
			println("Please enter your username and password for hub " + text.CYAN + "'" + hostname + "'" + text.RESET + ".")
			rline.SetPrompt("Username: ")
			username, _ := rline.Readline()
			rline.SetPrompt("Password: ")
			rline.PasswordMask = '▪'
			password, _ := rline.Readline()
			vmm.addHttpService(mc, hostpath, serviceName, username, password)
			continue
		}

		// Otherwise we have a path for which the getParts... function will have inferred a name if one was not supplied.
		hubService, ok := mc.HubServices[name] // If the service already exists, then we just need to check that it uses the same source file.
		if ok {
			if hubService.Cp.ScriptFilepath != path {
				vmm.uP.Throw("init/external/exist", *declaration.GetToken(), hubService.Cp.ScriptFilepath)
			} else {
				vmm.addExternalOnSameHub(mc, path, name)
			}
			continue // Either we've thrown an error or we don't need to do anything.
		}
		// Otherwise we need to start up the service, add it to the hub, and then declare it as external.
		sourcecode, err := os.ReadFile(path)
		if err != nil {
			vmm.uP.Throw("init/external/source", *declaration.GetToken(), path)
			continue
		}
		newService, newUP := StartService(path, string(sourcecode), mc.Database, mc.HubServices)
		// We return the Intializer newUP because if errors have been thrown that's where they are.
		if newUP.ErrorsExist() {
			vmm.uP.Parser.Errors = append(vmm.uP.Parser.Errors, newUP.Parser.Errors...)
			continue
		}
		mc.HubServices[name] = newService
		vmm.addExternalOnSameHub(mc, path, name)
	}
}

func (vmm *VmMaker) addExternalOnSameHub(mc *Vm, path, name string) {
	hubService := mc.HubServices[name]
	serviceToAdd := externalServiceOnSameHub{hubService}
	vmm.addAnyExternalService(mc, serviceToAdd, path, name)
}

func (vmm *VmMaker) addHttpService(mc *Vm, path, name, username, password string) {
	serviceToAdd := httpService{path, name, username, password}
	vmm.addAnyExternalService(mc, serviceToAdd, path, name)
}

func (vmm *VmMaker) addAnyExternalService(mc *Vm, serviceToAdd externalService, path, name string) {
	externalServiceOrdinal := uint32(len(mc.ExternalServices))
	vmm.cp.ExternalOrdinals[name] = externalServiceOrdinal
	mc.ExternalServices = append(mc.ExternalServices, serviceToAdd)
	serializedAPI := serviceToAdd.getAPI()
	sourcecode := SerializedAPIToDeclarations(serializedAPI, uint32(externalServiceOrdinal))
	newCp, newUp := initializeFromSourcecode(mc, path, sourcecode)
	if newUp.ErrorsExist() {
		vmm.cp.P.Errors = append(vmm.cp.P.Errors, newUp.Parser.Errors...)
		return
	}
	vmm.cp.P.NamespaceBranch[name] = &parser.ParserData{newCp.P, path}
	vmm.cp.Services[name] = &VmService{mc, newCp, false, false}
}

// On the one hand, the VM must know the names of the enums and their elements so it can describe them.
// Otoh, the compiler needs to know how to turn enum literals into values.
func (vmm *VmMaker) createEnums(mc *Vm) {
	for i, tokens := range vmm.uP.Parser.TokenizedDeclarations[enumDeclaration] {
		tokens.ToStart()
		tok1 := tokens.NextToken()
		tok2 := tokens.NextToken()
		if !(tok1.Type == token.IDENT && tok2.Type == token.ASSIGN) {
			vmm.uP.Throw("init/enum/lhs", tok1)
		}
		if parser.TypeExists(tok1.Literal, vmm.uP.Parser.TypeSystem) {
			vmm.uP.Throw("init/enum/type", tok1)
		}
		mc.concreteTypeNames = append(mc.concreteTypeNames, tok1.Literal)
		vmm.uP.Parser.Suffixes.Add(tok1.Literal)
		vmm.uP.Parser.TypeSystem.AddTransitiveArrow(tok1.Literal, "enum")
		typeNo := values.LB_ENUMS + values.ValueType(i)
		if vmm.uP.isPrivate(int(enumDeclaration), i) {
			mc.typeAccess = append(mc.typeAccess, PRIVATE)
		} else {
			mc.typeAccess = append(mc.typeAccess, PUBLIC)
		}
		vmm.cp.TypeNameToTypeList["single"] = vmm.cp.TypeNameToTypeList["single"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList["single?"] = vmm.cp.TypeNameToTypeList["single?"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList[tok1.Literal] = altType(typeNo)
		vmm.cp.TypeNameToTypeList[tok1.Literal+"?"] = altType(values.NULL, typeNo)
		mc.Enums = append(mc.Enums, []string{})
		vmm.cp.AnyTypeScheme = vmm.cp.AnyTypeScheme.Union(altType(typeNo))
		// We are now going to assume that the last element of anyType is a TypedTupleType and add the new enum type accordingly.
		lastType := vmm.cp.AnyTypeScheme[len(vmm.cp.AnyTypeScheme)-1].(TypedTupleType)
		vmm.cp.AnyTypeScheme[len(vmm.cp.AnyTypeScheme)-1] = TypedTupleType{(lastType.T).Union(altType(typeNo))}

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

			vmm.cp.EnumElements[tok.Literal] = vmm.cp.Reserve(mc, values.ValueType(i)+values.LB_ENUMS, len(mc.Enums[i]))
			mc.Enums[i] = append(mc.Enums[i], tok.Literal)

			tok = tokens.NextToken()
			if tok.Type != token.COMMA && tok.Type != token.WEAK_COMMA && tok.Type != token.EOF {
				vmm.uP.Throw("init/enum/comma", tok)
			}
			tok = tokens.NextToken()
		}
	}
}

func (vmm *VmMaker) createStructs(mc *Vm) {
	for i, node := range vmm.uP.Parser.ParsedDeclarations[structDeclaration] {
		lhs := node.(*ast.AssignmentExpression).Left
		if lhs.GetToken().Type != token.IDENT {
			vmm.uP.Throw("init/enum/lhs", *lhs.GetToken())
		}
		name := lhs.GetToken().Literal

		_, alreadyExists := vmm.cp.StructNameToTypeNumber[name]
		if alreadyExists {
			vmm.uP.Throw("init/struct/type", *lhs.GetToken())
		}

		// We make the type itself exist.

		typeNo := values.ValueType(len(mc.concreteTypeNames))
		mc.concreteTypeNames = append(mc.concreteTypeNames, name)
		if vmm.uP.isPrivate(int(structDeclaration), i) {
			mc.typeAccess = append(mc.typeAccess, PRIVATE)
		} else {
			mc.typeAccess = append(mc.typeAccess, PUBLIC)
		}
		vmm.cp.TypeNameToTypeList["single"] = vmm.cp.TypeNameToTypeList["single"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList["single?"] = vmm.cp.TypeNameToTypeList["single?"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList["struct"] = vmm.cp.TypeNameToTypeList["struct"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList["struct?"] = vmm.cp.TypeNameToTypeList["struct?"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList[name] = altType(typeNo)
		vmm.cp.TypeNameToTypeList[name+"?"] = altType(values.NULL, typeNo)
		vmm.cp.StructNameToTypeNumber[name] = typeNo
		vmm.cp.AnyTypeScheme = vmm.cp.AnyTypeScheme.Union(altType(typeNo))
		// We are now going to assume that the last element of anyType is a TypedTupleType and add the new struct type accordingly.
		lastType := vmm.cp.AnyTypeScheme[len(vmm.cp.AnyTypeScheme)-1].(TypedTupleType)
		vmm.cp.AnyTypeScheme[len(vmm.cp.AnyTypeScheme)-1] = TypedTupleType{(lastType.T).Union(altType(typeNo))}

		// The parser needs to know about it too.
		vmm.uP.Parser.Functions.Add(name)
		sig := node.(*ast.AssignmentExpression).Right.(*ast.StructExpression).Sig
		vmm.cp.P.FunctionTable.Add(vmm.cp.P.TypeSystem, name, ast.Function{Sig: sig, Body: &ast.BuiltInExpression{Name: name}}) // TODO --- give them their own ast type?

		// We make the labels exist.

		labelsForStruct := make([]int, 0, len(sig))
		typesForStruct := make([]AlternateType, 0, len(sig))
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
		for _, labelNameAndType := range sig {
			var abType values.AbstractType
			typeName := labelNameAndType.VarType
			switch {
			case typeName == "string":
				abType.Varchar = DUMMY
				abType.Types = []values.ValueType{values.STRING}
			case typeName == "string?":
				abType.Varchar = DUMMY
				abType.Types = []values.ValueType{values.NULL, values.STRING}
			case len(typeName) >= 8 && typeName[0:8] == "varchar(" && !(typeName[len(typeName)-1] != '?'):
				vc, _ := parser.GetLengthFromType(typeName)
				abType.Varchar = uint32(vc)
				abType.Types = []values.ValueType{values.STRING}
			case len(typeName) >= 8 && typeName[0:8] == "varchar(" && !(typeName[len(typeName)-1] == '?'):
				vc, _ := parser.GetLengthFromType(typeName)
				abType.Varchar = uint32(vc)
				abType.Types = []values.ValueType{values.NULL, values.STRING}
			default:
				abType = vmm.cp.TypeNameToTypeList[typeName].ToAbstractType()
			}
			typesForStructForVm = append(typesForStructForVm, abType)
		}
		mc.AbstractStructFields = append(mc.AbstractStructFields, typesForStructForVm)
		mc.AlternateStructFields = append(mc.AlternateStructFields, typesForStruct)
		mc.StructLabels = append(mc.StructLabels, labelsForStruct)
		mc.StructResolve = mc.StructResolve.Add(int(typeNo-mc.Ub_enums), labelsForStruct)
	}
}

func (vmm *VmMaker) createAbstractTypes(mc *Vm) {
	for _, tcc := range vmm.uP.Parser.TokenizedDeclarations[abstractDeclaration] {
		tcc.ToStart()
		nameTok := tcc.NextToken()
		if nameTok.Type != token.IDENT {
			vmm.uP.Throw("init/type/ident", nameTok)
		}
		newTypename := nameTok.Literal
		eqTok := tcc.NextToken()
		if eqTok.Type != token.ASSIGN {
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

func (vmm *VmMaker) createSnippetTypes(mc *Vm) {
	mc.Lb_snippets = values.ValueType(len(mc.concreteTypeNames))
	abTypes := []values.AbstractType{values.AbstractType{[]values.ValueType{values.STRING}, DUMMY}, values.AbstractType{[]values.ValueType{values.MAP}, DUMMY}}
	altTypes := []AlternateType{altType(values.STRING), altType(values.MAP)}
	for i, name := range vmm.cp.P.Snippets {
		sig := ast.Signature{ast.NameTypePair{VarName: "text", VarType: "string"}, ast.NameTypePair{VarName: "env", VarType: "map"}}
		typeNo := values.ValueType(len(mc.concreteTypeNames))
		mc.concreteTypeNames = append(mc.concreteTypeNames, name)
		if vmm.uP.isPrivate(int(languageDeclaration), i) {
			mc.typeAccess = append(mc.typeAccess, PRIVATE)
		} else {
			mc.typeAccess = append(mc.typeAccess, PRIVATE)
		}
		vmm.cp.TypeNameToTypeList["single"] = vmm.cp.TypeNameToTypeList["single"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList["single?"] = vmm.cp.TypeNameToTypeList["single?"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList["struct"] = vmm.cp.TypeNameToTypeList["struct"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList["struct?"] = vmm.cp.TypeNameToTypeList["struct?"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList["snippet"] = vmm.cp.TypeNameToTypeList["snippet"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList["snippet?"] = vmm.cp.TypeNameToTypeList["snippet?"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList[name] = altType(typeNo)
		vmm.cp.TypeNameToTypeList[name+"?"] = altType(values.NULL, typeNo)
		vmm.cp.StructNameToTypeNumber[name] = typeNo

		// The parser needs to know about it too.
		vmm.uP.Parser.Functions.Add(name)
		vmm.cp.P.FunctionTable.Add(vmm.cp.P.TypeSystem, name, ast.Function{Sig: sig, Body: &ast.BuiltInExpression{Name: name}})

		// And the vm.
		vmm.addStructLabelsToMc(mc, name, typeNo, sig)

		mc.AbstractStructFields = append(mc.AbstractStructFields, abTypes)
		mc.AlternateStructFields = append(mc.AlternateStructFields, altTypes)
	}
}

func (vmm *VmMaker) checkTypesForConsistency(mc *Vm) {
	for structOrdinalNumber, fields := range mc.AbstractStructFields {
		structTypeNumber := int(mc.Ub_enums) + structOrdinalNumber
		if mc.typeAccess[structTypeNumber] == PUBLIC {
			for _, ty := range fields {
				if mc.isPrivate(ty) {
					vmm.uP.Throw("init/struct/private", *vmm.uP.Parser.ParsedDeclarations[structDeclaration][structOrdinalNumber].GetToken(), mc.concreteTypeNames[int(mc.Ub_enums)+structOrdinalNumber], mc.DescribeAbstractType(ty))
				}
			}
		}
	}
	for i := len(nativeAbstractTypes); i < len(mc.AbstractTypes); i++ {
		abTypeInfo := mc.AbstractTypes[i]
		for _, vT := range abTypeInfo.AT.Types {
			if mc.typeAccess[vT] == PRIVATE {
				abDeclarationNo := i - len(nativeAbstractTypes)
				vmm.uP.Throw("init/abstract/private", *vmm.uP.Parser.ParsedDeclarations[abstractDeclaration][abDeclarationNo].GetToken(), abTypeInfo.Name)
			}
		}
	}
}

func (vmm *VmMaker) addStructLabelsToMc(mc *Vm, name string, typeNo values.ValueType, sig ast.Signature) {
	labelsForStruct := make([]int, 0, len(sig))
	for _, labelNameAndType := range sig {
		labelName := labelNameAndType.VarName
		labelLocation, alreadyExists := vmm.cp.FieldLabelsInMem[labelName]
		if alreadyExists { // Structs can of course have overlapping fields but we don't want to declare them twice.
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

func (vmm *VmMaker) makeConstructors(mc *Vm) {
	for i, node := range vmm.uP.Parser.ParsedDeclarations[structDeclaration] {
		name := node.(*ast.AssignmentExpression).Left.GetToken().Literal // We know this and the next line are safe because we already checked in createStructs
		sig := node.(*ast.AssignmentExpression).Right.(*ast.StructExpression).Sig
		vmm.cp.Fns = append(vmm.cp.Fns, vmm.compileConstructor(mc, name, sig))
		vmm.cp.Fns[len(vmm.cp.Fns)-1].Private = vmm.uP.isPrivate(int(structDeclaration), i)
	}
	sig := ast.Signature{ast.NameTypePair{VarName: "text", VarType: "string"}, ast.NameTypePair{VarName: "env", VarType: "map"}}
	for i, name := range vmm.cp.P.Snippets {
		vmm.cp.Fns = append(vmm.cp.Fns, vmm.compileConstructor(mc, name, sig))
		vmm.cp.Fns[len(vmm.cp.Fns)-1].Private = vmm.uP.isPrivate(int(languageDeclaration), i)
	}
}

func (vmm *VmMaker) compileConstructor(mc *Vm, name string, sig ast.Signature) *CpFunc {
	typeNo := vmm.cp.StructNameToTypeNumber[name]
	cpF := &CpFunc{Types: altType(typeNo), Builtin: name}
	fnenv := NewEnvironment() // Note that we don't use this for anything, we just need some environment to pass to addVariables.
	cpF.LoReg = mc.MemTop()
	for _, pair := range sig {
		vmm.cp.AddVariable(mc, fnenv, pair.VarName, FUNCTION_ARGUMENT, vmm.cp.TypeNameToTypeList[pair.VarType])
	}
	cpF.HiReg = mc.MemTop()
	return cpF
}

var nativeAbstractTypes = []string{"single", "struct", "snippet"}

// The Vm doesn't *use* abstract types, but they are what values of type TYPE contain, and so it needs to be able to describe them.
func (vmm *VmMaker) addAbstractTypesToVm(mc *Vm) {
	for _, t := range nativeAbstractTypes {
		mc.AbstractTypes = append(mc.AbstractTypes, values.NameAbstractTypePair{t, vmm.cp.TypeNameToTypeList[t].ToAbstractType()})
	}
}

func (vmm *VmMaker) compileFunction(mc *Vm, node ast.Node, private bool, outerEnv *Environment, dec declarationType) *CpFunc {
	cpF := CpFunc{}
	var ac cpAccess
	if dec == functionDeclaration {
		ac = DEF
	} else {
		ac = CMD
		cpF.Command = true
	}
	cpF.Private = private
	functionName, _, sig, _, body, given, tupleList := vmm.uP.Parser.ExtractPartsOfFunction(node)
	if body.GetToken().Type == token.PRELOG && body.GetToken().Literal == "" {
		body.(*ast.LogExpression).Value = parser.DescribeFunctionCall(functionName, &sig)
	}
	if vmm.uP.Parser.ErrorsExist() {
		return nil
	}
	if body.GetToken().Type == token.BUILTIN {
		cpF.Builtin = body.(*ast.BuiltInExpression).Name
	}
	if body.GetToken().Type == token.XCALL {
		Xargs := body.(*ast.PrefixExpression).Args
		cpF.Xcall = &XBindle{ExternalServiceOrdinal: uint32(Xargs[0].(*ast.IntegerLiteral).Value), FunctionName: Xargs[1].(*ast.StringLiteral).Value, Position: uint32(Xargs[2].(*ast.IntegerLiteral).Value)}
		serializedTypescheme := Xargs[3].(*ast.StringLiteral).Value
		cpF.Types = vmm.cp.deserializeTypescheme(serializedTypescheme)
	}
	fnenv := NewEnvironment()
	fnenv.Ext = outerEnv
	cpF.LoReg = mc.MemTop()
	for _, pair := range sig {
		vmm.cp.Reserve(mc, values.UNDEFINED_VALUE, DUMMY)
		if pair.VarType == "ref" {
			vmm.cp.AddVariable(mc, fnenv, pair.VarName, REFERENCE_VARIABLE, vmm.cp.TypeNameToTypeList[pair.VarType])
			continue
		}
		typeName := pair.VarType
		if len(typeName) >= 8 && typeName[0:8] == "varchar(" {
			if typeName[len(typeName)-1] == '?' {
				vmm.cp.AddVariable(mc, fnenv, pair.VarName, FUNCTION_ARGUMENT, vmm.cp.TypeNameToTypeList["string?"]) // TODO --- need to attach varchar to variables.
			} else {
				vmm.cp.AddVariable(mc, fnenv, pair.VarName, FUNCTION_ARGUMENT, vmm.cp.TypeNameToTypeList["string"])
			}
		} else {
			vmm.cp.AddVariable(mc, fnenv, pair.VarName, FUNCTION_ARGUMENT, vmm.cp.TypeNameToTypeList[pair.VarType])
		}
	}
	cpF.HiReg = mc.MemTop()
	cpF.CallTo = mc.CodeTop()
	if len(tupleList) > 0 {
		cpF.TupleReg = vmm.cp.Reserve(mc, values.INT_ARRAY, tupleList)
	} else {
		cpF.TupleReg = DUMMY
	}
	switch body.GetToken().Type {
	case token.BUILTIN:
		name := body.(*ast.BuiltInExpression).Name
		types, ok := BUILTINS[name]
		if ok {
			cpF.Types = types.T
		} else {
			structNo, ok := vmm.cp.StructNameToTypeNumber[name] // We treat the short struct constructors as builtins.
			if ok {
				cpF.Types = altType(structNo)
			}
		}
	case token.GOCODE:
		cpF.GoNumber = uint32(len(mc.GoFns))
		cpF.HasGo = true
		mc.GoFns = append(mc.GoFns, GoFn{body.(*ast.GolangExpression).ObjectCode,
			vmm.goToPf[body.GetToken().Source], vmm.pfToGo[body.GetToken().Source], body.(*ast.GolangExpression).Raw})
	case token.XCALL:
	default:
		if given != nil {
			vmm.cp.ThunkList = []Thunk{}
			vmm.cp.CompileNode(mc, given, fnenv, ac)
			cpF.CallTo = mc.CodeTop()
			for _, pair := range vmm.cp.ThunkList {
				vmm.cp.Emit(mc, Thnk, pair.MLoc, pair.CLoc)
			}
		}
		cpF.Types, _ = vmm.cp.CompileNode(mc, body, fnenv, ac) // TODO --- could we in fact do anything useful if we knew it was a constant?
		vmm.cp.Emit(mc, Ret)
		cpF.OutReg = mc.That()
	}
	vmm.cp.Fns = append(vmm.cp.Fns, &cpF)
	if ac == DEF && !cpF.Types.IsLegalDefReturn() {
		vmm.cp.P.Throw("comp/return/def/a", node.GetToken())
	}
	if ac == CMD && !cpF.Types.IsLegalCmdReturn() {
		vmm.cp.P.Throw("comp/return/cmd/a", node.GetToken())
	}
	return &cpF
}

func (vmm *VmMaker) evaluateConstantsAndVariables(mc *Vm) {
	vmm.cp.GlobalVars.Ext = vmm.cp.GlobalConsts
	vmm.cp.Reserve(mc, values.NULL, nil)
	vmm.cp.AddVariable(mc, vmm.cp.GlobalConsts, "NULL", GLOBAL_CONSTANT_PUBLIC, altType(values.NULL))
	vmm.cp.Reserve(mc, values.SUCCESSFUL_VALUE, nil)
	vmm.cp.AddVariable(mc, vmm.cp.GlobalConsts, "OK", GLOBAL_CONSTANT_PUBLIC, altType(values.SUCCESSFUL_VALUE))
	vmm.cp.Reserve(mc, values.BREAK, nil)
	vmm.cp.AddVariable(mc, vmm.cp.GlobalConsts, "break", GLOBAL_CONSTANT_PUBLIC, altType(values.BREAK))
	vmm.cp.TupleType = vmm.cp.Reserve(mc, values.TYPE, values.AbstractType{[]values.ValueType{values.TUPLE}, 0})
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
			inferedType, _ := vmm.cp.CompileNode(mc, rhs, vmm.cp.GlobalVars, INIT)
			if vmm.uP.ErrorsExist() {
				return
			}
			vmm.cp.Emit(mc, Ret)
			mc.Run(runFrom)
			isPrivate := vmm.uP.isPrivate(declarations, v)
			if declarations == int(constantDeclaration) {
				if isPrivate {
					vmm.cp.AddVariable(mc, vmm.cp.GlobalConsts, vname, GLOBAL_CONSTANT_PRIVATE, inferedType)
				} else {
					vmm.cp.AddVariable(mc, vmm.cp.GlobalConsts, vname, GLOBAL_CONSTANT_PUBLIC, inferedType)
				}
			} else {
				if isPrivate {
					vmm.cp.AddVariable(mc, vmm.cp.GlobalVars, vname, GLOBAL_VARIABLE_PRIVATE, inferedType)
				} else {
					vmm.cp.AddVariable(mc, vmm.cp.GlobalVars, vname, GLOBAL_VARIABLE_PUBLIC, inferedType)
				}
			}
			mc.Code = mc.Code[:runFrom]
		}
	}
}

func altType(t ...values.ValueType) AlternateType { // TODO --- Why!?!
	return AltType(t...)
}
