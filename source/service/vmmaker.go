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
// chunks from the tokens, so the vmMaker directs the initializer and compiler in the construction of the vmm.cp.vm.

type VmMaker struct {
	cp     *Compiler
	uP     *Initializer
	goToPf map[string]func(any) (uint32, []any, bool)
	pfToGo map[string]func(uint32, []any) any
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
			uP.Throw("init/source/a", token.Token{Source: "linking"}, scriptFilepath)
			return nil, uP
		}
	}
	return initializeFromSourcecode(mc, scriptFilepath, sourcecode)
}

func initializeFromSourcecode(mc *Vm, scriptFilepath, sourcecode string) (*Compiler, *Initializer) {
	vmm := newVmMaker(scriptFilepath, sourcecode, mc)
	vmm.makeAll(scriptFilepath, sourcecode)
	vmm.cp.ScriptFilepath = scriptFilepath
	if !(scriptFilepath == "" || (len(scriptFilepath) >= 5 && scriptFilepath[0:5] == "http:")) {
		file, err := os.Stat(scriptFilepath)
		if err != nil {
			uP := NewInitializer(scriptFilepath, sourcecode)
			uP.Throw("init/source/b", token.Token{Source: "linking"}, scriptFilepath)
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
	vmm.cp.ScriptFilepath = scriptFilepath
	vmm.cp.vm = mc
	vmm.uP.GetSource(scriptFilepath)
	return vmm
}

func (vmm *VmMaker) makeAll(scriptFilepath, sourcecode string) {
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

	unnamespacedImports := vmm.InitializeNamespacedImportsAndReturnUnnamespacedImports()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.uP.AddToNameSpace(unnamespacedImports)
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.initializeExternals()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.createEnums()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.uP.MakeSnippets()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.uP.addTypesToParser()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.uP.addConstructorsToParserAndParseStructDeclarations()
	if vmm.uP.ErrorsExist() {
		return
	}

	// We make the struct names and labels, but not the constructors, which come later.
	vmm.createStructs()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.defineAbstractTypes()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.addFieldsToStructs()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.createSnippetTypes()
	if vmm.uP.ErrorsExist() {
		return
	}

	// We want to ensure that no public type (whether a struct or abstract type) contains a private type.
	vmm.checkTypesForConsistency()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.addAbstractTypesToVm()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.uP.ParseEverything()
	if vmm.uP.ErrorsExist() {
		return
	}

	// An intermediate step that groups the functions by name and orders them by specificity in a "function table".
	// We return a GoHandler for the next step.
	goHandler := vmm.uP.MakeFunctions()
	if vmm.uP.ErrorsExist() {
		return
	}

	// We build the Go files, if any.
	vmm.MakeGoMods(goHandler)
	if vmm.uP.ErrorsExist() {
		return
	}

	// Now we turn this into a different data structure, a "function tree" with its branches labeled
	// with types. Following it tells us which version of an overloaded function to use.
	vmm.uP.MakeFunctionTrees()
	if vmm.uP.ErrorsExist() {
		return
	}

	// We add in constructors for the structs, languages, and externals.
	vmm.makeConstructors()
	if vmm.uP.ErrorsExist() {
		return
	}

	// And we compile the functions in what is mainly a couple of loops wrapping around the aptly-named
	// compileFunction method.
	vmm.compileFunctions(functionDeclaration)
	if vmm.uP.ErrorsExist() {
		return
	}

	// Finally we can evaluate the constants and variables, which needs the functions to be compiled
	// first because the RHS of the assignment can be any expression.
	// NOTE: is this even going to work any more? You also need to use the types of the variables/consts.
	// So it all needs to be thrown into a dependency digraph and sorted.
	vmm.evaluateConstantsAndVariables()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.compileFunctions(commandDeclaration)
	if vmm.uP.ErrorsExist() {
		return
	}
}

func (vmm *VmMaker) InitializeNamespacedImportsAndReturnUnnamespacedImports() []string {
	uP := vmm.uP
	unnamespacedImports := []string{}
	for i, imp := range uP.Parser.ParsedDeclarations[importDeclaration] {
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
		newCp, newUP := initializeFromFilepath(vmm.cp.vm, scriptFilepath)
		newUP.GetSource(scriptFilepath)
		if newUP.ErrorsExist() {
			uP.Parser.GetErrorsFrom(newUP.Parser)
			vmm.cp.Services[namespace] = &VmService{vmm.cp.vm, newCp, true, false}
		} else {
			vmm.cp.Services[namespace] = &VmService{vmm.cp.vm, newCp, false, false}
			vmm.cp.P.NamespaceBranch[namespace] = &parser.ParserData{newCp.P, scriptFilepath}
			newUP.Parser.Private = vmm.uP.isPrivate(int(importDeclaration), i)
		}
	}
	return unnamespacedImports
}

func (vmm *VmMaker) MakeGoMods(goHandler *GoHandler) {
	uP := vmm.uP
	for source := range goHandler.Modules {
		goHandler.TypeDeclarations[source] = vmm.cp.MakeTypeDeclarationsForGo(goHandler, source)
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

func (vmm *VmMaker) compileFunctions(args ...declarationType) {
	for _, j := range args {
		for i := 0; i < len(vmm.cp.P.ParsedDeclarations[j]); i++ {
			vmm.compileFunction(vmm.cp.P.ParsedDeclarations[j][i], vmm.uP.isPrivate(int(j), i), vmm.cp.GlobalConsts, j)
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
func (vmm *VmMaker) initializeExternals() {
	for _, declaration := range vmm.cp.P.ParsedDeclarations[externalDeclaration] {
		name, path := vmm.uP.getPartsOfImportOrExternalDeclaration(declaration)
		if path == "" { // Then this will work only if there's already an instance of a service of that name running on the hub.
			service, ok := vmm.cp.vm.HubServices[name]
			if !ok {
				vmm.uP.Throw("init/external/exist/a", *declaration.GetToken())
				continue
			}
			vmm.addExternalOnSameHub(service.Cp.ScriptFilepath, name)
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
			vmm.addHttpService(hostpath, serviceName, username, password)
			continue
		}

		// Otherwise we have a path for which the getParts... function will have inferred a name if one was not supplied.
		hubService, ok := vmm.cp.vm.HubServices[name] // If the service already exists, then we just need to check that it uses the same source file.
		if ok {
			if hubService.Cp.ScriptFilepath != path {
				vmm.uP.Throw("init/external/exist/b", *declaration.GetToken(), hubService.Cp.ScriptFilepath)
			} else {
				vmm.addExternalOnSameHub(path, name)
			}
			continue // Either we've thrown an error or we don't need to do anything.
		}
		// Otherwise we need to start up the service, add it to the hub, and then declare it as external.
		sourcecode, err := os.ReadFile(path)
		if err != nil {
			vmm.uP.Throw("init/external/source", *declaration.GetToken(), path)
			continue
		}
		newService, newUP := StartService(path, string(sourcecode), vmm.cp.vm.Database, vmm.cp.vm.HubServices)
		// We return the Intializer newUP because if errors have been thrown that's where they are.
		if newUP.ErrorsExist() {
			vmm.uP.Parser.GetErrorsFrom(newUP.Parser)
			continue
		}
		vmm.cp.vm.HubServices[name] = newService
		vmm.addExternalOnSameHub(path, name)
	}
}

func (vmm *VmMaker) addExternalOnSameHub(path, name string) {
	hubService := vmm.cp.vm.HubServices[name]
	serviceToAdd := externalCallToHubHandler{hubService}
	vmm.addAnyExternalService(serviceToAdd, path, name)
}

func (vmm *VmMaker) addHttpService(path, name, username, password string) {
	serviceToAdd := externalHttpCallHandler{path, name, username, password}
	vmm.addAnyExternalService(serviceToAdd, path, name)
}

func (vmm *VmMaker) addAnyExternalService(handlerForService externalCallHandler, path, name string) {
	externalServiceOrdinal := uint32(len(vmm.cp.vm.ExternalCallHandlers))
	vmm.cp.CallHandlerNumbersByName[name] = externalServiceOrdinal
	vmm.cp.vm.ExternalCallHandlers = append(vmm.cp.vm.ExternalCallHandlers, handlerForService)
	serializedAPI := handlerForService.getAPI()
	sourcecode := SerializedAPIToDeclarations(serializedAPI, externalServiceOrdinal)
	newCp, newUp := initializeFromSourcecode(vmm.cp.vm, path, sourcecode)
	if newUp.ErrorsExist() {
		vmm.cp.P.GetErrorsFrom(newUp.Parser)
		return
	}
	vmm.cp.P.NamespaceBranch[name] = &parser.ParserData{newCp.P, path}
	newCp.P.Private = vmm.uP.isPrivate(int(externalDeclaration), int(externalServiceOrdinal))
	vmm.cp.Services[name] = &VmService{vmm.cp.vm, newCp, false, false}
}

// On the one hand, the VM must know the names of the enums and their elements so it can describe them.
// Otoh, the compiler needs to know how to turn enum literals into values.
func (vmm *VmMaker) createEnums() {
	for i, tokens := range vmm.uP.Parser.TokenizedDeclarations[enumDeclaration] {
		tokens.ToStart()
		tok1 := tokens.NextToken()
		tokens.NextToken()
		vmm.uP.Parser.TypeSystem.AddTransitiveArrow(tok1.Literal, "enum")
		typeNo := values.FIRST_DEFINED_TYPE + values.ValueType(i)
		vmm.cp.TypeNameToTypeList["single"] = vmm.cp.TypeNameToTypeList["single"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList["single?"] = vmm.cp.TypeNameToTypeList["single?"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList[tok1.Literal] = altType(typeNo)
		vmm.cp.TypeNameToTypeList[tok1.Literal+"?"] = altType(values.NULL, typeNo)
		vmm.cp.AnyTypeScheme = vmm.cp.AnyTypeScheme.Union(altType(typeNo))
		// We are now going to assume that the last element of anyType is a TypedTupleType and add the new enum type accordingly.
		lastType := vmm.cp.AnyTypeScheme[len(vmm.cp.AnyTypeScheme)-1].(TypedTupleType)
		vmm.cp.AnyTypeScheme[len(vmm.cp.AnyTypeScheme)-1] = TypedTupleType{(lastType.T).Union(altType(typeNo))}

		tokens.NextToken() // This says "enum" or we wouldn't be here.
		elementNameList := []string{}
		for tok := tokens.NextToken(); tok.Type != token.EOF; {
			if tok.Type != token.IDENT {
				vmm.uP.Throw("init/enum/ident", tok)
			}
			_, alreadyExists := vmm.cp.EnumElements[tok.Literal]
			if alreadyExists { // Enums in the same namespace can't have overlapping elements or we wouldn't know their type.
				vmm.uP.Throw("init/enum/element", tok)
			}

			vmm.cp.EnumElements[tok.Literal] = vmm.cp.Reserve(values.ValueType(i)+values.FIRST_DEFINED_TYPE, len(vmm.cp.EnumElements))
			elementNameList = append(elementNameList, tok.Literal)
			tok = tokens.NextToken()
			if tok.Type != token.COMMA && tok.Type != token.WEAK_COMMA && tok.Type != token.EOF {
				vmm.uP.Throw("init/enum/comma", tok)
			}
			tok = tokens.NextToken()
		}
		vmm.cp.vm.concreteTypes = append(vmm.cp.vm.concreteTypes, enumType{name: tok1.Literal, elementNames: elementNameList, private: vmm.uP.isPrivate(int(enumDeclaration), i)})
	}
}

// We create the struct types and their field labels but we don't define the field types because we haven't defined all the types even lexically yet, let alone what they are.
func (vmm *VmMaker) createStructs() {
	vmm.cp.structDeclarationNumberToTypeNumber = make(map[int]values.ValueType)
	for i, node := range vmm.uP.Parser.ParsedDeclarations[structDeclaration] {
		lhs := node.(*ast.AssignmentExpression).Left
		name := lhs.GetToken().Literal
		// We make the type itself exist.
		typeNo := values.ValueType(len(vmm.cp.vm.concreteTypes))
		vmm.cp.TypeNameToTypeList["single"] = vmm.cp.TypeNameToTypeList["single"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList["single?"] = vmm.cp.TypeNameToTypeList["single?"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList["struct"] = vmm.cp.TypeNameToTypeList["struct"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList["struct?"] = vmm.cp.TypeNameToTypeList["struct?"].Union(altType(typeNo))
		vmm.cp.TypeNameToTypeList[name] = altType(typeNo)
		vmm.cp.TypeNameToTypeList[name+"?"] = altType(values.NULL, typeNo)
		vmm.cp.StructNameToTypeNumber[name] = typeNo
		vmm.cp.AnyTypeScheme = vmm.cp.AnyTypeScheme.Union(altType(typeNo))
		// We are now going to assume that the last element of AnyTypeScheme is a TypedTupleType and add the new struct type accordingly.
		lastType := vmm.cp.AnyTypeScheme[len(vmm.cp.AnyTypeScheme)-1].(TypedTupleType)
		vmm.cp.AnyTypeScheme[len(vmm.cp.AnyTypeScheme)-1] = TypedTupleType{(lastType.T).Union(altType(typeNo))}

		// The parser needs to know about it too.
		vmm.uP.Parser.Functions.Add(name)
		sig := node.(*ast.AssignmentExpression).Right.(*ast.StructExpression).Sig
		vmm.cp.P.FunctionTable.Add(vmm.cp.P.TypeSystem, name, ast.Function{Sig: sig, Body: &ast.BuiltInExpression{Name: name}}) // TODO --- give them their own ast type?

		// We make the labels exist.

		labelsForStruct := make([]int, 0, len(sig))

		for _, labelNameAndType := range sig {
			labelName := labelNameAndType.VarName
			labelLocation, alreadyExists := vmm.cp.FieldLabelsInMem[labelName]
			if alreadyExists { // Structs can of course have overlapping fields but we don't want to declare them twice..
				labelsForStruct = append(labelsForStruct, vmm.cp.vm.Mem[labelLocation].V.(int))
			} else {
				vmm.cp.FieldLabelsInMem[labelName] = vmm.cp.Reserve(values.LABEL, len(vmm.cp.vm.Labels))
				labelsForStruct = append(labelsForStruct, len(vmm.cp.vm.Labels))
				vmm.cp.vm.Labels = append(vmm.cp.vm.Labels, labelName)
			}
		}

		vmm.cp.structDeclarationNumberToTypeNumber[i] = values.ValueType(len(vmm.cp.vm.concreteTypes))
		stT := structType{name: name, labelNumbers: labelsForStruct, private: vmm.uP.isPrivate(int(structDeclaration), i)}
		stT = stT.addLabels(labelsForStruct)
		vmm.cp.vm.concreteTypes = append(vmm.cp.vm.concreteTypes, stT)
	}
	// A label is private iff it is *only* used by struct types that were declared private.
	// So we set them all private and then weed them out by iterating over the struct definitions.
	for i := 0; i < len(vmm.cp.FieldLabelsInMem); i++ {
		vmm.cp.LabelIsPrivate = append(vmm.cp.LabelIsPrivate, true)
	}
	i := -1
	for _, info := range vmm.cp.vm.concreteTypes {
		if !info.isStruct() {
			continue
		}
		i++
		if vmm.uP.isPrivate(int(structDeclaration), i) {
			continue
		}
		for _, lab := range info.(structType).labelNumbers {
			vmm.cp.LabelIsPrivate[lab] = false
		}
	}
}

func (vmm *VmMaker) defineAbstractTypes() {
	for _, tcc := range vmm.uP.Parser.TokenizedDeclarations[abstractDeclaration] {
		tcc.ToStart()
		nameTok := tcc.NextToken()
		newTypename := nameTok.Literal
		tcc.NextToken()
		typeNames := []string{}
		for {
			typeTok := tcc.NextToken()
			divTok := tcc.NextToken()
			if typeTok.Type != token.IDENT {
				vmm.uP.Throw("init/type/form/b", typeTok)
				break
			}
			if divTok.Type != token.EOF && !(divTok.Type == token.IDENT && divTok.Literal == "/") {
				vmm.uP.Throw("init/type/form/c", typeTok)
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
		vmm.cp.vm.AbstractTypes = append(vmm.cp.vm.AbstractTypes, values.NameAbstractTypePair{newTypename, vmm.cp.TypeNameToTypeList[newTypename].ToAbstractType()})
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

func (vmm *VmMaker) addFieldsToStructs() {
	for i, node := range vmm.uP.Parser.ParsedDeclarations[structDeclaration] {
		structNumber := vmm.cp.structDeclarationNumberToTypeNumber[i]
		structInfo := vmm.cp.vm.concreteTypes[structNumber].(structType)
		sig := node.(*ast.AssignmentExpression).Right.(*ast.StructExpression).Sig
		typesForStruct := make([]AlternateType, 0, len(sig))
		typesForStructForVm := make([]values.AbstractType, 0, len(sig))
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
			typesForStruct = append(typesForStruct, vmm.cp.TypeNameToTypeList[labelNameAndType.VarType])
		}
		structInfo.alternateStructFields = typesForStruct
		structInfo.abstractStructFields = typesForStructForVm
		vmm.cp.vm.concreteTypes[structNumber] = structInfo
	}
}

func (vmm *VmMaker) createSnippetTypes() {
	abTypes := []values.AbstractType{{[]values.ValueType{values.STRING}, DUMMY}, {[]values.ValueType{values.MAP}, DUMMY}}
	altTypes := []AlternateType{altType(values.STRING), altType(values.MAP)}
	for i, name := range vmm.cp.P.Snippets {
		sig := ast.AstSig{ast.NameTypenamePair{VarName: "text", VarType: "string"}, ast.NameTypenamePair{VarName: "env", VarType: "map"}}
		typeNo := values.ValueType(len(vmm.cp.vm.concreteTypes))
		vmm.cp.vm.concreteTypes = append(vmm.cp.vm.concreteTypes, structType{name: name, snippet: true, private: vmm.uP.isPrivate(int(snippetDeclaration), i), abstractStructFields: abTypes, alternateStructFields: altTypes})
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
		vmm.addStructLabelsToMc(name, typeNo, sig)
		vmm.cp.vm.codeGeneratingTypes.Add(typeNo) // This prevents the assignment of a snippet to a variable or a constant from being rolled back, erasing the generated code.
	}
}

func (vmm *VmMaker) checkTypesForConsistency() {
	for typeNumber := int(values.FIRST_DEFINED_TYPE); typeNumber < len(vmm.cp.vm.concreteTypes); typeNumber++ {
		if !vmm.cp.vm.concreteTypes[typeNumber].isStruct() {
			continue
		}
		if !vmm.cp.vm.concreteTypes[typeNumber].isPrivate() {
			for _, ty := range vmm.cp.vm.concreteTypes[typeNumber].(structType).abstractStructFields {
				if vmm.cp.vm.isPrivate(ty) {
					vmm.uP.Throw("init/private/struct", token.Token{}, vmm.cp.vm.concreteTypes[typeNumber], vmm.cp.vm.DescribeAbstractType(ty))
				}
			}
		}
	}
	for i := len(nativeAbstractTypes); i < len(vmm.cp.vm.AbstractTypes); i++ {
		if vmm.uP.isPrivate(int(abstractDeclaration), i) {
			continue
		}
		abTypeInfo := vmm.cp.vm.AbstractTypes[i]
		for _, vT := range abTypeInfo.AT.Types {
			if vmm.cp.vm.concreteTypes[vT].isPrivate() {
				abDeclarationNo := i - len(nativeAbstractTypes)
				vmm.uP.Throw("init/private/abstract", *vmm.uP.Parser.ParsedDeclarations[abstractDeclaration][abDeclarationNo].GetToken(), abTypeInfo.Name)
			}
		}
	}
}

func (vmm *VmMaker) addStructLabelsToMc(name string, typeNo values.ValueType, sig ast.AstSig) { // TODO --- seems like we're only using this for snippets and not regular structs?
	labelsForStruct := make([]int, 0, len(sig))
	for _, labelNameAndType := range sig {
		labelName := labelNameAndType.VarName
		labelLocation, alreadyExists := vmm.cp.FieldLabelsInMem[labelName]
		if alreadyExists { // Structs can of course have overlapping fields but we don't want to declare them twice.
			labelsForStruct = append(labelsForStruct, vmm.cp.vm.Mem[labelLocation].V.(int))
		} else {
			vmm.cp.FieldLabelsInMem[labelName] = vmm.cp.Reserve(values.LABEL, len(vmm.cp.vm.Labels))
			labelsForStruct = append(labelsForStruct, len(vmm.cp.vm.Labels))
			vmm.cp.vm.Labels = append(vmm.cp.vm.Labels, labelName)
		}
	}
	typeInfo := vmm.cp.vm.concreteTypes[typeNo].(structType)
	typeInfo.labelNumbers = labelsForStruct
	typeInfo = typeInfo.addLabels(labelsForStruct)
	vmm.cp.vm.concreteTypes[typeNo] = typeInfo
}

func (vmm *VmMaker) makeConstructors() {
	for i, node := range vmm.uP.Parser.ParsedDeclarations[structDeclaration] {
		name := node.(*ast.AssignmentExpression).Left.GetToken().Literal // We know this and the next line are safe because we already checked in createStructs
		sig := node.(*ast.AssignmentExpression).Right.(*ast.StructExpression).Sig
		vmm.cp.Fns = append(vmm.cp.Fns, vmm.compileConstructor(name, sig))
		vmm.cp.Fns[len(vmm.cp.Fns)-1].Private = vmm.uP.isPrivate(int(structDeclaration), i)
	}
	sig := ast.AstSig{ast.NameTypenamePair{VarName: "text", VarType: "string"}, ast.NameTypenamePair{VarName: "env", VarType: "map"}}
	for i, name := range vmm.cp.P.Snippets {
		vmm.cp.Fns = append(vmm.cp.Fns, vmm.compileConstructor(name, sig))
		vmm.cp.Fns[len(vmm.cp.Fns)-1].Private = vmm.uP.isPrivate(int(snippetDeclaration), i)
	}
}

func (vmm *VmMaker) compileConstructor(name string, sig ast.AstSig) *CpFunc {
	typeNo := vmm.cp.StructNameToTypeNumber[name]
	cpF := &CpFunc{Types: altType(typeNo), Builtin: name}
	fnenv := NewEnvironment() // Note that we don't use this for anything, we just need some environment to pass to addVariables.
	cpF.LoReg = vmm.cp.MemTop()
	for _, pair := range sig {
		vmm.cp.AddVariable(fnenv, pair.VarName, FUNCTION_ARGUMENT, vmm.cp.TypeNameToTypeList[pair.VarType])
	}
	cpF.HiReg = vmm.cp.MemTop()
	return cpF
}

var nativeAbstractTypes = []string{"single", "struct", "snippet"}

// The Vm doesn't *use* abstract types, but they are what values of type TYPE contain, and so it needs to be able to describe them.
func (vmm *VmMaker) addAbstractTypesToVm() {
	for _, t := range nativeAbstractTypes {
		vmm.cp.vm.AbstractTypes = append(vmm.cp.vm.AbstractTypes, values.NameAbstractTypePair{t, vmm.cp.TypeNameToTypeList[t].ToAbstractType()})
	}
}

func (vmm *VmMaker) compileFunction(node ast.Node, private bool, outerEnv *Environment, dec declarationType) *CpFunc {
	cpF := CpFunc{}
	var ac cpAccess
	if dec == functionDeclaration {
		ac = DEF
	} else {
		ac = CMD
		cpF.Command = true
	}
	cpF.Private = private
	functionName, _, sig, rtnSig, body, given, tupleList := vmm.uP.Parser.ExtractPartsOfFunction(node)

	if settings.FUNCTION_TO_PEEK == functionName {
		println(node.String() + "\n")
	}

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
	cpF.LoReg = vmm.cp.MemTop()
	for _, pair := range sig {
		vmm.cp.Reserve(values.UNDEFINED_VALUE, DUMMY)
		if pair.VarType == "ref" {
			vmm.cp.AddVariable(fnenv, pair.VarName, REFERENCE_VARIABLE, vmm.cp.TypeNameToTypeList[pair.VarType])
			continue
		}
		typeName := pair.VarType
		if len(typeName) >= 8 && typeName[0:8] == "varchar(" {
			if typeName[len(typeName)-1] == '?' {
				vmm.cp.AddVariable(fnenv, pair.VarName, FUNCTION_ARGUMENT, vmm.cp.TypeNameToTypeList["string?"]) // TODO --- need to attach varchar to variables.
			} else {
				vmm.cp.AddVariable(fnenv, pair.VarName, FUNCTION_ARGUMENT, vmm.cp.TypeNameToTypeList["string"])
			}
		} else {
			vmm.cp.AddVariable(fnenv, pair.VarName, FUNCTION_ARGUMENT, vmm.cp.TypeNameToTypeList[pair.VarType])
		}
	}
	cpF.HiReg = vmm.cp.MemTop()
	cpF.CallTo = vmm.cp.CodeTop()
	if len(tupleList) > 0 {
		cpF.TupleReg = vmm.cp.Reserve(values.INT_ARRAY, tupleList)
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
		cpF.GoNumber = uint32(len(vmm.cp.vm.GoFns))
		cpF.HasGo = true
		vmm.cp.vm.GoFns = append(vmm.cp.vm.GoFns, GoFn{body.(*ast.GolangExpression).ObjectCode,
			vmm.goToPf[body.GetToken().Source], vmm.pfToGo[body.GetToken().Source], body.(*ast.GolangExpression).Raw})
	case token.XCALL:
	default:
		if given != nil {
			vmm.cp.ThunkList = []Thunk{}
			vmm.cp.CompileNode(given, fnenv, ac)
			cpF.CallTo = vmm.cp.CodeTop()
			for _, pair := range vmm.cp.ThunkList {
				vmm.cp.Emit(Thnk, pair.MLoc, pair.CLoc)
			}
		}
		cpF.Types, _ = vmm.cp.CompileNode(body, fnenv, ac) // TODO --- could we in fact do anything useful if we knew it was a constant?
		cpF.OutReg = vmm.cp.That()

		if rtnSig != nil && !(body.GetToken().Type == token.GOCODE) {
			vmm.cp.emitTypeChecks(cpF.OutReg, cpF.Types, fnenv, rtnSig, ac, node.GetToken(), CHECK_RETURN_TYPES)
		}

		vmm.cp.Emit(Ret)
	}
	vmm.cp.Fns = append(vmm.cp.Fns, &cpF)
	if ac == DEF && !cpF.Types.IsLegalDefReturn() {
		vmm.cp.P.Throw("comp/return/def", node.GetToken())
	}
	if ac == CMD && !cpF.Types.IsLegalCmdReturn() {
		vmm.cp.P.Throw("comp/return/cmd", node.GetToken())
	}
	return &cpF
}

func (vmm *VmMaker) evaluateConstantsAndVariables() {
	vmm.cp.GlobalVars.Ext = vmm.cp.GlobalConsts
	vmm.cp.Reserve(values.NULL, nil)
	vmm.cp.AddVariable(vmm.cp.GlobalConsts, "NULL", GLOBAL_CONSTANT_PUBLIC, altType(values.NULL))
	vmm.cp.Reserve(values.SUCCESSFUL_VALUE, nil)
	vmm.cp.AddVariable(vmm.cp.GlobalConsts, "OK", GLOBAL_CONSTANT_PUBLIC, altType(values.SUCCESSFUL_VALUE))
	vmm.cp.Reserve(values.BREAK, nil)
	vmm.cp.AddVariable(vmm.cp.GlobalConsts, "break", GLOBAL_CONSTANT_PUBLIC, altType(values.BREAK))
	vmm.cp.TupleType = vmm.cp.Reserve(values.TYPE, values.AbstractType{[]values.ValueType{values.TUPLE}, 0})
	for declarations := int(constantDeclaration); declarations <= int(variableDeclaration); declarations++ {
		assignmentOrder := vmm.uP.ReturnOrderOfAssignments(declarations)
		for _, v := range assignmentOrder {
			dec := vmm.uP.Parser.ParsedDeclarations[declarations][v]
			lhs := dec.(*ast.AssignmentExpression).Left
			rhs := dec.(*ast.AssignmentExpression).Right
			sig, _ := vmm.cp.P.RecursivelySlurpSignature(lhs, "*inferred*")
			if vmm.uP.ErrorsExist() {
				return
			}
			rollbackTo := vmm.cp.getState() // Unless the assignment generates code, i.e. we're creating a lambda function or a snippet, then we can roll back the declarations after
			vmm.cp.CompileNode(rhs, vmm.cp.GlobalVars, INIT)
			if vmm.uP.ErrorsExist() {
				return
			}
			vmm.cp.Emit(Ret)
			vmm.cp.vm.Run(uint32(rollbackTo.code))
			result := vmm.cp.vm.Mem[vmm.cp.That()]
			if !vmm.cp.vm.codeGeneratingTypes.Contains(result.T) { // We don't want to roll back the code generated when we make a lambda or a snippet.
				vmm.cp.rollback(rollbackTo)
			}
			isPrivate := vmm.uP.isPrivate(declarations, v)
			var vAcc varAccess
			envToAddTo := vmm.cp.GlobalConsts
			if declarations == int(constantDeclaration) {
				if isPrivate {
					vAcc = GLOBAL_CONSTANT_PRIVATE
				} else {
					vAcc = GLOBAL_CONSTANT_PUBLIC
				}
			} else {
				envToAddTo = vmm.cp.GlobalVars
				if isPrivate {
					vAcc = GLOBAL_VARIABLE_PRIVATE
				} else {
					vAcc = GLOBAL_VARIABLE_PUBLIC
				}
			}

			last := len(sig) - 1
			lastIsTuple := sig[last].VarType == "tuple"
			rhsIsTuple := result.T == values.TUPLE
			tupleLen := 1
			if rhsIsTuple {
				tupleLen = len(result.V.([]values.Value))
			}
			if !lastIsTuple && tupleLen != len(sig) {
				vmm.cp.P.Throw("comp/assign/a", dec.GetToken(), tupleLen, len(sig))
				return
			}
			if lastIsTuple && tupleLen < len(sig)-1 {
				vmm.cp.P.Throw("comp/assign/b", dec.GetToken(), tupleLen, len(sig))
				return
			}
			loopTop := len(sig)
			head := []values.Value{result}
			if lastIsTuple {
				loopTop = last
				if rhsIsTuple {
					head = result.V.([]values.Value)[:last]
					vmm.cp.Reserve(values.TUPLE, result.V.([]values.Value)[last:])
				} else {
					if tupleLen == len(sig)-1 {
						vmm.cp.Reserve(values.TUPLE, []values.Value{})
					} else {
						vmm.cp.Reserve(values.TUPLE, result.V)
					}
				}
				vmm.cp.AddVariable(envToAddTo, sig[last].VarName, vAcc, altType(values.TUPLE))
			} else {
				if rhsIsTuple {
					head = result.V.([]values.Value)
				}
			}
			for i := 0; i < loopTop; i++ {
				vmm.cp.Reserve(head[i].T, head[i].V)
				if sig[i].VarType == "*inferred*" {
					vmm.cp.AddVariable(envToAddTo, sig[i].VarName, vAcc, altType(head[i].T))
				} else {
					allowedTypes := vmm.cp.TypeNameToTypeList[sig[i].VarType]
					if allowedTypes.isNoneOf(head[i].T) {
						vmm.cp.P.Throw("comp/assign/type", dec.GetToken())
						return
					} else {
						vmm.cp.AddVariable(envToAddTo, sig[i].VarName, vAcc, allowedTypes)
					}
				}
			}
		}
	}
}

func altType(t ...values.ValueType) AlternateType { // TODO --- Why!?!
	return AltType(t...)
}
