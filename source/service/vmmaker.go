package service

import (
	"database/sql"
	"embed"
	"os"
	"path/filepath"
	"sort"
	"testing"

	"pipefish/source/ast"
	"pipefish/source/lexer"
	"pipefish/source/parser"
	"pipefish/source/settings"
	"pipefish/source/text"
	"pipefish/source/token"
	"pipefish/source/values"
	"strings"

	"github.com/lmorg/readline"
)

// Do not under any cicumstances remove the following comment.
//
//go:embed test-files/*
var testFolder embed.FS

// The base case: we start off with a blank vm and common parser bindle.
func StartService(scriptFilepath, dir string, db *sql.DB, hubServices map[string]*Service) *Service {
	mc := BlankVm(db, hubServices)
	common := parser.NewCommonBindle()
	cp := initializeFromFilepath(mc, common, scriptFilepath, dir, "") // We pass back the uP bcause it contains the sources and/or errors (in the parser).
	result := &Service{Cp: cp}
	if cp.P.ErrorsExist() {
		return result
	}
	cp.makeFunctionTableAndGoMods()
	if cp.P.ErrorsExist() {
		return result
	}
	cp.populateAbstractTypesAndMakeFunctionTrees()
	if cp.P.ErrorsExist() {
		return result
	}
	cp.compileEverything()
	if cp.P.ErrorsExist() {
		return result
	}

	cp.ResolveInterfaceBacktracks()

	mc.OwnService = result
	return result
}

// Then we can recurse over this, passing it the same vm every time.
// This returns a compiler and and mutates the vm.
// In the case that any errors are produced, the will be in the comon bindle of the parseer of the returned compiler.
func initializeFromFilepath(mc *Vm, common *parser.CommonParserBindle, scriptFilepath, dir string, namespacePath string) *Compiler {
	sourcecode := ""
	var sourcebytes []byte
	var err error
	if scriptFilepath != "" { // In which case we're making a blank VM.
		if len(scriptFilepath) >= 11 && scriptFilepath[:11] == "test-files/" {
			sourcebytes, err = testFolder.ReadFile(scriptFilepath)
		} else {
			sourcebytes, err = os.ReadFile(MakeFilepath(scriptFilepath, dir))
		}
		sourcecode = string(sourcebytes) + "\n"
		if err != nil {
			p := parser.New(common, scriptFilepath, sourcecode, dir, namespacePath) // Just because it's expecting to get a compiler back, with errors contained in the common parser bindle.
			p.Throw("init/source/a", &token.Token{Source: "linking"}, scriptFilepath, err.Error())
			return NewCompiler(p)
		}
	}
	return initializeFromSourcecode(mc, common, scriptFilepath, sourcecode, dir, namespacePath)
}

func initializeFromSourcecode(mc *Vm, common *parser.CommonParserBindle, scriptFilepath, sourcecode, dir string, namespacePath string) *Compiler {
	cp := newVmMaker(common, scriptFilepath, sourcecode, dir, mc, namespacePath)
	cp.parseAll(scriptFilepath, sourcecode)
	cp.ScriptFilepath = scriptFilepath
	if !(scriptFilepath == "" || (len(scriptFilepath) >= 5 && scriptFilepath[0:5] == "http:")) &&
		!testing.Testing() && !(len(scriptFilepath) >= 11 && scriptFilepath[:11] == "test-files/") {
		file, err := os.Stat(MakeFilepath(scriptFilepath, dir))
		if err != nil {
			cp.Throw("init/source/b", token.Token{Source: "linking"}, scriptFilepath)
			return nil
		}
		cp.Timestamp = file.ModTime().UnixMilli()
	}
	cp.P.Common.Sources[scriptFilepath] = strings.Split(sourcecode, "\n")
	return cp
}

func newVmMaker(common *parser.CommonParserBindle, scriptFilepath, sourcecode, dir string, mc *Vm, namespacePath string) *Compiler {
	p := parser.New(common, scriptFilepath, sourcecode, dir, namespacePath)
	cp := NewCompiler(p)
	cp.ScriptFilepath = scriptFilepath
	cp.Vm = mc
	cp.TupleType = cp.Reserve(values.TYPE, values.AbstractType{[]values.ValueType{values.TUPLE}, 0}, &token.Token{Source: "Builtin constant"})
	return cp
}

func (cp *Compiler) cmI(s string) {
	if settings.SHOW_VMM {
		println(text.UNDERLINE + s + text.RESET)
	}
}

// This does everything up to and including parsing the code chunks, and then hands back flow of control to the
// StartService or RunTest method.
func (cp *Compiler) parseAll(scriptFilepath, sourcecode string) {
	cp.cmI("Starting makeall for script " + scriptFilepath + ".")

	if !settings.OMIT_BUILTINS {
		cp.cmI("Adding mandatory imports to namespace.")
		cp.AddToNameSpace(settings.MandatoryImports)
	}
	if len(scriptFilepath) >= 4 && scriptFilepath[len(scriptFilepath)-4:] == ".hub" {
		cp.cmI("Adding hub.pf to hub namespace.")
		cp.AddToNameSpace([]string{"rsc/pipefish/hub.pf"})
	}
	cp.cmI("Making new relexer.")
	cp.P.TokenizedCode = lexer.NewRelexer(scriptFilepath, sourcecode)

	cp.cmI("Making parser and tokenized program.")
	cp.MakeParserAndTokenizedProgram()
	if cp.ErrorsExist() {
		return
	}

	cp.cmI("Parsing import and external declarations.")
	cp.ParseImportsAndExternals() // That is, parse the import declarations. The files being imported are imported by the method with the long name below.
	if cp.ErrorsExist() {
		return
	}

	cp.cmI("Initializing imports.")
	unnamespacedImports := cp.InitializeNamespacedImportsAndReturnUnnamespacedImports()
	if cp.ErrorsExist() {
		return
	}

	cp.cmI("Adding unnamespaced imports to namespace.")
	cp.AddToNameSpace(unnamespacedImports)
	if cp.ErrorsExist() {
		return
	}

	cp.cmI("Initializing external services.")
	cp.initializeExternals()
	if cp.ErrorsExist() {
		return
	}

	cp.cmI("Creating enums.")
	cp.createEnums()
	if cp.ErrorsExist() {
		return
	}

	cp.cmI("Creating clone types.")
	cp.createClones()
	if cp.ErrorsExist() {
		return
	}

	cp.cmI("Creating snippet types, part 1.")
	cp.createSnippetsPart1()
	if cp.ErrorsExist() {
		return
	}

	cp.cmI("Adding types to parser.")
	cp.addTypesToParser()
	if cp.ErrorsExist() {
		return
	}

	cp.cmI("Adding constructors to parser, parsing struct declarations.")
	cp.addConstructorsToParserAndParseStructDeclarations()
	if cp.ErrorsExist() {
		return
	}

	cp.cmI("Creating struct names and labels.")
	cp.createStructNamesAndLabels()
	if cp.ErrorsExist() {
		return
	}

	cp.cmI("Creating abstract types.")
	cp.createAbstractTypes()
	if cp.ErrorsExist() {
		return
	}

	cp.cmI("Creating (but not populating) interface types.")
	cp.createInterfaceTypes()
	if cp.ErrorsExist() {
		return
	}

	cp.cmI("Adding fields to structs.")
	cp.addFieldsToStructs()
	if cp.ErrorsExist() {
		return
	}

	cp.cmI("Creating snippet types, part 2.")
	cp.createSnippetTypesPart2()
	if cp.ErrorsExist() {
		return
	}

	// We want to ensure that no public type (whether a struct or abstract type) contains a private type.
	cp.cmI("Checking types for consistency of encapsulation.")
	cp.checkTypesForConsistency()
	if cp.ErrorsExist() {
		return
	}

	cp.cmI("Parsing everything.")
	cp.ParseEverything()
	if cp.ErrorsExist() {
		return
	}
	// We hand back flow of control to StartService or RunTest.
}

func (cp *Compiler) makeFunctionTableAndGoMods() {
	for _, service := range cp.Services {
		service.Cp.makeFunctionTableAndGoMods()
	}
	// The vm needs to know how to describe the abstract types in words.
	cp.addAbstractTypesToVm()
	if cp.P.ErrorsExist() {
		return
	}

	// The compiler uses a somewhat richer type representation than the one used by the compiler and the
	// runtime.
	cp.makeAlternateTypesFromAbstractTypes()

	// An intermediate step that groups the functions by name and orders them by specificity in a "function table".
	// We return a GoHandler for the next step.
	goHandler := cp.MakeFunctionTable()
	if cp.P.ErrorsExist() {
		return
	}

	// We build the Go files, if any.
	cp.getGoFunctions(goHandler)

	// We add in constructors for the structs, snippets, and clones.
	cp.compileConstructors()
	if cp.P.ErrorsExist() {
		return
	}
}

func (cp *Compiler) InitializeNamespacedImportsAndReturnUnnamespacedImports() []string {
	unnamespacedImports := []string{}
	for i, imp := range cp.P.ParsedDeclarations[importDeclaration] {
		namespace := ""
		scriptFilepath := ""
		switch imp := (imp).(type) {
		case *ast.GolangExpression:
			cp.P.GoImports[imp.Token.Source] = append(cp.P.GoImports[imp.Token.Source], imp.Token.Literal)
			continue
		default:
			namespace, scriptFilepath = cp.getPartsOfImportOrExternalDeclaration(imp)
		}
		if namespace == "" {
			unnamespacedImports = append(unnamespacedImports, scriptFilepath)
		}
		newCp := initializeFromFilepath(cp.Vm, cp.P.Common, scriptFilepath, cp.P.Directory, namespace+"."+cp.P.NamespacePath)
		cp.Services[namespace] = &Service{newCp, false}
		for k, v := range newCp.declarationMap {
			cp.declarationMap[k] = v
		}
		cp.P.NamespaceBranch[namespace] = &parser.ParserData{newCp.P, scriptFilepath}
		newCp.P.Private = cp.P.IsPrivate(int(importDeclaration), i)
	}
	return unnamespacedImports
}

// OTOH, we want the type information spread across the parsers and shared in the common parser bindle to
// collectively be the any source of truth for our type system.
// But it can't be the only *representation* of the truth, becase that would slow things down 'cos the compiler
// would have to keep converting abstract types to alternate types to build the type schemes with.
// The solution is to build the alternate type schemes once and for all from the alternate types, after we've
// entirely finished generating the data in the parsers.
func (cp *Compiler) makeAlternateTypesFromAbstractTypes() {
	cp.typeNameToTypeScheme = make(map[string]AlternateType)
	for typename, abType := range cp.P.TypeMap {
		cp.typeNameToTypeScheme[typename] = AbstractTypeToAlternateType(abType)
	}
	for typename, abType := range cp.P.Common.Types {
		cp.typeNameToTypeScheme[typename] = AbstractTypeToAlternateType(abType)
	}
}

type labeledParsedCodeChunk struct {
	chunk     ast.Node
	decType   declarationType
	decNumber int
}

type serviceVariableData struct {
	ty          AlternateType
	deflt       values.Value
	mustBeConst bool
	vAcc        varAccess
}

var serviceVariables = map[string]serviceVariableData{
	"$LOGGING": {altType(), values.Value{}, true, GLOBAL_CONSTANT_PUBLIC}, // The values have to be extracted from the compiler.
}

// At this point we have our functions as parsed code chunks in the uP.Parser.ParsedDeclarations(functionDeclaration)
// slice. We want to read their signatures and order them according to specificity for the purposes of
// implementing overloading.
func (cp *Compiler) MakeFunctionTable() *GoHandler {
	// Some of our functions may be written in Go, so we have a GoHandler standing by just in case.
	goHandler := newGoHandler(cp.P)
	for j := functionDeclaration; j <= commandDeclaration; j++ {
		for i := 0; i < len(cp.P.ParsedDeclarations[j]); i++ {
			tok := cp.P.ParsedDeclarations[j][i].GetToken()
			functionName, position, sig, rTypes, body, given := cp.P.ExtractPartsOfFunction(cp.P.ParsedDeclarations[j][i])
			if body == nil {
				cp.P.Throw("init/func/body", tok)
				return nil
			}
			if body.GetToken().Type == token.PRELOG && body.GetToken().Literal == "" {
				body.(*ast.LogExpression).Value = parser.DescribeFunctionCall(functionName, &sig)
			}
			if cp.P.ErrorsExist() {
				return nil
			}
			functionToAdd := &ast.PrsrFunction{Sig: cp.P.MakeAbstractSigFromStringSig(sig), NameSig: sig, Position: position, NameRets: rTypes, RtnSig: cp.P.MakeAbstractSigFromStringSig(rTypes), Body: body, Given: given,
				Cmd: j == commandDeclaration, Private: cp.P.IsPrivate(int(j), i), Number: DUMMY, Compiler: cp, Tok: body.GetToken()}
			cp.fnIndex[fnSource{j, i}] = functionToAdd
			if cp.shareable(functionToAdd) || settings.MandatoryImportSet.Contains(tok.Source) {
				cp.cmI("Adding " + functionName + " to common functions.")
				cp.P.Common.Functions[parser.FuncSource{tok.Source, tok.Line, functionName, position}] = functionToAdd
			}
			conflictingFunction := cp.P.FunctionTable.Add(cp.P, functionName, functionToAdd)
			if conflictingFunction != nil && conflictingFunction != functionToAdd {
				cp.P.Throw("init/overload/a", body.GetToken(), functionName, conflictingFunction.Tok.Line)
				return nil
			}
			if body.GetToken().Type == token.GOCODE {
				for _, v := range sig {
					goHandler.UserDefinedTypes.Add(v.VarType)
				}
				for _, v := range rTypes {
					goHandler.UserDefinedTypes.Add(v.VarType)
				}
				cp.generateGoFunctionCode(goHandler, flatten(functionName), sig, rTypes, body.(*ast.GolangExpression), cp.P.Directory)
				if cp.P.ErrorsExist() {
					return nil
				}
				body.(*ast.GolangExpression).Sig = sig
				body.(*ast.GolangExpression).ReturnTypes = rTypes
			}
		}
	}

	// We may also have pure Go declarations:

	for _, golang := range cp.P.TokenizedDeclarations[golangDeclaration] {
		golang.ToStart()
		token := golang.NextToken()
		source := token.Source
		code := token.Literal[:len(token.Literal)]
		goHandler.addPureGoBlock(source, code)
	}
	return goHandler
}

type funcWithName struct {
	name  string
	pFunc *ast.PrsrFunction
}

func (cp *Compiler) populateAbstractTypesAndMakeFunctionTrees() {
	// First we recurse.
	for _, service := range cp.Services {
		service.Cp.populateAbstractTypesAndMakeFunctionTrees()
	}

	// Now we pull in all the shared functions that fulfill the interface types, populating the types as we go.
	for _, tcc := range cp.P.TokenizedDeclarations[interfaceDeclaration] {
		tcc.ToStart()
		nameTok := tcc.NextToken()
		typename := nameTok.Literal
		typeInfo, _ := cp.getDeclaration(decINTERFACE, &nameTok, DUMMY)
		types := values.MakeAbstractType()
		funcsToAdd := map[values.ValueType][]funcWithName{}
		for i, sigToMatch := range typeInfo.(interfaceInfo).sigs {
			typesMatched := values.MakeAbstractType()
			for key, fnToTry := range cp.P.Common.Functions {
				if key.FunctionName == sigToMatch.name {
					matches := cp.getMatches(sigToMatch, fnToTry, &nameTok)
					typesMatched = typesMatched.Union(matches)
					if !settings.MandatoryImportSet.Contains(fnToTry.Tok.Source) {
						for _, ty := range matches.Types {
							if _, ok := funcsToAdd[ty]; ok {
								funcsToAdd[ty] = append(funcsToAdd[ty], funcWithName{key.FunctionName, fnToTry})
							} else {
								funcsToAdd[ty] = []funcWithName{funcWithName{key.FunctionName, fnToTry}}
							}
						}
					}
				}
			}
			if i == 0 {
				types = typesMatched
			} else {
				types = types.Intersect(typesMatched)
			}
		}
		// We have created an abstract type from our interface! We put it in the type map.
		cp.P.TypeMap[typename] = types
		typesWithNull := types.Insert(values.NULL)
		cp.P.TypeMap[typename+"?"] = typesWithNull
		cp.AddTypeToVm(values.AbstractTypeInfo{typename, cp.P.NamespacePath, types})
		// And we add all the implicated functions to the function table.
		for _, ty := range types.Types {
			for _, fn := range funcsToAdd[ty] {
				conflictingFunction := cp.P.FunctionTable.Add(cp.P, fn.name, fn.pFunc)
				if conflictingFunction != nil && conflictingFunction != fn.pFunc {
					cp.P.Throw("init/overload/b", fn.pFunc.Tok, fn.name, conflictingFunction.Tok.Line)
				}
			}
		}
	}

	if settings.FUNCTION_TO_PEEK != "" {
		println(cp.P.FunctionTable.Describe(cp.P, settings.FUNCTION_TO_PEEK))
	}

	if cp.P.ErrorsExist() {
		return
	}
	// Now we turn the function table into a different data structure, a "function tree" with its branches labeled
	// with types. Following it tells us which version of an overloaded function to use.
	cp.MakeFunctionTrees()
	if cp.P.ErrorsExist() {
		return
	}
}

func (cp *Compiler) getMatches(sigToMatch fnSigInfo, fnToTry *ast.PrsrFunction, tok *token.Token) values.AbstractType {
	result := values.MakeAbstractType()
	if sigToMatch.sig.Len() != len(fnToTry.Sig) {
		return result
	}
	if sigToMatch.rtnSig.Len() != 0 && sigToMatch.rtnSig.Len() != len(fnToTry.RtnSig) {
		return result
	}
	foundSelf := false
	for i := 0; i < len(sigToMatch.sig); i++ {
		if sigToMatch.sig.GetVarType(i).(string) == "self" {
			if foundSelf {
				result = result.Intersect(fnToTry.Sig[i].VarType)
				if len(result.Types) == 0 {
					break
				}
			} else {
				foundSelf = true
				result = fnToTry.Sig[i].VarType
			}
		} else {
			if !cp.P.GetAbstractType(sigToMatch.sig.GetVarType(i).(string)).IsSubtypeOf(fnToTry.Sig[i].VarType) ||
				sigToMatch.sig.GetVarType(i).(string) == "bling" && sigToMatch.sig.GetVarName(i) != fnToTry.Sig[i].VarName {
				return values.MakeAbstractType()
			}
		}
	}
	if !foundSelf {
		cp.P.Throw("init/interface/self", tok)
		return values.MakeAbstractType()
	}
	for i := 0; i < sigToMatch.rtnSig.Len(); i++ {
		if sigToMatch.rtnSig[i].VarType == "self" {
			result = result.Intersect(fnToTry.RtnSig[i].VarType)
		} else {
			if !fnToTry.RtnSig[i].VarType.IsSubtypeOf(cp.P.GetAbstractType(sigToMatch.rtnSig[i].VarType)) {
				return values.MakeAbstractType()
			}
		}
	}
	return result
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
func (cp *Compiler) initializeExternals() {
	for _, declaration := range cp.P.ParsedDeclarations[externalDeclaration] {
		name, path := cp.getPartsOfImportOrExternalDeclaration(declaration)
		if path == "" { // Then this will work only if there's already an instance of a service of that name running on the hub.
			service, ok := cp.Vm.HubServices[name]
			if !ok {
				cp.Throw("init/external/exist/a", *declaration.GetToken())
				continue
			}
			cp.addExternalOnSameHub(service.Cp.ScriptFilepath, name)
			continue
		}
		if len(path) >= 5 && path[0:5] == "http:" {
			pos := strings.LastIndex(path, "/")
			if pos == -1 {
				cp.Throw("init/external/path/a", *declaration.GetToken())
				continue
			}
			hostpath := path[0:pos]
			serviceName := path[pos+1:]
			pos = strings.LastIndex(hostpath, "/")
			if pos == -1 {
				cp.Throw("init/external/path/b", *declaration.GetToken())
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
			cp.addHttpService(hostpath, serviceName, username, password)
			continue
		}

		// Otherwise we have a path for which the getParts... function will have inferred a name if one was not supplied.
		hubService, ok := cp.Vm.HubServices[name] // If the service already exists, then we just need to check that it uses the same source file.
		if ok {
			if hubService.Cp.ScriptFilepath != path {
				cp.Throw("init/external/exist/b", *declaration.GetToken(), hubService.Cp.ScriptFilepath)
			} else {
				cp.addExternalOnSameHub(path, name)
			}
			continue // Either we've thrown an error or we don't need to do anything.
		}
		// Otherwise we need to start up the service, add it to the hub, and then declare it as external.
		newService := StartService(path, cp.P.Directory, cp.Vm.Database, cp.Vm.HubServices)
		cp.Vm.HubServices[name] = newService
		cp.addExternalOnSameHub(path, name)
	}
}

func (cp *Compiler) addExternalOnSameHub(path, name string) {
	hubService := cp.Vm.HubServices[name]
	serviceToAdd := externalCallToHubHandler{hubService}
	cp.addAnyExternalService(serviceToAdd, path, name)
}

func (cp *Compiler) addHttpService(path, name, username, password string) {
	serviceToAdd := externalHttpCallHandler{path, name, username, password}
	cp.addAnyExternalService(serviceToAdd, path, name)
}

func (cp *Compiler) addAnyExternalService(handlerForService externalCallHandler, path, name string) {
	externalServiceOrdinal := uint32(len(cp.Vm.ExternalCallHandlers))
	cp.CallHandlerNumbersByName[name] = externalServiceOrdinal
	cp.Vm.ExternalCallHandlers = append(cp.Vm.ExternalCallHandlers, handlerForService)
	serializedAPI := handlerForService.getAPI()
	sourcecode := SerializedAPIToDeclarations(serializedAPI, externalServiceOrdinal)
	newCp := initializeFromSourcecode(cp.Vm, cp.P.Common, path, sourcecode, cp.P.Directory, name+"."+cp.P.NamespacePath)
	cp.P.NamespaceBranch[name] = &parser.ParserData{newCp.P, path}
	newCp.P.Private = cp.P.IsPrivate(int(externalDeclaration), int(externalServiceOrdinal))
	cp.Services[name] = &Service{newCp, false}
}

func (cp *Compiler) AddType(name, supertype string, typeNo values.ValueType) {
	cp.P.LocalConcreteTypes = cp.P.LocalConcreteTypes.Add(typeNo)
	cp.P.TypeMap[name] = values.MakeAbstractType(typeNo)
	cp.P.TypeMap[name+"?"] = values.MakeAbstractType(values.NULL, typeNo)
	types := []string{supertype}
	if supertype == "snippet" {
		types = append(types, "struct")
	}
	cp.Vm.AddTypeNumberToSharedAlternateTypes(typeNo, types...)
	types = append(types, "any")
	for _, sT := range types {
		cp.P.Common.Types[sT] = cp.P.Common.Types[sT].Insert(typeNo)
		cp.P.Common.Types[sT+"?"] = cp.P.Common.Types[sT+"?"].Insert(typeNo)
	}
}

// On the one hand, the VM must know the names of the enums and their elements so it can describe them.
// Otoh, the compiler needs to know how to turn enum literals into values.
func (cp *Compiler) createEnums() {
	for i, tokens := range cp.P.TokenizedDeclarations[enumDeclaration] {
		tokens.ToStart()
		tok1 := tokens.NextToken()
		var typeNo values.ValueType
		info, typeExists := cp.getDeclaration(decENUM, &tok1, DUMMY)
		if typeExists {
			typeNo = info.(values.ValueType)
			typeInfo := cp.Vm.concreteTypeInfo[typeNo].(enumType)
			typeInfo.path = cp.P.NamespacePath
			cp.Vm.concreteTypeInfo[typeNo] = typeInfo
		} else {
			typeNo = values.ValueType(len(cp.Vm.concreteTypeInfo))
			cp.setDeclaration(decENUM, &tok1, DUMMY, typeNo)
		}
		cp.AddType(tok1.Literal, "enum", typeNo)
		if typeExists {
			continue
		}

		tokens.NextToken() // Skip over the '='.
		tokens.NextToken() // This says 'enum' or we wouldn't be here.
		elementNameList := []string{}
		for tok := tokens.NextToken(); tok.Type != token.EOF; {
			if tok.Type != token.IDENT {
				cp.Throw("init/enum/ident", tok)
			}
			_, alreadyExists := cp.EnumElements[tok.Literal]
			if alreadyExists { // Enums in the same namespace can't have overlapping elements or we wouldn't know their type.
				cp.Throw("init/enum/element", tok)
			}

			cp.EnumElements[tok.Literal] = cp.Reserve(typeNo, len(elementNameList), &tok)
			elementNameList = append(elementNameList, tok.Literal)
			tok = tokens.NextToken()
			if tok.Type != token.COMMA && tok.Type != token.WEAK_COMMA && tok.Type != token.EOF {
				cp.Throw("init/enum/comma", tok)
			}
			tok = tokens.NextToken()
		}
		cp.Vm.concreteTypeInfo = append(cp.Vm.concreteTypeInfo, enumType{name: tok1.Literal, path: cp.P.NamespacePath, elementNames: elementNameList, private: cp.P.IsPrivate(int(enumDeclaration), i)})
	}
}

func (cp *Compiler) createClones() {
	for i, tokens := range cp.P.TokenizedDeclarations[cloneDeclaration] {
		private := cp.P.IsPrivate(int(cloneDeclaration), i)
		tokens.ToStart()
		tok1 := tokens.NextToken()
		name := tok1.Literal
		tokens.NextToken() // Skip over the '='.
		tokens.NextToken() // This says 'clone' or we wouldn't be here.
		typeToken := tokens.NextToken()
		typeToClone := typeToken.Literal
		parentTypeNo, ok := parser.ClonableTypes[typeToClone]
		if !ok {
			cp.Throw("init/clone/type", typeToken)
			return
		}
		abType := typeToClone + "like"
		var typeNo values.ValueType
		info, typeExists := cp.getDeclaration(decCLONE, &tok1, DUMMY)
		if typeExists {
			typeNo = info.(values.ValueType)
			typeInfo := cp.Vm.concreteTypeInfo[typeNo].(cloneType)
			typeInfo.path = cp.P.NamespacePath
			cp.Vm.concreteTypeInfo[typeNo] = typeInfo
		} else {
			typeNo = values.ValueType(len(cp.Vm.concreteTypeInfo))
			cp.setDeclaration(decCLONE, &tok1, DUMMY, typeNo)
			cp.Vm.concreteTypeInfo = append(cp.Vm.concreteTypeInfo, cloneType{name: name, path: cp.P.NamespacePath, parent: parentTypeNo, private: cp.P.IsPrivate(int(cloneDeclaration), i)})
			if parentTypeNo == values.LIST || parentTypeNo == values.STRING || parentTypeNo == values.SET || parentTypeNo == values.MAP {
				cp.Vm.IsRangeable = cp.Vm.IsRangeable.Union(altType(typeNo))
			}
		}
		// We make the conversion fuction.
		cp.AddType(name, abType, typeNo)
		cp.CloneNameToTypeNumber[name] = typeNo
		cp.P.AllFunctionIdents.Add(name)
		cp.P.Functions.Add(name)
		sig := ast.StringSig{ast.NameTypenamePair{"x", typeToClone}}
		fn := &ast.PrsrFunction{Sig: cp.P.MakeAbstractSigFromStringSig(sig), NameSig: sig, NameRets: sig, RtnSig: cp.P.MakeAbstractSigFromStringSig(sig), Body: &ast.BuiltInExpression{Name: name}, Number: DUMMY, Compiler: cp, Tok: &tok1}
		cp.P.FunctionTable.Add(cp.P, name, fn)
		cp.fnIndex[fnSource{cloneDeclaration, i}] = fn

		// We get the requested builtins.
		var opList []string
		usingOrEof := tokens.NextToken()
		if usingOrEof.Type != token.EOF {
			if usingOrEof.Literal != "using" {
				cp.Throw("init/clone/using", usingOrEof)
				return
			}
			for {
				op := tokens.NextToken()
				sep := tokens.NextToken()
				opList = append(opList, strings.Trim(op.Literal, "\n\r\t "))
				if sep.Type == token.EOF {
					break
				}
				if sep.Type != token.COMMA {
					cp.Throw("init/clone/comma", usingOrEof)
					break
				}
			}
		}
		if cp.P.ErrorsExist() {
			return
		}
		// And add them to the common functions.
		for _, op := range opList {
			rtnSig := ast.StringSig{{"", name}}
			switch parentTypeNo {
			case values.FLOAT:
				switch op {
				case "+":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"+", "bling"}, ast.NameTypenamePair{"y", name}}
					cp.makeCloneFunction("+", sig, "add_floats", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "-":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"-", "bling"}, ast.NameTypenamePair{"y", name}}
					cp.makeCloneFunction("-", sig, "subtract_floats", altType(typeNo), rtnSig, private, INFIX, &tok1)
					sig = ast.StringSig{ast.NameTypenamePair{"x", name}}
					cp.makeCloneFunction("-", sig, "negate_float", altType(typeNo), rtnSig, private, PREFIX, &tok1)
				case "*":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"*", "bling"}, ast.NameTypenamePair{"y", name}}
					cp.makeCloneFunction("*", sig, "multiply_floats", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "/":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"/", "bling"}, ast.NameTypenamePair{"y", name}}
					cp.makeCloneFunction("/", sig, "divide_floats", altType(typeNo), rtnSig, private, INFIX, &tok1)
				default:
					cp.Throw("init/request/float", usingOrEof, op)
				}
			case values.INT:
				switch op {
				case "+":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"+", "bling"}, ast.NameTypenamePair{"y", name}}
					cp.makeCloneFunction("+", sig, "add_integers", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "-":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"-", "bling"}, ast.NameTypenamePair{"y", name}}
					cp.makeCloneFunction("-", sig, "subtract_integers", altType(typeNo), rtnSig, private, INFIX, &tok1)
					sig = ast.StringSig{ast.NameTypenamePair{"x", name}}
					cp.makeCloneFunction("-", sig, "negate_integer", altType(typeNo), rtnSig, private, PREFIX, &tok1)
				case "*":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"*", "bling"}, ast.NameTypenamePair{"y", name}}
					cp.makeCloneFunction("*", sig, "multiply_integers", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "/":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"/", "bling"}, ast.NameTypenamePair{"y", name}}
					cp.makeCloneFunction("/", sig, "divide_integers", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "%":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"%", "bling"}, ast.NameTypenamePair{"y", name}}
					cp.makeCloneFunction("%", sig, "modulo_integers", altType(typeNo), rtnSig, private, INFIX, &tok1)
				default:
					cp.P.Throw("init/request/int", &usingOrEof, op)
				}
			case values.LIST:
				switch op {
				case "+":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"+", "bling"}, ast.NameTypenamePair{"y", name}}
					cp.makeCloneFunction("+", sig, "add_lists", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "with":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"with", "bling"}, ast.NameTypenamePair{"y", "...pair"}}
					cp.makeCloneFunction("with", sig, "list_with", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "?>":
					cloneData := cp.Vm.concreteTypeInfo[typeNo].(cloneType)
					cloneData.isFilterable = true
					cp.Vm.concreteTypeInfo[typeNo] = cloneData
				case ">>":
					cloneData := cp.Vm.concreteTypeInfo[typeNo].(cloneType)
					cloneData.isMappable = true
					cp.Vm.concreteTypeInfo[typeNo] = cloneData
				case "slice":
					cloneData := cp.Vm.concreteTypeInfo[typeNo].(cloneType)
					cloneData.isSliceable = true
					cp.Vm.concreteTypeInfo[typeNo] = cloneData
				default:
					cp.Throw("init/request/list", usingOrEof, op)
				}
			case values.MAP:
				switch op {
				case "with":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"with", "bling"}, ast.NameTypenamePair{"y", "...pair"}}
					cp.makeCloneFunction("with", sig, "map_with", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "without":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"without", "bling"}, ast.NameTypenamePair{"y", "...any?"}}
					cp.makeCloneFunction("without", sig, "map_without", altType(typeNo), rtnSig, private, INFIX, &tok1)
				default:
					cp.Throw("init/request/map", usingOrEof, op)
				}
			case values.PAIR:
				cp.Throw("init/request/pair", usingOrEof, op)
			case values.SET:
				switch op {
				case "+":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"+", "bling"}, ast.NameTypenamePair{"y", name}}
					cp.makeCloneFunction("+", sig, "add_sets", altType(typeNo), rtnSig, private, INFIX, &tok1)
				default:
					cp.Throw("init/request/set", usingOrEof, op)
				}
			case values.STRING:
				switch op {
				case "+":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"+", "bling"}, ast.NameTypenamePair{"y", name}}
					cp.makeCloneFunction("+", sig, "add_strings", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "slice":
					cloneData := cp.Vm.concreteTypeInfo[typeNo].(cloneType)
					cloneData.isSliceable = true
					cp.Vm.concreteTypeInfo[typeNo] = cloneData
				default:
					cp.Throw("init/request/string", usingOrEof, op)
				}
			}
		}
	}
	// For convenience, we give the compiler a map between types and the group of clones they belong to (no entry in the map if they're uncloneable).
	for typename := range parser.ClonableTypes {
		abType := typename + "like"
		cloneGroup := cp.Vm.sharedTypenameToTypeList[abType]
		for _, cloneTypeNo := range cloneGroup {
			cp.typeToCloneGroup[values.ValueType(cloneTypeNo.(simpleType))] = cloneGroup
		}
	}
}

func (cp *Compiler) makeCloneFunction(fnName string, sig ast.StringSig, builtinTag string, rtnTypes AlternateType, rtnSig ast.StringSig, isPrivate bool, pos uint32, tok *token.Token) {
	fn := &ast.PrsrFunction{Sig: cp.P.MakeAbstractSigFromStringSig(sig), Tok: tok, NameSig: sig, NameRets: rtnSig, RtnSig: cp.P.MakeAbstractSigFromStringSig(rtnSig), Body: &ast.BuiltInExpression{*tok, builtinTag}, Compiler: cp, Number: cp.addToBuiltins(sig, builtinTag, rtnTypes, isPrivate, tok)}
	cp.P.Common.Functions[parser.FuncSource{tok.Source, tok.Line, fnName, pos}] = fn
	if fnName == settings.FUNCTION_TO_PEEK {
		println("Making clone with sig", sig.String())
	}
	conflictingFunction := cp.P.FunctionTable.Add(cp.P, fnName, fn)
	if conflictingFunction != nil && conflictingFunction != fn {
		cp.P.Throw("init/overload/c", tok, fnName, conflictingFunction.Tok.Line)
	}
}

// We create the struct types and their field labels but we don't define the field types because we haven't defined all the types even lexically yet, let alone what they are.
func (cp *Compiler) createStructNamesAndLabels() {
	cp.structDeclarationNumberToTypeNumber = make(map[int]values.ValueType)
	for i, node := range cp.P.ParsedDeclarations[structDeclaration] {
		lhs := node.(*ast.AssignmentExpression).Left
		name := lhs.GetToken().Literal
		typeNo := values.ValueType(len(cp.Vm.concreteTypeInfo))
		typeInfo, typeExists := cp.getDeclaration(decSTRUCT, node.GetToken(), DUMMY)
		if typeExists { // We see if it's already been declared.
			typeNo = typeInfo.(structInfo).structNumber
			typeInfo := cp.Vm.concreteTypeInfo[typeNo].(structType)
			typeInfo.path = cp.P.NamespacePath
			cp.Vm.concreteTypeInfo[typeNo] = typeInfo
		} else {
			cp.setDeclaration(decSTRUCT, node.GetToken(), DUMMY, structInfo{typeNo, cp.P.IsPrivate(int(structDeclaration), i)})
		}
		cp.AddType(name, "struct", typeNo)
		cp.StructNameToTypeNumber[name] = typeNo
		if name == "Error" {
			cp.Vm.typeNumberOfUnwrappedError = typeNo // The vm needs to know this so it can convert an 'error' into an 'Error'.
		}
		// The parser needs to know about it too.
		cp.P.Functions.Add(name)
		cp.P.AllFunctionIdents.Add(name)
		sig := node.(*ast.AssignmentExpression).Right.(*ast.StructExpression).Sig
		fn := &ast.PrsrFunction{Sig: cp.P.MakeAbstractSigFromStringSig(sig), NameSig: sig, Body: &ast.BuiltInExpression{Name: name}, Number: DUMMY, Compiler: cp, Tok: node.GetToken()}
		cp.P.FunctionTable.Add(cp.P, name, fn) // TODO --- give them their own ast type?
		cp.fnIndex[fnSource{structDeclaration, i}] = fn
		// We make the labels exist, unless they already do.
		if typeExists { // Then the vm knows about it but we have to tell this compiler about it too.
			cp.structDeclarationNumberToTypeNumber[i] = typeInfo.(structInfo).structNumber
		} else { // Else we need to add the labels to the vm and cp.
			labelsForStruct := make([]int, 0, len(sig))
			for j, labelNameAndType := range sig {
				labelName := labelNameAndType.VarName
				labelLocation, alreadyExists := cp.Vm.FieldLabelsInMem[labelName]
				if alreadyExists { // Structs can of course have overlapping fields but we don't want to declare them twice.
					labelsForStruct = append(labelsForStruct, cp.Vm.Mem[labelLocation].V.(int))
					cp.setDeclaration(decLABEL, node.GetToken(), j, labelInfo{labelLocation, true}) // 'true' because we can't tell if it's private or not until we've defined all the structs.
				} else {
					cp.Vm.FieldLabelsInMem[labelName] = cp.Reserve(values.LABEL, len(cp.Vm.Labels), node.GetToken())
					cp.setDeclaration(decLABEL, node.GetToken(), j, labelInfo{cp.That(), true})
					labelsForStruct = append(labelsForStruct, len(cp.Vm.Labels))
					cp.Vm.Labels = append(cp.Vm.Labels, labelName)
					cp.Vm.LabelIsPrivate = append(cp.Vm.LabelIsPrivate, true)
				}
			}
			cp.structDeclarationNumberToTypeNumber[i] = values.ValueType(len(cp.Vm.concreteTypeInfo))
			stT := structType{name: name, path: cp.P.NamespacePath, labelNumbers: labelsForStruct, private: cp.P.IsPrivate(int(structDeclaration), i)}
			stT = stT.addLabels(labelsForStruct)
			cp.Vm.concreteTypeInfo = append(cp.Vm.concreteTypeInfo, stT)
		}
	}

	for i := range cp.P.ParsedDeclarations[structDeclaration] {
		if cp.P.IsPrivate(int(structDeclaration), i) {
			continue
		}
		tok := cp.P.ParsedDeclarations[structDeclaration][i].GetToken()
		sI, _ := cp.getDeclaration(decSTRUCT, tok, DUMMY)
		sT := cp.Vm.concreteTypeInfo[sI.(structInfo).structNumber]
		for i := range sT.(structType).labelNumbers {
			dec, _ := cp.getDeclaration(decLABEL, tok, i)
			decLabel := dec.(labelInfo)
			decLabel.private = false
			cp.setDeclaration(decLABEL, tok, i, decLabel)
			cp.Vm.LabelIsPrivate[cp.Vm.Mem[decLabel.loc].V.(int)] = false
		}
	}
}

func (cp *Compiler) createAbstractTypes() {
	for _, tcc := range cp.P.TokenizedDeclarations[abstractDeclaration] {
		tcc.ToStart()
		nameTok := tcc.NextToken()
		newTypename := nameTok.Literal
		tcc.NextToken() // The equals sign.
		tcc.NextToken() // The 'abstract' identifier.
		cp.P.TypeMap[newTypename] = values.MakeAbstractType()
		for {
			typeTok := tcc.NextToken()
			divTok := tcc.NextToken()
			if typeTok.Type != token.IDENT {
				cp.Throw("init/type/form/b", typeTok)
				break
			}
			if divTok.Type != token.EOF && !(divTok.Type == token.IDENT && divTok.Literal == "/") {
				cp.Throw("init/type/form/c", typeTok)
				break
			}
			tname := typeTok.Literal
			abTypeToAdd, ok := cp.P.TypeMap[tname]
			if !ok {
				cp.Throw("init/type/known", typeTok)
				break
			}
			cp.P.TypeMap[newTypename] = cp.P.TypeMap[newTypename].Union(abTypeToAdd)
			if divTok.Type == token.EOF {
				break
			}
		}
		cp.P.TypeMap[newTypename+"?"] = cp.P.TypeMap[newTypename].Insert(values.NULL)
		_, typeExists := cp.getDeclaration(decABSTRACT, &nameTok, DUMMY)
		if !typeExists {
			cp.setDeclaration(decABSTRACT, &nameTok, DUMMY, nil)
		}
		cp.P.Suffixes.Add(newTypename)
		cp.P.Suffixes.Add(newTypename + "?")
	}
}

func (cp *Compiler) createInterfaceTypes() {
	for _, tcc := range cp.P.TokenizedDeclarations[interfaceDeclaration] {
		tcc.ToStart()
		nameTok := tcc.NextToken()
		newTypename := nameTok.Literal
		tcc.NextToken() // The equals sign. We know this must be the case from the MakeParserAndTokenizedProgram method putting it here.
		tcc.NextToken() // The 'interface' identifier. Ditto.
		if shouldBeColon := tcc.NextToken(); shouldBeColon.Type != token.COLON {
			cp.P.Throw("init/interface/colon", &shouldBeColon)
			continue
		}
		// Now we get the signatures in the interface as a list of tokenized code chunks.
		tokenizedSigs := []*token.TokenizedCodeChunk{}
		// For consistency, an interface with just one signature can be declared as a one-liner, though you really shouldn't.
		tok := tcc.NextToken()
		beginHappened := tok.Type == token.LPAREN && tok.Literal == "|->"
		newSig := token.NewCodeChunk()
		if !beginHappened {
			newSig.Append(tok)
		}
		for {
			for {
				tok = tcc.NextToken()
				if tok.Type == token.NEWLINE || tok.Type == token.SEMICOLON || tok.Type == token.RPAREN && tok.Literal == "<-|" || tok.Type == token.EOF {
					break
				}
				newSig.Append(tok)
			}
			tokenizedSigs = append(tokenizedSigs, newSig)
			if tok.Type == token.EOF || tok.Type == token.RPAREN && tok.Literal == "<-|" || tok.Type == token.NEWLINE && !beginHappened {
				break
			}
			newSig = token.NewCodeChunk()
		}
		typeInfo := []fnSigInfo{}
		for _, sig := range tokenizedSigs {
			cp.P.TokenizedCode = sig
			lhs := sig
			astOfSig := cp.P.ParseTokenizedChunk()
			var astSig, retSig ast.StringSig
			var functionName string
			if astOfSig.GetToken().Type == token.PIPE {
				sig.ToStart()
				lhs = token.NewCodeChunk()
				for {
					tok := sig.NextToken()
					if tok.Type == token.PIPE {
						break
					}
					lhs.Append(tok)
				}
				functionName, _, astSig = cp.P.GetPartsOfSig(astOfSig.(*ast.PipingExpression).Left)
				retSig = cp.P.RecursivelySlurpReturnTypes(astOfSig.(*ast.PipingExpression).Right)
			} else {
				functionName, _, astSig = cp.P.GetPartsOfSig(astOfSig)
			}
			typeInfo = append(typeInfo, fnSigInfo{functionName, astSig, retSig})
			cp.addWordsToParser(lhs)
		}
		cp.P.TypeMap[newTypename] = values.MakeAbstractType() // We can't populate the interface types before we've parsed everything.
		cp.P.TypeMap[newTypename+"?"] = values.MakeAbstractType(values.NULL)
		_, typeExists := cp.getDeclaration(decINTERFACE, &nameTok, DUMMY)
		if !typeExists {
			cp.setDeclaration(decINTERFACE, &nameTok, DUMMY, interfaceInfo{typeInfo})
		}
		cp.P.Suffixes.Add(newTypename)
		cp.P.Suffixes.Add(newTypename + "?")
	}
}

func (cp *Compiler) addFieldsToStructs() {
	for i, node := range cp.P.ParsedDeclarations[structDeclaration] {
		structNumber := cp.structDeclarationNumberToTypeNumber[i]
		structInfo := cp.Vm.concreteTypeInfo[structNumber].(structType)
		sig := node.(*ast.AssignmentExpression).Right.(*ast.StructExpression).Sig
		typesForStruct := make([]AlternateType, 0, len(sig))
		typesForStructForVm := make([]values.AbstractType, 0, len(sig))
		for _, labelNameAndType := range sig {
			typeName := labelNameAndType.VarType
			abType := cp.P.GetAbstractType(typeName)
			typesForStructForVm = append(typesForStructForVm, abType)
			typesForStruct = append(typesForStruct, AbstractTypeToAlternateType(abType))
		}
		structInfo.alternateStructFields = typesForStruct // TODO --- even assuming we want this data duplicated, the AlternateType can't possibly be needed  at runtime and presumably belongs in a common compiler bindle.
		structInfo.abstractStructFields = typesForStructForVm
		cp.Vm.concreteTypeInfo[structNumber] = structInfo
	}
}

func (cp *Compiler) createSnippetTypesPart2() {
	abTypes := []values.AbstractType{{[]values.ValueType{values.STRING}, DUMMY}, {[]values.ValueType{values.MAP}, DUMMY}}
	altTypes := []AlternateType{altType(values.STRING), altType(values.MAP)}
	for i, name := range cp.P.Snippets {
		sig := ast.StringSig{ast.NameTypenamePair{VarName: "text", VarType: "string"}, ast.NameTypenamePair{VarName: "data", VarType: "list"}}
		typeNo := values.ValueType(len(cp.Vm.concreteTypeInfo))
		cp.P.TokenizedDeclarations[snippetDeclaration][i].ToStart()
		decTok := cp.P.TokenizedDeclarations[snippetDeclaration][i].NextToken()
		typeInfo, typeExists := cp.getDeclaration(decSTRUCT, &decTok, DUMMY)
		if typeExists { // We see if it's already been declared.
			typeNo = typeInfo.(structInfo).structNumber
			typeInfo := cp.Vm.concreteTypeInfo[typeNo].(structType)
			typeInfo.path = cp.P.NamespacePath
			cp.Vm.concreteTypeInfo[typeNo] = typeInfo
		} else {
			cp.setDeclaration(decSTRUCT, &decTok, DUMMY, structInfo{typeNo, cp.P.IsPrivate(int(snippetDeclaration), i)})
			cp.Vm.concreteTypeInfo = append(cp.Vm.concreteTypeInfo, structType{name: name, path: cp.P.NamespacePath, snippet: true, private: cp.P.IsPrivate(int(snippetDeclaration), i), abstractStructFields: abTypes, alternateStructFields: altTypes})
			cp.addStructLabelsToVm(name, typeNo, sig, &decTok)
			cp.Vm.codeGeneratingTypes.Add(typeNo)
		}
		cp.AddType(name, "snippet", typeNo)
		cp.StructNameToTypeNumber[name] = typeNo

		// The parser needs to know about it too.
		cp.P.Functions.Add(name)
		fn := &ast.PrsrFunction{Sig: cp.P.MakeAbstractSigFromStringSig(sig), NameSig: sig, Body: &ast.BuiltInExpression{Name: name, Token: decTok}, Tok: &decTok}
		cp.P.FunctionTable.Add(cp.P, name, fn)
		cp.fnIndex[fnSource{snippetDeclaration, i}] = fn
	}
}

func (cp *Compiler) checkTypesForConsistency() {
	for typeNumber := int(values.FIRST_DEFINED_TYPE); typeNumber < len(cp.Vm.concreteTypeInfo); typeNumber++ {
		if !cp.Vm.concreteTypeInfo[typeNumber].isStruct() {
			continue
		}
		if !cp.Vm.concreteTypeInfo[typeNumber].isPrivate() {
			for _, ty := range cp.Vm.concreteTypeInfo[typeNumber].(structType).abstractStructFields {
				if cp.Vm.isPrivate(ty) {
					cp.Throw("init/private/struct", token.Token{}, cp.Vm.concreteTypeInfo[typeNumber], cp.Vm.DescribeAbstractType(ty, LITERAL))
				}
			}
		}
	}

	for i, dec := range cp.P.TokenizedDeclarations[abstractDeclaration] {
		if cp.P.IsPrivate(int(abstractDeclaration), i) {
			continue
		}
		dec.ToStart()
		tok := dec.NextToken()
		name := tok.Literal
		abType := cp.P.GetAbstractType(name)
		for _, w := range abType.Types {
			if cp.Vm.concreteTypeInfo[w].isPrivate() {
				cp.Throw("init/private/abstract", tok, name)
			}
		}

	}
}

func (cp *Compiler) addStructLabelsToVm(name string, typeNo values.ValueType, sig ast.StringSig, tok *token.Token) { // TODO --- seems like we're only using this for snippets and not regular structs?
	labelsForStruct := make([]int, 0, len(sig))
	for _, labelNameAndType := range sig {
		labelName := labelNameAndType.VarName
		labelLocation, alreadyExists := cp.Vm.FieldLabelsInMem[labelName]
		if alreadyExists { // Structs can of course have overlapping fields but we don't want to declare them twice.
			labelsForStruct = append(labelsForStruct, cp.Vm.Mem[labelLocation].V.(int))
		} else {
			cp.Vm.FieldLabelsInMem[labelName] = cp.Reserve(values.LABEL, len(cp.Vm.Labels), tok)
			labelsForStruct = append(labelsForStruct, len(cp.Vm.Labels))
			cp.Vm.Labels = append(cp.Vm.Labels, labelName)
			cp.Vm.LabelIsPrivate = append(cp.Vm.LabelIsPrivate, true)
		}
	}
	typeInfo := cp.Vm.concreteTypeInfo[typeNo].(structType)
	typeInfo.labelNumbers = labelsForStruct
	typeInfo = typeInfo.addLabels(labelsForStruct)
	cp.Vm.concreteTypeInfo[typeNo] = typeInfo
}

func (cp *Compiler) compileConstructors() {
	// Struct declarations.
	for i, node := range cp.P.ParsedDeclarations[structDeclaration] {
		name := node.(*ast.AssignmentExpression).Left.GetToken().Literal // We know this and the next line are safe because we already checked in createStructs
		sig := node.(*ast.AssignmentExpression).Right.(*ast.StructExpression).Sig
		typeNo := cp.StructNameToTypeNumber[name]
		cp.fnIndex[fnSource{structDeclaration, i}].Number = cp.addToBuiltins(sig, name, altType(typeNo), cp.P.IsPrivate(int(structDeclaration), i), node.GetToken())
		cp.fnIndex[fnSource{structDeclaration, i}].Compiler = cp
	}
	// Snippets. TODO --- should this even exist? It seems like all it adds is that you could make ill-formed snippets if you chose.
	sig := ast.StringSig{ast.NameTypenamePair{VarName: "text", VarType: "string"}, ast.NameTypenamePair{VarName: "data", VarType: "list"}}
	for i, name := range cp.P.Snippets {
		typeNo := cp.StructNameToTypeNumber[name]
		cp.fnIndex[fnSource{snippetDeclaration, i}].Number = cp.addToBuiltins(sig, name, altType(typeNo), cp.P.IsPrivate(int(snippetDeclaration), i), cp.P.ParsedDeclarations[snippetDeclaration][i].GetToken())
		cp.fnIndex[fnSource{snippetDeclaration, i}].Compiler = cp
	}
	// Clones
	for i, dec := range cp.P.TokenizedDeclarations[cloneDeclaration] {
		dec.ToStart()
		nameTok := dec.NextToken()
		name := nameTok.Literal
		typeNo := cp.CloneNameToTypeNumber[name]
		sig := ast.StringSig{ast.NameTypenamePair{VarName: "x", VarType: cp.Vm.concreteTypeInfo[cp.Vm.concreteTypeInfo[typeNo].(cloneType).parent].getName(DEFAULT)}}
		cp.fnIndex[fnSource{cloneDeclaration, i}].Number = cp.addToBuiltins(sig, name, altType(typeNo), cp.P.IsPrivate(int(cloneDeclaration), i), &nameTok)
		cp.fnIndex[fnSource{cloneDeclaration, i}].Compiler = cp
	}
}

func (cp *Compiler) addToBuiltins(sig ast.StringSig, builtinTag string, returnTypes AlternateType, private bool, tok *token.Token) uint32 {
	cpF := &CpFunc{RtnTypes: returnTypes, Builtin: builtinTag}
	fnenv := NewEnvironment() // Note that we don't use this for anything, we just need some environment to pass to addVariables.
	cpF.LoReg = cp.MemTop()
	for _, pair := range sig {
		cp.AddVariable(fnenv, pair.VarName, FUNCTION_ARGUMENT, cp.TypeNameToTypeList(pair.VarType), tok)
	}
	cpF.HiReg = cp.MemTop()
	cpF.Private = private
	cp.Fns = append(cp.Fns, cpF)
	return uint32(len(cp.Fns) - 1)
}

var nativeAbstractTypes = []string{"any", "struct", "snippet"}

// The Vm doesn't *use* abstract types, but they are what values of type TYPE contain, and so it needs to be able to describe them.
func (cp *Compiler) addAbstractTypesToVm() {
	// For consistent results for tests, it is desirable that the types should be listed in a fixed order.
	keys := []string{}
	for typeName, _ := range cp.P.TypeMap {
		keys = append(keys, typeName)
	}
	for typeName, _ := range cp.P.Common.Types {
		keys = append(keys, typeName)
	}
	sort.Slice(keys, func(i, j int) bool { return keys[i] < keys[j] })
	for _, typeName := range keys {
		cp.AddTypeToVm(values.AbstractTypeInfo{typeName, cp.P.NamespacePath, cp.P.GetAbstractType(typeName)})
	}
}

// For reasons, it's a good idea to have the type info stored as an ordered list rather than a set or hashmap.
// So we need to do insertion by hand to avoid duplication.
func (cp *Compiler) AddTypeToVm(typeInfo values.AbstractTypeInfo) {
	for i, existingTypeInfo := range cp.Vm.AbstractTypes {
		if typeInfo.Name == existingTypeInfo.Name {
			if typeInfo.Path == existingTypeInfo.Path {
				return
			}
			if strings.Count(typeInfo.Path, ".") < strings.Count(existingTypeInfo.Path, ".") {
				cp.Vm.AbstractTypes[i] = typeInfo
				return
			}
			if len(typeInfo.Path) < len(existingTypeInfo.Path) {
				cp.Vm.AbstractTypes[i] = typeInfo
				return
			}
		}
	}
	cp.Vm.AbstractTypes = append(cp.Vm.AbstractTypes, typeInfo)
}

func altType(t ...values.ValueType) AlternateType {
	return AltType(t...)
}

func MakeFilepath(scriptFilepath, dir string) string {
	doctoredFilepath := strings.Clone(scriptFilepath)
	if len(scriptFilepath) >= 4 && scriptFilepath[0:4] == "hub/" {
		doctoredFilepath = filepath.Join(dir, filepath.FromSlash(scriptFilepath))
	}
	if len(scriptFilepath) >= 4 && scriptFilepath[0:4] == "rsc/" {
		doctoredFilepath = filepath.Join(dir, "source", "service", filepath.FromSlash(scriptFilepath))
	}
	if settings.StandardLibraries.Contains(scriptFilepath) {
		doctoredFilepath = dir + "lib/" + scriptFilepath
	}
	if len(scriptFilepath) >= 3 && scriptFilepath[len(scriptFilepath)-3:] != ".pf" && len(scriptFilepath) >= 4 && scriptFilepath[len(scriptFilepath)-4:] != ".hub" {
		doctoredFilepath = doctoredFilepath + ".pf"
	}
	return doctoredFilepath
}
