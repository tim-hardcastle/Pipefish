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

// Just as the initializer directs the tokenizer and the parser in the construction of the parsed code
// chunks from the tokens, so the vmMaker directs the initializer and compiler in the construction of the vmm.cp.vm.

type VmMaker struct {
	cp *Compiler
	uP *Initializer
}

// The base case: we start off with a blank vm and common parser bindle.
func StartService(scriptFilepath, dir string, db *sql.DB, hubServices map[string]*Service) (*Service, *Initializer) {
	mc := BlankVm(db, hubServices)
	common := parser.NewCommonBindle()
	cp, uP := initializeFromFilepath(mc, common, scriptFilepath, dir, "") // We pass back the uP bcause it contains the sources and/or errors (in the parser).
	result := &Service{Mc: mc, Cp: cp}
	if cp.P.ErrorsExist() {
		return result, uP
	}
	cp.makeFunctionTableAndGoMods()
	if cp.P.ErrorsExist() {
		return result, uP
	}
	cp.populateAbstractTypesAndMakeFunctionTrees()
	if cp.P.ErrorsExist() {
		return result, uP
	}
	cp.compileEverything()
	if cp.P.ErrorsExist() {
		return result, uP
	}

	cp.ResolveInterfaceBacktracks()

	mc.OwnService = result
	return result, uP
}

// Do not under any cicumstances remove the following comment.
//
//go:embed test-files/*
var testFolder embed.FS

// Then we can recurse over this, passing it the same vm every time.
// This returns a compiler and initializer and mutates the vm.
// We want the initializer back in case there are errors --- it will contain the source code and the errors in the store in its parser.
func initializeFromFilepath(mc *Vm, common *parser.CommonParserBindle, scriptFilepath, dir string, namespacePath string) (*Compiler, *Initializer) {
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
			uP := NewInitializer(common, scriptFilepath, sourcecode, dir, namespacePath) // Just because it's expecting to find errors in the uP.
			uP.Throw("init/source/a", token.Token{Source: "linking"}, scriptFilepath, err.Error())
			return nil, uP
		}
	}
	return initializeFromSourcecode(mc, common, scriptFilepath, sourcecode, dir, namespacePath)
}

func initializeFromSourcecode(mc *Vm, common *parser.CommonParserBindle, scriptFilepath, sourcecode, dir string, namespacePath string) (*Compiler, *Initializer) {
	vmm := newVmMaker(common, scriptFilepath, sourcecode, dir, mc, namespacePath)
	vmm.parseAll(scriptFilepath, sourcecode)
	vmm.cp.ScriptFilepath = scriptFilepath
	if !(scriptFilepath == "" || (len(scriptFilepath) >= 5 && scriptFilepath[0:5] == "http:")) &&
		!testing.Testing() && !(len(scriptFilepath) >= 11 && scriptFilepath[:11] == "test-files/") {
		file, err := os.Stat(MakeFilepath(scriptFilepath, dir))
		if err != nil {
			uP := NewInitializer(common, scriptFilepath, sourcecode, dir, namespacePath)
			uP.Throw("init/source/b", token.Token{Source: "linking"}, scriptFilepath)
			return nil, uP
		}
		vmm.cp.Timestamp = file.ModTime().UnixMilli()
	}
	vmm.cp.P.Common.Sources[scriptFilepath] = strings.Split(sourcecode, "\n")
	return vmm.cp, vmm.uP
}

func newVmMaker(common *parser.CommonParserBindle, scriptFilepath, sourcecode, dir string, mc *Vm, namespacePath string) *VmMaker {
	uP := NewInitializer(common, scriptFilepath, sourcecode, dir, namespacePath)
	vmm := &VmMaker{
		cp: NewCompiler(uP.Parser),
		uP: uP,
	}
	vmm.cp.ScriptFilepath = scriptFilepath
	vmm.cp.vm = mc
	vmm.cp.TupleType = vmm.cp.Reserve(values.TYPE, values.AbstractType{[]values.ValueType{values.TUPLE}, 0}, &token.Token{Source: "Builtin constant"})
	return vmm
}

func (vmm *VmMaker) cm(s string) {
	if settings.SHOW_VMM {
		println(text.UNDERLINE + s + text.RESET)
	}
}

// This does everything up to and including parsing the code chunks, and then hands back flow of control to the
// StartService or RunTest method.
func (vmm *VmMaker) parseAll(scriptFilepath, sourcecode string) {
	vmm.cm("Starting makeall for script " + scriptFilepath + ".")

	if !settings.OMIT_BUILTINS {
		vmm.cm("Adding mandatory imports to namespace.")
		vmm.uP.AddToNameSpace(settings.MandatoryImports)
	}
	if len(scriptFilepath) >= 4 && scriptFilepath[len(scriptFilepath)-4:] == ".hub" {
		vmm.cm("Adding hub.pf to hub namespace.")
		vmm.uP.AddToNameSpace([]string{"rsc/pipefish/hub.pf"})
	}
	vmm.cm("Making new relexer.")
	vmm.uP.Parser.TokenizedCode = lexer.NewRelexer(scriptFilepath, sourcecode)

	vmm.cm("Making parser and tokenized program.")
	vmm.uP.MakeParserAndTokenizedProgram()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.cm("Parsing import and external declarations.")
	vmm.uP.ParseImportsAndExternals() // That is, parse the import declarations. The files being imported are imported by the method with the long name below.
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.cm("Initializing imports.")
	unnamespacedImports := vmm.InitializeNamespacedImportsAndReturnUnnamespacedImports()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.cm("Adding unnamespaced imports to namespace.")
	vmm.uP.AddToNameSpace(unnamespacedImports)
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.cm("Initializing external services.")
	vmm.initializeExternals()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.cm("Creating enums.")
	vmm.createEnums()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.cm("Creating clone types.")
	vmm.createClones()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.cm("Creating snippet types, part 1.")
	vmm.uP.createSnippetsPart1()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.cm("Adding types to parser.")
	vmm.uP.addTypesToParser()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.cm("Adding constructors to parser, parsing struct declarations.")
	vmm.uP.addConstructorsToParserAndParseStructDeclarations()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.cm("Creating struct names and labels.")
	vmm.createStructNamesAndLabels()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.cm("Creating abstract types.")
	vmm.createAbstractTypes()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.cm("Creating (but not populating) interface types.")
	vmm.createInterfaceTypes()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.cm("Adding fields to structs.")
	vmm.addFieldsToStructs()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.cm("Creating snippet types, part 2.")
	vmm.createSnippetTypesPart2()
	if vmm.uP.ErrorsExist() {
		return
	}

	// We want to ensure that no public type (whether a struct or abstract type) contains a private type.
	vmm.cm("Checking types for consistency of encapsulation.")
	vmm.checkTypesForConsistency()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.cm("Parsing everything.")
	vmm.uP.ParseEverything()
	if vmm.uP.ErrorsExist() {
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
	cp.MakeGoMods(goHandler)

	// We add in constructors for the structs, snippets, and clones.
	cp.compileConstructors()
	if cp.P.ErrorsExist() {
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
		case *ast.GolangExpression:
			uP.Parser.GoImports[imp.Token.Source] = append(uP.Parser.GoImports[imp.Token.Source], imp.Token.Literal)
			continue
		default:
			namespace, scriptFilepath = uP.getPartsOfImportOrExternalDeclaration(imp)
		}
		if namespace == "" {
			unnamespacedImports = append(unnamespacedImports, scriptFilepath)
		}
		newCp, newUP := initializeFromFilepath(vmm.cp.vm, uP.Parser.Common, scriptFilepath, vmm.cp.P.Directory, namespace+"."+uP.Parser.NamespacePath)
		if newUP.ErrorsExist() {
			vmm.cp.Services[namespace] = &Service{vmm.cp.vm, newCp, true, false}
		} else {
			vmm.cp.Services[namespace] = &Service{vmm.cp.vm, newCp, false, false}
			for k, v := range newCp.declarationMap {
				vmm.cp.declarationMap[k] = v
			}
			vmm.cp.P.NamespaceBranch[namespace] = &parser.ParserData{newCp.P, scriptFilepath}
			newUP.Parser.Private = vmm.uP.Parser.IsPrivate(int(importDeclaration), i)
		}
	}
	return unnamespacedImports
}

// OTOH, we want the type information spread across the parsers and shared in the common parser bindle to
// collectively be the single source of truth for our type system.
// But it can't be the only *representation* of the truth, becase that would slow things down 'cos the compiler
// would have to keep converting abstract types to alternate types to build the type schemes with.
// The solution is to build the alternate type schemes once and for all from the alternate types, after we've
// entirely finished genrating the data in the parsers.
func (cp *Compiler) makeAlternateTypesFromAbstractTypes() {
	cp.typeNameToTypeScheme = make(map[string]AlternateType)
	for typename, abType := range cp.P.TypeMap {
		cp.typeNameToTypeScheme[typename] = AbstractTypeToAlternateType(abType)
	}
	for typename, abType := range cp.P.Common.Types {
		cp.typeNameToTypeScheme[typename] = AbstractTypeToAlternateType(abType)
	}
}

func (cp *Compiler) MakeGoMods(goHandler *GoHandler) {
	for source := range goHandler.Modules {
		goHandler.TypeDeclarations[source] = cp.MakeTypeDeclarationsForGo(goHandler, source)
		if cp.P.ErrorsExist() {
			return
		}
	}
	goHandler.BuildGoMods()
	if cp.P.ErrorsExist() {
		return
	}
	cp.goToPf = map[string]func(any) (uint32, []any, bool){}
	cp.pfToGo = map[string]func(uint32, []any) any{}
	for source := range goHandler.Modules {
		fnSymbol, _ := goHandler.Plugins[source].Lookup("ConvertGoStructHalfwayToPipefish")
		cp.goToPf[source] = fnSymbol.(func(any) (uint32, []any, bool))
		fnSymbol, _ = goHandler.Plugins[source].Lookup("ConvertPipefishStructToGoStruct")
		cp.pfToGo[source] = fnSymbol.(func(uint32, []any) any)
	}
	// TODO --- see if this plays nicely with function sharing and modules or if it needs more work.
	if cp.P.NamespacePath == "" {
		for k, v := range cp.P.Common.Functions {
			if v.Body.GetToken().Type == token.GOCODE {
				result := goHandler.GetFn(text.Flatten(k.FunctionName), v.Body.GetToken())
				v.Body.(*ast.GolangExpression).ObjectCode = result
			}
		}
	}
	for functionName, fns := range cp.P.FunctionTable { // TODO --- why are we doing it like this?
		for _, v := range fns {
			if v.Body.GetToken().Type == token.GOCODE {
				result := goHandler.GetFn(text.Flatten(functionName), v.Body.GetToken())
				v.Body.(*ast.GolangExpression).ObjectCode = result
			}
		}
	}
	goHandler.RecordGoTimes()
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
	goHandler := NewGoHandler(cp.P)
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
			functionToAdd := &ast.PrsrFunction{Sig: cp.P.Abstract(sig), NameSig: sig, Position: position, NameRets: rTypes, RtnSig: cp.P.Abstract(rTypes), Body: body, Given: given,
				Cmd: j == commandDeclaration, Private: cp.P.IsPrivate(int(j), i), Number: DUMMY, Compiler: cp, Tok: body.GetToken()}
			cp.fnIndex[fnSource{j, i}] = functionToAdd
			if cp.shareable(functionToAdd) || settings.MandatoryImportSet.Contains(tok.Source) {
				cp.cm("Adding "+functionName+" to common functions.", tok)
				cp.P.Common.Functions[parser.FuncSource{tok.Source, tok.Line, functionName, position}] = functionToAdd
			}
			conflictingFunction := cp.P.FunctionTable.Add(cp.P, functionName, functionToAdd)
			if conflictingFunction != nil && conflictingFunction != functionToAdd {
				cp.P.Throw("init/overload/a", body.GetToken(), functionName, functionToAdd.Sig, conflictingFunction)
				return nil
			}
			if body.GetToken().Type == token.GOCODE {
				body.(*ast.GolangExpression).Raw = []bool{}
				for i, v := range sig {
					body.(*ast.GolangExpression).Raw = append(body.(*ast.GolangExpression).Raw,
						len(v.VarType) > 4 && v.VarType[len(v.VarType)-4:] == " raw")
					if len(v.VarType) > 4 && v.VarType[len(v.VarType)-4:] == " raw" {
						sig[i].VarType = v.VarType[:len(v.VarType)-4]
					}
				}
				goHandler.MakeFunction(flatten(functionName), sig, rTypes, body.(*ast.GolangExpression), cp.P.Directory)
				if cp.P.ErrorsExist() {
					return nil
				}
				body.(*ast.GolangExpression).Sig = sig
				body.(*ast.GolangExpression).ReturnTypes = rTypes
			}
		}
	}

	// We may also have pure Go declarations:

	for _, gocode := range cp.P.TokenizedDeclarations[golangDeclaration] {
		gocode.ToStart()
		token := gocode.NextToken()
		source := token.Source
		code := token.Literal[:len(token.Literal)]
		goHandler.AddPureGoBlock(source, code)
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
					cp.P.Throw("init/overload/d", fn.pFunc.Tok, fn.name, fn.pFunc.Sig, conflictingFunction)
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
		newService, _ := StartService(path, vmm.cp.P.Directory, vmm.cp.vm.Database, vmm.cp.vm.HubServices)
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
	newCp, _ := initializeFromSourcecode(vmm.cp.vm, vmm.cp.P.Common, path, sourcecode, vmm.cp.P.Directory, name+"."+vmm.uP.Parser.NamespacePath)
	vmm.cp.P.NamespaceBranch[name] = &parser.ParserData{newCp.P, path}
	newCp.P.Private = vmm.uP.Parser.IsPrivate(int(externalDeclaration), int(externalServiceOrdinal))
	vmm.cp.Services[name] = &Service{vmm.cp.vm, newCp, newCp.P.ErrorsExist(), false}
}

func (vmm *VmMaker) AddType(name, supertype string, typeNo values.ValueType) {
	vmm.uP.Parser.LocalConcreteTypes = vmm.uP.Parser.LocalConcreteTypes.Add(typeNo)
	vmm.uP.Parser.TypeMap[name] = values.MakeAbstractType(typeNo)
	vmm.uP.Parser.TypeMap[name+"?"] = values.MakeAbstractType(values.NULL, typeNo)
	types := []string{supertype}
	if supertype == "snippet" {
		types = append(types, "struct")
	}
	vmm.cp.vm.AddTypeNumberToSharedAlternateTypes(typeNo, types...)
	types = append(types, "single")
	for _, sT := range types {
		vmm.uP.Parser.Common.Types[sT] = vmm.uP.Parser.Common.Types[sT].Insert(typeNo)
		vmm.uP.Parser.Common.Types[sT+"?"] = vmm.uP.Parser.Common.Types[sT+"?"].Insert(typeNo)
	}
}

// On the one hand, the VM must know the names of the enums and their elements so it can describe them.
// Otoh, the compiler needs to know how to turn enum literals into values.
func (vmm *VmMaker) createEnums() {
	for i, tokens := range vmm.uP.Parser.TokenizedDeclarations[enumDeclaration] {
		tokens.ToStart()
		tok1 := tokens.NextToken()
		var typeNo values.ValueType
		info, typeExists := vmm.cp.getDeclaration(decENUM, &tok1, DUMMY)
		if typeExists {
			typeNo = info.(values.ValueType)
			typeInfo := vmm.cp.vm.concreteTypes[typeNo].(enumType)
			typeInfo.path = vmm.cp.P.NamespacePath
			vmm.cp.vm.concreteTypes[typeNo] = typeInfo
		} else {
			typeNo = values.ValueType(len(vmm.cp.vm.concreteTypes))
			vmm.cp.setDeclaration(decENUM, &tok1, DUMMY, typeNo)
		}
		vmm.AddType(tok1.Literal, "enum", typeNo)
		if typeExists {
			continue
		}

		tokens.NextToken() // Skip over the '='.
		tokens.NextToken() // This says 'enum' or we wouldn't be here.
		elementNameList := []string{}
		for tok := tokens.NextToken(); tok.Type != token.EOF; {
			if tok.Type != token.IDENT {
				vmm.uP.Throw("init/enum/ident", tok)
			}
			_, alreadyExists := vmm.cp.EnumElements[tok.Literal]
			if alreadyExists { // Enums in the same namespace can't have overlapping elements or we wouldn't know their type.
				vmm.uP.Throw("init/enum/element", tok)
			}

			vmm.cp.EnumElements[tok.Literal] = vmm.cp.Reserve(typeNo, len(elementNameList), &tok)
			elementNameList = append(elementNameList, tok.Literal)
			tok = tokens.NextToken()
			if tok.Type != token.COMMA && tok.Type != token.WEAK_COMMA && tok.Type != token.EOF {
				vmm.uP.Throw("init/enum/comma", tok)
			}
			tok = tokens.NextToken()
		}
		vmm.cp.vm.concreteTypes = append(vmm.cp.vm.concreteTypes, enumType{name: tok1.Literal, path: vmm.cp.P.NamespacePath, elementNames: elementNameList, private: vmm.uP.Parser.IsPrivate(int(enumDeclaration), i)})
	}
}

func (vmm *VmMaker) createClones() {
	for i, tokens := range vmm.uP.Parser.TokenizedDeclarations[cloneDeclaration] {
		private := vmm.uP.Parser.IsPrivate(int(cloneDeclaration), i)
		tokens.ToStart()
		tok1 := tokens.NextToken()
		name := tok1.Literal
		tokens.NextToken() // Skip over the '='.
		tokens.NextToken() // This says 'clone' or we wouldn't be here.
		typeToken := tokens.NextToken()
		typeToClone := typeToken.Literal
		parentTypeNo, ok := parser.ClonableTypes[typeToClone]
		if !ok {
			vmm.uP.Throw("init/clone/type", typeToken)
			return
		}
		abType := typeToClone + "like"
		var typeNo values.ValueType
		info, typeExists := vmm.cp.getDeclaration(decCLONE, &tok1, DUMMY)
		if typeExists {
			typeNo = info.(values.ValueType)
			typeInfo := vmm.cp.vm.concreteTypes[typeNo].(cloneType)
			typeInfo.path = vmm.cp.P.NamespacePath
			vmm.cp.vm.concreteTypes[typeNo] = typeInfo
		} else {
			typeNo = values.ValueType(len(vmm.cp.vm.concreteTypes))
			vmm.cp.setDeclaration(decCLONE, &tok1, DUMMY, typeNo)
			vmm.cp.vm.concreteTypes = append(vmm.cp.vm.concreteTypes, cloneType{name: name, path: vmm.cp.P.NamespacePath, parent: parentTypeNo, private: vmm.uP.Parser.IsPrivate(int(cloneDeclaration), i)})
			if parentTypeNo == values.LIST || parentTypeNo == values.STRING || parentTypeNo == values.SET || parentTypeNo == values.MAP {
				vmm.cp.vm.IsRangeable = vmm.cp.vm.IsRangeable.Union(altType(typeNo))
			}
		}
		// We make the conversion fuction.
		vmm.AddType(name, abType, typeNo)
		vmm.cp.CloneNameToTypeNumber[name] = typeNo
		vmm.cp.P.AllFunctionIdents.Add(name)
		vmm.cp.P.Functions.Add(name)
		sig := ast.AstSig{ast.NameTypenamePair{"x", typeToClone}}
		fn := &ast.PrsrFunction{Sig: vmm.cp.P.Abstract(sig), NameSig: sig, NameRets: sig, RtnSig: vmm.cp.P.Abstract(sig), Body: &ast.BuiltInExpression{Name: name}, Number: DUMMY, Compiler: vmm.cp, Tok: &tok1}
		vmm.cp.P.FunctionTable.Add(vmm.cp.P, name, fn)
		vmm.cp.fnIndex[fnSource{cloneDeclaration, i}] = fn

		// We get the requested builtins.
		var opList []string
		usingOrEof := tokens.NextToken()
		if usingOrEof.Type != token.EOF {
			if usingOrEof.Literal != "using" {
				vmm.uP.Throw("init/clone/using", usingOrEof)
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
					vmm.uP.Throw("init/clone/comma", usingOrEof)
					break
				}
			}
		}
		if vmm.cp.P.ErrorsExist() {
			return
		}
		// And add them to the common functions.
		for _, op := range opList {
			rtnSig := ast.AstSig{{"*dummy*", name}}
			switch parentTypeNo {
			case values.FLOAT:
				switch op {
				case "+":
					sig := ast.AstSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"+", "bling"}, ast.NameTypenamePair{"y", name}}
					vmm.makeCloneFunction("+", sig, "add_floats", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "-":
					sig := ast.AstSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"-", "bling"}, ast.NameTypenamePair{"y", name}}
					vmm.makeCloneFunction("-", sig, "subtract_floats", altType(typeNo), rtnSig, private, INFIX, &tok1)
					sig = ast.AstSig{ast.NameTypenamePair{"x", name}}
					vmm.makeCloneFunction("-", sig, "negate_float", altType(typeNo), rtnSig, private, PREFIX, &tok1)
				case "*":
					sig := ast.AstSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"*", "bling"}, ast.NameTypenamePair{"y", name}}
					vmm.makeCloneFunction("*", sig, "multiply_floats", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "/":
					sig := ast.AstSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"/", "bling"}, ast.NameTypenamePair{"y", name}}
					vmm.makeCloneFunction("/", sig, "divide_floats", altType(typeNo), rtnSig, private, INFIX, &tok1)
				default:
					vmm.uP.Throw("init/request/float", usingOrEof, op)
				}
			case values.INT:
				switch op {
				case "+":
					sig := ast.AstSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"+", "bling"}, ast.NameTypenamePair{"y", name}}
					vmm.makeCloneFunction("+", sig, "add_integers", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "-":
					sig := ast.AstSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"-", "bling"}, ast.NameTypenamePair{"y", name}}
					vmm.makeCloneFunction("-", sig, "subtract_integers", altType(typeNo), rtnSig, private, INFIX, &tok1)
					sig = ast.AstSig{ast.NameTypenamePair{"x", name}}
					vmm.makeCloneFunction("-", sig, "negate_integer", altType(typeNo), rtnSig, private, PREFIX, &tok1)
				case "*":
					sig := ast.AstSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"*", "bling"}, ast.NameTypenamePair{"y", name}}
					vmm.makeCloneFunction("*", sig, "multiply_integers", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "/":
					sig := ast.AstSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"/", "bling"}, ast.NameTypenamePair{"y", name}}
					vmm.makeCloneFunction("/", sig, "divide_integers", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "%":
					sig := ast.AstSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"%", "bling"}, ast.NameTypenamePair{"y", name}}
					vmm.makeCloneFunction("%", sig, "modulo_integers", altType(typeNo), rtnSig, private, INFIX, &tok1)
				default:
					vmm.cp.P.Throw("init/request/int", &usingOrEof, op)
				}
			case values.LIST:
				switch op {
				case "+":
					sig := ast.AstSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"+", "bling"}, ast.NameTypenamePair{"y", name}}
					vmm.makeCloneFunction("+", sig, "add_lists", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "with":
					sig := ast.AstSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"with", "bling"}, ast.NameTypenamePair{"y", "...pair"}}
					vmm.makeCloneFunction("with", sig, "list_with", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "?>":
					cloneData := vmm.cp.vm.concreteTypes[typeNo].(cloneType)
					cloneData.isFilterable = true
					vmm.cp.vm.concreteTypes[typeNo] = cloneData
				case ">>":
					cloneData := vmm.cp.vm.concreteTypes[typeNo].(cloneType)
					cloneData.isMappable = true
					vmm.cp.vm.concreteTypes[typeNo] = cloneData
				case "slice":
					cloneData := vmm.cp.vm.concreteTypes[typeNo].(cloneType)
					cloneData.isSliceable = true
					vmm.cp.vm.concreteTypes[typeNo] = cloneData
				default:
					vmm.uP.Throw("init/request/list", usingOrEof, op)
				}
			case values.MAP:
				switch op {
				case "with":
					sig := ast.AstSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"with", "bling"}, ast.NameTypenamePair{"y", "...pair"}}
					vmm.makeCloneFunction("with", sig, "map_with", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "without":
					sig := ast.AstSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"without", "bling"}, ast.NameTypenamePair{"y", "...single?"}}
					vmm.makeCloneFunction("without", sig, "map_without", altType(typeNo), rtnSig, private, INFIX, &tok1)
				default:
					vmm.uP.Throw("init/request/map", usingOrEof, op)
				}
			case values.PAIR:
				vmm.uP.Throw("init/request/pair", usingOrEof, op)
			case values.SET:
				switch op {
				case "+":
					sig := ast.AstSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"+", "bling"}, ast.NameTypenamePair{"y", name}}
					vmm.makeCloneFunction("+", sig, "add_sets", altType(typeNo), rtnSig, private, INFIX, &tok1)
				default:
					vmm.uP.Throw("init/request/set", usingOrEof, op)
				}
			case values.STRING:
				switch op {
				case "+":
					sig := ast.AstSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"+", "bling"}, ast.NameTypenamePair{"y", name}}
					vmm.makeCloneFunction("+", sig, "add_strings", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "slice":
					cloneData := vmm.cp.vm.concreteTypes[typeNo].(cloneType)
					cloneData.isSliceable = true
					vmm.cp.vm.concreteTypes[typeNo] = cloneData
				default:
					vmm.uP.Throw("init/request/string", usingOrEof, op)
				}
			}
		}
	}
	// For convenience, we give the compiler a map between types and the group of clones they belong to (no entry in the map if they're uncloneable).
	for typename := range parser.ClonableTypes {
		abType := typename + "like"
		cloneGroup := vmm.cp.vm.sharedTypenameToTypeList[abType]
		for _, cloneTypeNo := range cloneGroup {
			vmm.cp.typeToCloneGroup[values.ValueType(cloneTypeNo.(simpleType))] = cloneGroup
		}
	}
}

func (vmm *VmMaker) makeCloneFunction(fnName string, sig ast.AstSig, builtinTag string, rtnTypes AlternateType, rtnSig ast.AstSig, isPrivate bool, pos uint32, tok *token.Token) {
	fn := &ast.PrsrFunction{Sig: vmm.cp.P.Abstract(sig), Tok: tok, NameSig: sig, NameRets: rtnSig, RtnSig: vmm.cp.P.Abstract(rtnSig), Body: &ast.BuiltInExpression{*tok, builtinTag}, Compiler: vmm.cp, Number: vmm.cp.addToBuiltins(sig, builtinTag, rtnTypes, isPrivate, tok)}
	vmm.cp.P.Common.Functions[parser.FuncSource{tok.Source, tok.Line, fnName, pos}] = fn
	if fnName == settings.FUNCTION_TO_PEEK {
		println("Making clone with sig", sig.String())
	}
	conflictingFunction := vmm.cp.P.FunctionTable.Add(vmm.cp.P, fnName, fn)
	if conflictingFunction != nil && conflictingFunction != fn {
		vmm.cp.P.Throw("init/overload/b", tok, fnName, fn.Sig, conflictingFunction)
	}
}

// We create the struct types and their field labels but we don't define the field types because we haven't defined all the types even lexically yet, let alone what they are.
func (vmm *VmMaker) createStructNamesAndLabels() {
	vmm.cp.structDeclarationNumberToTypeNumber = make(map[int]values.ValueType)
	for i, node := range vmm.uP.Parser.ParsedDeclarations[structDeclaration] {
		lhs := node.(*ast.AssignmentExpression).Left
		name := lhs.GetToken().Literal
		typeNo := values.ValueType(len(vmm.cp.vm.concreteTypes))
		typeInfo, typeExists := vmm.cp.getDeclaration(decSTRUCT, node.GetToken(), DUMMY)
		if typeExists { // We see if it's already been declared.
			typeNo = typeInfo.(structInfo).structNumber
			typeInfo := vmm.cp.vm.concreteTypes[typeNo].(structType)
			typeInfo.path = vmm.cp.P.NamespacePath
			vmm.cp.vm.concreteTypes[typeNo] = typeInfo
		} else {
			vmm.cp.setDeclaration(decSTRUCT, node.GetToken(), DUMMY, structInfo{typeNo, vmm.uP.Parser.IsPrivate(int(structDeclaration), i)})
		}
		vmm.AddType(name, "struct", typeNo)
		vmm.cp.StructNameToTypeNumber[name] = typeNo
		if name == "Error" {
			vmm.cp.vm.typeNumberOfUnwrappedError = typeNo // The vm needs to know this so it can convert an 'error' into an 'Error'.
		}
		// The parser needs to know about it too.
		vmm.cp.P.Functions.Add(name)
		vmm.cp.P.AllFunctionIdents.Add(name)
		sig := node.(*ast.AssignmentExpression).Right.(*ast.StructExpression).Sig
		fn := &ast.PrsrFunction{Sig: vmm.cp.P.Abstract(sig), NameSig: sig, Body: &ast.BuiltInExpression{Name: name}, Number: DUMMY, Compiler: vmm.cp, Tok: node.GetToken()}
		vmm.cp.P.FunctionTable.Add(vmm.cp.P, name, fn) // TODO --- give them their own ast type?
		vmm.cp.fnIndex[fnSource{structDeclaration, i}] = fn
		// We make the labels exist, unless they already do.
		if typeExists { // Then the vm knows about it but we have to tell this compiler about it too.
			vmm.cp.structDeclarationNumberToTypeNumber[i] = typeInfo.(structInfo).structNumber
		} else { // Else we need to add the labels to the vm and vmm.
			labelsForStruct := make([]int, 0, len(sig))
			for j, labelNameAndType := range sig {
				labelName := labelNameAndType.VarName
				labelLocation, alreadyExists := vmm.cp.vm.FieldLabelsInMem[labelName]
				if alreadyExists { // Structs can of course have overlapping fields but we don't want to declare them twice.
					labelsForStruct = append(labelsForStruct, vmm.cp.vm.Mem[labelLocation].V.(int))
					vmm.cp.setDeclaration(decLABEL, node.GetToken(), j, labelInfo{labelLocation, true}) // 'true' because we can't tell if it's private or not until we've defined all the structs.
				} else {
					vmm.cp.vm.FieldLabelsInMem[labelName] = vmm.cp.Reserve(values.LABEL, len(vmm.cp.vm.Labels), node.GetToken())
					vmm.cp.setDeclaration(decLABEL, node.GetToken(), j, labelInfo{vmm.cp.That(), true})
					labelsForStruct = append(labelsForStruct, len(vmm.cp.vm.Labels))
					vmm.cp.vm.Labels = append(vmm.cp.vm.Labels, labelName)
					vmm.cp.vm.LabelIsPrivate = append(vmm.cp.vm.LabelIsPrivate, true)
				}
			}
			vmm.cp.structDeclarationNumberToTypeNumber[i] = values.ValueType(len(vmm.cp.vm.concreteTypes))
			stT := structType{name: name, path: vmm.cp.P.NamespacePath, labelNumbers: labelsForStruct, private: vmm.uP.Parser.IsPrivate(int(structDeclaration), i)}
			stT = stT.addLabels(labelsForStruct)
			vmm.cp.vm.concreteTypes = append(vmm.cp.vm.concreteTypes, stT)
		}
	}

	for i := range vmm.uP.Parser.ParsedDeclarations[structDeclaration] {
		if vmm.uP.Parser.IsPrivate(int(structDeclaration), i) {
			continue
		}
		tok := vmm.uP.Parser.ParsedDeclarations[structDeclaration][i].GetToken()
		sI, _ := vmm.cp.getDeclaration(decSTRUCT, tok, DUMMY)
		sT := vmm.cp.vm.concreteTypes[sI.(structInfo).structNumber]
		for i := range sT.(structType).labelNumbers {
			dec, _ := vmm.cp.getDeclaration(decLABEL, tok, i)
			decLabel := dec.(labelInfo)
			decLabel.private = false
			vmm.cp.setDeclaration(decLABEL, tok, i, decLabel)
			vmm.cp.vm.LabelIsPrivate[vmm.cp.vm.Mem[decLabel.loc].V.(int)] = false
		}
	}
}

func (vmm *VmMaker) createAbstractTypes() {
	for _, tcc := range vmm.uP.Parser.TokenizedDeclarations[abstractDeclaration] {
		tcc.ToStart()
		nameTok := tcc.NextToken()
		newTypename := nameTok.Literal
		tcc.NextToken() // The equals sign.
		tcc.NextToken() // The 'abstract' identifier.
		vmm.cp.P.TypeMap[newTypename] = values.MakeAbstractType()
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
			abTypeToAdd, ok := vmm.cp.P.TypeMap[tname]
			if !ok {
				vmm.uP.Throw("init/type/known", typeTok)
				break
			}
			vmm.cp.P.TypeMap[newTypename] = vmm.cp.P.TypeMap[newTypename].Union(abTypeToAdd)
			if divTok.Type == token.EOF {
				break
			}
		}
		vmm.cp.P.TypeMap[newTypename+"?"] = vmm.cp.P.TypeMap[newTypename].Insert(values.NULL)
		_, typeExists := vmm.cp.getDeclaration(decABSTRACT, &nameTok, DUMMY)
		if !typeExists {
			vmm.cp.setDeclaration(decABSTRACT, &nameTok, DUMMY, nil)
		}
		vmm.uP.Parser.Suffixes.Add(newTypename)
		vmm.uP.Parser.Suffixes.Add(newTypename + "?")
	}
}

func (vmm *VmMaker) createInterfaceTypes() {
	for _, tcc := range vmm.uP.Parser.TokenizedDeclarations[interfaceDeclaration] {
		tcc.ToStart()
		nameTok := tcc.NextToken()
		newTypename := nameTok.Literal
		tcc.NextToken() // The equals sign. We know this must be the case from the MakeParserAndTokenizedProgram method putting it here.
		tcc.NextToken() // The 'interface' identifier. Ditto.
		if shouldBeColon := tcc.NextToken(); shouldBeColon.Type != token.COLON {
			vmm.cp.P.Throw("init/interface/colon", &shouldBeColon)
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
			vmm.uP.Parser.TokenizedCode = sig
			lhs := sig
			astOfSig := vmm.uP.Parser.ParseTokenizedChunk()
			var astSig, retSig ast.AstSig
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
				functionName, _, astSig = vmm.uP.Parser.GetPartsOfSig(astOfSig.(*ast.PipingExpression).Left)
				retSig = vmm.uP.Parser.RecursivelySlurpReturnTypes(astOfSig.(*ast.PipingExpression).Right)
			} else {
				functionName, _, astSig = vmm.uP.Parser.GetPartsOfSig(astOfSig)
			}
			typeInfo = append(typeInfo, fnSigInfo{functionName, astSig, retSig})
			vmm.uP.addWordsToParser(lhs)
		}
		vmm.cp.P.TypeMap[newTypename] = values.MakeAbstractType() // We can't populate the interface types before we've parsed everything.
		vmm.cp.P.TypeMap[newTypename+"?"] = values.MakeAbstractType(values.NULL)
		_, typeExists := vmm.cp.getDeclaration(decINTERFACE, &nameTok, DUMMY)
		if !typeExists {
			vmm.cp.setDeclaration(decINTERFACE, &nameTok, DUMMY, interfaceInfo{typeInfo})
		}
		vmm.uP.Parser.Suffixes.Add(newTypename)
		vmm.uP.Parser.Suffixes.Add(newTypename + "?")
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
			typeName := labelNameAndType.VarType
			abType := vmm.cp.P.GetAbstractType(typeName)
			typesForStructForVm = append(typesForStructForVm, abType)
			typesForStruct = append(typesForStruct, AbstractTypeToAlternateType(abType))
		}
		structInfo.alternateStructFields = typesForStruct // TODO --- even assuming we want this data duplicated, the AlternateType can't possibly be needed  at runtime and presumably belongs in a common compiler bindle.
		structInfo.abstractStructFields = typesForStructForVm
		vmm.cp.vm.concreteTypes[structNumber] = structInfo
	}
}

func (vmm *VmMaker) createSnippetTypesPart2() {
	abTypes := []values.AbstractType{{[]values.ValueType{values.STRING}, DUMMY}, {[]values.ValueType{values.MAP}, DUMMY}}
	altTypes := []AlternateType{altType(values.STRING), altType(values.MAP)}
	for i, name := range vmm.cp.P.Snippets {
		sig := ast.AstSig{ast.NameTypenamePair{VarName: "text", VarType: "string"}, ast.NameTypenamePair{VarName: "data", VarType: "list"}}
		typeNo := values.ValueType(len(vmm.cp.vm.concreteTypes))
		vmm.cp.P.TokenizedDeclarations[snippetDeclaration][i].ToStart()
		decTok := vmm.cp.P.TokenizedDeclarations[snippetDeclaration][i].NextToken()
		typeInfo, typeExists := vmm.cp.getDeclaration(decSTRUCT, &decTok, DUMMY)
		if typeExists { // We see if it's already been declared.
			typeNo = typeInfo.(structInfo).structNumber
			typeInfo := vmm.cp.vm.concreteTypes[typeNo].(structType)
			typeInfo.path = vmm.cp.P.NamespacePath
			vmm.cp.vm.concreteTypes[typeNo] = typeInfo
		} else {
			vmm.cp.setDeclaration(decSTRUCT, &decTok, DUMMY, structInfo{typeNo, vmm.uP.Parser.IsPrivate(int(snippetDeclaration), i)})
			vmm.cp.vm.concreteTypes = append(vmm.cp.vm.concreteTypes, structType{name: name, path: vmm.cp.P.NamespacePath, snippet: true, private: vmm.uP.Parser.IsPrivate(int(snippetDeclaration), i), abstractStructFields: abTypes, alternateStructFields: altTypes})
			vmm.addStructLabelsToVm(name, typeNo, sig, &decTok)
			vmm.cp.vm.codeGeneratingTypes.Add(typeNo)
		}
		vmm.AddType(name, "snippet", typeNo)
		vmm.cp.StructNameToTypeNumber[name] = typeNo

		// The parser needs to know about it too.
		vmm.uP.Parser.Functions.Add(name)
		fn := &ast.PrsrFunction{Sig: vmm.cp.P.Abstract(sig), NameSig: sig, Body: &ast.BuiltInExpression{Name: name, Token: decTok}, Tok: &decTok}
		vmm.cp.P.FunctionTable.Add(vmm.cp.P, name, fn)
		vmm.cp.fnIndex[fnSource{snippetDeclaration, i}] = fn
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
					vmm.uP.Throw("init/private/struct", token.Token{}, vmm.cp.vm.concreteTypes[typeNumber], vmm.cp.vm.DescribeAbstractType(ty, LITERAL))
				}
			}
		}
	}

	for i, dec := range vmm.uP.Parser.TokenizedDeclarations[abstractDeclaration] {
		if vmm.uP.Parser.IsPrivate(int(abstractDeclaration), i) {
			continue
		}
		dec.ToStart()
		tok := dec.NextToken()
		name := tok.Literal
		abType := vmm.cp.P.GetAbstractType(name)
		for _, w := range abType.Types {
			if vmm.cp.vm.concreteTypes[w].isPrivate() {
				vmm.uP.Throw("init/private/abstract", tok, name)
			}
		}

	}
}

func (vmm *VmMaker) addStructLabelsToVm(name string, typeNo values.ValueType, sig ast.AstSig, tok *token.Token) { // TODO --- seems like we're only using this for snippets and not regular structs?
	labelsForStruct := make([]int, 0, len(sig))
	for _, labelNameAndType := range sig {
		labelName := labelNameAndType.VarName
		labelLocation, alreadyExists := vmm.cp.vm.FieldLabelsInMem[labelName]
		if alreadyExists { // Structs can of course have overlapping fields but we don't want to declare them twice.
			labelsForStruct = append(labelsForStruct, vmm.cp.vm.Mem[labelLocation].V.(int))
		} else {
			vmm.cp.vm.FieldLabelsInMem[labelName] = vmm.cp.Reserve(values.LABEL, len(vmm.cp.vm.Labels), tok)
			labelsForStruct = append(labelsForStruct, len(vmm.cp.vm.Labels))
			vmm.cp.vm.Labels = append(vmm.cp.vm.Labels, labelName)
			vmm.cp.vm.LabelIsPrivate = append(vmm.cp.vm.LabelIsPrivate, true)
		}
	}
	typeInfo := vmm.cp.vm.concreteTypes[typeNo].(structType)
	typeInfo.labelNumbers = labelsForStruct
	typeInfo = typeInfo.addLabels(labelsForStruct)
	vmm.cp.vm.concreteTypes[typeNo] = typeInfo
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
	sig := ast.AstSig{ast.NameTypenamePair{VarName: "text", VarType: "string"}, ast.NameTypenamePair{VarName: "data", VarType: "list"}}
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
		sig := ast.AstSig{ast.NameTypenamePair{VarName: "x", VarType: cp.vm.concreteTypes[cp.vm.concreteTypes[typeNo].(cloneType).parent].getName(DEFAULT)}}
		cp.fnIndex[fnSource{cloneDeclaration, i}].Number = cp.addToBuiltins(sig, name, altType(typeNo), cp.P.IsPrivate(int(cloneDeclaration), i), &nameTok)
		cp.fnIndex[fnSource{cloneDeclaration, i}].Compiler = cp
	}
}

func (cp *Compiler) addToBuiltins(sig ast.AstSig, builtinTag string, returnTypes AlternateType, private bool, tok *token.Token) uint32 {
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

var nativeAbstractTypes = []string{"single", "struct", "snippet"}

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
	for i, existingTypeInfo := range cp.vm.AbstractTypes {
		if typeInfo.Name == existingTypeInfo.Name {
			if typeInfo.Path == existingTypeInfo.Path {
				return
			}
			if strings.Count(typeInfo.Path, ".") < strings.Count(existingTypeInfo.Path, ".") {
				cp.vm.AbstractTypes[i] = typeInfo
				return
			}
			if len(typeInfo.Path) < len(existingTypeInfo.Path) {
				cp.vm.AbstractTypes[i] = typeInfo
				return
			}
		}
	}
	cp.vm.AbstractTypes = append(cp.vm.AbstractTypes, typeInfo)
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
