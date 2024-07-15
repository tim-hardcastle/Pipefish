package service

import (
	"os"
	"pipefish/source/ast"
	"pipefish/source/dtypes"
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
	goToPf map[string]func(any) (uint32, []any, bool) // Used for Golang interop
	pfToGo map[string]func(uint32, []any) any         //           "                                     // We want to assign a new number to each function as we compile it.
}

// The base case: we start off with a blank vm.
func StartService(scriptFilepath, dir string, db *sql.DB, hubServices map[string]*VmService) (*VmService, *Initializer) {
	mc := BlankVm(db, hubServices)
	cp, uP := initializeFromFilepath(mc, scriptFilepath, dir) // We pass back the uP bcause it contains the sources and/or errors (in the parser).
	result := &VmService{Mc: mc, Cp: cp}
	mc.OwnService = result
	return result, uP
}

// Then we can recurse over this, passing it the same vm every time.
// This returns a compiler and initializer and mutates the vm.
// We want the initializer back in case there are errors --- it will contain the source code and the errors in the store in its parser.
func initializeFromFilepath(mc *Vm, scriptFilepath, dir string) (*Compiler, *Initializer) {
	if len(scriptFilepath) >= 4 && (scriptFilepath[0:4] == "lib/" || scriptFilepath[0:4] == "rsc/") {
		scriptFilepath = dir + scriptFilepath
	}
	sourcecode := ""
	if scriptFilepath != "" { // In which case we're making a blank VM.
		sourcebytes, err := os.ReadFile(scriptFilepath)
		sourcecode = string(sourcebytes) + "\n"
		if err != nil {
			uP := NewInitializer(scriptFilepath, sourcecode, dir)
			uP.Throw("init/source/a", token.Token{Source: "linking"}, scriptFilepath)
			return nil, uP
		}
	}
	return initializeFromSourcecode(mc, scriptFilepath, sourcecode, dir)
}

func initializeFromSourcecode(mc *Vm, scriptFilepath, sourcecode, dir string) (*Compiler, *Initializer) {
	vmm := newVmMaker(scriptFilepath, sourcecode, dir, mc)
	vmm.makeAll(scriptFilepath, sourcecode)
	vmm.cp.ScriptFilepath = scriptFilepath
	if !(scriptFilepath == "" || (len(scriptFilepath) >= 5 && scriptFilepath[0:5] == "http:")) {
		file, err := os.Stat(scriptFilepath)
		if err != nil {
			uP := NewInitializer(scriptFilepath, sourcecode, dir)
			uP.Throw("init/source/b", token.Token{Source: "linking"}, scriptFilepath)
			return nil, uP
		}
		vmm.cp.Timestamp = file.ModTime().UnixMilli()
	}
	return vmm.cp, vmm.uP
}

func newVmMaker(scriptFilepath, sourcecode, dir string, mc *Vm) *VmMaker {
	uP := NewInitializer(scriptFilepath, sourcecode, dir)
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

	// But not the constructors, which come later.
	vmm.createStructNamesAndLabels()
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

	vmm.createSnippetTypes(&token.Token{Source: scriptFilepath}) // We pass the token so the cp.cm method knows to ignore boilerplate.
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

	vmm.cp.TypeNameToTypeList["tuple"] = vmm.cp.AnyTuple

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
	vmm.compileConstructors()
	if vmm.uP.ErrorsExist() {
		return
	}

	// We do a topological sort on the commands, functions, variable and constant declarations and then declare them in the right order.
	vmm.compileEverything()
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
		newCp, newUP := initializeFromFilepath(vmm.cp.vm, scriptFilepath, vmm.cp.P.Directory)
		if len(scriptFilepath) >= 4 && (scriptFilepath[0:4] == "lib/" || scriptFilepath[0:4] == "rsc/") {
			scriptFilepath = uP.Parser.Directory + scriptFilepath
		}
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

type labeledParsedCodeChunk struct {
	chunk     ast.Node
	decType   declarationType
	decNumber int
}

// Now we need to do a big topological sort on everything, according to the following rules:
// A function, variable or constant can't depend on a command.
// A constant can't depend on a variable.
// A variable or constant can't depend on itself.
func (vmm *VmMaker) compileEverything() [][]labeledParsedCodeChunk {
	vmm.cp.GlobalVars.Ext = vmm.cp.GlobalConsts
	namesToDeclarations := map[string][]labeledParsedCodeChunk{}
	result := [][]labeledParsedCodeChunk{}
	for dT := constantDeclaration; dT <= variableDeclaration; dT++ {
		for i, dec := range vmm.cp.P.ParsedDeclarations[dT] {
			names := vmm.cp.P.GetVariablesFromSig(dec.(*ast.AssignmentExpression).Left)
			for _, name := range names {
				existingName, alreadyExists := namesToDeclarations[name]
				if alreadyExists {
					vmm.cp.P.Throw("init/name/exists/a", dec.GetToken(), vmm.cp.P.ParsedDeclarations[existingName[0].decType][existingName[0].decNumber].GetToken(), name)
					return nil
				}
				namesToDeclarations[name] = []labeledParsedCodeChunk{{dec, dT, i}}
			}
		}
	}
	for dT := functionDeclaration; dT <= commandDeclaration; dT++ {
		for i, dec := range vmm.cp.P.ParsedDeclarations[dT] {
			name, _, _, _, _, _ := vmm.cp.GetParser().ExtractPartsOfFunction(dec) // TODO --- refactor ExtractPartsOfFunction so there's a thing called ExtractNameOfFunction which you can call there and here.
			_, alreadyExists := namesToDeclarations[name]
			if alreadyExists {
				names := namesToDeclarations[name]
				for _, existingName := range names {
					if existingName.decType == variableDeclaration || existingName.decType == constantDeclaration { // We can't redeclare variables or constants.
						vmm.cp.P.Throw("init/name/exists/b", dec.GetToken(), vmm.cp.P.ParsedDeclarations[existingName.decType][existingName.decNumber].GetToken(), name)
					}
					if existingName.decType == functionDeclaration && dT == commandDeclaration { // We don't want to overload anything so it can be a command or a function 'cos that would be weird.
						vmm.cp.P.Throw("init/name/exists/c", dec.GetToken(), vmm.cp.P.ParsedDeclarations[existingName.decType][existingName.decNumber].GetToken(), name)
					}
				}
				namesToDeclarations[name] = append(names, labeledParsedCodeChunk{dec, dT, i})
			} else {
				namesToDeclarations[name] = []labeledParsedCodeChunk{{dec, dT, i}}
			}
		}
	}
	graph := dtypes.Digraph[string]{}
	for name, decs := range namesToDeclarations { // The same name may be used for different overloaded functions.
		graph.Add(name, []string{})
		for _, dec := range decs {
			rhsNames := vmm.extractNamesFromCodeChunk(dec)
			// IMPORTANT NOTE. 'extractNamesFromCodeChunk' will also slurp up a lot of cruft: type names, for example; bling; local true variables in cmds.
			// So we do nothing to throw an error if a name doesn't exist. That will happen when we try to compile the function. What we're trying to
			// do here is establish the relationship between the comds/defs/vars/consts that *do* exist.
			for rhsName := range rhsNames {
				rhsDecs, ok := namesToDeclarations[rhsName]
				if ok { // Again, we don't care if 'ok' is 'false', just about the relationships between the declarations if it's true.
					if dec.decType != commandDeclaration {
						// We check for forbidden relationships.
						for _, rhsDec := range rhsDecs {
							if rhsDec.decType == commandDeclaration {
								vmm.cp.P.Throw("init/depend/cmd", dec.chunk.GetToken())
								return nil
							}
							if rhsDec.decType == variableDeclaration {
								vmm.cp.P.Throw("init/depend/const/var", dec.chunk.GetToken())
								return nil
							}
						}
					}
					// And if there are no forbidden relationships we can add the dependency to the graph.
					graph.AddTransitiveArrow(name, rhsName)
				}
			}
		}
	}

	order := graph.Tarjan()

	// We now have a list of lists of names to declare. We're off to the races!
	for _, namesToDeclare := range order { // 'namesToDeclare' is one Tarjan partition.
		groupOfDeclarations := []labeledParsedCodeChunk{}
		for _, nameToDeclare := range namesToDeclare {
			for _, dec := range namesToDeclarations[nameToDeclare] {
				groupOfDeclarations = append(groupOfDeclarations, dec)
			}
		}
		// If the declaration type is constant or variable it must be the only member of its Tarjan partion and there must only be one thing of that name.
		if groupOfDeclarations[0].decType == constantDeclaration || groupOfDeclarations[0].decType == variableDeclaration {
			vmm.compileGlobalConstantOrVariable(groupOfDeclarations[0].decType, groupOfDeclarations[0].decNumber)
			continue
		}
		// So we have a group of functions/commands (but not both) which need to be declared together because either they have the same name or they
		// have a recursive relationship, or both.
		// We can't tell before we compile the group whether there is a recursive relationship in there, because we don't know how the dispatch is going to
		// shake out. E.g. suppose we have a type 'Money = struct(dollars, cents int)' and we wish to implement '+'. We will of course do it using '+' for ints.
		// This will not be recursion, but before we get that far we won't be able to tell whether it is or not.
		vmm.cp.recursionStore = []bkRecursion{} // The compiler will put all the places it needs to backtrack for recursion here.
		fCount := uint32(len(vmm.cp.Fns))       // We can give the function data in the parser the right numbers for the group of functions in the parser before compiling them, since we know what order they come in.
		for _, dec := range groupOfDeclarations {
			vmm.uP.fnIndex[fnSource{dec.decType, dec.decNumber}].Number = fCount
			fCount++
		}
		for _, dec := range groupOfDeclarations {
			switch dec.decType {
			case functionDeclaration:
				vmm.compileFunction(vmm.cp.P.ParsedDeclarations[functionDeclaration][dec.decNumber], vmm.uP.isPrivate(int(dec.decType), dec.decNumber), vmm.cp.GlobalConsts, functionDeclaration)
			case commandDeclaration:
				vmm.compileFunction(vmm.cp.P.ParsedDeclarations[commandDeclaration][dec.decNumber], vmm.uP.isPrivate(int(dec.decType), dec.decNumber), vmm.cp.GlobalVars, commandDeclaration)
			}
			vmm.uP.fnIndex[fnSource{dec.decType, dec.decNumber}].Number = uint32(len(vmm.cp.Fns) - 1)
		}
		// We've reached the end of the group and can go back and put the recursion in.
		for _, rDat := range vmm.cp.recursionStore {
			funcNumber := rDat.functionNumber
			addr := rDat.address
			vmm.cp.vm.Code[addr].Args[0] = vmm.cp.Fns[funcNumber].CallTo
			vmm.cp.vm.Code[addr].Args[1] = vmm.cp.Fns[funcNumber].LoReg
			vmm.cp.vm.Code[addr].Args[2] = vmm.cp.Fns[funcNumber].HiReg
			vmm.cp.vm.Code[addr+2].Args[1] = vmm.cp.Fns[funcNumber].OutReg
		}
	}
	return result
}

// This is a fairly crude way of slurping the names of functions, commands, constants, and variables out of a declaration.
// It is crude in that it will slurp other things too: type names, for example; bling; local true variables in cmds. We can live
// with the false positives so long as there are no false negatives.
func (vmm *VmMaker) extractNamesFromCodeChunk(dec labeledParsedCodeChunk) dtypes.Set[string] {
	if dec.decType == variableDeclaration || dec.decType == constantDeclaration {
		return ast.ExtractAllNames(dec.chunk.(*ast.AssignmentExpression).Right)
	}
	_, _, sig, _, body, given := vmm.cp.P.ExtractPartsOfFunction(vmm.cp.P.ParsedDeclarations[dec.decType][dec.decNumber])
	sigNames := dtypes.Set[string]{}
	for _, pair := range sig {
		if pair.VarType != "bling" {
			sigNames = sigNames.Add(pair.VarName)
		}
	}
	bodyNames := ast.ExtractAllNames(body)
	lhsG, rhsG := ast.ExtractNamesFromLhsAndRhsOfGivenBlock(given)
	bodyNames.AddSet(rhsG)
	bodyNames = bodyNames.SubtractSet(lhsG)
	return bodyNames.SubtractSet(sigNames)
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
		newService, newUP := StartService(path, vmm.cp.P.Directory, vmm.cp.vm.Database, vmm.cp.vm.HubServices)
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
	newCp, newUp := initializeFromSourcecode(vmm.cp.vm, path, sourcecode, vmm.cp.P.Directory)
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

			vmm.cp.EnumElements[tok.Literal] = vmm.cp.Reserve(values.ValueType(i)+values.FIRST_DEFINED_TYPE, len(vmm.cp.EnumElements), &tok)
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
func (vmm *VmMaker) createStructNamesAndLabels() {
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
		if name == "Error" {
			vmm.cp.vm.typeNumberOfUnwrappedError = typeNo // The vm needs to know this so it can convert an 'error' into an 'Error'.
		}
		vmm.cp.AnyTypeScheme = vmm.cp.AnyTypeScheme.Union(altType(typeNo))
		// We are now going to assume that the last element of AnyTypeScheme is a TypedTupleType and add the new struct type accordingly.
		lastType := vmm.cp.AnyTypeScheme[len(vmm.cp.AnyTypeScheme)-1].(TypedTupleType)
		vmm.cp.AnyTypeScheme[len(vmm.cp.AnyTypeScheme)-1] = TypedTupleType{(lastType.T).Union(altType(typeNo))}

		// The parser needs to know about it too.
		vmm.uP.Parser.Functions.Add(name)
		sig := node.(*ast.AssignmentExpression).Right.(*ast.StructExpression).Sig
		fn := &ast.PrsrFunction{Sig: sig, Body: &ast.BuiltInExpression{Name: name}, Number: DUMMY}
		vmm.cp.P.FunctionTable.Add(vmm.cp.P.TypeSystem, name, fn) // TODO --- give them their own ast type?
		vmm.uP.fnIndex[fnSource{structDeclaration, i}] = fn
		// We make the labels exist.

		labelsForStruct := make([]int, 0, len(sig))

		for _, labelNameAndType := range sig {
			labelName := labelNameAndType.VarName
			labelLocation, alreadyExists := vmm.cp.FieldLabelsInMem[labelName]
			if alreadyExists { // Structs can of course have overlapping fields but we don't want to declare them twice..
				labelsForStruct = append(labelsForStruct, vmm.cp.vm.Mem[labelLocation].V.(int))
			} else {
				vmm.cp.FieldLabelsInMem[labelName] = vmm.cp.Reserve(values.LABEL, len(vmm.cp.vm.Labels), node.GetToken())
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

func (vmm *VmMaker) createSnippetTypes(tok *token.Token) {
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
		fn := &ast.PrsrFunction{Sig: sig, Body: &ast.BuiltInExpression{Name: name}}
		vmm.cp.P.FunctionTable.Add(vmm.cp.P.TypeSystem, name, fn)
		vmm.uP.fnIndex[fnSource{snippetDeclaration, i}] = fn

		// And the vm.
		vmm.addStructLabelsToMc(name, typeNo, sig, tok)
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

func (vmm *VmMaker) addStructLabelsToMc(name string, typeNo values.ValueType, sig ast.AstSig, tok *token.Token) { // TODO --- seems like we're only using this for snippets and not regular structs?
	labelsForStruct := make([]int, 0, len(sig))
	for _, labelNameAndType := range sig {
		labelName := labelNameAndType.VarName
		labelLocation, alreadyExists := vmm.cp.FieldLabelsInMem[labelName]
		if alreadyExists { // Structs can of course have overlapping fields but we don't want to declare them twice.
			labelsForStruct = append(labelsForStruct, vmm.cp.vm.Mem[labelLocation].V.(int))
		} else {
			vmm.cp.FieldLabelsInMem[labelName] = vmm.cp.Reserve(values.LABEL, len(vmm.cp.vm.Labels), tok)
			labelsForStruct = append(labelsForStruct, len(vmm.cp.vm.Labels))
			vmm.cp.vm.Labels = append(vmm.cp.vm.Labels, labelName)
		}
	}
	typeInfo := vmm.cp.vm.concreteTypes[typeNo].(structType)
	typeInfo.labelNumbers = labelsForStruct
	typeInfo = typeInfo.addLabels(labelsForStruct)
	vmm.cp.vm.concreteTypes[typeNo] = typeInfo
}

func (vmm *VmMaker) compileConstructors() {
	for i, node := range vmm.uP.Parser.ParsedDeclarations[structDeclaration] {
		name := node.(*ast.AssignmentExpression).Left.GetToken().Literal // We know this and the next line are safe because we already checked in createStructs
		sig := node.(*ast.AssignmentExpression).Right.(*ast.StructExpression).Sig
		vmm.uP.fnIndex[fnSource{structDeclaration, i}].Number = uint32(len(vmm.cp.Fns))
		vmm.cp.Fns = append(vmm.cp.Fns, vmm.compileConstructor(name, sig, node.GetToken()))
		vmm.cp.Fns[len(vmm.cp.Fns)-1].Private = vmm.uP.isPrivate(int(structDeclaration), i)
	}
	sig := ast.AstSig{ast.NameTypenamePair{VarName: "text", VarType: "string"}, ast.NameTypenamePair{VarName: "env", VarType: "map"}}
	for i, name := range vmm.cp.P.Snippets {
		vmm.uP.fnIndex[fnSource{snippetDeclaration, i}].Number = uint32(len(vmm.cp.Fns))
		vmm.cp.Fns = append(vmm.cp.Fns, vmm.compileConstructor(name, sig, vmm.uP.Parser.ParsedDeclarations[snippetDeclaration][i].GetToken()))
		vmm.cp.Fns[len(vmm.cp.Fns)-1].Private = vmm.uP.isPrivate(int(snippetDeclaration), i)
	}
}

func (vmm *VmMaker) compileConstructor(name string, sig ast.AstSig, tok *token.Token) *CpFunc {
	typeNo := vmm.cp.StructNameToTypeNumber[name]
	cpF := &CpFunc{Types: altType(typeNo), Builtin: name}
	fnenv := NewEnvironment() // Note that we don't use this for anything, we just need some environment to pass to addVariables.
	cpF.LoReg = vmm.cp.MemTop()
	for _, pair := range sig {
		vmm.cp.AddVariable(fnenv, pair.VarName, FUNCTION_ARGUMENT, vmm.cp.TypeNameToTypeList[pair.VarType], tok)
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
	functionName, _, sig, rtnSig, body, given := vmm.uP.Parser.ExtractPartsOfFunction(node)

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
		vmm.cp.Reserve(values.UNDEFINED_VALUE, DUMMY, node.GetToken())
		if pair.VarType == "ref" {
			vmm.cp.AddVariable(fnenv, pair.VarName, REFERENCE_VARIABLE, vmm.cp.TypeNameToTypeList[pair.VarType], node.GetToken())
			continue
		}
		typeName := pair.VarType
		isVarargs := len(typeName) >= 3 && typeName[:3] == "..."
		if isVarargs {
			typeName = typeName[3:]
		}
		if len(typeName) >= 8 && typeName[0:8] == "varchar(" {
			if typeName[len(typeName)-1] == '?' {
				typeName = "string?"
			} else {
				typeName = "string"
			}
		}
		if isVarargs {
			vmm.cp.AddVariable(fnenv, pair.VarName, FUNCTION_ARGUMENT, AlternateType{TypedTupleType{vmm.cp.TypeNameToTypeList[pair.VarType]}}, node.GetToken())
		} else {
			vmm.cp.AddVariable(fnenv, pair.VarName, FUNCTION_ARGUMENT, vmm.cp.TypeNameToTypeList[pair.VarType], node.GetToken())
		}
	}
	cpF.HiReg = vmm.cp.MemTop()
	cpF.CallTo = vmm.cp.CodeTop()
	tupleData := make([]uint32, 0, len(sig))
	var foundTupleOrVarArgs bool
	for _, param := range sig {
		switch {
		case len(param.VarType) >= 3 && param.VarType[:3] == "...":
			tupleData = append(tupleData, 1)
			foundTupleOrVarArgs = true
		case param.VarType == "tuple":
			tupleData = append(tupleData, 2)
			foundTupleOrVarArgs = true
		default:
			tupleData = append(tupleData, 0)
		}

	}
	if foundTupleOrVarArgs {
		cpF.locOfTupleAndVarargData = vmm.cp.Reserve(values.INT_ARRAY, tupleData, node.GetToken())
	} else {
		cpF.locOfTupleAndVarargData = DUMMY
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
			vmm.cp.ThunkList = []ThunkData{}
			givenContext := context{fnenv, DEF, nil, cpF.LoReg}
			vmm.cp.compileGivenBlock(given, givenContext)
			cpF.CallTo = vmm.cp.CodeTop()
			if len(vmm.cp.ThunkList) > 0 {
				vmm.cp.cm("Initializing thunks for outer function.", body.GetToken())
			}
			for _, thunks := range vmm.cp.ThunkList {
				vmm.cp.Emit(Thnk, thunks.dest, thunks.value.MLoc, thunks.value.CAddr)
			}
		}
		bodyContext := context{fnenv, ac, vmm.cp.returnSigToAlternateType(rtnSig), cpF.LoReg}
		cpF.Types, _ = vmm.cp.CompileNode(body, bodyContext) // TODO --- could we in fact do anything useful if we knew it was a constant?
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

// We add various global constants.
func (vmm *VmMaker) initializeBuiltInConstants() {
	vmm.cp.Reserve(values.NULL, nil, &token.Token{Source: "Builtin constant"})
	vmm.cp.AddVariable(vmm.cp.GlobalConsts, "NULL", GLOBAL_CONSTANT_PUBLIC, altType(values.NULL), &token.Token{Source: "Builtin constant"})
	vmm.cp.Reserve(values.SUCCESSFUL_VALUE, nil, &token.Token{Source: "Builtin constant"})
	vmm.cp.AddVariable(vmm.cp.GlobalConsts, "OK", GLOBAL_CONSTANT_PUBLIC, altType(values.SUCCESSFUL_VALUE), &token.Token{Source: "Builtin constant"})
	vmm.cp.TupleType = vmm.cp.Reserve(values.TYPE, values.AbstractType{[]values.ValueType{values.TUPLE}, 0}, &token.Token{Source: "Builtin constant"})
}

func (vmm *VmMaker) compileGlobalConstantOrVariable(declarations declarationType, v int) {
	dec := vmm.uP.Parser.ParsedDeclarations[declarations][v]
	lhs := dec.(*ast.AssignmentExpression).Left
	rhs := dec.(*ast.AssignmentExpression).Right
	sig, _ := vmm.cp.P.RecursivelySlurpSignature(lhs, "*inferred*")
	if vmm.uP.ErrorsExist() {
		return
	}
	rollbackTo := vmm.cp.getState() // Unless the assignment generates code, i.e. we're creating a lambda function or a snippet, then we can roll back the declarations afterwards.
	ctxt := context{vmm.cp.GlobalVars, INIT, nil, DUMMY}
	vmm.cp.CompileNode(rhs, ctxt)
	if vmm.uP.ErrorsExist() {
		return
	}
	vmm.cp.Emit(Ret)
	vmm.cp.cm("Calling Run from vmMaker's compileGlobalConstantOrVariable method.", dec.GetToken())
	vmm.cp.vm.Run(uint32(rollbackTo.code))
	result := vmm.cp.vm.Mem[vmm.cp.That()]
	if !vmm.cp.vm.codeGeneratingTypes.Contains(result.T) { // We don't want to roll back the code generated when we make a lambda or a snippet.
		vmm.cp.rollback(rollbackTo, dec.GetToken())
	}
	isPrivate := vmm.uP.isPrivate(int(declarations), v)
	var vAcc varAccess
	envToAddTo := vmm.cp.GlobalConsts
	if declarations == constantDeclaration {
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
			vmm.cp.Reserve(values.TUPLE, result.V.([]values.Value)[last:], rhs.GetToken())
		} else {
			if tupleLen == len(sig)-1 {
				vmm.cp.Reserve(values.TUPLE, []values.Value{}, rhs.GetToken())
			} else {
				vmm.cp.Reserve(values.TUPLE, result.V, rhs.GetToken())
			}
		}
		vmm.cp.AddVariable(envToAddTo, sig[last].VarName, vAcc, altType(values.TUPLE), rhs.GetToken())
	} else {
		if rhsIsTuple {
			head = result.V.([]values.Value)
		}
	}
	for i := 0; i < loopTop; i++ {
		vmm.cp.Reserve(head[i].T, head[i].V, rhs.GetToken())
		if sig[i].VarType == "*inferred*" {
			vmm.cp.AddVariable(envToAddTo, sig[i].VarName, vAcc, altType(head[i].T), rhs.GetToken())
		} else {
			allowedTypes := vmm.cp.TypeNameToTypeList[sig[i].VarType]
			if allowedTypes.isNoneOf(head[i].T) {
				vmm.cp.P.Throw("comp/assign/type", dec.GetToken())
				return
			} else {
				vmm.cp.AddVariable(envToAddTo, sig[i].VarName, vAcc, allowedTypes, rhs.GetToken())
			}
		}
	}
}

func altType(t ...values.ValueType) AlternateType { // TODO --- Why!?!
	return AltType(t...)
}
