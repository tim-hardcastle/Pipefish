package initializer

import (
	"os"
	"strings"

	"github.com/lmorg/readline"
	"github.com/tim-hardcastle/Pipefish/source/ast"
	"github.com/tim-hardcastle/Pipefish/source/compiler"
	"github.com/tim-hardcastle/Pipefish/source/err"
	"github.com/tim-hardcastle/Pipefish/source/lexer"
	"github.com/tim-hardcastle/Pipefish/source/parser"
	"github.com/tim-hardcastle/Pipefish/source/settings"
	"github.com/tim-hardcastle/Pipefish/source/text"
	"github.com/tim-hardcastle/Pipefish/source/token"
	"github.com/tim-hardcastle/Pipefish/source/values"
	"github.com/tim-hardcastle/Pipefish/source/vm"
	"src.elv.sh/pkg/persistent/vector"
)

// This consists of everything we do up until we parse the functions, validation, etc.

// TODO ---
//
// Ideally, this would contain only things that need to be initialized before we perform
// the parsing. In fact, it doesn't, and a lot of the work of initializing types is in
// this section, and should be moved if possible.
//
// The parseEverythingFromFilepath function is recursive, calling itself (by a circuitous route)
// when it uses imports or external services. Hence it will in fact have parsed *everything*
// by the time it hands back control to initializer.go.

// Just exists to wrap around the next function.
func (iz *Initializer) ParseEverythingFromFilePath(mc *vm.Vm, cpb *parser.CommonParserBindle, ccb *compiler.CommonCompilerBindle, scriptFilepath, namespacePath string) (*compiler.Compiler, error) {
	sourcecode, e := compiler.GetSourceCode(scriptFilepath)
	if e != nil {
		return nil, e
	}
	return iz.ParseEverythingFromSourcecode(mc, cpb, ccb, scriptFilepath, sourcecode, namespacePath), nil
}

// This is broken into separate named steps basically so that I can in fact give the steps names.
func (iz *Initializer) parseEverything(scriptFilepath, sourcecode string) {
	iz.cmI("Starting parseEverything for script " + scriptFilepath + ".")

	if !settings.OMIT_BUILTINS {
		iz.cmI("Adding mandatory imports to namespace.")
		iz.addToNameSpace(settings.MandatoryImports)
	}
	if len(scriptFilepath) >= 4 && scriptFilepath[len(scriptFilepath)-4:] == ".hub" {
		iz.cmI("Adding hub.pf to hub namespace.")
		iz.addToNameSpace([]string{"rsc-pf/hub.pf"})
	}
	iz.cmI("Making new relexer with filepath '" + scriptFilepath + "'")
	iz.P.TokenizedCode = lexer.NewRelexer(scriptFilepath, sourcecode)

	iz.cmI("Making parser and tokenized program.")
	iz.getTokenizedCode()
	if iz.errorsExist() {
		return
	}

	iz.cmI("Initializing imports.")
	unnamespacedImports := iz.recursivelyParseImports()
	if iz.errorsExist() {
		return
	}

	iz.cmI("Initializing external services.")
	iz.initializeExternals()
	if iz.errorsExist() {
		return
	}

	iz.cmI("Adding unnamespaced imports to namespace.")
	iz.addToNameSpace(unnamespacedImports)
	if iz.errorsExist() {
		return
	}

	iz.cmI("Creating enums.")
	iz.createEnums()
	if iz.errorsExist() {
		return
	}

	iz.cmI("Creating clone types.")
	iz.createClones()
	if iz.errorsExist() {
		return
	}

	iz.cmI("Assigning type numbers to struct names.")
	iz.createStructNames()
	if iz.errorsExist() {
		return
	}

	iz.cmI("Creating abstract types.")
	iz.createAbstractTypes()
	if iz.errorsExist() {
		return
	}

	iz.cmI("Creating (but not populating) interface types.")
	iz.createInterfaceTypes()
	if iz.errorsExist() {
		return
	}

	iz.cmI("Creating struct labels.")
	iz.createStructLabels()
	if iz.errorsExist() {
		return
	}

	iz.cmI("Parse everything else.")
	iz.parseEverythingElse()
	if iz.errorsExist() {
		return
	}

	// TODO --- you can move this now.
	iz.cmI("Instantiating parameterized types.")
	iz.instantiateParameterizedTypes()
	if iz.errorsExist() {
		return
	}
	// We hand back flow of control to initializer.go.
}

// Besides the script being initialized, we want its namespace to contain NULL-namespaced
// imports and the built-in Pipefish functions, interfaces, generics, etc.
func (iz *Initializer) addToNameSpace(thingsToImport []string) {
	for _, fname := range thingsToImport {
		iz.cmI("Adding '" + fname + "' to namespace")
		var libDat []byte
		var err error
		if len(fname) >= 7 && fname[:7] == "rsc-pf/" {
			libDat, err = folder.ReadFile(fname)
		} else {
			libDat, err = os.ReadFile(fname)
		}
		if err != nil {
			iz.P.Throw("init/import/found", &token.Token{}, fname)
		}
		stdImp := strings.TrimRight(string(libDat), "\n") + "\n"
		iz.cmI("Making new relexer with filepath '" + fname + "'")
		iz.P.TokenizedCode = lexer.NewRelexer(fname, stdImp)
		iz.getTokenizedCode() // This is cumulative, it throws them all into the parser together.
		iz.P.Common.Sources[fname] = strings.Split(stdImp, "\n")
	}
}

// This spawns a child initializer for each namespaced import and then calls its
// `parseEverythingFromFilepath` method. It returns a list of `NULL`-namespaced imports which
// can then be thrown into the parent initializer's namespace using the `addToNamespace` method.
func (iz *Initializer) recursivelyParseImports() []string {
	unnamespacedImports := []string{}
	for _, tc := range iz.tokenizedCode[importDeclaration] {
		dec := tc.(*tokenizedExternalOrImportDeclaration)
		if dec.golang {
			iz.goBucket.imports[dec.path.Source] = append(iz.goBucket.imports[dec.path.Source], dec.path.Literal)
			continue
		}
		name := dec.name.Literal
		path := dec.path.Literal
		name, path = text.TweakNameAndPath(name, path, dec.path.Source)
		if dec.name.Literal == "NULL" {
			unnamespacedImports = append(unnamespacedImports, path)
			continue
		}
		newIz := NewInitializer()
		newIz.Common = iz.Common
		iz.initializers[path] = newIz
		newCp, e := newIz.ParseEverythingFromFilePath(iz.cp.Vm, iz.P.Common, iz.cp.Common, path, name+"."+iz.P.NamespacePath)
		if e != nil { // Then we couldn't open the file.
			iz.throw("init/import/file", &dec.path, path, e)
			return []string{}
		}
		iz.cp.Modules[name] = newCp
		iz.P.NamespaceBranch[name] = &parser.ParserData{newCp.P, path}
		newCp.P.Private = dec.private
	}
	return unnamespacedImports
}

// We add the external services, initializing them if necessary.
//
// There are three possibilities. Either we have a namespace without a path, in which case we're looking for
// a service with that name already running on the hub. Or we have a namespace and a filename, in which case
// we're looking for a service with that name running on the hub, checking that it has the same filename,
// updating it if necessary, and if it doesn't exist, trying to launch it.
//
// The third case is that we have a namespace and a path to a website. In that case, we need to find out whether
// there is in fact a Pipefish service, or at least something emulating one, on the other end.
//
// Either way, we then need to extract a stub of the external service's public functions, types, etc.
//
// Details of the external services are kept in the vm, because it will have to make the external calls.
func (iz *Initializer) initializeExternals() {
	for _, tc := range iz.tokenizedCode[externalDeclaration] {
		dec := tc.(*tokenizedExternalOrImportDeclaration)
		name := dec.name.Literal
		path := dec.path.Literal
		name, path = text.TweakNameAndPath(name, path, dec.path.Source)
		if path == "" { // Then this will work only if there's already an instance of a service of that name running on the hub.
			externalCP, ok := iz.Common.HubCompilers[name]
			if !ok {
				iz.throw("init/external/exist/a", &dec.name)
				continue
			}
			iz.addExternalOnSameHub(externalCP.ScriptFilepath, name)
			continue
		}
		if len(path) >= 5 && path[0:5] == "http:" {
			pos := strings.LastIndex(path, "/")
			if pos == -1 {
				iz.throw("init/external/path/a", &dec.path)
				continue
			}
			hostpath := path[0:pos]
			serviceName := path[pos+1:]
			pos = strings.LastIndex(hostpath, "/")
			if pos == -1 {
				iz.throw("init/external/path/b", &dec.path)
				continue
			}
			hostname := hostpath[pos+1:]
			// TODO --- you need ways of doing this remotely at least in HubTalk.
			rline := readline.NewInstance()
			println("Please enter your username and password for hub " + text.CYAN + "'" + hostname + "'" + text.RESET + ".")
			rline.SetPrompt("Username: ")
			username, _ := rline.Readline()
			rline.SetPrompt("Password: ")
			rline.PasswordMask = '▪'
			password, _ := rline.Readline()
			iz.addHttpService(hostpath, serviceName, username, password)
			continue
		}

		// Otherwise we have a path for which the Tweak function will have inferred a name if one was not supplied.
		hubServiceCp, ok := iz.Common.HubCompilers[name] // If the service already exists, then we just need to check that it uses the same source file.
		if ok {
			if hubServiceCp.ScriptFilepath != path {
				iz.throw("init/external/exist/b", &dec.path, hubServiceCp.ScriptFilepath)
			} else {
				iz.addExternalOnSameHub(path, name)
			}
			continue // Either we've thrown an error or we don't need to do anything.
		}
		// Otherwise we need to start up the service, add it to the hub, and then declare it as external.
		newServiceCp, e := StartCompilerFromFilepath(path, iz.Common.HubCompilers, iz.Common.HubStore)
		if e != nil { // Then we couldn't open the file.
			iz.throw("init/external/file", &dec.path, path, e.Error())
		}
		if len(newServiceCp.P.Common.Errors) > 0 {
			newServiceCp.P.Common.IsBroken = true
		}
		iz.Common.HubCompilers[name] = newServiceCp
		iz.addExternalOnSameHub(path, name)
	}
}

// Functions auxiliary to the above.
func (iz *Initializer) addExternalOnSameHub(path, name string) {
	hubService := iz.Common.HubCompilers[name]
	ev := func(line string) values.Value {
		exVal := hubService.Do(line)
		serialize := hubService.Vm.Literal(exVal)
		return iz.cp.Do(serialize)
	}
	pr := func() bool {
		return hubService.P.ErrorsExist()
	}
	se := func() string {
		return hubService.API
	}
	serviceToAdd := ExternalCallToHubHandler{ev, pr, se}
	iz.addAnyExternalService(serviceToAdd, path, name)
}

func (iz *Initializer) addHttpService(path, name, username, password string) {
	ds := func(valAsString string) values.Value {
		return iz.cp.Do(valAsString)
	}
	serviceToAdd := ExternalHttpCallHandler{path, name, username, password, ds}
	iz.addAnyExternalService(serviceToAdd, path, name)
}

func (iz *Initializer) addAnyExternalService(handlerForService vm.ExternalCallHandler, path, name string) {
	externalServiceOrdinal := uint32(len(iz.cp.Vm.ExternalCallHandlers))
	iz.cp.CallHandlerNumbersByName[name] = externalServiceOrdinal
	iz.cp.Vm.ExternalCallHandlers = append(iz.cp.Vm.ExternalCallHandlers, handlerForService)
	serializedAPI := handlerForService.GetAPI()
	sourcecode := SerializedAPIToDeclarations(serializedAPI, externalServiceOrdinal) // This supplies us with a stub that know how to call the external servie.
	if settings.SHOW_EXTERNAL_STUBS {
		println("Making stub for external service '", name, "'.\n\n")
		println(sourcecode)
		println("\n\n")
	}
	newIz := NewInitializer()
	newIz.Common = iz.Common
	iz.initializers[name] = newIz
	newCp := newIz.ParseEverythingFromSourcecode(iz.cp.Vm, iz.P.Common, iz.cp.Common, path, sourcecode, name+"."+iz.P.NamespacePath)
	iz.P.NamespaceBranch[name] = &parser.ParserData{newCp.P, path}
	newCp.P.Private = iz.tokenizedCode[externalDeclaration][externalServiceOrdinal].(*tokenizedExternalOrImportDeclaration).private
	iz.cp.Modules[name] = newCp
}

// This method takes the tokens from the relexer and splits it up into
// code types according  to the headword, which is discarded. It breaks these up into function
// declarations, variable intializations, etc. These are themselves split up into meaningful
// chunks of tokesn as a pre-parsing step, e.g. we will distinguish between a function name,
// its bling, its parameters, their types, its body, its `given` block, and validate that they
// have at least the correct lexical form.
func (iz *Initializer) getTokenizedCode() {
	var headword token.TokenType
	headword = token.ILLEGAL
	private := false
	if iz.P.CurToken.Type == token.EOF {
		iz.P.SafeNextToken()
	}
	if iz.P.CurToken.Type == token.EOF {
		iz.P.SafeNextToken()
	}
loop:
	for iz.P.CurToken.Type != token.EOF {
		var result tokenizedCode
		switch {
		case iz.P.CurToken.Type == "": // We just continue.
		case iz.P.CurToken.Type == token.NEWLINE: // We just continue.
		case token.TokenTypeIsHeadword(iz.P.CurToken.Type):
			headword = iz.P.CurToken.Type
			private = false
		case iz.P.CurToken.Type == token.PRIVATE:
			private = true
		default:
			switch headword {
			case token.ILLEGAL:
				iz.throw("init/head", &iz.P.CurToken)
				return
			case token.CMD, token.DEF:
				if iz.P.CurToken.Type == token.GOLANG { // Then we have a block of pure Go.
					result = &tokenizedGolangDeclaration{private, iz.P.CurToken}
				} else {
					result, _ = iz.ChunkFunction(headword == token.CMD, private)
				}
			case token.VAR, token.CONST:
				result, _ = iz.ChunkConstOrVarDeclaration(headword == token.CONST, private)
			case token.IMPORT, token.EXTERNAL:
				result, _ = iz.ChunkImportOrExternalDeclaration(headword == token.EXTERNAL, private)
			case token.NEWTYPE:
				result, _ = iz.ChunkTypeDeclaration(private)
				if result.getDeclarationType() == makeDeclarations {
					for _, ty := range result.(*tokenizedMakeDeclarations).types {
						iz.tokenizedCode[makeDeclaration] = append(iz.tokenizedCode[makeDeclaration],
							&tokenizedMakeDeclaration{
								private:  private,
								typeToks: ty,
							})
					}
					continue loop
				}
			default:
				panic("Unhandled headword.")
			}
		}
		if result != nil {
			iz.tokenizedCode[result.getDeclarationType()] =
				append(iz.tokenizedCode[result.getDeclarationType()], result)
			// println(result.getDeclarationType(), SummaryString(result))
		}
		iz.P.NextToken()
	}
	iz.P.Common.Errors = err.MergeErrors(iz.P.TokenizedCode.(*lexer.Relexer).GetErrors(), iz.P.Common.Errors)
}

// Function auxiliary to the above and to `createInterfaceTypes`. This extracts the words from a function definition
// and decides on their "grammatical" role: are they prefixes, suffixes, bling?
func (iz *Initializer) addWordsToParser(tc *tokenizedFunctionDeclaration) {
	startAt := 0
	switch tc.pos {
	case unfix:
		iz.P.Unfixes.Add(tc.op.Literal)
		return
	case suffix:
		iz.P.Suffixes.Add(tc.op.Literal)
		return
	case infix:
		iz.P.Infixes.Add(tc.op.Literal)
		for startAt = 2; !tc.sig[startAt-1].IsBling(); startAt++ {
		}
	}
	lastWasBling := true
	hasBling := false
	for ix := startAt; ix < len(tc.sig); ix++ {
		if tc.sig[ix].IsBling() {
			hasBling = true
			word := tc.sig[ix].Name.Literal
			if ix == len(tc.sig)-1 {
				iz.P.Endfixes.Add(word)
				break
			}
			if lastWasBling {
				iz.P.Forefixes.Add(word)
			} else {
				iz.P.Midfixes.Add((word))
			}
		}
		lastWasBling = tc.sig[ix].IsBling()
	}
	// TODO --- I've retained this distinction for back-compatibility but don't know if
	// it's important. It may be to do with using functions as first-class objects etc.
	if tc.pos == prefix {
		if hasBling {
			iz.P.Prefixes.Add(tc.op.Literal)
		} else {
			iz.P.Functions.Add(tc.op.Literal)
		}
	}
}

// Now we can start creating the user-defined types.

// We compile the enums.
//
// On the one hand, the VM must know the names of the enums and their elements so it can describe them.
// Otoh, the compiler needs to know how to turn enum literals into values.
func (iz *Initializer) createEnums() {
	for _, tc := range iz.tokenizedCode[enumDeclaration] {
		dec := tc.(*tokenizedEnumDeclaration)
		name := dec.op.Literal
		var typeNo values.ValueType
		info, typeExists := iz.getDeclaration(decENUM, &dec.op, DUMMY)
		if typeExists {
			typeNo = info.(values.ValueType)
			typeInfo := iz.cp.Vm.ConcreteTypeInfo[typeNo].(vm.EnumType)
			typeInfo.Path = iz.P.NamespacePath
			iz.cp.Vm.ConcreteTypeInfo[typeNo] = typeInfo
			for i, elementName := range typeInfo.ElementNames {
				iz.cp.EnumElements[elementName] = values.Value{typeNo, i}
			}
		} else {
			typeNo = values.ValueType(len(iz.cp.Vm.ConcreteTypeInfo))
			iz.setDeclaration(decENUM, &dec.op, DUMMY, typeNo)
		}
		iz.addType(dec.op.Literal, "enum", typeNo)
		iz.cp.P.EnumTypeNames.Add(dec.op.Literal)

		// We make the constructor function.
		iz.P.Functions.Add(name)
		sig := ast.AstSig{ast.NameTypeAstPair{"x", &ast.TypeWithName{token.Token{}, "int"}}}
		rtnSig := ast.AstSig{ast.NameTypeAstPair{"*dummy*", &ast.TypeWithName{token.Token{}, name}}}
		fnNo := iz.addToBuiltins(sig, name, altType(typeNo), dec.private, &dec.op)
		fn := &parsedFunction{
			decType:   functionDeclaration,
			decNumber: DUMMY,
			private:   dec.private,
			op:        dec.op,
			pos:       prefix,
			sig:       sig,
			body:      &ast.BuiltInExpression{Name: name},
			callInfo:  &compiler.CallInfo{iz.cp, fnNo, rtnSig},
		}
		iz.Add(name, fn)
		if typeExists {
			continue
		}
		vec := vector.Empty
		elementNameList := []string{}
		for ord, tok := range dec.elements {
			_, alreadyExists := iz.cp.EnumElements[tok.Literal]
			if alreadyExists { // Enums in the same namespace can't have overlapping elements or we wouldn't know their type.
				iz.throw("init/enum/element", &tok)
			}
			iz.cp.P.EnumElementNames.Add(tok.Literal)
			iz.cp.EnumElements[tok.Literal] = values.Value{typeNo, ord}
			vec = vec.Conj(values.Value{typeNo, ord})
			elementNameList = append(elementNameList, tok.Literal)
		}
		iz.cp.Vm.ConcreteTypeInfo = append(iz.cp.Vm.ConcreteTypeInfo, vm.EnumType{Name: name, Path: iz.P.NamespacePath, ElementNames: elementNameList,
			ElementValues: values.Value{values.LIST, vec}, Private: dec.private, IsMI: settings.MandatoryImportSet().Contains(dec.op.Source)})
	}
}

// We create the clone types.
func (iz *Initializer) createClones() {
	for _, tc := range iz.tokenizedCode[cloneDeclaration] {
		dec := tc.(*tokenizedCloneDeclaration)
		name := dec.op.Literal
		typeToClone := dec.parentTok.Literal
		parentTypeNo := parser.ClonableTypes[typeToClone]
		if len(dec.params) > 0 {
			astType := iz.makeTypeWithParameters(dec.op, dec.params)
			ok := iz.registerParameterizedType(name, astType, dec.requests, dec.body, typeToClone, dec.private, ixPtr(dec))
			if !ok {
				iz.throw("init/clone/exists", ixPtr(dec))
				continue
			}
			iz.setDeclaration(decPARAMETERIZED, ixPtr(dec), DUMMY, DUMMY)
			continue
		}
		typeNo, fn := iz.addCloneTypeAndConstructor(name, typeToClone, dec.private, ixPtr(dec))
		sig := ast.AstSig{ast.NameTypeAstPair{VarName: "x", VarType: ast.MakeAstTypeFrom(iz.cp.Vm.ConcreteTypeInfo[iz.cp.Vm.ConcreteTypeInfo[typeNo].(vm.CloneType).Parent].GetName(vm.DEFAULT))}}
		fn.callInfo.Number = iz.addToBuiltins(sig, name, altType(typeNo), dec.private, ixPtr(dec))
		iz.createOperations(&ast.TypeWithName{token.Token{}, name}, typeNo, dec.requests, parentTypeNo, dec.private)
	}
}

func (iz *Initializer) addCloneTypeAndConstructor(name, typeToClone string, private bool, decTok *token.Token) (values.ValueType, *parsedFunction) {
	typeNo, ok := iz.addCloneType(name, typeToClone, private, decTok)
	if !ok {
		return DUMMY, nil
	}
	// We make the conversion function.
	iz.P.Functions.Add(name)
	sig := ast.AstSig{ast.NameTypeAstPair{"x", &ast.TypeWithName{token.Token{}, typeToClone}}}
	rtnSig := ast.AstSig{ast.NameTypeAstPair{"*dummy*", &ast.TypeWithName{token.Token{}, name}}}
	fn := &parsedFunction{
		decType:   functionDeclaration,
		decNumber: DUMMY,
		private:   private,
		op:        *decTok,
		pos:       prefix,
		sig:       sig,
		body:      &ast.BuiltInExpression{Name: name},
		callInfo:  &compiler.CallInfo{iz.cp, DUMMY, rtnSig},
	}
	iz.Add(name, fn)
	if typeToClone == "int" || typeToClone == "float" {
		iz.P.Suffixes.Add(name)
	}
	return typeNo, fn
}

func (iz *Initializer) addCloneType(name, typeToClone string, private bool, decTok *token.Token) (values.ValueType, bool) {
	parentTypeNo, ok := parser.ClonableTypes[typeToClone]
	if !ok {
		iz.throw("init/clone/type/c", decTok, typeToClone)
		return DUMMY, false
	}
	abType := "clones{" + typeToClone + "}"
	var typeNo values.ValueType
	info, typeExists := iz.getDeclaration(decCLONE, decTok, DUMMY)
	if typeExists {
		typeNo = info.(values.ValueType)
		typeInfo := iz.cp.Vm.ConcreteTypeInfo[typeNo].(vm.CloneType)
		typeInfo.Path = iz.P.NamespacePath
		iz.cp.Vm.ConcreteTypeInfo[typeNo] = typeInfo
	} else {
		typeNo = values.ValueType(len(iz.cp.Vm.ConcreteTypeInfo))
		iz.setDeclaration(decCLONE, decTok, DUMMY, typeNo)
		iz.cp.Vm.ConcreteTypeInfo = append(iz.cp.Vm.ConcreteTypeInfo, vm.CloneType{Name: name, Path: iz.P.NamespacePath, Parent: parentTypeNo,
			Private: private, IsMI: settings.MandatoryImportSet().Contains(decTok.Source)})
		if parentTypeNo == values.LIST || parentTypeNo == values.STRING || parentTypeNo == values.SET || parentTypeNo == values.MAP {
			iz.cp.Common.IsRangeable = iz.cp.Common.IsRangeable.Union(altType(typeNo))
		}
	}
	cloneGroup := iz.cp.Common.SharedTypenameToTypeList[abType]
	iz.cp.TypeToCloneGroup[typeNo] = cloneGroup
	iz.addType(name, abType, typeNo)
	return typeNo, true
}

func (iz *Initializer) createOperations(nameAst ast.TypeNode, typeNo values.ValueType, opList []token.Token, parentTypeNo values.ValueType, private bool) {
	for i, opTok := range opList {
		op := opTok.Literal
		tok1 := &(opList[i])
		rtnSig := ast.AstSig{{"", nameAst}}
		switch parentTypeNo {
		case values.FLOAT:
			switch op {
			case "+":
				sig := ast.AstSig{ast.NameTypeAstPair{"x", nameAst}, ast.NameTypeAstPair{"+", ast.AsBling("+")}, ast.NameTypeAstPair{"y", nameAst}}
				iz.makeCloneFunction("+", sig, "add_floats", altType(typeNo), rtnSig, private, vm.INFIX, tok1)
			case "-":
				sig := ast.AstSig{ast.NameTypeAstPair{"x", nameAst}, ast.NameTypeAstPair{"-", ast.AsBling("-")}, ast.NameTypeAstPair{"y", nameAst}}
				iz.makeCloneFunction("-", sig, "subtract_floats", altType(typeNo), rtnSig, private, vm.INFIX, tok1)
				sig = ast.AstSig{ast.NameTypeAstPair{"x", nameAst}}
				iz.makeCloneFunction("-", sig, "negate_float", altType(typeNo), rtnSig, private, vm.PREFIX, tok1)
			case "*":
				sig := ast.AstSig{ast.NameTypeAstPair{"x", nameAst}, ast.NameTypeAstPair{"*", ast.AsBling("*")}, ast.NameTypeAstPair{"y", nameAst}}
				iz.makeCloneFunction("*", sig, "multiply_floats", altType(typeNo), rtnSig, private, vm.INFIX, tok1)
			case "/":
				sig := ast.AstSig{ast.NameTypeAstPair{"x", nameAst}, ast.NameTypeAstPair{"/", ast.AsBling("/")}, ast.NameTypeAstPair{"y", nameAst}}
				iz.makeCloneFunction("/", sig, "divide_floats", altType(typeNo), rtnSig, private, vm.INFIX, tok1)
			default:
				iz.throw("init/request/float", tok1, op)
			}
		case values.INT:
			switch op {
			case "+":
				sig := ast.AstSig{ast.NameTypeAstPair{"x", nameAst}, ast.NameTypeAstPair{"+", ast.AsBling("+")}, ast.NameTypeAstPair{"y", nameAst}}
				iz.makeCloneFunction("+", sig, "add_integers", altType(typeNo), rtnSig, private, vm.INFIX, tok1)
			case "-":
				sig := ast.AstSig{ast.NameTypeAstPair{"x", nameAst}, ast.NameTypeAstPair{"-", ast.AsBling("-")}, ast.NameTypeAstPair{"y", nameAst}}
				iz.makeCloneFunction("-", sig, "subtract_integers", altType(typeNo), rtnSig, private, vm.INFIX, tok1)
				sig = ast.AstSig{ast.NameTypeAstPair{"x", nameAst}}
				iz.makeCloneFunction("-", sig, "negate_integer", altType(typeNo), rtnSig, private, vm.PREFIX, tok1)
			case "*":
				sig := ast.AstSig{ast.NameTypeAstPair{"x", nameAst}, ast.NameTypeAstPair{"*", ast.AsBling("*")}, ast.NameTypeAstPair{"y", nameAst}}
				iz.makeCloneFunction("*", sig, "multiply_integers", altType(typeNo), rtnSig, private, vm.INFIX, tok1)
			case "div":
				sig := ast.AstSig{ast.NameTypeAstPair{"x", nameAst}, ast.NameTypeAstPair{"/", ast.AsBling("/")}, ast.NameTypeAstPair{"y", nameAst}}
				iz.makeCloneFunction("div", sig, "divide_integers", altType(typeNo), rtnSig, private, vm.INFIX, tok1)
			case "mod":
				sig := ast.AstSig{ast.NameTypeAstPair{"x", nameAst}, ast.NameTypeAstPair{"%", ast.AsBling("&")}, ast.NameTypeAstPair{"y", nameAst}}
				iz.makeCloneFunction("mod", sig, "modulo_integers", altType(typeNo), rtnSig, private, vm.INFIX, tok1)
			default:
				iz.P.Throw("init/request/int", tok1, op)
			}
		case values.LIST:
			switch op {
			case "+":
				sig := ast.AstSig{ast.NameTypeAstPair{"x", nameAst}, ast.NameTypeAstPair{"+", ast.AsBling("+")}, ast.NameTypeAstPair{"y", nameAst}}
				iz.makeCloneFunction("+", sig, "add_lists", altType(typeNo), rtnSig, private, vm.INFIX, tok1)
			case "with":
				sig := ast.AstSig{ast.NameTypeAstPair{"x", nameAst}, ast.NameTypeAstPair{"with", ast.AsBling("with")}, ast.NameTypeAstPair{"y", ast.DOTDOTDOT_PAIR}}
				iz.makeCloneFunction("with", sig, "list_with", altType(typeNo), rtnSig, private, vm.INFIX, tok1)
			case "?>":
				cloneData := iz.cp.Vm.ConcreteTypeInfo[typeNo].(vm.CloneType)
				cloneData.IsFilterable = true
				iz.cp.Vm.ConcreteTypeInfo[typeNo] = cloneData
			case ">>":
				cloneData := iz.cp.Vm.ConcreteTypeInfo[typeNo].(vm.CloneType)
				cloneData.IsMappable = true
				iz.cp.Vm.ConcreteTypeInfo[typeNo] = cloneData
			case "slice":
				cloneData := iz.cp.Vm.ConcreteTypeInfo[typeNo].(vm.CloneType)
				cloneData.IsSliceable = true
				iz.cp.Vm.ConcreteTypeInfo[typeNo] = cloneData
			default:
				iz.throw("init/request/list", tok1, op)
			}
		case values.MAP:
			switch op {
			case "with":
				sig := ast.AstSig{ast.NameTypeAstPair{"x", nameAst}, ast.NameTypeAstPair{"with", ast.AsBling("with")}, ast.NameTypeAstPair{"y", ast.DOTDOTDOT_PAIR}}
				iz.makeCloneFunction("with", sig, "map_with", altType(typeNo), rtnSig, private, vm.INFIX, tok1)
			case "without":
				sig := ast.AstSig{ast.NameTypeAstPair{"x", nameAst}, ast.NameTypeAstPair{"without", ast.AsBling("without")}, ast.NameTypeAstPair{"y", ast.DOTDOTDOT_ANY_NULLABLE}}
				iz.makeCloneFunction("without", sig, "map_without", altType(typeNo), rtnSig, private, vm.INFIX, tok1)
			default:
				iz.throw("init/request/map", tok1, op)
			}
		case values.PAIR:
			iz.throw("init/request/pair", tok1)
		case values.SET:
			switch op {
			case "+":
				sig := ast.AstSig{ast.NameTypeAstPair{"x", nameAst}, ast.NameTypeAstPair{"+", ast.AsBling("+")}, ast.NameTypeAstPair{"y", nameAst}}
				iz.makeCloneFunction("+", sig, "add_sets", altType(typeNo), rtnSig, private, vm.INFIX, tok1)
			case "-":
				sig := ast.AstSig{ast.NameTypeAstPair{"x", nameAst}, ast.NameTypeAstPair{"-", ast.AsBling("-")}, ast.NameTypeAstPair{"y", nameAst}}
				iz.makeCloneFunction("-", sig, "subtract_sets", altType(typeNo), rtnSig, private, vm.INFIX, tok1)
			case "/\\":
				sig := ast.AstSig{ast.NameTypeAstPair{"x", nameAst}, ast.NameTypeAstPair{"/\\", ast.AsBling("/\\")}, ast.NameTypeAstPair{"y", nameAst}}
				iz.makeCloneFunction("/\\", sig, "intersect_sets", altType(typeNo), rtnSig, private, vm.INFIX, tok1)
			default:
				iz.throw("init/request/set", tok1, op)
			}
		case values.STRING:
			switch op {
			case "+":
				sig := ast.AstSig{ast.NameTypeAstPair{"x", nameAst}, ast.NameTypeAstPair{"+", ast.AsBling("+")}, ast.NameTypeAstPair{"y", nameAst}}
				iz.makeCloneFunction("+", sig, "add_strings", altType(typeNo), rtnSig, private, vm.INFIX, tok1)
			case "slice":
				cloneData := iz.cp.Vm.ConcreteTypeInfo[typeNo].(vm.CloneType)
				cloneData.IsSliceable = true
				iz.cp.Vm.ConcreteTypeInfo[typeNo] = cloneData
			default:
				iz.throw("init/request/string", tok1, op)
			}
		}
	}
	cloneData := iz.cp.Vm.ConcreteTypeInfo[typeNo].(vm.CloneType)
	cloneData.Using = opList
	iz.cp.Vm.ConcreteTypeInfo[typeNo] = cloneData
}

// Function auxiliary to the previous one, to make constructors for the clone types.
func (iz *Initializer) makeCloneFunction(fnName string, sig ast.AstSig, builtinTag string, rtnTypes compiler.AlternateType, rtnSig ast.AstSig, IsPrivate bool, pos uint32, tok *token.Token) {
	fnNo := iz.addToBuiltins(sig, builtinTag, rtnTypes, IsPrivate, tok)
	fn := &parsedFunction{
		decType:   functionDeclaration,
		decNumber: DUMMY,
		private:   IsPrivate,
		op:        *tok,
		pos:       prefix,
		sig:       sig,
		body:      &ast.BuiltInExpression{Name: fnName},
		callInfo:  &compiler.CallInfo{iz.cp, fnNo, rtnSig},
	}
	iz.Common.Functions[funcSource{tok.Source, tok.Line, fnName, pos}] = fn
	conflictingFunction := iz.Add(fnName, fn)
	if conflictingFunction != nil && conflictingFunction != fn {
		iz.P.Throw("init/overload/c", tok, fnName, conflictingFunction.op)
	}
}

// We create the structs as names and type numbers in the type system. We can't populate
// their fields yet because we haven't even declared the abstract and interface types
// lexically yet.
func (iz *Initializer) createStructNames() {
	iz.structDeclarationNumberToTypeNumber = map[int]values.ValueType{}
	// First we need to make the struct types into types so the parser parses them properly.
	for i, tc := range iz.tokenizedCode[structDeclaration] {
		dec := tc.(*tokenizedStructDeclaration)
		if len(dec.params) != 0 {
			ok := iz.registerParameterizedType(dec.op.Literal, iz.makeTypeWithParameters(dec.op, dec.params), nil, nil, "struct", dec.private, ixPtr(dec))
			if !ok {
				iz.throw("init/struct/exists", ixPtr(dec))
				continue
			}
			iz.setDeclaration(decPARAMETERIZED, ixPtr(dec), DUMMY, DUMMY)
			continue
		}
		typeNo := iz.addStructType(dec.op.Literal, dec.private, ixPtr(dec))
		iz.structDeclarationNumberToTypeNumber[i] = typeNo
		// The VM needs fast access to a few special types.
		switch dec.op.Literal {
		case "Error":
			iz.cp.Vm.UsefulTypes.UnwrappedError = typeNo
		case "File":
			iz.cp.Vm.UsefulTypes.File = typeNo
		case "Terminal":
			iz.cp.Vm.UsefulTypes.Terminal = typeNo
		case "Output":
			iz.cp.Vm.UsefulTypes.Output = typeNo
		}
	}
}

func (iz *Initializer) addStructType(name string, private bool, indexToken *token.Token) values.ValueType {
	iz.P.Functions.Add(name)
	typeNo := values.ValueType(len(iz.cp.Vm.ConcreteTypeInfo))
	typeInfo, typeExists := iz.getDeclaration(decSTRUCT, indexToken, DUMMY)
	if typeExists { // TODO --- can this in fact occur? Why?
		typeNo = typeInfo.(structInfo).structNumber
		typeInfo := iz.cp.Vm.ConcreteTypeInfo[typeNo].(vm.StructType)
		typeInfo.Path = iz.P.NamespacePath
		iz.cp.Vm.ConcreteTypeInfo[typeNo] = typeInfo
	} else {
		iz.cp.Vm.ConcreteTypeInfo = append(iz.cp.Vm.ConcreteTypeInfo, vm.StructType{}) // As a placeholder.
		iz.setDeclaration(decSTRUCT, indexToken, DUMMY, structInfo{typeNo, private, nil})
	}
	iz.addType(name, "struct", typeNo)
	return typeNo
}

// We can now create the struct labels and define their type as an AstSig, though
// we can't make an abstract sig yet because we haven't populated the abstract types.
func (iz *Initializer) createStructLabels() {
	for i, tc := range iz.tokenizedCode[structDeclaration] {
		dec := tc.(*tokenizedStructDeclaration)
		name := dec.op.Literal
		indexToken := ixPtr(dec)
		labelsForStruct := iz.makeLabelsFromSig(iz.makeAstSigFromTokenizedSig(dec.sig), dec.private, indexToken)
		sig := iz.makeAstSigFromTokenizedSig(dec.sig)
		if len(dec.params) > 0 {
			ty := iz.makeTypeWithParameters(dec.op, dec.params)
			argIndex := iz.paramTypeExists(ty)
			iz.parameterizedTypes[ty.Name][argIndex].Sig = sig
			iz.parameterizedTypes[ty.Name][argIndex].Typecheck = dec.body
			continue
		}
		typeNo := iz.structDeclarationNumberToTypeNumber[i]
		iz.setDeclaration(decSTRUCT, indexToken, DUMMY, structInfo{typeNo, dec.private, sig})
		stT := vm.StructType{Name: name, Path: iz.P.NamespacePath, LabelNumbers: labelsForStruct,
			LabelValues: labelValuesFromLabelNumbers(labelsForStruct),
			Private:     dec.private, IsMI: settings.MandatoryImportSet().Contains(indexToken.Source)}
		stT = stT.AddLabels(labelsForStruct)
		iz.cp.Vm.ConcreteTypeInfo[typeNo] = stT
		fnNo := iz.addToBuiltins(sig, name, altType(typeNo), dec.private, indexToken)
		fn := &parsedFunction{
			decType:   functionDeclaration,
			decNumber: DUMMY,
			private:   dec.private,
			op:        dec.op,
			pos:       prefix,
			sig:       sig,
			body:      &ast.BuiltInExpression{Name: name},
			callInfo:  &compiler.CallInfo{iz.cp, fnNo, nil},
		}
		iz.Add(name, fn)
	}
}

func labelValuesFromLabelNumbers(numbers []int) values.Value {
	vec := vector.Empty
	for _, v := range numbers {
		vec = vec.Conj(values.Value{values.LABEL, v})
	}
	return values.Value{values.LIST, vec}
}

func (iz *Initializer) makeLabelsFromSig(sig ast.AstSig, private bool, indexTok *token.Token) []int {
	labelsForStruct := make([]int, 0, len(sig))
	for j, labelNameAndType := range sig {
		labelName := labelNameAndType.VarName
		labelLocation, alreadyExists := iz.cp.Vm.FieldLabelsInMem[labelName]
		if alreadyExists { // Structs can of course have overlapping fields but we don't want to declare them twice.
			labelsForStruct = append(labelsForStruct, iz.cp.Vm.Mem[labelLocation].V.(int))
			iz.setDeclaration(decLABEL, indexTok, j, labelInfo{labelLocation, true}) // 'true' because we can't tell if it's private or not until we've defined all the structs.
		} else {
			iz.cp.Vm.FieldLabelsInMem[labelName] = iz.cp.Reserve(values.LABEL, len(iz.cp.Vm.Labels), indexTok)
			iz.setDeclaration(decLABEL, indexTok, j, labelInfo{iz.cp.That(), true})
			labelsForStruct = append(labelsForStruct, len(iz.cp.Vm.Labels))
			iz.cp.Vm.Labels = append(iz.cp.Vm.Labels, labelName)
			iz.cp.Common.LabelIsPrivate = append(iz.cp.Common.LabelIsPrivate, private)
		}
	}
	return labelsForStruct
}

func (iz *Initializer) registerParameterizedType(name string, ty *ast.TypeWithParameters, opList []token.Token, typeCheck *token.TokenizedCodeChunk, parentType string, private bool, tok *token.Token) bool {
	info, ok := iz.parameterizedTypes[name]
	if ok {
		if iz.paramTypeExists(ty) == DUMMY { // TODO --- why?
			return false
		}
	}
	blankType := ty.Blank()
	blankType.Name = name
	supertype := blankType.String()
	iz.cp.GeneratedAbstractTypes.Add(supertype)
	thingToAdd := ParameterInfo{iz.astParamsToNames(ty.Parameters),
		iz.astParamsToValueTypes(ty.Parameters), opList,
		typeCheck, parentType, nil, private, supertype, tok}
	iz.cp.P.TypeMap[supertype] = values.AbstractType{}
	if ok {
		info = append(info, thingToAdd)
		iz.parameterizedTypes[name] = info
	} else {
		info = []ParameterInfo{thingToAdd}
		iz.parameterizedTypes[name] = info
		iz.cp.P.ParameterizedTypes = iz.cp.P.ParameterizedTypes.Add(name)
	}
	return true
}

func (iz *Initializer) paramTypeExists(ty *ast.TypeWithParameters) int {
	typesToMatch := iz.astParamsToValueTypes(ty.Parameters)
	for i, pty := range iz.parameterizedTypes[ty.Name] {
		if iz.parameterTypesMatch(typesToMatch, pty.Types) {
			return i
		}
	}
	return DUMMY
}

func (iz *Initializer) astParamsToValueTypes(params []*ast.Parameter) []values.ValueType {
	result := []values.ValueType{}
	for _, v := range params {
		result = append(result, iz.cp.ConcreteTypeNow(v.Type))
	}
	return result
}

func (iz *Initializer) astParamsToNames(params []*ast.Parameter) []string {
	result := []string{}
	for _, v := range params {
		result = append(result, v.Name)
	}
	return result
}

func (iz *Initializer) parameterTypesMatch(paramsToCheck, paramsToMatch []values.ValueType) bool {
	for i, v := range paramsToCheck {
		if i >= len(paramsToMatch) {
			return false
		}
		if v != paramsToMatch[i] {
			return false
		}
	}
	return true
}

// We create the abstract types as type names.
// TODO -- there's no sense in populating the type map here, we need to do it later and find
// dependencies if any between abstract types. However, at this point if we remove this then
// stuff starts to break.
func (iz *Initializer) createAbstractTypes() {
	for _, tc := range iz.tokenizedCode[abstractDeclaration] {
		dec := tc.(*tokenizedAbstractDeclaration)
		newTypename := dec.op.Literal
		iz.P.TypeMap[newTypename] = values.MakeAbstractType()
		if settings.MandatoryImportSet().Contains(dec.op.Source) {
			iz.unserializableTypes.Add(newTypename)
		}
		for _, typeAsTokens := range dec.types {
			typeTok := typeAsTokens[0]
			tname := typeTok.Literal
			abTypeToAdd, ok := iz.P.TypeMap[tname]
			if !ok {
				iz.throw("init/type/known", &typeTok)
				break
			}
			iz.P.TypeMap[newTypename] = iz.P.TypeMap[newTypename].Union(abTypeToAdd)
		}
		_, typeExists := iz.getDeclaration(decABSTRACT, ixPtr(dec), DUMMY)
		if !typeExists {
			iz.setDeclaration(decABSTRACT, ixPtr(dec), DUMMY, nil)
		}
		iz.P.Suffixes.Add(newTypename)
	}
}

// Phase 1L of compilation. Creates the interface types as names but doesn't populate them: parses the signatures
// of the functions in the interface definitions.
func (iz *Initializer) createInterfaceTypes() {
	for _, tc := range iz.tokenizedCode[interfaceDeclaration] {
		dec := tc.(*tokenizedInterfaceDeclaration)
		nameTok := dec.op
		newTypename := nameTok.Literal
		if settings.MandatoryImportSet().Contains(nameTok.Source) {
			iz.unserializableTypes.Add(newTypename)
		}
		typeInfo := []fnSigInfo{}
		for _, sig := range dec.sigs {
			functionName := sig.op.Literal
			astSig := iz.makeAstSigFromTokenizedSig(sig.sig)
			retSig := iz.makeRetsFromTokenizedReturns(sig.rets)
			typeInfo = append(typeInfo, fnSigInfo{functionName, astSig, retSig})
			iz.addWordsToParser(sig)
		}
		iz.P.TypeMap[newTypename] = values.MakeAbstractType() // We can't populate the interface types before we've parsed everything.
		_, typeExists := iz.getDeclaration(decINTERFACE, &nameTok, DUMMY)
		if !typeExists {
			iz.setDeclaration(decINTERFACE, &nameTok, DUMMY, interfaceInfo{typeInfo})
		}
		iz.P.Suffixes.Add(newTypename)
	}
}

func (iz *Initializer) instantiateParameterizedTypes() {
	typeOperators := make(map[string]typeOperatorInfo)
	for _, tc := range iz.tokenizedCode[makeDeclaration] {
		dec := tc.(*tokenizedMakeDeclaration)
		typeAst := iz.makeTypeAstFromTokens(dec.typeToks)
		if typeAst == nil {
			iz.throw("init/make/type", &dec.typeToks[0])
			continue
		}
		ty, ok := typeAst.(*ast.TypeWithArguments)
		if !ok {
			iz.throw("init/make/instance", &dec.typeToks[0])
			continue
		}
		private := dec.private
		// The parser doesn't know the types and values of enums, 'cos of being a
		// parser. So we kludge them in here.
		for i, v := range ty.Values() {
			if maybeEnum, ok := v.V.(string); ok && v.T == 0 {
				w := iz.cp.EnumElements[maybeEnum]
				ty.Arguments[i].Type = w.T
				ty.Arguments[i].Value = w.V
			}
		}
		argIndex := iz.findParameterizedType(ty.Name, ty.Values())
		if argIndex == DUMMY {
			iz.throw("init/type/args", &ty.Token)
			continue
		}
		parTypeInfo := iz.parameterizedTypes[ty.Name][argIndex]
		isClone := !(parTypeInfo.ParentType == "struct")
		newEnv := compiler.NewEnvironment()
		vals := []values.Value{}
		for i, name := range parTypeInfo.Names {
			iz.cp.Reserve(ty.Arguments[i].Type, ty.Arguments[i].Value, &ty.Arguments[i].Token)
			newEnv.Data[name] = compiler.Variable{iz.cp.That(), compiler.LOCAL_CONSTANT, altType(ty.Arguments[i].Type)}
			vals = append(vals, values.Value{ty.Arguments[i].Type, ty.Arguments[i].Value})
		}
		newTypeName := ty.String()
		parentTypeNo, ok := parser.ClonableTypes[parTypeInfo.ParentType]
		if !(ok || parTypeInfo.ParentType == "struct") {
			iz.throw("init/clone/type", &ty.Token)
			return
		}
		var (
			typeNo values.ValueType
			sig    ast.AstSig
		)
		if isClone {
			typeNo, _ = iz.addCloneType(ty.String(), parTypeInfo.ParentType, false, &ty.Token)
			iz.createOperations(ty, typeNo, parTypeInfo.Operations, parentTypeNo, parTypeInfo.IsPrivate)
			sig = ast.AstSig{ast.NameTypeAstPair{VarName: "x", VarType: ast.MakeAstTypeFrom(iz.cp.Vm.ConcreteTypeInfo[iz.cp.Vm.ConcreteTypeInfo[typeNo].(vm.CloneType).Parent].GetName(vm.DEFAULT))}}
		} else {
			typeNo = iz.addStructType(ty.String(), parTypeInfo.IsPrivate, &ty.Token)
			sig = parTypeInfo.Sig
			iz.setDeclaration(decSTRUCT, &ty.Token, DUMMY, structInfo{typeNo, private, sig})
			labelsForStruct := iz.makeLabelsFromSig(sig, private, &ty.Token)
			stT := vm.StructType{Name: newTypeName, Path: iz.P.NamespacePath, LabelNumbers: labelsForStruct,
				LabelValues: labelValuesFromLabelNumbers(labelsForStruct),
				Private:     private, IsMI: settings.MandatoryImportSet().Contains(ty.Token.Source)}
			stT = stT.AddLabels(labelsForStruct)
			iz.cp.Vm.ConcreteTypeInfo[typeNo] = stT
		}
		iz.cp.P.TypeMap[ty.String()] = values.AbstractType{[]values.ValueType{typeNo}}
		if opInfo, ok := typeOperators[ty.Name]; ok {
			opInfo.returnTypes = opInfo.returnTypes.Union(altType(typeNo))
			opInfo.definedAt = append(opInfo.definedAt, &ty.Token)
			typeOperators[ty.Name] = opInfo
			// TODO --- Check for matching sigs, being a clone.
		} else {
			typeOperators[ty.Name] = typeOperatorInfo{sig, isClone, altType(values.ERROR, typeNo), []*token.Token{&ty.Token}}
		}
		iz.parameterizedInstanceMap[newTypeName] = parameterizedTypeInstance{ty, newEnv, parTypeInfo.Typecheck, sig, vals}
		iz.cp.P.TypeMap[parTypeInfo.Supertype] = iz.cp.P.TypeMap[parTypeInfo.Supertype].Insert(typeNo)
	}
	// Now we can make a constructor function for each of the type operators.
	for typeOperator, operatorInfo := range typeOperators {
		name := typeOperator + "{}"
		sig := append(ast.AstSig{ast.NameTypeAstPair{"+t", &ast.TypeWithName{*operatorInfo.definedAt[0], "type"}}}, operatorInfo.constructorSig...)
		fnNo := iz.addToBuiltins(sig, name, operatorInfo.returnTypes, false, operatorInfo.definedAt[0])
		newOp := *operatorInfo.definedAt[0]
		newOp.Literal = name
		fn := &parsedFunction{
			decType:   functionDeclaration,
			decNumber: DUMMY,
			private:   false, // TODO --- why don't you know this?
			op:        newOp,
			pos:       prefix,
			sig:       sig,
			body:      &ast.BuiltInExpression{Name: name},
			callInfo:  &compiler.CallInfo{iz.cp, fnNo, nil},
		}
		iz.cp.P.Functions.Add(name)
		iz.Add(name, fn)
	}
}

// Auxilliary function to the type-defining function which adds the constructors to the builtins.
func (iz *Initializer) addToBuiltins(sig ast.AstSig, builtinTag string, returnTypes compiler.AlternateType, private bool, tok *token.Token) uint32 {
	cpF := &compiler.CpFunc{RtnTypes: returnTypes, Builtin: builtinTag}
	fnenv := compiler.NewEnvironment() // Note that we don't use this for anything, we just need some environment to pass to addVariables.
	cpF.LoReg = iz.cp.MemTop()
	for _, pair := range sig {
		iz.cp.AddVariable(fnenv, pair.VarName, compiler.FUNCTION_ARGUMENT, iz.cp.GetAlternateTypeFromTypeAst(pair.VarType), tok)
	}
	cpF.HiReg = iz.cp.MemTop()
	cpF.Private = private
	iz.cp.Fns = append(iz.cp.Fns, cpF)
	return uint32(len(iz.cp.Fns) - 1)
}

// Having declared the names of the various types and functions of the namespaces, we can now parse the
// chunks of actual code in the function bodies, `given` blcoks, assignments, validation.
var PARSEABLE = []declarationType{cloneDeclaration, structDeclaration, constantDeclaration,
	variableDeclaration, functionDeclaration, commandDeclaration}

func (iz *Initializer) parseEverythingElse() {
	iz.parsedCode = make([][]parsedCode, len(iz.tokenizedCode))
	for _, decType := range PARSEABLE {
		iz.parsedCode[decType] = make([]parsedCode, len(iz.tokenizedCode[decType]))
		for i, _ := range iz.tokenizedCode[decType] {
			iz.parsedCode[decType][i] = iz.parse(decType, i)
			iz.P.ResetNesting()
		}
	}
}

func (iz *Initializer) parse(decType declarationType, decNumber int) parsedCode {
	tc := iz.tokenizedCode[decType][decNumber]
	switch tc := tc.(type) {
	case *tokenizedCloneDeclaration:
		var body ast.Node
		if tc.body.Length() != 0 {
			iz.P.TokenizedCode = tc.body
			body = iz.P.ParseTokenizedChunk()
		}
		return &parsedTypecheck{
			decType:    decType,
			decNumber:  decNumber,
			indexTok:   ixPtr(tc),
			parameters: iz.makeAstSigFromTokenizedSig(tc.params),
			body:       body,
		}
	case *tokenizedConstOrVarDeclaration:
		sig := iz.makeAstSigFromTokenizedSig(tc.sig)
		iz.P.TokenizedCode = tc.body
		return &parsedAssignment{
			decType:   decType,
			decNumber: decNumber,
			indexTok:  ixPtr(tc),
			sig:       sig,
			body:      iz.P.ParseTokenizedChunk(),
		}
	case *tokenizedFunctionDeclaration:
		iz.P.TokenizedCode = tc.body
		parsedBody := iz.P.ParseTokenizedChunk()
		var parsedGiven ast.Node
		if tc.given != nil {
			iz.P.TokenizedCode = tc.given
			parsedGiven = iz.P.ParseTokenizedChunk()
		}
		return &parsedFunction{
			decType:   decType,
			decNumber: decNumber,
			private:   tc.private,
			op:        tc.op,
			pos:       tc.pos,
			sig:       iz.makeAstSigFromTokenizedSig(tc.sig),

			body:  parsedBody,
			given: parsedGiven,
			callInfo: &compiler.CallInfo{
				Compiler:    iz.cp,
				Number:      DUMMY,
				ReturnTypes: iz.makeRetsFromTokenizedReturns(tc.rets),
			},
		}
	case *tokenizedStructDeclaration:
		var body ast.Node
		if tc.body.Length() != 0 {
			iz.P.TokenizedCode = tc.body
			body = iz.P.ParseTokenizedChunk()
		}
		return &parsedTypecheck{
			decType:    decType,
			decNumber:  decNumber,
			indexTok:   ixPtr(tc),
			parameters: iz.makeAstSigFromTokenizedSig(tc.params),
			body:       body,
		}
	default:
		panic("You're not meant to parse that!")
	}
}
