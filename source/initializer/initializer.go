package initializer

import (
	"embed"
	"os"
	"path/filepath"
	"sort"
	"testing"

	"strings"

	"github.com/tim-hardcastle/Pipefish/source/ast"
	"github.com/tim-hardcastle/Pipefish/source/compiler"
	"github.com/tim-hardcastle/Pipefish/source/dtypes"
	"github.com/tim-hardcastle/Pipefish/source/err"
	"github.com/tim-hardcastle/Pipefish/source/lexer"
	"github.com/tim-hardcastle/Pipefish/source/parser"
	"github.com/tim-hardcastle/Pipefish/source/settings"
	"github.com/tim-hardcastle/Pipefish/source/text"
	"github.com/tim-hardcastle/Pipefish/source/token"
	"github.com/tim-hardcastle/Pipefish/source/values"
	"github.com/tim-hardcastle/Pipefish/source/vm"

	"github.com/lmorg/readline"
	"src.elv.sh/pkg/persistent/vector"
)

//go:embed rsc-pf/*
var folder embed.FS

// The initializer contains the methods and data which are only needed when parsing and compiling a script.
// It returns a compiler which has a parser which are capable between them of parsing, compiling, and running
// a single block/AST of Pipefish code, and which can therefore cope with everything demanded of them via
// the REPL at runtime.

// There is one initializer per module, just as there is one compiler and one parser. The others are constructed
// recursively as made necessary by the `import` and `external` declarations.

// Definition of the initializer type.
type initializer struct {
	cp                                  *compiler.Compiler             // The compiler for the module being intitialized.
	p                                   *parser.Parser                 // The parser for the module being initialized.
	initializers                        map[string]*initializer        // The child initializers of this one, to initialize imports and external stubs.
	TokenizedDeclarations               [14]TokenizedCodeChunks        // The declarations in the script, converted from text to tokens and sorted by purpose.
	ParsedDeclarations                  [13]parser.ParsedCodeChunks    // ASTs produced by parsing the tokenized chunks in the field above, sorted in the same way.
	localConcreteTypes                  dtypes.Set[values.ValueType]   // All the struct, enum, and clone types defined in a given module.
	goBucket                            *GoBucket                      // Where the initializer keeps information gathered during parsing the script that will be needed to compile the Go modules.
	Snippets                            []string                       // Names of snippet types visible to the module.
	fnIndex                             map[fnSource]*ast.PrsrFunction // The point of this is that once we've compiled a function, we need to set the Number field of the PrsrFunction representation to be the number of the cpFunc in the cp.Fns list.
	Common                              *CommonInitializerBindle       // The information all the initializers have in Common.
	structDeclarationNumberToTypeNumber map[int]values.ValueType       // Maps the order of the declaration of the struct in the script to its type number in the VM. TODO --- there must be something better than this.
	unserializableTypes                 dtypes.Set[string]             // Keeps track of which abstract types are mandatory imports/singletons of a concrete type so we don't try to serialize them.
	
}

// Makes a new initializer.
func NewInitializer() *initializer {
	iz := initializer{
		initializers:        make(map[string]*initializer),
		localConcreteTypes:  make(dtypes.Set[values.ValueType]),
		fnIndex:             make(map[fnSource]*ast.PrsrFunction),
		unserializableTypes: make(dtypes.Set[string]),
	}
	iz.newGoBucket()
	return &iz
}

// The CommonInitializerBindle contains information that all the initializers need to share.
type CommonInitializerBindle struct {
	Functions      map[FuncSource]*ast.PrsrFunction // This is to ensure that the same function (i.e. from the same place in source code) isn't parsed more than once.
	DeclarationMap map[decKey]any                   // This prevents redeclaration of types in the same sort of way.
	HubCompilers   map[string]*compiler.Compiler    // This is a map of the compilers of all the (potential) external services on the same hub.
	HubStore       *values.Map
}

// Initializes the `CommonInitializerBindle`
func NewCommonInitializerBindle(store *values.Map) *CommonInitializerBindle {
	b := CommonInitializerBindle{
		Functions:      make(map[FuncSource]*ast.PrsrFunction),
		DeclarationMap: make(map[decKey]any),
		HubCompilers:   make(map[string]*compiler.Compiler),
		HubStore:       store,
	}
	return &b
}

// Initializes a compiler.
func newCompiler(Common *parser.CommonParserBindle, ccb *compiler.CommonCompilerBindle, scriptFilepath, sourcecode string, mc *vm.Vm, namespacePath string) *compiler.Compiler {
	p := parser.New(Common, scriptFilepath, sourcecode, namespacePath)
	cp := compiler.NewCompiler(p, ccb)
	cp.ScriptFilepath = scriptFilepath
	cp.Vm = mc
	cp.TupleType = cp.Reserve(values.TYPE, values.AbstractType{[]values.ValueType{values.TUPLE}, 0}, &token.Token{Source: "Builtin constant"})
	return cp
}

func (iz *initializer) ParseEverythingFromFilePath(mc *vm.Vm, cpb *parser.CommonParserBindle, ccb *compiler.CommonCompilerBindle, scriptFilepath, namespacePath string) (*compiler.Compiler, error) {
	sourcecode, e := compiler.GetSourceCode(scriptFilepath)
	if e != nil {
		return nil, e
	}
	return iz.ParseEverythingFromSourcecode(mc, cpb, ccb, scriptFilepath, sourcecode, namespacePath), nil
}

func StartCompilerFromFilepath(filepath string, svs map[string]*compiler.Compiler, store *values.Map) (*compiler.Compiler, error) {
	sourcecode, e := compiler.GetSourceCode(filepath)
	if e != nil {
		return nil, e
	}
	return StartCompiler(filepath, sourcecode, svs, store), nil
}

// We begin by manufacturing a blank VM, a `CommonParserBindle` for all the parsers to share, and a
// `CommonInitializerBindle` for the initializers to share. These Common bindles are then passed down to the
// "children" of the intitializer and the parser when new modules are created.
func StartCompiler(scriptFilepath, sourcecode string, hubServices map[string]*compiler.Compiler, store *values.Map) *compiler.Compiler {
	iz := NewInitializer()
	iz.Common = NewCommonInitializerBindle(store)
	iz.Common.HubCompilers = hubServices
	// We then carry out eleven phases of initialization each of which is performed recursively on all of the
	// modules in the dependency tree before moving on to the next. (The need to do this is in fact what
	// defines the phases, so you shouldn't bother looking for some deeper logic in that.)
	iz.cmI("Parsing everything.")
	result := iz.ParseEverythingFromSourcecode(vm.BlankVm(), parser.NewCommonParserBindle(), compiler.NewCommonCompilerBindle(), scriptFilepath, sourcecode, "")
	if iz.ErrorsExist() {
		iz.cp.P.Common.IsBroken = true
		return result
	}
	iz.cmI("Finding shareable functions.")
	iz.findAllShareableFunctions()
	if iz.ErrorsExist() {
		iz.cp.P.Common.IsBroken = true
		return result
	}
	iz.cmI("Populating interface types.")
	iz.populateInterfaceTypes()
	if iz.ErrorsExist() {
		iz.cp.P.Common.IsBroken = true
		return result
	}
	iz.cmI("Populating abstract types and creating alternate types.")
	iz.populateAbstractTypes()
	if iz.ErrorsExist() {
		iz.cp.P.Common.IsBroken = true
		return result
	}
	iz.cmI("Making function tables.")
	iz.MakeFunctionTables()
	if iz.ErrorsExist() {
		iz.cp.P.Common.IsBroken = true
		return result
	}

	iz.cmI("Making function forest.")
	iz.MakeFunctionForests()
	if iz.ErrorsExist() {
		iz.cp.P.Common.IsBroken = true
		return result
	}
	iz.cmI("Adding abstract types to struct fields.")
	iz.AddFieldsToStructsAndCheckForConsistency()
	if iz.ErrorsExist() {
		iz.cp.P.Common.IsBroken = true
		return result
	}
	iz.cmI("Compiling Go.")
	iz.compileGoModules()
	if iz.ErrorsExist() {
		iz.cp.P.Common.IsBroken = true
		return result
	}
	iz.cmI("Compiling everything else.")
	iz.CompileEverything()
	if iz.ErrorsExist() {
		iz.cp.P.Common.IsBroken = true
		return result
	}
	iz.cmI("Resolving interface backtracks.")
	iz.ResolveInterfaceBacktracks()
	return result
}

// This initializes the initializers compiler (which initializes its parser), and
// extracts the source code from the given file, and then calls the `parseEverything“
// method, below.
func (iz *initializer) ParseEverythingFromSourcecode(mc *vm.Vm, cpb *parser.CommonParserBindle, ccb *compiler.CommonCompilerBindle, scriptFilepath, sourcecode, namespacePath string) *compiler.Compiler {
	iz.cp = newCompiler(cpb, ccb, scriptFilepath, sourcecode, mc, namespacePath)
	iz.p = iz.cp.P
	iz.parseEverything(scriptFilepath, sourcecode)
	iz.cp.ScriptFilepath = scriptFilepath
	if !(scriptFilepath == "" || scriptFilepath == "InitializeFromCode" ||
		(len(scriptFilepath) >= 5 && scriptFilepath[0:5] == "http:")) &&
		!testing.Testing() && !(len(scriptFilepath) >= 11 && scriptFilepath[:11] == "test-files/") {
		file, err := os.Stat(text.MakeFilepath(scriptFilepath))
		if err != nil {
			iz.Throw("init/source", LINKING_TOKEN, scriptFilepath)
			return nil
		}
		iz.cp.Timestamp = file.ModTime().UnixMilli()
	}
	iz.p.Common.Sources[scriptFilepath] = strings.Split(sourcecode, "\n")
	return iz.cp
}

// This in the heart of phase 1 compilation, and does everything up to and including parsing the code chunks,
// and then hands back flow of control to the StartService or RunTest method.
func (iz *initializer) parseEverything(scriptFilepath, sourcecode string) {
	iz.cmI("Starting makeall for script " + scriptFilepath + ".")

	if !settings.OMIT_BUILTINS {
		iz.cmI("Adding mandatory imports to namespace.")
		iz.AddToNameSpace(settings.MandatoryImports)
	}
	if len(scriptFilepath) >= 4 && scriptFilepath[len(scriptFilepath)-4:] == ".hub" {
		iz.cmI("Adding hub.pf to hub namespace.")
		iz.AddToNameSpace([]string{"rsc-pf/hub.pf"})
	}
	iz.cmI("Making new relexer with filepath '" + scriptFilepath + "'")
	iz.p.TokenizedCode = lexer.NewRelexer(scriptFilepath, sourcecode)

	iz.cmI("Making parser and tokenized program.")
	iz.MakeParserAndTokenizedProgram()
	if iz.ErrorsExist() {
		return
	}

	iz.cmI("Parsing import and external declarations.")
	iz.ParseImportAndExternalDeclarations() // That is, parse the import declarations. The files being imported are imported by the method with the long name below.
	if iz.ErrorsExist() {
		return
	}

	iz.cmI("Initializing imports.")
	unnamespacedImports := iz.ParseNamespacedImportsAndReturnUnnamespacedImports()
	if iz.ErrorsExist() {
		return
	}

	iz.cmI("Initializing external services.")
	iz.initializeExternals()
	if iz.ErrorsExist() {
		return
	}

	iz.cmI("Adding unnamespaced imports to namespace.")
	iz.AddToNameSpace(unnamespacedImports)
	if iz.ErrorsExist() {
		return
	}

	iz.cmI("Creating enums.")
	iz.createEnums()
	if iz.ErrorsExist() {
		return
	}

	iz.cmI("Creating clone types.")
	iz.createClones()
	if iz.ErrorsExist() {
		return
	}

	iz.cmI("Instantiating parameterized types.")
	iz.instantiateParameterizedTypes()
	if iz.ErrorsExist() {
		return
	}

	iz.cmI("Adding constructors to parser, parsing struct declarations.")
	iz.createStructNames()
	if iz.ErrorsExist() {
		return
	}

	iz.cmI("Creating abstract types.")
	iz.createAbstractTypes()
	if iz.ErrorsExist() {
		return
	}

	iz.cmI("Creating (but not populating) interface types.")
	iz.createInterfaceTypes()
	if iz.ErrorsExist() {
		return
	}

	iz.cmI("Creating struct labels.")
	iz.createStructLabels()
	if iz.ErrorsExist() {
		return
	}

	iz.cmI("Parsing everything else.")
	iz.parseEverythingElse()
	if iz.ErrorsExist() {
		return
	}
	// We hand back flow of control to StartService.
}

// Function auxilliary to phase 1. NULL-namespaced imports are first read from file and then tokenized
// into the same array of `TokenizedodeChunks` as the main file.
func (iz *initializer) AddToNameSpace(thingsToImport []string) {
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
			iz.p.Throw("init/import/found", &token.Token{}, fname)
		}
		stdImp := strings.TrimRight(string(libDat), "\n") + "\n"
		iz.cmI("Making new relexer with filepath '" + fname + "'")
		iz.p.TokenizedCode = lexer.NewRelexer(fname, stdImp)
		iz.MakeParserAndTokenizedProgram() // This is cumulative, it throws them all into the parser together.
		iz.p.Common.Sources[fname] = strings.Split(stdImp, "\n")
	}
}

// Phase 1A of compilation. This method takes the tokens from the relexer and splits it up into
// code types according  to the headword, which is discarded. It breaks these up into function
// declarations, variable intializations, etc.

// As it does so it checks out the signatures of the functions and commands and decides
// what "grammatical" role the words in the function signature play, and deposits
// lists of these into a Parser object: the Prefix, Forefix, Midfix, Suffix, Endfix etc classes.

// We then have a tokenized program broken into parts, and a parser primed to
// parse tokens into ASTs. We apply one to the other and produce ASTs from our
// tokenized code.
func (iz *initializer) MakeParserAndTokenizedProgram() {
	currentSection := UndefinedSection
	beginCount := 0
	indentCount := 0
	lastTokenWasColon := false
	typeDefined := declarationType(DUMMY)
	IsPrivate := false
	var (
		tok token.Token
	)

	tok = iz.p.TokenizedCode.NextToken() // note that we've already removed leading newlines.
	if settings.SHOW_RELEXER && !(settings.IGNORE_BOILERPLATE && settings.ThingsToIgnore.Contains(tok.Source)) {
		println(text.PURPLE+tok.Type, tok.Literal+text.RESET)
	}

	if tok.Type == token.EOF { // An empty file should still initiate a service, but one with no data.
		return
	}
	if !token.TokenTypeIsHeadword(tok.Type) {
		iz.Throw("init/head", &tok)
		return
	}

	currentSection = tokenTypeToSection[tok.Type]
	colonMeansFunctionOrCommand := (currentSection == CmdSection || currentSection == DefSection)

	line := token.NewCodeChunk()

	for tok = iz.p.TokenizedCode.NextToken(); true; tok = iz.p.TokenizedCode.NextToken() {
		if settings.SHOW_RELEXER && !(settings.IGNORE_BOILERPLATE && settings.ThingsToIgnore.Contains(tok.Source)) {
			println(text.PURPLE+tok.Type, tok.Literal+text.RESET)
		}
		if token.TokenTypeIsHeadword(tok.Type) {
			if tok.Literal == "import" {
				iz.Throw("init/import/first", &tok)
			}
			currentSection = tokenTypeToSection[tok.Type]
			IsPrivate = false
			lastTokenWasColon = false
			colonMeansFunctionOrCommand = (currentSection == CmdSection || currentSection == DefSection)
			continue
		}
		if tok.Type == token.PRIVATE {
			if IsPrivate {
				iz.Throw("init/private", &tok)
			}
			IsPrivate = true
			continue
		}
		if currentSection == TypesSection && tok.Type == token.IDENT {
			if tok.Literal == "make" {
				typeDefined = makeDeclaration
			} else {
				tD, ok := typeMap[tok.Literal]
				if ok {
					typeDefined = tD
				}
			}
		}
		if tok.Type == token.LPAREN {
			beginCount++
			if tok.Literal == "|->" {
				indentCount++
			}
		}
		if tok.Type == token.RPAREN {
			beginCount--
			if tok.Literal == "<-|" {
				indentCount--
			}
		}
		if ((tok.Type == token.NEWLINE || tok.Type == token.EOF) && !lastTokenWasColon && indentCount == 0 && line.Length() != 0) ||
			tok.Type == token.GOCODE {
			if tok.Type == token.GOCODE {
				line.Append(tok)
			}
			if beginCount != 0 {
				iz.Throw("init/close", &tok)
				beginCount = 0 // Prevents error storm.
				typeDefined = declarationType(DUMMY)
				colonMeansFunctionOrCommand = (currentSection == CmdSection || currentSection == DefSection)
				continue
			}
			switch currentSection {
			case ImportSection:
				iz.addTokenizedDeclaration(importDeclaration, line, IsPrivate)

			case ExternalSection:
				iz.addTokenizedDeclaration(externalDeclaration, line, IsPrivate)
			case CmdSection:
				line.ToStart()
				if line.Length() == 1 && line.NextToken().Type == token.GOCODE {
					iz.addTokenizedDeclaration(golangDeclaration, line, IsPrivate)
				} else {
					iz.addTokenizedDeclaration(commandDeclaration, line, IsPrivate)
				}
			case DefSection:
				line.ToStart()
				if line.Length() == 1 && line.NextToken().Type == token.GOCODE {
					iz.addTokenizedDeclaration(golangDeclaration, line, IsPrivate)
				} else {
					iz.addTokenizedDeclaration(functionDeclaration, line, IsPrivate)
				}

			case VarSection, ConstSection:
				// As a wretched kludge, we will now weaken some of the commas on the LHS of
				// the assignment so that it parses properly. (TODO: at this point it would be much easier to
				// do this in the relexer.)
				lastWasType := false
				lastWasVar := false
				line.ToStart()
				for t := line.NextToken(); !(t.Type == token.ASSIGN); t = line.NextToken() {
					if t.Type == token.COMMA {
						if lastWasType {
							line.Change(token.Token{Type: token.WEAK_COMMA, Line: tok.Line, Literal: ","})
						}
						lastWasType = false
						lastWasVar = false
					} else {
						lastWasType = lastWasVar
						lastWasVar = !lastWasType
					}
				}
				if currentSection == VarSection {
					iz.addTokenizedDeclaration(variableDeclaration, line, IsPrivate)
				} else {
					iz.addTokenizedDeclaration(constantDeclaration, line, IsPrivate)
				}
			case TypesSection:
				if typeDefined != declarationType(DUMMY) {
					iz.addTokenizedDeclaration(typeDefined, line, IsPrivate)
				} else {
					iz.Throw("init/type/defined", &tok)
				}
			default:
				panic("Unhandled section type.")
			}
			line = token.NewCodeChunk()
			typeDefined = declarationType(DUMMY)
			colonMeansFunctionOrCommand = (currentSection == CmdSection || currentSection == DefSection)
			continue
		}
		if tok.Type == token.NEWLINE && line.Length() == 0 {
			continue
		}

		lastTokenWasColon = (tok.Type == token.COLON)

		if (lastTokenWasColon || tok.Type == token.PIPE) && colonMeansFunctionOrCommand { // If we found the first : in a command/function declaration, then what is to the left of the colon is the command/function's signature.
			colonMeansFunctionOrCommand = false
			iz.addWordsToParser(line)
		}
		line.Append(tok)
		if tok.Type == token.EOF {
			break
		}
	}

	iz.p.Common.Errors = err.MergeErrors(iz.p.TokenizedCode.(*lexer.Relexer).GetErrors(), iz.p.Common.Errors)
}

// Function auxiliary to the above and to `createInterfaceTypes`. This extracts the words from a function definition
// and decides on their "grammatical" role: are they prefixes, suffixes, bling?
func (iz *initializer) addWordsToParser(currentChunk *token.TokenizedCodeChunk) {
	inParenthesis := false
	hasPrefix := false
	hasParams := false
	hasMidOrEndfix := false
	lastTokenWasFix := false
	prefix := ""
	tok := token.Token{}
	currentChunk.ToStart()
	for j := 0; j < currentChunk.Length(); j++ {
		tok = currentChunk.NextToken()
		if tok.Type == token.LPAREN {
			hasParams = true
			inParenthesis = true
			lastTokenWasFix = false
			continue
		}

		if tok.Type == token.RPAREN {
			inParenthesis = false
			continue
		}

		if inParenthesis {
			continue
		}

		if tok.Type != token.IDENT {
			iz.Throw("init/inexplicable", &tok)
		}

		if j == 0 {
			prefix = tok.Literal
			hasPrefix = true
			lastTokenWasFix = true
			continue
		}

		if j < currentChunk.Length()-1 {
			if hasPrefix {
				if lastTokenWasFix {
					iz.p.Forefixes.Add(tok.Literal)
				} else {
					iz.p.Midfixes.Add(tok.Literal)
				}
			} else {
				iz.p.Infixes.Add(tok.Literal)
			}
			hasMidOrEndfix = true
			lastTokenWasFix = true
			continue
		}

		if hasPrefix || hasMidOrEndfix {
			iz.p.Endfixes.Add(tok.Literal)
		} else {
			iz.p.Suffixes.Add(tok.Literal)
		}
		hasMidOrEndfix = true
		lastTokenWasFix = true
	}

	if hasPrefix {
		if hasMidOrEndfix {
			iz.p.Prefixes.Add(prefix)
		} else {
			if hasParams {
				iz.p.Functions.Add(prefix)
			} else {
				iz.p.Unfixes.Add(prefix)
			}
		}
	} else {
		if hasMidOrEndfix && !inParenthesis && !(tok.Literal == ")") && !iz.p.Suffixes.Contains(tok.Literal) {
			iz.p.Endfixes.Add(tok.Literal)
		}
	}
}

// Phase 1B of compilation. At this point we parse the import and external declarations but then just stow away the
// resulting ASTs for the next two steps, where we have to treat imports and externals very differently.
func (iz *initializer) ParseImportAndExternalDeclarations() {
	for kindOfDeclarationToParse := importDeclaration; kindOfDeclarationToParse <= externalDeclaration; kindOfDeclarationToParse++ {
		iz.ParsedDeclarations[kindOfDeclarationToParse] = parser.ParsedCodeChunks{}
		for chunk := 0; chunk < len(iz.TokenizedDeclarations[kindOfDeclarationToParse]); chunk++ {
			iz.p.TokenizedCode = iz.TokenizedDeclarations[kindOfDeclarationToParse][chunk]
			iz.TokenizedDeclarations[kindOfDeclarationToParse][chunk].ToStart()
			iz.ParsedDeclarations[kindOfDeclarationToParse] = append(iz.ParsedDeclarations[kindOfDeclarationToParse], iz.p.ParseTokenizedChunk())
		}
	}
}

// Phase 1C of compilation. We call ParseEverything on the namespaced imports, returning a
// list of unnamespaced imports which the main phase 1 function will add to the parser.
func (iz *initializer) ParseNamespacedImportsAndReturnUnnamespacedImports() []string {
	unnamespacedImports := []string{}
	for i, imp := range iz.ParsedDeclarations[importDeclaration] {
		namespace := ""
		scriptFilepath := ""
		switch imp := (imp).(type) {
		case *ast.GolangExpression:
			iz.goBucket.imports[imp.Token.Source] = append(iz.goBucket.imports[imp.Token.Source], imp.Token.Literal)
			continue
		default:
			namespace, scriptFilepath = iz.getPartsOfImportOrExternalDeclaration(imp)
		}
		if namespace == "" {
			unnamespacedImports = append(unnamespacedImports, scriptFilepath)
		}
		newIz := NewInitializer()
		newIz.Common = iz.Common
		iz.initializers[scriptFilepath] = newIz
		newCp, e := newIz.ParseEverythingFromFilePath(iz.cp.Vm, iz.p.Common, iz.cp.Common, scriptFilepath, namespace+"."+iz.p.NamespacePath)
		if e != nil { // Then we couldn't open the file.
			iz.Throw("init/import/file", imp.GetToken(), scriptFilepath, e)
			return []string{}
		}
		iz.cp.Modules[namespace] = newCp
		iz.p.NamespaceBranch[namespace] = &parser.ParserData{newCp.P, scriptFilepath}
		newCp.P.Private = iz.IsPrivate(int(importDeclaration), i)
	}
	return unnamespacedImports
}

// Phase 1D of compilation. We add the external services, initializing them if necessary.
//
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
func (iz *initializer) initializeExternals() {
	for _, declaration := range iz.ParsedDeclarations[externalDeclaration] {
		name, path := iz.getPartsOfImportOrExternalDeclaration(declaration)
		if path == "" { // Then this will work only if there's already an instance of a service of that name running on the hub.
			externalCP, ok := iz.Common.HubCompilers[name]
			if !ok {
				iz.Throw("init/external/exist/a", declaration.GetToken())
				continue
			}
			iz.addExternalOnSameHub(externalCP.ScriptFilepath, name)
			continue
		}
		if len(path) >= 5 && path[0:5] == "http:" {
			pos := strings.LastIndex(path, "/")
			if pos == -1 {
				iz.Throw("init/external/path/a", declaration.GetToken())
				continue
			}
			hostpath := path[0:pos]
			serviceName := path[pos+1:]
			pos = strings.LastIndex(hostpath, "/")
			if pos == -1 {
				iz.Throw("init/external/path/b", declaration.GetToken())
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
			iz.addHttpService(hostpath, serviceName, username, password)
			continue
		}

		// Otherwise we have a path for which the getParts... function will have inferred a name if one was not supplied.
		hubServiceCp, ok := iz.Common.HubCompilers[name] // If the service already exists, then we just need to check that it uses the same source file.
		if ok {
			if hubServiceCp.ScriptFilepath != path {
				iz.Throw("init/external/exist/b", declaration.GetToken(), hubServiceCp.ScriptFilepath)
			} else {
				iz.addExternalOnSameHub(path, name)
			}
			continue // Either we've thrown an error or we don't need to do anything.
		}
		// Otherwise we need to start up the service, add it to the hub, and then declare it as external.
		newServiceCp, e := StartCompilerFromFilepath(path, iz.Common.HubCompilers, iz.Common.HubStore)
		if e != nil { // Then we couldn't open the file.
			iz.Throw("init/external/file", declaration.GetToken(), path, e.Error())
		}
		if len(newServiceCp.P.Common.Errors) > 0 {
			newServiceCp.P.Common.IsBroken = true
		}
		iz.Common.HubCompilers[name] = newServiceCp
		iz.addExternalOnSameHub(path, name)
	}
}

// Functions auxiliary to the above.
func (iz *initializer) addExternalOnSameHub(path, name string) {
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
		return hubService.SerializeApi()
	}
	serviceToAdd := compiler.ExternalCallToHubHandler{ev, pr, se}
	iz.addAnyExternalService(serviceToAdd, path, name)
}

func (iz *initializer) addHttpService(path, name, username, password string) {
	ds := func(valAsString string) values.Value {
		return iz.cp.Do(valAsString)
	}
	serviceToAdd := compiler.ExternalHttpCallHandler{path, name, username, password, ds}
	iz.addAnyExternalService(serviceToAdd, path, name)
}

func (iz *initializer) addAnyExternalService(handlerForService vm.ExternalCallHandler, path, name string) {
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
	newCp := newIz.ParseEverythingFromSourcecode(iz.cp.Vm, iz.p.Common, iz.cp.Common, path, sourcecode, name+"."+iz.p.NamespacePath)
	iz.p.NamespaceBranch[name] = &parser.ParserData{newCp.P, path}
	newCp.P.Private = iz.IsPrivate(int(externalDeclaration), int(externalServiceOrdinal))
	iz.cp.Modules[name] = newCp
}

// Now we can start creating the user-defined types.

// Phase 1E of compilation. We compile the enums.
//
// On the one hand, the VM must know the names of the enums and their elements so it can describe them.
// Otoh, the compiler needs to know how to turn enum literals into values.
func (iz *initializer) createEnums() {
	for i, tokens := range iz.TokenizedDeclarations[enumDeclaration] {
		tokens.ToStart()
		tok1 := tokens.NextToken()
		name := tok1.Literal
		var typeNo values.ValueType
		info, typeExists := iz.getDeclaration(decENUM, &tok1, DUMMY)
		if typeExists {
			typeNo = info.(values.ValueType)
			typeInfo := iz.cp.Vm.ConcreteTypeInfo[typeNo].(vm.EnumType)
			typeInfo.Path = iz.p.NamespacePath
			iz.cp.Vm.ConcreteTypeInfo[typeNo] = typeInfo
			for i, elementName := range typeInfo.ElementNames {
				iz.cp.EnumElements[elementName] = values.Value{typeNo, i}
			}
		} else {
			typeNo = values.ValueType(len(iz.cp.Vm.ConcreteTypeInfo))
			iz.setDeclaration(decENUM, &tok1, DUMMY, typeNo)
		}
		iz.AddType(tok1.Literal, "enum", typeNo)

		// We make the constructor function.

		iz.p.AllFunctionIdents.Add(name)
		iz.p.Functions.Add(name)
		sig := ast.AstSig{ast.NameTypeAstPair{"x", &ast.TypeWithName{token.Token{}, "int"}}}
		rtnSig := ast.AstSig{ast.NameTypeAstPair{"*dummy*", &ast.TypeWithName{token.Token{}, name}}}
		fnNo := iz.addToBuiltins(sig, name, altType(typeNo), iz.IsPrivate(int(enumDeclaration), i), &tok1)
		fn := &ast.PrsrFunction{NameSig: sig, NameRets: rtnSig, Body: &ast.BuiltInExpression{Name: name}, Number: fnNo, Compiler: iz.cp, Tok: &tok1}
		iz.Add(name, fn)
		if typeExists {
			continue
		}
		tokens.NextToken() // Skip over the '='.
		tokens.NextToken() // This says 'enum' or we wouldn't be here.
		elementNameList := []string{}
		for tok := tokens.NextToken(); tok.Type != token.EOF; {
			if tok.Type != token.IDENT {
				iz.Throw("init/enum/ident", &tok)
			}
			_, alreadyExists := iz.cp.EnumElements[tok.Literal]
			if alreadyExists { // Enums in the same namespace can't have overlapping elements or we wouldn't know their type.
				iz.Throw("init/enum/element", &tok)
			}

			iz.cp.EnumElements[tok.Literal] = values.Value{typeNo, len(elementNameList)}
			elementNameList = append(elementNameList, tok.Literal)
			tok = tokens.NextToken()
			if tok.Type != token.COMMA && tok.Type != token.WEAK_COMMA && tok.Type != token.EOF {
				iz.Throw("init/enum/comma", &tok)
			}
			tok = tokens.NextToken()
		}
		iz.cp.Vm.ConcreteTypeInfo = append(iz.cp.Vm.ConcreteTypeInfo, vm.EnumType{Name: name, Path: iz.p.NamespacePath, ElementNames: elementNameList,
			Private: iz.IsPrivate(int(enumDeclaration), i), IsMI: settings.MandatoryImportSet().Contains(tok1.Source)})
	}
}

// Phase 1F of compilation. We compile the clone types.
func (iz *initializer) createClones() {
mainLoop:
	for i, tokens := range iz.TokenizedDeclarations[cloneDeclaration] {
		tok1 := tokens.IndexToken()
		private := iz.IsPrivate(int(cloneDeclaration), i)
		tokens.ToStart()
		iz.cp.P.TokenizedCode = tokens
		name, paramSig, typeToClone := iz.cp.P.ParseClone()
		parentTypeNo, ok := parser.ClonableTypes[typeToClone]
		if !ok {
			iz.Throw("init/clone/type/a", tok1, typeToClone)
			return
		}
		switch paramSig := paramSig.(type) {
		case *ast.TypeWithName:
		case *ast.TypeWithParameters:
			opList, typeCheck := iz.getOpList(tokens)
			ok := iz.registerParameterizedType(tok1.Literal, paramSig, opList, typeCheck, typeToClone, iz.IsPrivate(int(cloneDeclaration), i))
			if !ok {
				iz.Throw("init/clone/exists", tokens.IndexToken())
				continue mainLoop
			}
			iz.setDeclaration(decPARAMETERIZED, tok1, DUMMY, DUMMY)
			continue mainLoop
		default:
			iz.Throw("init/clone/type", tokens.IndexToken())
			continue mainLoop
		}
		
		typeNo, fn := iz.addTypeAndConstructor(name, typeToClone, private, tok1)
		sig := ast.AstSig{ast.NameTypeAstPair{VarName: "x", VarType: ast.MakeAstTypeFrom(iz.cp.Vm.ConcreteTypeInfo[iz.cp.Vm.ConcreteTypeInfo[typeNo].(vm.CloneType).Parent].GetName(vm.DEFAULT))}}
		fn.Number = iz.addToBuiltins(sig, name, altType(typeNo), iz.IsPrivate(int(cloneDeclaration), i), tok1)

		// We get the requested builtins.
		opList, _ := iz.getOpList(tokens)
		if iz.ErrorsExist() {
			return
		}
		// And add them to the Common functions.
		iz.createOperations(&ast.TypeWithName{token.Token{}, name}, typeNo, opList, parentTypeNo, private, tok1)
	}	
}

func (iz *initializer) addTypeAndConstructor(name, typeToClone string, private bool, decTok *token.Token) (values.ValueType, *ast.PrsrFunction) {
	parentTypeNo, ok := parser.ClonableTypes[typeToClone]
	if !ok {
		iz.Throw("init/clone/type/a", decTok, typeToClone)
		return DUMMY, nil
	}
	abType := typeToClone + "like"
	var typeNo values.ValueType
	info, typeExists := iz.getDeclaration(decCLONE, decTok, DUMMY)
	if typeExists { 
		typeNo = info.(values.ValueType)
		typeInfo := iz.cp.Vm.ConcreteTypeInfo[typeNo].(vm.CloneType)
		typeInfo.Path = iz.p.NamespacePath
		iz.cp.Vm.ConcreteTypeInfo[typeNo] = typeInfo
	} else {
		typeNo = values.ValueType(len(iz.cp.Vm.ConcreteTypeInfo))
		iz.setDeclaration(decCLONE, decTok, DUMMY, typeNo)
		iz.cp.Vm.ConcreteTypeInfo = append(iz.cp.Vm.ConcreteTypeInfo, vm.CloneType{Name: name, Path: iz.p.NamespacePath, Parent: parentTypeNo,
			Private: private, IsMI: settings.MandatoryImportSet().Contains(decTok.Source)})
		if parentTypeNo == values.LIST || parentTypeNo == values.STRING || parentTypeNo == values.SET || parentTypeNo == values.MAP {
			iz.cp.Common.IsRangeable = iz.cp.Common.IsRangeable.Union(altType(typeNo))
		}
	}
	cloneGroup := iz.cp.Common.SharedTypenameToTypeList[abType]
	iz.cp.TypeToCloneGroup[typeNo] = cloneGroup
	// We make the conversion function.
	iz.AddType(name, abType, typeNo)
	iz.p.AllFunctionIdents.Add(name)
	iz.p.Functions.Add(name)
	sig := ast.AstSig{ast.NameTypeAstPair{"x", &ast.TypeWithName{token.Token{}, typeToClone}}}
	rtnSig := ast.AstSig{ast.NameTypeAstPair{"*dummy*", &ast.TypeWithName{token.Token{}, name}}}
	fn := &ast.PrsrFunction{NameSig: sig, NameRets: rtnSig, Body: &ast.BuiltInExpression{Name: name}, Number: DUMMY, Compiler: iz.cp, Tok: decTok}
	iz.Add(name, fn)
	if typeToClone == "int" || typeToClone == "float" {
		iz.p.Suffixes.Add(name)
		iz.p.Suffixes.Add(name + "?")
	}
	return typeNo, fn
}

func (iz *initializer) createOperations(nameAst ast.TypeNode, typeNo values.ValueType, opList []string, parentTypeNo values.ValueType, private bool, tok1 *token.Token) {
		for _, op := range opList {
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
					iz.Throw("init/request/float", tok1, op)
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
					iz.p.Throw("init/request/int", tok1, op)
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
					iz.Throw("init/request/list", tok1, op)
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
					iz.Throw("init/request/map", tok1, op)
				}
			case values.PAIR:
				iz.Throw("init/request/pair", tok1)
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
					iz.Throw("init/request/set", tok1, op)
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
					iz.Throw("init/request/string", tok1, op)
				}
			}
		}
		cloneData := iz.cp.Vm.ConcreteTypeInfo[typeNo].(vm.CloneType)
		cloneData.Using = opList
		iz.cp.Vm.ConcreteTypeInfo[typeNo] = cloneData
}

func (iz *initializer) getOpList(tokens *token.TokenizedCodeChunk) ([]string, ast.Node) {
	var opList []string
	var typeCheck ast.Node
	sep := tokens.NextToken()
	if sep.Type != token.EOF && sep.Type != token.COLON {
		if sep.Literal != "using" {
			iz.Throw("init/clone/using", &sep)
			return opList, typeCheck
		}
		for {
			op := tokens.NextToken()
			sep = tokens.NextToken()
			opList = append(opList, strings.Trim(op.Literal, "\n\r\t "))
			if sep.Type == token.COLON || sep.Type == token.EOF {
				break
			}
			if sep.Type != token.COMMA {
				iz.Throw("init/clone/comma", &sep)
				break
			}
		}
	}
	if sep.Type == token.COLON {
		iz.cp.P.TokenizedCode = tokens
		typeCheck = iz.cp.P.ParseTokenizedChunk()
	}
	return opList, typeCheck
}

func (iz *initializer) instantiateParameterizedTypes() {
loop:
	for _, dec := range iz.TokenizedDeclarations[makeDeclaration] {
		dec.ToStart()
		iz.cp.P.TokenizedCode = dec
		iz.cp.P.SafeNextToken()
		iz.cp.P.SafeNextToken()
		if iz.cp.P.PeekToken.Type == token.EOF {
			iz.Throw("init/make/empty", dec.IndexToken())
			continue loop
		}
		// First we slurp all the information we need out of the tokenized code.
		for {
			tok := iz.cp.P.CurToken
			ty := iz.cp.P.ParseType(parser.T_LOWEST)
			if ty == nil {
				iz.Throw("init/make/type", dec.IndexToken())
				continue loop
			}
			if ty, ok := ty.(*ast.TypeWithArguments); ok {
				argIndex := iz.cp.FindParameterizedType(ty.Name, ty.Values())
				if argIndex == DUMMY {
					iz.Throw("init/type/args", &tok)
					continue loop
				}
				parTypeInfo := iz.cp.ParameterizedTypes[ty.Name][argIndex]
				println("Type is", ty.String(), "with typecheck", parTypeInfo.Typecheck.String(), "and requests", len(parTypeInfo.Operations), "ops") 
				newEnv := compiler.NewEnvironment()
				for i, name := range parTypeInfo.Names {
					iz.cp.Reserve(ty.Arguments[i].Type, ty.Arguments[i].Value, &ty.Arguments[i].Token)
					newEnv.Data[name] = compiler.Variable{iz.cp.That(), compiler.LOCAL_CONSTANT, altType(ty.Arguments[i].Type)}
				}
				iz.cp.P.NextToken()
				newTypeName := ty.String()
				parentTypeNo, ok := parser.ClonableTypes[parTypeInfo.ParentType]
				if !ok {
					iz.Throw("init/clone/type", dec.IndexToken())
					return
				}
				// Now we have all our ducks in a row. We can compile the various bits 
				// of the new type using the same apparatus as we used for plain old 
				// clone types.

				typeNo, fn := iz.addTypeAndConstructor(newTypeName, parTypeInfo.ParentType, false, &tok)
				sig := ast.AstSig{ast.NameTypeAstPair{VarName: "x", VarType: ast.MakeAstTypeFrom(iz.cp.Vm.ConcreteTypeInfo[iz.cp.Vm.ConcreteTypeInfo[typeNo].(vm.CloneType).Parent].GetName(vm.DEFAULT))}}
				fn.Number = iz.addToBuiltins(sig, newTypeName, altType(typeNo), false, dec.IndexToken())
				println("Creating ops for", ty.String(), typeNo)
				iz.createOperations(ty, typeNo, parTypeInfo.Operations, 
					parentTypeNo, parTypeInfo.IsPrivate, &tok)

			} else {
				iz.Throw("init/make/instance", dec.IndexToken())
				continue loop
			}
			iz.cp.P.NextToken()
			switch iz.cp.P.CurToken.Type {
			case token.EOF:
				continue loop
			case token.COMMA:
			default:
				tok := iz.cp.P.CurToken
				iz.Throw("init/make/comma", &tok)
				continue loop
			}
		}
	}
}

// Function auxiliary to the previous one, to make constructors for the clone types.
func (iz *initializer) makeCloneFunction(fnName string, sig ast.AstSig, builtinTag string, rtnTypes compiler.AlternateType, rtnSig ast.AstSig, IsPrivate bool, pos uint32, tok *token.Token) {
	fn := &ast.PrsrFunction{Tok: tok, NameSig: sig, NameRets: rtnSig, Body: &ast.BuiltInExpression{*tok, builtinTag}, Compiler: iz.cp, Number: iz.addToBuiltins(sig, builtinTag, rtnTypes, IsPrivate, tok)}
	iz.Common.Functions[FuncSource{tok.Source, tok.Line, fnName, pos}] = fn
	conflictingFunction := iz.Add(fnName, fn)
	if conflictingFunction != nil && conflictingFunction != fn {
		iz.p.Throw("init/overload/c", tok, fnName, conflictingFunction.Tok)
	}
}

// We create the structs as names and type numbers in the type system. We can't populate
// their fields yet because we haven't even declared the abstract and interface types
// lexically yet.
func (iz *initializer) createStructNames() {
	iz.structDeclarationNumberToTypeNumber = map[int]values.ValueType{}
	// First we need to make the struct types into types so the parser parses them properly.
	for i := 0; i < len(iz.TokenizedDeclarations[structDeclaration]); i++ {
		indexToken := iz.TokenizedDeclarations[structDeclaration][i].IndexToken()
		name := indexToken.Literal
		iz.p.AllFunctionIdents.Add(name)
		iz.p.Functions.Add(name)
		typeNo := values.ValueType(len(iz.cp.Vm.ConcreteTypeInfo))
		typeInfo, typeExists := iz.getDeclaration(decSTRUCT, indexToken, DUMMY)
		if typeExists { // TODO --- can this in fact occur? Why?
			typeNo = typeInfo.(structInfo).structNumber
			typeInfo := iz.cp.Vm.ConcreteTypeInfo[typeNo].(vm.StructType)
			typeInfo.Path = iz.p.NamespacePath
			iz.cp.Vm.ConcreteTypeInfo[typeNo] = typeInfo
		} else {
			iz.cp.Vm.ConcreteTypeInfo = append(iz.cp.Vm.ConcreteTypeInfo, vm.StructType{}) // As a placeholder.
			iz.setDeclaration(decSTRUCT, indexToken, DUMMY, structInfo{typeNo, iz.IsPrivate(int(structDeclaration), i), nil})
		}
		iz.AddType(name, "struct", typeNo)
		iz.structDeclarationNumberToTypeNumber[i] = typeNo
		// The VM needs fast access to a few special types.
		if name == "Error" {
			iz.cp.Vm.UsefulTypes.UnwrappedError = typeNo
		}
		if name == "File" {
			iz.cp.Vm.UsefulTypes.File = typeNo
		}
		if name == "Terminal" {
			iz.cp.Vm.UsefulTypes.Terminal = typeNo
		}
		if name == "Output" {
			iz.cp.Vm.UsefulTypes.Output = typeNo
		}
	}
}

// We can now create the struct labels and define their type as an AstSig, though
// we can't make an abstract sig yet because we haven't populated the abstract types.
func (iz *initializer) createStructLabels() {
	for i, tcc := range iz.TokenizedDeclarations[structDeclaration] {
		indexToken := tcc.IndexToken()
		name := indexToken.Literal
		// We will now extract the AstSig lexically.
		sig := iz.cp.P.ParseSigFromTcc(tcc)
		labelsForStruct := make([]int, 0, len(sig))
		for j, labelNameAndType := range sig {
			labelName := labelNameAndType.VarName
			labelLocation, alreadyExists := iz.cp.Vm.FieldLabelsInMem[labelName]
			if alreadyExists { // Structs can of course have overlapping fields but we don't want to declare them twice.
				labelsForStruct = append(labelsForStruct, iz.cp.Vm.Mem[labelLocation].V.(int))
				iz.setDeclaration(decLABEL, indexToken, j, labelInfo{labelLocation, true}) // 'true' because we can't tell if it's private or not until we've defined all the structs.
			} else {
				iz.cp.Vm.FieldLabelsInMem[labelName] = iz.cp.Reserve(values.LABEL, len(iz.cp.Vm.Labels), indexToken)
				iz.setDeclaration(decLABEL, indexToken, j, labelInfo{iz.cp.That(), true})
				labelsForStruct = append(labelsForStruct, len(iz.cp.Vm.Labels))
				iz.cp.Vm.Labels = append(iz.cp.Vm.Labels, labelName)
				iz.cp.Common.LabelIsPrivate = append(iz.cp.Common.LabelIsPrivate, iz.IsPrivate(int(structDeclaration), i))
			}
		}
		typeNo := iz.structDeclarationNumberToTypeNumber[i]
		fnNo := iz.addToBuiltins(sig, name, altType(typeNo), iz.IsPrivate(int(structDeclaration), i), tcc.IndexToken())
		fn := &ast.PrsrFunction{NameSig: sig, Body: &ast.BuiltInExpression{Name: name}, Number: fnNo, Compiler: iz.cp, Tok: indexToken}
		iz.Add(name, fn)
		iz.setDeclaration(decSTRUCT, indexToken, DUMMY, structInfo{typeNo, iz.IsPrivate(int(structDeclaration), i), sig})
		stT := vm.StructType{Name: name, Path: iz.p.NamespacePath, LabelNumbers: labelsForStruct,
			Private: iz.IsPrivate(int(structDeclaration), i), IsMI: settings.MandatoryImportSet().Contains(indexToken.Source)}
		stT = stT.AddLabels(labelsForStruct)
		iz.cp.Vm.ConcreteTypeInfo[typeNo] = stT
	}
}

func (iz *initializer) registerParameterizedType(name string, ty *ast.TypeWithParameters, opList []string, typeCheck ast.Node, parentType string, private bool) bool {
	info, ok := iz.cp.ParameterizedTypes[name]
	if ok {
		if iz.paramTypeExists(ty) {
			return false
		}
	}
	thingToAdd := compiler.ParameterInfo{iz.astParamsToNames(ty.Parameters), 
		iz.astParamsToValueTypes(ty.Parameters), []compiler.ArgumentInfo{}, opList, 
		typeCheck, parentType, private}
	if ok {
		info = append(info, thingToAdd)
		iz.cp.ParameterizedTypes[name] = info
	} else {
		info = []compiler.ParameterInfo{thingToAdd}
		iz.cp.ParameterizedTypes[name] = info
		iz.cp.P.ParameterizedTypes = iz.cp.P.ParameterizedTypes.Add(name)
	}
	return true
}

func (iz *initializer) paramTypeExists(ty *ast.TypeWithParameters) bool {
	typesToMatch := iz.astParamsToValueTypes(ty.Parameters)
	for _, pty := range iz.cp.ParameterizedTypes[ty.Name] {
		if iz.ParameterTypesMatch(typesToMatch, pty.Types) {
			return true
		}
	}
	return false
}

func (iz *initializer) astParamsToValueTypes(params []*ast.Parameter) []values.ValueType {
	result := []values.ValueType{}
	for _, v := range params {
		result = append(result, iz.cp.ConcreteTypeNow(v.Type))
	}
	return result
}

func (iz *initializer) astParamsToNames(params []*ast.Parameter) []string {
	result := []string{}
	for _, v := range params {
		result = append(result, v.Name)
	}
	return result
}

func (iz *initializer) ParameterTypesMatch(paramsToCheck, paramsToMatch []values.ValueType) bool {
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

// Phase 1K of compilation. We create the abstract types as type names but don't populate them.
func (iz *initializer) createAbstractTypes() {
	for _, tcc := range iz.TokenizedDeclarations[abstractDeclaration] {
		tcc.ToStart()
		nameTok := tcc.NextToken()
		newTypename := nameTok.Literal
		tcc.NextToken() // The equals sign.
		tcc.NextToken() // The 'abstract' identifier.
		iz.p.TypeMap[newTypename] = values.MakeAbstractType()
		if settings.MandatoryImportSet().Contains(nameTok.Source) {
			iz.unserializableTypes.Add(nameTok.Literal)
		}
		for {
			typeTok := tcc.NextToken()
			divTok := tcc.NextToken()
			if typeTok.Type != token.IDENT {
				iz.Throw("init/type/form/b", &typeTok)
				break
			}
			if divTok.Type != token.EOF && !(divTok.Type == token.IDENT && divTok.Literal == "/") {
				iz.Throw("init/type/form/c", &typeTok)
				break
			}
			tname := typeTok.Literal
			abTypeToAdd, ok := iz.p.TypeMap[tname]
			if !ok {
				iz.Throw("init/type/known", &typeTok)
				break
			}
			iz.p.TypeMap[newTypename] = iz.p.TypeMap[newTypename].Union(abTypeToAdd)
			if divTok.Type == token.EOF {
				break
			}
		}
		iz.p.TypeMap[newTypename+"?"] = iz.p.TypeMap[newTypename].Insert(values.NULL)
		_, typeExists := iz.getDeclaration(decABSTRACT, &nameTok, DUMMY)
		if !typeExists {
			iz.setDeclaration(decABSTRACT, &nameTok, DUMMY, nil)
		}
		iz.p.Suffixes.Add(newTypename)
		iz.p.Suffixes.Add(newTypename + "?")
	}
}

// Phase 1L of compilation. Creates the interface types as names but doesn't populate them: parses the signatures
// of the functions in the interface definitions.
func (iz *initializer) createInterfaceTypes() {
	for _, tcc := range iz.TokenizedDeclarations[interfaceDeclaration] {
		tcc.ToStart()
		nameTok := tcc.NextToken()
		newTypename := nameTok.Literal
		if settings.MandatoryImportSet().Contains(nameTok.Source) {
			iz.unserializableTypes.Add(newTypename)
		}
		tcc.NextToken() // The equals sign. We know this must be the case from the MakeParserAndTokenizedProgram method putting it here.
		tcc.NextToken() // The 'interface' identifier. Ditto.
		if shouldBeColon := tcc.NextToken(); shouldBeColon.Type != token.COLON {
			iz.p.Throw("init/interface/colon", &shouldBeColon)
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
			iz.p.TokenizedCode = sig
			lhs := sig
			astOfSig := iz.p.ParseTokenizedChunk()
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
				functionName, _, astSig = iz.p.GetPartsOfSig(astOfSig.(*ast.PipingExpression).Left)
				retSig = iz.p.RecursivelySlurpReturnTypes(astOfSig.(*ast.PipingExpression).Right)
			} else {
				functionName, _, astSig = iz.p.GetPartsOfSig(astOfSig)
			}
			typeInfo = append(typeInfo, fnSigInfo{functionName, astSig, retSig})
			iz.addWordsToParser(lhs)
		}
		iz.p.TypeMap[newTypename] = values.MakeAbstractType() // We can't populate the interface types before we've parsed everything.
		iz.p.TypeMap[newTypename+"?"] = values.MakeAbstractType(values.NULL)
		_, typeExists := iz.getDeclaration(decINTERFACE, &nameTok, DUMMY)
		if !typeExists {
			iz.setDeclaration(decINTERFACE, &nameTok, DUMMY, interfaceInfo{typeInfo})
		}
		iz.p.Suffixes.Add(newTypename)
		iz.p.Suffixes.Add(newTypename + "?")
	}
}

// Phase 1P of compilation. We parse the snippet types, abstract types, clone types, constants, variables,
// functions, commands.
func (iz *initializer) parseEverythingElse() {
	for declarations := snippetDeclaration; declarations <= commandDeclaration; declarations++ {
		if declarations == cloneDeclaration || declarations == interfaceDeclaration { // TODO --- yeah, yeah, I am filled with shame.
			continue
		}
		for chunk := 0; chunk < len(iz.TokenizedDeclarations[declarations]); chunk++ {
			iz.p.TokenizedCode = iz.TokenizedDeclarations[declarations][chunk]
			iz.TokenizedDeclarations[declarations][chunk].ToStart()
			iz.ParsedDeclarations[declarations] = append(iz.ParsedDeclarations[declarations], iz.p.ParseTokenizedChunk())
		}
	}

	iz.p.AllFunctionIdents.AddSet(iz.p.Functions)
	iz.p.AllFunctionIdents.AddSet(iz.p.Prefixes)
	iz.p.AllFunctionIdents.AddSet(iz.p.Forefixes)
	iz.p.AllFunctionIdents.AddSet(iz.p.Midfixes)
	iz.p.AllFunctionIdents.AddSet(iz.p.Endfixes)
	iz.p.AllFunctionIdents.AddSet(iz.p.Infixes)
	iz.p.AllFunctionIdents.AddSet(iz.p.Suffixes)
	iz.p.AllFunctionIdents.AddSet(iz.p.Unfixes)

	iz.p.Bling.AddSet(iz.p.Forefixes)
	iz.p.Bling.AddSet(iz.p.Midfixes)
	iz.p.Bling.AddSet(iz.p.Endfixes)
}

// Phase 2. We find the shareable functions.
func (iz *initializer) findAllShareableFunctions() {
	// First we recursively call the method on all the dependencies of the module.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.findAllShareableFunctions()
	}
	// If at least one of the parameters of a function in the module has a parameter which
	// can only accept locally defined concrete types, then in principle this function might
	// fulfil an interface and be shared with the module that defines the interface.
	iz.findShareableFunctions()
}

// If a function in the module must have at least one of its parameters some type
// defined in the module, then we add an ast.PrsrFunction representation of it
// to the list of functions in the common initializer bindle.
func (iz *initializer) findShareableFunctions() {
	for j := functionDeclaration; j <= commandDeclaration; j++ {
		for i := 0; i < len(iz.ParsedDeclarations[j]); i++ {
			tok := iz.ParsedDeclarations[j][i].GetToken()
			functionName, position, sig, rTypes, body, given := iz.p.ExtractPartsOfFunction(iz.ParsedDeclarations[j][i])
			if body == nil {
				iz.p.Throw("init/func/body", tok)
				return
			}
			if iz.ErrorsExist() {
				return
			}
			functionToAdd := &ast.PrsrFunction{FName: functionName, NameSig: sig, Position: position, NameRets: rTypes, Body: body, Given: given,
				Cmd: j == commandDeclaration, Private: iz.IsPrivate(int(j), i), Number: DUMMY, Compiler: iz.cp, Tok: body.GetToken()}
			if iz.shareable(functionToAdd) || settings.MandatoryImportSet().Contains(tok.Source) {
				iz.Common.Functions[FuncSource{tok.Source, tok.Line, functionName, position}] = functionToAdd
				iz.fnIndex[fnSource{j, i}] = functionToAdd
			}
		}
	}
}

// Function auxiliary to the above. A function is shareable if at least one of its parameters must be of a type
// declared in the same module.
func (iz *initializer) shareable(f *ast.PrsrFunction) bool {
	for _, pair := range f.NameSig {
		ty := pair.VarType
		if _, ok := ty.(*ast.Bling); ok {
			continue
		}
		if t, ok := ty.(*ast.TypeDotDotDot); ok {
			ty = t.Right
		}
		if t, ok := ty.(*ast.TypeWithName); ok &&
			(t.Name == "struct" || t.Name == "enum") {
			continue
		}
		abType := iz.p.GetAbstractType(ty)
		ok := true
		for _, concType := range abType.Types {
			if !iz.localConcreteTypes.Contains(concType) {
				ok = false
			}
		}
		if ok {
			return true
		}
	}
	return false
}

func (iz *initializer) populateInterfaceTypes() {
	// First we recursively call the method on all the dependencies of the module.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.populateInterfaceTypes()
	}
	iz.addAbstractTypesToVm() // TODO --- this is the first of two times we're going to do this when what we really need is another topological sort.
	// We pull in all the shared functions that fulfill the interface types, populating the types as we go.
	for _, tcc := range iz.TokenizedDeclarations[interfaceDeclaration] {
		tcc.ToStart()
		nameTok := tcc.NextToken()
		typename := nameTok.Literal
		typeInfo, _ := iz.getDeclaration(decINTERFACE, &nameTok, DUMMY)
		types := values.MakeAbstractType()
		funcsToAdd := map[values.ValueType][]*ast.PrsrFunction{}
		for i, sigToMatch := range typeInfo.(interfaceInfo).sigs {
			typesMatched := values.MakeAbstractType()
			for key, fnToTry := range iz.Common.Functions {
				if key.FunctionName == sigToMatch.name {
					abSig := fnToTry.Compiler.(*compiler.Compiler).P.MakeAbstractSigFromStringSig(fnToTry.NameSig)
					abRets := fnToTry.Compiler.(*compiler.Compiler).P.MakeAbstractSigFromStringSig(fnToTry.NameRets)
					matches := iz.getMatches(sigToMatch, abSig, abRets, fnToTry, &nameTok)
					typesMatched = typesMatched.Union(matches)
					if !settings.MandatoryImportSet().Contains(fnToTry.Tok.Source) {
						for _, ty := range matches.Types {
							if _, ok := funcsToAdd[ty]; ok {
								funcsToAdd[ty] = append(funcsToAdd[ty], fnToTry)
							} else {
								funcsToAdd[ty] = []*ast.PrsrFunction{fnToTry}
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
		iz.p.TypeMap[typename] = types
		typesWithNull := types.Insert(values.NULL)
		iz.p.TypeMap[typename+"?"] = typesWithNull
		iz.AddTypeToVm(values.AbstractTypeInfo{typename, iz.p.NamespacePath, types, settings.MandatoryImportSet().Contains(nameTok.Source)})
		// And we add all the implicated functions to the function table.
		for _, ty := range types.Types {
			for _, fn := range funcsToAdd[ty] {
				conflictingFunction := iz.Add(fn.FName, fn)
				if conflictingFunction != nil && conflictingFunction != fn {
					iz.p.Throw("init/overload/b", fn.Tok, fn.FName, conflictingFunction.Tok)
				}
			}
		}
	}
}

func (iz *initializer) populateAbstractTypes() {
	// First we recursively call the method on all the dependencies of the module.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.populateAbstractTypes()
	}
	// The vm needs to know how to describe the abstract types in words.
	iz.addAbstractTypesToVm()
	if iz.ErrorsExist() {
		return
	}

	// The compiler uses a somewhat richer type representation than the one used by the compiler and the
	// runtime.
	iz.makeAlternateTypesFromAbstractTypes()
}

// Phase 2A of compilation. We add the abstract types to the VM.
//
// The service.Vm doesn't *use* abstract types, but they are what values of type TYPE contain, and so it needs to
// be able to describe them.
func (iz *initializer) addAbstractTypesToVm() {
	// For consistent results for tests, it is desirable that the types should be listed in a fixed order.
	keys := []string{}
	for typeName, _ := range iz.p.TypeMap {
		keys = append(keys, typeName)
	}
	for typeName, _ := range iz.p.Common.Types {
		keys = append(keys, typeName)
	}
	sort.Slice(keys, func(i, j int) bool { return keys[i] < keys[j] })
	for _, typeName := range keys {
		iz.AddTypeToVm(values.AbstractTypeInfo{Name: typeName, Path: iz.p.NamespacePath,
			AT: iz.p.GetAbstractTypeFromTypeSys(typeName), IsMI: iz.unserializableTypes.Contains(typeName)})
	}
}

// Phase 2B of compilation. We make the alternate types from the abstract types, because the compiler
// is shortly going to need them.
//
// OTOH, we want the type information spread across the parsers and shared in the Common parser bindle to
// collectively be the any source of truth for our type system.
// But it can't be the only *representation* of the truth, becase that would slow things down 'cos the compiler
// would have to keep converting abstract types to alternate types to build the type schemes with.
// The solution is to build the alternate type schemes once and for all from the alternate types, after we've
// entirely finished generating the data in the parsers.
func (iz *initializer) makeAlternateTypesFromAbstractTypes() {
	iz.cp.TypeNameToTypeScheme = make(map[string]compiler.AlternateType)
	for typename, abType := range iz.p.TypeMap {
		iz.cp.TypeNameToTypeScheme[typename] = compiler.AbstractTypeToAlternateType(abType)
	}
	for typename, abType := range iz.p.Common.Types {
		iz.cp.TypeNameToTypeScheme[typename] = compiler.AbstractTypeToAlternateType(abType)
	}
}


// Function auxiliary to the above and to `makeCloneFunction` which adds the constructors to the builtins.
func (iz *initializer) addToBuiltins(sig ast.AstSig, builtinTag string, returnTypes compiler.AlternateType, private bool, tok *token.Token) uint32 {
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

// Phase 4 of compilation. At this point we have our functions as parsed code chunks in the
// `uP.Parser.ParsedDeclarations(<function/command>Declaration)` slice. We want to read their signatures
// and order them according to specificity for the purposes of implementing overloading.
func (iz *initializer) MakeFunctionTables() {
	// First we recursively call the method on all the dependencies of the module.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.MakeFunctionTables()
	}
	iz.makeFunctionTable()
	if settings.FUNCTION_TO_PEEK != "" {
		println("In namespace", iz.p.NamespacePath)
		println(iz.p.FunctionTable.Describe(iz.p, settings.FUNCTION_TO_PEEK))
	}
}

// Function auxillary to the above for making one function table.
func (iz *initializer) makeFunctionTable() {
	for j := functionDeclaration; j <= commandDeclaration; j++ {
		for i := 0; i < len(iz.ParsedDeclarations[j]); i++ {
			tok := iz.ParsedDeclarations[j][i].GetToken()
			functionName, position, sig, rTypes, body, given := iz.p.ExtractPartsOfFunction(iz.ParsedDeclarations[j][i])
			var (
				ok            bool
				functionToAdd *ast.PrsrFunction
			)
			if functionToAdd, ok = iz.Common.Functions[FuncSource{tok.Source, tok.Line, functionName, position}]; ok {
			} else {
				// TODO --- this is vile shotgun parsing and is probably duplicated elsewhere.
				if body == nil {
					iz.p.Throw("init/func/body", tok)
					return
				}
				if iz.ErrorsExist() {
					return
				}
				functionToAdd = &ast.PrsrFunction{FName: functionName, NameSig: sig, Position: position, NameRets: rTypes, Body: body, Given: given,
					Cmd: j == commandDeclaration, Private: iz.IsPrivate(int(j), i), Number: DUMMY, Compiler: iz.cp, Tok: body.GetToken()}
			}
			iz.fnIndex[fnSource{j, i}] = functionToAdd
			conflictingFunction := iz.Add(functionName, functionToAdd)
			if conflictingFunction != nil && conflictingFunction != functionToAdd {
				iz.p.Throw("init/overload/a", body.GetToken(), functionName, conflictingFunction.Tok)
				return
			}
		}
	}
}

func (iz *initializer) MakeFunctionForests() {
	// First we recurse.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.MakeFunctionForests()
	}

	// Now we turn the function tables into a different data structure, a "function tree" with its branches labeled
	// with types. Following it tells us which version of an overloaded function to use.
	iz.MakeFunctionTrees()
	if tree, ok := iz.p.FunctionForest[settings.FUNCTION_TO_PEEK]; ok && settings.FUNCTION_TO_PEEK != "" {
		println("In namespace", iz.p.NamespacePath, "function tree for "+settings.FUNCTION_TO_PEEK)
		println(tree.Tree.IndentString("") + "\n")
	}
}

// Function auxiliary to the above. Having made the parsers FunctionTable, each function name is associated with a
// (partially) ordered list of associated functions such that a more specific type signature comes before a less
// specific one. We will now re-represent this as a tree.
func (iz *initializer) MakeFunctionTrees() {
	iz.p.FunctionForest = map[string]*ast.FunctionTree{}
	rc := 0
	for k, v := range iz.p.FunctionTable {
		tree := &ast.FnTreeNode{Fn: nil, Branch: []*ast.TypeNodePair{}}
		for i := range v {
			tree = iz.addSigToTree(tree, v[i], 0)

			refs := 0 // Overloaded functions must have the same number of reference variables, which go at the start.
			for ; refs < len(v[i].NameSig) && ast.IsRef(v[i].NameSig[refs].VarType); refs++ {
			}
			if i == 0 {
				rc = refs
			} else {
				if refs != rc {
					iz.p.Throw("init/overload/ref", v[i].Body.GetToken())
					break
				}
			}
		}
		iz.p.FunctionForest[k] = &ast.FunctionTree{Tree: tree, RefCount: rc}
	}
}

// Note that the sigs have already been sorted on their specificity.
func (iz *initializer) addSigToTree(tree *ast.FnTreeNode, fn *ast.PrsrFunction, pos int) *ast.FnTreeNode {
	nameSig := fn.NameSig // TODO --- do we really need both of these?
	sig := fn.Compiler.(*compiler.Compiler).P.MakeAbstractSigFromStringSig(nameSig)
	if pos < len(sig) {
		var currentTypeName string
		currentAbstractType := sig[pos].VarType
		if _, ok := nameSig[pos].VarType.(*ast.Bling); ok {
			currentTypeName = nameSig[pos].VarName
		} else {
			currentTypeName = nameSig[pos].VarType.String()
		}
		isVararg := len(currentTypeName) >= 3 && currentTypeName[:3] == "..."
		if isVararg {
			currentTypeName = currentTypeName[3:]
		}
		isPresent := false
		for _, v := range tree.Branch {
			if currentAbstractType.Equals(v.Type) {
				isPresent = true
				break
			}
		}
		if !isPresent {
			tree.Branch = append(tree.Branch, &ast.TypeNodePair{Type: currentAbstractType, IsVararg: isVararg, Node: &ast.FnTreeNode{Fn: nil, Branch: []*ast.TypeNodePair{}}})
		}
		for _, branch := range tree.Branch {
			if branch.Type.IsSubtypeOf(currentAbstractType) {
				branch.Node = iz.addSigToTree(branch.Node, fn, pos+1)
				if (currentTypeName == "tuple") && !(branch.Type.Contains(values.TUPLE)) {
					iz.addSigToTree(branch.Node, fn, pos)
				}
				if isVararg && !branch.IsVararg {
					iz.addSigToTree(branch.Node, fn, pos)
				}
			}
		}
	} else {
		if tree.Fn == nil { // If it is non-nil then a sig of greater specificity has already led us here and we're good.
			tree.Branch = append(tree.Branch, &ast.TypeNodePair{Type: values.MakeAbstractType(), Node: &ast.FnTreeNode{Fn: fn, Branch: []*ast.TypeNodePair{}}})
		}
	}
	return tree
}

// Phase 3-1/2 of compilation.

// We assign abstract types to the fields of the structs, and chek for consistency of
// private types, i.e. a struct type declared public can't have field types declared private.
func (iz *initializer) AddFieldsToStructsAndCheckForConsistency() {
	// First we recurse.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.MakeFunctionForests()
	}
	iz.cmI("Adding abstract types of fields to structs.")
	iz.addFieldsToStructs()
	if iz.ErrorsExist() {
		return
	}
	// We want to ensure that no public type (whether a struct or abstract type) contains a private type.
	iz.cmI("Checking types for consistency of encapsulation.")
	iz.checkTypesForConsistency()
	if iz.ErrorsExist() {
		return
	}
}

// Phase 3-1/2A, formerly phase 1M of compilation. Adds field types to structs.
func (iz *initializer) addFieldsToStructs() {
	for i, tcc := range iz.TokenizedDeclarations[structDeclaration] {
		izTypeInfo, _ := iz.getDeclaration(decSTRUCT, tcc.IndexToken(), DUMMY)
		izStructInfo := izTypeInfo.(structInfo)
		typeNumber := iz.structDeclarationNumberToTypeNumber[i]
		structInfo := iz.cp.Vm.ConcreteTypeInfo[typeNumber].(vm.StructType)
		sig := izStructInfo.sig
		structTypes := make([]values.AbstractType, 0, len(sig))
		for _, labelNameAndType := range sig {
			typeAst := labelNameAndType.VarType
			abType := iz.p.GetAbstractType(typeAst)
			structTypes = append(structTypes, abType)
		}
		structInfo.AbstractStructFields = structTypes
		iz.cp.Vm.ConcreteTypeInfo[typeNumber] = structInfo
	}
}

// Phase 3-1/2B, formerly phase 1N of compilation. We check that if a struct type is public, so are its fields.
func (iz *initializer) checkTypesForConsistency() {
	for typeNumber := int(values.FIRST_DEFINED_TYPE); typeNumber < len(iz.cp.Vm.ConcreteTypeInfo); typeNumber++ {
		if !iz.cp.Vm.ConcreteTypeInfo[typeNumber].IsStruct() {
			continue
		}
		if !iz.cp.Vm.ConcreteTypeInfo[typeNumber].IsPrivate() {
			for _, ty := range iz.cp.Vm.ConcreteTypeInfo[typeNumber].(vm.StructType).AbstractStructFields {
				if iz.cp.IsPrivate(ty) {
					iz.Throw("init/private/struct", &token.Token{}, iz.cp.Vm.ConcreteTypeInfo[typeNumber], iz.cp.Vm.DescribeAbstractType(ty, vm.LITERAL))
				}
			}
		}
	}

	for i, dec := range iz.TokenizedDeclarations[abstractDeclaration] {
		if iz.IsPrivate(int(abstractDeclaration), i) {
			continue
		}
		dec.ToStart()
		tok := dec.NextToken()
		name := tok.Literal
		abType := iz.p.GetAbstractTypeFromTypeSys(name)
		for _, w := range abType.Types {
			if iz.cp.Vm.ConcreteTypeInfo[w].IsPrivate() {
				iz.Throw("init/private/abstract", &tok, name)
			}
		}

	}
}

// Phase 3-3/4 of compilation.
// We slurp the functions and converters out of the .so files, if necessary building or rebuilding
// the .so files first.

func (iz *initializer) compileGoModules() {
	// First of all, the recursion.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.compileGoModules()
	}

	iz.compileGo() // This is in 'gohandler.go' in this package.
}

// Phase 4 of compilation. We compile the constants, variables, functions, and commands.
func (iz *initializer) CompileEverything() [][]labeledParsedCodeChunk { // TODO --- do we do anything with the return type?
	// First of all, the recursion.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.CompileEverything()
	}
	// And now we compile the module.
	//
	// First we need to do a big topological sort on everything, according to the following rules:
	// * A function, variable or constant can't depend on a command.
	// * A constant can't depend on a variable.
	// * A variable or constant can't depend on itself.
	iz.cmI("Mapping variable names to the parsed code chunks in which they occur.")
	iz.cp.GlobalVars.Ext = iz.cp.GlobalConsts
	namesToDeclarations := map[string][]labeledParsedCodeChunk{}
	result := [][]labeledParsedCodeChunk{}
	for dT := constantDeclaration; dT <= variableDeclaration; dT++ {
		for i, dec := range iz.ParsedDeclarations[dT] {
			if _, ok := dec.(*ast.AssignmentExpression); !ok {
				iz.p.Throw("init/assign", dec.GetToken())
				continue
			}
			names := iz.p.GetVariablesFromSig(dec.(*ast.AssignmentExpression).Left)
			for _, name := range names {
				existingName, alreadyExists := namesToDeclarations[name]
				if alreadyExists {
					iz.p.Throw("init/name/exists/a", dec.GetToken(), iz.ParsedDeclarations[existingName[0].decType][existingName[0].decNumber].GetToken(), name)
					return nil
				}
				namesToDeclarations[name] = []labeledParsedCodeChunk{{dec, dT, i, name}}
			}
		}
	}
	iz.cmI("Mapping names of functions to their declarations.")
	for dT := functionDeclaration; dT <= commandDeclaration; dT++ {
		for i, dec := range iz.ParsedDeclarations[dT] {
			name, _, _, _, _, _ := iz.p.ExtractPartsOfFunction(dec) // TODO --- refactor ExtractPartsOfFunction so there's a thing called ExtractNameOfFunction which you can call there and here.
			_, alreadyExists := namesToDeclarations[name]
			if alreadyExists {
				names := namesToDeclarations[name]
				for _, existingName := range names {
					if existingName.decType == variableDeclaration || existingName.decType == constantDeclaration { // We can't redeclare variables or constants.
						iz.p.Throw("init/name/exists/b", dec.GetToken(), iz.ParsedDeclarations[existingName.decType][existingName.decNumber].GetToken(), name)
					}
					if existingName.decType == functionDeclaration && dT == commandDeclaration { // We don't want to overload anything so it can be both a command and a function 'cos that would be weird.
						iz.p.Throw("init/name/exists/c", dec.GetToken(), iz.ParsedDeclarations[existingName.decType][existingName.decNumber].GetToken(), name)
					}
				}
				namesToDeclarations[name] = append(names, labeledParsedCodeChunk{dec, dT, i, name})
			} else {
				namesToDeclarations[name] = []labeledParsedCodeChunk{{dec, dT, i, name}}
			}
		}
	}
	iz.cmI("Adding struct typechecks to declarations.")
	// Since the name of a type will appear already in the map as the name of the function
	// constructing it, we'll mangle the names by adding a `*` to the front of each.
	for i, tcc := range iz.TokenizedDeclarations[structDeclaration] {
		tcc.ToStart()
		iz.cp.P.TokenizedCode = tcc
		if iz.cp.P.SeekColon() {
			ast := iz.cp.P.ParseTokenizedChunk()
			name := "*" + tcc.IndexToken().Literal
			namesToDeclarations[name] = []labeledParsedCodeChunk{{ast, structDeclaration, i, name[1:]}}
		}
	}
	iz.cmI("Adding clone typechecks to declarations.")
	// Since the name of a type will appear already in the map as the name of the function
	// constructing it, we'll mangle the names by adding a `*` to the front of each.
	for i, tcc := range iz.TokenizedDeclarations[cloneDeclaration] {
		tcc.ToStart()
		name := "*" + tcc.IndexToken().Literal
		iz.cp.P.TokenizedCode = tcc
		if iz.cp.P.SeekColon() {
			typecheck := iz.cp.P.ParseTokenizedChunk()
			namesToDeclarations[name] = []labeledParsedCodeChunk{{typecheck, cloneDeclaration, i, name[1:]}}
		}
	}

	iz.cmI("Building digraph of dependencies.")
	// We build a digraph of the dependencies between the constant/variable/function/command declarations.
	graph := dtypes.Digraph[string]{}
	for name, decs := range namesToDeclarations { // The same name may be used for different overloaded functions.
		graph.Add(name, []string{})
		for _, dec := range decs {
			rhsNames := iz.extractNamesFromCodeChunk(dec)
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
								iz.p.Throw("init/depend/cmd", dec.chunk.GetToken())
								return nil
							}
							if rhsDec.decType == variableDeclaration && dec.decType == constantDeclaration {
								iz.p.Throw("init/depend/var", dec.chunk.GetToken())
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
	iz.cmI("Initializing service variables.")
	// $We need a few bits and pieeces to assemble the types and content of the variables.
	loggingOptionsType, _ := iz.cp.GetConcreteType("$Logging")
	outputOptionsType, _ := iz.cp.GetConcreteType("$OutputAs")
	outputStructType, _ := iz.cp.GetConcreteType("Output")
	terminalStructType, _ := iz.cp.GetConcreteType("Terminal")
	fileStructType, _ := iz.cp.GetConcreteType("File")
	logToTypes := altType(outputStructType, terminalStructType, fileStructType)
	dir, _ := os.Getwd()
	cliArgs := vector.Empty
	if len(os.Args) >= 2 {
		firstArg := 2
		if os.Args[1] == "run" {
			firstArg = 3
		}
		if len(os.Args) > firstArg {
			for _, v := range os.Args[firstArg:] {
				cliArgs = cliArgs.Conj(val(values.STRING, v))
			}
		}
	}

	serviceVariables := map[string]serviceVariableData{
		"$logging":         {loggingOptionsType, 1, altType(loggingOptionsType)},
		"$logTo":           {terminalStructType, []values.Value{}, logToTypes},
		"$outputAs":        {outputOptionsType, 1, altType(outputOptionsType)},
		"$cliDirectory":    {values.STRING, dir, altType(values.STRING)},
		"$cliArguments":    {values.LIST, cliArgs, altType(values.LIST)},
		"$moduleDirectory": {values.STRING, filepath.Dir(iz.cp.ScriptFilepath), altType(values.STRING)},
		"$hub":             {values.MAP, iz.Common.HubStore, altType(values.MAP)},
	}
	// Service variables which tell the compiler how to compile things must be
	// set before we compile the functions, and so can't be calculated but must
	// be literal.
	compilerDirectives := dtypes.MakeFromSlice([]string{"$logging", "$logTo"})
	// Add variables to environment.
	for svName, svData := range serviceVariables {
		rhs, ok := graph[svName]
		if ok && compilerDirectives.Contains(svName) { // Then we've declared a service variable which is also a compiler directive, and must compile the declaration.
			tok := namesToDeclarations[svName][0].chunk.GetToken()
			decType := namesToDeclarations[svName][0].decType
			decNumber := namesToDeclarations[svName][0].decNumber
			if len(rhs) > 0 {
				iz.p.Throw("init/service/depends", tok, svName)
				return nil
			}
			iz.compileGlobalConstantOrVariable(decType, decNumber)
			if !svData.alt.Contains(iz.cp.Vm.Mem[iz.cp.That()].T) {
				iz.p.Throw("init/service/type", tok, svName, compiler.Describe(svData.alt, iz.cp.Vm))
				return nil
			}
			delete(graph, svName)
		} else if !ok { // Then the service variable isn't declared, and we need to stick in a default value.
			dummyTok := token.Token{}
			iz.cp.Reserve(svData.t, svData.v, &dummyTok)
			iz.cp.AddVariable(iz.cp.GlobalVars, svName, compiler.GLOBAL_VARIABLE_PRIVATE, svData.alt, &dummyTok)
		}
		// The third possibility here is that we've declared a service variable which isn't
		// a compiler directive. In that case, it can be compiled in the usual way.
	}
	iz.cp.Vm.UsefulValues.OutputAs = iz.cp.GlobalVars.Data["$outputAs"].MLoc
	iz.cmI("Performing sort on digraph.")
	order := graph.Tarjan()

	// We now have a list of lists of names to declare. We're off to the races!
	iz.cmI("Compiling the variables/functions in the order given by the sort.")
	for _, namesToDeclare := range order { // 'namesToDeclare' is one Tarjan partition.
		groupOfDeclarations := []labeledParsedCodeChunk{}
		for _, nameToDeclare := range namesToDeclare {
			groupOfDeclarations = append(groupOfDeclarations, namesToDeclarations[nameToDeclare]...)
		}
		// If the declaration type is constant or variable it must be the only member of its Tarjan partion and there must only be one thing of that name.
		if groupOfDeclarations[0].decType == constantDeclaration || groupOfDeclarations[0].decType == variableDeclaration {
			iz.compileGlobalConstantOrVariable(groupOfDeclarations[0].decType, groupOfDeclarations[0].decNumber)
			continue
		}
		// So we have a group of functions/commands (but not both) which need to be declared together because either they have the same name or they
		// have a recursive relationship, or both.
		// We can't tell before we compile the group whether there is a recursive relationship in there, because we don't know how the dispatch is going to
		// shake out. E.g. suppose we have a type 'Money = struct(dollars, cents int)' and we wish to implement '+'. We will of course do it using '+' for ints.
		// This will not be recursion, but before we get that far we won't be able to tell whether it is or not.
		iz.cp.RecursionStore = []compiler.BkRecursion{} // The compiler will put all the places it needs to backtrack for recursion here.
		fCount := uint32(len(iz.cp.Fns))                // We can give the function data in the parser the right numbers for the group of functions in the parser before compiling them, since we know what order they come in.
		for _, dec := range groupOfDeclarations {
			if dec.decType == functionDeclaration || dec.decType == commandDeclaration {
				iz.fnIndex[fnSource{dec.decType, dec.decNumber}].Number = fCount
				iz.fnIndex[fnSource{dec.decType, dec.decNumber}].Compiler = iz.cp
				fCount++
			}
		}
		loop:
		for i, dec := range groupOfDeclarations {
			switch dec.decType {
			case structDeclaration, cloneDeclaration:
				if _, ok := iz.getDeclaration(decPARAMETERIZED, iz.TokenizedDeclarations[dec.decType][i].IndexToken(), DUMMY); ok {
					continue loop
				}
				iz.compileTypecheck(dec.name, dec.chunk)
				continue
			case functionDeclaration:
				iz.compileFunction(dec.chunk, iz.IsPrivate(int(dec.decType), dec.decNumber), iz.cp.GlobalConsts, functionDeclaration)
			case commandDeclaration:
				iz.compileFunction(dec.chunk, iz.IsPrivate(int(dec.decType), dec.decNumber), iz.cp.GlobalVars, commandDeclaration)
			}
			iz.fnIndex[fnSource{dec.decType, dec.decNumber}].Number = uint32(len(iz.cp.Fns) - 1) // TODO --- is this necessary given the line a little above which seems to do this pre-emptively?
		}
		// We've reached the end of the group and can go back and put the recursion in.

		if iz.ErrorsExist() {
			continue
		}

		for _, rDat := range iz.cp.RecursionStore {
			funcNumber := rDat.FunctionNumber
			addr := rDat.Address
			iz.cp.Vm.Code[addr].Args[0] = iz.cp.Fns[funcNumber].CallTo
			iz.cp.Vm.Code[addr].Args[1] = iz.cp.Fns[funcNumber].LoReg
			iz.cp.Vm.Code[addr].Args[2] = iz.cp.Fns[funcNumber].HiReg
			iz.cp.Vm.Code[addr+2].Args[1] = iz.cp.Fns[funcNumber].OutReg
		}
	}
	iz.cmI("Calling 'init' if it exists.")
	iz.cp.CallIfExists("init")
	return result
}

type serviceVariableData struct {
	t   values.ValueType
	v   any
	alt compiler.AlternateType
}

// Function auxiliary to the above for compiling constant and variable declarations.
func (iz *initializer) compileGlobalConstantOrVariable(declarations declarationType, v int) {
	dec := iz.ParsedDeclarations[declarations][v]
	iz.cp.Cm("Compiling assignment "+dec.String(), dec.GetToken())
	lhs := dec.(*ast.AssignmentExpression).Left
	rhs := dec.(*ast.AssignmentExpression).Right
	sig, _ := iz.p.RecursivelySlurpSignature(lhs, ast.INFERRED_TYPE_AST)
	if iz.ErrorsExist() {
		return
	}
	rollbackTo := iz.cp.GetState() // Unless the assignment generates code, i.e. we're creating a lambda function or a snippet, then we can roll back the declarations afterwards.
	ctxt := compiler.Context{Env: iz.cp.GlobalVars, Access: compiler.INIT, LowMem: DUMMY, TrackingFlavor: compiler.LF_INIT}
	iz.cp.CompileNode(rhs, ctxt)
	if iz.ErrorsExist() {
		return
	}
	iz.cp.Emit(vm.Ret)
	iz.cp.Cm("Calling Run from initializer's compileGlobalConstantOrVariable method.", dec.GetToken())
	iz.cp.Vm.Run(uint32(rollbackTo.Code))
	result := iz.cp.Vm.Mem[iz.cp.That()]
	if !iz.cp.Common.CodeGeneratingTypes.Contains(result.T) { // We don't want to roll back the code generated when we make a lambda or a snippet.
		iz.cp.Rollback(rollbackTo, dec.GetToken())
	}

	envToAddTo, vAcc := iz.getEnvAndAccessForConstOrVarDeclaration(declarations, v)

	last := len(sig) - 1
	t, ok := sig[last].VarType.(*ast.TypeWithName)
	lastIsTuple := ok && t.Name == "tuple"
	rhsIsTuple := result.T == values.TUPLE
	tupleLen := 1
	if rhsIsTuple {
		tupleLen = len(result.V.([]values.Value))
	}
	if !lastIsTuple && tupleLen > len(sig) {
		iz.p.Throw("comp/assign/a", dec.GetToken(), tupleLen, len(sig))
		return
	}
	if !lastIsTuple && tupleLen < len(sig) {
		iz.p.Throw("comp/assign/b", dec.GetToken(), tupleLen, len(sig))
		return
	}
	if lastIsTuple && tupleLen < len(sig)-1 {
		iz.p.Throw("comp/assign/c", dec.GetToken(), tupleLen, len(sig))
		return
	}
	loopTop := len(sig)
	head := []values.Value{result}
	if lastIsTuple {
		loopTop = last
		if rhsIsTuple {
			head = result.V.([]values.Value)[:last]
			iz.cp.Reserve(values.TUPLE, result.V.([]values.Value)[last:], rhs.GetToken())
		} else {
			if tupleLen == len(sig)-1 {
				iz.cp.Reserve(values.TUPLE, []values.Value{}, rhs.GetToken())
			} else {
				iz.cp.Reserve(values.TUPLE, result.V, rhs.GetToken())
			}
		}
		iz.cp.AddVariable(envToAddTo, sig[last].VarName, vAcc, altType(values.TUPLE), rhs.GetToken())
	} else {
		if rhsIsTuple {
			head = result.V.([]values.Value)
		}
	}
	for i := 0; i < loopTop; i++ {
		iz.cp.Reserve(head[i].T, head[i].V, rhs.GetToken())
		if sig[i].VarType == ast.INFERRED_TYPE_AST {
			iz.cp.AddVariable(envToAddTo, sig[i].VarName, vAcc, altType(head[i].T), rhs.GetToken())
		} else {
			allowedTypes := iz.cp.GetAlternateTypeFromTypeAst(sig[i].VarType)
			if allowedTypes.IsNoneOf(head[i].T) {
				iz.p.Throw("comp/assign/type/a", dec.GetToken(), sig[i].VarName, iz.cp.GetTypeNameFromNumber(head[i].T))
				return
			} else {
				iz.cp.AddVariable(envToAddTo, sig[i].VarName, vAcc, allowedTypes, rhs.GetToken())
			}
		}
	}
}

// Function auxiliary to the above.
func (iz *initializer) getEnvAndAccessForConstOrVarDeclaration(dT declarationType, i int) (*compiler.Environment, compiler.VarAccess) {
	IsPrivate := iz.IsPrivate(int(dT), i)
	var vAcc compiler.VarAccess
	envToAddTo := iz.cp.GlobalConsts
	if dT == constantDeclaration {
		if IsPrivate {
			vAcc = compiler.GLOBAL_CONSTANT_PRIVATE
		} else {
			vAcc = compiler.GLOBAL_CONSTANT_PUBLIC
		}
	} else {
		envToAddTo = iz.cp.GlobalVars
		if IsPrivate {
			vAcc = compiler.GLOBAL_VARIABLE_PRIVATE
		} else {
			vAcc = compiler.GLOBAL_VARIABLE_PUBLIC
		}
	}
	return envToAddTo, vAcc
}

// Method for compiling the runtime typechecks on structs and clones
func (iz *initializer) compileTypecheck(name string, node ast.Node) {
	typeNumber, _ := iz.cp.GetConcreteType(name)
	typeInfo := iz.cp.TypeInfoNow(name)
	thatType := typeNumber
	if typeInfo.IsClone() {
		thatType = typeInfo.(vm.CloneType).Parent
	}
	iz.cmI("Compiling typecheck for '" + name + "'")
	callAddress := iz.cp.CodeTop()
	info := iz.cp.Vm.ConcreteTypeInfo[typeNumber]
	resultLoc := iz.cp.Reserve(values.UNDEFINED_TYPE, nil, node.GetToken())
	tokNumberLoc := iz.cp.Reserve(values.UNDEFINED_TYPE, nil, node.GetToken())
	newEnv := compiler.NewEnvironment()
	newEnv.Ext = iz.cp.GlobalConsts
	inLoc := iz.cp.Reserve(values.UNDEFINED_TYPE, nil, node.GetToken())
	iz.cp.AddVariable(newEnv, "that", compiler.VERY_LOCAL_VARIABLE, altType(thatType), node.GetToken())
	chunks := iz.cp.SplitOnNewlines(node)
	for _, chunk := range chunks {
		context := compiler.Context{Env: newEnv}
		rTypes, _ := iz.cp.CompileNode(chunk, context)
		if !rTypes.Contains(values.BOOL) {
			iz.Throw("init/typecheck/bool", chunk.GetToken(), iz.cp.P.PrettyPrint(chunk), name)
		}
		errNo := iz.cp.ReserveTypeCheckError(chunk, name, inLoc)
		iz.cp.Emit(vm.Chck, resultLoc, iz.cp.That(), tokNumberLoc, errNo) // This will do its own early return from the typecheck.
	}
	iz.cp.Emit(vm.Ret)
	typeCheck := &vm.TypeCheck{CallAddress: callAddress, InLoc: inLoc, ResultLoc: resultLoc, TokNumberLoc: tokNumberLoc}
	if info.IsClone() {
		info = info.(vm.CloneType).AddTypeCheck(typeCheck)
	} else {
		info = info.(vm.StructType).AddTypeCheck(typeCheck)
	}
	iz.cp.Vm.ConcreteTypeInfo[typeNumber] = info
}

// Method for compiling a top-level function.
func (iz *initializer) compileFunction(node ast.Node, private bool, outerEnv *compiler.Environment, dec declarationType) *compiler.CpFunc {
	if info, functionExists := iz.getDeclaration(decFUNCTION, node.GetToken(), DUMMY); functionExists {
		iz.cp.Fns = append(iz.cp.Fns, info.(*compiler.CpFunc))
		return info.(*compiler.CpFunc)
	}
	cpF := compiler.CpFunc{}
	var ac compiler.CpAccess
	if dec == functionDeclaration {
		ac = compiler.DEF
	} else {
		ac = compiler.CMD
		cpF.Command = true
	}
	cpF.Private = private
	functionName, _, sig, rtnSig, body, given := iz.p.ExtractPartsOfFunction(node)
	iz.cp.Cm("Compiling function '"+functionName+"' with sig "+sig.String()+".", body.GetToken())
	if iz.ErrorsExist() {
		return nil
	}
	if body.GetToken().Type == token.XCALL {
		Xargs := body.(*ast.PrefixExpression).Args
		cpF.Xcall = &compiler.XBindle{ExternalServiceOrdinal: uint32(Xargs[0].(*ast.IntegerLiteral).Value),
			FunctionName: Xargs[1].(*ast.StringLiteral).Value, Position: uint32(Xargs[2].(*ast.IntegerLiteral).Value)}
		serializedTypescheme := Xargs[3].(*ast.StringLiteral).Value
		cpF.RtnTypes = iz.deserializeTypescheme(serializedTypescheme)
	}
	fnenv := compiler.NewEnvironment()
	fnenv.Ext = outerEnv
	cpF.LoReg = iz.cp.MemTop()
	for _, pair := range sig {
		iz.cp.Reserve(values.UNDEFINED_TYPE, DUMMY, node.GetToken())
		if ast.IsRef(pair.VarType) {
			iz.cp.AddVariable(fnenv, pair.VarName, compiler.REFERENCE_VARIABLE, iz.cp.Common.AnyTypeScheme, node.GetToken())
			continue
		}
		typeName := pair.VarType
		_, isVarargs := typeName.(*ast.TypeDotDotDot)
		if isVarargs {
			iz.cp.AddVariable(fnenv, pair.VarName, compiler.FUNCTION_ARGUMENT, compiler.AlternateType{compiler.TypedTupleType{iz.cp.GetAlternateTypeFromTypeAst(pair.VarType)}}, node.GetToken())
		} else {
			if !ast.IsAstBling(pair.VarType) {
				iz.cp.AddVariable(fnenv, pair.VarName, compiler.FUNCTION_ARGUMENT, iz.cp.GetAlternateTypeFromTypeAst(pair.VarType), node.GetToken())
			}
		}
	}
	cpF.HiReg = iz.cp.MemTop()
	cpF.CallTo = iz.cp.CodeTop()
	tupleData := make([]uint32, 0, len(sig))
	var foundTupleOrVarArgs bool
	for _, param := range sig {
		switch {
		case ast.IsVarargs(param.VarType):
			tupleData = append(tupleData, 1)
			foundTupleOrVarArgs = true
		case ast.Is(param.VarType, "tuple"):
			tupleData = append(tupleData, 2)
			foundTupleOrVarArgs = true
		default:
			tupleData = append(tupleData, 0)
		}
	}
	if foundTupleOrVarArgs {
		cpF.LocOfTupleAndVarargData = iz.cp.Reserve(values.INT_ARRAY, tupleData, node.GetToken())
	} else {
		cpF.LocOfTupleAndVarargData = DUMMY
	}
	switch body.GetToken().Type {
	case token.BUILTIN:
		name := body.(*ast.BuiltInExpression).Name
		types, ok := compiler.BUILTINS[name]
		if ok {
			cpF.RtnTypes = types.T
		} else {
			typeNumber, ok := iz.cp.GetConcreteType(name) // We treat the clone constructors and short struct constructors as builtins. TODO --- todon't.
			if ok {
				cpF.RtnTypes = altType(typeNumber)
			}
		}
		cpF.Builtin = name
	case token.GOCODE:
		cpF.GoNumber = uint32(len(iz.cp.Vm.GoFns))
		cpF.HasGo = true
		iz.cp.Vm.GoFns = append(iz.cp.Vm.GoFns, vm.GoFn{Code: body.(*ast.GolangExpression).GoFunction})
	case token.XCALL:
	default:
		areWeTracking := compiler.LF_NONE
		if iz.cp.GetTrackingScope() == 2 {
			areWeTracking = compiler.LF_TRACK
		}
		if given != nil {
			iz.cp.ThunkList = []compiler.ThunkData{}
			givenContext := compiler.Context{fnenv, functionName, compiler.DEF, false, nil, cpF.LoReg, areWeTracking, compiler.LF_NONE}
			iz.cp.CompileGivenBlock(given, givenContext)
			cpF.CallTo = iz.cp.CodeTop()
			if len(iz.cp.ThunkList) > 0 {
				iz.cp.Cm("Initializing thunks for outer function.", body.GetToken())
			}
			for _, thunks := range iz.cp.ThunkList {
				iz.cp.Emit(vm.Thnk, thunks.Dest, thunks.Value.MLoc, thunks.Value.CAddr)
			}
		}
		// Logging the function call, if we do it, goes here.
		// 'stringify' is secret sauce, users aren't meant to know it exists. TODO --- conceal it better.

		trackingOn := areWeTracking == compiler.LF_TRACK && (functionName != "stringify")
		log, nodeHasLog := body.(*ast.LogExpression)
		autoOn := nodeHasLog && log.Token.Type == token.PRELOG && log.Value == ""
		if trackingOn || autoOn {
			iz.cp.TrackOrLog(vm.TR_FNCALL, trackingOn, autoOn, node.GetToken(), functionName, sig, cpF.LoReg)
		}
		if nodeHasLog && log.Token.Type == token.PRELOG && log.Value != "" {

		}
		// Now the main body of the function, just as a lagniappe.
		bodyContext := compiler.Context{fnenv, functionName, ac, true, iz.cp.ReturnSigToAlternateType(rtnSig), cpF.LoReg, areWeTracking, compiler.LF_NONE}
		cpF.RtnTypes, _ = iz.cp.CompileNode(body, bodyContext) // TODO --- could we in fact do anything useful if we knew it was a constant?
		cpF.OutReg = iz.cp.That()

		if rtnSig != nil && !(body.GetToken().Type == token.GOCODE) {
			iz.cp.EmitTypeChecks(cpF.OutReg, cpF.RtnTypes, fnenv, rtnSig, ac, node.GetToken(), compiler.CHECK_RETURN_TYPES)
		}

		iz.cp.Emit(vm.Ret)
	}
	iz.cp.Fns = append(iz.cp.Fns, &cpF)
	if ac == compiler.DEF && !cpF.RtnTypes.IsLegalDefReturn() {
		iz.p.Throw("comp/return/def", node.GetToken())
	}
	if ac == compiler.CMD && !cpF.RtnTypes.IsLegalCmdReturn() {
		iz.p.Throw("comp/return/cmd", node.GetToken())
	}
	iz.setDeclaration(decFUNCTION, node.GetToken(), DUMMY, &cpF)

	// We capture the 'stringify' function for use by the VM. TODO --- somewhere else altogether.

	if functionName == "stringify" {
		iz.cp.Vm.StringifyLoReg = cpF.LoReg
		iz.cp.Vm.StringifyCallTo = cpF.CallTo
		iz.cp.Vm.StringifyOutReg = cpF.OutReg
	}

	return &cpF
}

// Phase 5 of compilation.
func (iz *initializer) ResolveInterfaceBacktracks() {
	for _, rDat := range iz.p.Common.InterfaceBacktracks {
		prsrFunction := rDat.Fn
		resolvingCompiler := prsrFunction.Compiler.(*compiler.Compiler)
		CpFunction := resolvingCompiler.Fns[prsrFunction.Number]
		addr := rDat.Addr
		iz.cp.Vm.Code[addr].Args[0] = CpFunction.CallTo
		iz.cp.Vm.Code[addr].Args[1] = CpFunction.LoReg
		iz.cp.Vm.Code[addr].Args[2] = CpFunction.HiReg
		iz.cp.Vm.Code[addr+1].Args[1] = CpFunction.OutReg
	}
}

// Various miscellaneous types and functions supporting compilation.

// Adds a concrete type to the parser, and to the common types it falls under (at least `any` and `any?`).
func (iz *initializer) AddType(name, supertype string, typeNo values.ValueType) {
	iz.localConcreteTypes = iz.localConcreteTypes.Add(typeNo)
	iz.p.TypeMap[name] = values.MakeAbstractType(typeNo)
	iz.p.TypeMap[name+"?"] = values.MakeAbstractType(values.NULL, typeNo)
	iz.unserializableTypes.Add(name).Add(name + "?")
	types := []string{supertype}
	iz.cp.Common.AddTypeNumberToSharedAlternateTypes(typeNo, types...)
	types = append(types, "any")
	for _, sT := range types {
		iz.p.Common.Types[sT] = iz.p.Common.Types[sT].Insert(typeNo)
		iz.p.Common.Types[sT+"?"] = iz.p.Common.Types[sT+"?"].Insert(typeNo)
	}
}

// Adds type information to the VM.
//
// For reasons, it's a good idea to have the type info stored as an ordered list rather than a set or hashmap.
// So we need to do insertion by hand to avoid duplication.
func (iz *initializer) AddTypeToVm(typeInfo values.AbstractTypeInfo) {
	for i, existingTypeInfo := range iz.cp.Vm.AbstractTypes {
		if typeInfo.Name == existingTypeInfo.Name {
			if typeInfo.Path == existingTypeInfo.Path {
				return
			}
			if strings.Count(typeInfo.Path, ".") < strings.Count(existingTypeInfo.Path, ".") {
				iz.cp.Vm.AbstractTypes[i] = typeInfo
				return
			}
			if len(typeInfo.Path) < len(existingTypeInfo.Path) {
				iz.cp.Vm.AbstractTypes[i] = typeInfo
				return
			}
		}
	}
	iz.cp.Vm.AbstractTypes = append(iz.cp.Vm.AbstractTypes, typeInfo)
}

func altType(t ...values.ValueType) compiler.AlternateType {
	return compiler.AltType(t...)
}

func (i initializer) IsPrivate(x, y int) bool {
	return i.TokenizedDeclarations[x][y].Private
}

// For indexing the functions in the common function map, to prevent duplication.
type FuncSource struct {
	Filename     string
	LineNo       int
	FunctionName string
	Pos          uint32 // Exists to distinguish '-' as a prefix from '-' as an infix when defining clone types
}

// Stores pretokenized chunks of code for later parsing.
type TokenizedCodeChunks []*token.TokenizedCodeChunk

// You may wonder why the declarationMap is stored in the initializer and copied from one to the other rather than held
// in the Common initializer and shared. So do I, but we get all sorts of weird bugs if we try. TODO --- investigate.
type decKey struct {
	dOf declarationOf // A struct, a label, a function ...
	src string        // The filepath to the source code.
	lNo int           // Line number of the declaration.
	chS int
	ix  int           // If it's an element of an enum, the index of the element in its type.
}

func makeKey(dOf declarationOf, tok *token.Token, ix int) decKey {
	return decKey{dOf: dOf, src: tok.Source, lNo: tok.Line, chS: tok.ChStart, ix: ix}
}

func (iz *initializer) getDeclaration(dOf declarationOf, tok *token.Token, ix int) (any, bool) {
	result, ok := iz.Common.DeclarationMap[makeKey(dOf, tok, ix)]
	return result, ok
}

func (iz *initializer) setDeclaration(dOf declarationOf, tok *token.Token, ix int, v any) {
	iz.Common.DeclarationMap[makeKey(dOf, tok, ix)] = v
}

// Tokens to return when no token is available.
var LINKING_TOKEN = &token.Token{Source: "Pipefish linker"}

// Types and functions to assist the `MakeParserndTokenizedProgram` method.
type Section int

const (
	ImportSection Section = iota
	ExternalSection
	VarSection
	CmdSection
	DefSection
	TypesSection
	ConstSection
	UndefinedSection
)

type declarationType int

// The fact that these things come in this order is used in the code and should not be changed without a great deal of forethought.
const (
	importDeclaration    declarationType = iota
	externalDeclaration                  //
	enumDeclaration                      //
	structDeclaration                    //
	snippetDeclaration                   //
	abstractDeclaration                  //
	interfaceDeclaration                 //
	cloneDeclaration                     //
	constantDeclaration                  //
	variableDeclaration                  //
	functionDeclaration                  //
	commandDeclaration                   //
	golangDeclaration                    // Pure golang in a block; the Pipefish functions with golang bodies don't go here but under function or command as they were declared.
	makeDeclaration                      // Instantiates parameterized types.
)

var tokenTypeToSection = map[token.TokenType]Section{
	token.IMPORT:  ImportSection,
	token.EXTERN:  ExternalSection,
	token.VAR:     VarSection,
	token.CMD:     CmdSection,
	token.DEF:     DefSection,
	token.NEWTYPE: TypesSection,
	token.CONST:   ConstSection,
}

type fnSource struct {
	decType   declarationType
	decNumber int
}

type labeledParsedCodeChunk struct {
	chunk     ast.Node
	decType   declarationType
	decNumber int
	name      string
}

func (iz *initializer) addTokenizedDeclaration(decType declarationType, line *token.TokenizedCodeChunk, private bool) {
	line.Private = private
	iz.TokenizedDeclarations[decType] = append(iz.TokenizedDeclarations[decType], line)
}

var typeMap = map[string]declarationType{"struct": structDeclaration, "enum": enumDeclaration,
	"abstract": abstractDeclaration, "clone": cloneDeclaration, "interface": interfaceDeclaration}

// Types and functions to help with housekeeping. The initializer stores the declarations of types and functions
// in a map keyed by their source and line number. This is to prevent the same source code being compiled twice onto
// the same VM, which only needs it once.
type declarationOf int

const (
	decSTRUCT declarationOf = iota
	decLABEL
	decENUM
	decCLONE
	decABSTRACT
	decINTERFACE
	decFUNCTION
	decPARAMETERIZED // A placeholder/
)

type labelInfo struct {
	loc     uint32 // The location in the VM where we store a value {LABEL, n}.
	private bool
}

type structInfo struct {
	structNumber values.ValueType
	private      bool
	sig          ast.AstSig
}

type fnSigInfo struct {
	name   string
	sig    ast.AstSig
	rtnSig ast.AstSig
}

type interfaceInfo struct {
	sigs []fnSigInfo
}

// The maximum value of a `uint32`. Used as a dummy/sentinel value when `0` is not appropriate.
const DUMMY = 4294967295

// Manufactures a value.
func val(T values.ValueType, V any) values.Value {
	return values.Value{T: T, V: V}
}

// Function for commenting on what the initializer is doing. Only mentions the largest steps, hence
// the lack of a Token parameter in its sig. For more detail, turn on the SHOW_COMPILER flag.
func (iz *initializer) cmI(s string) {
	if settings.SHOW_INITIALIZER {
		if iz.cp != nil && iz.cp.P != nil {
			println(text.UNDERLINE + s + text.RESET + " (" + iz.cp.P.NamespacePath + ")")
		}
	}
}

// Like everything else, the initializer sends its errors to the Common parser bindle via the parser.
func (iz *initializer) Throw(errorID string, tok *token.Token, args ...any) {
	iz.p.Throw(errorID, tok, args...)
}

// Return whether the initializer has encountered errors.
func (iz *initializer) ErrorsExist() bool {
	return iz.p.ErrorsExist()
}

// Methods for manipulating the function table.

func (iz *initializer) Add(functionName string, f *ast.PrsrFunction) *ast.PrsrFunction {
	if functions, ok := iz.cp.P.FunctionTable[functionName]; ok {
		functions, conflictingFunction := iz.AddInOrder(functions, f)
		iz.cp.P.FunctionTable[functionName] = functions
		return conflictingFunction
	}
	iz.cp.P.FunctionTable[functionName] = []*ast.PrsrFunction{f}
	return nil
}

func (iz *initializer) AddInOrder(S []*ast.PrsrFunction, f *ast.PrsrFunction) ([]*ast.PrsrFunction, *ast.PrsrFunction) {
	fSig := f.Compiler.(*compiler.Compiler).P.MakeAbstractSigFromStringSig(f.NameSig)
	for i := 0; i < len(S); i++ {
		gSig := S[i].Compiler.(*compiler.Compiler).P.MakeAbstractSigFromStringSig(S[i].NameSig)
		yes, ok := parser.IsMoreSpecific(fSig, gSig)
		if !ok {
			return S, S[i]
		}
		if yes {
			S = insert(S, f, i)
			return S, nil
		}
	}
	S = append(S, f)
	return S, nil
}

func insert(a []*ast.PrsrFunction, value *ast.PrsrFunction, index int) []*ast.PrsrFunction {
	if len(a) == index { // nil or empty slice or after last element
		return append(a, value)
	}
	a = append(a[:index+1], a[index:]...) // index < len(a)
	a[index] = value
	return a
}
