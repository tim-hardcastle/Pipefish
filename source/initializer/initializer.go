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

// Definition of the Initializer type.
type Initializer struct {
	cp                                  *compiler.Compiler             // The compiler for the module being intitialized.
	P                                   *parser.Parser                 // The parser for the module being initialized.
	initializers                        map[string]*Initializer        // The child initializers of this one, to initialize imports and external stubs.
	TokenizedDeclarations               [14]TokenizedCodeChunks        // The declarations in the script, converted from text to tokens and sorted by purpose.
	ParsedDeclarations                  [13]parser.ParsedCodeChunks    // ASTs produced by parsing the tokenized chunks in the field above, sorted in the same way.
	tokenizedCode                       [][]tokenizedCode            // Code arranged by declaration type and lightly chunked and validated.
	localConcreteTypes                  dtypes.Set[values.ValueType]   // All the struct, enum, and clone types defined in a given module.
	goBucket                            *GoBucket                      // Where the initializer keeps information gathered during parsing the script that will be needed to compile the Go modules.
	Snippets                            []string                       // Names of snippet types visible to the module.
	fnIndex                             map[fnSource]*ast.PrsrFunction // The point of this is that once we've compiled a function, we need to set the Number field of the PrsrFunction representation to be the number of the cpFunc in the cp.Fns list.
	Common                              *CommonInitializerBindle       // The information all the initializers have in Common.
	structDeclarationNumberToTypeNumber map[int]values.ValueType       // Maps the order of the declaration of the struct in the script to its type number in the VM. TODO --- there must be something better than this.
	unserializableTypes                 dtypes.Set[string]             // Keeps track of which abstract types are mandatory imports/singletons of a concrete type so we don't try to serialize them.
	// This contains information about the operators of parameterized types, e.g. Z.
	typeOperators map[string]typeOperatorInfo
	// This contains information about the parameterized types that we're going to instantiate when
	// we scrape them out of signatures and 'make' statements.
	parameterizedTypesToDeclare map[string]typeInstantiationInfo
	// Whereas these contain information about the instances of the types after we've created them,
	// e.g. Z{5}, Z{12}.
	parameterizedTypeMap       map[string]int              // Maps names to the numbered type instances below.
	parameterizedTypeInstances []parameterizedTypeInstance // Stores information we need to compile the runtime typechecks on parameterized type instances.
}

// Makes a new initializer.
func NewInitializer() *Initializer {
	iz := Initializer{
		initializers:                make(map[string]*Initializer),
		localConcreteTypes:          make(dtypes.Set[values.ValueType]),
		fnIndex:                     make(map[fnSource]*ast.PrsrFunction),
		unserializableTypes:         make(dtypes.Set[string]),
		typeOperators:               make(map[string]typeOperatorInfo),
		parameterizedTypesToDeclare: make(map[string]typeInstantiationInfo),
		parameterizedTypeMap:        make(map[string]int),
		parameterizedTypeInstances:  make([]parameterizedTypeInstance, 0),
		tokenizedCode:               make([][]tokenizedCode, 14),
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

// Type instantiations as scraped out of signatures and make statements.
type typeInstantiationInfo struct {
	ty      *ast.TypeWithArguments
	private bool
}

// After hooking it up with the parameterized type definitions.
type parameterizedTypeInstance struct {
	astType   ast.TypeNode
	env       *compiler.Environment
	typeCheck *token.TokenizedCodeChunk
	fields    ast.AstSig
	vals      []values.Value
}

type typeOperatorInfo struct {
	constructorSig ast.AstSig // The signature of the constructor, before we prepend the secret-sauce `+t type` parameter.
	isClone        bool
	returnTypes    compiler.AlternateType
	definedAt      []*token.Token
}

// Initializes a compiler.
func newCompiler(Common *parser.CommonParserBindle, ccb *compiler.CommonCompilerBindle, scriptFilepath, sourcecode string, mc *vm.Vm, namespacePath string) *compiler.Compiler {
	p := parser.New(Common, scriptFilepath, sourcecode, namespacePath)
	cp := compiler.NewCompiler(p, ccb)
	cp.ScriptFilepath = scriptFilepath
	cp.Vm = mc
	cp.TupleType = cp.Reserve(values.TYPE, values.AbstractType{[]values.ValueType{values.TUPLE}}, &token.Token{Source: "Builtin constant"})
	return cp
}

func (iz *Initializer) ParseEverythingFromFilePath(mc *vm.Vm, cpb *parser.CommonParserBindle, ccb *compiler.CommonCompilerBindle, scriptFilepath, namespacePath string) (*compiler.Compiler, error) {
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
	iz.cmI("Adding abstract types to parameterized types.")
	iz.tweakParameterizedTypes()
	if iz.ErrorsExist() {
		iz.cp.P.Common.IsBroken = true
		return result
	}

	iz.cmI("Adding parameterized types to VM.")
	iz.addParameterizedTypesToVm()
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
func (iz *Initializer) ParseEverythingFromSourcecode(mc *vm.Vm, cpb *parser.CommonParserBindle, ccb *compiler.CommonCompilerBindle, scriptFilepath, sourcecode, namespacePath string) *compiler.Compiler {
	iz.cp = newCompiler(cpb, ccb, scriptFilepath, sourcecode, mc, namespacePath)
	iz.P = iz.cp.P
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
	iz.P.Common.Sources[scriptFilepath] = strings.Split(sourcecode, "\n")
	return iz.cp
}

// This in the heart of phase 1 compilation, and does everything up to and including parsing the code chunks,
// and then hands back flow of control to the StartService or RunTest method.
func (iz *Initializer) parseEverything(scriptFilepath, sourcecode string) {
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
	iz.P.TokenizedCode = lexer.NewRelexer(scriptFilepath, sourcecode)

	iz.cmI("Making parser and tokenized program.")
	iz.MakeParserAndTokenizedProgram()
	if iz.ErrorsExist() {
		return
	}

	// TEMPORARY until I get rid of the top-level tccs.
	iz.cmI("Translating tccs to tcs.")
	iz.TranslateEverything()
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

	iz.cmI("Assigning type numbers to struct names.")
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

	iz.cmI("Instantiating parameterized types.")
	iz.instantiateParameterizedTypes()
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
func (iz *Initializer) AddToNameSpace(thingsToImport []string) {
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
		iz.MakeParserAndTokenizedProgram() // This is cumulative, it throws them all into the parser together.
		iz.P.Common.Sources[fname] = strings.Split(stdImp, "\n")
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
func (iz *Initializer) MakeParserAndTokenizedProgram() {
	currentSection := UndefinedSection
	beginCount := 0
	indentCount := 0
	lastTokenWasColon := false
	typeDefined := declarationType(DUMMY)
	IsPrivate := false
	var (
		tok token.Token
	)

	tok = iz.P.TokenizedCode.NextToken() // note that we've already removed leading newlines.
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
	
	line := token.NewCodeChunk()

	for tok = iz.P.TokenizedCode.NextToken(); true; tok = iz.P.TokenizedCode.NextToken() {
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
			continue
		}
		if tok.Type == token.NEWLINE && line.Length() == 0 {
			continue
		}

		lastTokenWasColon = (tok.Type == token.COLON)
		line.Append(tok)
		if tok.Type == token.EOF {
			break
		}
	}
	iz.P.Common.Errors = err.MergeErrors(iz.P.TokenizedCode.(*lexer.Relexer).GetErrors(), iz.P.Common.Errors)
}

// ****** TODO --- remove dependence of interface types on this and remove it.
// Function auxiliary to the above and to `createInterfaceTypes`. This extracts the words from a function definition
// and decides on their "grammatical" role: are they prefixes, suffixes, bling?
func (iz *Initializer) addWordsToParser(currentChunk *token.TokenizedCodeChunk) {
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
					iz.P.Forefixes.Add(tok.Literal)
				} else {
					iz.P.Midfixes.Add(tok.Literal)
				}
			} else {
				iz.P.Infixes.Add(tok.Literal)
			}
			hasMidOrEndfix = true
			lastTokenWasFix = true
			continue
		}

		if hasPrefix || hasMidOrEndfix {
			iz.P.Endfixes.Add(tok.Literal)
		} else {
			iz.P.Suffixes.Add(tok.Literal)
		}
		hasMidOrEndfix = true
		lastTokenWasFix = true
	}

	if hasPrefix {
		if hasMidOrEndfix {
			iz.P.Prefixes.Add(prefix)
		} else {
			if hasParams {
				iz.P.Functions.Add(prefix)
			} else {
				iz.P.Unfixes.Add(prefix)
			}
		}
	} else {
		if hasMidOrEndfix && !inParenthesis && !(tok.Literal == ")") && !iz.P.Suffixes.Contains(tok.Literal) {
			iz.P.Endfixes.Add(tok.Literal)
		}
	}
}

// Function auxiliary to the above and to `createInterfaceTypes`. This extracts the words from a function definition
// and decides on their "grammatical" role: are they prefixes, suffixes, bling?
func (iz *Initializer) addWordsToParser2(tc *tokenizedFunctionDeclaration) {
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
		for startAt = 2; !tc.sig[startAt-1].IsBling(); startAt++ {}
	}
	lastWasBling := true
	hasBling := false
	for ix := startAt; ix < len(tc.sig); ix++ {
		if tc.sig[ix].IsBling() {
			hasBling = true
			word := tc.sig[ix].Name.Literal
			if ix == len(tc.sig) - 1 {
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

// We call ParseEverything on the namespaced imports, returning a list of unnamespaced 
// imports which the main phase 1 function will add to the parser.
func (iz *Initializer) ParseNamespacedImportsAndReturnUnnamespacedImports() []string {
	unnamespacedImports := []string{}
	for i, tc := range iz.tokenizedCode[importDeclaration] {
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
			iz.Throw("init/import/file", &dec.path, path, e)
			return []string{}
		}
		iz.cp.Modules[name] = newCp
		iz.P.NamespaceBranch[name] = &parser.ParserData{newCp.P, path}
		newCp.P.Private = iz.IsPrivate(int(importDeclaration), i)
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
				iz.Throw("init/external/exist/a", &dec.name)
				continue
			}
			iz.addExternalOnSameHub(externalCP.ScriptFilepath, name)
			continue
		}
		if len(path) >= 5 && path[0:5] == "http:" {
			pos := strings.LastIndex(path, "/")
			if pos == -1 {
				iz.Throw("init/external/path/a", &dec.path)
				continue
			}
			hostpath := path[0:pos]
			serviceName := path[pos+1:]
			pos = strings.LastIndex(hostpath, "/")
			if pos == -1 {
				iz.Throw("init/external/path/b", &dec.path)
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
				iz.Throw("init/external/exist/b", &dec.path, hubServiceCp.ScriptFilepath)
			} else {
				iz.addExternalOnSameHub(path, name)
			}
			continue // Either we've thrown an error or we don't need to do anything.
		}
		// Otherwise we need to start up the service, add it to the hub, and then declare it as external.
		newServiceCp, e := StartCompilerFromFilepath(path, iz.Common.HubCompilers, iz.Common.HubStore)
		if e != nil { // Then we couldn't open the file.
			iz.Throw("init/external/file", &dec.path, path, e.Error())
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
		return hubService.SerializeApi()
	}
	serviceToAdd := compiler.ExternalCallToHubHandler{ev, pr, se}
	iz.addAnyExternalService(serviceToAdd, path, name)
}

func (iz *Initializer) addHttpService(path, name, username, password string) {
	ds := func(valAsString string) values.Value {
		return iz.cp.Do(valAsString)
	}
	serviceToAdd := compiler.ExternalHttpCallHandler{path, name, username, password, ds}
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
	newCp.P.Private = iz.IsPrivate(int(externalDeclaration), int(externalServiceOrdinal))
	iz.cp.Modules[name] = newCp
}

// Now we can start creating the user-defined types.

// We compile the enums.
//
// On the one hand, the VM must know the names of the enums and their elements so it can describe them.
// Otoh, the compiler needs to know how to turn enum literals into values.
func (iz *Initializer) createEnums() {
	for i, tc := range iz.tokenizedCode[enumDeclaration] {
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
		iz.AddType(dec.op.Literal, "enum", typeNo)
		iz.cp.P.EnumTypeNames.Add(dec.op.Literal)

		// We make the constructor function.

		iz.P.AllFunctionIdents.Add(name)
		iz.P.Functions.Add(name)
		sig := ast.AstSig{ast.NameTypeAstPair{"x", &ast.TypeWithName{token.Token{}, "int"}}}
		rtnSig := ast.AstSig{ast.NameTypeAstPair{"*dummy*", &ast.TypeWithName{token.Token{}, name}}}
		fnNo := iz.addToBuiltins(sig, name, altType(typeNo), iz.IsPrivate(int(enumDeclaration), i), &dec.op)
		fn := &ast.PrsrFunction{NameSig: sig, NameRets: rtnSig, Body: &ast.BuiltInExpression{Name: name}, Number: fnNo, Compiler: iz.cp, Tok: &dec.op}
		iz.Add(name, fn)
		if typeExists {
			continue
		}
		vec := vector.Empty
		elementNameList := []string{}
		for ord, tok := range dec.elements {
			_, alreadyExists := iz.cp.EnumElements[tok.Literal]
			if alreadyExists { // Enums in the same namespace can't have overlapping elements or we wouldn't know their type.
				iz.Throw("init/enum/element", &tok)
			}
			iz.cp.P.EnumElementNames.Add(tok.Literal)
			iz.cp.EnumElements[tok.Literal] = values.Value{typeNo, ord}
			vec = vec.Conj(values.Value{typeNo, ord})
			elementNameList = append(elementNameList, tok.Literal)
		}
		iz.cp.Vm.ConcreteTypeInfo = append(iz.cp.Vm.ConcreteTypeInfo, vm.EnumType{Name: name, Path: iz.P.NamespacePath, ElementNames: elementNameList,
			ElementValues: values.Value{values.LIST, vec}, Private: iz.IsPrivate(int(enumDeclaration), i), IsMI: settings.MandatoryImportSet().Contains(dec.op.Source)})
	}
}

// We create the clone types.
func (iz *Initializer) createClones() {
mainLoop:
	for _, tc := range iz.tokenizedCode[cloneDeclaration] {
		dec := tc.(*tokenizedCloneDeclaration)
		name := dec.op.Literal
		typeToClone := dec.parentTok.Literal
		parentTypeNo, _ := parser.ClonableTypes[typeToClone]
		if len(dec.params) > 0 {
			astType := iz.makeTypeWithParameters(dec.op, dec.params)
			ok := iz.registerParameterizedType(name, astType, dec.requests, dec.body, typeToClone, dec.private, ixPtr(dec))
			if !ok {
				iz.Throw("init/clone/exists", ixPtr(dec))
				continue mainLoop
			}
			iz.setDeclaration(decPARAMETERIZED, ixPtr(dec), DUMMY, DUMMY)
			continue mainLoop
		}
		typeNo, fn := iz.addCloneTypeAndConstructor(name, typeToClone, dec.private, ixPtr(dec))
		sig := ast.AstSig{ast.NameTypeAstPair{VarName: "x", VarType: ast.MakeAstTypeFrom(iz.cp.Vm.ConcreteTypeInfo[iz.cp.Vm.ConcreteTypeInfo[typeNo].(vm.CloneType).Parent].GetName(vm.DEFAULT))}}
		fn.Number = iz.addToBuiltins(sig, name, altType(typeNo), dec.private, ixPtr(dec))
		iz.createOperations(&ast.TypeWithName{token.Token{}, name}, typeNo, dec.requests, parentTypeNo, dec.private)
	}
}

func (iz *Initializer) addCloneTypeAndConstructor(name, typeToClone string, private bool, decTok *token.Token) (values.ValueType, *ast.PrsrFunction) {
	typeNo, ok := iz.addCloneType(name, typeToClone, private, decTok)
	if !ok {
		return DUMMY, nil
	}
	// We make the conversion function.
	iz.P.AllFunctionIdents.Add(name)
	iz.P.Functions.Add(name)
	sig := ast.AstSig{ast.NameTypeAstPair{"x", &ast.TypeWithName{token.Token{}, typeToClone}}}
	rtnSig := ast.AstSig{ast.NameTypeAstPair{"*dummy*", &ast.TypeWithName{token.Token{}, name}}}
	fn := &ast.PrsrFunction{NameSig: sig, NameRets: rtnSig, Body: &ast.BuiltInExpression{Name: name}, Number: DUMMY, Compiler: iz.cp, Tok: decTok}
	iz.Add(name, fn)
	if typeToClone == "int" || typeToClone == "float" {
		iz.P.Suffixes.Add(name)
	}
	return typeNo, fn
}

func (iz *Initializer) addCloneType(name, typeToClone string, private bool, decTok *token.Token) (values.ValueType, bool) {
	parentTypeNo, ok := parser.ClonableTypes[typeToClone]
	if !ok {
		iz.Throw("init/clone/type/c", decTok, typeToClone)
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
	iz.AddType(name, abType, typeNo)
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

// Function auxiliary to the previous one, to make constructors for the clone types.
func (iz *Initializer) makeCloneFunction(fnName string, sig ast.AstSig, builtinTag string, rtnTypes compiler.AlternateType, rtnSig ast.AstSig, IsPrivate bool, pos uint32, tok *token.Token) {
	fn := &ast.PrsrFunction{Tok: tok, NameSig: sig, NameRets: rtnSig, Body: &ast.BuiltInExpression{*tok, builtinTag}, Compiler: iz.cp, Number: iz.addToBuiltins(sig, builtinTag, rtnTypes, IsPrivate, tok)}
	iz.Common.Functions[FuncSource{tok.Source, tok.Line, fnName, pos}] = fn
	conflictingFunction := iz.Add(fnName, fn)
	if conflictingFunction != nil && conflictingFunction != fn {
		iz.P.Throw("init/overload/c", tok, fnName, conflictingFunction.Tok)
	}
}

// We create the structs as names and type numbers in the type system. We can't populate
// their fields yet because we haven't even declared the abstract and interface types
// lexically yet.
func (iz *Initializer) createStructNames() {
	iz.structDeclarationNumberToTypeNumber = map[int]values.ValueType{}
	// First we need to make the struct types into types so the parser parses them properly.
loop:
	for i := range iz.TokenizedDeclarations[structDeclaration] {
		indexToken := iz.TokenizedDeclarations[structDeclaration][i].IndexToken()
		iz.P.TokenizedCode = iz.TokenizedDeclarations[structDeclaration][i]
		iz.P.NextToken()
		iz.P.NextToken()
		iz.P.NextToken()
		ty := iz.P.ParseType(parser.T_LOWEST)
		name := indexToken.Literal
		switch ty := ty.(type) {
		case *ast.TypeWithName:
		case *ast.TypeWithParameters:
			ok := iz.registerParameterizedType(name, ty, nil, nil, "struct", iz.IsPrivate(int(structDeclaration), i), indexToken)
			if !ok {
				iz.Throw("init/struct/exists", indexToken)
			}
			iz.setDeclaration(decPARAMETERIZED, indexToken, DUMMY, DUMMY)
			continue loop
		default:
			iz.Throw("init/struct/define", indexToken)
			continue loop
		}
		private := iz.IsPrivate(int(structDeclaration), i)
		typeNo := iz.addStructType(name, private, indexToken)
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

func (iz *Initializer) addStructType(name string, private bool, indexToken *token.Token) values.ValueType {
	iz.P.AllFunctionIdents.Add(name)
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
	iz.AddType(name, "struct", typeNo)
	return typeNo
}

// We can now create the struct labels and define their type as an AstSig, though
// we can't make an abstract sig yet because we haven't populated the abstract types.
func (iz *Initializer) createStructLabels() {
	for i, tcc := range iz.TokenizedDeclarations[structDeclaration] {
		sDec := iz.tokenizedCode[structDeclaration][i].(*tokenizedStructDeclaration)
		indexToken := tcc.IndexToken()
		name := indexToken.Literal
		// We will now extract the AstSig lexically.
		dec, sig, _ := iz.cp.P.ParseStructSigFromTcc(tcc)
		labelsForStruct := iz.makeLabelsFromSig(sig, iz.IsPrivate(int(structDeclaration), i), indexToken)
		if ty, ok := dec.(*ast.TypeWithParameters); ok { // The labels are common to all the instances of the type.
			ty.Name = name
			argIndex := iz.paramTypeExists(ty)
			iz.cp.ParameterizedTypes[ty.Name][argIndex].Sig = sig
			iz.cp.ParameterizedTypes[ty.Name][argIndex].Typecheck = sDec.body
			continue
		}
		typeNo := iz.structDeclarationNumberToTypeNumber[i]
		private := iz.IsPrivate(int(structDeclaration), i)
		iz.setDeclaration(decSTRUCT, indexToken, DUMMY, structInfo{typeNo, private, sig})
		stT := vm.StructType{Name: name, Path: iz.P.NamespacePath, LabelNumbers: labelsForStruct,
			LabelValues: labelValuesFromLabelNumbers(labelsForStruct),
			Private:     private, IsMI: settings.MandatoryImportSet().Contains(indexToken.Source)}
		stT = stT.AddLabels(labelsForStruct)
		iz.cp.Vm.ConcreteTypeInfo[typeNo] = stT
		fnNo := iz.addToBuiltins(sig, name, altType(typeNo), private, indexToken)
		fn := &ast.PrsrFunction{NameSig: sig, Body: &ast.BuiltInExpression{Name: name}, Number: fnNo, Compiler: iz.cp, Tok: indexToken}
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

func (iz *Initializer) makeLabelsFromSig(sig ast.AstSig, private bool, indexToken *token.Token) []int {
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
			iz.cp.Common.LabelIsPrivate = append(iz.cp.Common.LabelIsPrivate, private)
		}
	}
	return labelsForStruct
}

func (iz *Initializer) registerParameterizedType(name string, ty *ast.TypeWithParameters, opList []token.Token, typeCheck *token.TokenizedCodeChunk, parentType string, private bool, tok *token.Token) bool {
	info, ok := iz.cp.ParameterizedTypes[name]
	if ok {
		if iz.paramTypeExists(ty) == DUMMY { // TODO --- why?
			return false
		}
	}
	blankType := ty.Blank()
	blankType.Name = name
	supertype := blankType.String()
	iz.cp.GeneratedAbstractTypes.Add(supertype)
	thingToAdd := compiler.ParameterInfo{iz.astParamsToNames(ty.Parameters),
		iz.astParamsToValueTypes(ty.Parameters), opList,
		typeCheck, parentType, nil, private, supertype, tok}
	iz.cp.P.TypeMap[supertype] = values.AbstractType{}
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

func (iz *Initializer) paramTypeExists(ty *ast.TypeWithParameters) int {
	typesToMatch := iz.astParamsToValueTypes(ty.Parameters)
	for i, pty := range iz.cp.ParameterizedTypes[ty.Name] {
		if iz.ParameterTypesMatch(typesToMatch, pty.Types) {
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

func (iz *Initializer) ParameterTypesMatch(paramsToCheck, paramsToMatch []values.ValueType) bool {
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
func (iz *Initializer) createAbstractTypes() {
	for _, tcc := range iz.TokenizedDeclarations[abstractDeclaration] {
		tcc.ToStart()
		nameTok := tcc.NextToken()
		newTypename := nameTok.Literal
		tcc.NextToken() // The equals sign.
		tcc.NextToken() // The 'abstract' identifier.
		iz.P.TypeMap[newTypename] = values.MakeAbstractType()
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
			abTypeToAdd, ok := iz.P.TypeMap[tname]
			if !ok {
				iz.Throw("init/type/known", &typeTok)
				break
			}
			iz.P.TypeMap[newTypename] = iz.P.TypeMap[newTypename].Union(abTypeToAdd)
			if divTok.Type == token.EOF {
				break
			}
		}
		_, typeExists := iz.getDeclaration(decABSTRACT, &nameTok, DUMMY)
		if !typeExists {
			iz.setDeclaration(decABSTRACT, &nameTok, DUMMY, nil)
		}
		iz.P.Suffixes.Add(newTypename)
	}
}

// Phase 1L of compilation. Creates the interface types as names but doesn't populate them: parses the signatures
// of the functions in the interface definitions.
func (iz *Initializer) createInterfaceTypes() {
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
			iz.P.Throw("init/interface/colon", &shouldBeColon)
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
			iz.P.TokenizedCode = sig
			lhs := sig
			astOfSig := iz.P.ParseTokenizedChunk()
			if iz.ErrorsExist() {
				break
			}
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
				functionName, _, astSig = iz.P.GetPartsOfSig(astOfSig.(*ast.PipingExpression).Left)
				retSig = iz.P.RecursivelySlurpReturnTypes(astOfSig.(*ast.PipingExpression).Right)
			} else {
				functionName, _, astSig = iz.P.GetPartsOfSig(astOfSig)
			}
			typeInfo = append(typeInfo, fnSigInfo{functionName, astSig, retSig})
			iz.addWordsToParser(lhs)
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
	// First, all the stuff in make statements needs adding to the implicitly instantiated
	// types.
loop:
	for i, dec := range iz.TokenizedDeclarations[makeDeclaration] {
		dec.ToStart()
		iz.cp.P.TokenizedCode = dec
		iz.cp.P.SafeNextToken()
		iz.cp.P.SafeNextToken()
		if iz.cp.P.PeekToken.Type == token.EOF {
			iz.Throw("init/make/empty", dec.IndexToken())
			continue loop
		}
		private := iz.IsPrivate(int(makeDeclaration), i)
		for {
			ty := iz.cp.P.ParseType(parser.T_LOWEST)
			if ty == nil {
				iz.Throw("init/make/type", dec.IndexToken())
				continue loop
			}
			if ty, ok := ty.(*ast.TypeWithArguments); !ok {
				iz.Throw("init/make/instance", dec.IndexToken())
				continue loop
			} else {
				iz.parameterizedTypesToDeclare[ty.String()] =
					typeInstantiationInfo{ty, private}
			}
			iz.cp.P.NextToken()
			switch iz.cp.P.PeekToken.Type {
			case token.EOF:
				continue loop
			case token.COMMA:
				iz.cp.P.NextToken()
			default:
				tok := iz.cp.P.CurToken
				iz.Throw("init/make/comma", &tok)
				continue loop
			}
		}
	}
	for _, info := range iz.parameterizedTypesToDeclare {
		ty := info.ty
		private := info.private
		// The parser doesn't know the types and values of enums, 'cos of being a
		// parser. So we kludge them in here.
		for i, v := range ty.Values() {
			if maybeEnum, ok := v.V.(string); ok && v.T == 0 {
				w := iz.cp.EnumElements[maybeEnum]
				ty.Arguments[i].Type = w.T
				ty.Arguments[i].Value = w.V
			}
		}
		argIndex := iz.cp.FindParameterizedType(ty.Name, ty.Values())
		if argIndex == DUMMY {
			iz.Throw("init/type/args", &ty.Token)
			continue
		}
		parTypeInfo := iz.cp.ParameterizedTypes[ty.Name][argIndex]
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
			iz.Throw("init/clone/type", &ty.Token)
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
		if opInfo, ok := iz.typeOperators[ty.Name]; ok {
			opInfo.returnTypes = opInfo.returnTypes.Union(altType(typeNo))
			opInfo.definedAt = append(opInfo.definedAt, &ty.Token)
			iz.typeOperators[ty.Name] = opInfo
			// TODO --- Check for matching sigs, being a clone.
		} else {
			iz.typeOperators[ty.Name] = typeOperatorInfo{sig, isClone, altType(values.ERROR, typeNo), []*token.Token{&ty.Token}}
		}
		if _, ok := iz.parameterizedTypeMap[newTypeName]; !ok {
			iz.parameterizedTypeMap[newTypeName] = len(iz.parameterizedTypeInstances)
			iz.parameterizedTypeInstances = append(iz.parameterizedTypeInstances, parameterizedTypeInstance{ty, newEnv, parTypeInfo.Typecheck, sig, vals})
		}
		iz.cp.P.TypeMap[parTypeInfo.Supertype] = iz.cp.P.TypeMap[parTypeInfo.Supertype].Insert(typeNo)
	}
	// Now we can make a constructor function for each of the type operators.
	for typeOperator, operatorInfo := range iz.typeOperators {
		name := typeOperator + "{}"
		sig := append(ast.AstSig{ast.NameTypeAstPair{"+t", &ast.TypeWithName{*operatorInfo.definedAt[0], "type"}}}, operatorInfo.constructorSig...)
		fnNo := iz.addToBuiltins(sig, name, operatorInfo.returnTypes, false, operatorInfo.definedAt[0])
		fn := &ast.PrsrFunction{NameSig: sig, Body: &ast.BuiltInExpression{Name: name}, Number: fnNo, Compiler: iz.cp, Tok: operatorInfo.definedAt[0]}
		iz.cp.P.Functions.Add(name)
		iz.Add(name, fn)
	}
}

// We parse the snippet types, abstract types, clone types, constants, variables,
// functions, commands.
func (iz *Initializer) parseEverythingElse() {
	for declarations := snippetDeclaration; declarations <= commandDeclaration; declarations++ {
		if declarations == cloneDeclaration || declarations == interfaceDeclaration { // TODO --- yeah, yeah, I am filled with shame.
			continue
		}
		for chunk := 0; chunk < len(iz.TokenizedDeclarations[declarations]); chunk++ {
			iz.P.TokenizedCode = iz.TokenizedDeclarations[declarations][chunk]
			iz.TokenizedDeclarations[declarations][chunk].ToStart()
			iz.ParsedDeclarations[declarations] = append(iz.ParsedDeclarations[declarations], iz.P.ParseTokenizedChunk())
		}
	}

	iz.P.AllFunctionIdents.AddSet(iz.P.Functions)
	iz.P.AllFunctionIdents.AddSet(iz.P.Prefixes)
	iz.P.AllFunctionIdents.AddSet(iz.P.Forefixes)
	iz.P.AllFunctionIdents.AddSet(iz.P.Midfixes)
	iz.P.AllFunctionIdents.AddSet(iz.P.Endfixes)
	iz.P.AllFunctionIdents.AddSet(iz.P.Infixes)
	iz.P.AllFunctionIdents.AddSet(iz.P.Suffixes)
	iz.P.AllFunctionIdents.AddSet(iz.P.Unfixes)

	iz.P.Bling.AddSet(iz.P.Forefixes)
	iz.P.Bling.AddSet(iz.P.Midfixes)
	iz.P.Bling.AddSet(iz.P.Endfixes)
}

// Phase 2. We find the shareable functions.
func (iz *Initializer) findAllShareableFunctions() {
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
func (iz *Initializer) findShareableFunctions() {
	for j := functionDeclaration; j <= commandDeclaration; j++ {
		for i := 0; i < len(iz.ParsedDeclarations[j]); i++ {
			tok := iz.ParsedDeclarations[j][i].GetToken()
			functionName, position, sig, rTypes, body, given := iz.P.ExtractPartsOfFunction(iz.ParsedDeclarations[j][i])
			if body == nil {
				iz.P.Throw("init/func/body", tok)
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
func (iz *Initializer) shareable(f *ast.PrsrFunction) bool {
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
		abType := iz.P.GetAbstractType(ty)
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

func (iz *Initializer) populateInterfaceTypes() {
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
					matches := iz.getMatches(sigToMatch, fnToTry, &nameTok)
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
		iz.P.TypeMap[typename] = types
		iz.AddTypeToVm(values.AbstractTypeInfo{typename, iz.P.NamespacePath, types, settings.MandatoryImportSet().Contains(nameTok.Source)})
		// And we add all the implicated functions to the function table.
		for _, ty := range types.Types {
			for _, fn := range funcsToAdd[ty] {
				conflictingFunction := iz.Add(fn.FName, fn)
				if conflictingFunction != nil && conflictingFunction != fn {
					iz.P.Throw("init/overload/b", fn.Tok, fn.FName, conflictingFunction.Tok)
				}
			}
		}
	}
}

func (iz *Initializer) populateAbstractTypes() {
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

// We add the abstract types to the VM.
//
// The VM doesn't *use* abstract types, but they are what values of type TYPE contain, and so it needs to
// be able to describe them.
func (iz *Initializer) addAbstractTypesToVm() {
	// For consistent results for tests, it is desirable that the types should be listed in a fixed order.
	keys := []string{}
	for typeName, _ := range iz.P.TypeMap {
		keys = append(keys, typeName)
	}
	for typeName, _ := range iz.P.Common.Types {
		keys = append(keys, typeName)
	}
	sort.Slice(keys, func(i, j int) bool { return keys[i] < keys[j] })
	for _, typeName := range keys {
		if text.Head(typeName, "clones{") && len(iz.P.GetAbstractTypeFromTypeSys(typeName).Types) == 1 {
			continue
		}
		iz.AddTypeToVm(values.AbstractTypeInfo{Name: typeName, Path: iz.P.NamespacePath,
			AT: iz.P.GetAbstractTypeFromTypeSys(typeName), IsMI: iz.unserializableTypes.Contains(typeName)})
	}
	for _, v := range parser.ClonableTypes { // Clonable types are clones of themselves.
		selfInfo := iz.cp.Vm.ConcreteTypeInfo[v].(vm.BuiltinType)
		selfInfo = selfInfo.AddClone(values.ValueType(v))
		iz.cp.Vm.ConcreteTypeInfo[v] = selfInfo
	}
	for i, v := range iz.cp.Vm.ConcreteTypeInfo {
		if v.IsClone() {
			parentType := v.(vm.CloneType).Parent
			parentInfo := iz.cp.Vm.ConcreteTypeInfo[parentType].(vm.BuiltinType)
			parentInfo = parentInfo.AddClone(values.ValueType(i))
			iz.cp.Vm.ConcreteTypeInfo[parentType] = parentInfo
		}
	}
}

// We make the alternate types from the abstract types, because the compiler
// is shortly going to need them.
//
// OTOH, we want the type information spread across the parsers and shared in the Common parser bindle to
// collectively be the any source of truth for our type system.
// But it can't be the only *representation* of the truth, becase that would slow things down 'cos the compiler
// would have to keep converting abstract types to alternate types to build the type schemes with.
// The solution is to build the alternate type schemes once and for all from the alternate types, after we've
// entirely finished generating the data in the parsers.
func (iz *Initializer) makeAlternateTypesFromAbstractTypes() {
	iz.cp.TypeNameToTypeScheme = make(map[string]compiler.AlternateType)
	for typename, abType := range iz.P.TypeMap {
		iz.cp.TypeNameToTypeScheme[typename] = compiler.AbstractTypeToAlternateType(abType)
	}
	for typename, abType := range iz.P.Common.Types {
		iz.cp.TypeNameToTypeScheme[typename] = compiler.AbstractTypeToAlternateType(abType)
	}
}

// Function auxiliary to the above and to `makeCloneFunction` which adds the constructors to the builtins.
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

// Phase 4 of compilation. At this point we have our functions as parsed code chunks in the
// `uP.Parser.ParsedDeclarations(<function/command>Declaration)` slice. We want to read their signatures
// and order them according to specificity for the purposes of implementing overloading.
func (iz *Initializer) MakeFunctionTables() {
	// First we recursively call the method on all the dependencies of the module.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.MakeFunctionTables()
	}
	iz.makeFunctionTable()
	if settings.FUNCTION_TO_PEEK != "" {
		println("In namespace", iz.P.NamespacePath)
		println(iz.P.FunctionTable.Describe(iz.P, settings.FUNCTION_TO_PEEK))
	}
}

// Function auxillary to the above for making one function table.
func (iz *Initializer) makeFunctionTable() {
	for j := functionDeclaration; j <= commandDeclaration; j++ {
		for i := 0; i < len(iz.ParsedDeclarations[j]); i++ {
			tok := iz.ParsedDeclarations[j][i].GetToken()
			functionName, position, sig, rTypes, body, given := iz.P.ExtractPartsOfFunction(iz.ParsedDeclarations[j][i])
			var (
				ok            bool
				functionToAdd *ast.PrsrFunction
			)
			if functionToAdd, ok = iz.Common.Functions[FuncSource{tok.Source, tok.Line, functionName, position}]; ok {
			} else {
				// TODO --- this is vile shotgun parsing and is probably duplicated elsewhere.
				if body == nil {
					iz.P.Throw("init/func/body", tok)
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
				iz.P.Throw("init/overload/a", body.GetToken(), functionName, conflictingFunction.Tok)
				return
			}
		}
	}
}

func (iz *Initializer) MakeFunctionForests() {
	// First we recurse.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.MakeFunctionForests()
	}

	// Now we turn the function tables into a different data structure, a "function tree" with its branches labeled
	// with types. Following it tells us which version of an overloaded function to use.
	iz.MakeFunctionTrees()
	if tree, ok := iz.P.FunctionForest[settings.FUNCTION_TO_PEEK]; ok && settings.FUNCTION_TO_PEEK != "" {
		println("In namespace", iz.P.NamespacePath, "function tree for "+settings.FUNCTION_TO_PEEK)
		println(tree.Tree.IndentString("") + "\n")
	}
}

// Function auxiliary to the above. Having made the parsers FunctionTable, each function name is associated with a
// (partially) ordered list of associated functions such that a more specific type signature comes before a less
// specific one. We will now re-represent this as a tree.
func (iz *Initializer) MakeFunctionTrees() {
	iz.P.FunctionForest = map[string]*ast.FunctionTree{}
	rc := 0
	for k, v := range iz.P.FunctionTable {
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
					iz.P.Throw("init/overload/ref", v[i].Body.GetToken())
					break
				}
			}
		}
		iz.P.FunctionForest[k] = &ast.FunctionTree{Tree: tree, RefCount: rc}
	}
}

// Note that the sigs have already been sorted on their specificity.
func (iz *Initializer) addSigToTree(tree *ast.FnTreeNode, fn *ast.PrsrFunction, pos int) *ast.FnTreeNode {
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

// We assign abstract types to the fields of the structs, and chek for consistency of
// private types, i.e. a struct type declared public can't have field types declared private.
func (iz *Initializer) AddFieldsToStructsAndCheckForConsistency() {
	// First we recurse.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.MakeFunctionForests()
	}
	iz.cmI("Adding abstract types of fields to structs.")
	iz.addFieldsToStructs()
	if iz.ErrorsExist() {
		return
	}

	iz.cmI("Adding abstract types of fields to perameterized structs.")
	iz.addFieldsToParameterizedStructs()
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

// Adds field types to structs.
func (iz *Initializer) addFieldsToStructs() {
	for i, tcc := range iz.TokenizedDeclarations[structDeclaration] {
		izTypeInfo, _ := iz.getDeclaration(decSTRUCT, tcc.IndexToken(), DUMMY)
		if izTypeInfo == nil { // This will happen if it's a parameterized declaration.
			continue
		}
		izStructInfo := izTypeInfo.(structInfo)
		typeNumber := iz.structDeclarationNumberToTypeNumber[i]
		iz.addFields(typeNumber, izStructInfo.sig)
	}
}

func (iz *Initializer) addFieldsToParameterizedStructs() {
	for _, ty := range iz.parameterizedTypeMap {
		parTypeInfo := iz.parameterizedTypeInstances[ty]
		typeNo := iz.cp.ConcreteTypeNow(parTypeInfo.astType.String())
		if iz.cp.Vm.ConcreteTypeInfo[typeNo].IsStruct() {
			iz.addFields(typeNo, parTypeInfo.fields)
		}
	}
}

func (iz *Initializer) addFields(typeNumber values.ValueType, sig ast.AstSig) {
	structInfo := iz.cp.Vm.ConcreteTypeInfo[typeNumber].(vm.StructType)
	structTypes := make([]values.AbstractType, 0, len(sig))
	for _, labelNameAndType := range sig {
		typeAst := labelNameAndType.VarType
		abType := iz.P.GetAbstractType(typeAst)
		structTypes = append(structTypes, abType)
	}
	structInfo.AbstractStructFields = structTypes
	iz.cp.Vm.ConcreteTypeInfo[typeNumber] = structInfo
}

func (iz *Initializer) tweakParameterizedTypes() {
	// We replace the astTypes in the environment for typechecking a parameterized type with AbstractTypes.
	for _, pti := range iz.parameterizedTypeInstances {
		for _, v := range pti.env.Data {
			if iz.cp.Vm.Mem[v.MLoc].T == values.TYPE {
				iz.cp.Vm.Mem[v.MLoc].V = iz.P.GetAbstractType(iz.cp.Vm.Mem[v.MLoc].V.(ast.TypeNode))
			}
		}
	}
	//
	for typename, i := range iz.parameterizedTypeMap {
		typeNo := iz.cp.ConcreteTypeNow(typename)
		typeInfo := iz.cp.Vm.ConcreteTypeInfo[typeNo]
		pti := iz.parameterizedTypeInstances[i]
		vals := make([]values.Value, len(pti.vals))
		for i, v := range pti.vals {
			vals[i] = iz.tweakValue(v)
		}
		switch typeInfo := typeInfo.(type) {
		case vm.CloneType:
			typeInfo.TypeArguments = vals
			iz.cp.Vm.ConcreteTypeInfo[typeNo] = typeInfo
		case vm.StructType:
			typeInfo.TypeArguments = vals
			iz.cp.Vm.ConcreteTypeInfo[typeNo] = typeInfo
		default:
			panic("unhandled case")
		}
	}
}

// This initializes a map in the compiler associating each type operator to
// the number of a list in the compiler containing maps from type arguments to concrete type numbers.
// This method hooks them together.
// It also tweaks the arguments to convert the payload of TYPE from the improper AstType to the correct AbstractType.
func (iz *Initializer) addParameterizedTypesToVm() {
	for _, ty := range iz.parameterizedTypeInstances { // TODO --- is there a reason why there aren't all *ast.TypeWithArguments?
		name := ty.astType.(*ast.TypeWithArguments).Name
		typeArgs := []values.Value{}
		for _, v := range ty.astType.(*ast.TypeWithArguments).Arguments {
			if v.Type == values.TYPE {
				typeArgs = append(typeArgs, values.Value{values.TYPE, iz.P.GetAbstractType(v.Value.(ast.TypeNode))})
			} else {
				typeArgs = append(typeArgs, values.Value{v.Type, v.Value})
			}
		}
		concreteType := iz.cp.ConcreteTypeNow(ty.astType.String())
		concreteTypeInfo := iz.cp.Vm.ConcreteTypeInfo[concreteType]
		if info, ok := iz.P.ParTypes2[name]; ok {
			iz.P.ParTypes2[name] = parser.TypeExpressionInfo{info.VmTypeInfo, concreteTypeInfo.IsClone(), iz.P.ParTypes2[name].PossibleReturnTypes.Union(values.MakeAbstractType(concreteType))}
		} else {
			iz.P.ParTypes2[name] = parser.TypeExpressionInfo{uint32(len(iz.cp.Vm.ParameterizedTypeInfo)), concreteTypeInfo.IsClone(), values.MakeAbstractType(values.ERROR, concreteType)}
			iz.cp.Vm.ParameterizedTypeInfo = append(iz.cp.Vm.ParameterizedTypeInfo, &values.Map{})
		}
		iz.cp.Vm.ParameterizedTypeInfo[iz.P.ParTypes2[name].VmTypeInfo] = iz.cp.Vm.ParameterizedTypeInfo[iz.P.ParTypes2[name].VmTypeInfo].Set(values.Value{values.TUPLE, typeArgs}, values.Value{values.TYPE, values.AbstractType{[]values.ValueType{concreteType}}})
	}
}

func (iz *Initializer) tweakValue(v values.Value) values.Value {
	if v.T == values.TYPE {
		v.V = iz.P.GetAbstractType(v.V.(ast.TypeNode))
	}
	return v
}

// We check that if a struct type is public, so are its fields.
func (iz *Initializer) checkTypesForConsistency() {
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
		abType := iz.P.GetAbstractTypeFromTypeSys(name)
		for _, w := range abType.Types {
			if iz.cp.Vm.ConcreteTypeInfo[w].IsPrivate() {
				iz.Throw("init/private/abstract", &tok, name)
			}
		}

	}
}

// We slurp the functions and converters out of the .so files, if necessary building or rebuilding
// the .so files first.

func (iz *Initializer) compileGoModules() {
	// First of all, the recursion.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.compileGoModules()
	}

	iz.compileGo() // This is in 'gohandler.go' in this package.
}

// Phase 4 of compilation. We compile the constants, variables, functions, and commands.
func (iz *Initializer) CompileEverything() [][]labeledParsedCodeChunk { // TODO --- do we do anything with the return type?
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
				iz.P.Throw("init/assign", dec.GetToken())
				continue
			}
			names := iz.P.GetVariablesFromSig(dec.(*ast.AssignmentExpression).Left)
			for _, name := range names {
				existingName, alreadyExists := namesToDeclarations[name]
				if alreadyExists {
					iz.P.Throw("init/name/exists/a", dec.GetToken(), iz.ParsedDeclarations[existingName[0].decType][existingName[0].decNumber].GetToken(), name)
					return nil
				}
				namesToDeclarations[name] = []labeledParsedCodeChunk{{dec, dT, i, name, iz.TokenizedDeclarations[dT][i].IndexToken()}}
			}
		}
	}
	iz.cmI("Mapping names of functions to their declarations.")
	for dT := functionDeclaration; dT <= commandDeclaration; dT++ {
		for i, dec := range iz.ParsedDeclarations[dT] {
			name, _, _, _, _, _ := iz.P.ExtractPartsOfFunction(dec) // TODO --- refactor ExtractPartsOfFunction so there's a thing called ExtractNameOfFunction which you can call there and here.
			_, alreadyExists := namesToDeclarations[name]
			if alreadyExists {
				names := namesToDeclarations[name]
				for _, existingName := range names {
					if existingName.decType == variableDeclaration || existingName.decType == constantDeclaration { // We can't redeclare variables or constants.
						iz.P.Throw("init/name/exists/b", dec.GetToken(), iz.ParsedDeclarations[existingName.decType][existingName.decNumber].GetToken(), name)
					}
					if existingName.decType == functionDeclaration && dT == commandDeclaration { // We don't want to overload anything so it can be both a command and a function 'cos that would be weird.
						iz.P.Throw("init/name/exists/c", dec.GetToken(), iz.ParsedDeclarations[existingName.decType][existingName.decNumber].GetToken(), name)
					}
				}
				namesToDeclarations[name] = append(names, labeledParsedCodeChunk{dec, dT, i, name, iz.TokenizedDeclarations[dT][i].IndexToken()})
			} else {
				namesToDeclarations[name] = []labeledParsedCodeChunk{{dec, dT, i, name, iz.TokenizedDeclarations[dT][i].IndexToken()}}
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
			namesToDeclarations[name] = []labeledParsedCodeChunk{{ast, structDeclaration, i, name[1:], tcc.IndexToken()}}
		}
		if iz.ErrorsExist() {
			return nil
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
			namesToDeclarations[name] = []labeledParsedCodeChunk{{typecheck, cloneDeclaration, i, name[1:], tcc.IndexToken()}}
		}
		if iz.ErrorsExist() {
			return nil
		}
	}

	for i, v := range iz.parameterizedTypeInstances {
		if v.typeCheck.Length() == 0 {
			continue
		}
		name := "*" + v.astType.String() // Again we mangle the name with a '*' to distinguish is from the constructor.
		v.typeCheck.ToStart()
		iz.P.TokenizedCode = v.typeCheck
		node := iz.P.ParseTokenizedChunk()
		namesToDeclarations[name] = []labeledParsedCodeChunk{{node, makeDeclaration, i, name[1:], v.typeCheck.IndexToken()}}
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
								iz.P.Throw("init/depend/cmd", dec.chunk.GetToken())
								return nil
							}
							if rhsDec.decType == variableDeclaration && dec.decType == constantDeclaration {
								iz.P.Throw("init/depend/var", dec.chunk.GetToken())
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
				iz.P.Throw("init/service/depends", tok, svName)
				return nil
			}
			iz.compileGlobalConstantOrVariable(decType, decNumber)
			if !svData.alt.Contains(iz.cp.Vm.Mem[iz.cp.That()].T) {
				iz.P.Throw("init/service/type", tok, svName, compiler.Describe(svData.alt, iz.cp.Vm))
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
		for _, dec := range groupOfDeclarations {
			switch dec.decType {
			case structDeclaration, cloneDeclaration:
				tok := dec.indexTok
				if _, ok := iz.getDeclaration(decPARAMETERIZED, tok, DUMMY); ok {
					continue loop
				}
				iz.compileTypecheck(dec.name, dec.chunk, compiler.NewEnvironment())
				continue
			case makeDeclaration:
				iz.compileTypecheck(dec.name, dec.chunk, iz.parameterizedTypeInstances[dec.decNumber].env)
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
func (iz *Initializer) compileGlobalConstantOrVariable(declarations declarationType, v int) {
	dec := iz.ParsedDeclarations[declarations][v]
	iz.cp.Cm("Compiling assignment "+dec.String(), dec.GetToken())
	lhs := dec.(*ast.AssignmentExpression).Left
	rhs := dec.(*ast.AssignmentExpression).Right
	sig, _ := iz.P.RecursivelySlurpSignature(lhs, ast.INFERRED_TYPE_AST)
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
		iz.P.Throw("comp/assign/a", dec.GetToken(), tupleLen, len(sig))
		return
	}
	if !lastIsTuple && tupleLen < len(sig) {
		iz.P.Throw("comp/assign/b", dec.GetToken(), tupleLen, len(sig))
		return
	}
	if lastIsTuple && tupleLen < len(sig)-1 {
		iz.P.Throw("comp/assign/c", dec.GetToken(), tupleLen, len(sig))
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
				iz.P.Throw("comp/assign/type/a", dec.GetToken(), sig[i].VarName, iz.cp.GetTypeNameFromNumber(head[i].T))
				return
			} else {
				iz.cp.AddVariable(envToAddTo, sig[i].VarName, vAcc, allowedTypes, rhs.GetToken())
			}
		}
	}
}

// Function auxiliary to the above.
func (iz *Initializer) getEnvAndAccessForConstOrVarDeclaration(dT declarationType, i int) (*compiler.Environment, compiler.VarAccess) {
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
func (iz *Initializer) compileTypecheck(name string, node ast.Node, newEnv *compiler.Environment) {
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
func (iz *Initializer) compileFunction(node ast.Node, private bool, outerEnv *compiler.Environment, dec declarationType) *compiler.CpFunc {
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
	functionName, _, sig, rtnSig, body, given := iz.P.ExtractPartsOfFunction(node)
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
	// First we do the local variables that are in the signature of the struct.
	for _, pair := range sig {
		iz.cp.Reserve(values.UNDEFINED_TYPE, DUMMY, node.GetToken())
		if ast.IsRef(pair.VarType) {
			iz.cp.AddVariable(fnenv, pair.VarName, compiler.REFERENCE_VARIABLE, iz.cp.Common.AnyTypeScheme, node.GetToken())
			continue
		}
		_, isVarargs := pair.VarType.(*ast.TypeDotDotDot)
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

	// And then we take care of the arguments of a parameterized type.
	vmap := map[string][]uint32{}
	for _, pair := range sig {
		if twp, ok := pair.VarType.(*ast.TypeWithParameters); ok {
			yeetTo := iz.cp.That() + 1
			// We range over the parameters of the type.
			for _, param := range twp.Parameters {
				// We may already have declared it.
				variable, ok := fnenv.GetVar(param.Name)
				// If we have, we check that it's also a type parameter, and of compatible
				// type. If not, we skip the rest of the loop ...
				if ok {
					if variable.Access != compiler.TYPE_ARGUMENT {
						iz.Throw("init/param/var", node.GetToken(), param.Name)
						continue
					}
					if !compiler.Equals(variable.Types, iz.cp.GetAlternateTypeFromConcreteTypeName(param.Type)) {
						iz.Throw("init/param/var", node.GetToken(), param.Name)
						continue
					}
				}
				// We use the 'vmap' to keep track of what type parameters have been declared
				// and when, including duplicates.
				iz.cp.Reserve(values.UNDEFINED_TYPE, DUMMY, node.GetToken())
				if _, ok := vmap[param.Name]; !ok {
					vmap[param.Name] = []uint32{iz.cp.That()}
				} else {
					vmap[param.Name] = append(vmap[param.Name], iz.cp.That())
				}
				iz.cp.AddVariable(fnenv, param.Name, compiler.TYPE_ARGUMENT, iz.cp.GetAlternateTypeFromConcreteTypeName(param.Type), node.GetToken())
			}
			variable, _ := fnenv.GetVar(pair.VarName)
			iz.cp.Emit(vm.Yeet, yeetTo, variable.MLoc)
		}
	}
	paramChecks := []any{} // Though they will in fact all be early returns.
	for k, locs := range vmap {
		if len(locs) > 1 {
			for _, v := range locs[1:] {
				errLoc := iz.cp.ReserveError("vm/type/conflict", node.GetToken(), k)
				iz.cp.Put(vm.Eqxx, locs[0], v, errLoc) // TODO: you have specialized versions of this for speed.
				paramChecks = append(paramChecks, iz.cp.VmConditionalEarlyReturn(vm.Qfls, iz.cp.That(), errLoc))
			}
		}
	}

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
			givenContext := compiler.Context{fnenv, functionName, compiler.DEF, false, nil, cpF.LoReg, areWeTracking, compiler.LF_NONE, altType()}
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
		bodyContext := compiler.Context{fnenv, functionName, ac, true, iz.cp.ReturnSigToAlternateType(rtnSig), cpF.LoReg, areWeTracking, compiler.LF_NONE, altType()}
		cpF.RtnTypes, _ = iz.cp.CompileNode(body, bodyContext) // TODO --- could we in fact do anything useful if we knew it was a constant?
		if len(paramChecks) > 0 {
			cpF.RtnTypes = cpF.RtnTypes.Union(altType(values.ERROR))
		}
		cpF.OutReg = iz.cp.That()

		if rtnSig != nil && !(body.GetToken().Type == token.GOCODE) {
			iz.cp.EmitTypeChecks(cpF.OutReg, cpF.RtnTypes, fnenv, rtnSig, ac, node.GetToken(), compiler.CHECK_RETURN_TYPES, bodyContext)
		}
		iz.cp.VmComeFrom(paramChecks...)
		iz.cp.Emit(vm.Ret)
	}
	iz.cp.Fns = append(iz.cp.Fns, &cpF)
	if ac == compiler.DEF && !cpF.RtnTypes.IsLegalDefReturn() {
		iz.P.Throw("comp/return/def", node.GetToken())
	}
	if ac == compiler.CMD && !cpF.RtnTypes.IsLegalCmdReturn() {
		iz.P.Throw("comp/return/cmd", node.GetToken())
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

// We left DUMMY values in the code for where we'd call a function which fits the interface but
// hasn't been defined yet. Now we go back and fill in the gaps.
// TODO --- why are these stored in the common parser bindle and not the common initializer
// bindle?
func (iz *Initializer) ResolveInterfaceBacktracks() {
	for _, rDat := range iz.P.Common.InterfaceBacktracks {
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
func (iz *Initializer) AddType(name, supertype string, typeNo values.ValueType) {
	iz.localConcreteTypes = iz.localConcreteTypes.Add(typeNo)
	iz.P.TypeMap[name] = values.MakeAbstractType(typeNo)
	types := []string{supertype}
	iz.cp.Common.AddTypeNumberToSharedAlternateTypes(typeNo, types...)
	types = append(types, "any")
	for _, sT := range types {
		iz.P.Common.Types[sT] = iz.P.Common.Types[sT].Insert(typeNo)
	}
}

// Adds type information to the VM.
//
// For reasons, it's a good idea to have the type info stored as an ordered list rather than a set or hashmap.
// So we need to do insertion by hand to avoid duplication.
func (iz *Initializer) AddTypeToVm(typeInfo values.AbstractTypeInfo) {
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

func (i Initializer) IsPrivate(x, y int) bool {
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
	ix  int // If it's an element of an enum, the index of the element in its type.
}

func makeKey(dOf declarationOf, tok *token.Token, ix int) decKey {
	return decKey{dOf: dOf, src: tok.Source, lNo: tok.Line, chS: tok.ChStart, ix: ix}
}

func (iz *Initializer) getDeclaration(dOf declarationOf, tok *token.Token, ix int) (any, bool) {
	result, ok := iz.Common.DeclarationMap[makeKey(dOf, tok, ix)]
	return result, ok
}

func (iz *Initializer) setDeclaration(dOf declarationOf, tok *token.Token, ix int, v any) {
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
	snippetDeclaration                   // TODO --- this no longer serves any purpose.
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
	indexTok  *token.Token
}

func (iz *Initializer) addTokenizedDeclaration(decType declarationType, line *token.TokenizedCodeChunk, private bool) {
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
func (iz *Initializer) cmI(s string) {
	if settings.SHOW_INITIALIZER {
		if iz.cp != nil && iz.cp.P != nil {
			println(text.UNDERLINE + s + text.RESET + " (" + iz.cp.P.NamespacePath + ")")
		}
	}
}

// Like everything else, the initializer sends its errors to the Common parser bindle via the parser.
func (iz *Initializer) Throw(errorID string, tok *token.Token, args ...any) {
	iz.P.Throw(errorID, tok, args...)
}

// Return whether the initializer has encountered errors.
func (iz *Initializer) ErrorsExist() bool {
	return iz.P.ErrorsExist()
}

// Methods for manipulating the function table.

func (iz *Initializer) Add(functionName string, f *ast.PrsrFunction) *ast.PrsrFunction {
	if functions, ok := iz.cp.P.FunctionTable[functionName]; ok {
		functions, conflictingFunction := iz.AddInOrder(functions, f)
		iz.cp.P.FunctionTable[functionName] = functions
		return conflictingFunction
	}
	iz.cp.P.FunctionTable[functionName] = []*ast.PrsrFunction{f}
	return nil
}

func (iz *Initializer) AddInOrder(S []*ast.PrsrFunction, f *ast.PrsrFunction) ([]*ast.PrsrFunction, *ast.PrsrFunction) {
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
