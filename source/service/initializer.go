package service

import (
	"database/sql"
	"embed"
	"os"
	"path/filepath"
	"sort"
	"testing"

	"pipefish/source/ast"
	"pipefish/source/dtypes"
	"pipefish/source/err"
	"pipefish/source/lexer"
	"pipefish/source/parser"
	"pipefish/source/settings"
	"pipefish/source/text"
	"pipefish/source/token"
	"pipefish/source/values"
	"strings"

	"github.com/lmorg/readline"
	"src.elv.sh/pkg/persistent/vector"
)

// Do not under any cicumstances remove the following comment.
//
//go:embed test-files/*
var testFolder embed.FS

// The initializer contains the methods and data which are only needed when parsing and compiling a script.
// It returns a compiler which has a parser which are capable between them of parsing, compiling, and running 
// a single block/AST of Pipefish code, and which can therefore cope with everything demanded of them via
// the REPL at runtime.

// There is one initializer per module, just as there is one compiler and one parser. The others are constructed
// recursively as made necessary by the `import` and `external` declarations.

// Definition of the initializer type.
type initializer struct {
	cp     *Compiler            // The compiler for the module being intitialized.
	p      *parser.Parser       // The parser for the module being initialized.
	initializers map[string]*initializer               // The child initializers of this one, to initialize imports and external stubs.
	TokenizedDeclarations [13]TokenizedCodeChunks      // The declarations in the script, converted from text to tokens and sorted by purpose.
	ParsedDeclarations    [13]parser.ParsedCodeChunks  // ASTs produced by parsing the tokenized chunks in the field above, sorted in the same way.
	localConcreteTypes dtypes.Set[values.ValueType]    // All the struct, enum, and clone types defined in a given module.
	goBucket           *GoBucket                       // Where the initializer keeps information gathered during parsing the script that will be needed to compile the Go modules.
	Snippets           []string                        // Names of snippet types visible to the module.
	fnIndex map[fnSource]*ast.PrsrFunction             // Map from sources to how the parser sees functions. 
	declarationMap map[decKey]any                      // TODO --- it is not at all clear that this is doing what it ought to.
	common *commonInitializerBindle                    // The information all the initializers have in common.
}

func newInitializer() *initializer {
	iz := initializer{
		initializers:       make(map[string]*initializer),
		localConcreteTypes: make(dtypes.Set[values.ValueType]),
		fnIndex:            make(map[fnSource]*ast.PrsrFunction),
		declarationMap:     make(map[decKey]any),
	}
	iz.newGoBucket()
	return &iz
}

type commonInitializerBindle struct {
	Functions map[FuncSource]*ast.PrsrFunction
}

func newCommonInitializerBindle() *commonInitializerBindle {
	b := commonInitializerBindle{
		Functions: make(map[FuncSource]*ast.PrsrFunction),
	}
	return &b
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
// in the common initializer and shared. So do I, but we get all sorts of weird bugs if we try. TODO --- investigate.
type decKey struct {
	dOf declarationOf // A struct, a label, a function ...
	src string        // The filepath to the source code.
	lNo int           // Line number of the declaration.
	ix  int           // If it's an element of an enum, the index of the element in its type.
}

func makeKey(dOf declarationOf, tok *token.Token, ix int) decKey {
	return decKey{dOf: dOf, src: tok.Source, lNo: tok.Line, ix: ix}
}

func (iz *initializer) getDeclaration(dOf declarationOf, tok *token.Token, ix int) (any, bool) {
	result, ok := iz.declarationMap[makeKey(dOf, tok, ix)]
	return result, ok
}

func (iz *initializer) setDeclaration(dOf declarationOf, tok *token.Token, ix int, v any) {
	iz.declarationMap[makeKey(dOf, tok, ix)] = v
}

// We begin by manufacturing a blank VM, a `CommonParserBindle` for all the parsers to share, and a 
// `CommonInitializerBindle` for the initializers to share. These common bindles are then passed down to the
// "children" of the intitializer and the parser when new modules are created.
func StartService(scriptFilepath string, db *sql.DB, hubServices map[string]*Service) *Service {
	iz := newInitializer()
	iz.common = newCommonInitializerBindle()
	// We then carry out five phases of initialization each of which is performed recursively on all of the 
	// modules in the dependency tree before moving on to the next. (The need to do this is in fact what
	// defines the phases, so you shouldn't bother looking for some deeper logic in that.)
	//
	// NOTE that these five phases are repeated in an un-DRY way in `test_helper.go` in this package, and that
	// any changes here will also need to be reflected there.
	cp := iz.initializeFromFilepath(BlankVm(db, hubServices), parser.NewCommonParserBindle(), scriptFilepath, "") 
	result := &Service{Cp: cp}
	if iz.ErrorsExist() {
		return result
	}
	iz.makeFunctionTableAndGoMods()
	if iz.ErrorsExist() {
		return result
	}
	iz.populateAbstractTypesAndMakeFunctionTrees()
	if iz.ErrorsExist() {
		return result
	}
	iz.compileEverything()
	if iz.ErrorsExist() {
		return result
	}
	iz.ResolveInterfaceBacktracks()
	result.Cp.Vm.OwnService = result
	return result
}

// Then we can recurse over this, passing it the same vm every time.
// This returns a compiler and and mutates the vm.
// In the case that any errors are produced, the will be in the comon bindle of the parseer of the returned compiler.
func (iz *initializer) initializeFromFilepath(mc *Vm, common *parser.CommonParserBindle, scriptFilepath, namespacePath string) *Compiler {
	sourcecode := ""
	var sourcebytes []byte
	var err error
	if scriptFilepath != "" { // In which case we're making a blank VM.
		if len(scriptFilepath) >= 11 && scriptFilepath[:11] == "test-files/" {
			sourcebytes, err = testFolder.ReadFile(scriptFilepath)
		} else {
			sourcebytes, err = os.ReadFile(MakeFilepath(scriptFilepath))
		}
		sourcecode = string(sourcebytes) + "\n"
		if err != nil {
			p := parser.New(common, scriptFilepath, sourcecode, namespacePath) // Just because it's expecting to get a compiler back, with errors contained in the common parser bindle.
			p.Throw("init/source/a", &token.Token{Source: "linking"}, scriptFilepath, err.Error())
			return NewCompiler(p)
		}
	}
	return iz.initializeFromSourcecode(mc, common, scriptFilepath, sourcecode, namespacePath)
}

func (iz *initializer) initializeFromSourcecode(mc *Vm, common *parser.CommonParserBindle, scriptFilepath, sourcecode, namespacePath string) *Compiler {
	iz.cp = newCompiler(common, scriptFilepath, sourcecode, mc, namespacePath)
	iz.p  = iz.cp.P
	iz.parseAll(scriptFilepath, sourcecode)
	iz.cp.ScriptFilepath = scriptFilepath
	if !(scriptFilepath == "" || (len(scriptFilepath) >= 5 && scriptFilepath[0:5] == "http:")) &&
		!testing.Testing() && !(len(scriptFilepath) >= 11 && scriptFilepath[:11] == "test-files/") {
		file, err := os.Stat(MakeFilepath(scriptFilepath))
		if err != nil {
			iz.Throw("init/source/b", token.Token{Source: "linking"}, scriptFilepath)
			return nil
		}
		iz.cp.Timestamp = file.ModTime().UnixMilli()
	}
	iz.p.Common.Sources[scriptFilepath] = strings.Split(sourcecode, "\n")
	return iz.cp
}

func newCompiler(common *parser.CommonParserBindle, scriptFilepath, sourcecode string, mc *Vm, namespacePath string) *Compiler {
	p := parser.New(common, scriptFilepath, sourcecode, namespacePath)
	cp := NewCompiler(p)
	cp.ScriptFilepath = scriptFilepath
	cp.Vm = mc
	cp.TupleType = cp.Reserve(values.TYPE, values.AbstractType{[]values.ValueType{values.TUPLE}, 0}, &token.Token{Source: "Builtin constant"})
	return cp
}

func (iz *initializer) cmI(s string) {
	if settings.SHOW_VMM {
		println(text.UNDERLINE + s + text.RESET)
	}
}

// This does everything up to and including parsing the code chunks, and then hands back flow of control to the
// StartService or RunTest method.
func (iz *initializer) parseAll(scriptFilepath, sourcecode string) {
	iz.cmI("Starting makeall for script " + scriptFilepath + ".")

	if !settings.OMIT_BUILTINS {
		iz.cmI("Adding mandatory imports to namespace.")
		iz.AddToNameSpace(settings.MandatoryImports)
	}
	if len(scriptFilepath) >= 4 && scriptFilepath[len(scriptFilepath)-4:] == ".hub" {
		iz.cmI("Adding hub.pf to hub namespace.")
		iz.AddToNameSpace([]string{"rsc/pipefish/hub.pf"})
	}
	iz.cmI("Making new relexer.")
	iz.p.TokenizedCode = lexer.NewRelexer(scriptFilepath, sourcecode)

	iz.cmI("Making parser and tokenized program.")
	iz.MakeParserAndTokenizedProgram()
	if iz.ErrorsExist() {
		return
	}

	iz.cmI("Parsing import and external declarations.")
	iz.ParseImportsAndExternals() // That is, parse the import declarations. The files being imported are imported by the method with the long name below.
	if iz.ErrorsExist() {
		return
	}

	iz.cmI("Initializing imports.")
	unnamespacedImports := iz.InitializeNamespacedImportsAndReturnUnnamespacedImports()
	if iz.ErrorsExist() {
		return
	}

	iz.cmI("Adding unnamespaced imports to namespace.")
	iz.AddToNameSpace(unnamespacedImports)
	if iz.ErrorsExist() {
		return
	}

	iz.cmI("Initializing external services.")
	iz.initializeExternals()
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

	iz.cmI("Creating snippet types, part 1.")
	iz.createSnippetsPart1()
	if iz.ErrorsExist() {
		return
	}

	iz.cmI("Adding types to parser.")
	iz.addTypesToParser()
	if iz.ErrorsExist() {
		return
	}

	iz.cmI("Adding constructors to parser, parsing struct declarations.")
	iz.addConstructorsToParserAndParseStructDeclarations()
	if iz.ErrorsExist() {
		return
	}

	iz.cmI("Creating struct names and labels.")
	iz.createStructNamesAndLabels()
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

	iz.cmI("Adding fields to structs.")
	iz.addFieldsToStructs()
	if iz.ErrorsExist() {
		return
	}

	iz.cmI("Creating snippet types, part 2.")
	iz.createSnippetTypesPart2()
	if iz.ErrorsExist() {
		return
	}

	// We want to ensure that no public type (whether a struct or abstract type) contains a private type.
	iz.cmI("Checking types for consistency of encapsulation.")
	iz.checkTypesForConsistency()
	if iz.ErrorsExist() {
		return
	}

	iz.cmI("Parsing everything.")
	iz.ParseEverything()
	if iz.ErrorsExist() {
		return
	}
	// We hand back flow of control to StartService or RunTest.
}

func (iz *initializer) ErrorsExist() bool {
	return iz.p.ErrorsExist()
}


// Do not under any cicumstances remove the following comment.
//
//go:embed rsc/pipefish/*
var folder embed.FS

type Section int

const (
	ImportSection Section = iota
	ExternalSection
	VarSection
	CmdSection
	DefSection
	LanguagesSection
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
	golangDeclaration                    // Pure golang in a block; the Charm functions with golang bodies don't go here but under function or command as they were declared.

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

func (iz *initializer) AddToNameSpace(thingsToImport []string) {
	for _, fname := range thingsToImport {
		var libDat []byte
		var err error
		if len(fname) >= 13 && fname[:13] == "rsc/pipefish/" {
			libDat, err = folder.ReadFile(fname)
		} else {
			libDat, err = os.ReadFile(MakeFilepath(fname))
		}
		if err != nil {
			iz.p.Throw("init/import/found", &token.Token{})
		}
		stdImp := strings.TrimRight(string(libDat), "\n") + "\n"
		iz.p.TokenizedCode = lexer.NewRelexer(fname, stdImp)
		iz.MakeParserAndTokenizedProgram() // This is cumulative, it throws them all into the parser together.
		iz.p.Common.Sources[fname] = strings.Split(stdImp, "\n")
	}
}

func (iz *initializer) addTokenizedDeclaration(decType declarationType, line *token.TokenizedCodeChunk, private bool) {
	line.Private = private
	iz.TokenizedDeclarations[decType] = append(iz.TokenizedDeclarations[decType], line)
}

var typeMap = map[string]declarationType{"struct": structDeclaration, "enum": enumDeclaration, "snippet": snippetDeclaration,
	"abstract": abstractDeclaration, "clone": cloneDeclaration, "interface": interfaceDeclaration}


// This method takes the tokens from the relexer and splits it up into code types according to the headword,
// which is discarded. It breaks these up into function declarations, variable intializations, etc.

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
	isPrivate := false
	var (
		tok token.Token
	)

	tok = iz.p.TokenizedCode.NextToken() // note that we've already removed leading newlines.
	if settings.SHOW_RELEXER && !(settings.IGNORE_BOILERPLATE && settings.ThingsToIgnore.Contains(tok.Source)) {
		println(tok.Type, tok.Literal)
	}

	if tok.Type == token.EOF { // An empty file should still initiate a service, but one with no data.
		return
	}
	if !token.TokenTypeIsHeadword(tok.Type) {
		iz.Throw("init/head", tok)
		return
	}

	currentSection = tokenTypeToSection[tok.Type]
	colonMeansFunctionOrCommand := (currentSection == CmdSection || currentSection == DefSection)

	line := token.NewCodeChunk()

	for tok = iz.p.TokenizedCode.NextToken(); tok.Type != token.EOF; tok = iz.p.TokenizedCode.NextToken() {
		if settings.SHOW_RELEXER && !(settings.IGNORE_BOILERPLATE && settings.ThingsToIgnore.Contains(tok.Source)) {
			println(tok.Type, tok.Literal)
		}
		if token.TokenTypeIsHeadword(tok.Type) {
			if tok.Literal == "import" {
				iz.Throw("init/import/first", tok)
			}
			currentSection = tokenTypeToSection[tok.Type]
			isPrivate = false
			lastTokenWasColon = false
			colonMeansFunctionOrCommand = (currentSection == CmdSection || currentSection == DefSection)
			continue
		}
		if tok.Type == token.PRIVATE {
			if isPrivate {
				iz.Throw("init/private", tok)
			}
			isPrivate = true
			continue
		}
		if currentSection == TypesSection && tok.Type == token.IDENT {
			tD, ok := typeMap[tok.Literal]
			if ok {
				typeDefined = tD
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
		if ((tok.Type == token.NEWLINE) && !lastTokenWasColon && indentCount == 0 && line.Length() != 0) ||
			tok.Type == token.GOCODE {
			if tok.Type == token.GOCODE {
				line.Append(tok)
			}
			if beginCount != 0 {
				iz.Throw("init/close", tok)
				beginCount = 0 // Prevents error storm.
				typeDefined = declarationType(DUMMY)
				colonMeansFunctionOrCommand = (currentSection == CmdSection || currentSection == DefSection)
				continue
			}
			switch currentSection {
			case ImportSection:
				iz.addTokenizedDeclaration(importDeclaration, line, isPrivate)
			case LanguagesSection:
				iz.addTokenizedDeclaration(snippetDeclaration, line, isPrivate)
			case ExternalSection:
				iz.addTokenizedDeclaration(externalDeclaration, line, isPrivate)
			case CmdSection:
				line.ToStart()
				if line.Length() == 1 && line.NextToken().Type == token.GOCODE {
					iz.addTokenizedDeclaration(golangDeclaration, line, isPrivate)
				} else {
					iz.addTokenizedDeclaration(commandDeclaration, line, isPrivate)
				}
			case DefSection:
				line.ToStart()
				if line.Length() == 1 && line.NextToken().Type == token.GOCODE {
					iz.addTokenizedDeclaration(golangDeclaration, line, isPrivate)
				} else {
					iz.addTokenizedDeclaration(functionDeclaration, line, isPrivate)
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
					iz.addTokenizedDeclaration(variableDeclaration, line, isPrivate)
				} else {
					iz.addTokenizedDeclaration(constantDeclaration, line, isPrivate)
				}
			case TypesSection:
				if typeDefined != declarationType(DUMMY) {
					iz.addTokenizedDeclaration(typeDefined, line, isPrivate)
				} else {
					iz.Throw("init/type/form", tok)
				}
			default:
				panic("Unhandled section type.")
			}
			line = token.NewCodeChunk()
			typeDefined = declarationType(DUMMY)
			colonMeansFunctionOrCommand = (currentSection == CmdSection || currentSection == DefSection)
			continue
		}

		if (tok.Type == token.NEWLINE) && line.Length() == 0 {
			continue
		}

		lastTokenWasColon = (tok.Type == token.COLON)

		if (lastTokenWasColon || tok.Type == token.PIPE) && colonMeansFunctionOrCommand { // If we found the first : in a command/function declaration, then what is to the left of the colon is the command/function's signature.
			colonMeansFunctionOrCommand = false
			iz.addWordsToParser(line)
		}
		line.Append(tok)
	}

	iz.p.Common.Errors = err.MergeErrors(iz.p.TokenizedCode.(*lexer.Relexer).GetErrors(), iz.p.Common.Errors)
}

func (iz *initializer) ParseImportsAndExternals() {
	for kindOfDeclarationToParse := importDeclaration; kindOfDeclarationToParse <= externalDeclaration; kindOfDeclarationToParse++ {
		iz.ParsedDeclarations[kindOfDeclarationToParse] = parser.ParsedCodeChunks{}
		for chunk := 0; chunk < len(iz.TokenizedDeclarations[kindOfDeclarationToParse]); chunk++ {
			iz.p.TokenizedCode = iz.TokenizedDeclarations[kindOfDeclarationToParse][chunk]
			iz.TokenizedDeclarations[kindOfDeclarationToParse][chunk].ToStart()
			iz.ParsedDeclarations[kindOfDeclarationToParse] = append(iz.ParsedDeclarations[kindOfDeclarationToParse], iz.p.ParseTokenizedChunk())
		}
	}
}

func (iz *initializer) getPartsOfImportOrExternalDeclaration(imp ast.Node) (string, string) {
	namespace := ""
	scriptFilepath := ""
	switch imp := (imp).(type) {
	case *ast.StringLiteral:
		scriptFilepath = imp.Value
		if settings.StandardLibraries.Contains(scriptFilepath) {
			scriptFilepath = settings.PipefishHomeDirectory + "lib/" + scriptFilepath + ".pf"
		}
		namespace = text.ExtractFileName(scriptFilepath)
		return namespace, scriptFilepath
	case *ast.Identifier:
		namespace = imp.Value
		return namespace, scriptFilepath
	case *ast.InfixExpression:
		if imp.GetToken().Literal != "::" {
			iz.Throw("init/import/infix", imp.Token)
		}
		lhs := imp.Args[0]
		rhs := imp.Args[2]
		switch rhs := rhs.(type) {
		case *ast.StringLiteral:
			scriptFilepath = rhs.Value
			if settings.StandardLibraries.Contains(scriptFilepath) {
				namespace = scriptFilepath
				scriptFilepath = settings.PipefishHomeDirectory + "lib/" + scriptFilepath + ".pf"
			}
			switch lhs := lhs.(type) {
			case *ast.Identifier:
				if lhs.Value != "NULL" {
					namespace = lhs.Value
				} else {
					namespace = ""
				}
				return namespace, scriptFilepath
			default:
				iz.Throw("init/import/ident", *lhs.GetToken())
			}
		default:
			iz.Throw("init/import/string", *lhs.GetToken())
		}
	}
	iz.Throw("init/import/weird", *imp.GetToken())
	return "", ""
}

// We need to declare all the types as suffixes for all the user-defined types
func (iz *initializer) addTypesToParser() { /// TODO --- some of this seems to replicate boilerplate in the parsing functions, so you should be able to remove the latter.
	for kindOfType := enumDeclaration; kindOfType <= cloneDeclaration; kindOfType++ {
		for chunk := 0; chunk < len(iz.TokenizedDeclarations[kindOfType]); chunk++ {
			// Each of them should begin with the name of the type being declared, and then followed by an =..
			iz.TokenizedDeclarations[kindOfType][chunk].ToStart()
			tok1 := iz.TokenizedDeclarations[kindOfType][chunk].NextToken()
			tok2 := iz.TokenizedDeclarations[kindOfType][chunk].NextToken()
			if tok1.Type != token.IDENT || tok2.Type != token.ASSIGN {
				iz.Throw("init/type/form/a", tok1)
				continue
			}
			name := tok1.Literal
			if iz.p.Suffixes.Contains(name) {
				iz.Throw("init/type/exists", tok1)
				continue
			}
			iz.p.Suffixes.Add(name)
			iz.p.Suffixes.Add(name + "?")
		}
	}
}

func (iz *initializer) addConstructorsToParserAndParseStructDeclarations() {
	// First we need to make the struct types into types so the parser parses them properly.
	for chunk := 0; chunk < len(iz.TokenizedDeclarations[structDeclaration]); chunk++ {
		iz.TokenizedDeclarations[structDeclaration][chunk].ToStart()
		// Note that the first two tokens should already have been validated by the createTypeSuffixes method as IDENT and ASSIGN respectively.
		tok1 := iz.TokenizedDeclarations[structDeclaration][chunk].NextToken()
		iz.TokenizedDeclarations[structDeclaration][chunk].NextToken() // We skip the = sign.
		iz.p.AllFunctionIdents.Add(tok1.Literal)
		iz.p.Functions.Add(tok1.Literal)
	}
	// Now we can parse them.
	for chunk := 0; chunk < len(iz.TokenizedDeclarations[structDeclaration]); chunk++ {
		iz.p.TokenizedCode = iz.TokenizedDeclarations[structDeclaration][chunk]
		iz.TokenizedDeclarations[structDeclaration][chunk].ToStart()
		iz.ParsedDeclarations[structDeclaration] = append(iz.ParsedDeclarations[structDeclaration], iz.p.ParseTokenizedChunk())
	}
}

func (iz *initializer) createSnippetsPart1() {
	for _, v := range iz.TokenizedDeclarations[snippetDeclaration] {
		v.ToStart()
		// Note that the first tokens should already have been validated by the createTypeSuffixes method as IDENT.
		tok1 := v.NextToken()
		name := tok1.Literal
		iz.Snippets = append(iz.Snippets, name)
		iz.p.AllFunctionIdents.Add(name)
		iz.p.Functions.Add(name)
	}
}

func (iz *initializer) ParseEverything() {
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

// Having made the parsers FunctionTable, each function name is associated with a (partially) ordered list of
// associated functions such that a more specific type signature comes before a less specific one.
// We will now re-represent this as a tree.
func (iz *initializer) MakeFunctionTrees() {
	iz.p.FunctionForest = map[string]*ast.FunctionTree{}
	rc := 0
	for k, v := range iz.p.FunctionTable {
		tree := &ast.FnTreeNode{Fn: nil, Branch: []*ast.TypeNodePair{}}
		for i := range v {
			tree = iz.addSigToTree(tree, v[i], 0)

			refs := 0 // Overloaded functions must have the same number of reference variables, which go at the start.
			for ; refs < len(v[i].NameSig) && v[i].NameSig[refs].VarType == "ref"; refs++ {
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
		if settings.FUNCTION_TO_PEEK != "" && k == settings.FUNCTION_TO_PEEK {
			println("Function tree for " + k)
			println(iz.p.FunctionForest[k].Tree.IndentString("") + "\n")
		}
	}
}

// Note that the sigs have already been sorted on their specificity.
func (iz *initializer) addSigToTree(tree *ast.FnTreeNode, fn *ast.PrsrFunction, pos int) *ast.FnTreeNode {
	sig := fn.Sig
	nameSig := fn.NameSig
	if pos < len(sig) {
		var currentTypeName string
		currentAbstractType := sig[pos].VarType
		if nameSig[pos].VarType == "bling" {
			currentTypeName = nameSig[pos].VarName
		} else {
			currentTypeName = nameSig[pos].VarType
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
				if currentTypeName == "tuple" && !(branch.Type.Contains(values.TUPLE)) {
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

// This extracts the words from a function definition and decides on their "grammatical" role:
// are they prefixes, suffixes, bling?

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
			iz.Throw("init/inexplicable", tok)
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


func (iz *initializer) makeFunctionTableAndGoMods() {
	// First we recursively call the method on all the dependencies of the module.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.makeFunctionTableAndGoMods()
	}
	// The vm needs to know how to describe the abstract types in words.
	iz.addAbstractTypesToVm()
	if iz.ErrorsExist() {
		return
	}

	// The compiler uses a somewhat richer type representation than the one used by the compiler and the
	// runtime.
	iz.makeAlternateTypesFromAbstractTypes()

	// An intermediate step that groups the functions by name and orders them by specificity in a "function table".
	// We return a GoHandler for the next step.
	iz.MakeFunctionTable()
	if iz.ErrorsExist() {
		return
	}

	// We slurp the functions and converters out of the .so files, if necessary building or rebuilding
	// the .so files first.
	iz.compileGo()

	// We add in constructors for the structs, snippets, and clones.
	iz.compileConstructors()
	if iz.ErrorsExist() {
		return
	}
}

func (iz *initializer) InitializeNamespacedImportsAndReturnUnnamespacedImports() []string {
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
		newIz := newInitializer()
		newIz.common = iz.common
		iz.initializers[scriptFilepath] = newIz
		newCp := newIz.initializeFromFilepath(iz.cp.Vm, iz.p.Common, scriptFilepath, namespace+"."+iz.p.NamespacePath)
		iz.cp.Services[namespace] = &Service{newCp, false}
		for k, v := range newIz.declarationMap { // See note above. It's not clear why we have to do it this way rather than sharing it in the bindle, and we should find out.
			iz.declarationMap[k] = v
		}
		iz.p.NamespaceBranch[namespace] = &parser.ParserData{newCp.P, scriptFilepath}
		newCp.P.Private = iz.IsPrivate(int(importDeclaration), i)
	}
	return unnamespacedImports
}

// OTOH, we want the type information spread across the parsers and shared in the common parser bindle to
// collectively be the any source of truth for our type system.
// But it can't be the only *representation* of the truth, becase that would slow things down 'cos the compiler
// would have to keep converting abstract types to alternate types to build the type schemes with.
// The solution is to build the alternate type schemes once and for all from the alternate types, after we've
// entirely finished generating the data in the parsers.
func (iz *initializer) makeAlternateTypesFromAbstractTypes() {
	iz.cp.typeNameToTypeScheme = make(map[string]AlternateType)
	for typename, abType := range iz.p.TypeMap {
		iz.cp.typeNameToTypeScheme[typename] = AbstractTypeToAlternateType(abType)
	}
	for typename, abType := range iz.p.Common.Types {
		iz.cp.typeNameToTypeScheme[typename] = AbstractTypeToAlternateType(abType)
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
	"$logging":      {altType(), values.Value{}, true, GLOBAL_VARIABLE_PRIVATE}, // The values have to be extracted from the compiler.
	"$cliDirectory": {altType(values.STRING), values.Value{values.STRING, ""}, true, GLOBAL_VARIABLE_PRIVATE},
	"$cliArguments": {altType(values.LIST), values.Value{values.LIST, vector.Empty}, true, GLOBAL_VARIABLE_PRIVATE},
}

// At this point we have our functions as parsed code chunks in the uP.Parser.ParsedDeclarations(functionDeclaration)
// slice. We want to read their signatures and order them according to specificity for the purposes of
// implementing overloading.
func (iz *initializer) MakeFunctionTable() {
	for j := functionDeclaration; j <= commandDeclaration; j++ {
		for i := 0; i < len(iz.ParsedDeclarations[j]); i++ {
			tok := iz.ParsedDeclarations[j][i].GetToken()
			functionName, position, sig, rTypes, body, given := iz.p.ExtractPartsOfFunction(iz.ParsedDeclarations[j][i])
			if body == nil {
				iz.p.Throw("init/func/body", tok)
				return
			}
			if body.GetToken().Type == token.PRELOG && body.GetToken().Literal == "" {
				body.(*ast.LogExpression).Value = parser.DescribeFunctionCall(functionName, &sig)
			}
			if iz.ErrorsExist() {
				return
			}
			functionToAdd := &ast.PrsrFunction{FName: functionName, Sig: iz.p.MakeAbstractSigFromStringSig(sig), NameSig: sig, Position: position, NameRets: rTypes, RtnSig: iz.p.MakeAbstractSigFromStringSig(rTypes), Body: body, Given: given,
				Cmd: j == commandDeclaration, Private: iz.IsPrivate(int(j), i), Number: DUMMY, Compiler: iz.cp, Tok: body.GetToken()}
			iz.fnIndex[fnSource{j, i}] = functionToAdd
			if iz.shareable(functionToAdd) || settings.MandatoryImportSet().Contains(tok.Source) {
				iz.cmI("Adding " + functionName + " to common functions.")
				iz.common.Functions[FuncSource{tok.Source, tok.Line, functionName, position}] = functionToAdd
			}
			conflictingFunction := iz.p.FunctionTable.Add(iz.p, functionName, functionToAdd)
			if conflictingFunction != nil && conflictingFunction != functionToAdd {
				iz.p.Throw("init/overload/a", body.GetToken(), functionName, conflictingFunction.Tok.Line)
				return
			}
			if body.GetToken().Type == token.GOCODE {
				iz.goBucket.sources.Add(body.GetToken().Source)
				iz.goBucket.functions[body.GetToken().Source] = append(iz.goBucket.functions[body.GetToken().Source], functionToAdd)
				body.(*ast.GolangExpression).Sig = sig
				body.(*ast.GolangExpression).ReturnTypes = rTypes
			}
		}
	}
}

// For compiling a top-level function.
func (iz *initializer) compileFunction(node ast.Node, private bool, outerEnv *Environment, dec declarationType) *CpFunc {
	if info, functionExists := iz.getDeclaration(decFUNCTION, node.GetToken(), DUMMY); functionExists {
		iz.cp.Fns = append(iz.cp.Fns, info.(*CpFunc))
		return info.(*CpFunc)
	}
	cpF := CpFunc{}
	var ac cpAccess
	if dec == functionDeclaration {
		ac = DEF
	} else {
		ac = CMD
		cpF.Command = true
	}
	cpF.Private = private
	functionName, _, sig, rtnSig, body, given := iz.p.ExtractPartsOfFunction(node)
	iz.cp.cm("Compiling function '"+functionName+"' with sig "+sig.String()+".", node.GetToken())

	if body.GetToken().Type == token.PRELOG && body.GetToken().Literal == "" {
		body.(*ast.LogExpression).Value = parser.DescribeFunctionCall(functionName, &sig)
	}
	if iz.ErrorsExist() {
		return nil
	}

	if body.GetToken().Type == token.XCALL {
		Xargs := body.(*ast.PrefixExpression).Args
		cpF.Xcall = &XBindle{ExternalServiceOrdinal: uint32(Xargs[0].(*ast.IntegerLiteral).Value), FunctionName: Xargs[1].(*ast.StringLiteral).Value, Position: uint32(Xargs[2].(*ast.IntegerLiteral).Value)}
		serializedTypescheme := Xargs[3].(*ast.StringLiteral).Value
		cpF.RtnTypes = iz.cp.deserializeTypescheme(serializedTypescheme)
	}
	fnenv := NewEnvironment()
	fnenv.Ext = outerEnv
	cpF.LoReg = iz.cp.MemTop()
	for _, pair := range sig {
		iz.cp.Reserve(values.UNDEFINED_VALUE, DUMMY, node.GetToken())
		if pair.VarType == "ref" {
			iz.cp.AddVariable(fnenv, pair.VarName, REFERENCE_VARIABLE, iz.cp.Vm.AnyTypeScheme, node.GetToken())
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
			iz.cp.AddVariable(fnenv, pair.VarName, FUNCTION_ARGUMENT, AlternateType{TypedTupleType{iz.cp.getAlternateTypeFromTypeName(pair.VarType)}}, node.GetToken())
		} else {
			if pair.VarType != "bling" {
				iz.cp.AddVariable(fnenv, pair.VarName, FUNCTION_ARGUMENT, iz.cp.getAlternateTypeFromTypeName(pair.VarType), node.GetToken())
			}
		}
	}
	cpF.HiReg = iz.cp.MemTop()
	cpF.CallTo = iz.cp.CodeTop()
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
		cpF.locOfTupleAndVarargData = iz.cp.Reserve(values.INT_ARRAY, tupleData, node.GetToken())
	} else {
		cpF.locOfTupleAndVarargData = DUMMY
	}
	switch body.GetToken().Type {
	case token.BUILTIN:
		name := body.(*ast.BuiltInExpression).Name
		types, ok := BUILTINS[name]
		if ok {
			cpF.RtnTypes = types.T
		} else {
			typeNumber, ok := iz.cp.getConcreteType(name) // We treat the clone constructors and short struct constructors as builtins. TODO --- todon't.
			if ok {
				cpF.RtnTypes = altType(typeNumber)
			}
		}
		cpF.Builtin = name
	case token.GOCODE:
		cpF.GoNumber = uint32(len(iz.cp.Vm.GoFns))
		cpF.HasGo = true
		iz.cp.Vm.GoFns = append(iz.cp.Vm.GoFns, GoFn{Code: body.(*ast.GolangExpression).GoFunction})
	case token.XCALL:
	default:
		logFlavor := LF_NONE
		if iz.cp.getLoggingScope() == 2 {
			logFlavor = LF_TRACK
		}
		if given != nil {
			iz.cp.ThunkList = []ThunkData{}
			givenContext := context{fnenv, functionName, DEF, false, nil, cpF.LoReg, logFlavor}
			iz.cp.compileGivenBlock(given, givenContext)
			cpF.CallTo = iz.cp.CodeTop()
			if len(iz.cp.ThunkList) > 0 {
				iz.cp.cm("Initializing thunks for outer function.", body.GetToken())
			}
			for _, thunks := range iz.cp.ThunkList {
				iz.cp.Emit(Thnk, thunks.dest, thunks.value.MLoc, thunks.value.CAddr)
			}
		}
		// Logging the function call, if we do it, goes here.
		// 'stringify' is secret sauce, users aren't meant to know it exists. TODO --- conceal it better.
		// If the body starts with a 'PRELOG' then the user has put in a logging statement which should override the tracking.
		if logFlavor == LF_TRACK && !(body.GetToken().Type == token.PRELOG) && (functionName != "stringify") {
			iz.cp.track(trFNCALL, node.GetToken(), functionName, sig, cpF.LoReg)
		}

		// Now the main body of the function, just as a lagniappe.
		bodyContext := context{fnenv, functionName, ac, true, iz.cp.returnSigToAlternateType(rtnSig), cpF.LoReg, logFlavor}
		cpF.RtnTypes, _ = iz.cp.CompileNode(body, bodyContext) // TODO --- could we in fact do anything useful if we knew it was a constant?
		cpF.OutReg = iz.cp.That()

		if rtnSig != nil && !(body.GetToken().Type == token.GOCODE) {
			iz.cp.emitTypeChecks(cpF.OutReg, cpF.RtnTypes, fnenv, rtnSig, ac, node.GetToken(), CHECK_RETURN_TYPES)
		}

		iz.cp.Emit(Ret)
	}
	iz.cp.Fns = append(iz.cp.Fns, &cpF)
	if ac == DEF && !cpF.RtnTypes.IsLegalDefReturn() {
		iz.p.Throw("comp/return/def", node.GetToken())
	}
	if ac == CMD && !cpF.RtnTypes.IsLegalCmdReturn() {
		iz.p.Throw("comp/return/cmd", node.GetToken())
	}
	iz.setDeclaration(decFUNCTION, node.GetToken(), DUMMY, &cpF)

	// We capture the 'stringify' function for use by the VM. TODO --- somewhere else altogether.

	if functionName == "stringify" {
		iz.cp.Vm.Stringify = &cpF
	}

	return &cpF
}

// This is a fairly crude way of slurping the names of functions, commands, constants, and variables out of a declaration.
// It is crude in that it will slurp other things too: type names, for example; bling; local true variables in cmds. We can live
// with the false positives so long as there are no false negatives.
func (iz *initializer) extractNamesFromCodeChunk(dec labeledParsedCodeChunk) dtypes.Set[string] {
	if dec.decType == variableDeclaration || dec.decType == constantDeclaration {
		return ast.ExtractAllNames(dec.chunk.(*ast.AssignmentExpression).Right)
	}
	_, _, sig, _, body, given := iz.p.ExtractPartsOfFunction(iz.ParsedDeclarations[dec.decType][dec.decNumber])
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

// Now we need to do a big topological sort on everything, according to the following rules:
// A function, variable or constant can't depend on a command.
// A constant can't depend on a variable.
// A variable or constant can't depend on itself.
func (iz *initializer) compileEverything() [][]labeledParsedCodeChunk {
	// First of all, the recursion.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.compileEverything()
	}
	// And now we compile the module.
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
				namesToDeclarations[name] = []labeledParsedCodeChunk{{dec, dT, i}}
			}
		}
	}
	iz.cmI("Extracting variable names from functions.")
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
				namesToDeclarations[name] = append(names, labeledParsedCodeChunk{dec, dT, i})
			} else {
				namesToDeclarations[name] = []labeledParsedCodeChunk{{dec, dT, i}}
			}
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
							if rhsDec.decType == variableDeclaration {
								iz.p.Throw("init/depend/const/var", dec.chunk.GetToken())
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
	// $logging
	loggingOptionsType := values.ValueType(iz.cp.typeNameToTypeScheme["$Logging"][0].(simpleType))
	loggingScopeType := values.ValueType(iz.cp.typeNameToTypeScheme["$LoggingScope"][0].(simpleType))
	value := val(loggingOptionsType, []values.Value{{loggingScopeType, 1}})
	serviceVariables["$logging"] = serviceVariableData{altType(loggingOptionsType), value, true, GLOBAL_CONSTANT_PRIVATE}
	// $cliDirectory
	cliDirData := serviceVariables["$cliDirectory"]
	dir, _ := os.Getwd()
	cliDirData.deflt = val(values.STRING, dir)
	serviceVariables["$cliDirectory"] = cliDirData
	// $cliArguments
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
	cliArgsData := serviceVariables["$cliArguments"]
	cliArgsData.deflt = val(values.LIST, cliArgs)
	serviceVariables["$cliArguments"] = cliArgsData

	// Add variables to environment.
	for svName, svData := range serviceVariables {
		rhs, ok := graph[svName]
		if ok {
			tok := namesToDeclarations[svName][0].chunk.GetToken()
			decType := namesToDeclarations[svName][0].decType
			decNumber := namesToDeclarations[svName][0].decNumber
			if decType == variableDeclaration && svData.mustBeConst {
				iz.p.Throw("init/service/const", tok)
				return nil
			}
			if len(rhs) > 0 {
				iz.p.Throw("init/service/depends", tok)
				return nil
			}
			iz.compileGlobalConstantOrVariable(decType, decNumber)
			if !svData.ty.Contains(iz.cp.Vm.Mem[iz.cp.That()].T) {
				iz.p.Throw("init/service/type", tok)
				return nil
			}
			delete(graph, svName)
		} else {
			dummyTok := token.Token{}
			vAcc := svData.vAcc
			envToAddTo := iz.cp.GlobalVars
			if vAcc == GLOBAL_CONSTANT_PUBLIC || vAcc == GLOBAL_CONSTANT_PRIVATE {
				envToAddTo = iz.cp.GlobalConsts
			}
			iz.cp.Reserve(svData.deflt.T, svData.deflt.V, &dummyTok)
			iz.cp.AddVariable(envToAddTo, svName, vAcc, altType(svData.deflt.T), &dummyTok)
		}
	}

	iz.cmI("Performing sort on digraph.")
	order := graph.Tarjan()

	// We now have a list of lists of names to declare. We're off to the races!
	iz.cmI("Compiling the variables/functions in the order give by the sort.")
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
		iz.cp.recursionStore = []bkRecursion{} // The compiler will put all the places it needs to backtrack for recursion here.
		fCount := uint32(len(iz.cp.Fns))       // We can give the function data in the parser the right numbers for the group of functions in the parser before compiling them, since we know what order they come in.
		for _, dec := range groupOfDeclarations {
			iz.fnIndex[fnSource{dec.decType, dec.decNumber}].Number = fCount
			iz.fnIndex[fnSource{dec.decType, dec.decNumber}].Compiler = iz.cp
			fCount++
		}
		for _, dec := range groupOfDeclarations {
			switch dec.decType {
			case functionDeclaration:
				iz.compileFunction(iz.ParsedDeclarations[functionDeclaration][dec.decNumber], iz.IsPrivate(int(dec.decType), dec.decNumber), iz.cp.GlobalConsts, functionDeclaration)
			case commandDeclaration:
				iz.compileFunction(iz.ParsedDeclarations[commandDeclaration][dec.decNumber], iz.IsPrivate(int(dec.decType), dec.decNumber), iz.cp.GlobalVars, commandDeclaration)
			}
			iz.fnIndex[fnSource{dec.decType, dec.decNumber}].Number = uint32(len(iz.cp.Fns) - 1) // TODO --- is this necessary given the line a little above which seems to do this pre-emptively?
		}
		// We've reached the end of the group and can go back and put the recursion in.
		for _, rDat := range iz.cp.recursionStore {
			funcNumber := rDat.functionNumber
			addr := rDat.address
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

func (iz *initializer) compileGlobalConstantOrVariable(declarations declarationType, v int) {
	dec := iz.ParsedDeclarations[declarations][v]
	iz.cp.cm("Compiling assignment "+dec.String(), dec.GetToken())
	lhs := dec.(*ast.AssignmentExpression).Left
	rhs := dec.(*ast.AssignmentExpression).Right
	sig, _ := iz.p.RecursivelySlurpSignature(lhs, "*inferred*")
	if iz.ErrorsExist() {
		return
	}
	rollbackTo := iz.cp.getState() // Unless the assignment generates code, i.e. we're creating a lambda function or a snippet, then we can roll back the declarations afterwards.
	ctxt := context{env: iz.cp.GlobalVars, access: INIT, lowMem: DUMMY, logFlavor: LF_INIT}
	iz.cp.CompileNode(rhs, ctxt)
	if iz.ErrorsExist() {
		return
	}
	iz.cp.Emit(Ret)
	iz.cp.cm("Calling Run from vmMaker's compileGlobalConstantOrVariable method.", dec.GetToken())
	iz.cp.Vm.Run(uint32(rollbackTo.code))
	result := iz.cp.Vm.Mem[iz.cp.That()]
	if !iz.cp.Vm.codeGeneratingTypes.Contains(result.T) { // We don't want to roll back the code generated when we make a lambda or a snippet.
		iz.cp.rollback(rollbackTo, dec.GetToken())
	}

	envToAddTo, vAcc := iz.getEnvAndAccessForConstOrVarDeclaration(declarations, v)

	last := len(sig) - 1
	lastIsTuple := sig[last].VarType == "tuple"
	rhsIsTuple := result.T == values.TUPLE
	tupleLen := 1
	if rhsIsTuple {
		tupleLen = len(result.V.([]values.Value))
	}
	if !lastIsTuple && tupleLen != len(sig) {
		iz.p.Throw("comp/assign/a", dec.GetToken(), tupleLen, len(sig))
		return
	}
	if lastIsTuple && tupleLen < len(sig)-1 {
		iz.p.Throw("comp/assign/b", dec.GetToken(), tupleLen, len(sig))
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
		if sig[i].VarType == "*inferred*" {
			iz.cp.AddVariable(envToAddTo, sig[i].VarName, vAcc, altType(head[i].T), rhs.GetToken())
		} else {
			allowedTypes := iz.cp.getAlternateTypeFromTypeName(sig[i].VarType)
			if allowedTypes.isNoneOf(head[i].T) {
				iz.p.Throw("comp/assign/type", dec.GetToken())
				return
			} else {
				iz.cp.AddVariable(envToAddTo, sig[i].VarName, vAcc, allowedTypes, rhs.GetToken())
			}
		}
	}
}

func (iz *initializer) getEnvAndAccessForConstOrVarDeclaration(dT declarationType, i int) (*Environment, varAccess) {
	isPrivate := iz.IsPrivate(int(dT), i)
	var vAcc varAccess
	envToAddTo := iz.cp.GlobalConsts
	if dT == constantDeclaration {
		if isPrivate {
			vAcc = GLOBAL_CONSTANT_PRIVATE
		} else {
			vAcc = GLOBAL_CONSTANT_PUBLIC
		}
	} else {
		envToAddTo = iz.cp.GlobalVars
		if isPrivate {
			vAcc = GLOBAL_VARIABLE_PRIVATE
		} else {
			vAcc = GLOBAL_VARIABLE_PUBLIC
		}
	}
	return envToAddTo, vAcc
}

// A function is shareable if at least one of its parameters must be of a type declared in the same module.
func (iz *initializer) shareable(f *ast.PrsrFunction) bool {
	for _, pair := range f.NameSig {
		ty := pair.VarType
		if ty == "bling" {
			continue
		}
		if len(ty) >= 3 && ty[:3] == "..." {
			ty = ty[3:]
		}
		if ty == "struct" || ty == "enum" {
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

func (iz *initializer) ResolveInterfaceBacktracks() {
	for _, rDat := range iz.p.Common.InterfaceBacktracks {
		prsrFunction := rDat.Fn
		resolvingCompiler := prsrFunction.Compiler.(*Compiler)
		cpFunction := resolvingCompiler.Fns[prsrFunction.Number]
		addr := rDat.Addr
		iz.cp.Vm.Code[addr].Args[0] = cpFunction.CallTo
		iz.cp.Vm.Code[addr].Args[1] = cpFunction.LoReg
		iz.cp.Vm.Code[addr].Args[2] = cpFunction.HiReg
		iz.cp.Vm.Code[addr+1].Args[1] = cpFunction.OutReg
	}
}

type funcWithName struct {
	name  string
	pFunc *ast.PrsrFunction
}

func (iz *initializer) populateAbstractTypesAndMakeFunctionTrees() {
	// First we recurse.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.populateAbstractTypesAndMakeFunctionTrees()
	}

	// Now we pull in all the shared functions that fulfill the interface types, populating the types as we go.
	for _, tcc := range iz.TokenizedDeclarations[interfaceDeclaration] {
		tcc.ToStart()
		nameTok := tcc.NextToken()
		typename := nameTok.Literal
		typeInfo, _ := iz.getDeclaration(decINTERFACE, &nameTok, DUMMY)
		types := values.MakeAbstractType()
		funcsToAdd := map[values.ValueType][]funcWithName{}
		for i, sigToMatch := range typeInfo.(interfaceInfo).sigs {
			typesMatched := values.MakeAbstractType()
			for key, fnToTry := range iz.common.Functions {
				if key.FunctionName == sigToMatch.name {
					matches := iz.getMatches(sigToMatch, fnToTry, &nameTok)
					typesMatched = typesMatched.Union(matches)
					if !settings.MandatoryImportSet().Contains(fnToTry.Tok.Source) {
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
		iz.p.TypeMap[typename] = types
		typesWithNull := types.Insert(values.NULL)
		iz.p.TypeMap[typename+"?"] = typesWithNull
		iz.AddTypeToVm(values.AbstractTypeInfo{typename, iz.p.NamespacePath, types, settings.MandatoryImportSet().Contains(nameTok.Source)})
		// And we add all the implicated functions to the function table.
		for _, ty := range types.Types {
			for _, fn := range funcsToAdd[ty] {
				conflictingFunction := iz.p.FunctionTable.Add(iz.p, fn.name, fn.pFunc)
				if conflictingFunction != nil && conflictingFunction != fn.pFunc {
					iz.p.Throw("init/overload/b", fn.pFunc.Tok, fn.name, conflictingFunction.Tok.Line)
				}
			}
		}
	}

	if settings.FUNCTION_TO_PEEK != "" {
		println(iz.p.FunctionTable.Describe(iz.p, settings.FUNCTION_TO_PEEK))
	}

	if iz.ErrorsExist() {
		return
	}
	// Now we turn the function table into a different data structure, a "function tree" with its branches labeled
	// with types. Following it tells us which version of an overloaded function to use.
	iz.MakeFunctionTrees()
	if iz.ErrorsExist() {
		return
	}
}

func (iz *initializer) getMatches(sigToMatch fnSigInfo, fnToTry *ast.PrsrFunction, tok *token.Token) values.AbstractType {
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
			if !iz.p.GetAbstractType(sigToMatch.sig.GetVarType(i).(string)).IsSubtypeOf(fnToTry.Sig[i].VarType) ||
				sigToMatch.sig.GetVarType(i).(string) == "bling" && sigToMatch.sig.GetVarName(i) != fnToTry.Sig[i].VarName {
				return values.MakeAbstractType()
			}
		}
	}
	if !foundSelf {
		iz.p.Throw("init/interface/self", tok)
		return values.MakeAbstractType()
	}
	for i := 0; i < sigToMatch.rtnSig.Len(); i++ {
		if sigToMatch.rtnSig[i].VarType == "self" {
			result = result.Intersect(fnToTry.RtnSig[i].VarType)
		} else {
			if !fnToTry.RtnSig[i].VarType.IsSubtypeOf(iz.p.GetAbstractType(sigToMatch.rtnSig[i].VarType)) {
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
func (iz *initializer) initializeExternals() {
	for _, declaration := range iz.ParsedDeclarations[externalDeclaration] {
		name, path := iz.getPartsOfImportOrExternalDeclaration(declaration)
		if path == "" { // Then this will work only if there's already an instance of a service of that name running on the hub.
			service, ok := iz.cp.Vm.HubServices[name]
			if !ok {
				iz.Throw("init/external/exist/a", *declaration.GetToken())
				continue
			}
			iz.addExternalOnSameHub(service.Cp.ScriptFilepath, name)
			continue
		}
		if len(path) >= 5 && path[0:5] == "http:" {
			pos := strings.LastIndex(path, "/")
			if pos == -1 {
				iz.Throw("init/external/path/a", *declaration.GetToken())
				continue
			}
			hostpath := path[0:pos]
			serviceName := path[pos+1:]
			pos = strings.LastIndex(hostpath, "/")
			if pos == -1 {
				iz.Throw("init/external/path/b", *declaration.GetToken())
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
		hubService, ok := iz.cp.Vm.HubServices[name] // If the service already exists, then we just need to check that it uses the same source file.
		if ok {
			if hubService.Cp.ScriptFilepath != path {
				iz.Throw("init/external/exist/b", *declaration.GetToken(), hubService.Cp.ScriptFilepath)
			} else {
				iz.addExternalOnSameHub(path, name)
			}
			continue // Either we've thrown an error or we don't need to do anything.
		}
		// Otherwise we need to start up the service, add it to the hub, and then declare it as external.
		newService := StartService(path, iz.cp.Vm.Database, iz.cp.Vm.HubServices)
		if len(newService.Cp.P.Common.Errors) > 0 {
			newService.Cp.P.Common.IsBroken = true
		}
		iz.cp.Vm.HubServices[name] = newService
		iz.addExternalOnSameHub(path, name)
	}
}

func (iz *initializer) addExternalOnSameHub(path, name string) {
	hubService := iz.cp.Vm.HubServices[name]
	serviceToAdd := externalCallToHubHandler{hubService}
	iz.addAnyExternalService(serviceToAdd, path, name)
}

func (iz *initializer) addHttpService(path, name, username, password string) {
	serviceToAdd := externalHttpCallHandler{path, name, username, password}
	iz.addAnyExternalService(serviceToAdd, path, name)
}

func (iz *initializer) addAnyExternalService(handlerForService externalCallHandler, path, name string) {
	externalServiceOrdinal := uint32(len(iz.cp.Vm.ExternalCallHandlers))
	iz.cp.CallHandlerNumbersByName[name] = externalServiceOrdinal
	iz.cp.Vm.ExternalCallHandlers = append(iz.cp.Vm.ExternalCallHandlers, handlerForService)
	serializedAPI := handlerForService.getAPI()
	sourcecode := SerializedAPIToDeclarations(serializedAPI, externalServiceOrdinal) // This supplies us with a stub that know how to call the external servie.
	newIz := newInitializer()
	newIz.common = iz.common
	iz.initializers[name] = newIz
	newCp := newIz.initializeFromSourcecode(iz.cp.Vm, iz.p.Common, path, sourcecode, name+"."+iz.p.NamespacePath)
	iz.p.NamespaceBranch[name] = &parser.ParserData{newCp.P, path}
	newCp.P.Private = iz.IsPrivate(int(externalDeclaration), int(externalServiceOrdinal))
	iz.cp.Services[name] = &Service{newCp, false}
}

func (iz *initializer) AddType(name, supertype string, typeNo values.ValueType) {
	iz.localConcreteTypes = iz.localConcreteTypes.Add(typeNo)
	iz.p.TypeMap[name] = values.MakeAbstractType(typeNo)
	iz.p.TypeMap[name+"?"] = values.MakeAbstractType(values.NULL, typeNo)
	types := []string{supertype}
	if supertype == "snippet" {
		types = append(types, "struct")
	}
	iz.cp.Vm.AddTypeNumberToSharedAlternateTypes(typeNo, types...)
	types = append(types, "any")
	for _, sT := range types {
		iz.p.Common.Types[sT] = iz.p.Common.Types[sT].Insert(typeNo)
		iz.p.Common.Types[sT+"?"] = iz.p.Common.Types[sT+"?"].Insert(typeNo)
	}
}

// On the one hand, the VM must know the names of the enums and their elements so it can describe them.
// Otoh, the compiler needs to know how to turn enum literals into values.
func (iz *initializer) createEnums() {
	for i, tokens := range iz.TokenizedDeclarations[enumDeclaration] {
		tokens.ToStart()
		tok1 := tokens.NextToken()
		var typeNo values.ValueType
		info, typeExists := iz.getDeclaration(decENUM, &tok1, DUMMY)
		if typeExists {
			typeNo = info.(values.ValueType)
			typeInfo := iz.cp.Vm.concreteTypeInfo[typeNo].(enumType)
			typeInfo.path = iz.p.NamespacePath
			iz.cp.Vm.concreteTypeInfo[typeNo] = typeInfo
		} else {
			typeNo = values.ValueType(len(iz.cp.Vm.concreteTypeInfo))
			iz.setDeclaration(decENUM, &tok1, DUMMY, typeNo)
		}
		iz.AddType(tok1.Literal, "enum", typeNo)
		if typeExists {
			continue
		}

		tokens.NextToken() // Skip over the '='.
		tokens.NextToken() // This says 'enum' or we wouldn't be here.
		elementNameList := []string{}
		for tok := tokens.NextToken(); tok.Type != token.EOF; {
			if tok.Type != token.IDENT {
				iz.Throw("init/enum/ident", tok)
			}
			_, alreadyExists := iz.cp.EnumElements[tok.Literal]
			if alreadyExists { // Enums in the same namespace can't have overlapping elements or we wouldn't know their type.
				iz.Throw("init/enum/element", tok)
			}

			iz.cp.EnumElements[tok.Literal] = iz.cp.Reserve(typeNo, len(elementNameList), &tok)
			elementNameList = append(elementNameList, tok.Literal)
			tok = tokens.NextToken()
			if tok.Type != token.COMMA && tok.Type != token.WEAK_COMMA && tok.Type != token.EOF {
				iz.Throw("init/enum/comma", tok)
			}
			tok = tokens.NextToken()
		}
		iz.cp.Vm.concreteTypeInfo = append(iz.cp.Vm.concreteTypeInfo, enumType{name: tok1.Literal, path: iz.p.NamespacePath, elementNames: elementNameList, private: iz.IsPrivate(int(enumDeclaration), i)})
	}
}

func (iz *initializer) createClones() {
	for i, tokens := range iz.TokenizedDeclarations[cloneDeclaration] {
		private := iz.IsPrivate(int(cloneDeclaration), i)
		tokens.ToStart()
		tok1 := tokens.NextToken()
		name := tok1.Literal
		tokens.NextToken() // Skip over the '='.
		tokens.NextToken() // This says 'clone' or we wouldn't be here.
		typeToken := tokens.NextToken()
		typeToClone := typeToken.Literal
		parentTypeNo, ok := parser.ClonableTypes[typeToClone]
		if !ok {
			iz.Throw("init/clone/type", typeToken)
			return
		}
		abType := typeToClone + "like"
		var typeNo values.ValueType
		info, typeExists := iz.getDeclaration(decCLONE, &tok1, DUMMY)
		if typeExists {
			typeNo = info.(values.ValueType)
			typeInfo := iz.cp.Vm.concreteTypeInfo[typeNo].(cloneType)
			typeInfo.path = iz.p.NamespacePath
			iz.cp.Vm.concreteTypeInfo[typeNo] = typeInfo
		} else {
			typeNo = values.ValueType(len(iz.cp.Vm.concreteTypeInfo))
			iz.setDeclaration(decCLONE, &tok1, DUMMY, typeNo)
			iz.cp.Vm.concreteTypeInfo = append(iz.cp.Vm.concreteTypeInfo, cloneType{name: name, path: iz.p.NamespacePath, parent: parentTypeNo, private: iz.IsPrivate(int(cloneDeclaration), i)})
			if parentTypeNo == values.LIST || parentTypeNo == values.STRING || parentTypeNo == values.SET || parentTypeNo == values.MAP {
				iz.cp.Vm.IsRangeable = iz.cp.Vm.IsRangeable.Union(altType(typeNo))
			}
		}
		// We make the conversion fuction.
		iz.AddType(name, abType, typeNo)
		iz.p.AllFunctionIdents.Add(name)
		iz.p.Functions.Add(name)
		sig := ast.StringSig{ast.NameTypenamePair{"x", typeToClone}}
		rtnSig := ast.StringSig{ast.NameTypenamePair{"*dummy*", name}}
		fn := &ast.PrsrFunction{Sig: iz.p.MakeAbstractSigFromStringSig(sig), NameSig: sig, NameRets: rtnSig, RtnSig: iz.p.MakeAbstractSigFromStringSig(rtnSig), Body: &ast.BuiltInExpression{Name: name}, Number: DUMMY, Compiler: iz.cp, Tok: &tok1}
		iz.p.FunctionTable.Add(iz.p, name, fn)
		iz.fnIndex[fnSource{cloneDeclaration, i}] = fn

		// We get the requested builtins.
		var opList []string
		usingOrEof := tokens.NextToken()
		if usingOrEof.Type != token.EOF {
			if usingOrEof.Literal != "using" {
				iz.Throw("init/clone/using", usingOrEof)
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
					iz.Throw("init/clone/comma", usingOrEof)
					break
				}
			}
		}
		if iz.ErrorsExist() {
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
					iz.makeCloneFunction("+", sig, "add_floats", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "-":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"-", "bling"}, ast.NameTypenamePair{"y", name}}
					iz.makeCloneFunction("-", sig, "subtract_floats", altType(typeNo), rtnSig, private, INFIX, &tok1)
					sig = ast.StringSig{ast.NameTypenamePair{"x", name}}
					iz.makeCloneFunction("-", sig, "negate_float", altType(typeNo), rtnSig, private, PREFIX, &tok1)
				case "*":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"*", "bling"}, ast.NameTypenamePair{"y", name}}
					iz.makeCloneFunction("*", sig, "multiply_floats", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "/":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"/", "bling"}, ast.NameTypenamePair{"y", name}}
					iz.makeCloneFunction("/", sig, "divide_floats", altType(typeNo), rtnSig, private, INFIX, &tok1)
				default:
					iz.Throw("init/request/float", usingOrEof, op)
				}
			case values.INT:
				switch op {
				case "+":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"+", "bling"}, ast.NameTypenamePair{"y", name}}
					iz.makeCloneFunction("+", sig, "add_integers", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "-":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"-", "bling"}, ast.NameTypenamePair{"y", name}}
					iz.makeCloneFunction("-", sig, "subtract_integers", altType(typeNo), rtnSig, private, INFIX, &tok1)
					sig = ast.StringSig{ast.NameTypenamePair{"x", name}}
					iz.makeCloneFunction("-", sig, "negate_integer", altType(typeNo), rtnSig, private, PREFIX, &tok1)
				case "*":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"*", "bling"}, ast.NameTypenamePair{"y", name}}
					iz.makeCloneFunction("*", sig, "multiply_integers", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "/":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"/", "bling"}, ast.NameTypenamePair{"y", name}}
					iz.makeCloneFunction("/", sig, "divide_integers", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "%":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"%", "bling"}, ast.NameTypenamePair{"y", name}}
					iz.makeCloneFunction("%", sig, "modulo_integers", altType(typeNo), rtnSig, private, INFIX, &tok1)
				default:
					iz.p.Throw("init/request/int", &usingOrEof, op)
				}
			case values.LIST:
				switch op {
				case "+":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"+", "bling"}, ast.NameTypenamePair{"y", name}}
					iz.makeCloneFunction("+", sig, "add_lists", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "with":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"with", "bling"}, ast.NameTypenamePair{"y", "...pair"}}
					iz.makeCloneFunction("with", sig, "list_with", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "?>":
					cloneData := iz.cp.Vm.concreteTypeInfo[typeNo].(cloneType)
					cloneData.isFilterable = true
					iz.cp.Vm.concreteTypeInfo[typeNo] = cloneData
				case ">>":
					cloneData := iz.cp.Vm.concreteTypeInfo[typeNo].(cloneType)
					cloneData.isMappable = true
					iz.cp.Vm.concreteTypeInfo[typeNo] = cloneData
				case "slice":
					cloneData := iz.cp.Vm.concreteTypeInfo[typeNo].(cloneType)
					cloneData.isSliceable = true
					iz.cp.Vm.concreteTypeInfo[typeNo] = cloneData
				default:
					iz.Throw("init/request/list", usingOrEof, op)
				}
			case values.MAP:
				switch op {
				case "with":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"with", "bling"}, ast.NameTypenamePair{"y", "...pair"}}
					iz.makeCloneFunction("with", sig, "map_with", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "without":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"without", "bling"}, ast.NameTypenamePair{"y", "...any?"}}
					iz.makeCloneFunction("without", sig, "map_without", altType(typeNo), rtnSig, private, INFIX, &tok1)
				default:
					iz.Throw("init/request/map", usingOrEof, op)
				}
			case values.PAIR:
				iz.Throw("init/request/pair", usingOrEof, op)
			case values.SET:
				switch op {
				case "+":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"+", "bling"}, ast.NameTypenamePair{"y", name}}
					iz.makeCloneFunction("+", sig, "add_sets", altType(typeNo), rtnSig, private, INFIX, &tok1)
				default:
					iz.Throw("init/request/set", usingOrEof, op)
				}
			case values.STRING:
				switch op {
				case "+":
					sig := ast.StringSig{ast.NameTypenamePair{"x", name}, ast.NameTypenamePair{"+", "bling"}, ast.NameTypenamePair{"y", name}}
					iz.makeCloneFunction("+", sig, "add_strings", altType(typeNo), rtnSig, private, INFIX, &tok1)
				case "slice":
					cloneData := iz.cp.Vm.concreteTypeInfo[typeNo].(cloneType)
					cloneData.isSliceable = true
					iz.cp.Vm.concreteTypeInfo[typeNo] = cloneData
				default:
					iz.Throw("init/request/string", usingOrEof, op)
				}
			}
		}
	}
	// For convenience, we give the compiler a map between types and the group of clones they belong to (no entry in the map if they're uncloneable).
	for typename := range parser.ClonableTypes {
		abType := typename + "like"
		cloneGroup := iz.cp.Vm.sharedTypenameToTypeList[abType]
		for _, cloneTypeNo := range cloneGroup {
			iz.cp.typeToCloneGroup[values.ValueType(cloneTypeNo.(simpleType))] = cloneGroup
		}
	}
}

const (
	PREFIX uint32 = iota
	INFIX
	SUFFIX
	UNFIX
)

func (iz *initializer) makeCloneFunction(fnName string, sig ast.StringSig, builtinTag string, rtnTypes AlternateType, rtnSig ast.StringSig, isPrivate bool, pos uint32, tok *token.Token) {
	fn := &ast.PrsrFunction{Sig: iz.p.MakeAbstractSigFromStringSig(sig), Tok: tok, NameSig: sig, NameRets: rtnSig, RtnSig: iz.p.MakeAbstractSigFromStringSig(rtnSig), Body: &ast.BuiltInExpression{*tok, builtinTag}, Compiler: iz.cp, Number: iz.addToBuiltins(sig, builtinTag, rtnTypes, isPrivate, tok)}
	iz.common.Functions[FuncSource{tok.Source, tok.Line, fnName, pos}] = fn
	if fnName == settings.FUNCTION_TO_PEEK {
		println("Making clone with sig", sig.String())
	}
	conflictingFunction := iz.p.FunctionTable.Add(iz.p, fnName, fn)
	if conflictingFunction != nil && conflictingFunction != fn {
		iz.p.Throw("init/overload/c", tok, fnName, conflictingFunction.Tok.Line)
	}
}

// We create the struct types and their field labels but we don't define the field types because we haven't defined all the types even lexically yet, let alone what they are.
func (iz *initializer) createStructNamesAndLabels() {
	iz.cp.structDeclarationNumberToTypeNumber = make(map[int]values.ValueType)
	for i, node := range iz.ParsedDeclarations[structDeclaration] {
		lhs := node.(*ast.AssignmentExpression).Left
		name := lhs.GetToken().Literal
		typeNo := values.ValueType(len(iz.cp.Vm.concreteTypeInfo))
		typeInfo, typeExists := iz.getDeclaration(decSTRUCT, node.GetToken(), DUMMY)
		if typeExists { // We see if it's already been declared.
			typeNo = typeInfo.(structInfo).structNumber
			typeInfo := iz.cp.Vm.concreteTypeInfo[typeNo].(structType)
			typeInfo.path = iz.p.NamespacePath
			iz.cp.Vm.concreteTypeInfo[typeNo] = typeInfo
		} else {
			iz.setDeclaration(decSTRUCT, node.GetToken(), DUMMY, structInfo{typeNo, iz.IsPrivate(int(structDeclaration), i)})
		}
		iz.AddType(name, "struct", typeNo)
		if name == "Error" {
			iz.cp.Vm.typeNumberOfUnwrappedError = typeNo // The vm needs to know this so it can convert an 'error' into an 'Error'.
		}
		// The parser needs to know about it too.
		iz.p.Functions.Add(name)
		iz.p.AllFunctionIdents.Add(name)
		sig := node.(*ast.AssignmentExpression).Right.(*ast.StructExpression).Sig
		fn := &ast.PrsrFunction{Sig: iz.p.MakeAbstractSigFromStringSig(sig), NameSig: sig, Body: &ast.BuiltInExpression{Name: name}, Number: DUMMY, Compiler: iz.cp, Tok: node.GetToken()}
		iz.p.FunctionTable.Add(iz.p, name, fn) // TODO --- give them their own ast type?
		iz.fnIndex[fnSource{structDeclaration, i}] = fn
		// We make the labels exist, unless they already do.
		if typeExists { // Then the vm knows about it but we have to tell this compiler about it too.
			iz.cp.structDeclarationNumberToTypeNumber[i] = typeInfo.(structInfo).structNumber
		} else { // Else we need to add the labels to the vm and cp.
			labelsForStruct := make([]int, 0, len(sig))
			for j, labelNameAndType := range sig {
				labelName := labelNameAndType.VarName
				labelLocation, alreadyExists := iz.cp.Vm.FieldLabelsInMem[labelName]
				if alreadyExists { // Structs can of course have overlapping fields but we don't want to declare them twice.
					labelsForStruct = append(labelsForStruct, iz.cp.Vm.Mem[labelLocation].V.(int))
					iz.setDeclaration(decLABEL, node.GetToken(), j, labelInfo{labelLocation, true}) // 'true' because we can't tell if it's private or not until we've defined all the structs.
				} else {
					iz.cp.Vm.FieldLabelsInMem[labelName] = iz.cp.Reserve(values.LABEL, len(iz.cp.Vm.Labels), node.GetToken())
					iz.setDeclaration(decLABEL, node.GetToken(), j, labelInfo{iz.cp.That(), true})
					labelsForStruct = append(labelsForStruct, len(iz.cp.Vm.Labels))
					iz.cp.Vm.Labels = append(iz.cp.Vm.Labels, labelName)
					iz.cp.Vm.LabelIsPrivate = append(iz.cp.Vm.LabelIsPrivate, true)
				}
			}
			iz.cp.structDeclarationNumberToTypeNumber[i] = values.ValueType(len(iz.cp.Vm.concreteTypeInfo))
			stT := structType{name: name, path: iz.p.NamespacePath, labelNumbers: labelsForStruct, private: iz.IsPrivate(int(structDeclaration), i)}
			stT = stT.addLabels(labelsForStruct)
			iz.cp.Vm.concreteTypeInfo = append(iz.cp.Vm.concreteTypeInfo, stT)
		}
	}

	for i := range iz.ParsedDeclarations[structDeclaration] {
		if iz.IsPrivate(int(structDeclaration), i) {
			continue
		}
		tok := iz.ParsedDeclarations[structDeclaration][i].GetToken()
		sI, _ := iz.getDeclaration(decSTRUCT, tok, DUMMY)
		sT := iz.cp.Vm.concreteTypeInfo[sI.(structInfo).structNumber]
		for i := range sT.(structType).labelNumbers {
			dec, _ := iz.getDeclaration(decLABEL, tok, i)
			decLabel := dec.(labelInfo)
			decLabel.private = false
			iz.setDeclaration(decLABEL, tok, i, decLabel)
			iz.cp.Vm.LabelIsPrivate[iz.cp.Vm.Mem[decLabel.loc].V.(int)] = false
		}
	}
}

func (iz *initializer) createAbstractTypes() {
	for _, tcc := range iz.TokenizedDeclarations[abstractDeclaration] {
		tcc.ToStart()
		nameTok := tcc.NextToken()
		newTypename := nameTok.Literal
		tcc.NextToken() // The equals sign.
		tcc.NextToken() // The 'abstract' identifier.
		iz.p.TypeMap[newTypename] = values.MakeAbstractType()
		for {
			typeTok := tcc.NextToken()
			divTok := tcc.NextToken()
			if typeTok.Type != token.IDENT {
				iz.Throw("init/type/form/b", typeTok)
				break
			}
			if divTok.Type != token.EOF && !(divTok.Type == token.IDENT && divTok.Literal == "/") {
				iz.Throw("init/type/form/c", typeTok)
				break
			}
			tname := typeTok.Literal
			abTypeToAdd, ok := iz.p.TypeMap[tname]
			if !ok {
				iz.Throw("init/type/known", typeTok)
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

func (iz *initializer) createInterfaceTypes() {
	for _, tcc := range iz.TokenizedDeclarations[interfaceDeclaration] {
		tcc.ToStart()
		nameTok := tcc.NextToken()
		newTypename := nameTok.Literal
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

func (iz *initializer) addFieldsToStructs() {
	for i, node := range iz.ParsedDeclarations[structDeclaration] {
		structNumber := iz.cp.structDeclarationNumberToTypeNumber[i]
		structInfo := iz.cp.Vm.concreteTypeInfo[structNumber].(structType)
		sig := node.(*ast.AssignmentExpression).Right.(*ast.StructExpression).Sig
		typesForStruct := make([]AlternateType, 0, len(sig))
		typesForStructForVm := make([]values.AbstractType, 0, len(sig))
		for _, labelNameAndType := range sig {
			typeName := labelNameAndType.VarType
			abType := iz.p.GetAbstractType(typeName)
			typesForStructForVm = append(typesForStructForVm, abType)
			typesForStruct = append(typesForStruct, AbstractTypeToAlternateType(abType))
		}
		structInfo.alternateStructFields = typesForStruct // TODO --- even assuming we want this data duplicated, the AlternateType can't possibly be needed  at runtime and presumably belongs in a common compiler bindle.
		structInfo.abstractStructFields = typesForStructForVm
		iz.cp.Vm.concreteTypeInfo[structNumber] = structInfo
	}
}

func (iz *initializer) createSnippetTypesPart2() {
	abTypes := []values.AbstractType{{[]values.ValueType{values.STRING}, DUMMY}, {[]values.ValueType{values.MAP}, DUMMY}}
	altTypes := []AlternateType{altType(values.STRING), altType(values.MAP)}
	for i, name := range iz.Snippets {
		sig := ast.StringSig{ast.NameTypenamePair{VarName: "text", VarType: "string"}, ast.NameTypenamePair{VarName: "data", VarType: "list"}}
		typeNo := values.ValueType(len(iz.cp.Vm.concreteTypeInfo))
		iz.TokenizedDeclarations[snippetDeclaration][i].ToStart()
		decTok := iz.TokenizedDeclarations[snippetDeclaration][i].NextToken()
		typeInfo, typeExists := iz.getDeclaration(decSTRUCT, &decTok, DUMMY)
		if typeExists { // We see if it's already been declared.
			typeNo = typeInfo.(structInfo).structNumber
			typeInfo := iz.cp.Vm.concreteTypeInfo[typeNo].(structType)
			typeInfo.path = iz.p.NamespacePath
			iz.cp.Vm.concreteTypeInfo[typeNo] = typeInfo
		} else {
			iz.setDeclaration(decSTRUCT, &decTok, DUMMY, structInfo{typeNo, iz.IsPrivate(int(snippetDeclaration), i)})
			iz.cp.Vm.concreteTypeInfo = append(iz.cp.Vm.concreteTypeInfo, structType{name: name, path: iz.p.NamespacePath, snippet: true, private: iz.IsPrivate(int(snippetDeclaration), i), abstractStructFields: abTypes, alternateStructFields: altTypes})
			iz.addStructLabelsToVm(name, typeNo, sig, &decTok)
			iz.cp.Vm.codeGeneratingTypes.Add(typeNo)
		}
		iz.AddType(name, "snippet", typeNo)
		// The parser needs to know about it too.
		iz.p.Functions.Add(name)
		fn := &ast.PrsrFunction{Sig: iz.p.MakeAbstractSigFromStringSig(sig), NameSig: sig, Body: &ast.BuiltInExpression{Name: name, Token: decTok}, Tok: &decTok}
		iz.p.FunctionTable.Add(iz.p, name, fn)
		iz.fnIndex[fnSource{snippetDeclaration, i}] = fn
	}
}

func (iz *initializer) checkTypesForConsistency() {
	for typeNumber := int(values.FIRST_DEFINED_TYPE); typeNumber < len(iz.cp.Vm.concreteTypeInfo); typeNumber++ {
		if !iz.cp.Vm.concreteTypeInfo[typeNumber].isStruct() {
			continue
		}
		if !iz.cp.Vm.concreteTypeInfo[typeNumber].isPrivate() {
			for _, ty := range iz.cp.Vm.concreteTypeInfo[typeNumber].(structType).abstractStructFields {
				if iz.cp.isPrivate(ty) {
					iz.Throw("init/private/struct", token.Token{}, iz.cp.Vm.concreteTypeInfo[typeNumber], iz.cp.Vm.DescribeAbstractType(ty, LITERAL))
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
		abType := iz.p.GetAbstractType(name)
		for _, w := range abType.Types {
			if iz.cp.Vm.concreteTypeInfo[w].isPrivate() {
				iz.Throw("init/private/abstract", tok, name)
			}
		}

	}
}

func (iz *initializer) addStructLabelsToVm(name string, typeNo values.ValueType, sig ast.StringSig, tok *token.Token) { // TODO --- seems like we're only using this for snippets and not regular structs?
	labelsForStruct := make([]int, 0, len(sig))
	for _, labelNameAndType := range sig {
		labelName := labelNameAndType.VarName
		labelLocation, alreadyExists := iz.cp.Vm.FieldLabelsInMem[labelName]
		if alreadyExists { // Structs can of course have overlapping fields but we don't want to declare them twice.
			labelsForStruct = append(labelsForStruct, iz.cp.Vm.Mem[labelLocation].V.(int))
		} else {
			iz.cp.Vm.FieldLabelsInMem[labelName] = iz.cp.Reserve(values.LABEL, len(iz.cp.Vm.Labels), tok)
			labelsForStruct = append(labelsForStruct, len(iz.cp.Vm.Labels))
			iz.cp.Vm.Labels = append(iz.cp.Vm.Labels, labelName)
			iz.cp.Vm.LabelIsPrivate = append(iz.cp.Vm.LabelIsPrivate, true)
		}
	}
	typeInfo := iz.cp.Vm.concreteTypeInfo[typeNo].(structType)
	typeInfo.labelNumbers = labelsForStruct
	typeInfo = typeInfo.addLabels(labelsForStruct)
	iz.cp.Vm.concreteTypeInfo[typeNo] = typeInfo
}

func (iz *initializer) compileConstructors() {
	// Struct declarations.
	for i, node := range iz.ParsedDeclarations[structDeclaration] {
		name := node.(*ast.AssignmentExpression).Left.GetToken().Literal // We know this and the next line are safe because we already checked in createStructs
		typeNo := iz.cp.ConcreteTypeNow(name)
		sig := node.(*ast.AssignmentExpression).Right.(*ast.StructExpression).Sig
		iz.fnIndex[fnSource{structDeclaration, i}].Number = iz.addToBuiltins(sig, name, altType(typeNo), iz.IsPrivate(int(structDeclaration), i), node.GetToken())
		iz.fnIndex[fnSource{structDeclaration, i}].Compiler = iz.cp
	}
	// Snippets. TODO --- should this even exist? It seems like all it adds is that you could make ill-formed snippets if you chose.
	sig := ast.StringSig{ast.NameTypenamePair{VarName: "text", VarType: "string"}, ast.NameTypenamePair{VarName: "data", VarType: "list"}}
	for i, name := range iz.Snippets {
		typeNo := iz.cp.ConcreteTypeNow(name)
		iz.fnIndex[fnSource{snippetDeclaration, i}].Number = iz.addToBuiltins(sig, name, altType(typeNo), iz.IsPrivate(int(snippetDeclaration), i), iz.ParsedDeclarations[snippetDeclaration][i].GetToken())
		iz.fnIndex[fnSource{snippetDeclaration, i}].Compiler = iz.cp
	}
	// Clones
	for i, dec := range iz.TokenizedDeclarations[cloneDeclaration] {
		dec.ToStart()
		nameTok := dec.NextToken()
		name := nameTok.Literal
		typeNo := iz.cp.ConcreteTypeNow(name)
		sig := ast.StringSig{ast.NameTypenamePair{VarName: "x", VarType: iz.cp.Vm.concreteTypeInfo[iz.cp.Vm.concreteTypeInfo[typeNo].(cloneType).parent].getName(DEFAULT)}}
		iz.fnIndex[fnSource{cloneDeclaration, i}].Number = iz.addToBuiltins(sig, name, altType(typeNo), iz.IsPrivate(int(cloneDeclaration), i), &nameTok)
		iz.fnIndex[fnSource{cloneDeclaration, i}].Compiler = iz.cp
	}
}

func (iz *initializer) addToBuiltins(sig ast.StringSig, builtinTag string, returnTypes AlternateType, private bool, tok *token.Token) uint32 {
	cpF := &CpFunc{RtnTypes: returnTypes, Builtin: builtinTag}
	fnenv := NewEnvironment() // Note that we don't use this for anything, we just need some environment to pass to addVariables.
	cpF.LoReg = iz.cp.MemTop()
	for _, pair := range sig {
		iz.cp.AddVariable(fnenv, pair.VarName, FUNCTION_ARGUMENT, iz.cp.getAlternateTypeFromTypeName(pair.VarType), tok)
	}
	cpF.HiReg = iz.cp.MemTop()
	cpF.Private = private
	iz.cp.Fns = append(iz.cp.Fns, cpF)
	return uint32(len(iz.cp.Fns) - 1)
}

var nativeAbstractTypes = []string{"any", "struct", "snippet"}

// The Vm doesn't *use* abstract types, but they are what values of type TYPE contain, and so it needs to be able to describe them.
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
		iz.AddTypeToVm(values.AbstractTypeInfo{typeName, iz.p.NamespacePath, iz.p.GetAbstractType(typeName), false}) // TODO --- this only happens to be false because you didn't define any.
	}
}

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

func altType(t ...values.ValueType) AlternateType {
	return AltType(t...)
}

func (i initializer) IsPrivate(x, y int) bool {
	return i.TokenizedDeclarations[x][y].Private
}

func MakeFilepath(scriptFilepath string) string {
	doctoredFilepath := strings.Clone(scriptFilepath)
	if len(scriptFilepath) >= 4 && scriptFilepath[0:4] == "hub/" {
		doctoredFilepath = filepath.Join(settings.PipefishHomeDirectory, filepath.FromSlash(scriptFilepath))
	}
	if len(scriptFilepath) >= 4 && scriptFilepath[0:4] == "rsc/" {
		doctoredFilepath = filepath.Join(settings.PipefishHomeDirectory, "source", "service", filepath.FromSlash(scriptFilepath))
	}
	if settings.StandardLibraries.Contains(scriptFilepath) {
		doctoredFilepath = settings.PipefishHomeDirectory + "lib/" + scriptFilepath
	}
	if len(scriptFilepath) >= 3 && scriptFilepath[len(scriptFilepath)-3:] != ".pf" && len(scriptFilepath) >= 4 && scriptFilepath[len(scriptFilepath)-4:] != ".hub" {
		doctoredFilepath = doctoredFilepath + ".pf"
	}
	return doctoredFilepath
}

// Like everything else, the initializer sends its errors to the common parser bindle via the parser.
func (iz *initializer) Throw(errorID string, tok token.Token, args ...any) {
	iz.p.Throw(errorID, &tok, args...)
}
