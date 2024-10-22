// The uberparser helps the vmmaker by doing a lot of the work involved in setting up the parser.
// It is largely separate from the main vmmaker for historical reason but it also adds some clarity
// to group together aspects of the initialization process which don't need to touch the vm.

// In operation, it takes the tokens from the relexer and splits it up into code types according to the headword,
// which is discarded. It breaks these up into function declarations, variable intializations, etc.

// As it does so it checks out the signatures of the functions and commands and decides
// what "grammatical" role the words in the function signature play, and deposits
// lists of these into a Parser object: the Prefix, Forefix, Midfix, Suffix, Endfix etc classes.

// We then have a tokenized program broken into parts, and a parser primed to
// parse tokens into ASTs. We apply one to the other and produce ASTs from our
// tokenized code.

package service

import (
	"embed"
	"os"
	"strings"

	"pipefish/source/ast"
	"pipefish/source/lexer"
	"pipefish/source/parser"
	"pipefish/source/report"
	"pipefish/source/settings"
	"pipefish/source/text"
	"pipefish/source/token"
	"pipefish/source/values"
)

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
	importDeclaration   declarationType = iota
	externalDeclaration                 //
	enumDeclaration                     //
	structDeclaration                   //
	snippetDeclaration                  //
	abstractDeclaration                 //
	interfaceDeclaration                //
	cloneDeclaration                    //
	constantDeclaration                 //
	variableDeclaration                 //
	functionDeclaration                 //
	commandDeclaration                  //
	golangDeclaration                   // Pure golang in a block; the Charm functions with golang bodies don't go here but under function or command as they were declared.

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

type Initializer struct {
	rl      lexer.Relexer
	Parser  *parser.Parser
	Sources map[string][]string
}

func NewInitializer(common *parser.CommonParserBindle, source, sourceCode, dir string, namespacePath string) *Initializer {
	uP := &Initializer{
		rl:      *lexer.NewRelexer(source, sourceCode),
		Parser:  parser.New(common, dir, namespacePath),
		Sources: make(map[string][]string),
	}
	uP.Sources[source] = strings.Split(sourceCode, "\n")
	return uP
}

// Do not under any cicumstances remove the following comment.
//
//go:embed rsc/pipefish/*
var folder embed.FS

func (init *Initializer) AddToNameSpace(thingsToImport []string) {
	for _, fname := range thingsToImport {
		var libDat []byte
		var err error
		if len(fname) >= 13 && fname[:13] == "rsc/pipefish/" {
			libDat, err = folder.ReadFile(fname)
		} else {
			libDat, err = os.ReadFile(MakeFilepath(fname, init.Parser.Directory))
		}
		if err != nil {
			init.Throw("init/import/found", token.Token{})
		}
		stdImp := strings.TrimRight(string(libDat), "\n") + "\n"
		init.SetRelexer(*lexer.NewRelexer(fname, stdImp))
		init.MakeParserAndTokenizedProgram() // This is cumulative, it throws them all into the parser together.
		init.Sources[fname] = strings.Split(stdImp, "\n")
	}
}

func (uP *Initializer) addTokenizedDeclaration(decType declarationType, line *token.TokenizedCodeChunk, private bool) {
	line.Private = private
	uP.Parser.TokenizedDeclarations[decType] = append(uP.Parser.TokenizedDeclarations[decType], line)
}


var typeMap = map[string]declarationType{"struct": structDeclaration, "enum": enumDeclaration, "snippet": snippetDeclaration, 
                                         "abstract": abstractDeclaration, "clone": cloneDeclaration, "interface": interfaceDeclaration}

func (uP *Initializer) MakeParserAndTokenizedProgram() {
	currentSection := UndefinedSection
	beginCount := 0
	indentCount := 0
	lastTokenWasColon := false
	typeDefined := declarationType(DUMMY)
	isPrivate := false
	var (
		tok token.Token
	)

	tok = uP.rl.NextToken() // note that we've already removed leading newlines.
	if settings.SHOW_RELEXER && !(settings.IGNORE_BOILERPLATE && settings.ThingsToIgnore.Contains(tok.Source)) {
		println(tok.Type, tok.Literal)
	}

	if tok.Type == token.EOF { // An empty file should still initiate a service, but one with no data.
		return
	}
	if !token.TokenTypeIsHeadword(tok.Type) {
		uP.Throw("init/head", tok)
		return
	}

	currentSection = tokenTypeToSection[tok.Type]
	colonMeansFunctionOrCommand := (currentSection == CmdSection || currentSection == DefSection)

	line := token.NewCodeChunk()

	for tok = uP.rl.NextToken(); tok.Type != token.EOF; tok = uP.rl.NextToken() {
		if settings.SHOW_RELEXER && !(settings.IGNORE_BOILERPLATE && settings.ThingsToIgnore.Contains(tok.Source)) {
			println(tok.Type, tok.Literal)
		}
		if token.TokenTypeIsHeadword(tok.Type) {
			if tok.Literal == "import" {
				uP.Throw("init/import/first", tok)
			}
			currentSection = tokenTypeToSection[tok.Type]
			isPrivate = false
			lastTokenWasColon = false
			colonMeansFunctionOrCommand = (currentSection == CmdSection || currentSection == DefSection)
			continue
		}
		if tok.Type == token.PRIVATE {
			if isPrivate {
				uP.Throw("init/private", tok)
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
				uP.Throw("init/close", tok)
				beginCount = 0 // Prevents error storm.
				typeDefined = declarationType(DUMMY)
				colonMeansFunctionOrCommand = (currentSection == CmdSection || currentSection == DefSection)
				continue
			}
			switch currentSection {
			case ImportSection:
				uP.addTokenizedDeclaration(importDeclaration, line, isPrivate)
			case LanguagesSection:
				uP.addTokenizedDeclaration(snippetDeclaration, line, isPrivate)
			case ExternalSection:
				uP.addTokenizedDeclaration(externalDeclaration, line, isPrivate)
			case CmdSection:
				line.ToStart()
				if line.Length() == 1 && line.NextToken().Type == token.GOCODE {
					uP.addTokenizedDeclaration(golangDeclaration, line, isPrivate)
				} else {
					uP.addTokenizedDeclaration(commandDeclaration, line, isPrivate)
				}
			case DefSection:
				line.ToStart()
				if line.Length() == 1 && line.NextToken().Type == token.GOCODE {
					uP.addTokenizedDeclaration(golangDeclaration, line, isPrivate)
				} else {
					uP.addTokenizedDeclaration(functionDeclaration, line, isPrivate)
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
					uP.addTokenizedDeclaration(variableDeclaration, line, isPrivate)
				} else {
					uP.addTokenizedDeclaration(constantDeclaration, line, isPrivate)
				}
			case TypesSection:
				if typeDefined != declarationType(DUMMY) {
					uP.addTokenizedDeclaration(typeDefined, line, isPrivate)
				} else {
					uP.Throw("init/type/form", tok)
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
			uP.addWordsToParser(line)
		}
		line.Append(tok)
	}

	uP.Parser.Errors = report.MergeErrors(uP.rl.GetErrors(), uP.Parser.Errors)
}

func (uP *Initializer) ParseImportsAndExternals() {
	for kindOfDeclarationToParse := importDeclaration; kindOfDeclarationToParse <= externalDeclaration; kindOfDeclarationToParse++ {
		uP.Parser.ParsedDeclarations[kindOfDeclarationToParse] = parser.ParsedCodeChunks{}
		for chunk := 0; chunk < len(uP.Parser.TokenizedDeclarations[kindOfDeclarationToParse]); chunk++ {
			uP.Parser.TokenizedCode = uP.Parser.TokenizedDeclarations[kindOfDeclarationToParse][chunk]
			uP.Parser.TokenizedDeclarations[kindOfDeclarationToParse][chunk].ToStart()
			uP.Parser.ParsedDeclarations[kindOfDeclarationToParse] = append(uP.Parser.ParsedDeclarations[kindOfDeclarationToParse], uP.Parser.ParseTokenizedChunk())
		}
	}
}

func (uP *Initializer) getPartsOfImportOrExternalDeclaration(imp ast.Node) (string, string) {
	namespace := ""
	scriptFilepath := ""
	switch imp := (imp).(type) {
	case *ast.StringLiteral:
		scriptFilepath = imp.Value
		if settings.StandardLibraries.Contains(scriptFilepath) {
			scriptFilepath = uP.Parser.Directory + "lib/" + scriptFilepath + ".pf"
		}
		namespace = text.ExtractFileName(scriptFilepath)
		return namespace, scriptFilepath
	case *ast.Identifier:
		namespace = imp.Value
		return namespace, scriptFilepath
	case *ast.InfixExpression:
		if imp.GetToken().Literal != "::" {
			uP.Throw("init/import/infix", imp.Token)
		}
		lhs := imp.Args[0]
		rhs := imp.Args[2]
		switch rhs := rhs.(type) {
		case *ast.StringLiteral:
			scriptFilepath = rhs.Value
			if settings.StandardLibraries.Contains(scriptFilepath) {
				namespace = scriptFilepath
				scriptFilepath = uP.Parser.Directory + "lib/" + scriptFilepath + ".pf"
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
				uP.Throw("init/import/ident", *lhs.GetToken())
			}
		default:
			uP.Throw("init/import/string", *lhs.GetToken())
		}
	}
	uP.Throw("init/import/weird", *imp.GetToken())
	return "", ""
}

// We need to declare all the types as suffixes for all the user-defined types
func (uP *Initializer) addTypesToParser() { /// TODO --- some of this seems to replicate boilerplate in the parsing functions, so you should be able to remove the latter.
	for kindOfType := enumDeclaration; kindOfType <= cloneDeclaration; kindOfType++ {
		for chunk := 0; chunk < len(uP.Parser.TokenizedDeclarations[kindOfType]); chunk++ {
			// Each of them should begin with the name of the type being declared, and then followed by an =..
			uP.Parser.TokenizedDeclarations[kindOfType][chunk].ToStart()
			tok1 := uP.Parser.TokenizedDeclarations[kindOfType][chunk].NextToken()
			tok2 := uP.Parser.TokenizedDeclarations[kindOfType][chunk].NextToken()
			if tok1.Type != token.IDENT || tok2.Type != token.ASSIGN {
				uP.Throw("init/type/form/a", tok1)
				continue
			}
			name := tok1.Literal
			if uP.Parser.Suffixes.Contains(name) {
				uP.Throw("init/type/exists", tok1)
				continue
			}
			uP.Parser.Suffixes.Add(name)
			uP.Parser.Suffixes.Add(name + "?")
		}
	}
}

func (uP *Initializer) addConstructorsToParserAndParseStructDeclarations() {
	// First we need to make the struct types into types so the parser parses them properly.
	for chunk := 0; chunk < len(uP.Parser.TokenizedDeclarations[structDeclaration]); chunk++ {
		uP.Parser.TokenizedDeclarations[structDeclaration][chunk].ToStart()
		// Note that the first two tokens should already have been validated by the createTypeSuffixes method as IDENT and ASSIGN respectively.
		tok1 := uP.Parser.TokenizedDeclarations[structDeclaration][chunk].NextToken()
		uP.Parser.TokenizedDeclarations[structDeclaration][chunk].NextToken() // We skip the = sign.
		uP.Parser.AllFunctionIdents.Add(tok1.Literal)
		uP.Parser.Functions.Add(tok1.Literal)
		uP.Parser.Structs.Add(tok1.Literal)
	}
	// Now we can parse them.
	for chunk := 0; chunk < len(uP.Parser.TokenizedDeclarations[structDeclaration]); chunk++ {
		uP.Parser.TokenizedCode = uP.Parser.TokenizedDeclarations[structDeclaration][chunk]
		uP.Parser.TokenizedDeclarations[structDeclaration][chunk].ToStart()
		uP.Parser.ParsedDeclarations[structDeclaration] = append(uP.Parser.ParsedDeclarations[structDeclaration], uP.Parser.ParseTokenizedChunk())
	}
}

func (uP *Initializer) createSnippetsPart1() {
	for _, v := range uP.Parser.TokenizedDeclarations[snippetDeclaration] {
		v.ToStart()
		// Note that the first tokens should already have been validated by the createTypeSuffixes method as IDENT.
		tok1 := v.NextToken()
		name := tok1.Literal
		uP.Parser.Snippets = append(uP.Parser.Snippets, name)
		uP.Parser.AllFunctionIdents.Add(name)
		uP.Parser.Functions.Add(name)
		uP.Parser.Structs.Add(name)
	}
}

func (uP *Initializer) ParseEverything() {
	for declarations := snippetDeclaration; declarations <= commandDeclaration; declarations++ {
		if declarations == cloneDeclaration || declarations == interfaceDeclaration { // TODO --- yeah, yeah, I am filled with shame.
			continue
		}
		for chunk := 0; chunk < len(uP.Parser.TokenizedDeclarations[declarations]); chunk++ {
			uP.Parser.TokenizedCode = uP.Parser.TokenizedDeclarations[declarations][chunk]
			uP.Parser.TokenizedDeclarations[declarations][chunk].ToStart()
			uP.Parser.ParsedDeclarations[declarations] = append(uP.Parser.ParsedDeclarations[declarations], uP.Parser.ParseTokenizedChunk())
		}
	}

	uP.Parser.AllFunctionIdents.AddSet(uP.Parser.Functions)
	uP.Parser.AllFunctionIdents.AddSet(uP.Parser.Prefixes)
	uP.Parser.AllFunctionIdents.AddSet(uP.Parser.Forefixes)
	uP.Parser.AllFunctionIdents.AddSet(uP.Parser.Midfixes)
	uP.Parser.AllFunctionIdents.AddSet(uP.Parser.Endfixes)
	uP.Parser.AllFunctionIdents.AddSet(uP.Parser.Infixes)
	uP.Parser.AllFunctionIdents.AddSet(uP.Parser.Suffixes)
	uP.Parser.AllFunctionIdents.AddSet(uP.Parser.Unfixes)

	uP.Parser.Bling.AddSet(uP.Parser.Forefixes)
	uP.Parser.Bling.AddSet(uP.Parser.Midfixes)
	uP.Parser.Bling.AddSet(uP.Parser.Endfixes)
}

func (uP *Initializer) SetRelexer(rl lexer.Relexer) {
	uP.rl = rl
}

func flatten(s string) string {
	return strings.ReplaceAll(s, ".", "_")
}

// Having made the parsers FunctionTable, each function name is associated with an (partially) ordered list of
// associated functions such that a more specific type signature comes before a less specific one.
// We will now re-represent this as a tree.
func (cp *Compiler) MakeFunctionTrees() {
	cp.P.FunctionForest = map[string]*ast.FunctionGroup{}
	rc := 0
	for k, v := range cp.P.FunctionTable {
		tree := &ast.FnTreeNode{Fn: nil, Branch: []*ast.TypeNodePair{}}
		for i := range v {
			tree = cp.addSigToTree(tree, v[i], 0)

			refs := 0 // Overloaded functions must have the same number of reference variables, which go at the start.
			for ; refs < len(v[i].NameSig) && v[i].NameSig[refs].VarType == "ref"; refs++ {
			}
			if i == 0 {
				rc = refs
			} else {
				if refs != rc {
					cp.P.Throw("init/overload/ref", v[i].Body.GetToken())
					break
				}
			}
		}
		cp.P.FunctionForest[k] = &ast.FunctionGroup{Tree: tree, RefCount: rc}
		if settings.FUNCTION_TO_PEEK != "" && k == settings.FUNCTION_TO_PEEK {
			println("Function tree for " + k)
			println(cp.P.FunctionForest[k].Tree.IndentString(""))
		}
	}
}

// Note that the sigs have already been sorted on their specificity.
func (cp *Compiler) addSigToTree(tree *ast.FnTreeNode, fn *ast.PrsrFunction, pos int) *ast.FnTreeNode {
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
				branch.Node = cp.addSigToTree(branch.Node, fn, pos+1)
				if currentTypeName == "tuple" && !(branch.Type.Contains(values.TUPLE)) {
					cp.addSigToTree(branch.Node, fn, pos)
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

func (uP *Initializer) addWordsToParser(currentChunk *token.TokenizedCodeChunk) {
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
			uP.Throw("init/inexplicable", tok)
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
					uP.Parser.Forefixes.Add(tok.Literal)
				} else {
					uP.Parser.Midfixes.Add(tok.Literal)
				}
			} else {
				uP.Parser.Infixes.Add(tok.Literal)
			}
			hasMidOrEndfix = true
			lastTokenWasFix = true
			continue
		}

		if hasPrefix || hasMidOrEndfix {
			uP.Parser.Endfixes.Add(tok.Literal)
		} else {
			uP.Parser.Suffixes.Add(tok.Literal)
		}
		hasMidOrEndfix = true
		lastTokenWasFix = true
	}

	if hasPrefix {
		if hasMidOrEndfix {
			uP.Parser.Prefixes.Add(prefix)
		} else {
			if hasParams {
				uP.Parser.Functions.Add(prefix)
			} else {
				uP.Parser.Unfixes.Add(prefix)
			}
		}
	} else {
		if hasMidOrEndfix && !inParenthesis && !(tok.Literal == ")") && !uP.Parser.Suffixes.Contains(tok.Literal) {
			uP.Parser.Endfixes.Add(tok.Literal)
		}
	}
}

////////////////////////////////////////////////////////////////////////////

// The initializer keeps its errors inside the parser it's initializing.

func (uP *Initializer) Throw(errorID string, tok token.Token, args ...any) {
	uP.Parser.Throw(errorID, &tok, args...)
}

func (uP *Initializer) ErrorsExist() bool {
	return len(uP.Parser.Errors) > 0
}

func (uP *Initializer) ReturnErrors() string {
	return uP.Parser.ReturnErrors()
}
