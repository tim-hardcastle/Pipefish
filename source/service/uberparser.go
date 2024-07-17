// The uberparser helps the vmmaker by doing a lot of the work involved in setting up the parser.
// It is largely separate from the main vmmaker for historical reason but it also adds some clarity
// to group together aspects of the initialization process which don't need to touch the vm.

// In opertation, it takes the tokens from the relexer and splits it up into code types according to the headword,
// which is discarded. It breaks these up into function declarations, variable intializations, etc.

// As it does so it checks out the signatures of the functions and commands and decides
// what "grammatical" role the words in the function signature play, and deposits
// lists of these into a Parser object: the Prefix, Forefix, Midfix, Suffix, Endfix etc classes.

// We then have a tokenized program broken into parts, and a parser primed to
// parse tokens into ASTs. We apply one to the other and produce ASTs from our
// tokenized code.

package service

import (
	"bufio"
	"os"
	"strings"

	"pipefish/source/ast"
	"pipefish/source/lexer"
	"pipefish/source/parser"
	"pipefish/source/report"
	"pipefish/source/settings"
	"pipefish/source/text"
	"pipefish/source/token"
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
	token.LANG:    LanguagesSection,
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
	fnIndex map[fnSource]*ast.PrsrFunction // We need to number the functions after we sort them into the function tree, in order of compilation. This keeps track of where they are.
}

func NewInitializer(source, sourceCode, dir string) *Initializer {
	uP := &Initializer{
		rl:      *lexer.NewRelexer(source, sourceCode),
		Parser:  parser.New(dir),
		Sources: make(map[string][]string),
		fnIndex: make(map[fnSource]*ast.PrsrFunction),
	}
	uP.GetSource(source)
	return uP
}

func (init *Initializer) AddToNameSpace(thingsToImport []string) {
	for _, fname := range thingsToImport {
		fname = init.Parser.Directory + fname
		libDat, err := os.ReadFile(fname)
		if err != nil {
			println("Here's your problem.")
			init.Throw("init/import/found", token.Token{})
		}
		stdImp := strings.TrimRight(string(libDat), "\n") + "\n"
		init.SetRelexer(*lexer.NewRelexer(fname, stdImp))
		init.MakeParserAndTokenizedProgram() // This is cumulative, it throws them all into the parser together.
		init.GetSource(fname)
	}
}

func (uP *Initializer) GetSource(source string) {
	if source == "" || len(source) >= 5 && source[0:5] == "http:" {
		return
	}
	if len(source) >= 4 && source[0:4] == "rsc/" {
		source = uP.Parser.Directory + source
	}
	file, err := os.Open(source)
	if err != nil {
		uP.Throw("init/source/c", token.Token{}, source)
	}
	defer file.Close()

	uP.Sources[source] = []string{}

	scanner := bufio.NewScanner(file) // TODO --- is there any reason this is line by line? Also why do we need the sources in the uP at this stage anyway?
	for scanner.Scan() {
		uP.Sources[source] = append(uP.Sources[source], scanner.Text())
	}
}

func (uP *Initializer) addTokenizedDeclaration(decType declarationType, line *token.TokenizedCodeChunk, private bool) {
	line.Private = private
	uP.Parser.TokenizedDeclarations[decType] = append(uP.Parser.TokenizedDeclarations[decType], line)
}

func (uP *Initializer) MakeParserAndTokenizedProgram() {
	currentSection := UndefinedSection
	beginCount := 0
	indentCount := 0
	lastTokenWasColon := false
	expressionIsStruct := false
	expressionIsEnum := false
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
			if tok.Literal == "struct" {
				expressionIsStruct = true
			}
			if tok.Literal == "enum" {
				expressionIsEnum = true
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
				expressionIsStruct = false
				expressionIsEnum = false
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
				switch {
				case expressionIsStruct:
					uP.addTokenizedDeclaration(structDeclaration, line, isPrivate)
				case expressionIsEnum:
					uP.addTokenizedDeclaration(enumDeclaration, line, isPrivate)
				default:
					uP.addTokenizedDeclaration(abstractDeclaration, line, isPrivate)
				}
			default:
				panic("Unhandled section type.")
			}
			line = token.NewCodeChunk()
			expressionIsStruct = false
			expressionIsEnum = false
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
	if lastTokenWasColon {
		uP.Throw("init/unfinished", tok)
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
			namespace = scriptFilepath
			scriptFilepath = uP.Parser.Directory + "lib/" + scriptFilepath + ".pf"
		} else {
			namespace = text.ExtractFileName(scriptFilepath)
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

var correspondingAbstractType = map[declarationType]string{enumDeclaration: "enum", structDeclaration: "struct", snippetDeclaration: "snippet", abstractDeclaration: "single"}

// We need to declare all the types as suffixes for all the user-defined types, and add at least their existence to the parser's type system,
// so that the parser will be able to parse the struct definitions.
func (uP *Initializer) addTypesToParser() {
	for kindOfType := enumDeclaration; kindOfType <= abstractDeclaration; kindOfType++ {
		for chunk := 0; chunk < len(uP.Parser.TokenizedDeclarations[kindOfType]); chunk++ {
			// Each of them should begin with the name of the type being declared, and then followed by an = unless it's a snippet declaration.
			uP.Parser.TokenizedDeclarations[kindOfType][chunk].ToStart()
			tok1 := uP.Parser.TokenizedDeclarations[kindOfType][chunk].NextToken()
			tok2 := uP.Parser.TokenizedDeclarations[kindOfType][chunk].NextToken()
			if tok1.Type != token.IDENT || (kindOfType != snippetDeclaration && tok2.Type != token.ASSIGN) {
				uP.Throw("init/type/form/a", tok1)
				continue
			}
			name := tok1.Literal
			hasNull := (name[len(name)-1] == '?')
			if hasNull && !(kindOfType == abstractDeclaration) {
				uP.Throw("init/type/null", tok1)
				continue
			}
			if uP.Parser.Suffixes.Contains(name) {
				uP.Throw("init/type/exists", tok1)
				continue
			}
			uP.Parser.Suffixes.Add(name)
			supertype := correspondingAbstractType[kindOfType]
			uP.Parser.TypeSystem.AddTransitiveArrow(name, supertype)
			if hasNull {
				uP.Parser.TypeSystem.AddTransitiveArrow("null", name)
				uP.Parser.TypeSystem.AddTransitiveArrow(name, supertype+"?")
				continue
			}
			uP.Parser.Suffixes.Add(name + "?")
			uP.Parser.TypeSystem.AddTransitiveArrow("null", name+"?")
			uP.Parser.TypeSystem.AddTransitiveArrow(name, name+"?")
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

func (uP *Initializer) MakeSnippets() {
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

func (uP *Initializer) isPrivate(x, y int) bool {
	return uP.Parser.TokenizedDeclarations[x][y].Private
}

// At this point we have our functions as parsed code chunks in the uP.Parser.ParsedDeclarations(functionDeclaration)
// slice. We want to read their signatures and order them according to specificity for the purposes of
// implementing overloading.
//
// We return the GoHandler, because the VmMaker is going to need the VM to fully build the Go source.
func (uP *Initializer) MakeFunctions() *GoHandler {
	// Some of our functions may be written in Go, so we have a GoHandler standing by just in case.
	goHandler := NewGoHandler(uP.Parser)
	for j := functionDeclaration; j <= commandDeclaration; j++ {
		for i := 0; i < len(uP.Parser.ParsedDeclarations[j]); i++ {
			functionName, position, sig, rTypes, body, given := uP.Parser.ExtractPartsOfFunction(uP.Parser.ParsedDeclarations[j][i])
			if body == nil {
				uP.Throw("init/func/body", *uP.Parser.ParsedDeclarations[j][i].GetToken())
				return nil
			}
			if body.GetToken().Type == token.PRELOG && body.GetToken().Literal == "" {
				body.(*ast.LogExpression).Value = parser.DescribeFunctionCall(functionName, &sig)
			}
			if uP.Parser.ErrorsExist() {
				return nil
			}
			functionToAdd := ast.PrsrFunction{Sig: sig, Position: position, Rets: rTypes, Body: body, Given: given,
				Cmd: j == commandDeclaration, Private: uP.isPrivate(int(j), i), Number: DUMMY}
			uP.fnIndex[fnSource{j, i}] = &functionToAdd
			ok := uP.Parser.FunctionTable.Add(uP.Parser.TypeSystem, functionName, &functionToAdd)
			if !ok {
				uP.Throw("init/overload", *body.GetToken(), functionName)
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
				goHandler.MakeFunction(flatten(functionName), sig, rTypes, body.(*ast.GolangExpression))
				if uP.Parser.ErrorsExist() {
					return nil
				}
				body.(*ast.GolangExpression).Sig = sig
				body.(*ast.GolangExpression).ReturnTypes = rTypes
			}
		}
	}

	// We may also have pure Go declarations:

	for _, gocode := range uP.Parser.TokenizedDeclarations[golangDeclaration] {
		gocode.ToStart()
		token := gocode.NextToken()
		source := token.Source
		code := token.Literal[:len(token.Literal)]
		goHandler.AddPureGoBlock(source, code)
	}
	return goHandler
}

func flatten(s string) string {
	return strings.ReplaceAll(s, ".", "_")
}

// Having made the parsers FunctionTable, each function name is associated with an (partially) ordered list of
// associated functions such that a more specific type signature comes before a less specific one.
// We will now re-represent this as a tree.
func (uP *Initializer) MakeFunctionTrees() {
	uP.Parser.FunctionGroupMap = map[string]*ast.FunctionGroup{}
	rc := 0
	for k, v := range uP.Parser.FunctionTable {
		tree := &ast.FnTreeNode{Fn: nil, Branch: []*ast.TypeNodePair{}}
		for i := range v {
			tree = uP.addSigToTree(tree, v[i], 0)

			refs := 0 // Overloaded functions must have the same number of reference variables, which go at the start.
			for ; refs < len(v[i].Sig) && v[i].Sig[refs].VarType == "ref"; refs++ {
			}
			if i == 0 {
				rc = refs
			} else {
				if refs != rc {
					uP.Throw("init/overload/ref", *v[i].Body.GetToken())
					break
				}
			}
		}
		uP.Parser.FunctionGroupMap[k] = &ast.FunctionGroup{Tree: tree, RefCount: rc}
		if settings.FUNCTION_TO_PEEK != "" && k == settings.FUNCTION_TO_PEEK {
			println("Function tree for " + k)
			println(uP.Parser.FunctionGroupMap[k].Tree.IndentString(""))
		}
	}
}

// Note that the sigs have already been sorted on their specificity.
func (uP *Initializer) addSigToTree(tree *ast.FnTreeNode, fn *ast.PrsrFunction, pos int) *ast.FnTreeNode {
	sig := fn.Sig
	if pos < len(sig) {
		var currentType string
		if sig[pos].VarType == "bling" {
			currentType = sig[pos].VarName
		} else {
			currentType = sig[pos].VarType
		}
		isPresent := false
		for _, v := range tree.Branch {
			if currentType == v.TypeName {
				isPresent = true
				break
			}
		}
		if !isPresent {
			tree.Branch = append(tree.Branch, &ast.TypeNodePair{TypeName: currentType, Node: &ast.FnTreeNode{Fn: nil, Branch: []*ast.TypeNodePair{}}})
		}
		for _, branch := range tree.Branch {
			if parser.IsSameTypeOrSubtype(uP.Parser.TypeSystem, branch.TypeName, currentType) {
				branch.Node = uP.addSigToTree(branch.Node, fn, pos+1)
				if currentType == "tuple" && !(branch.TypeName == "tuple") {
					uP.addSigToTree(branch.Node, fn, pos)
				}
			}
		}
	} else {
		if tree.Fn == nil { // If it is non-nil then a sig of greater specificity has already led us here and we're good.
			tree.Branch = append(tree.Branch, &ast.TypeNodePair{TypeName: "", Node: &ast.FnTreeNode{Fn: fn, Branch: []*ast.TypeNodePair{}}})
		}
	}
	return tree
}

/////////////////////////////////////////////////////////////////////////////////////////////////

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
