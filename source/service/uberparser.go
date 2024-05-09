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
	"pipefish/source/dtypes"
	"pipefish/source/lexer"
	"pipefish/source/parser"
	"pipefish/source/report"
	"pipefish/source/settings"
	"pipefish/source/token"
)

type Section int

const (
	ImportSection Section = iota
	VarSection
	CmdSection
	DefSection
	LanguagesSection
	ExternalSection
	TypesSection
	ConstSection
	UndefinedSection
)

type declarationType int

const (
	importDeclaration          declarationType = iota
	enumDeclaration                            //
	structDeclaration                          //
	languageDeclaration                        //
	externalDeclaration                        //
	abstractDeclaration                        // The fact that these things come
	constantDeclaration                        // in this order is used in the code
	variableDeclaration                        // and should not be changed without
	functionDeclaration                        // a great deal of forethought.
	privateFunctionDeclaration                 //
	commandDeclaration                         //
	privateCommandDeclaration                  //
	golangDeclaration                          // Pure golang in a block; the Charm functions with golang bodies don't go here.

)

var tokenTypeToSection = map[token.TokenType]Section{
	token.IMPORT:  ImportSection,
	token.VAR:     VarSection,
	token.CMD:     CmdSection,
	token.DEF:     DefSection,
	token.LANG:    LanguagesSection,
	token.EXTERN:  ExternalSection,
	token.NEWTYPE: TypesSection,
	token.CONST:   ConstSection,
}

type Initializer struct {
	rl      lexer.Relexer
	Parser  *parser.Parser
	Sources map[string][]string
}

func NewInitializer(source, input string) *Initializer {
	uP := &Initializer{
		rl:      *lexer.NewRelexer(source, input),
		Parser:  parser.New(),
		Sources: make(map[string][]string),
	}
	uP.GetSource(source)
	return uP
}

func (init *Initializer) AddToNameSpace(thingsToImport []string) {
	for _, fname := range thingsToImport {
		libDat, _ := os.ReadFile(fname)
		stdImp := strings.TrimRight(string(libDat), "\n") + "\n"
		init.SetRelexer(*lexer.NewRelexer(fname, stdImp))
		init.MakeParserAndTokenizedProgram() // This is cumulative, it throws them all into the parser together.
		init.GetSource(fname)
	}
}

func (uP *Initializer) GetSource(source string) {
	if source == "" {
		return
	}
	file, err := os.Open(source)
	if err != nil {
		uP.Throw("init/source/open", token.Token{}, source)
	}
	defer file.Close()

	uP.Sources[source] = []string{}

	scanner := bufio.NewScanner(file) // TODO --- is there any reason this is line by line? Also why do we need the sources in the uP at this stage anyway?
	for scanner.Scan() {
		uP.Sources[source] = append(uP.Sources[source], scanner.Text())
	}
}

func (uP *Initializer) MakeParserAndTokenizedProgram() {
	currentSection := UndefinedSection
	beginCount := 0
	indentCount := 0
	lastTokenWasColon := false
	colonMeansFunctionOrCommand := true
	expressionIsAssignment := false
	expressionIsStruct := false
	expressionIsFunction := false
	expressionIsEnum := false
	isPrivate := false
	var (
		tok           token.Token
		definingToken token.Token
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
			colonMeansFunctionOrCommand = true
			continue
		}

		if tok.Type == token.PRIVATE {
			if isPrivate {
				uP.Throw("init/private", tok)
			}
			isPrivate = true
			continue
		}

		if tok.Type == token.IDENT && tok.Literal == "struct" && expressionIsAssignment {
			expressionIsAssignment = false
			expressionIsStruct = true
			definingToken = tok
		}

		if tok.Type == token.IDENT && tok.Literal == "enum" && expressionIsAssignment {
			expressionIsAssignment = false
			expressionIsEnum = true
			definingToken = tok
		}

		if tok.Literal == "=" && !(tok.Type == token.GVN_ASSIGN || tok.Type == token.STRING) {
			if currentSection != CmdSection {
				colonMeansFunctionOrCommand = false
				expressionIsAssignment = true
				definingToken = tok
			}
			switch currentSection {
			case DefSection:
				tok.Type = token.DEF_ASSIGN
				if expressionIsFunction {
					uP.Throw("init/def/assign", definingToken)
				}
			case TypesSection:
				tok.Type = token.DEF_ASSIGN // TODO --- might want to give it its own token. Or alternatively cull a few.
			case VarSection:
				if isPrivate {
					tok.Type = token.PVR_ASSIGN
				} else {
					tok.Type = token.VAR_ASSIGN
				}
			case CmdSection:
				tok.Type = token.CMD_ASSIGN
			default:
				tok.Type = token.ASSIGN
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
				expressionIsAssignment = false
				expressionIsStruct = false
				expressionIsEnum = false
				expressionIsFunction = false
				colonMeansFunctionOrCommand = true

				continue
			}
			switch currentSection {
			case ImportSection:
				if expressionIsAssignment {
					uP.Throw("init/import/assign", definingToken)
				} else {
					uP.Parser.TokenizedDeclarations[importDeclaration] =
						append(uP.Parser.TokenizedDeclarations[importDeclaration], line)
				}
			case LanguagesSection:
				if expressionIsAssignment {
					uP.Throw("init/lang/assign", definingToken)
				} else {
					uP.Parser.TokenizedDeclarations[languageDeclaration] =
						append(uP.Parser.TokenizedDeclarations[languageDeclaration], line)
				}
			case ExternalSection:
				if expressionIsAssignment {
					uP.Throw("init/external/assign", definingToken)
				} else {
					uP.Parser.TokenizedDeclarations[externalDeclaration] =
						append(uP.Parser.TokenizedDeclarations[externalDeclaration], line)
				}
			case CmdSection:
				line.ToStart()
				if line.Length() == 1 && line.NextToken().Type == token.GOCODE {
					uP.Parser.TokenizedDeclarations[golangDeclaration] =
						append(uP.Parser.TokenizedDeclarations[golangDeclaration], line)
				} else {
					if expressionIsAssignment {
						uP.Throw("init/cmd/assign", definingToken)
					} else {
						if isPrivate {
							uP.Parser.TokenizedDeclarations[privateCommandDeclaration] =
								append(uP.Parser.TokenizedDeclarations[privateCommandDeclaration], line)
						} else {
							uP.Parser.TokenizedDeclarations[commandDeclaration] =
								append(uP.Parser.TokenizedDeclarations[commandDeclaration], line)
						}
					}
				}
			case VarSection:
				switch {
				case !expressionIsAssignment:
					uP.Throw("init/var/function", definingToken)
				default:
					// As a wretched kludge, we will now weaken some of the commas on the LHS of
					// the assignment so that it parses properly. (TODO: at this point it would be much easier to
					// do this in the relexer.)
					lastWasType := false
					lastWasVar := false
					line.ToStart()
					for t := line.NextToken(); !(t.Type == token.VAR_ASSIGN || t.Type == token.PVR_ASSIGN); t = line.NextToken() {
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

					uP.Parser.TokenizedDeclarations[variableDeclaration] =
						append(uP.Parser.TokenizedDeclarations[variableDeclaration], line)

				}
			case DefSection:
				line.ToStart()
				if line.Length() == 1 && line.NextToken().Type == token.GOCODE {
					uP.Parser.TokenizedDeclarations[golangDeclaration] =
						append(uP.Parser.TokenizedDeclarations[golangDeclaration], line)
				} else {
					switch {
					case expressionIsAssignment:
						uP.Parser.TokenizedDeclarations[constantDeclaration] =
							append(uP.Parser.TokenizedDeclarations[constantDeclaration], line)
					case expressionIsStruct:
						uP.Parser.TokenizedDeclarations[structDeclaration] =
							append(uP.Parser.TokenizedDeclarations[structDeclaration], line)
					case expressionIsEnum:
						uP.Parser.TokenizedDeclarations[enumDeclaration] =
							append(uP.Parser.TokenizedDeclarations[enumDeclaration], line)
					default:
						if isPrivate {
							uP.Parser.TokenizedDeclarations[privateFunctionDeclaration] =
								append(uP.Parser.TokenizedDeclarations[privateFunctionDeclaration], line)
						} else {
							uP.Parser.TokenizedDeclarations[functionDeclaration] =
								append(uP.Parser.TokenizedDeclarations[functionDeclaration], line)
						}
					}
				}
			case ConstSection:
				if expressionIsAssignment {
					uP.Parser.TokenizedDeclarations[constantDeclaration] =
						append(uP.Parser.TokenizedDeclarations[constantDeclaration], line)
				}
			case TypesSection:
				switch {
				case expressionIsAssignment:
					uP.Parser.TokenizedDeclarations[abstractDeclaration] =
						append(uP.Parser.TokenizedDeclarations[abstractDeclaration], line)
				case expressionIsStruct:
					uP.Parser.TokenizedDeclarations[structDeclaration] =
						append(uP.Parser.TokenizedDeclarations[structDeclaration], line)
				case expressionIsEnum:
					uP.Parser.TokenizedDeclarations[enumDeclaration] =
						append(uP.Parser.TokenizedDeclarations[enumDeclaration], line)
				}
			}
			line = token.NewCodeChunk()
			expressionIsAssignment = false
			expressionIsStruct = false
			expressionIsEnum = false
			expressionIsFunction = false
			colonMeansFunctionOrCommand = true
			continue
		}

		if (tok.Type == token.NEWLINE) && line.Length() == 0 {
			continue
		}

		lastTokenWasColon = (tok.Type == token.COLON || tok.Type == token.WEAK_COLON)

		if (lastTokenWasColon || tok.Type == token.PIPE) && colonMeansFunctionOrCommand {
			colonMeansFunctionOrCommand = false
			uP.addWordsToParser(line)
			if currentSection == DefSection {
				expressionIsFunction = true
				definingToken = tok
			}
		}
		line.Append(tok)
	}
	if lastTokenWasColon {
		uP.Throw("init/unfinished", tok)
	}
	uP.Parser.Errors = report.MergeErrors(uP.rl.GetErrors(), uP.Parser.Errors)
}

func (uP *Initializer) ParseImports() {
	uP.Parser.ParsedDeclarations[importDeclaration] = parser.ParsedCodeChunks{}
	for chunk := 0; chunk < len(uP.Parser.TokenizedDeclarations[importDeclaration]); chunk++ {
		uP.Parser.TokenizedCode = uP.Parser.TokenizedDeclarations[importDeclaration][chunk]
		uP.Parser.TokenizedDeclarations[importDeclaration][chunk].ToStart()
		uP.Parser.ParsedDeclarations[importDeclaration] = append(uP.Parser.ParsedDeclarations[importDeclaration], uP.Parser.ParseTokenizedChunk())
	}
}

func (uP *Initializer) ParseTypeDefs() {
	// First we need to make the struct types into types so the parser parses them properly.
	for chunk := 0; chunk < len(uP.Parser.TokenizedDeclarations[structDeclaration]); chunk++ {
		uP.Parser.TokenizedDeclarations[structDeclaration][chunk].ToStart()
		tok1 := uP.Parser.TokenizedDeclarations[structDeclaration][chunk].NextToken()
		tok2 := uP.Parser.TokenizedDeclarations[structDeclaration][chunk].NextToken()
		if !(tok1.Type == token.IDENT && tok2.Type == token.DEF_ASSIGN) {
			uP.Throw("init/struct", tok1)
		} else {
			uP.Parser.TokenizedDeclarations[structDeclaration][chunk].Change(token.Token{Type: token.TYP_ASSIGN, Literal: "=", Line: tok2.Line, Source: tok2.Source})
			uP.Parser.TypeSystem.AddTransitiveArrow(tok1.Literal, "struct")
			uP.Parser.TypeSystem.AddTransitiveArrow(tok1.Literal+"?", "struct?")
			uP.Parser.TypeSystem.AddTransitiveArrow("null", tok1.Literal+"?")
			uP.Parser.TypeSystem.AddTransitiveArrow(tok1.Literal, tok1.Literal+"?")
			uP.Parser.Suffixes.Add(tok1.Literal)
			uP.Parser.Suffixes.Add(tok1.Literal + "?")
			uP.Parser.AllFunctionIdents.Add(tok1.Literal)
			uP.Parser.Functions.Add(tok1.Literal)
			uP.Parser.Structs.Add(tok1.Literal)
		}
	}

	// Now we can parse them.

	for chunk := 0; chunk < len(uP.Parser.TokenizedDeclarations[structDeclaration]); chunk++ {
		uP.Parser.TokenizedCode = uP.Parser.TokenizedDeclarations[structDeclaration][chunk]
		uP.Parser.TokenizedDeclarations[structDeclaration][chunk].ToStart()
		uP.Parser.ParsedDeclarations[structDeclaration] = append(uP.Parser.ParsedDeclarations[structDeclaration], uP.Parser.ParseTokenizedChunk())
	}
}

func (uP *Initializer) MakeLanguagesAndExternals() {
	for kindOfDeclarationToParse := languageDeclaration; kindOfDeclarationToParse <= externalDeclaration; kindOfDeclarationToParse++ {
		for _, v := range uP.Parser.TokenizedDeclarations[kindOfDeclarationToParse] {
			v.ToStart()
			uP.Parser.TokenizedCode = v
			parsedCode := uP.Parser.ParseTokenizedChunk()
			name := ""
			path := ""
			switch parsedCode := parsedCode.(type) {
			case *ast.Identifier:
				name = parsedCode.Value
			case *ast.InfixExpression:
				if kindOfDeclarationToParse == languageDeclaration {
					uP.Throw("init/lang/infix", parsedCode.Token)
				}
				if parsedCode.GetToken().Literal != "::" {
					uP.Throw("init/external/infix", parsedCode.Token)
				}
				lhs := parsedCode.Args[0]
				rhs := parsedCode.Args[2]
				switch rhs := rhs.(type) {
				case *ast.StringLiteral:
					path = rhs.Value
					switch lhs := lhs.(type) {
					case *ast.Identifier:
						name = lhs.Value
					default:
						uP.Throw("init/external/ident", *lhs.GetToken())
					}
				default:
					uP.Throw("init/external/string", *lhs.GetToken())
				}
			case *ast.StringLiteral:
				path = parsedCode.Value
				name = path
				if strings.LastIndex(name, ".") >= 0 {
					name = name[:strings.LastIndex(name, ".")]
				}
				if strings.LastIndex(name, "/") >= 0 {
					name = name[strings.LastIndex(name, "/")+1:]
				}
			default:
				if kindOfDeclarationToParse == externalDeclaration {
					uP.Throw("init/external/form", *parsedCode.GetToken())
				}
				uP.Throw("init/lang/form", *parsedCode.GetToken())
			}
			if name != "" {
				var ty string
				if kindOfDeclarationToParse == languageDeclaration {
					ty = "language"
					uP.Parser.Languages = append(uP.Parser.Languages, name)

				} else {
					ty = "external"
					uP.Parser.Externals[name] = path
				}
				uP.Parser.TypeSystem.AddTransitiveArrow(name, ty)
				uP.Parser.TypeSystem.AddTransitiveArrow(name, name+"?")
				uP.Parser.TypeSystem.AddTransitiveArrow("null", name+"?")
				uP.Parser.Suffixes.Add(name)
				uP.Parser.AllFunctionIdents.Add(name)
				uP.Parser.Functions.Add(name)
				uP.Parser.Structs.Add(name)
			}
		}
	}
}

func (uP *Initializer) ParseEverything() {
	// uP.Parser.Unfixes.Add("break")
	uP.Parser.Unfixes.Add("stop")
	for declarations := languageDeclaration; declarations <= privateCommandDeclaration; declarations++ {
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

func (uP *Initializer) ImportsExist() bool {
	return len(uP.Parser.TokenizedDeclarations[importDeclaration]) > 0
}

func (uP *Initializer) ReturnOrderOfAssignments(declarations int) []int {
	D := dtypes.Digraph[int]{}
	// I build the map and the dtypes.
	for i := range uP.Parser.TokenizedDeclarations[declarations] {
		D.AddSafe(i, []int{})
		// Then for each constant assignment i we slurp out the variables used on the RHS into a set.Set[string]
		uP.Parser.TokenizedDeclarations[declarations][i].ToStart()
		_, RHS := uP.Parser.ExtractVariables(uP.Parser.TokenizedDeclarations[declarations][i])
		for j := range uP.Parser.TokenizedDeclarations[declarations] {
			// And then the same for the left hand side of each assignment j.
			uP.Parser.TokenizedDeclarations[declarations][j].ToStart()
			LHS, _ := uP.Parser.ExtractVariables(uP.Parser.TokenizedDeclarations[declarations][j])
			// If the RHS of i refers to variables on the LHS of j, then assignment j
			// must be performed before assignment i, and we represent this by adding an arrow
			// from i to j in the digraph with transitive closure.
			if RHS.OverlapsWith(LHS) {
				D.AddTransitiveArrow(i, j)
			}
		}
	}
	// And then we use the topological sort method of the digraph and return the result of the sort:
	result, _ := dtypes.Ordering(D)
	return result
}

// At this point we have our functions as parsed code chunks in the uP.Parser.ParsedDeclarations(functionDeclaration)
// slice. We want to read their signatures and order them according to specificity for the purposes of
// implementing overloading.
//
// We return the GoHandler, because the VmMaker is going to need the VM to fully build the Go source.
func (uP *Initializer) MakeFunctions(sourceName string) *GoHandler {
	// Some of our functions may be written in Go, so we have a GoHandler standing by just in case.
	goHandler := NewGoHandler(uP.Parser)
	for j := functionDeclaration; j <= privateCommandDeclaration; j++ {
		for i := 0; i < len(uP.Parser.ParsedDeclarations[j]); i++ {
			functionName, sig, rTypes, body, given, _ := uP.Parser.ExtractPartsOfFunction(uP.Parser.ParsedDeclarations[j][i])
			if body.GetToken().Type == token.PRELOG && body.GetToken().Literal == "" {
				body.(*ast.LogExpression).Value = parser.DescribeFunctionCall(functionName, &sig)
			}
			if uP.Parser.ErrorsExist() {
				return nil
			}
			ok := uP.Parser.FunctionTable.Add(uP.Parser.TypeSystem, functionName,
				ast.Function{Sig: sig, Rets: rTypes, Body: body, Given: given,
					Cmd:     j == commandDeclaration || j == privateCommandDeclaration,
					Private: j == privateCommandDeclaration || j == privateFunctionDeclaration})
			if !ok {
				uP.Throw("init/overload", token.Token{}, functionName)
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

// In order to handle dispatch at runtime, we will re-represent this as a tree. This will apart
// from anything else be rather faster. It also allows us to perform dispatch by evaluating one
// argument of the function at a time.
func (uP *Initializer) MakeFunctionTrees() {
	uP.Parser.FunctionGroupMap = map[string]*ast.FunctionGroup{}
	rc := 0
	for k, v := range uP.Parser.FunctionTable {
		tree := &ast.FnTreeNode{Fn: nil, Branch: []*ast.TypeNodePair{}}
		for i := range v {
			tree = uP.addSigToTree(tree, &v[i], 0)

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
func (uP *Initializer) addSigToTree(tree *ast.FnTreeNode, fn *ast.Function, pos int) *ast.FnTreeNode {
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

func (uP *Initializer) addError(err *report.Error) {
	uP.Parser.Errors = append(uP.Parser.Errors, err)
}

func (uP *Initializer) ErrorsExist() bool {
	return len(uP.Parser.Errors) > 0
}

func (uP *Initializer) ReturnErrors() string {
	return uP.Parser.ReturnErrors()
}
