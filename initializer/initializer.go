// Another day, another layer. The initializer takes the tokens from the
// relexer and splits it up into code types according to the headword, which
// is discarded. It breaks these up into function declarations, variable intializations, etc.

// As it does so it checks out the signatures of the functions and commands and decides
// what "grammatical" role the words in the function signature play, and deposits
// lists of these into a Parser object: the Prefix, Forefix, Midfix, Suffix, Endfix etc classes.

// We then have a tokenized program broken into parts, and a parser primed to
// parse tokens into ASTs. We apply one to the other and produce ASTs from our
// tokenized code, which we can then put into the evaluator.

// The variable and constant initializations are carried out to produce the initial environment,
// and the functions are processed to produce a function table in the Parser.

// The result of this is an environment and a parser which are put into a Service object in the
// hub's map of services.

package initializer

import (
	"fmt"
	"os"
	"strings"

	"charm/ast"
	"charm/digraph"
	"charm/evaluator"
	"charm/object"
	"charm/parser"
	"charm/relexer"
	"charm/signature"
	"charm/sysvars"
	"charm/text"
	"charm/token"
	"charm/tokenized_code_chunk"
)


type tokenizedCodeChunks []*tokenized_code_chunk.TokenizedCodeChunk

type parsedCodeChunks []*ast.Node

type Section int

const (
	ImportSection Section =  iota
	VarSection 
	CmdSection
	DefSection
	UndefinedSection
)

type declarationType int

const (	importDeclaration declarationType = iota
		typeDeclaration       //
		constantDeclaration   // The fact that these things come
		variableDeclaration   // in this order is used in the code
		functionDeclaration   // and should not be changed.
		commandDeclaration    //
		   
)

var sectionMap = map[Section]string {
	ImportSection : "import",
	VarSection : "var",
	CmdSection : "cmd",
	DefSection : "def",	
}

var tokenTypeToSection = map[token.TokenType] Section {
	token.IMPORT : ImportSection,
	token.VAR : VarSection,
	token.CMD : CmdSection,
	token.DEF : DefSection,
}

type Initializer struct {
	rl                 relexer.Relexer
	Parser             parser.Parser
	tokenizedDeclarations [6]tokenizedCodeChunks
	parsedDeclarations	  [6]parsedCodeChunks
}

func New(source, input string) *Initializer {	
	uP := &Initializer{
		rl:     *relexer.New(source, input),
		Parser: *parser.New(),
	}
	return uP
}


func (uP *Initializer) MakeParserAndTokenizedProgram(){
	currentSection:= UndefinedSection
	beginCount:= 0
	indentCount:=0;
	lastTokenWasColon:= false
	colonMeansFunction:= true
	expressionIsAssignment := false
	expressionIsStruct := false
	isPrivate := false
	var tok token.Token

	tok = uP.rl.NextToken() // note that we've already removed leading newlines.
	if tok.Type == token.EOF { // An empty file should still initiate a service, but one with no data.
		return
	}
	if !token.TokenTypeIsHeadword(tok.Type) {
		uP.MakeError(fmt.Sprintf("occurence of %v without a headword", text.Emph(tok.Literal)), tok)
		return
	}
	currentSection = tokenTypeToSection[tok.Type]

	line := tokenized_code_chunk.New()
	
	for tok = uP.rl.NextToken(); tok.Type != token.EOF; tok = uP.rl.NextToken()  {
		// if tok.Source != "builtin library" { fmt.Printf("Relexer says %v\n", tok) }
		if token.TokenTypeIsHeadword(tok.Type) {
			if tok.Literal == "import" {
				uP.MakeError("if it occurs " + text.Emph("import") + " must be the first headword", tok)
			}
			currentSection = tokenTypeToSection[tok.Type]
			isPrivate = false
			lastTokenWasColon = false
			colonMeansFunction = true
			continue
		}

		if tok.Type == token.PRIVATE {
			if isPrivate { uP.MakeError(fmt.Sprintf(text.Emph("private") + " redeclared"), tok) }
			isPrivate = true
			continue
		}

		if tok.Type == token.IDENT && tok.Literal == "struct" && expressionIsAssignment {
			expressionIsAssignment = false
			expressionIsStruct = true
		}

		if tok.Literal == "=" && ! (tok.Type == token.GVN_ASSIGN || tok.Type == token.STRING) {
			if currentSection != CmdSection {
				colonMeansFunction = false
				expressionIsAssignment = true
			}
			switch currentSection {
			case DefSection : tok.Type = token.DEF_ASSIGN
			case VarSection :
				if isPrivate {
					tok.Type = token.PVR_ASSIGN
				} else {
					tok.Type = token.VAR_ASSIGN	
				}
			case CmdSection : tok.Type = token.CMD_ASSIGN
			default : tok.Type = token.ASSIGN
			}
		}

		if tok.Type == token.LPAREN {
			beginCount++
			if tok.Literal == "|->" {
				indentCount ++
			}
		}

		if tok.Type == token.RPAREN {
			beginCount--
			if tok.Literal == "<-|" {
				indentCount --
			}
		}

		if (tok.Type == token.NEWLINE) && 
		       !lastTokenWasColon && indentCount == 0 && line.Length() != 0 {
			if beginCount != 0 {
				uP.MakeError(text.Emph("(") + " unclosed by outdent", tok)
				continue
			}
			switch currentSection {
			case ImportSection:
				if expressionIsAssignment {
					uP.MakeError("assignment in import section", tok)
				} else {
					uP.tokenizedDeclarations[importDeclaration] = 
					/**/append(uP.tokenizedDeclarations[importDeclaration], line)
				}
			case CmdSection:
			if expressionIsAssignment {
				uP.MakeError("assignment outside of command block in cmd section", tok)
			} else {
				uP.tokenizedDeclarations[commandDeclaration] = 
				/**/append(uP.tokenizedDeclarations[commandDeclaration], line)
			}
			case VarSection:
				if !expressionIsAssignment {
					uP.MakeError("function/command declaration in var section", tok)
				} else {

					// As a wretched kludge, we will now weaken some of the commas on the LHS of
					// the assignment so that it parses properly.
					lastWasType := false
					line.ToStart()
					for t := line.NextToken() ; !(t.Type == token.VAR_ASSIGN || t.Type == token.PVR_ASSIGN) ; t = line.NextToken() {
						if t.Type == token.COMMA && lastWasType {
							line.Change(token.Token{Type: token.WEAK_COMMA, Line: tok.Line, Literal: ","})
						}
						lastWasType = t.Type == token.IDENT && parser.TypeExists(t.Literal, uP.Parser.TypeSystem)
					}

					uP.tokenizedDeclarations[variableDeclaration] = 
					/**/append(uP.tokenizedDeclarations[variableDeclaration], line)
				}
			case DefSection:
				switch {
				case expressionIsAssignment :
					uP.tokenizedDeclarations[constantDeclaration] = 
					/**/append(uP.tokenizedDeclarations[constantDeclaration], line)
				case expressionIsStruct :
					uP.tokenizedDeclarations[typeDeclaration] = 
					/**/append(uP.tokenizedDeclarations[typeDeclaration], line)
				default :
					uP.tokenizedDeclarations[functionDeclaration] = 
					/**/append(uP.tokenizedDeclarations[functionDeclaration], line)
				}
			}
			line = tokenized_code_chunk.New()
			expressionIsAssignment = false
			expressionIsStruct = false
			colonMeansFunction = true
			continue
		}

		if (tok.Type == token.NEWLINE) && line.Length() == 0 {
			continue
		}

		lastTokenWasColon = (tok.Type == token.COLON) 

		if lastTokenWasColon && colonMeansFunction {
			colonMeansFunction = false
			uP.addWordsToParser(line)
		}

        line.Append(tok)
	}
	uP.Parser.Errors = append(uP.Parser.Errors, uP.rl.Errors...)
}

func (uP *Initializer) ParseImports() {
	for chunk := 0; chunk < len(uP.tokenizedDeclarations[importDeclaration]); chunk++ {
		uP.Parser.TokenizedCode = uP.tokenizedDeclarations[importDeclaration][chunk]
		uP.tokenizedDeclarations[importDeclaration][chunk].ToStart()
		uP.parsedDeclarations[importDeclaration] = append(uP.parsedDeclarations[importDeclaration], uP.Parser.ParseTokenizedChunk())
	}
}

func (uP *Initializer) ParseTypeDefs() {
		// First we need to make the struct types into types so the parser parses them properly.
		for chunk := 0; chunk < len(uP.tokenizedDeclarations[typeDeclaration]); chunk++ {
			uP.tokenizedDeclarations[typeDeclaration][chunk].ToStart()
			tok1 := uP.tokenizedDeclarations[typeDeclaration][chunk].NextToken();
			tok2 := uP.tokenizedDeclarations[typeDeclaration][chunk].NextToken();
			if !(tok1.Type == token.IDENT && tok2.Type == token.DEF_ASSIGN) {
				uP.MakeError("malformed struct declaration", tok1)
			} else {
				uP.tokenizedDeclarations[typeDeclaration][chunk].Change(token.Token{Type: token.TYP_ASSIGN, Literal: "=", Line: tok2.Line, Source: tok2.Source})
				uP.Parser.TypeSystem.AddTransitiveArrow(tok1.Literal, "struct")
				uP.Parser.TypeSystem.AddTransitiveArrow("nil", tok1.Literal)
				uP.Parser.Suffixes.Add(tok1.Literal)
				uP.Parser.AllFunctionIdents.Add(tok1.Literal)
				uP.Parser.Functions.Add(tok1.Literal)
			}
		}
		// Now we can parse them.

	for chunk := 0; chunk < len(uP.tokenizedDeclarations[typeDeclaration]); chunk++ {
		uP.Parser.TokenizedCode = uP.tokenizedDeclarations[typeDeclaration][chunk]
		uP.tokenizedDeclarations[typeDeclaration][chunk].ToStart()
		uP.parsedDeclarations[typeDeclaration] = append(uP.parsedDeclarations[typeDeclaration], uP.Parser.ParseTokenizedChunk())
	}
}


func (uP *Initializer) EvaluateTypeDefs(env *object.Environment) {
	for _, v := range(uP.parsedDeclarations[typeDeclaration]) {
		result := evaluator.Evaluate(*v, &(uP.Parser), env)
		if result.Type() == object.ERROR_OBJ {
			uP.MakeError(result.(*object.Error).Message, result.(*object.Error).Token)
		}

	}
}

func (uP *Initializer) ParseEverything() {
	for declarations := constantDeclaration; declarations <= commandDeclaration; declarations++ {
		for chunk := 0; chunk < len(uP.tokenizedDeclarations[declarations]); chunk++ {
			uP.Parser.TokenizedCode = uP.tokenizedDeclarations[declarations][chunk]
			uP.tokenizedDeclarations[declarations][chunk].ToStart()
			uP.parsedDeclarations[declarations] = append(uP.parsedDeclarations[declarations], uP.Parser.ParseTokenizedChunk())
			// uP.tokenizedDeclarations[declarations][chunk].ToStart()
			// fmt.Println(uP.tokenizedDeclarations[declarations][chunk].String())
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
}

func (uP *Initializer) InitializeEverything(env *object.Environment) {
	uP.makeFunctions()
	env.InitializeConstant("NIL", object.NIL)
	// Initialize the user-declared constants and variables
	for declarations := constantDeclaration; declarations <= variableDeclaration; declarations ++ {
		assignmentOrder := uP.returnOrderOfAssignments(declarations);
		for k := range(*assignmentOrder) {
			result := evaluator.Evaluate(*uP.parsedDeclarations[declarations][k], &(uP.Parser), env)
			if result.Type() == object.ERROR_OBJ {
				uP.MakeError(result.(*object.Error).Message, result.(*object.Error).Token)
			}
		}
		if declarations == constantDeclaration {
			// We copy this to the global constants map
			for k, v := range env.Store {
				uP.Parser.Globals.Store[k] = v
			}
			// And we intialize the sysvars before doing the user variables.
			for k, v := range (sysvars.Sysvars) {
				env.InitializeVariable(k, v.Dflt, object.TrueType(v.Dflt))
			}
		}
	}
}

func (uP *Initializer) ImportEverything() {
	// First the standard library

	libDat, _ := os.ReadFile("rsc/builtins.ch")
	stdImp := strings.TrimRight(string(libDat), "\n") + "\n"
	uP.rl = *relexer.New("builtin library", stdImp)
	uP.MakeParserAndTokenizedProgram()

	// And then the declared imports
	for _, imp := range(uP.parsedDeclarations[importDeclaration]) {
		scriptFilepath := ""
		namespace := ""
		switch imp := (*imp).(type) {
		case *ast.StringLiteral : 
			scriptFilepath = imp.Value
		case *ast.InfixExpression :
			if imp.TokenLiteral() != "::" {
				uP.MakeError("unexpected occurrence of " + text.Emph(imp.TokenLiteral()) +
				/**/ " in the " + text.Emph("import") + " section ", imp.Token)
			}
			lhs := imp.Left; rhs := imp.Right
			switch rhs := rhs.(type) {
			case *ast.StringLiteral :
				namespace = rhs.Value
				switch lhs := lhs.(type) {
					case *ast.StringLiteral : 
						scriptFilepath = lhs.Value
					default :
					uP.MakeError("unexpected occurrence of " + text.Emph(lhs.TokenLiteral()) +
					/**/ " in the " + text.Emph("import") + " section ", lhs.GetToken())	
				}
			default :
			uP.MakeError("unexpected occurrence of " + text.Emph(lhs.TokenLiteral()) +
			/**/ " in the " + text.Emph("import") + " section ", lhs.GetToken())	
			}	
		default :
			uP.MakeError("unexpected occurrence of " + text.Emph(imp.TokenLiteral()) +
			/**/ " in the " + text.Emph("import") + " section ", imp.GetToken())
		}
		code := ""
		if scriptFilepath != "" {
			dat, err := os.ReadFile(scriptFilepath)
			if err != nil {
				uP.MakeError("import " + text.Emph(scriptFilepath) + " not found", (*imp).GetToken())
				return
			} else {
				code = strings.TrimRight(string(dat), "\n") + "\n"
			}
		}
	uP.rl = *relexer.New(scriptFilepath, code)
	if namespace != "" { fmt.Println("****namespace = " + namespace) }
	uP.MakeParserAndTokenizedProgram()
	}
}

func (uP *Initializer) returnOrderOfAssignments(declarations declarationType) *[]int {
	
	D := digraph.Digraph[int]{}
	// I build the map and the digraph.
	for i := range(uP.tokenizedDeclarations[declarations]) {
		D.AddSafe(i, []int{})
		// Then for each constant assignment i we slurp out the variables used on the RHS into a set.Set[string]
		uP.tokenizedDeclarations[declarations][i].ToStart()
		_, RHS := uP.Parser.ExtractVariables(uP.tokenizedDeclarations[declarations][i]);
		for j := range(uP.tokenizedDeclarations[declarations])  {
			// And then the same for the left hand side of each assignment j.
			uP.tokenizedDeclarations[declarations][j].ToStart()
			LHS, _ := uP.Parser.ExtractVariables(uP.tokenizedDeclarations[declarations][j]);
			// If the RHS of i refers to variables on the LHS of j, then assignment j
			// must be performed before assignment i, and we represent this by adding an arrow
			// from i to j in the digraph with transitive closure.
			if RHS.OverlapsWith(LHS) { D.AddTransitiveArrow(i, j) }
		}	
	}
	// And then we use the topological sort method of the digraph and return the result of the sort:
	result, _ := digraph.Ordering(D)
	return result
}

// At this point we have our functions as parsed code chunks in the uP.parsedDeclarations(functionDeclaration)
// slice. We want to read their signatures and order them according to specificity for the purposes of 
// implementing overloading.
func (uP *Initializer) makeFunctions() {
	for j := functionDeclaration; j <= commandDeclaration; j++ {
		for i := 0; i < len(uP.parsedDeclarations[j]) ; i++ {
			keyword, sig, body, given, error := uP.extractSignature(*uP.parsedDeclarations[j][i])
			if error == nil {
				ok := uP.Parser.FunctionTable.Add(uP.Parser.TypeSystem, keyword, ast.Function{Sig: sig, Body: body, Given: given, Cmd: j == commandDeclaration})
				if ! ok {
					uP.MakeError("too much overloading of keyword " + text.Emph(keyword), error.Token) 
				}
			} else {
				uP.MakeError(error.Message, error.Token)
			}
		}	
	}
}

// Slurps the signature of a function out of it. As the colon after a function definition has
// extremely low precedence, we should find it at the root of the tree.
// We extract the keyword first and then hand its branch or branches off to a recursive tree-slurper.
func (uP *Initializer) extractSignature(fn ast.Node) (string, signature.NamedSignature, ast.Node, ast.Node, *object.Error) {
	var (
		keyword string
		sig signature.NamedSignature
		start, content, given ast.Node
	)
	if fn.TokenLiteral() == "given" {
		given = fn.(*ast.InfixExpression).Right
		fn = fn.(*ast.InfixExpression).Left
	}

	switch fn := fn.(type) {
	case *ast.LazyInfixExpression :	
		if fn.Token.Type != token.COLON {
			return keyword, sig, content, given, &object.Error{Message: "malformed command or function definition", Token: fn.GetToken()}
		}
		start = fn.Left
		content = fn.Right
	default :
		return keyword, sig, content, given, &object.Error{Message: "malformed command or function definition", Token: fn.GetToken()}
	}
	switch start := start.(type) {
		case *ast.PrefixExpression :
			keyword = start.Operator
			sig = uP.Parser.RecursivelySlurpSignature(start.Right, "single")
		case *ast.InfixExpression :
			keyword = start.Operator
			LHS := uP.Parser.RecursivelySlurpSignature(start.Left, "single")
			RHS := uP.Parser.RecursivelySlurpSignature(start.Right, "single")
			middle := signature.NameTypePair{VarName: start.Operator, VarType: "bling"}
			sig = append(append(LHS, middle), RHS...) 
		case *ast.SuffixExpression :
			keyword = start.Operator
			sig = uP.Parser.RecursivelySlurpSignature(start.Left, "single")
		case *ast.UnfixExpression :
			keyword = start.Operator
			sig = signature.NamedSignature{}
		default :
			return keyword, sig, content, given, &object.Error{Message: "malformed command or function definition", Token: fn.GetToken()}
	}
	return keyword, sig, content, given, nil
}


func (uP *Initializer) addWordsToParser(currentChunk *tokenized_code_chunk.TokenizedCodeChunk){
	inParenthesis := false
	hasPrefix := false
	hasParams := false
	hasMidOrEndfix := false
	lastTokenWasType := false
	lastTokenWasFix := false
	prefix := ""
	currentChunk.ToStart()
	for j := 0; j < currentChunk.Length(); j++ {
		tok := currentChunk.NextToken()
		
		if tok.Type == token.LPAREN {
			hasParams = true
			inParenthesis = true
			lastTokenWasFix = false
			continue
		}

		if inParenthesis {   // We identify types in function definitions syntactically and give their commas 
			if lastTokenWasType && tok.Type == token.COMMA {   // lower precedence.
				currentChunk.Change(token.Token{Type: token.WEAK_COMMA, Literal: ",,", Line: tok.Line,})
				lastTokenWasType = false
			}
			if tok.Type == token.IDENT {
				if parser.TypeExists(tok.Literal, uP.Parser.TypeSystem) {
					lastTokenWasType = true
				} else {
					lastTokenWasType = false
				}
			}
		}

		if tok.Type == token.RPAREN {
			inParenthesis = false
	        lastTokenWasType = false
			continue
		}

		if inParenthesis {
			continue
		}

		if tok.Type != token.IDENT {
			uP.MakeError(fmt.Sprintf("inexplicable occurence of %v", tok.Literal), tok)
		}
			
		if j == 0 {
			prefix = tok.Literal
			hasPrefix = true
			lastTokenWasFix = true
			continue
		}
			
		if j < currentChunk.Length() - 1 {
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
	}

}

func (uP *Initializer) ReportScriptFailure() {
	fmt.Print("\nThere were errors in initializing the script:\n\n")
	fmt.Println(uP.ReturnErrors())
}

func (uP *Initializer) MakeError(msg string, token token.Token) {
	uP.Parser.Errors = append(uP.Parser.Errors, &object.Error{Message: msg, Token: token})
}

func (uP *Initializer) ErrorsExist() bool {
	return len(uP.Parser.Errors) > 0
}

func (uP *Initializer) ReturnErrors() string {
	return uP.Parser.ReturnErrors()
}

func (uP *Initializer) ClearErrors() {
	uP.Parser.ClearErrors()
}



