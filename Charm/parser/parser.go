package parser

import (
	"fmt"
	"strconv"
	"charm/ast"
	"charm/object"
	"charm/relexer"
	"charm/set"
	"charm/signature"
	"charm/text"
	"charm/token"
)

const (
	_ int = iota
	LOWEST
	SEMICOLON   // semantic newline or ;
	FUNC        // Lambda declaration
	GIVEN       // given
	ASSIGN      // =
	COLON       // :
	AND         // &&
	OR          // ||
	EQUALS      // == or !=
	LESSGREATER // > or < or <= or >=
	WEAK_COMMA  // a kludge to let me use Go-like syntax in function definitions


	FPREFIX     // user-defined prefix or function
	PLUSPLUS    // to make the ++ operator weaker than ::
	FMIDFIX     // user-defined midfix or forefix
	FENDFIX     // user-defined endfix
	COMMA       // ,
	FINFIX      // user-defined infix
	SUM         // + or -	
	PRODUCT     // * or / or %
	FSUFFIX     // user-defined suffix, or type in type declaration
	PREFIX      // -X or not X , the "native prefixes"
	
	INDEX       // after [
	
)

var precedences = map[token.TokenType]int{
	token.GIVEN:	GIVEN,
	token.RETURN:	FUNC,
	token.FUNC:		FUNC,
	token.SEMICOLON: SEMICOLON,
	token.NEWLINE: SEMICOLON,
	token.ASSIGN:	ASSIGN,
	token.CMD_ASSIGN: ASSIGN,
	token.VAR_ASSIGN: ASSIGN,
	token.DEF_ASSIGN: ASSIGN,
	token.GVN_ASSIGN: ASSIGN,
	token.TYP_ASSIGN: ASSIGN,
	token.PVR_ASSIGN: ASSIGN,
	token.COLON:    COLON,
	token.OR:		OR,
	token.AND:		AND,
	token.EQ:       EQUALS,
	token.NOT_EQ:   EQUALS,
	token.WEAK_COMMA : WEAK_COMMA,
	token.COMMA:	COMMA,
	token.LBRACK:	INDEX,
	token.NOT:		PREFIX,
	token.EVAL:		FPREFIX,
}

type TokenSupplier interface{NextToken() token.Token}

func String(t TokenSupplier) string {
	result := ""
	for tok := t.NextToken(); tok.Type != "EOF"; tok = t.NextToken() {
		 result = result + fmt.Sprintf("%+v\n", tok)
	}
	return result
}

type Parser struct {
	TokenizedCode      TokenSupplier
		
	Errors text.Errors

	curToken  token.Token
	peekToken token.Token

	Functions set.Set[string]
	Prefixes set.Set[string]
	Forefixes set.Set[string]
	Midfixes set.Set[string]
	Endfixes set.Set[string]
	Infixes set.Set[string]
	Suffixes set.Set[string]
	Unfixes set.Set[string]
	AllFunctionIdents set.Set[string]

	nativeInfixes  set.Set[token.TokenType]
	lazyInfixes  set.Set[token.TokenType]

	FunctionTable FunctionTable

	Globals *object.Environment
	TypeSystem TypeSystem

	BuiltinFunctions map[string] func(args ...object.Object) object.Object
}

func New() *Parser {
	p := &Parser{
		Errors: text.Errors{},
		Functions: make(set.Set[string]),
		Prefixes: make(set.Set[string]),
		Forefixes: *set.MakeFromSlice([]string{"having"}),
		Midfixes: make(set.Set[string]),
		Endfixes: make(set.Set[string]),
		Infixes: make(set.Set[string]),
		Suffixes: make(set.Set[string]),
		Unfixes: make(set.Set[string]),
		AllFunctionIdents:  make(set.Set[string]),
		nativeInfixes: *set.MakeFromSlice([]token.TokenType{
			token.COMMA, token.EQ, token.NOT_EQ, token.WEAK_COMMA, 
			token.ASSIGN, token.DEF_ASSIGN, token.CMD_ASSIGN, token.PVR_ASSIGN,
			token.VAR_ASSIGN, token.GVN_ASSIGN, token.TYP_ASSIGN, token.GIVEN, token.LBRACK}),
		lazyInfixes: *set.MakeFromSlice([]token.TokenType{token.AND,
				token.OR, token.COLON, token.SEMICOLON, token.NEWLINE}),
		FunctionTable : make(FunctionTable),
		Globals : object.NewEnvironment(), // I need my functions to be able to see the global constants.
		TypeSystem : NewTypeSystem(),
	}
		
	for k, _ := range *p.TypeSystem {
		p.Suffixes.Add(k)
	}

	p.Functions.AddSet(*set.MakeFromSlice([]string{"builtin"}))

	// The parser adds constructors for structs to the builtins and so must keep its own
	// collection of them.
	p.BuiltinFunctions = make(map[string] func(args ...object.Object) object.Object)

	for k, v := range Builtins {
		p.BuiltinFunctions[k] = v
	}

	return p
}

func (p *Parser) NextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.TokenizedCode.NextToken()
}

func (p *Parser) curTokenIs(t token.TokenType) bool {
	return p.curToken.Type == t
}

func (p *Parser) peekTokenIs(t token.TokenType) bool {
	return p.peekToken.Type == t
}

func (p *Parser) expectPeek(t token.TokenType) bool {
	if p.peekTokenIs(t) {
		p.NextToken()
		return true
	} else {
		p.peekError(token.Token{Type : t, Line: p.curToken.Line})
		return false
	}
}

func (p *Parser) peekError(t token.Token) {
	msg := fmt.Sprintf("expected next token to be %s, got %s instead",
		text.Emph(string(t.Type)), text.Emph(string(p.peekToken.Type)))
	p.Errors = append(p.Errors, text.Err{Msg: msg, Line: t.Line})
}

func (p *Parser) noPrefixParseFnError(t token.Token) {
	msg := fmt.Sprintf("can't parse %s as a prefix.", text.Emph(string(t.Type)))
	p.Errors = append(p.Errors, text.Err{Msg: msg, Line: t.Line})
}

func (p *Parser) prefixSuffixError() {
	msg := fmt.Sprintf("you can't put %s before %s",text.Emph(string(string(p.curToken.Type))), 
		text.Emph(string(p.peekToken.Type)))
	p.Errors = append(p.Errors, text.Err{Msg: msg, Line: p.curToken.Line})
}


func (p *Parser) ParseTokenizedChunk() *ast.Node {
	p.NextToken()
	p.NextToken()
	expn := p.parseExpression(LOWEST)
	return &expn
}


var literals = *set.MakeFromSlice([]token.TokenType{token.INT, token.FLOAT, token.STRING, token.TRUE, token.FALSE, token.ELSE})
var literalsAndLParen = *set.MakeFromSlice([]token.TokenType{token.INT, token.FLOAT, token.STRING, token.TRUE, token.FALSE, token.ELSE, 
	token.LPAREN, token.LBRACE, token.EVAL, token.FUNC})
var assignmentTokens = *set.MakeFromSlice([]token.TokenType{token.ASSIGN, token.VAR_ASSIGN, token.DEF_ASSIGN, 
	/**/ token.CMD_ASSIGN, token.GVN_ASSIGN, token.PVR_ASSIGN, token.TYP_ASSIGN})

func (p *Parser) parseExpression(precedence int) ast.Node {
	
	 if literals.Contains(p.curToken.Type) && literalsAndLParen.Contains(p.peekToken.Type) {
	 		p.prefixSuffixError()
	 	}
	var leftExp ast.Node 
	noNativePrefix := false

	switch p.curToken.Type {
		case token.INT : leftExp = p.parseIntegerLiteral()
		case token.FLOAT : leftExp = p.parseFloatLiteral()
		case token.STRING : leftExp = p.parseStringLiteral()
		case token.NOT : leftExp = p.parseNativePrefixExpression()
		case token.EVAL : leftExp = p.parsePrefixExpression()
		case token.TRUE : leftExp = p.parseBoolean()
		case token.FALSE : leftExp = p.parseBoolean()
		case token.ELSE : leftExp = p.parseElse()
		case token.LPAREN : leftExp = p.parseGroupedExpression()
		case token.LBRACK : leftExp = p.parseListExpression()
		case token.LBRACE : leftExp = p.parseSetExpression()
		case token.FUNC : leftExp = p.parseFuncExpression()
		case token.RETURN : leftExp = p.parseReturnExpression()
		default : 
		if p.curToken.Type == token.IDENT {
			if p.curToken.Literal == "-" {
				leftExp = p.parseNativePrefixExpression()
			} else { noNativePrefix = true }
		}
	}

	// So what we're going to do is find out if the identifier *thinks* it's a function, i.e. if it precedes
	// something that's a prefix (in the broader sense, i.e. an identifier, literal, LPAREN, etc). But not a
	// minus sign, that would be confusing, people can use parentheses.
	// If so, then we will parse it as though it's a Function, and it had better turn out to be a lambda at
	// runtime. If it isn't, then we'll treat it as an identifier.
	if noNativePrefix {
		if p.curToken.Type == token.IDENT {
			if p.curToken.Literal == "builtin" {
				leftExp = p.parseBuiltInExpression()
				return leftExp
			}

			if p.curToken.Literal == "struct" { 
				leftExp = p.parseStructExpression()
				return leftExp
			}
			// Here we step in and deal with things that are functions and objects, like the type conversion
			// functions and their associated types. Before we look them up as functions, we want to
			// be sure that they're not in such a position that they're being used as literals.
			if !p.positionallyFunctional() {
				if TypeExists(p.curToken.Literal, p.TypeSystem) {
					leftExp = &ast.TypeLiteral{Token: p.curToken, Value: p.curToken.Literal}
				} else {
					if p.Unfixes.Contains(p.curToken.Literal) {
						leftExp = p.parseUnfixExpression()
					} else {
						leftExp = p.parseIdentifier()
					}
				}
			} else {
				switch {
				case p.Prefixes.Contains(p.curToken.Literal) || p.Forefixes.Contains(p.curToken.Literal) :
					leftExp = p.parsePrefixExpression()
				default :
					leftExp = p.parseFunctionExpression() // That, at least, is what it is syntactictally.
				} 
			}
		} else {
			p.noPrefixParseFnError(p.curToken)
		}
	}
	for precedence < p.peekPrecedence() {
		for p.Suffixes.Contains(p.peekToken.Literal) || p.Endfixes.Contains(p.peekToken.Literal) {
			if p.curToken.Type == token.NOT|| p.curToken.Type == token.IDENT && p.curToken.Literal == "-" || p.curToken.Type == token.ELSE {
				p.prefixSuffixError()
				return nil
			}
			p.NextToken()
			leftExp = p.parseSuffixExpression(leftExp)
		}

		if precedence >= p.peekPrecedence() {break}
	
		foundInfix := p.nativeInfixes.Contains(p.peekToken.Type) ||
			 p.lazyInfixes.Contains(p.peekToken.Type) ||
		     p.Infixes.Contains(p.peekToken.Literal) || 
			 p.Midfixes.Contains(p.peekToken.Literal)
		if !foundInfix {
			return leftExp
		}

		p.NextToken()
		
		if foundInfix {
			if p.lazyInfixes.Contains(p.curToken.Type) {
				leftExp = p.parseLazyInfixExpression(leftExp)
			} else {
				if p.curToken.Type == token.LBRACK {
					leftExp = p.parseIndexExpression(leftExp)
				} else {
					leftExp = p.parseInfixExpression(leftExp)
				}
			}
		}
	}

	return leftExp
}

func (p *Parser) positionallyFunctional() bool {
	if assignmentTokens.Contains(p.peekToken.Type) {return false}
	if p.curToken.Literal == "type" && TypeExists(p.peekToken.Literal, p.TypeSystem) { return true }
	if p.Functions.Contains(p.curToken.Literal) && ! TypeExists(p.curToken.Literal, p.TypeSystem) && 
	/**/ p.peekToken.Type != token.EOF { return true }
	if p.Prefixes.Contains(p.curToken.Literal){ return p.peekToken.Type != token.EOF }
	if literalsAndLParen.Contains(p.peekToken.Type) { return true }
	if p.peekToken.Type != token.IDENT { return false }
	if p.Infixes.Contains(p.peekToken.Literal) { return false }
	if p.Midfixes.Contains(p.peekToken.Literal) { return false }
	if p.Endfixes.Contains(p.peekToken.Literal) { return false }
	if p.Suffixes.Contains(p.peekToken.Literal) { return false }
	return true
}

func (p *Parser) peekPrecedence() int {
	if p, ok := precedences[p.peekToken.Type]; ok {
		return p
	}
	if p.Infixes.Contains(p.peekToken.Literal) {
		if p.peekToken.Literal == "+" || p.peekToken.Literal == "-" {
			return SUM
		}
		if p.peekToken.Literal == "*" || p.peekToken.Literal == "/"  || p.peekToken.Literal == "%" {
			return PRODUCT
		}
		if p.peekToken.Literal == "<" || p.peekToken.Literal == "<="  || p.peekToken.Literal == ">" || p.peekToken.Literal == ">=" {
			return LESSGREATER
		}
		if p.peekToken.Literal == "in" {
			return EQUALS
		}
		if p.peekToken.Literal == "++" {
			return PLUSPLUS
		}
		return FINFIX
	}
	if p.Prefixes.Contains(p.peekToken.Literal) || p.Functions.Contains(p.peekToken.Literal) {
		return FPREFIX
	}
	if p.Midfixes.Contains(p.peekToken.Literal) || p.Forefixes.Contains(p.peekToken.Literal) {
		return FMIDFIX
	}
	if p.Endfixes.Contains(p.peekToken.Literal) {
		return FENDFIX
	}
	if p.Suffixes.Contains(p.peekToken.Literal) {
		return FSUFFIX
	}
	return LOWEST
}

func (p *Parser) curPrecedence() int {
	if p, ok := precedences[p.curToken.Type]; ok {
		return p
	}

	if p.curToken.Type == token.IDENT {
		if p.Infixes.Contains(p.curToken.Literal) {
			if p.curToken.Literal == "+" || p.curToken.Literal == "-" {
				return SUM
			}
			if p.curToken.Literal == "*" || p.curToken.Literal == "/"  || p.curToken.Literal == "%" {
				return PRODUCT
			}
			if p.curToken.Literal == "<" || p.curToken.Literal == "<="  || p.curToken.Literal == ">" || p.curToken.Literal == ">=" {
				return LESSGREATER
			}
			if p.curToken.Literal == "in" {
				return EQUALS
			}
			if p.curToken.Literal == "++" {
				return PLUSPLUS
			}
			return FINFIX
		}
		if p.Prefixes.Contains(p.curToken.Literal) || p.Functions.Contains(p.curToken.Literal) {
			return FPREFIX
		}
		if p.Midfixes.Contains(p.curToken.Literal)  || p.Forefixes.Contains(p.peekToken.Literal) {
			return FMIDFIX
		}
		if p.Endfixes.Contains(p.curToken.Literal) {
			return FENDFIX
		}
		if p.Suffixes.Contains(p.curToken.Literal) {
			return FSUFFIX
		}
	}

	return LOWEST
}

func (p *Parser) parseIndexExpression(left ast.Node) ast.Node {
	exp := &ast.IndexExpression{Token: p.curToken, Left: left}

	p.NextToken()
	exp.Index = p.parseExpression(LOWEST)

	if !p.expectPeek(token.RBRACK) {
		p.MakeError("malformed index expression, unclosed bracket", p.curToken.Line)
		return nil
	}
	return exp
}

func (p *Parser) parseIdentifier() ast.Node {
	return &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseUnfixExpression() ast.Node {
	return &ast.UnfixExpression{Token: p.curToken,  Operator: p.curToken.Literal}
}

func (p *Parser) parseIntegerLiteral() ast.Node {
	lit := &ast.IntegerLiteral{Token: p.curToken}
	value, e := strconv.ParseInt(p.curToken.Literal, 0, 64)
	if e != nil {
		msg := fmt.Sprintf("couldn't parse %q as integer.", p.curToken.Literal)
		p.Errors = append(p.Errors, text.Err{Msg : msg , Line : p.curToken.Line})
		return nil
	}

	lit.Value = value
	return lit
}

func (p *Parser) parseFloatLiteral() ast.Node {
	lit := &ast.FloatLiteral{Token: p.curToken}
	value, e := strconv.ParseFloat(p.curToken.Literal, 64)
	if e != nil {
		msg := fmt.Sprintf("couldn't parse %q as float.", p.curToken.Literal)
		p.Errors = append(p.Errors, text.Err{Msg : msg , Line : p.curToken.Line})
		return nil
	}

	lit.Value = value
	return lit
}

func (p *Parser) parseStringLiteral() ast.Node {
	return &ast.StringLiteral{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseNativePrefixExpression() ast.Node {
	expression := &ast.PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}

	p.NextToken()
	expression.Right = p.parseExpression(PREFIX)

	return expression
}

func (p *Parser) parsePrefixExpression() ast.Node {
	expression := &ast.PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}
	p.NextToken()
	expression.Right = p.parseExpression(FPREFIX)
	return expression
}

func (p *Parser) parseStructExpression() ast.Node {
	expression := &ast.StructExpression{
		Token:    p.curToken,
	}

	p.NextToken()
	sigtree := p.parseExpression(FPREFIX)
	expression.Sig = p.RecursivelySlurpSignature(sigtree, "any")
	return expression
}

// This is to allow me to use the initializer to pour builtins into the parser's function table.
func (p *Parser) parseBuiltInExpression() ast.Node {
	expression := &ast.BuiltInExpression{}
	p.NextToken()
	if p.curToken.Type == token.STRING {
		expression.Name = p.curToken.Literal
	} else {
		p.Errors = append(p.Errors, text.Err{Msg: "A builtin declaration should be followed by a string literal", Line: -1})
	}
	p.NextToken()
	return expression
}


func (p *Parser) parseFunctionExpression() ast.Node {
	expression := &ast.PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}

	p.NextToken()
	if p.curToken.Type == token.LPAREN {
		expression.Right = p.parseExpression(PREFIX)
	} else {
		expression.Right = p.parseExpression(FPREFIX)
    }

	return expression
}


func (p *Parser) parseSuffixExpression(left ast.Node) ast.Node {
	expression := &ast.SuffixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Left: left,	
	}
	return expression
}


func (p *Parser) parseInfixExpression(left ast.Node) ast.Node {

	if p.curToken.Type == token.ASSIGN || p.curToken.Type == token.CMD_ASSIGN ||
	   p.curToken.Type == token.VAR_ASSIGN || p.curToken.Type == token.DEF_ASSIGN ||
	   p.curToken.Type == token.GVN_ASSIGN || p.curToken.Type == token.PVR_ASSIGN ||
	   p.curToken.Type == token.TYP_ASSIGN { 
		   return p.parseAssignmentExpression(left) 
		}
	
	expression := &ast.InfixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Left:     left,
	}
	precedence := p.curPrecedence()
	p.NextToken()
	expression.Right = p.parseExpression(precedence)
	return expression

}

func (p *Parser) parseAssignmentExpression(left ast.Node) ast.Node {	
	expression := &ast.AssignmentExpression {
		Token:    p.curToken,
		Left:     left,
	}
	precedence := p.curPrecedence()
	p.NextToken()
	expression.Right = p.parseExpression(precedence)
	return expression
}


// The only difference between this and parseInfixExpression is the return type,
// which will later be used to help the evaluator.
func (p *Parser) parseLazyInfixExpression(left ast.Node) ast.Node {
	expression := &ast.LazyInfixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Left:     left,
	}
	precedence := p.curPrecedence()
	p.NextToken()
	expression.Right = p.parseExpression(precedence)
	return expression
}

func (p *Parser) parseBoolean() ast.Node {
	return &ast.BooleanLiteral{Token: p.curToken, Value: p.curTokenIs(token.TRUE)}
}

// in Charm else is in fact syntactic sugar for true
func (p *Parser) parseElse() ast.Node {
	return &ast.BooleanLiteral{Token: p.curToken, Value: true}
}

func (p *Parser) parseGroupedExpression() ast.Node {
	p.NextToken()
	exp := p.parseExpression(LOWEST)
	if !p.expectPeek(token.RPAREN) {
		p.MakeError("malformed expression, unclosed parenthesis or indent", p.curToken.Line)
		return nil
	}
	return exp
}

func (p *Parser) parseReturnExpression() ast.Node {
	p.NextToken()
	expression := &ast.PrefixExpression{
		Token: p.curToken,
		Operator: token.RETURN,
		Right: p.parseExpression(FUNC),
	}
	return expression
}


func (p *Parser) parseFuncExpression() ast.Node {
	expression := &ast.FuncExpression{
		Token:    p.curToken,
	}
	p.NextToken()
	RHS := p.parseExpression(FUNC)
	// At this point the root of the RHS should be a GIVEN or a COLON or who knows what's 
	// happened?
	root := RHS
	switch RHS := RHS.(type) {
	case *ast.InfixExpression :
		if RHS.Token.Type == token.GIVEN {
			root = RHS.Left
			expression.Given = RHS.Right
		}
	}
	switch root := root.(type) {
	case *ast.LazyInfixExpression :
		if root.Token.Type != token.COLON {
			p.MakeError(text.Emph("func") + " expression should have a colon", p.curToken.Line)
			return nil
		}
		expression.Sig = p.RecursivelySlurpSignature(root.Left, "single")
		expression.Body = root.Right
		return expression
	default : 
		p.MakeError("malformed " +text.Emph("func") + " expression", p.curToken.Line)	
		return nil
	}
}

func (p *Parser) parseListExpression() ast.Node {
	p.NextToken()
	if p.curToken.Type == token.RBRACK {
		return &ast.ListExpression{List: nil, Token: p.curToken}
	}
	exp := p.parseExpression(LOWEST)
	if !p.expectPeek(token.RBRACK) {
		p.MakeError("malformed list expression, unclosed bracket", p.curToken.Line)
		return nil
	}
	expression := &ast.ListExpression{List:	exp, Token: p.curToken}
	return expression
}

func (p *Parser) parseSetExpression() ast.Node {
	p.NextToken()
	if p.curToken.Type == token.RBRACE {
		return &ast.SetExpression{Set: nil, Token: p.curToken}
	}
	exp := p.parseExpression(LOWEST)
	if !p.expectPeek(token.RBRACE) {
		p.MakeError("malformed set expression, unclosed brace", p.curToken.Line)
		return nil
	}
	expression := &ast.SetExpression{Set:	exp, Token: p.curToken}
	return expression
}

func (p *Parser) ParseLine(source, input string) *ast.Node {
	p.TokenizedCode = relexer.New(source, input)
	return p.ParseTokenizedChunk()
}

func (p *Parser) ParseDump(source, input string) {
	parsedLine := p.ParseLine(source, input)
	fmt.Printf("Parser returns: %v\n\n", (*parsedLine).String())
}

func (p *Parser) MakeError(msg string, line int) {
	p.Errors = append(p.Errors, text.Err{Msg: msg, Line: line})
}

func (p *Parser) ErrorsExist() bool {
	return len(p.Errors) > 0
}

func (p *Parser)  ReturnErrors() string {
	return p.Errors.String()
}

func (p *Parser)  ClearErrors() {
	p.Errors = text.Errors{}
}

func (p *Parser) RecursivelySlurpSignature(node ast.Node, dflt string) signature.NamedSignature {
    switch typednode := node.(type) {
    case *ast.Identifier :
        return signature.NamedSignature{signature.NameTypePair{VarName: typednode.Value, VarType: dflt}}
    case *ast.PrefixExpression :
        switch {
        case p.Forefixes.Contains(typednode.Operator):
            RHS := p.RecursivelySlurpSignature(typednode.Right, dflt)
            front := signature.NamedSignature{signature.NameTypePair{VarName: typednode.Operator, VarType: "bling"}}
            return append(front, RHS...)
        default :
            p.Errors = append(p.Errors, text.Err{Msg: "unexpected occurrence of " + text.Emph(typednode.Operator), Line: typednode.Token.Line})
        }
    case *ast.InfixExpression :
        switch {
        case p.Midfixes.Contains(typednode.Operator):
            LHS := p.RecursivelySlurpSignature(typednode.Left, dflt)
            RHS := p.RecursivelySlurpSignature(typednode.Right, dflt)
            middle := signature.NameTypePair{VarName: typednode.Operator, VarType: "bling"}
            return append(append(LHS, middle), RHS...) 
        case typednode.Token.Type == token.COMMA || typednode.Token.Type == token.WEAK_COMMA:
            LHS := p.RecursivelySlurpSignature(typednode.Left, dflt)
            RHS := p.RecursivelySlurpSignature(typednode.Right, dflt)
            return append(LHS, RHS...)
        default :
            p.Errors = append(p.Errors, text.Err{Msg: "unexpected occurrence of " + text.Emph(typednode.Operator), Line: typednode.Token.Line})        }
    case *ast.SuffixExpression :
        switch {
        case TypeExists(typednode.Operator, p.TypeSystem):
            LHS := p.RecursivelySlurpSignature(typednode.Left, dflt)
            for k, _ := range(LHS) {
                LHS[k].VarType = typednode.Operator
            }
            return LHS
        case p.Endfixes.Contains(typednode.Operator):
            LHS := p.RecursivelySlurpSignature(typednode.Left, dflt)
            end := signature.NameTypePair{VarName: typednode.Operator, VarType: "bling"}
            return append(LHS, end)
        default :
            p.Errors = append(p.Errors, text.Err{Msg: "unexpected occurrence of " + text.Emph(typednode.Operator), Line: typednode.Token.Line})
        }
    default : p.Errors = append(p.Errors, text.Err{Msg: "One of your function signatures is just too weird for Charm to handle at present", Line: -1})
	}
    return nil
}

// Variable assumed to exist.
func (p *Parser) CanHold(e *object.Environment, name string, ty string) bool {
	v, ok := e.Store[name]
	if ok {
		return IsSameTypeOrSubtype(p.TypeSystem, v.VarType, ty) 
	}
	return p.CanHold(e, name, ty)
}

func (p *Parser) ExtractVariables(T TokenSupplier) (set.Set[string], set.Set[string]) {
	LHS := make(set.Set[string])
	RHS := make(set.Set[string])
	assignHasHappened := false
	for tok:= T.NextToken(); tok.Type != token.EOF; tok = T.NextToken() {
		if tok.Type == token.IDENT && 
			/**/ ! p.AllFunctionIdents.Contains(tok.Literal) {
			if assignHasHappened {RHS.Add(tok.Literal)} else {LHS.Add(tok.Literal)}
		}
		if tok.Type == token.ASSIGN || tok.Type == token.DEF_ASSIGN ||
		   tok.Type == token.VAR_ASSIGN || tok.Type == token.CMD_ASSIGN ||
		   tok.Type == token.PVR_ASSIGN {
			   assignHasHappened = true
			}

	}
	return LHS, RHS
}