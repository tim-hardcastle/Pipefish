package parser

import (
	"fmt"
	"strconv"

	"charm/ast"
	"charm/object"
	"charm/relexer"
	"charm/set"
	"charm/signature"
	"charm/stack"
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
	OR          // ||
	AND         // &&
	EQUALS      // == or !=
	LESSGREATER // > or < or <= or >=
	WEAK_COMMA  // a kludge to let me use Go-like syntax in function definitions --- change to FMIDFIX?

	FPREFIX // user-defined prefix or function
	FMIDFIX // user-defined midfix or forefix
	FENDFIX // user-defined endfix
	COMMA   // ,
	FINFIX  // user-defined infix or ->
	SUM     // + or -
	PRODUCT // * or / or %
	FSUFFIX // user-defined suffix, or type in type declaration
	PREFIX  // -X or not X , the "native prefixes"

	INDEX // after [

)

var precedences = map[token.TokenType]int{
	token.GIVEN:       GIVEN,
	token.EXEC:        FUNC,
	token.RETURN:      FUNC,
	token.SEMICOLON:   SEMICOLON,
	token.NEWLINE:     SEMICOLON,
	token.ASSIGN:      ASSIGN,
	token.CMD_ASSIGN:  ASSIGN,
	token.VAR_ASSIGN:  ASSIGN,
	token.DEF_ASSIGN:  ASSIGN,
	token.GVN_ASSIGN:  ASSIGN,
	token.TYP_ASSIGN:  ASSIGN,
	token.PVR_ASSIGN:  ASSIGN,
	token.COLON:       COLON,
	token.MAGIC_COLON: COLON,
	token.OR:          OR,
	token.AND:         AND,
	token.EQ:          EQUALS,
	token.NOT_EQ:      EQUALS,
	token.WEAK_COMMA:  WEAK_COMMA,
	token.COMMA:       COMMA,
	token.LBRACK:      INDEX,
	token.NOT:         PREFIX,
	token.EVAL:        FPREFIX,
	token.RIGHTARROW:  OR,
}

type TokenSupplier interface{ NextToken() token.Token }

func String(t TokenSupplier) string {
	result := ""
	for tok := t.NextToken(); tok.Type != "EOF"; tok = t.NextToken() {
		result = result + fmt.Sprintf("%+v\n", tok)
	}
	return result
}

type Parser struct {

	// Temporary state: things that are used to parse one line.

	TokenizedCode TokenSupplier
	Errors        object.Errors
	nesting       stack.Stack[token.Token]
	curToken      token.Token
	peekToken     token.Token

	// Permanent state: things set up by the initializer which are
	// then constant for the lifetime of the service.

	Functions         set.Set[string]
	Prefixes          set.Set[string]
	Forefixes         set.Set[string]
	Midfixes          set.Set[string]
	Endfixes          set.Set[string]
	Infixes           set.Set[string]
	Suffixes          set.Set[string]
	Unfixes           set.Set[string]
	AllFunctionIdents set.Set[string]

	nativeInfixes set.Set[token.TokenType]
	lazyInfixes   set.Set[token.TokenType]

	FunctionTable FunctionTable
	FunctionTreeMap map[string]*ast.FnTreeNode

	Globals    *object.Environment
	TypeSystem TypeSystem

	BuiltinFunctions map[string]func(p *Parser, args ...object.Object) object.Object

	Enums map[string][]*object.Label

	Structs set.Set[string]

	Parsers map[string]*Parser

	GolangImports []string

	Namespace string

	Namespaces map[string]string
}

func New() *Parser {
	p := &Parser{
		Errors:            []*object.Error{},
		nesting:           *stack.NewStack[token.Token](),
		Functions:         make(set.Set[string]),
		Prefixes:          make(set.Set[string]),
		Forefixes:         make(set.Set[string]),
		Midfixes:          make(set.Set[string]),
		Endfixes:          make(set.Set[string]),
		Infixes:           make(set.Set[string]),
		Suffixes:          make(set.Set[string]),
		Unfixes:           make(set.Set[string]),
		AllFunctionIdents: make(set.Set[string]),
		nativeInfixes: *set.MakeFromSlice([]token.TokenType{
			token.RIGHTARROW, token.COMMA, token.EQ, token.NOT_EQ, token.WEAK_COMMA,
			token.ASSIGN, token.DEF_ASSIGN, token.CMD_ASSIGN, token.PVR_ASSIGN,
			token.VAR_ASSIGN, token.GVN_ASSIGN, token.TYP_ASSIGN, token.GIVEN, token.EXEC, token.LBRACK, token.MAGIC_COLON}),
		lazyInfixes: *set.MakeFromSlice([]token.TokenType{token.AND,
			token.OR, token.COLON, token.SEMICOLON, token.NEWLINE}),
		FunctionTable: make(FunctionTable),
		FunctionTreeMap: make(map[string]*ast.FnTreeNode),
		Globals:       object.NewEnvironment(), // I need my functions to be able to see the global constants.
		TypeSystem:    NewTypeSystem(),
		Structs:       make(set.Set[string]),
		GolangImports: []string{},
		Namespaces:    make(map[string]string),
	}

	for k := range *p.TypeSystem {
		p.Suffixes.Add(k)
	}

	p.Suffixes.Add("raw")
	p.Suffixes.Add("ast")
	p.Suffixes.Add("varname")

	p.Functions.AddSet(*set.MakeFromSlice([]string{"builtin"}))

	// The parser adds constructors for structs to the builtins and so must keep its own
	// collection of them.
	p.BuiltinFunctions = make(map[string]func(p *Parser, args ...object.Object) object.Object)

	for k, v := range Builtins {
		p.BuiltinFunctions[k] = v
	}

	p.Enums = make(map[string][]*object.Label)

	p.Parsers = make(map[string]*Parser)

	return p
}

func (p *Parser) NextToken() {
	p.checkNesting()
	p.SafeNextToken()
	// if p.curToken.Source != "builtin library" {
	//	fmt.Println(p.curToken)

	//}
}

func (p *Parser) SafeNextToken() {
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
	}
	return false
}

func (p *Parser) peekError(t token.Token) {
	p.Throw("parse/expect", t, p.peekToken)
}

func (p *Parser) noPrefixParseFnError(t token.Token) {
	p.Throw("parse/prefix", t)
}

func (p *Parser) prefixSuffixError() {
	p.Throw("parse/before", p.curToken, p.peekToken)
}

func (p *Parser) ParseTokenizedChunk() *ast.Node {
	p.nesting = *stack.NewStack[token.Token]()
	p.SafeNextToken()
	p.SafeNextToken()
	expn := p.parseExpression(LOWEST)
	p.NextToken()
	if p.curToken.Type != token.EOF {
		p.Throw("parse/expected", p.curToken)
	}
	return &expn
}

var literals = *set.MakeFromSlice([]token.TokenType{token.INT, token.FLOAT, token.STRING, token.TRUE, token.FALSE, token.ELSE})
var literalsAndLParen = *set.MakeFromSlice([]token.TokenType{token.INT, token.FLOAT, token.STRING, token.TRUE, token.FALSE, token.ELSE,
	token.LPAREN, token.LBRACE, token.EVAL})
var assignmentTokens = *set.MakeFromSlice([]token.TokenType{token.ASSIGN, token.VAR_ASSIGN, token.DEF_ASSIGN,
	token.CMD_ASSIGN, token.GVN_ASSIGN, token.PVR_ASSIGN, token.TYP_ASSIGN})

func (p *Parser) parseExpression(precedence int) ast.Node {

	if literals.Contains(p.curToken.Type) && literalsAndLParen.Contains(p.peekToken.Type) {
		p.prefixSuffixError()
	}
	var leftExp ast.Node
	noNativePrefix := false

	switch p.curToken.Type {
	case token.INT:
		leftExp = p.parseIntegerLiteral()
	case token.FLOAT:
		leftExp = p.parseFloatLiteral()
	case token.STRING:
		leftExp = p.parseStringLiteral()
	case token.NOT:
		leftExp = p.parseNativePrefixExpression()
	case token.EVAL:
		leftExp = p.parsePrefixExpression()
	case token.TRUE:
		leftExp = p.parseBoolean()
	case token.FALSE:
		leftExp = p.parseBoolean()
	case token.ELSE:
		leftExp = p.parseElse()
	case token.LPAREN:
		leftExp = p.parseGroupedExpression()
	case token.LBRACK:
		leftExp = p.parseListExpression()
	case token.LBRACE:
		leftExp = p.parseSetExpression()
	case token.RETURN:
		leftExp = p.parseReturnExpression()
	case token.GOLANG:
		leftExp = p.parseGolangExpression()
	default:
		noNativePrefix = true
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

			if p.curToken.Literal == "func" {
				leftExp = p.parseFuncExpression()
				return leftExp
			}

			if p.curToken.Literal == "code" {
				leftExp = p.parseCodeLiteral()
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
				case p.Prefixes.Contains(p.curToken.Literal) || p.Forefixes.Contains(p.curToken.Literal):
					leftExp = p.parsePrefixExpression()
				default:
					leftExp = p.parseFunctionExpression() // That, at least, is what it is syntactictally.
				}
			}
		} else {
			p.noPrefixParseFnError(p.curToken)
		}
	}

	for precedence < p.peekPrecedence() {
		for p.Suffixes.Contains(p.peekToken.Literal) || p.Endfixes.Contains(p.peekToken.Literal) {
			if p.curToken.Type == token.NOT || p.curToken.Type == token.IDENT && p.curToken.Literal == "-" || p.curToken.Type == token.ELSE {
				p.prefixSuffixError()
				return nil
			}
			p.NextToken()
			leftExp = p.parseSuffixExpression(leftExp)
		}

		if precedence >= p.peekPrecedence() {
			break
		}

		foundInfix := p.nativeInfixes.Contains(p.peekToken.Type) ||
			p.lazyInfixes.Contains(p.peekToken.Type) ||
			p.Infixes.Contains(p.peekToken.Literal) ||
			p.Midfixes.Contains(p.peekToken.Literal)
		if !foundInfix {
			return leftExp
		}
		p.NextToken()

		if foundInfix {
			switch {
			case p.lazyInfixes.Contains(p.curToken.Type):
				leftExp = p.parseLazyInfixExpression(leftExp)
			case p.curToken.Type == token.EXEC:
				leftExp = p.parseExecExpression(leftExp)
			case p.curToken.Type == token.LBRACK:
				leftExp = p.parseIndexExpression(leftExp)
			default:
				leftExp = p.parseInfixExpression(leftExp)
			}
		}
	}
	if leftExp == nil {
		if p.curToken.Type == token.EOF {
			p.Throw("parse/line", p.curToken)
			return nil
		}
		if p.curToken.Literal == "<-|" || p.curToken.Literal == ")" ||
			p.curToken.Literal == "]" || p.curToken.Literal == "}" {
			p.Throw("parse/close", p.curToken)
			return nil
		}
		p.Throw("parse/missing", p.curToken)
		return nil
	}
	return leftExp
}

func (p *Parser) positionallyFunctional() bool {
	if assignmentTokens.Contains(p.peekToken.Type) {
		return false
	}
	if p.curToken.Literal == "type" && TypeExists(p.peekToken.Literal, p.TypeSystem) {
		return true
	}
	if p.Functions.Contains(p.curToken.Literal) && !TypeExists(p.curToken.Literal, p.TypeSystem) &&
		p.peekToken.Type != token.EOF {
		return true
	}
	if p.Prefixes.Contains(p.curToken.Literal) {
		return p.peekToken.Type != token.EOF
	}
	if literalsAndLParen.Contains(p.peekToken.Type) {
		return true
	}
	if p.peekToken.Type != token.IDENT {
		return false
	}
	if p.Infixes.Contains(p.peekToken.Literal) {
		return false
	}
	if p.Midfixes.Contains(p.peekToken.Literal) {
		return false
	}
	if p.Endfixes.Contains(p.peekToken.Literal) {
		return false
	}
	if p.Suffixes.Contains(p.peekToken.Literal) {
		return false
	}
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
		if p.peekToken.Literal == "*" || p.peekToken.Literal == "/" || p.peekToken.Literal == "%" {
			return PRODUCT
		}
		if p.peekToken.Literal == "<" || p.peekToken.Literal == "<=" || p.peekToken.Literal == ">" || p.peekToken.Literal == ">=" {
			return LESSGREATER
		}
		if p.peekToken.Literal == "in" {
			return EQUALS
		}
		if p.peekToken.Literal == "with" {
			return FMIDFIX
		}
		return FINFIX
	}
	if p.Prefixes.Contains(p.peekToken.Literal) || p.Functions.Contains(p.peekToken.Literal) {
		if p.peekToken.Literal == "func" {
			return FUNC
		}
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
			if p.curToken.Literal == "*" || p.curToken.Literal == "/" || p.curToken.Literal == "%" {
				return PRODUCT
			}
			if p.curToken.Literal == "<" || p.curToken.Literal == "<=" || p.curToken.Literal == ">" || p.curToken.Literal == ">=" {
				return LESSGREATER
			}
			if p.curToken.Literal == "in" {
				return EQUALS
			}
			if p.curToken.Literal == "with" {
				return FMIDFIX
			}
			return FINFIX
		}
		if p.Prefixes.Contains(p.curToken.Literal) || p.Functions.Contains(p.curToken.Literal) {
			if p.curToken.Literal == "func" {
				return FUNC
			}
			return FPREFIX
		}
		if p.Midfixes.Contains(p.curToken.Literal) || p.Forefixes.Contains(p.peekToken.Literal) {
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
		p.NextToken() // Forces emission of error
		return nil
	}
	return exp
}

func (p *Parser) parseIdentifier() ast.Node {
	return &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseUnfixExpression() ast.Node {
	return &ast.UnfixExpression{Token: p.curToken, Operator: p.curToken.Literal}
}

func (p *Parser) parseIntegerLiteral() ast.Node {
	lit := &ast.IntegerLiteral{Token: p.curToken}
	value, e := strconv.Atoi(p.curToken.Literal)
	if e != nil {
		p.Throw("parse/int", p.curToken)
		return nil
	}

	lit.Value = value
	return lit
}

func (p *Parser) parseFloatLiteral() ast.Node {
	lit := &ast.FloatLiteral{Token: p.curToken}
	value, e := strconv.ParseFloat(p.curToken.Literal, 64)
	if e != nil {
		p.Throw("parse/float64", p.curToken)
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
	prefix := p.curToken
	p.NextToken()
	expression.Right = p.parseExpression(PREFIX)
	if expression.Right == nil {
		p.Throw("parse/follow", prefix)
	}
	return expression
}

func (p *Parser) parsePrefixExpression() ast.Node {
	expression := &ast.PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}
	p.NextToken()
	expression.Right = p.parseExpression(FPREFIX)
	expression.Args = p.recursivelyListify(expression.Right)
	return expression
}

func (p *Parser) parseStructExpression() ast.Node {
	expression := &ast.StructExpression{
		Token: p.curToken,
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
		p.Throw("parse/builtin", p.curToken)
	}
	p.NextToken()
	return expression
}

func (p *Parser) parseCodeLiteral() ast.Node {
	expression := &ast.CodeLiteral{}
	p.NextToken()
	expression.Right = p.parseExpression(FUNC)
	return expression
}

func (p *Parser) parseFunctionExpression() ast.Node {
	expression := &ast.PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}

	p.NextToken()
	if p.curToken.Type == token.LPAREN || expression.Operator == "-" {
		expression.Right = p.parseExpression(PREFIX)
	} else {
		expression.Right = p.parseExpression(FPREFIX)
	}

	expression.Args = p.recursivelyListify(expression.Right)

	return expression
}

func (p *Parser) parseSuffixExpression(left ast.Node) ast.Node {
	expression := &ast.SuffixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Left:     left,
	}


	if p.curToken.Source != "rsc/builtins.ch" {
		//printNodeList(p.recursivelyListify(expression.Left))
	}
	expression.Args = p.recursivelyListify(expression.Left)

	return expression
}

func (p *Parser) parseInfixExpression(left ast.Node) ast.Node {

	if p.curToken.Type == token.ASSIGN || p.curToken.Type == token.CMD_ASSIGN ||
		p.curToken.Type == token.VAR_ASSIGN || p.curToken.Type == token.DEF_ASSIGN ||
		p.curToken.Type == token.GVN_ASSIGN || p.curToken.Type == token.PVR_ASSIGN ||
		p.curToken.Type == token.TYP_ASSIGN {
		return p.parseAssignmentExpression(left)
	}

	if p.curToken.Type == token.MAGIC_COLON {
		// Then we will magically convert a function declaration into an assignment of a lambda to a
		// constant.
		newTok := p.curToken
		newTok.Type = token.GVN_ASSIGN
		newTok.Literal = "="
		p.NextToken()
		right := p.parseExpression(COLON)
		switch left := left.(type) {
		case *ast.PrefixExpression:
			expression := &ast.AssignmentExpression{
				Token: newTok,
				Left:  &ast.Identifier{Token: left.Token, Value: left.Token.Literal},
			}
			fn := &ast.FuncExpression{Token: p.curToken}
			fn.Body = right
			fn.Sig = p.RecursivelySlurpSignature(left.Right, "single")
			expression.Right = fn
			return expression
		default:
			p.Throw("parse/inner", p.curToken)
		}
	}

	expression := &ast.InfixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Left:     left,
	}
	precedence := p.curPrecedence()
	p.NextToken()
	expression.Right = p.parseExpression(precedence)
	expression.Args = p.listify(expression)
	return expression

}

func (p *Parser) parseExecExpression(left ast.Node) ast.Node {

	expression := &ast.ExecExpression{
		Token: p.curToken,
		Left:  left,
	}
	precedence := p.curPrecedence()
	p.NextToken()

	switch t := left.(type) {
	case *ast.Identifier:
		q, ok := p.Parsers[t.Value]
		if !ok {
			p.Throw("parse/exec/found", p.curToken, t.Value)
			return nil
		}
		q.TokenizedCode = p.TokenizedCode
		q.Errors = p.Errors
		q.nesting = p.nesting
		q.curToken = p.curToken
		q.peekToken = p.peekToken
		expression.Right = q.parseExpression(precedence)
		p.TokenizedCode = q.TokenizedCode
		p.curToken = q.curToken
		p.peekToken = q.peekToken
		p.nesting = q.nesting
		return expression
	default:
		p.Throw("parse/exec/name", p.curToken)
		return nil
	}
}

func (p *Parser) parseAssignmentExpression(left ast.Node) ast.Node {
	expression := &ast.AssignmentExpression{
		Token: p.curToken,
		Left:  left,
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
	if p.curToken.Type == token.RPAREN { // then what we must have is an empty tuple
		return &ast.EmptyTuple{Token: p.curToken}
	}
	exp := p.parseExpression(LOWEST)
	if !p.expectPeek(token.RPAREN) {
		p.NextToken() // Forces emission of the error
		return nil
	}
	if p.peekToken.Type == token.LPAREN {
		p.NextToken()
		exp = p.parsePresumedApplication(exp)
	}
	return exp
}

// We assume that in things of the form (<expression 1>) (<expression 2>), the intention is that expression 1
// should return a function which is applied to expression 2.
func (p *Parser) parsePresumedApplication(left ast.Node) ast.Node {
	expression := &ast.ApplicationExpression{
		Token: p.curToken,
		Left:  left,
	}
	p.NextToken()
	if p.curToken.Type == token.RPAREN { // then what we must have is an empty tuple
		expression.Right = &ast.EmptyTuple{Token: p.curToken}
		return expression
	}
	expression.Right = p.parseExpression(LOWEST)
	if !p.expectPeek(token.RPAREN) {
		p.NextToken() // Forces emission of the error
		return nil
	}
	return expression
}

func (p *Parser) parseReturnExpression() ast.Node {
	p.NextToken()
	expression := &ast.PrefixExpression{
		Token:    p.curToken,
		Operator: token.RETURN,
		Right:    p.parseExpression(FUNC),
	}
	return expression
}

func (p *Parser) parseGolangExpression() ast.Node {
	expression := &ast.GolangExpression{
		Token: p.curToken,
	}
	p.NextToken()
	return expression
}

func (p *Parser) parseFuncExpression() ast.Node {
	expression := &ast.FuncExpression{
		Token: p.curToken,
	}
	p.NextToken()
	RHS := p.parseExpression(FUNC)
	// At this point the root of the RHS should be a GIVEN or a COLON or who knows what's
	// happened?
	root := RHS
	switch RHS := RHS.(type) {
	case *ast.InfixExpression:
		if RHS.Token.Type == token.GIVEN {
			root = RHS.Left
			expression.Given = RHS.Right
		}
	}
	switch root := root.(type) {
	case *ast.LazyInfixExpression:
		if root.Token.Type != token.COLON {
			p.Throw("parse/colon", p.curToken)
			return nil
		}
		expression.Sig = p.RecursivelySlurpSignature(root.Left, "single")
		expression.Body = root.Right
		return expression
	default:
		p.Throw("parse/malfunc", p.curToken)
		return nil
	}
}

func (p *Parser) parseListExpression() ast.Node {
	p.NextToken()
	if p.curToken.Type == token.RBRACK { // Deals with the case where the list is []
		return &ast.ListExpression{List: nil, Token: p.curToken}
	}
	exp := p.parseExpression(LOWEST)
	if !p.expectPeek(token.RBRACK) {
		p.NextToken() // Forces emission of error.
		return nil
	}
	expression := &ast.ListExpression{List: exp, Token: p.curToken}
	return expression
}

func (p *Parser) parseSetExpression() ast.Node {
	p.NextToken()
	if p.curToken.Type == token.RBRACE { // Deals with the case where the set is {}
		return &ast.SetExpression{Set: nil, Token: p.curToken}
	}
	exp := p.parseExpression(LOWEST)
	if !p.expectPeek(token.RBRACE) {
		p.NextToken() // Forces emission of error.
		return nil
	}
	expression := &ast.SetExpression{Set: exp, Token: p.curToken}
	return expression
}

// The next two functions take the arguments at the *call site* of a function and puts them
// into a list for us.

// This first one allows us to treat infixes as a special case. (We can't treat infixes as bling 
// recursively, 'cos they aren't.)
func (p *Parser) listify(start ast.Node) []ast.Node {
	switch start := start.(type) {
	case *ast.InfixExpression :
		if start.Operator == "," { break }
		left := p.recursivelyListify(start.Left)
		left = append(left, &ast.Bling{Value: start.Operator, Token: start.Token})
		left = append(left, p.recursivelyListify(start.Right)...)
		return left
	}
	return p.recursivelyListify(start)
}

func (p *Parser) recursivelyListify(start ast.Node) []ast.Node {
	switch start := start.(type) {
	case *ast.InfixExpression :
		if start.Operator == "," {
			left := p.recursivelyListify(start.Left)
			left = append(left, p.recursivelyListify(start.Right)...)
			return left
		}
		if p.Midfixes.Contains(start.Operator) {
			left := p.recursivelyListify(start.Left)
			left = append(left, &ast.Bling{Value: start.Operator, Token: start.Token})
			left = append(left, p.recursivelyListify(start.Right)...)
			return left
		}
	case *ast.PrefixExpression :
		if p.Forefixes.Contains(start.Operator) {
			left := []ast.Node{&ast.Bling{Value: start.Operator, Token: start.Token}}
			left = append(left, p.recursivelyListify(start.Right)...)
			return left
		}
	case *ast.SuffixExpression :
		if p.Endfixes.Contains(start.Operator) {
			left := p.recursivelyListify(start.Left)
			left = append(left, &ast.Bling{Value: start.Operator, Token: start.Token})
			return left
		}
	}
	return []ast.Node{start}
}

func printNodeList(L []ast.Node) {
	for _, v := range L {
		fmt.Println(v.String())
	}
}


func (p *Parser) checkNesting() {
	// if p.curToken.Source != "builtin library" {fmt.Printf("Checking nesting %v\n", p.curToken)}
	if p.curToken.Type == token.LPAREN || p.curToken.Type == token.LBRACE ||
		p.curToken.Type == token.LBRACK {
		p.nesting.Push(p.curToken)
	}
	if p.curToken.Type == token.RPAREN || p.curToken.Type == token.RBRACE ||
		p.curToken.Type == token.RBRACK {
		popped, poppable := p.nesting.Pop()
		if !poppable {
			p.Throw("parse/match", p.curToken)
			return
		}
		if !checkConsistency(popped, p.curToken) {
			p.Throw("parse/nesting", p.curToken, popped)
		}
	}
	if p.curToken.Type == token.EOF {
		for popped, poppable := p.nesting.Pop(); poppable; popped, poppable = p.nesting.Pop() {
			p.Throw("parse/eol", p.curToken, popped)
		}
	}
}

func checkConsistency(left, right token.Token) bool {
	// if left.Source != "builtin library" {
	//	fmt.Printf("Checking consistency: %v, %v\n", left, right)
	// }
	if left.Type == token.LPAREN && left.Literal == "(" &&
		right.Type == token.RPAREN && right.Literal == ")" {
		return true
	}
	if left.Type == token.LPAREN && left.Literal == "|->" &&
		right.Type == token.RPAREN && right.Literal == "<-|" {
		return true
	}
	if left.Type == token.LBRACK && right.Type == token.RBRACK {
		return true
	}
	if left.Type == token.LBRACE && right.Type == token.RBRACE {
		return true
	}
	return false
}

func (p *Parser) ParseLine(source, input string) *ast.Node {
	p.ClearErrors()
	rl := relexer.New(source, input)
	p.TokenizedCode = rl
	result := p.ParseTokenizedChunk()
	p.Errors = append(rl.GetErrors(), p.Errors...)
	return result
}

func (p *Parser) ParseDump(source, input string) {
	parsedLine := p.ParseLine(source, input)
	fmt.Printf("Parser returns: %v\n\n", (*parsedLine).String())
}

func (p *Parser) Throw(errorID string, tok token.Token, args ...any) {
	p.Errors = object.Throw(errorID, p.Errors, tok, args...)
}

func (p *Parser) ErrorsExist() bool {
	return len(p.Errors) > 0
}

func (p *Parser) ReturnErrors() string {
	return object.GetList(p.Errors)
}

func (p *Parser) ClearErrors() {
	p.Errors = []*object.Error{}
}

// Slurps the signature of a function out of it. As the colon after a function definition has
// extremely low precedence, we should find it at the root of the tree.
// We extract the keyword first and then hand its branch or branches off to a recursive tree-slurper.
func (prsr *Parser) ExtractSignature(fn ast.Node) (string, signature.Signature, signature.Signature, ast.Node, ast.Node, *object.Error) {
	var (
		keyword               string
		sig                   signature.Signature
		rTypes                signature.Signature
		start, content, given ast.Node
	)
	if fn.GetToken().Type == token.GIVEN {
		given = fn.(*ast.InfixExpression).Right
		fn = fn.(*ast.InfixExpression).Left
	}

	switch fn := fn.(type) {
	case *ast.LazyInfixExpression:
		if fn.Token.Type != token.COLON {
			return keyword, sig, rTypes, content, given, &object.Error{Message: "malformed command or function definition", Token: fn.GetToken()}
		}
		start = fn.Left
		content = fn.Right
	case *ast.InfixExpression:
		if fn.Token.Type != token.MAGIC_COLON {
			return keyword, sig, rTypes, content, given, &object.Error{Message: "malformed command or function definition", Token: fn.GetToken()}
		}
		start = fn.Left
		content = fn.Right
	default:
		return keyword, sig, rTypes, content, given, &object.Error{Message: "malformed command or function definition", Token: fn.GetToken()}
	}

	if start.GetToken().Type == token.RIGHTARROW {
		rTypes = prsr.RecursivelySlurpReturnTypes(start.(*ast.InfixExpression).Right)
		start = start.(*ast.InfixExpression).Left
	}

	switch start := start.(type) {
	case *ast.PrefixExpression:
		keyword = start.Operator
		sig = prsr.RecursivelySlurpSignature(start.Right, "single")
	case *ast.InfixExpression:
		keyword = start.Operator
		LHS := prsr.RecursivelySlurpSignature(start.Left, "single")
		RHS := prsr.RecursivelySlurpSignature(start.Right, "single")
		middle := signature.NameTypePair{VarName: start.Operator, VarType: "bling"}
		sig = append(append(LHS, middle), RHS...)
	case *ast.SuffixExpression:
		keyword = start.Operator
		sig = prsr.RecursivelySlurpSignature(start.Left, "single")
	case *ast.UnfixExpression:
		keyword = start.Operator
		sig = signature.Signature{}
	default:
		return keyword, sig, rTypes, content, given, &object.Error{Message: "malformed command or function definition", Token: fn.GetToken()}
	}
	return keyword, sig, rTypes, content, given, nil
}

func (p *Parser) RecursivelySlurpSignature(node ast.Node, dflt string) signature.Signature {
	switch typednode := node.(type) {
	case *ast.InfixExpression:
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
		default:
			p.Throw("parse/sig/b", typednode.Token)
		}
	case *ast.SuffixExpression:
		switch {
		case TypeExists(typednode.Operator, p.TypeSystem) || 
				typednode.Operator == "ast" || typednode.Operator == "varname" :
			LHS := p.RecursivelySlurpSignature(typednode.Left, dflt)
			for k := range LHS {
				LHS[k].VarType = typednode.Operator
			}
			return LHS
		case typednode.Operator == "raw":
			LHS := p.RecursivelySlurpSignature(typednode.Left, dflt)
			for k := range LHS {
				LHS[k].VarType = LHS[k].VarType + " raw"
			}
			return LHS
		case p.Endfixes.Contains(typednode.Operator):
			LHS := p.RecursivelySlurpSignature(typednode.Left, dflt)
			end := signature.NameTypePair{VarName: typednode.Operator, VarType: "bling"}
			return append(LHS, end)
		default:
			p.Throw("parse/sig/c", typednode.Token)
		}
		p.Throw("parse/sig/d", node.GetToken())
	case *ast.Identifier:
		return signature.Signature{signature.NameTypePair{VarName: typednode.Value, VarType: dflt}}
	case *ast.PrefixExpression:
		if p.Forefixes.Contains(typednode.Operator) {
			RHS := p.RecursivelySlurpSignature(typednode.Right, dflt)
			front := signature.Signature{signature.NameTypePair{VarName: typednode.Operator, VarType: "bling"}}
			return append(front, RHS...)
		} else {
			p.Throw("parse/sig/a", typednode.Token)
		}
	}
	return nil
}

func (p *Parser) RecursivelySlurpReturnTypes(node ast.Node) signature.Signature {
	switch typednode := node.(type) {
	case *ast.InfixExpression:
		switch {
		case typednode.Token.Type == token.COMMA:
			LHS := p.RecursivelySlurpReturnTypes(typednode.Left)
			RHS := p.RecursivelySlurpReturnTypes(typednode.Right)
			return append(LHS, RHS...)
		default:
			p.Throw("parse/ret/a", typednode.Token)
		}
	case *ast.TypeLiteral:
		return signature.Signature{signature.NameTypePair{VarName: "x", VarType: typednode.Value}}
	default:
		p.Throw("parse/ret/b", typednode.GetToken())
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
	for tok := T.NextToken(); tok.Type != token.EOF; tok = T.NextToken() {
		if tok.Type == token.IDENT &&
			!p.AllFunctionIdents.Contains(tok.Literal) &&
			!TypeExists(tok.Literal, p.TypeSystem) {
			if assignHasHappened {
				RHS.Add(tok.Literal)
			} else {
				LHS.Add(tok.Literal)
			}
		}
		if tok.Type == token.ASSIGN || tok.Type == token.DEF_ASSIGN ||
			tok.Type == token.VAR_ASSIGN || tok.Type == token.CMD_ASSIGN ||
			tok.Type == token.PVR_ASSIGN {
			assignHasHappened = true
		}

	}
	return LHS, RHS
}
