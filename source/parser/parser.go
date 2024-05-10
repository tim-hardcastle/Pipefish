package parser

import (
	"fmt"
	"reflect"
	"strconv"
	"strings"

	"pipefish/source/ast"
	"pipefish/source/dtypes"
	"pipefish/source/lexer"
	"pipefish/source/report"
	"pipefish/source/token"
)

const (
	_ int = iota
	LOWEST
	SEMICOLON   // semantic newline or ;
	FUNC        // Lambda declaration
	GIVEN       // given
	WEAK_COLON  // kludge to make logging work. TODO --- find out how.
	GVN_ASSIGN  //
	LOGGING     //
	COLON       // :
	ASSIGN      // =
	PIPING      // ->, >>, ?>
	OR          // or
	AND         // and
	NOT         // not
	EQUALS      // == or !=
	LESSGREATER // > or < or <= or >=
	WEAK_COMMA  // a kludge to let me use Go-like syntax in function definitions --- change to FMIDFIX?
	FPREFIX     // user-defined prefix or function
	FMIDFIX     // user-defined midfix or forefix
	FENDFIX     // user-defined endfix
	COMMA       // ,
	WITH        // with, but ONLY when peeking ahead, otherwise it's an FMIDFIX
	FINFIX      // user-defined infix or ->
	SUM         // + or -
	PRODUCT     // * or / or %
	FSUFFIX     // user-defined suffix, or type in type declaration
	MINUS       //  - as a prefix
	INDEX       // after [
	BELOW_NAMESPACE
	NAMESPACE // 'foo.bar'
)

var precedences = map[token.TokenType]int{
	token.SEMICOLON:           SEMICOLON,
	token.NEWLINE:             SEMICOLON,
	token.WEAK_COLON:          WEAK_COLON,
	token.LOG:                 LOGGING,
	token.IFLOG:               LOGGING,
	token.PRELOG:              LOGGING,
	token.GLOBAL:              FUNC,
	token.GIVEN:               GIVEN,
	token.LOOP:                GIVEN,
	token.ASSIGN:              ASSIGN,
	token.CMD_ASSIGN:          ASSIGN,
	token.GVN_ASSIGN:          GVN_ASSIGN,
	token.LZY_ASSIGN:          ASSIGN,
	token.COLON:               COLON,
	token.MAGIC_COLON:         COLON,
	token.PIPE:                PIPING,
	token.MAPPING:             PIPING,
	token.FILTER:              PIPING,
	token.OR:                  OR,
	token.AND:                 AND,
	token.NOT:                 NOT,
	token.EQ:                  EQUALS,
	token.NOT_EQ:              EQUALS,
	token.WEAK_COMMA:          WEAK_COMMA,
	token.COMMA:               COMMA,
	token.LBRACK:              INDEX,
	token.EVAL:                FPREFIX,
	token.EMDASH:              FSUFFIX,
	token.NAMESPACE_SEPARATOR: NAMESPACE,
}

type TokenSupplier interface{ NextToken() token.Token }

func String(t TokenSupplier) string {
	result := ""
	for tok := t.NextToken(); tok.Type != "EOF"; tok = t.NextToken() {
		result = result + fmt.Sprintf("%+v\n", tok)
	}
	return result
}

type tokenizedCodeChunks []*token.TokenizedCodeChunk

type ParsedCodeChunks []ast.Node

type ParserData struct {
	Parser         *Parser
	ScriptFilepath string
}

type Parser struct {

	// Temporary state: things that are used to parse one line.

	TokenizedCode         TokenSupplier
	Errors                report.Errors
	nesting               dtypes.Stack[token.Token]
	curToken              token.Token
	peekToken             token.Token
	Logging               bool
	TokenizedDeclarations [13]tokenizedCodeChunks // TODO --- neither this nor the thing below should be in the parser at all
	ParsedDeclarations    [13]ParsedCodeChunks    // since they're no use after the uberparser has finished with them.
	CurrentNamespace      []string

	// Permanent state: things set up by the initializer which are
	// then constant for the lifetime of the service.

	Functions         dtypes.Set[string]
	Prefixes          dtypes.Set[string]
	Forefixes         dtypes.Set[string]
	Midfixes          dtypes.Set[string]
	Endfixes          dtypes.Set[string]
	Infixes           dtypes.Set[string]
	Suffixes          dtypes.Set[string]
	Unfixes           dtypes.Set[string]
	Bling             dtypes.Set[string]
	AllFunctionIdents dtypes.Set[string]

	nativeInfixes dtypes.Set[token.TokenType]
	lazyInfixes   dtypes.Set[token.TokenType]

	FunctionTable    FunctionTable
	FunctionGroupMap map[string]*ast.FunctionGroup
	TypeSystem       TypeSystem
	Structs          dtypes.Set[string]       // TODO --- remove: this has nothing to do that can't be done by the presence of a key
	StructSig        map[string]ast.Signature // <--- in here.
	Externals        map[string]string
	Languages        []string
	GoImports        map[string][]string
	NamespaceBranch  map[string]*ParserData
	NamespacePath    string
}

func New() *Parser {
	p := &Parser{
		Errors:            []*report.Error{},
		Logging:           true,
		nesting:           *dtypes.NewStack[token.Token](),
		Functions:         make(dtypes.Set[string]),
		Prefixes:          make(dtypes.Set[string]),
		Forefixes:         make(dtypes.Set[string]),
		Midfixes:          make(dtypes.Set[string]),
		Endfixes:          make(dtypes.Set[string]),
		Infixes:           make(dtypes.Set[string]),
		Suffixes:          make(dtypes.Set[string]),
		Unfixes:           make(dtypes.Set[string]),
		AllFunctionIdents: make(dtypes.Set[string]),
		Bling:             make(dtypes.Set[string]),
		nativeInfixes: dtypes.MakeFromSlice([]token.TokenType{
			token.COMMA, token.EQ, token.NOT_EQ, token.WEAK_COMMA,
			token.ASSIGN, token.CMD_ASSIGN, token.GVN_ASSIGN, token.LZY_ASSIGN,
			token.GIVEN, token.LBRACK, token.MAGIC_COLON, token.PIPE, token.MAPPING,
			token.FILTER, token.NAMESPACE_SEPARATOR, token.IFLOG}),
		lazyInfixes: dtypes.MakeFromSlice([]token.TokenType{token.AND,
			token.OR, token.COLON, token.WEAK_COLON, token.SEMICOLON, token.NEWLINE}),
		FunctionTable:    make(FunctionTable),
		FunctionGroupMap: make(map[string]*ast.FunctionGroup), // The logger needs to be able to see service variables and this is the simplest way.
		TypeSystem:       NewTypeSystem(),
		Structs:          make(dtypes.Set[string]),
		GoImports:        make(map[string][]string),
		NamespaceBranch:  make(map[string]*ParserData),
		Externals:        make(map[string]string),
	}

	for k := range *p.TypeSystem {
		p.Suffixes.Add(k)
	}

	p.Suffixes.Add("raw")
	p.Suffixes.Add("ast")
	p.Suffixes.Add("ref")
	p.Suffixes.Add("ident")

	p.Infixes.Add("varchar")

	p.Functions.AddSet(dtypes.MakeFromSlice([]string{"builtin"}))

	p.StructSig = make(map[string]ast.Signature)

	return p
}

func (p *Parser) NextToken() {
	p.checkNesting()
	p.SafeNextToken()
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

func (p *Parser) prefixSuffixError() {
	p.Throw("parse/before", &p.curToken, &p.peekToken)
}

func (p *Parser) ParseTokenizedChunk() ast.Node {
	p.nesting = *dtypes.NewStack[token.Token]()
	p.SafeNextToken()
	p.SafeNextToken()
	expn := p.parseExpression(LOWEST)
	p.NextToken()
	if p.curToken.Type != token.EOF {
		p.Throw("parse/expected", &p.curToken)
	}
	return expn
}

var literals = dtypes.MakeFromSlice([]token.TokenType{token.INT, token.FLOAT, token.STRING, token.TRUE, token.FALSE, token.ELSE})
var literalsAndLParen = dtypes.MakeFromSlice([]token.TokenType{token.INT, token.FLOAT, token.STRING, token.TRUE, token.FALSE, token.ELSE,
	token.LPAREN, token.LBRACE, token.EVAL})
var assignmentTokens = dtypes.MakeFromSlice([]token.TokenType{token.ASSIGN, token.CMD_ASSIGN, token.GVN_ASSIGN, token.LZY_ASSIGN})

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
	case token.EVAL, token.GLOBAL:
		leftExp = p.parsePrefixExpression()
	case token.LOOP:
		leftExp = p.parseLoopExpression()
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
	case token.TRY:
		leftExp = p.parseTryExpression()
	case token.GOCODE:
		leftExp = p.parseGolangExpression()
	case token.PRELOG:
		leftExp = p.parsePrelogExpression()
	default:
		noNativePrefix = true
	}

	// We're looking at an identifier.
	// If we're in a namespace, we need the symbol to be resolved by the appropriate parser.
	resolvingParser := p.getResolvingParser()
	if p.ErrorsExist() {
		return nil
	}

	// So what we're going to do is find out if the identifier *thinks* it's a function, i.e. if it precedes
	// something that's a prefix (in the broader sense, i.e. an identifier, literal, LPAREN, etc). But not a
	// minus sign, that would be confusing, people can use parentheses.
	// If so, then we will parse it as though it's a Function, and it had better turn out to be a lambda at
	// runtime. If it isn't, then we'll treat it as an identifier.
	if noNativePrefix { // TODO -- why aren't builtin, func, and struct not native prefixes?
		if p.curToken.Type == token.IDENT {
			if p.curToken.Literal == "builtin" {
				leftExp = p.parseBuiltInExpression()
				return leftExp
			}
			// Here we step in and deal with things that are functions and objects, like the type conversion
			// functions and their associated types. Before we look them up as functions, we want to
			// be sure that they're not in such a position that they're being used as literals.
			if !resolvingParser.positionallyFunctional() {
				switch {
				case TypeExists(p.curToken.Literal, resolvingParser.TypeSystem):
					leftExp = &ast.TypeLiteral{Token: p.curToken, Value: p.curToken.Literal}
				case resolvingParser.Unfixes.Contains(p.curToken.Literal):
					leftExp = p.parseUnfixExpression()
				case resolvingParser.Bling.Contains(p.curToken.Literal):
					leftExp = &ast.Bling{Token: p.curToken, Value: p.curToken.Literal}
				default:
					leftExp = p.parseIdentifier()
				}
			} else {
				if p.curToken.Literal == "func" {
					leftExp = p.parseFuncExpression()
					return leftExp
				}
				if p.curToken.Literal == "struct" {
					leftExp = p.parseStructExpression()
					return leftExp
				}
				switch {
				case resolvingParser.Prefixes.Contains(p.curToken.Literal) || resolvingParser.Forefixes.Contains(p.curToken.Literal):
					leftExp = p.parsePrefixExpression()
				default:
					leftExp = p.parseFunctionExpression() // That, at least, is what it is syntactictally.
				}
			}
		} else {
			p.Throw("parse/prefix", &p.curToken)
		}
	}

	for precedence < p.peekPrecedence() {
		for resolvingParser.Suffixes.Contains(p.peekToken.Literal) || resolvingParser.Endfixes.Contains(p.peekToken.Literal) || p.peekToken.Type == token.EMDASH {
			if p.curToken.Type == token.NOT || p.curToken.Type == token.IDENT && p.curToken.Literal == "-" || p.curToken.Type == token.ELSE {
				p.prefixSuffixError()
				return nil
			}
			p.NextToken()
			leftExp = p.parseSuffixExpression(leftExp)
		}

		if p.peekToken.Type == token.LOG {
			p.NextToken()
			leftExp = p.parseLogExpression(leftExp)
		}

		if precedence >= p.peekPrecedence() {
			break
		}

		foundInfix := p.nativeInfixes.Contains(p.peekToken.Type) ||
			p.lazyInfixes.Contains(p.peekToken.Type) ||
			resolvingParser.Infixes.Contains(p.peekToken.Literal) ||
			resolvingParser.Midfixes.Contains(p.peekToken.Literal)
		if !foundInfix {
			return leftExp
		}
		p.NextToken()

		if foundInfix {
			switch {
			case p.lazyInfixes.Contains(p.curToken.Type):
				leftExp = p.parseLazyInfixExpression(leftExp)
			case p.curToken.Type == token.LBRACK:
				leftExp = p.parseIndexExpression(leftExp)
			case p.curToken.Type == token.PIPE || p.curToken.Type == token.MAPPING ||
				p.curToken.Type == token.FILTER:
				leftExp = p.parseStreamingExpression(leftExp)
			case p.curToken.Type == token.IFLOG:
				leftExp = p.parseIfLogExpression(leftExp)
			case p.curToken.Type == token.NAMESPACE_SEPARATOR:
				leftExp = p.parseNamespaceExpression(leftExp)
			default:
				leftExp = p.parseInfixExpression(leftExp)
			}
		}
	}
	if leftExp == nil {
		if p.curToken.Type == token.EOF {
			p.Throw("parse/line", &p.curToken)
			return nil
		}
		if p.curToken.Literal == "<-|" || p.curToken.Literal == ")" ||
			p.curToken.Literal == "]" || p.curToken.Literal == "}" {
			p.Throw("parse/close", &p.curToken)
			return nil
		}
		p.Throw("parse/missing", &p.curToken)
		return nil
	}
	return leftExp
}

// The parser accumulates the names in foo.bar.troz as it goes along. Now we follow the trail of namespaces
// to find what parser should resolve the symbol.
func (p *Parser) getResolvingParser() *Parser {
	lP := p
	for _, name := range p.CurrentNamespace {
		s, ok := lP.NamespaceBranch[name]
		if !ok {
			p.Throw("parse/namespace/exist", &p.curToken, name)
			return nil
		}
		lP = s.Parser
	}
	// We don't need the resolving parser to parse anything but we *do* need to call positionallyFunctional,
	// so it needs the following data to work.
	lP.curToken = p.curToken
	lP.peekToken = p.peekToken
	return lP
}

func (p *Parser) positionallyFunctional() bool {
	if assignmentTokens.Contains(p.peekToken.Type) {
		return false
	}
	if p.peekToken.Type == token.RPAREN || p.peekToken.Type == token.PIPE ||
		p.peekToken.Type == token.MAPPING || p.peekToken.Type == token.FILTER ||
		p.peekToken.Type == token.COLON || p.peekToken.Type == token.MAGIC_COLON ||
		p.peekToken.Type == token.COMMA {
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
	if p.Suffixes.Contains(p.peekToken.Literal) {
		return FSUFFIX
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
		if p.peekToken.Literal == "with" || p.peekToken.Literal == "without" { // Note, this is the one assymmetry in the system of precedence.
			return WITH // When not peeking ahead, `with` has precedence just *below* a comma.
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
	return LOWEST
}

func (p *Parser) curPrecedence() int {
	if p.curToken.Type == token.NAMESPACE_SEPARATOR {
		return BELOW_NAMESPACE
	}
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
			if p.curToken.Literal == "with" || p.curToken.Literal == "without" {
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
	p.CurrentNamespace = nil
	return &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseUnfixExpression() ast.Node {
	p.CurrentNamespace = nil
	return &ast.UnfixExpression{Token: p.curToken, Operator: p.curToken.Literal}
}

func (p *Parser) parseIntegerLiteral() ast.Node {
	lit := &ast.IntegerLiteral{Token: p.curToken}
	value, e := strconv.Atoi(p.curToken.Literal)
	if e != nil {
		p.Throw("parse/int", &p.curToken)
		return nil
	}

	lit.Value = value
	return lit
}

func (p *Parser) parseFloatLiteral() ast.Node {
	lit := &ast.FloatLiteral{Token: p.curToken}
	value, e := strconv.ParseFloat(p.curToken.Literal, 64)
	if e != nil {
		p.Throw("parse/float64", &p.curToken)
		return nil
	}

	lit.Value = value
	return lit
}

func (p *Parser) parseStringLiteral() ast.Node {
	return &ast.StringLiteral{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseAutoLog() ast.Node {
	return &ast.StringLiteral{Token: p.curToken}
}

func (p *Parser) parseNativePrefixExpression() ast.Node {
	expression := &ast.PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}
	prefix := p.curToken
	p.NextToken()
	var right ast.Node
	if prefix.Type == token.NOT {
		right = p.parseExpression(NOT)
	} else {
		right = p.parseExpression(MINUS)
	}
	if right == nil {
		p.Throw("parse/follow", &prefix)
	}
	expression.Args = []ast.Node{right}
	return expression
}

func (p *Parser) parsePrefixExpression() ast.Node {
	p.CurrentNamespace = nil
	expression := &ast.PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}
	p.NextToken()
	expression.Args = p.recursivelyListify(p.parseExpression(FPREFIX))
	//expression.Right = p.parseExpression(FPREFIX)
	return expression
}

func (p *Parser) parseLoopExpression() ast.Node {
	expression := &ast.LoopExpression{
		Token: p.curToken,
	}
	p.NextToken()
	expression.Code = p.parseExpression(GIVEN)
	return expression
}

func (p *Parser) parseStructExpression() ast.Node {
	expression := &ast.StructExpression{
		Token: p.curToken,
	}

	p.NextToken()
	sigtree := p.parseExpression(FPREFIX)
	expression.Sig = p.extractSig(p.recursivelyListify(sigtree))
	return expression
}

// This is to allow me to use the initializer to pour builtins into the parser's function table.
func (p *Parser) parseBuiltInExpression() ast.Node {
	expression := &ast.BuiltInExpression{}
	p.NextToken()
	if p.curToken.Type == token.STRING {
		expression.Name = p.curToken.Literal
	} else {
		p.Throw("parse/builtin", &p.curToken)
	}
	p.NextToken()
	return expression
}

func (p *Parser) parseFunctionExpression() ast.Node {
	p.CurrentNamespace = nil
	expression := &ast.PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}

	p.NextToken()

	var right ast.Node
	if p.curToken.Type == token.LPAREN || expression.Operator == "-" {
		right = p.parseExpression(MINUS)
	} else {
		right = p.parseExpression(FPREFIX)
	}

	expression.Args = p.recursivelyListify(right)

	return expression
}

func (p *Parser) parseSuffixExpression(left ast.Node) ast.Node {
	p.CurrentNamespace = nil
	expression := &ast.SuffixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Args:     p.recursivelyListify(left),
	}
	return expression
}

func (p *Parser) parseLogExpression(left ast.Node) ast.Node {

	expression := &ast.LogExpression{
		Token: p.curToken,
		Left:  left,
		Value: p.curToken.Literal,
	}
	return expression
}

func (p *Parser) parseNamespaceExpression(left ast.Node) ast.Node {
	p.NextToken()
	if left.GetToken().Type != token.IDENT {
		p.Throw("parse/namespace/lhs", left.GetToken())
		return nil
	}
	name := left.GetToken().Literal
	p.CurrentNamespace = append(p.CurrentNamespace, name)
	right := p.parseExpression(NAMESPACE)
	if p.ErrorsExist() {
		return nil
	}
	switch right := right.(type) {
	case *ast.Bling:
		right.Namespace = append(right.Namespace, name)
	case *ast.Identifier:
		right.Namespace = append(right.Namespace, name)
	case *ast.InfixExpression:
		right.Namespace = append(right.Namespace, name)
	case *ast.PrefixExpression:
		right.Namespace = append(right.Namespace, name)
	case *ast.SuffixExpression:
		right.Namespace = append(right.Namespace, name)
	case *ast.TypeLiteral:
		right.Namespace = append(right.Namespace, name)
	case *ast.UnfixExpression:
		right.Namespace = append(right.Namespace, name)
	default:
		p.Throw("parse/namespace/rhs", right.GetToken())
	}

	return right
}

func (p *Parser) parseInfixExpression(left ast.Node) ast.Node {
	if assignmentTokens.Contains(p.curToken.Type) {
		return p.parseAssignmentExpression(left)
	}

	if p.curToken.Type == token.MAGIC_COLON {
		// Then we will magically convert a function declaration into an assignment of a lambda to a
		// constant.
		newTok := p.curToken
		newTok.Type = token.GVN_ASSIGN
		newTok.Literal = "="
		p.NextToken()
		right := p.parseExpression(WEAK_COLON)
		switch left := left.(type) {
		case *ast.PrefixExpression:
			expression := &ast.AssignmentExpression{
				Token: newTok,
				Left:  &ast.Identifier{Token: left.Token, Value: left.Token.Literal},
			}
			fn := &ast.FuncExpression{Token: p.curToken}
			fn.Body = right
			fn.Sig, _ = p.getSigFromArgs(left.Args, "single?")
			expression.Right = fn
			if fn.Body.GetToken().Type == token.PRELOG && fn.Body.GetToken().Literal == "" {
				fn.Body.(*ast.LogExpression).Value = DescribeFunctionCall(left.Token.Literal, &fn.Sig)
			}
			return expression
		default:
			p.Throw("parse/inner", &p.curToken)
		}
	}

	expression := &ast.InfixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}
	precedence := p.curPrecedence()
	p.NextToken()
	right := p.parseExpression(precedence)
	if expression.Operator == "," {
		expression.Args = []ast.Node{left, &ast.Bling{Value: expression.Operator, Token: expression.Token}, right}
		return expression
	}
	expression.Args = p.recursivelyListify(left)
	expression.Args = append(expression.Args, &ast.Bling{Value: expression.Operator, Token: expression.Token})
	rightArgs := p.recursivelyListify(right)
	expression.Args = append(expression.Args, rightArgs...)
	return expression
}

// In a streaming expression we need to desugar e.g. 'x -> foo' to 'x -> foo that', etc.
func (p *Parser) parseStreamingExpression(left ast.Node) ast.Node {
	expression := &ast.PipingExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Left:     left,
	}
	precedence := p.curPrecedence()
	p.NextToken()
	expression.Right = p.parseExpression(precedence)
	expression.Right = p.recursivelyDesugarAst(expression.Right)
	return expression
}

func (p *Parser) parseIfLogExpression(left ast.Node) ast.Node {

	expression := &ast.LogExpression{
		Token: p.curToken,
		Left:  left,
		Value: p.curToken.Literal,
	}
	precedence := p.curPrecedence()
	p.NextToken()
	expression.Right = p.parseExpression(precedence)
	return expression
}

func (p *Parser) parsePrelogExpression() ast.Node {

	expression := &ast.LogExpression{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}
	precedence := p.curPrecedence()
	p.NextToken()
	expression.Right = p.parseExpression(precedence)
	return expression
}

func DescribeFunctionCall(name string, sig *ast.Signature) string {
	result := "Called '" + name + "'"
	vars := []string{}
	for _, pair := range *sig {
		if pair.VarType != "bling" {
			vars = append(vars, "||"+pair.VarName+"||")
		}
	}
	if len(vars) > 0 {
		result = result + " with " + strings.Join(vars, ", ")
	}
	return result + "."
}

func (p *Parser) recursivelyDesugarAst(exp ast.Node) ast.Node { // Adds "that" after piping, works through namespaces
	switch typedExp := exp.(type) {
	case *ast.Identifier:
		if p.Functions.Contains(exp.GetToken().Literal) {
			exp = &ast.PrefixExpression{Token: *typedExp.GetToken(),
				Operator: exp.GetToken().Literal,
				Args:     []ast.Node{&ast.Identifier{Value: "that"}}}
		}
		if p.Suffixes.Contains(exp.GetToken().Literal) {
			exp = &ast.SuffixExpression{Token: *typedExp.GetToken(),
				Operator: exp.GetToken().Literal,
				Args:     []ast.Node{&ast.Identifier{Value: "that"}}}
		}
	case *ast.InfixExpression:
		if typedExp.GetToken().Type == token.NAMESPACE_SEPARATOR {
			if typedExp.Args[0].GetToken().Type == token.IDENT {
				service, ok := p.NamespaceBranch[typedExp.Args[0].(*ast.Identifier).Value]
				if ok {
					exp.(*ast.InfixExpression).Args[2] = service.Parser.recursivelyDesugarAst(typedExp.Args[2])
				}
			}
		}
	}
	return exp
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
		return &ast.Nothing{Token: p.curToken}
	}
	exp := p.parseExpression(LOWEST)
	if !p.expectPeek(token.RPAREN) {
		p.NextToken() // Forces emission of the error
		return nil
	}
	return exp
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
			root = RHS.Args[0]
			expression.Given = RHS.Args[2]
		}
	}
	switch root := root.(type) {
	case *ast.LazyInfixExpression:
		if root.Token.Type != token.COLON {
			p.Throw("parse/colon", &p.curToken)
			return nil
		}
		expression.Sig, _ = p.RecursivelySlurpSignature(root.Left, "single?")
		if p.ErrorsExist() {
			return nil
		}
		expression.Body = root.Right
		return expression
	default:
		p.Throw("parse/malfunc", &p.curToken)
		return nil
	}
}

func (p *Parser) parseListExpression() ast.Node {
	p.NextToken()
	if p.curToken.Type == token.RBRACK { // Deals with the case where the list is []
		return &ast.ListExpression{List: &ast.Nothing{Token: p.curToken}, Token: p.curToken}
	}
	exp := p.parseExpression(LOWEST)
	if !p.expectPeek(token.RBRACK) {
		p.NextToken() // Forces emission of error.
		return nil
	}
	expression := &ast.ListExpression{List: exp, Token: p.curToken}
	return expression
}

func (p *Parser) parseTryExpression() ast.Node {
	p.NextToken()
	if p.curToken.Type == token.COLON {
		p.NextToken()
		exp := p.parseExpression(COLON)
		return &ast.TryExpression{Token: p.curToken, Right: exp, VarName: ""}
	}
	if p.curToken.Type == token.IDENT {
		varName := p.curToken.Literal
		p.NextToken()
		if p.curToken.Type != token.COLON {
			p.Throw("parse/try/colon", &p.curToken)
		}
		p.NextToken()
		exp := p.parseExpression(COLON)
		return &ast.TryExpression{Token: p.curToken, Right: exp, VarName: varName}
	} else {
		p.Throw("parse/try/ident", &p.curToken)
		return nil
	}
}

// This takes the arguments at the *call site* of a function and puts them
// into a list for us.
func (p *Parser) recursivelyListify(start ast.Node) []ast.Node {
	switch start := start.(type) {
	case *ast.InfixExpression:
		if start.Operator == "," {
			left := p.recursivelyListify(start.Args[0])
			left = append(left, p.recursivelyListify(start.Args[2])...)
			return left
		}
		if p.Midfixes.Contains(start.Operator) {
			return start.Args
		}
	case *ast.PrefixExpression:
		if p.Forefixes.Contains(start.Operator) {
			left := []ast.Node{&ast.Bling{Value: start.Operator, Token: start.Token}}
			left = append(left, start.Args...)
			return left
		}
	case *ast.SuffixExpression:
		if p.Endfixes.Contains(start.Operator) {
			left := start.Args
			left = append(left, &ast.Bling{Value: start.Operator, Token: start.Token})
			return left
		}
	}
	return []ast.Node{start}
}

func (p *Parser) checkNesting() {
	// if p.curToken.Source != "builtin library" {fmt.Printf("Checking nesting %v\n", &p.curToken)}
	if p.curToken.Type == token.LPAREN || p.curToken.Type == token.LBRACE ||
		p.curToken.Type == token.LBRACK {
		p.nesting.Push(p.curToken)
	}
	if p.curToken.Type == token.RPAREN || p.curToken.Type == token.RBRACE ||
		p.curToken.Type == token.RBRACK {
		popped, poppable := p.nesting.Pop()
		if !poppable {
			p.Throw("parse/match", &p.curToken)
			return
		}
		if !checkConsistency(popped, p.curToken) {
			p.Throw("parse/nesting", &p.curToken, &popped)
		}
	}
	if p.curToken.Type == token.EOF {
		for popped, poppable := p.nesting.Pop(); poppable; popped, poppable = p.nesting.Pop() {
			p.Throw("parse/eol", &p.curToken, &popped)
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

func (p *Parser) ParseLine(source, input string) ast.Node {
	p.ClearErrors()
	rl := lexer.NewRelexer(source, input)
	p.TokenizedCode = rl
	result := p.ParseTokenizedChunk()
	p.Errors = append(rl.GetErrors(), p.Errors...)
	return result
}

func (p *Parser) ParseDump(source, input string) {
	parsedLine := p.ParseLine(source, input) // TODO --- why do you have a pointer to an interface?
	if parsedLine == nil {
		fmt.Printf("Parser returns: nil")
	}
	fmt.Printf("Parser returns: %v\n\n", (parsedLine).String())
}

func (p *Parser) Throw(errorID string, tok *token.Token, args ...any) {
	p.Errors = report.Throw(errorID, p.Errors, tok, args...)
}

func (p *Parser) ErrorsExist() bool {
	return len(p.Errors) > 0
}

func (p *Parser) ReturnErrors() string {
	return report.GetList(p.Errors)
}

func (p *Parser) ClearErrors() {
	p.Errors = []*report.Error{}
}

// Slurps the signature of a function out of it. As the colon after a function definition has
// extremely low precedence, we should find it at the root of the tree.
// We extract the function name first and then hand its branch or branches off to a recursive tree-slurper.
func (prsr *Parser) ExtractPartsOfFunction(fn ast.Node) (string, ast.Signature, ast.Signature, ast.Node, ast.Node, []uint32) {
	var (
		functionName          string
		sig                   ast.Signature
		rTypes                ast.Signature
		start, content, given ast.Node
	)
	tupleList := make([]uint32, 0)
	if fn.GetToken().Type == token.GIVEN {
		given = fn.(*ast.InfixExpression).Args[2]
		fn = fn.(*ast.InfixExpression).Args[0]
	}

	switch fn := fn.(type) {
	case *ast.LazyInfixExpression:
		if !(fn.Token.Type == token.COLON || fn.Token.Type == token.WEAK_COLON) {
			prsr.Throw("parse/sig/malformed/a", fn.GetToken())
			return functionName, sig, rTypes, content, given, tupleList
		}
		start = fn.Left
		content = fn.Right
	case *ast.InfixExpression:
		if fn.Token.Type != token.MAGIC_COLON {
			prsr.Throw("parse/sig/malformed/b", fn.GetToken())
			return functionName, sig, rTypes, content, given, tupleList
		}
		start = fn.Args[0]
		content = fn.Args[2]
	default:
		prsr.Throw("parse/sig/malformed/c", fn.GetToken())
		return functionName, sig, rTypes, content, given, tupleList
	}

	if start.GetToken().Type == token.PIPE {
		rTypes = prsr.RecursivelySlurpReturnTypes(start.(*ast.PipingExpression).Right)
		start = start.(*ast.PipingExpression).Left
	}

	switch start := start.(type) {
	case *ast.PrefixExpression:
		functionName = start.Operator
		sig = prsr.extractSig(start.Args)
	case *ast.InfixExpression:
		functionName = start.Operator
		sig = prsr.extractSig(start.Args)
	case *ast.SuffixExpression:
		functionName = start.Operator
		sig = prsr.extractSig(start.Args)
	case *ast.UnfixExpression:
		functionName = start.Operator
		sig = ast.Signature{}
	default:
		prsr.Throw("parse/sig/malformed/d", fn.GetToken())
		return functionName, sig, rTypes, content, given, tupleList
	}
	for j, param := range sig {
		if param.VarType == "tuple" {
			tupleList = append(tupleList, uint32(j))
		}
	}
	return functionName, sig, rTypes, content, given, tupleList
}

func (p *Parser) extractSig(args []ast.Node) ast.Signature {
	sig := ast.Signature{}
	if len(args) == 0 || (len(args) == 1 && reflect.TypeOf(args[0]) == reflect.TypeOf(&ast.Nothing{})) {
		return sig
	}
	backTrackTo := 0
	for j, arg := range args {
		varName := ""
		varType := "*"
		switch arg := arg.(type) {
		case *ast.SuffixExpression:
			if arg.Operator == "raw" {
				switch inner := arg.Args[0].(type) {
				case *ast.SuffixExpression:
					if !TypeExists(inner.Operator, p.TypeSystem) {
						p.Throw("parse/raw/type", inner.GetToken())
						return nil
					}
					switch inmost := inner.Args[0].(type) {
					case *ast.Identifier:
						varName = inmost.Value
						varType = inner.Operator + " raw"
					default:
						p.Throw("parse/raw/ident", inmost.GetToken())
						return nil
					}
				case *ast.Identifier:
					varName = inner.Value
					varType = "raw"
				default:
					p.Throw("parse/raw/form", arg.GetToken())
					return nil
				}
			} else { // The suffix is not 'raw'
				if !(TypeExists(arg.Operator, p.TypeSystem) ||
					arg.Operator == "ast" || arg.Operator == "ident") {
					p.Throw("parse/sig/type/a", &arg.Token)
					return nil
				}
				switch inner := arg.Args[0].(type) {
				case *ast.Identifier:
					varName = inner.Value
					varType = arg.Operator
				default:
					p.Throw("parse/sig/ident/a", inner.GetToken())
					return nil
				}
			}
		case *ast.Identifier:
			if p.Endfixes.Contains(arg.Value) {
				varName = arg.Value
				varType = "bling"
			} else {
				varName = arg.Value
				varType = "*"
			}
		case *ast.PrefixExpression:
			if p.Forefixes.Contains(arg.Operator) {
				varName = arg.Operator
				varType = "bling"
			} else {
				// We may well be declaring a parameter which will have the same name as a function --- e.g. 'f'.
				// The parser will have parsed this as a prefix expression if it was followed by a type, e.g.
				// 'foo (f func) : <function body>'. We ought therefore to be interpreting it as a parameter
				// name under those circumstances. This tends to make the whole thing stupid, we should have
				// done all this before it got near the Pratt parser.
				switch inner := arg.Args[0].(type) {
				case *ast.Identifier:
					varName = arg.Operator
					varType = inner.Value
					if !(TypeExists(inner.Value, p.TypeSystem) ||
						arg.Operator == "ast" || arg.Operator == "ident") {
						p.Throw("parse/sig/type/b", arg.GetToken())
						return nil
					}
				default:
					p.Throw("parse/sig/ident/b", inner.GetToken())
					return nil
				}
			}
		case *ast.InfixExpression:
			if arg.Operator == "varchar" {
				switch potentialVariable := arg.Args[0].(type) {
				case *ast.Identifier:
					varName = potentialVariable.Value
				default:
					p.Throw("parse/sig/ident/c", potentialVariable.GetToken())
					return nil
				}
				switch potentialInteger := arg.Args[2].(type) {
				case *ast.IntegerLiteral:
					varType = "varchar(" + strconv.Itoa(potentialInteger.Value) + ")"
				default:
					p.Throw("parse/sig/varchar/int/a", potentialInteger.GetToken())
					return nil
				}
			} else {
				if p.Midfixes.Contains(arg.Operator) {
					varName = arg.Operator
					varType = "bling"
				} else {
					p.Throw("parse/sig/infix", arg.GetToken())
					return nil
				}
			}
		case *ast.Bling:
			varName = arg.Value
			varType = "bling"
		}
		if j == len(args)-1 && varType == "*" {
			for i := backTrackTo; i < len(sig); i++ {
				sig[i].VarType = "single?"
			}
			varType = "single?"
		}
		if !(varType == "bling" || varType == "*") {
			for i := backTrackTo; i < len(sig); i++ {
				sig[i].VarType = varType
			}

		}
		if varType == "bling" {
			if len(sig) > 0 && sig[len(sig)-1].VarType == "*" {
				for i := backTrackTo; i < len(sig); i++ {
					sig[i].VarType = "single?"
				}
			}
		}
		sig = append(sig, ast.NameTypePair{VarName: varName, VarType: varType})
		if !(varType == "*") {
			backTrackTo = len(sig)
		}
		if varType == "bling" {
			varType = "*"
		}
	}
	return sig
}

// TODO --- this function is a refactoring patch over RecursivelySlurpSignature and they could probably be more sensibly combined in a single function.
func (p *Parser) getSigFromArgs(args []ast.Node, dflt string) (ast.Signature, *report.Error) {
	sig := ast.Signature{}
	for _, arg := range args {
		if arg.GetToken().Type == token.IDENT && p.Bling.Contains(arg.GetToken().Literal) {
			sig = append(sig, ast.NameTypePair{VarName: arg.GetToken().Literal, VarType: "bling"})
		} else {
			partialSig, err := p.RecursivelySlurpSignature(arg, dflt)
			if err != nil {
				return nil, err
			}
			sig = append(sig, partialSig...)
		}
	}
	return sig, nil
}

func (p *Parser) RecursivelySlurpSignature(node ast.Node, dflt string) (ast.Signature, *report.Error) {
	switch typednode := node.(type) {
	case *ast.InfixExpression:
		switch {
		case p.Midfixes.Contains(typednode.Operator):
			LHS, err := p.RecursivelySlurpSignature(typednode.Args[0], dflt)
			if err != nil {
				return nil, err
			}
			RHS, err := p.RecursivelySlurpSignature(typednode.Args[2], dflt)
			if err != nil {
				return nil, err
			}
			middle := ast.NameTypePair{VarName: typednode.Operator, VarType: "bling"}
			return append(append(LHS, middle), RHS...), nil
		case typednode.Token.Type == token.COMMA || typednode.Token.Type == token.WEAK_COMMA:
			LHS, err := p.RecursivelySlurpSignature(typednode.Args[0], dflt)
			if err != nil {
				return nil, err
			}
			RHS, err := p.RecursivelySlurpSignature(typednode.Args[2], dflt)
			if err != nil {
				return nil, err
			}
			return append(LHS, RHS...), nil
		case typednode.Operator == "varchar":
			switch potentialInteger := typednode.Args[2].(type) {
			case *ast.IntegerLiteral:
				varType := "varchar(" + strconv.Itoa(potentialInteger.Value) + ")"
				return p.RecursivelySlurpSignature(typednode.Args[0], varType)
			default:
				return nil, newError("parse/sig/varchar/int/b", potentialInteger.GetToken())
			}
		case typednode.Operator == ".":
			namespacedIdent, err := recursivelySlurpNamespace(typednode)
			if err != nil {
				return nil, err
			}
			return ast.Signature{ast.NameTypePair{VarName: namespacedIdent, VarType: dflt}}, nil
		default:
			return nil, newError("parse/sig/b", typednode.GetToken())
		}
	case *ast.SuffixExpression:
		switch {
		case TypeExists(typednode.Operator, p.TypeSystem) ||
			typednode.Operator == "ast" || typednode.Operator == "ident":
			LHS, err := p.getSigFromArgs(typednode.Args, dflt)
			if err != nil {
				return nil, err
			}
			for k := range LHS {
				LHS[k].VarType = typednode.Operator
			}
			return LHS, nil
		case typednode.Operator == "raw":
			LHS, err := p.getSigFromArgs(typednode.Args, dflt)
			if err != nil {
				return nil, err
			}
			for k := range LHS {
				LHS[k].VarType = LHS[k].VarType + " raw"
			}
			return LHS, nil
		case p.Endfixes.Contains(typednode.Operator):
			LHS, err := p.getSigFromArgs(typednode.Args, dflt)
			if err != nil {
				return nil, err
			}
			end := ast.NameTypePair{VarName: typednode.Operator, VarType: "bling"}
			return append(LHS, end), nil
		default:
			return nil, newError("parse/sig/c", typednode.GetToken())
		}
	case *ast.Identifier:
		if p.Endfixes.Contains(typednode.Value) {
			return ast.Signature{ast.NameTypePair{VarName: typednode.Value, VarType: "bling"}}, nil
		}
		return ast.Signature{ast.NameTypePair{VarName: typednode.Value, VarType: dflt}}, nil
	case *ast.PrefixExpression:
		if p.Forefixes.Contains(typednode.Operator) {
			RHS, err := p.getSigFromArgs(typednode.Args, dflt)
			if err != nil {
				return nil, err
			}
			front := ast.Signature{ast.NameTypePair{VarName: typednode.Operator, VarType: "bling"}}
			return append(front, RHS...), nil
		} else {
			// We may well be declaring a parameter which will have the same name as a function --- e.g. 'f'.
			// The parser will have parsed this as a prefix expression if it was followed by a type, e.g.
			// 'foo (f func) : <function body>'. We ought therefore to be interpreting it as a parameter
			// name under those circumstances.
			return ast.Signature{ast.NameTypePair{VarName: typednode.Operator, VarType: dflt}}, nil
		}
	}
	return nil, newError("parse/sig/d", node.GetToken())
}

func recursivelySlurpNamespace(root *ast.InfixExpression) (string, *report.Error) {
	if len(root.Args) != 3 {
		return "", newError("parse/sig.namespace/a", root.Args[1].GetToken())
	}
	if root.Operator != "." {
		return "", newError("parse/sig.namespace/b", root.Args[1].GetToken())
	}
	LHS := ""
	RHS := ""
	var err *report.Error
	switch leftNode := root.Args[0].(type) {
	case *ast.Identifier:
		LHS = leftNode.Value
	case *ast.InfixExpression:
		LHS, err = recursivelySlurpNamespace(leftNode)
		if err != nil {
			return "", err
		}
	default:
		return "", newError("parse/sig.namespace/c", root.Args[1].GetToken())
	}
	switch rightNode := root.Args[2].(type) {
	case *ast.Identifier:
		RHS = rightNode.Value
	case *ast.InfixExpression:
		RHS, err = recursivelySlurpNamespace(rightNode)
		if err != nil {
			return "", err
		}
	default:
		return "", newError("parse/sig.namespace/d", root.Args[1].GetToken())
	}
	return LHS + "." + RHS, nil
}

func (p *Parser) RecursivelySlurpReturnTypes(node ast.Node) ast.Signature {
	switch typednode := node.(type) {
	case *ast.InfixExpression:
		switch {
		case typednode.Token.Type == token.COMMA:
			LHS := p.RecursivelySlurpReturnTypes(typednode.Args[0])
			RHS := p.RecursivelySlurpReturnTypes(typednode.Args[2])
			return append(LHS, RHS...)
		default:
			p.Throw("parse/ret/a", typednode.GetToken())
		}
	case *ast.TypeLiteral:
		return ast.Signature{ast.NameTypePair{VarName: "x", VarType: typednode.Value}}
	default:
		p.Throw("parse/ret/b", typednode.GetToken())
	}
	return nil
}

func (p *Parser) ExtractVariables(T TokenSupplier) (dtypes.Set[string], dtypes.Set[string]) {
	LHS := make(dtypes.Set[string])
	RHS := make(dtypes.Set[string])
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
		if tok.Type == token.ASSIGN || tok.Type == token.CMD_ASSIGN {
			assignHasHappened = true
		}

	}
	return LHS, RHS
}

func newError(ident string, tok *token.Token, args ...any) *report.Error {
	errorToReturn := report.CreateErr(ident, tok, args...)
	errorToReturn.Trace = []*token.Token{tok}
	return errorToReturn
}
