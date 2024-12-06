package parser

import (
	"fmt"
	"strconv"
	"strings"
	"unicode/utf8"

	"pipefish/source/ast"
	"pipefish/source/dtypes"
	"pipefish/source/err"
	"pipefish/source/lexer"
	"pipefish/source/token"
	"pipefish/source/values"
)

type Parser struct {

	// Temporary state: things that are used to parse one line.

	TokenizedCode         TokenSupplier
	nesting               dtypes.Stack[token.Token]
	curToken              token.Token
	peekToken             token.Token
	Logging               bool
	CurrentNamespace      []string

	// When we call a function in a namespace, we wish to parse it so that literal enum elements and bling are looked for
	// in that namespace without being namespaced.
	enumResolvingParsers []*Parser

	// Permanent state: things set up by the initializer which are
	// then constant for the lifetime of the service.

	// Things that need to be attached to every parser: common information about the type system, functions, etc.
	Common *CommonParserBindle

	// Names/token types of identifiers.
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
	Typenames         dtypes.Set[string]
	nativeInfixes dtypes.Set[token.TokenType]
	lazyInfixes   dtypes.Set[token.TokenType]

	// Used for multiple dispatch.

	// While this is mostly just used by the initializer to construct the function trees (below), it is also used
	// to serialize the API and so may be needed at runtime.
	FunctionTable   FunctionTable  
	// Trees, one for each function identifier, for figuring out how to make function calls given the possibility
	// of multiple dispatch.
	FunctionForest  map[string]*ast.FunctionTree
	
	// Maps names to abstract types. This *is* the type system, at least as far as the compiler knows about it, 
	// because there is a natural partial order on abstract types.
	TypeMap         TypeSys                      
	
	ExternalParsers map[string]*Parser // A map from the name of the external service to the parser of the service. This should be the same as the one in the vm.	
	NamespaceBranch map[string]*ParserData  // Map from the namespaces immediately available to this parser to the parsers they access.
	NamespacePath   string                  // The chain of namespaces that got us to this parser, as a string.
	Private         bool               // Indicates if it's the parser of a private library/external/whatevs.
}

func New(common *CommonParserBindle, source, sourceCode, namespacePath string) *Parser {
	p := &Parser{
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
		Typenames:         make(dtypes.Set[string]),
		nativeInfixes: dtypes.MakeFromSlice([]token.TokenType{
			token.COMMA, token.EQ, token.NOT_EQ, token.WEAK_COMMA, token.ASSIGN, token.GVN_ASSIGN, token.FOR,
			token.GIVEN, token.LBRACK, token.MAGIC_COLON, token.MAGIC_SEMICOLON, token.PIPE, token.MAPPING,
			token.FILTER, token.NAMESPACE_SEPARATOR, token.IFLOG}),
		lazyInfixes: dtypes.MakeFromSlice([]token.TokenType{token.AND,
			token.OR, token.COLON, token.SEMICOLON, token.NEWLINE}),
		FunctionTable:  make(FunctionTable),
		FunctionForest: make(map[string]*ast.FunctionTree), // The logger needs to be able to see service variables and this is the simplest way.
		TypeMap:        make(TypeSys),
		NamespaceBranch:    make(map[string]*ParserData),
		ExternalParsers:    make(map[string]*Parser),
		NamespacePath:      namespacePath,
		Common:             common,
	}
	p.Common.Sources[source] = strings.Split(sourceCode, "\n") // TODO --- something else.
	p.TokenizedCode = lexer.NewRelexer(source, sourceCode)

	for k := range p.Common.Types {
		p.Suffixes.Add(k)
	}
	p.Suffixes.Add("ref")
	p.Suffixes.Add("self")

	p.Infixes.Add("varchar")

	p.Functions.Add("builtin")

	p.pushRParser(p)

	return p
}

// Parses one line of code supplied as a string.
func (p *Parser) ParseLine(source, input string) ast.Node {
	p.ResetAfterError()
	rl := lexer.NewRelexer(source, input)
	p.TokenizedCode = rl
	result := p.ParseTokenizedChunk()
	p.Common.Errors = append(rl.GetErrors(), p.Common.Errors...)
	return result
}

// Shows output of parser for debugging purposes.
func (p *Parser) ParseDump(source, input string) {
	parsedLine := p.ParseLine(source, input)
	if parsedLine == nil {
		fmt.Printf("Parser returns: nil")
	}
	fmt.Printf("Parser returns: %v\n\n", (parsedLine).String())
}

// Some supporting types for the parser and their methods.

// For data that needs to be shared by all parsers. It is initialized when we start initializing a service
// and passed to the first parser, which then passes it down to its children.
type CommonParserBindle struct {
	Types               TypeSys
	InterfaceBacktracks []BkInterface
	Errors              []*err.Error
	IsBroken            bool
	Sources             map[string][]string
}

// Initializes the common parser bindle.
func NewCommonParserBindle() *CommonParserBindle {
	result := CommonParserBindle{Types: NewCommonTypeMap(),
		Errors:              []*err.Error{},                // This is where all the errors emitted by enything end up.
		Sources:             make(map[string][]string),     // Source code --- TODO: remove.
		InterfaceBacktracks: []BkInterface{},               // Although these are only ever used at compile time, they are emited by the `seekFunctionCall` method, which belongs to the compiler.
	}
	return &result
}

// When we dispatch on a function which is semantically available to us because it fulfills an interface, but we 
// haven't compiled it yet, this keeps track of where we backtrack to.
type BkInterface struct {
	Fn   *ast.PrsrFunction
	Addr uint32
}

// Stores parse code chunks for subsequent tokenization.
type ParsedCodeChunks []ast.Node

// Stores information about other parsers. TODO, deprecate.
type ParserData struct {
	Parser         *Parser
	ScriptFilepath string
}

// This is indeed the whole of the type system as the parser sees it, because one abstract type is a subtype
// of another just if all the concrete types making up the former are found in the latter.
type TypeSys map[string]values.AbstractType

func (p *Parser) parseExpression(precedence int) ast.Node {

	if literals.Contains(p.curToken.Type) && literalsAndLParen.Contains(p.peekToken.Type) {
		p.Throw("parse/before/a", &p.curToken, &p.peekToken)
	}
	var leftExp ast.Node
	noNativePrefix := false

	switch p.curToken.Type {

	// These just need a rhs.
	case token.EVAL, token.GLOBAL, token.XCALL:
		leftExp = p.parsePrefixExpression()

	// Remaining prefix-position token types are in alphabetical order.
	case token.BREAK:
		leftExp = p.parseBreak()
	case token.CONTINUE:
		leftExp = p.parseContinue()
	case token.ELSE:
		leftExp = p.parseElse()
	case token.FALSE:
		leftExp = p.parseBooleanLiteral()
	case token.FLOAT:
		leftExp = p.parseFloatLiteral()
	case token.FOR:
		p.NextToken()
		leftExp = p.parseForExpression()
	case token.GOCODE:
		leftExp = p.parseGolangExpression()
	case token.INT:
		leftExp = p.parseIntegerLiteral()
	case token.LBRACK:
		leftExp = p.parseListExpression()
	case token.LPAREN:
		leftExp = p.parseGroupedExpression()
	case token.NOT:
		leftExp = p.parseNativePrefixExpression()
	case token.PRELOG:
		leftExp = p.parsePrelogExpression()
	case token.STRING:
		leftExp = p.parseStringLiteral()
	case token.RANGE:
		leftExp = p.parseNativePrefixExpression()
	case token.RUNE:
		leftExp = p.parseRuneLiteral()
	case token.TRUE:
		leftExp = p.parseBooleanLiteral()
	case token.TRY:
		leftExp = p.parseTryExpression()
	case token.UNWRAP:
		leftExp = p.parseNativePrefixExpression()
	case token.VALID:
		leftExp = p.parseNativePrefixExpression()
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
	// TODO -- why aren't builtin, func, and struct not native prefixes? Possibly func and struct aren't because they're types?
	// 'from' isn't because we want to be able to use it as an infix and 'for' may end up the same way for the same reason.
	if noNativePrefix {
		if p.curToken.Type == token.IDENT {
			if p.curToken.Literal == "builtin" {
				p.curToken.Type = token.BUILTIN
				leftExp = p.parseBuiltInExpression()
				return leftExp
			}
			// Here we step in and deal with things that are functions and values, like the type conversion
			// functions and their associated types. Before we look them up as functions, we want to
			// be sure that they're not in such a position that they're being used as literals.
			if !resolvingParser.isPositionallyFunctional() {
				switch {
				case resolvingParser.TypeExists(p.curToken.Literal):
					leftExp = &ast.TypeLiteral{Token: p.curToken, Value: p.curToken.Literal}
				case resolvingParser.Unfixes.Contains(p.curToken.Literal):
					leftExp = p.parseUnfixExpression()
				case p.topRParser().Bling.Contains(p.curToken.Literal):
					leftExp = &ast.Bling{Token: p.curToken, Value: p.curToken.Literal}
				default:
					leftExp = p.parseIdentifier()
				}
			} else {
				if p.curToken.Literal == "func" {
					leftExp = p.parseLambdaExpression()
					return leftExp
				}
				if p.curToken.Literal == "struct" {
					leftExp = p.parseStructExpression()
					return leftExp
				}
				if p.curToken.Literal == "from" {
					leftExp = p.parseFromExpression()
					return leftExp
				}
				switch {
				case resolvingParser.Prefixes.Contains(p.curToken.Literal) || resolvingParser.Forefixes.Contains(p.curToken.Literal):
					p.pushRParser(resolvingParser)
					leftExp = p.parsePrefixExpression()
					p.popRParser()
				default:
					p.pushRParser(resolvingParser)
					leftExp = p.parseFunctionExpression() // That, at least, is what it is syntactictally.
					p.popRParser()
				}
			}
		} else {
			p.Throw("parse/prefix", &p.curToken)
		}
	}

	for precedence < p.peekPrecedence() {
		for resolvingParser.Suffixes.Contains(p.peekToken.Literal) || resolvingParser.Endfixes.Contains(p.peekToken.Literal) || p.peekToken.Type == token.EMDASH || p.peekToken.Type == token.DOTDOTDOT {
			if p.curToken.Type == token.NOT || p.curToken.Type == token.IDENT && p.curToken.Literal == "-" || p.curToken.Type == token.ELSE {
				p.Throw("parse/before/b", &p.curToken, &p.peekToken)
				return nil
			}
			p.NextToken()
			p.pushRParser(resolvingParser)
			leftExp = p.parseSuffixExpression(leftExp)
			p.popRParser()
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
			case p.curToken.Type == token.FOR:
				leftExp = p.parseForAsInfix(leftExp) // For the (usual) case where the 'for' is inside a 'from' and the leftExp is, or should be, the bound variables of the loop.
			case p.curToken.Type == token.EQ || p.curToken.Type == token.NOT_EQ:
				leftExp = p.parseComparisonExpression(leftExp)
			default:
				p.pushRParser(resolvingParser)
				leftExp = p.parseInfixExpression(leftExp)
				p.popRParser()
			}
		}
	}
	if leftExp == nil {
		if p.curToken.Type == token.EOF {
			p.Throw("parse/line", &p.curToken)
			return nil
		}
		if p.curToken.Literal == "<-|" || p.curToken.Literal == ")" || // TODO --- it's not clear this or the following error can ever actually be thrown.
			p.curToken.Literal == "]" || p.curToken.Literal == "}" {
			p.Throw("parse/close", &p.curToken)
			return nil
		}
		p.Throw("parse/missing", &p.curToken)
		return nil
	}
	return leftExp
}

// Now we have all the functions with names of the form `parseXxxxx`, arranged in alphabetical order.

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

func (p *Parser) parseBooleanLiteral() ast.Node {
	return &ast.BooleanLiteral{Token: p.curToken, Value: p.curTokenIs(token.TRUE)}
}

func (p *Parser) parseBreak() ast.Node {
	if p.isPositionallyFunctional() {
		t := p.curToken
		p.NextToken()                  // Skips the 'break' token
		exp := p.parseExpression(FUNC) // If this is a multiple return, we don't want its elements to be treated as parameters of a function. TODO --- gve 'break' its own node type?
		return &ast.PrefixExpression{t, "break", []ast.Node{exp}, []string{}}
	}
	return &ast.Identifier{Token: p.curToken, Value: "break"}
}

// This is to allow me to use the initializer to pour builtins into the parser's function table.
func (p *Parser) parseBuiltInExpression() ast.Node {
	expression := &ast.BuiltInExpression{}
	expression.Token = p.curToken
	p.NextToken()
	if p.curToken.Type == token.STRING {
		expression.Name = p.curToken.Literal
	} else {
		panic("Expecting a string after 'builtin'.")
	}
	p.NextToken()
	return expression
}

func (p *Parser) parseComparisonExpression(left ast.Node) ast.Node {
	expression := &ast.ComparisonExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Left:     left,
	}
	precedence := p.curPrecedence()
	p.NextToken()
	expression.Right = p.parseExpression(precedence)
	return expression
}

func (p *Parser) parseContinue() ast.Node {
	return &ast.Identifier{Token: p.curToken, Value: "continue"}
}

func (p *Parser) parseElse() ast.Node {
	return &ast.BooleanLiteral{Token: p.curToken, Value: true}
}

// The fact that it is a valid float has been checked by the lexer.
func (p *Parser) parseFloatLiteral() ast.Node {
	fVal, _ := strconv.ParseFloat(p.curToken.Literal, 64)
	return &ast.FloatLiteral{Token: p.curToken, Value: fVal}
}

func (p *Parser) parseForAsInfix(left ast.Node) *ast.ForExpression {
	expression := p.parseForExpression()
	if p.ErrorsExist() {
		return nil
	}
	expression.BoundVariables = left
	return expression
}

func (p *Parser) parseForExpression() *ast.ForExpression {
	p.CurrentNamespace = nil
	expression := &ast.ForExpression{
		Token: p.curToken,
	}
	p.NextToken()
	// We handle the 'for :' as "while true" case.
	if p.curToken.Type == token.COLON {
		p.NextToken()
		expression.Body = p.parseExpression(COLON)
		if p.ErrorsExist() {
			return nil
		}
		return expression
	}

	pieces := p.parseExpression(GIVEN)
	if p.ErrorsExist() {
		return nil
	}
	if pieces.GetToken().Type == token.COLON {
		expression.Body = pieces.(*ast.LazyInfixExpression).Right
		header := pieces.(*ast.LazyInfixExpression).Left
		if header.GetToken().Type == token.MAGIC_SEMICOLON { // If it has one, it should have two.
			leftBitOfHeader := header.(*ast.InfixExpression).Args[0]
			rightBitOfHeader := header.(*ast.InfixExpression).Args[2]
			if leftBitOfHeader.GetToken().Type == token.MAGIC_SEMICOLON {
				expression.Initializer = leftBitOfHeader.(*ast.InfixExpression).Args[0]
				expression.ConditionOrRange = leftBitOfHeader.(*ast.InfixExpression).Args[2]
				expression.Update = rightBitOfHeader
			} else {
				p.Throw("parse/for/a", &expression.Token)
				return nil
			}
		} else {
			expression.ConditionOrRange = header
		}
	} else {
		p.Throw("parse/for/b", &expression.Token)
		return nil
	}
	return expression
}

func (p *Parser) parseFromExpression() ast.Node {
	p.CurrentNamespace = nil
	fromToken := p.curToken
	p.NextToken()
	expression := p.parseExpression(LOWEST)
	if p.ErrorsExist() {
		return nil
	}
	var givenBlock ast.Node
	if expression.GetToken().Type == token.GIVEN {
		givenBlock = expression.(*ast.InfixExpression).Args[2]
		expression = expression.(*ast.InfixExpression).Args[0]
	}
	exp, ok := expression.(*ast.ForExpression)
	if ok {
		exp.Given = givenBlock
		return exp
	}
	p.Throw("parse/from", &fromToken)
	return nil
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

func (p *Parser) parseGolangExpression() ast.Node {
	expression := &ast.GolangExpression{
		Token: p.curToken,
	}
	p.NextToken()
	return expression
}

func (p *Parser) parseGroupedExpression() ast.Node {
	p.NextToken()
	if p.curToken.Type == token.RPAREN { // Then what we must have is an empty tuple.
		return &ast.Nothing{Token: p.curToken}
	}
	exp := p.parseExpression(LOWEST)
	if !p.expectPeek(token.RPAREN) {
		p.NextToken() // Forces emission of the error.
		return nil
	}
	return exp
}

func (p *Parser) parseIdentifier() ast.Node {
	p.CurrentNamespace = nil
	return &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
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

func (p *Parser) parseInfixExpression(left ast.Node) ast.Node {
	p.CurrentNamespace = nil
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
		right := p.parseExpression(FUNC)
		fn := &ast.FuncExpression{Token: newTok}
		expression := &ast.AssignmentExpression{Token: newTok}
		switch left := left.(type) {
		case *ast.PipingExpression:
			if left.GetToken().Literal != "->" {
				p.Throw("parse/inner/a", left.GetToken())
			}
			fn.NameRets = p.RecursivelySlurpReturnTypes(left.Right)
			switch newLeft := left.Left.(type) {
			case *ast.PrefixExpression:
				expression.Left = &ast.Identifier{Token: *newLeft.GetToken(), Value: newLeft.GetToken().Literal}
				fn.NameSig, _ = p.getSigFromArgs(newLeft.Args, "any?")
				fn.Sig = p.MakeAbstractSigFromStringSig(fn.NameSig) // TODO --- elsewhere.
			default:
				p.Throw("parse/inner/b", newLeft.GetToken())
			}
		case *ast.PrefixExpression:
			expression.Left = &ast.Identifier{Token: *left.GetToken(), Value: left.GetToken().Literal}
			fn.NameSig, _ = p.getSigFromArgs(left.Args, "any?")
		default:
			p.Throw("parse/inner/c", left.GetToken())
			return nil
		}
		if right.GetToken().Type == token.GIVEN {
			fn.Body = right.(*ast.InfixExpression).Args[0]
			fn.Given = right.(*ast.InfixExpression).Args[2]
		} else {
			fn.Body = right
		}
		expression.Right = fn
		if fn.Body.GetToken().Type == token.PRELOG && fn.Body.GetToken().Literal == "" {
			fn.Body.(*ast.LogExpression).Value = DescribeFunctionCall(left.GetToken().Literal, &fn.NameSig)
		}
		return expression
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

// Auxiliary fnction to the previous one for describing function calls for logging purposes.
func DescribeFunctionCall(name string, sig *ast.StringSig) string {
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

func (p *Parser) parseIntegerLiteral() ast.Node {
	iVal, _ := strconv.Atoi(p.curToken.Literal)
	return &ast.IntegerLiteral{Token: p.curToken, Value: iVal}
}

func (p *Parser) parseLambdaExpression() ast.Node {
	expression := &ast.FuncExpression{
		Token: p.curToken,
	}
	p.NextToken()
	RHS := p.parseExpression(WEAK_COLON)
	// At this point the root of the RHS should be the colon dividing the function sig from its body.
	root := RHS
	if root.GetToken().Type != token.COLON {
		p.Throw("parse/colon", &p.curToken)
		return nil
	}
	expression.NameSig, _ = p.RecursivelySlurpSignature(root.(*ast.LazyInfixExpression).Left, "any?")
	if p.ErrorsExist() {
		return nil
	}
	expression.Sig = p.MakeAbstractSigFromStringSig(expression.NameSig) // TODO --- elsewhere.
	bodyRoot := root.(*ast.LazyInfixExpression).Right
	if bodyRoot.GetToken().Type == token.GIVEN {
		expression.Body = bodyRoot.(*ast.InfixExpression).Args[0]
		expression.Given = bodyRoot.(*ast.InfixExpression).Args[2]
	} else {
		expression.Body = bodyRoot
	}
	return expression
}

// I.e `and`, `or`, `:`, and `;`.
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

// For things like NOT, UNWRAP, VALID where we don't want to treat it as a function but to evaluate the RHS and then handle it.
func (p *Parser) parseNativePrefixExpression() ast.Node {
	expression := &ast.PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}
	prefix := p.curToken
	p.NextToken()
	right := p.parseExpression(precedences[prefix.Type])
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

func (p *Parser) parseRuneLiteral() ast.Node {
	r, _ := utf8.DecodeRune([]byte(p.curToken.Literal)) // We have already checked that the literal is a any rune at the lexing stage.
	return &ast.RuneLiteral{Token: p.curToken, Value: r}
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

// Function auxiliary to the previous one to get rid of syntactic sugar in streaming expressions.
// Adds "that" after piping, works through namespaces.
func (p *Parser) recursivelyDesugarAst(exp ast.Node) ast.Node {
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

func (p *Parser) parseStringLiteral() ast.Node {
	return &ast.StringLiteral{Token: p.curToken, Value: p.curToken.Literal}
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

func (p *Parser) parseSuffixExpression(left ast.Node) ast.Node {
	p.CurrentNamespace = nil
	expression := &ast.SuffixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Args:     p.recursivelyListify(left),
	}
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

func (p *Parser) parseUnfixExpression() ast.Node {
	p.CurrentNamespace = nil
	return &ast.UnfixExpression{Token: p.curToken, Operator: p.curToken.Literal}
}

// This takes the arguments at the call site of a function and puts them
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

// Functions for keeping track of the `resolving parser`, i.e. the one that knows about the namespace we're in.
func (p *Parser) pushRParser(q *Parser) {
	p.enumResolvingParsers = append(p.enumResolvingParsers, q)
}
func (p *Parser) topRParser() *Parser {
	return p.enumResolvingParsers[len(p.enumResolvingParsers)-1]
}
func (p *Parser) popRParser() {
	p.enumResolvingParsers = p.enumResolvingParsers[1:]
}

// The parser accumulates the names in foo.bar.troz as it goes along. Now we follow the trail of namespaces
// to find which parser should resolve the symbol.
func (p *Parser) getResolvingParser() *Parser {
	lP := p
	for _, name := range p.CurrentNamespace {
		s, ok := lP.NamespaceBranch[name]
		if ok {
			lP = s.Parser
			continue
		}
		p.Throw("parse/namespace/exist", &p.curToken, name)
		return nil
	}
	// We don't need the resolving parser to parse anything but we *do* need to call positionallyFunctional,
	// so it needs the following data to work.
	lP.curToken = p.curToken
	lP.peekToken = p.peekToken
	return lP
}

// Some functions for interacting with a `TokenSupplier`.

// This interface allows the parser to get its supply of tokens either from the relexer directly or from
// a `TokenizedCodeChunk`.
type TokenSupplier interface{ NextToken() token.Token }

// Dumps the contents of a `TokenSupplier` into a string.
func String(t TokenSupplier) string {
	result := ""
	for tok := t.NextToken(); tok.Type != "EOF"; tok = t.NextToken() {
		result = result + fmt.Sprintf("%+v\n", tok)
	}
	return result
}

func (p *Parser) NextToken() {
	p.checkNesting()
	p.SafeNextToken()
}

func (p *Parser) SafeNextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.TokenizedCode.NextToken()
}

// Function auxiliary to `NextToken` which will throw an error if the rules for nesting brackets are violated.
func (p *Parser) checkNesting() {
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

// A function auxiliary to the previous one to check whether a puported pair of brackets matches up.
func checkConsistency(left, right token.Token) bool {
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

// Functions for dealing with Pipefish errors.

func (p *Parser) Throw(errorID string, tok *token.Token, args ...any) {
	c := *tok
	p.Common.Errors = err.Throw(errorID, p.Common.Errors, &c, args...)
}

func (p *Parser) ErrorsExist() bool {
	return len(p.Common.Errors) > 0
}

func (p *Parser) ReturnErrors() string {
	return err.GetList(p.Common.Errors)
}

func (p *Parser) ResetAfterError() {
	p.Common.Errors = []*err.Error{}
	p.CurrentNamespace = []string{}
	p.enumResolvingParsers = []*Parser{p}
}

func newError(ident string, tok *token.Token, args ...any) *err.Error {
	errorToReturn := err.CreateErr(ident, tok, args...)
	errorToReturn.Trace = []*token.Token{tok}
	return errorToReturn
}
