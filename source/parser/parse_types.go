package parser

import (
	"strconv"

	"github.com/tim-hardcastle/Pipefish/source/ast"
	"github.com/tim-hardcastle/Pipefish/source/dtypes"
	"github.com/tim-hardcastle/Pipefish/source/token"
	"github.com/tim-hardcastle/Pipefish/source/values"
)

// The descriptions of types are sufficiently complicated that they have their own little parser
// to deal with them.

// Things which are not type names but can be used for constructing types or for other
// purposes:
var PSEUDOTYPES = dtypes.MakeFromSlice([]string{"clone", "like"})

type typePrecedence = int

const (
	T_LOWEST = iota
	T_OR
	T_AND
	T_SUFFIX
)

func (p *Parser) IsTypePrefix(s string) bool {
	return s == "..." || (p.TypeExists(s) ||
	PSEUDOTYPES.Contains(s) || p.ParameterizedTypes.Contains(s))
}

func (p *Parser) ParseType(prec typePrecedence) ast.TypeNode {
	if !((p.PeekToken.Type == token.DOTDOTDOT) ||
		(p.PeekToken.Type == token.IDENT && p.IsTypePrefix(p.PeekToken.Literal))) {
		return nil
	}
	p.NextToken()
	return p.ParseTypeFromCurTok(prec)
}

func (p *Parser) ParseTypeFromCurTok(prec typePrecedence) ast.TypeNode {
	var leftExp ast.TypeNode
	tok := p.CurToken
	// Prefixes
	if p.PeekToken.Type == token.LBRACE {
		leftExp = p.parseParamsOrArgs()
	} else {
		if p.CurToken.Type == token.DOTDOTDOT {
			right := p.ParseType(T_LOWEST)
			leftExp = &ast.TypeDotDotDot{tok, right}
		} else {
			leftExp = &ast.TypeWithName{tok, p.CurToken.Literal}
		}
	}
	// Infixes
	for prec < p.peekTypePrecedence() && p.PeekToken.Type == token.IDENT &&
		(p.PeekToken.Literal == "/" || p.PeekToken.Literal == "&") {
		infix := p.PeekToken.Literal
		newPrec := p.peekTypePrecedence()
		p.NextToken()
		leftExp = &ast.TypeInfix{tok, infix, leftExp, p.ParseType(newPrec)}
	}
	// Suffixes
	for p.PeekToken.Type == token.IDENT &&
		(p.PeekToken.Literal == "?" || p.PeekToken.Literal == "!") {
		p.NextToken()
		leftExp = &ast.TypeSuffix{p.CurToken, p.CurToken.Literal, leftExp}
	}
	return leftExp
}

func (p *Parser) peekTypePrecedence() typePrecedence {
	switch p.PeekToken.Literal {
	case "/":
		return T_OR
	case "&":
		return T_AND
	case "?", "!":
		return T_SUFFIX
	default:
		return T_LOWEST
	}
}

func (p *Parser) parseParamsOrArgs() ast.TypeNode {
	nameTok := p.CurToken
	p.NextToken() // The one with the name in.
	// So we're now at the token with the `[`, which we won't skip over because sluriping
	// the type needs to be done with a peek first and a NextToken afterwards.
	if p.PeekToken.Type == token.IDENT &&
		!(p.IsTypePrefix(p.PeekToken.Literal)) {
		p.NextToken()
		return p.parseParams(nameTok)
	}
	result := p.parseArgs(nameTok)
	return result
}

var acceptableTypes = dtypes.MakeFromSlice([]string{"float", "int", "string", "rune", "bool", "type"})

func (p *Parser) parseParams(nameTok token.Token) ast.TypeNode {
	indexTok := p.CurToken
	blank := true
	result := ast.TypeWithParameters{nameTok, nameTok.Literal, []*ast.Parameter{}}
	for {
		tok := &p.CurToken
		if p.CurToken.Type != token.IDENT {
			p.Throw("parse/type/form/c", tok)
			break
		}
		result.Parameters = append(result.Parameters, &ast.Parameter{p.CurToken.Literal, ""})
		blank = blank && p.CurToken.Literal == "_"
		p.NextToken()
		if p.CurToken.Type == token.IDENT {
			if acceptableTypes.Contains(p.CurToken.Literal) {
				for _, v := range result.Parameters {
					if v.Type == "" {
						v.Type = p.CurToken.Literal
					}
				}
			} else {
				p.Throw("parse/type/form/d", tok)
			}
			p.NextToken()
		}
		if p.CurToken.Type == token.COMMA {
			p.NextToken()
			continue
		}
		if p.CurToken.Type == token.RBRACE {
			break
		}
		p.Throw("parse/type/form/e", tok)
		break
	}
	if blank {
		return &ast.TypeWithName{indexTok, result.String()}
	}
	return &result
}

func (p *Parser) parseArgs(nameTok token.Token) ast.TypeNode {
	result := ast.TypeWithArguments{nameTok, nameTok.Literal, []*ast.Argument{}}
	for {
		tok := p.PeekToken
		var newArg *ast.Argument
		switch tok.Type {
		case token.FLOAT:
			number, _ := strconv.ParseFloat(tok.Literal, 64)
			newArg = &ast.Argument{tok, values.FLOAT, number}
		case token.INT:
			number, _ := strconv.Atoi(tok.Literal)
			newArg = &ast.Argument{tok, values.INT, number}
		case token.STRING:
			newArg = &ast.Argument{tok, values.STRING, tok.Literal}
		case token.RUNE:
			newArg = &ast.Argument{tok, values.RUNE, tok.Literal}
		case token.IDENT:
			if p.IsTypePrefix(tok.Literal) {
				newType := p.ParseType(T_LOWEST)
				newArg = &ast.Argument{tok, values.TYPE, newType}
			} else {
				p.Throw("parse/type/form/f", &tok)
			}
		case token.FALSE:
			newArg = &ast.Argument{tok, values.BOOL, false}
		case token.TRUE:
			newArg = &ast.Argument{tok, values.BOOL, true}
		default:
			p.Throw("parse/type/form/g", &tok)
		}
		result.Arguments = append(result.Arguments, newArg)
		if tok.Type != token.IDENT { // In which case parsing the type will have moved us on to the next token.
			p.NextToken()
		}
		if p.PeekToken.Type == token.COMMA {
			p.NextToken()
			continue
		}
		if p.PeekToken.Type == token.RBRACE {
			break
		}
		p.Throw("parse/type/form/h", &tok)
		break
	}
	return &result
}
