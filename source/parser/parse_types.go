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
var PSEUDOTYPES = dtypes.MakeFromSlice[string]([]string{"clone", "like"})


type typePrecedence = int

const (
	T_LOWEST = iota
	T_OR
	T_AND
	T_SUFFIX
)

func (p *Parser) ParseType(prec typePrecedence) ast.TypeNode {
	var leftExp ast.TypeNode
	if p.curToken.Type != token.IDENT || 
			!(p.TypeExists(p.curToken.Literal) || PSEUDOTYPES.Contains(p.curToken.Literal)) {
		return leftExp
	}
	tok := p.curToken
	// Prefixes
	if p.peekToken.Type == token.LBRACK {
		leftExp = p.parseParamsOrArgs()
	} else {
		leftExp = &ast.TypeWithName{tok, p.curToken.Literal}
		p.NextToken()
	}
	// Infixes
	for prec < p.peekTypePrecedence() && p.curToken.Type == token.IDENT && 
				(p.curToken.Literal == "/" || p.curToken.Literal == "&") {
		infix := p.curToken.Literal
		p.NextToken()
		leftExp = &ast.TypeInfix{tok, infix, leftExp, p.ParseType(prec)}
	}
	// Suffixes
	for p.curToken.Type == token.IDENT && 
			(p.curToken.Literal == "?" || p.curToken.Literal == "!") {
		leftExp = &ast.TypeSuffix{tok, p.curToken.Literal, leftExp}
		p.NextToken()
	}
	return leftExp
}

func (p *Parser) peekTypePrecedence() typePrecedence {
	switch p.curToken.Literal {
	case "/":
		return T_OR 
	case "&":
		return T_AND
	case "?", "!":
		return T_SUFFIX
	default :
		return T_LOWEST
	}
}

func (p *Parser) parseParamsOrArgs() ast.TypeNode {
	nameTok := p.curToken
	p.NextToken() // The one with the name in.
	p.NextToken() // Contains a left square bracket which we already know to be there.
	if p.curToken.Type == token.IDENT && 
			!(p.TypeExists(p.curToken.Literal) || PSEUDOTYPES.Contains(p.curToken.Literal)) {
		return p.parseParams(nameTok)
	}
	println("Calling parseargs; current token is", p.curToken.Literal)
	return p.parseArgs(nameTok)
}

var acceptableTypes = dtypes.MakeFromSlice([]string{"float", "int", "string", "rune", "bool", "type"})

func (p *Parser) parseParams(nameTok token.Token) ast.TypeNode {
	result := ast.TypeWithParameters{nameTok, nameTok.Literal, []*ast.Parameter{}}
	for {
		tok := &p.curToken
		if p.curToken.Type != token.IDENT {
			p.Throw("parse/type/form/c", tok)
			break
		}
		result.Parameters = append(result.Parameters, &ast.Parameter{p.curToken.Literal,""})
		p.NextToken()
		if p.curToken.Type == token.IDENT {
			if acceptableTypes.Contains(p.curToken.Literal) {
				for _, v := range result.Parameters {
					if v.Type == "" {
						v.Type = p.curToken.Literal
					}
				}
			} else { 
				p.Throw("parse/type/form/d", tok)
			}
			p.NextToken()
		}
		if p.curToken.Type == token.COMMA {
			p.NextToken()
			continue
		}
		if p.curToken.Type == token.RBRACK {
			p.NextToken()
			break
		}
		p.Throw("parse/type/form/e", tok)
		break
	}	
	return &result
}

func (p *Parser) parseArgs(nameTok token.Token) ast.TypeNode {
	result := ast.TypeWithArguments{nameTok, nameTok.Literal, []*ast.Argument{}}
	for {
		tok := p.curToken
		var newArg *ast.Argument
		switch p.curToken.Type {
		case token.FLOAT:
			number, _ :=  strconv.ParseFloat(p.curToken.Literal, 64)
			newArg = &ast.Argument{tok, values.FLOAT, number}
		case token.INT:
			number, _ :=  strconv.Atoi(p.curToken.Literal)
			newArg = &ast.Argument{tok, values.INT, number}
		case token.STRING:
			newArg = &ast.Argument{tok, values.STRING, p.curToken.Literal}
		case token.RUNE:
			newArg = &ast.Argument{tok, values.RUNE, p.curToken.Literal}
		case token.IDENT:
			if p.TypeExists(p.curToken.Literal) || PSEUDOTYPES.Contains(p.curToken.Literal) {
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
		if p.curToken.Type == token.COMMA {
			p.NextToken()
			continue
		}
		if p.curToken.Type == token.RBRACK {
			p.NextToken()
			break
		}
		p.Throw("parse/type/form/h", &tok)
		break
	}	
	return &result
}