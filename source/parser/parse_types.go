package parser

import (
	"bytes"
	"strconv"

	"github.com/tim-hardcastle/Pipefish/source/dtypes"
	"github.com/tim-hardcastle/Pipefish/source/token"
	"github.com/tim-hardcastle/Pipefish/source/values"
)

// The descriptions of types are sufficiently complicated that they have their own little parser
// to deal with them.

// The base TypeNode interface
type TypeNode interface {
	String() string
}

// Contains basic things like `int` and `string`
type TypeLiteral struct {
	Token            *token.Token
	Name             string
}

// Things which are not type names but can be used for constructing types or for other
// purposes:
var PSEUDOTYPES = dtypes.MakeFromSlice[string]([]string{"clone", "like"})

func (tl *TypeLiteral) String() string {
	return tl.Name
}

// Contains a parameter for parameterizing types
// The types should be bool, float, int, rune, string or type.
type Parameter struct {
	Name, Type string
}

// Contains parameterized types, e.g. list[T type].
type TypeWithParameters struct {
	Token            *token.Token
	Name             string
	Parameters       []*Parameter
}

func (twp *TypeWithParameters) String() string {
	var out bytes.Buffer
	out.WriteString(twp.Name)
	out.WriteString("[")
	sep := ""
	for _, v := range twp.Parameters {
		out.WriteString(sep)
		out.WriteString(v.Name)
		out.WriteString(" ")
		out.WriteString(v.Type)
		sep = ", "
	}
	out.WriteString("]")
	return out.String()
}

type Argument struct {
	Token   *token.Token
	Type    values.ValueType
	Value   any
}

// Contains types with arguments, e.g. list[int].
type TypeWithArguments struct {
	Token            *token.Token
	Name             string
	Arguments        []*Argument
}

func (twp *TypeWithArguments) String() string {
	var out bytes.Buffer
	out.WriteString(twp.Name)
	out.WriteString("[")
	sep := ""
	for _, v := range twp.Arguments {
		out.WriteString(sep)
		if v.Type == values.STRING {
			out.WriteRune('"')
		}
		if v.Type == values.RUNE {
			out.WriteRune('\'')
		}
		if v.Type == values.TYPE {
			out.WriteString(v.Value.(TypeNode).String())
		} else {
			out.WriteString(v.Token.Literal)
		}
		if v.Type == values.STRING {
			out.WriteRune('"')
		}
		if v.Type == values.RUNE {
			out.WriteRune('"')
		}
		sep = ", "
	}
	out.WriteString("]")
	return out.String()
}

// Has '/' or '&' as an operator.
type TypeInfix struct {
	Token            *token.Token
	Operator         string
	Left, Right      TypeNode
}

func (ti *TypeInfix) String() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(ti.Left.String())
	out.WriteString(" ")
	out.WriteString(ti.Operator)
	out.WriteString(" ")
	out.WriteString(ti.Right.String())
	out.WriteString(")")
	return out.String()
}

// Has '?' or '!' as an operator.
type TypeSuffix struct {
	Token            *token.Token
	Operator         string
	Left             TypeNode
}

func (ts *TypeSuffix) String() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(ts.Left.String())
	out.WriteString(" ")
	out.WriteString(ts.Operator)
	out.WriteString(" ")
	out.WriteString(")")
	return out.String()
}

type typePrecedence = int

const (
	T_LOWEST = iota
	T_OR
	T_AND
	T_SUFFIX
)

func (p *Parser) ParseType(prec typePrecedence) TypeNode {
	var leftExp TypeNode
	if p.curToken.Type != token.IDENT && 
			!(p.TypeExists(p.curToken.Literal) || PSEUDOTYPES.Contains(p.curToken.Literal)) {
		p.Throw("parse/type/form/a", &p.curToken)
	}
	// Prefixes
	if p.peekToken.Type == token.LBRACK {
		leftExp = p.parseParamsOrArgs()
	} else {
		leftExp = &TypeLiteral{&p.curToken, p.curToken.Literal}
		p.NextToken()
	}
	// Infixes
	for prec < p.peekTypePrecedence() && p.curToken.Type == token.IDENT && 
				(p.curToken.Literal == "/" || p.curToken.Literal == "&") {
		leftExp = &TypeInfix{&p.curToken, p.curToken.Literal, leftExp, p.ParseType(prec)}
	}
	// Suffixes
	for p.curToken.Type == token.IDENT && 
			(p.curToken.Literal == "?" || p.curToken.Literal == "!") {
		leftExp = &TypeSuffix{&p.curToken, p.curToken.Literal, leftExp}
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
	}
	panic("No idea.")
}

func (p *Parser) parseParamsOrArgs() TypeNode {
	nameTok := p.curToken
	p.NextToken() // The one with the name in.
	p.NextToken() // Contains a left square bracket which we already know to be there.
	if p.curToken.Type == token.IDENT && 
			!(p.TypeExists(p.curToken.Literal) || PSEUDOTYPES.Contains(p.curToken.Literal)) {
		return p.parseParams(&nameTok)
	}
	return p.parseArgs(&nameTok)
}

var acceptableTypes = dtypes.MakeFromSlice([]string{"float", "int", "string", "rune", "bool", "type"})

func (p *Parser) parseParams(nameTok *token.Token) TypeNode {
	result := TypeWithParameters{nameTok, nameTok.Literal, []*Parameter{}}
	for {
		if p.curToken.Type != token.IDENT {
			p.Throw("parse/type/form/c", &p.curToken)
			break
		}
		result.Parameters = append(result.Parameters, &Parameter{p.curToken.Literal,""})
		p.NextToken()
		if p.curToken.Type == token.IDENT {
			if acceptableTypes.Contains(p.curToken.Literal) {
				for _, v := range result.Parameters {
					if v.Type == "" {
						v.Type = p.curToken.Literal
					}
				}
			} else { 
				p.Throw("parse/type/form/d", &p.curToken)
			}
			p.NextToken()
		}
		if p.curToken.Type == token.COMMA {
			p.NextToken()
			continue
		}
		if p.curToken.Type == token.LBRACK {
			p.NextToken()
			break
		}
		p.Throw("parse/type/form/e", &p.curToken)
		break
	}	
	return &result
}

func (p *Parser) parseArgs(nameTok *token.Token) TypeNode {
	result := TypeWithArguments{nameTok, nameTok.Literal, []*Argument{}}
	for {
		var newArg *Argument
		switch p.curToken.Type {
		case token.FLOAT:
			number, _ :=  strconv.ParseFloat(p.curToken.Literal, 64)
			newArg = &Argument{&p.curToken, values.FLOAT, number}
		case token.INT:
			number, _ :=  strconv.Atoi(p.curToken.Literal)
			newArg = &Argument{&p.curToken, values.INT, number}
		case token.STRING:
			newArg = &Argument{&p.curToken, values.STRING, p.curToken.Literal}
		case token.RUNE:
			newArg = &Argument{&p.curToken, values.RUNE, p.curToken.Literal}
		case token.IDENT:
			switch {
			case p.curToken.Literal == "false":
				newArg = &Argument{&p.curToken, values.BOOL, false}
			case p.curToken.Literal == "true":
				newArg = &Argument{&p.curToken, values.BOOL, true}
			case p.TypeExists(p.curToken.Literal) || PSEUDOTYPES.Contains(p.curToken.Literal):
				newType := p.ParseType(T_LOWEST)
				newArg = &Argument{&p.curToken, values.TYPE, newType}
			default :
				p.Throw("parse/type/form/f", &p.curToken)
			}
		default:
			p.Throw("parse/type/form/g", &p.curToken)
		}
		result.Arguments = append(result.Arguments, newArg)
		p.NextToken()
		if p.curToken.Type == token.COMMA {
			p.NextToken()
			continue
		}
		if p.curToken.Type == token.LBRACK {
			p.NextToken()
			break
		}
		p.Throw("parse/type/form/h", &p.curToken)
		break
	}	
	return &result
}