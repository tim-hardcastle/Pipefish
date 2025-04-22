package ast

import (
	"bytes"

	"github.com/tim-hardcastle/Pipefish/source/token"
	"github.com/tim-hardcastle/Pipefish/source/values"
)

// The base TypeNode interface
type TypeNode interface {
	String() string
}

// Contains basic things like `int` and `string`
type TypeWithName struct {
	Token            token.Token
	Name             string
}

func (tl *TypeWithName) String() string {
	return tl.Name
}

// Contains a parameter for parameterizing types
// The types should be bool, float, int, rune, string or type.
type Parameter struct {
	Name, Type string
}

// Contains parameterized types, e.g. list[T type].
type TypeWithParameters struct {
	Token            token.Token
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
	Token   token.Token
	Type    values.ValueType
	Value   any
}

// Contains types with arguments, e.g. list[int].
type TypeWithArguments struct {
	Token            token.Token
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
			out.WriteRune('\'')
		}
		sep = ", "
	}
	out.WriteString("]")
	return out.String()
}

// Has '/' or '&' as an operator.
type TypeInfix struct {
	Token            token.Token
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
	Token            token.Token
	Operator         string
	Left             TypeNode
}

func (ts *TypeSuffix) String() string {
	var out bytes.Buffer
	out.WriteString(ts.Left.String())
	out.WriteString(ts.Operator)
	return out.String()
}