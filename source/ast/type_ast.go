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

// TODO --- will this work if remived 'cos of them all being the same object?
func IsAnyNullableType(t TypeNode) bool {
	if t, ok := t.(*TypeSuffix); ok && t.Operator == "?" {
		u, ok := t.Left.(*TypeWithName)
		return ok && u.Name == "any"
	}
	return false
}

// Contains basic things like `int` and `string`
type TypeWithName struct {
	Token token.Token
	Name  string
}

func (tl *TypeWithName) String() string {
	return tl.Name
}

// Contains a parameter for parameterizing types
// The types should be bool, float, int, rune, string or type.
type Parameter struct {
	Name string
	Type string
}

// Contains parameterized types, e.g. list[T type].
type TypeWithParameters struct {
	Token      token.Token
	Name       string
	Parameters []*Parameter
}

func (twp *TypeWithParameters) String() string {
	var out bytes.Buffer
	out.WriteString(twp.Name)
	out.WriteString("{")
	sep := ""
	for _, v := range twp.Parameters {
		out.WriteString(sep)
		out.WriteString(v.Name)
		out.WriteString(" ")
		out.WriteString(v.Type)
		sep = ", "
	}
	out.WriteString("}")
	return out.String()
}

type Argument struct {
	Token token.Token
	Type  values.ValueType
	Value any
}

// Contains types with arguments, e.g. list[int].
type TypeWithArguments struct {
	Token     token.Token
	Name      string
	Arguments []*Argument
}

// TODO, we can probably replace the Arguments field with just this.
func (twa *TypeWithArguments) Values() []values.Value {
	result := []values.Value{}
	for _, arg := range twa.Arguments {
		result = append(result, values.Value{arg.Type, arg.Value})
	}
	return result
}

func (twa *TypeWithArguments) String() string {
	var out bytes.Buffer
	out.WriteString(twa.Name)
	out.WriteString("{")
	sep := ""
	for _, v := range twa.Arguments {
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
	out.WriteString("}")
	return out.String()
}

// Has '/' or '&' as an operator.
type TypeInfix struct {
	Token       token.Token
	Operator    string
	Left, Right TypeNode
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
	Token    token.Token
	Operator string
	Left     TypeNode
}

func (ts *TypeSuffix) String() string {
	var out bytes.Buffer
	out.WriteString(ts.Left.String())
	out.WriteString(ts.Operator)
	return out.String()
}

// Has '...' as an operator.
type TypeDotDotDot struct {
	Token token.Token
	Right TypeNode
}

func (td *TypeDotDotDot) String() string {
	var out bytes.Buffer
	out.WriteString("...")
	if td.Right != nil {
		out.WriteString(td.Right.String())
	}
	return out.String()
}

func MakeAstTypeFrom(s string) TypeNode {
	return &TypeWithName{token.Token{}, s}
}

var (
	DOTDOTDOT_PAIR                 = &TypeDotDotDot{token.Token{}, &TypeWithName{token.Token{}, "pair"}}
	ANY_NULLABLE_TYPE_AST          = &TypeSuffix{token.Token{}, "?", &TypeWithName{token.Token{}, "any"}}
	DOTDOTDOT_ANY_NULLABLE         = &TypeDotDotDot{token.Token{}, ANY_NULLABLE_TYPE_AST}
	ANY_NULLABLE_TYPE_AST_OR_ERROR = &TypeSuffix{token.Token{}, "!", &TypeSuffix{token.Token{}, "?", &TypeWithName{token.Token{}, "any"}}}
	STRUCT_TYPE_AST       		   = &TypeWithName{token.Token{}, "struct"}
	INFERRED_TYPE_AST              = &TypeWithName{token.Token{}, "*inferred*"}
	DEFAULT_TYPE_AST               = &TypeWithName{token.Token{}, "*default*"}
	DUMMY_TYPE_AST               = &TypeWithName{token.Token{}, "*dummy*"}
	TUPLE_TYPE_AST                 = &TypeWithName{token.Token{}, "tuple"}
	ERROR_OR_UNWRAPPED_ERROR       = &TypeSuffix{token.Token{}, "!", &TypeWithName{token.Token{}, "ERROR"}}
)

// Contains bling for when the parser needs to treat it as a type.
type TypeBling struct {
	Token token.Token
	Bling string
}

func (tb *TypeBling) String() string {
	return tb.Bling
}

func AsBling(s string) TypeNode {
	return &TypeBling{token.Token{}, s}
}

func IsRef(t TypeNode) bool {
	switch t := t.(type) {
	case *TypeWithName:
		return t.Name == "ref"
	case *TypeWithArguments:
		return t.Name == "ref"
	default:
		return false
	}
}

func IsVarargs(node TypeNode) bool {
	_, ok := node.(*TypeDotDotDot)
	return ok
}

func IsAstBling(node TypeNode) bool {
	_, ok := node.(*TypeBling)
	return ok
}

func Is(node TypeNode, s string) bool {
	t, ok := node.(*TypeWithName)
	return ok && t.Name == s
}