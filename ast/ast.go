package ast

import (
	"bytes"
	
	"charm/signature"
	"charm/token"
)

// The base Node interface
type Node interface {
	GetToken() token.Token
	TokenLiteral() string
	String() string
}

type Expression struct {
	Token      token.Token // the first token of the expression
	Node Node
}
func (es *Expression) GetToken() token.Token { return es.Token }
func (es *Expression) TokenLiteral() string { return es.Token.Literal }
func (es *Expression) String() string {
	return es.Node.String()
}

type Identifier struct {
	Token token.Token 
	Value string
}

func (i *Identifier) GetToken() token.Token { return i.Token }
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }
func (i *Identifier) String() string       { return i.Value }

type TypeLiteral struct {
	Token token.Token 
	Value string
}

func (t * TypeLiteral) GetToken() token.Token { return t.Token }
func (t *TypeLiteral) TokenLiteral() string { return t.Token.Literal }
func (t *TypeLiteral) String() string       { return t.Value }

type BooleanLiteral struct {
	Token token.Token
	Value bool
}

func (b *BooleanLiteral) GetToken() token.Token { return b.Token }
func (b *BooleanLiteral) TokenLiteral() string { return b.Token.Literal }
func (b *BooleanLiteral) String() string       { return b.Token.Literal }

type IntegerLiteral struct {
	Token token.Token
	Value int64
}

func (il *IntegerLiteral) GetToken() token.Token { return il.Token }
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Literal }
func (il *IntegerLiteral) String() string       { return il.Token.Literal }

type FloatLiteral struct {
	Token token.Token
	Value float64
}

func (fl *FloatLiteral) GetToken() token.Token { return fl.Token }
func (fl *FloatLiteral) TokenLiteral() string { return fl.Token.Literal }
func (fl *FloatLiteral) String() string       { return fl.Token.Literal }

type FuncExpression struct {
	Token token.Token
	Function
}

func (fe *FuncExpression) GetToken() token.Token { return fe.Token }
func (fe *FuncExpression) TokenLiteral() string { return "func" }
func (fe *FuncExpression) String() string { 
	result := "func " + fe.Sig.String() + " : " + fe.Body.String()
	if fe.Given != nil {
		result = "(" + result + ")" + " given : " + "(" + fe.Given.String() + ")"
	}
	
return result }


type StructExpression struct {
	Token token.Token
	Sig signature.Signature
}

func (st *StructExpression) GetToken() token.Token { return st.Token }
func (st *StructExpression) TokenLiteral() string { return "func" }
func (st *StructExpression) String() string { return "struct " + st.Sig.String() }


type PrefixExpression struct {
	Token    token.Token 
	Operator string
	Right    Node
}

func (pe *PrefixExpression) GetToken() token.Token { return pe.Token }
func (pe *PrefixExpression) TokenLiteral() string { return pe.Token.Literal }
func (pe *PrefixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(pe.Operator)
	out.WriteString(" ")
	out.WriteString(pe.Right.String())
	out.WriteString(")")

	return out.String()
}

type UnfixExpression struct {
	Token    token.Token 
	Operator string
	Right    Node
}

func (uf *UnfixExpression) GetToken() token.Token { return uf.Token }
func (uf *UnfixExpression) TokenLiteral() string { return uf.Token.Literal }
func (uf *UnfixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(uf.Operator)
	out.WriteString(" ")
	out.WriteString(uf.Right.String())
	out.WriteString(")")

	return out.String()
}

type InfixExpression struct {
	Token    token.Token 
	Left     Node
	Operator string
	Right    Node
}

func (ie *InfixExpression) GetToken() token.Token { return ie.Token }
func (ie *InfixExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *InfixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(ie.Left.String())
	out.WriteString(" " + ie.Operator + " ")
	out.WriteString(ie.Right.String())
	out.WriteString(")")

	return out.String()
}


type ExecExpression struct {
	Token    token.Token 
	Left     Node
	Right    Node
}

func (ee *ExecExpression) GetToken() token.Token { return ee.Token }
func (ee *ExecExpression) TokenLiteral() string { return ee.Token.Literal }
func (ee *ExecExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(ee.Left.String())
	out.WriteString(" exec ")
	out.WriteString(ee.Right.String())
	out.WriteString(")")

	return out.String()
}


type LazyInfixExpression struct {
	Token    token.Token 
	Left     Node
	Operator string
	Right    Node
}

func (ie *LazyInfixExpression) GetToken() token.Token { return ie.Token }
func (ie *LazyInfixExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *LazyInfixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(ie.Left.String())
	out.WriteString(" " + ie.Operator + " ")
	out.WriteString(ie.Right.String())
	out.WriteString(")")

	return out.String()
}


type AssignmentExpression struct {
	Left     Node
	Right    Node
	Token    token.Token
}

func (ae *AssignmentExpression) GetToken() token.Token { return ae.Token }
func (ae *AssignmentExpression) TokenLiteral() string { return "=" }
func (ae *AssignmentExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(ae.Left.String())
	out.WriteString(" = ")
	out.WriteString(ae.Right.String())
	out.WriteString(")")

	return out.String()
}

type ApplicationExpression struct {
	Left     Node
	Right    Node
	Token    token.Token
}

func (ae *ApplicationExpression) GetToken() token.Token { return ae.Token }
func (ae *ApplicationExpression) TokenLiteral() string { return "(" }
func (ae *ApplicationExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(ae.Left.String())
	out.WriteString(" (")
	out.WriteString(ae.Right.String())
	out.WriteString("))")

	return out.String()
}

type SuffixExpression struct {
	Token    token.Token // The prefix token, e.g. !
	Operator string
	Left    Node
}

func (se *SuffixExpression) GetToken() token.Token { return se.Token }
func (se *SuffixExpression) TokenLiteral() string { return se.Token.Literal }
func (se *SuffixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(se.Left.String()+" ")
	out.WriteString(se.Operator)
	out.WriteString(")")

	return out.String()
}


type ListExpression struct {
	Token token.Token // The [ token
	List    Node
}

func (le *ListExpression) GetToken() token.Token { return le.Token }
func (le *ListExpression) TokenLiteral() string { return "list" }
func (le *ListExpression) String() string {
	var out bytes.Buffer

	out.WriteString("[")
	out.WriteString(le.List.String()+" ")
	out.WriteString("]")

	return out.String()
}

type SetExpression struct {
	Token token.Token // The [ token
	Set    Node
}

func (se *SetExpression) GetToken() token.Token { return se.Token }
func (se *SetExpression) TokenLiteral() string { return "set" }
func (se *SetExpression) String() string {
	var out bytes.Buffer

	out.WriteString("{")
	out.WriteString(se.Set.String()+" ")
	out.WriteString("}")

	return out.String()
}


type IndexExpression struct {
	Token token.Token // The [ token
	Left  Node
	Index Node
}

func (ie *IndexExpression) GetToken() token.Token { return ie.Token }
func (ie *IndexExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *IndexExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(ie.Left.String())
	out.WriteString("[")
	out.WriteString(ie.Index.String())
	out.WriteString("])")

	return out.String()
}

type StringLiteral struct {
	Token token.Token
	Value string
}

func (sl *StringLiteral) GetToken() token.Token { return sl.Token }
func (sl *StringLiteral) TokenLiteral() string { return sl.Token.Literal }
func (sl *StringLiteral) String() string       { return "\"" + sl.Token.Literal + "\""}

type EmptyTuple struct {
	Token token.Token
	Value string
}

func (et *EmptyTuple) GetToken() token.Token { return et.Token }
func (et *EmptyTuple) TokenLiteral() string { return "()" }
func (et *EmptyTuple) String() string       { return "()"}


type BuiltInExpression struct {
	Name string
}

func (bi *BuiltInExpression) GetToken() token.Token { return token.Token{Type: token.BUILTIN, Literal: bi.Name, Line: -1} }
func (bi *BuiltInExpression) TokenLiteral() string { return "builtin \"" + bi.Name + "\"" }
func (bi *BuiltInExpression) String() string       { return "builtin \"" + bi.Name + "\"" }

type GolangExpression struct {
	Token token.Token
	ObjectCode func(args ...any) any
	Raw []bool
	Sig signature.Signature
	ReturnTypes signature.Signature
}

func (ge *GolangExpression) GetToken() token.Token { return ge.Token }
func (ge *GolangExpression) TokenLiteral() string { return "golang expression" }
func (ge *GolangExpression) String() string       { return ge.Token.Literal }


type Function = struct {
	Sig signature.Signature
	Rets signature.Signature
	Body Node
	Given Node
	Cmd bool
	Private bool
}