package ast

import (
	"bytes"
	"reflect"

	"pipefish/source/set"
	"pipefish/source/signature"
	"pipefish/source/token"
)

// The base Node interface
type Node interface {
	Children() []Node
	GetToken() *token.Token
	String() string
}

type Callable interface {
	Node
	GetArgs() []Node
}

// Nodes in alphabetical order. Other structures and functions are in a separate section at the bottom.

type ApplicationExpression struct {
	Left  Node
	Right Node
	Token token.Token
}

func (ae *ApplicationExpression) Children() []Node       { return []Node{ae.Left, ae.Right} }
func (ae *ApplicationExpression) GetToken() *token.Token { return &ae.Token }
func (ae *ApplicationExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(ae.Left.String())
	out.WriteString(" (")
	out.WriteString(ae.Right.String())
	out.WriteString("))")

	return out.String()
}

func (ae *AssignmentExpression) Children() []Node { return []Node{ae.Left, ae.Right} }

type AssignmentExpression struct {
	Left  Node
	Right Node
	Token token.Token
}

func (ae *AssignmentExpression) GetToken() *token.Token { return &ae.Token }
func (ae *AssignmentExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(ae.Left.String())
	out.WriteString(" = ")
	out.WriteString(ae.Right.String())
	out.WriteString(")")

	return out.String()
}

type Bling struct {
	Token token.Token
	Value string
}

func (bl *Bling) Children() []Node       { return []Node{} }
func (bl *Bling) GetToken() *token.Token { return &bl.Token }
func (bl *Bling) String() string         { return bl.Value }

type BooleanLiteral struct {
	Token token.Token
	Value bool
}

func (b *BooleanLiteral) Children() []Node       { return []Node{} }
func (b *BooleanLiteral) GetToken() *token.Token { return &b.Token }
func (b *BooleanLiteral) String() string         { return b.Token.Literal }

type BuiltInExpression struct {
	Name string
}

func (bi *BuiltInExpression) Children() []Node { return []Node{} }
func (bi *BuiltInExpression) GetToken() *token.Token {
	return &token.Token{Type: token.BUILTIN, Literal: bi.Name, Line: -1}
}
func (bi *BuiltInExpression) String() string { return "builtin \"" + bi.Name + "\"" }

type EmptyTuple struct {
	Token token.Token
	Value string
}

func (et *EmptyTuple) Children() []Node       { return []Node{} }
func (et *EmptyTuple) GetToken() *token.Token { return &et.Token }
func (et *EmptyTuple) String() string         { return "()" }

type FloatLiteral struct {
	Token token.Token
	Value float64
}

func (fl *FloatLiteral) Children() []Node       { return []Node{} }
func (fl *FloatLiteral) GetToken() *token.Token { return &fl.Token }
func (fl *FloatLiteral) String() string         { return fl.Token.Literal }

type FuncExpression struct {
	Token token.Token
	Function
}

func (fe *FuncExpression) Children() []Node       { return []Node{fe.Body, fe.Given} }
func (fe *FuncExpression) GetToken() *token.Token { return &fe.Token }
func (fe *FuncExpression) String() string {
	result := "func " + fe.Sig.String() + " : " + fe.Body.String()
	if fe.Given != nil {
		result = "(" + result + ")" + " given : " + "(" + fe.Given.String() + ")"
	}

	return result
}

type GolangExpression struct {
	Token       token.Token
	ObjectCode  func(args ...any) any
	Raw         []bool
	Sig         signature.Signature
	ReturnTypes signature.Signature
}

func (ge *GolangExpression) Children() []Node       { return []Node{} }
func (ge *GolangExpression) GetToken() *token.Token { return &ge.Token }
func (ge *GolangExpression) String() string         { return ge.Token.Literal }

type Identifier struct {
	Token token.Token
	Value string
}

func (i *Identifier) Children() []Node       { return []Node{} }
func (i *Identifier) GetToken() *token.Token { return &i.Token }
func (i *Identifier) String() string         { return i.Value }

type IndexExpression struct {
	Token token.Token // The [ token
	Left  Node
	Index Node
}

func (ie *IndexExpression) Children() []Node       { return []Node{ie.Left, ie.Index} }
func (ie *IndexExpression) GetToken() *token.Token { return &ie.Token }
func (ie *IndexExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(ie.Left.String())
	out.WriteString("[")
	out.WriteString(ie.Index.String())
	out.WriteString("])")

	return out.String()
}

type InfixExpression struct {
	Token    token.Token
	Operator string
	Args     []Node
}

func (ie *InfixExpression) Children() []Node       { return ie.Args }
func (ie *InfixExpression) GetArgs() []Node        { return ie.Args }
func (ie *InfixExpression) GetToken() *token.Token { return &ie.Token }
func (ie *InfixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	for i, v := range ie.Args {
		out.WriteString(v.String())
		if i < (len(ie.Args)-1) && !(reflect.TypeOf(v) == reflect.TypeOf(&Bling{})) &&
			!(reflect.TypeOf(ie.Args[i+1]) == reflect.TypeOf(&Bling{})) {
			out.WriteString(",")
		}
		if i < (len(ie.Args) - 1) {
			out.WriteString(" ")
		}
	}
	out.WriteString(")")

	return out.String()
}

type IntegerLiteral struct {
	Token token.Token
	Value int
}

func (il *IntegerLiteral) Children() []Node       { return []Node{} }
func (il *IntegerLiteral) GetToken() *token.Token { return &il.Token }
func (il *IntegerLiteral) String() string         { return il.Token.Literal }

type LazyInfixExpression struct {
	Token    token.Token
	Left     Node
	Operator string
	Right    Node
}

func (ie *LazyInfixExpression) Children() []Node       { return []Node{ie.Left, ie.Right} }
func (ie *LazyInfixExpression) GetToken() *token.Token { return &ie.Token }
func (ie *LazyInfixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(ie.Left.String())
	out.WriteString(" " + ie.Operator + " ")
	out.WriteString(ie.Right.String())
	out.WriteString(")")

	return out.String()
}

type ListExpression struct {
	Token token.Token // The [ token
	List  Node
}

func (le *ListExpression) Children() []Node       { return []Node{le.List} }
func (le *ListExpression) GetToken() *token.Token { return &le.Token }
func (le *ListExpression) String() string {
	var out bytes.Buffer

	out.WriteString("[")
	if le.List == nil {
		out.WriteString("<nil list! error! bad!>")
	}
	out.WriteString(le.List.String() + " ")
	out.WriteString("]")

	return out.String()
}

type LogExpression struct {
	Token token.Token
	Value string
	Left  Node
	Right Node
}

func (le *LogExpression) Children() []Node       { return []Node{le.Left, le.Right} }
func (le *LogExpression) GetToken() *token.Token { return &le.Token }
func (le *LogExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	if le.Left != nil {
		out.WriteString(le.Left.String())
	}
	out.WriteString(") \\ ")
	out.WriteString(le.Value)
	if le.Right != nil {
		out.WriteString(le.Right.String())
	}
	return out.String()
}

type LoopExpression struct {
	Token token.Token
	Code  Node
}

func (le *LoopExpression) Children() []Node       { return []Node{le.Code} }
func (le *LoopExpression) GetToken() *token.Token { return &le.Token }
func (le *LoopExpression) String() string {
	var out bytes.Buffer

	out.WriteString("loop (")
	out.WriteString(le.Code.String())
	out.WriteString(")")

	return out.String()
}

type Nothing struct {
	Token token.Token
}

func (ne *Nothing) Children() []Node       { return []Node{} }
func (ne *Nothing) GetToken() *token.Token { return &ne.Token }
func (ne *Nothing) String() string         { return "" }

type PrefixExpression struct {
	Token    token.Token
	Operator string
	Args     []Node
}

func (pe *PrefixExpression) Children() []Node       { return pe.Args }
func (pe *PrefixExpression) GetArgs() []Node        { return pe.Args }
func (pe *PrefixExpression) GetToken() *token.Token { return &pe.Token }
func (pe *PrefixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(pe.Operator)
	out.WriteString(" ")
	for i, v := range pe.Args {
		out.WriteString(v.String())
		if i < (len(pe.Args)-1) && !(reflect.TypeOf(v) == reflect.TypeOf(&Bling{})) &&
			!(reflect.TypeOf(pe.Args[i+1]) == reflect.TypeOf(&Bling{})) {
			out.WriteString(",")
		}
		if i < (len(pe.Args) - 1) {
			out.WriteString(" ")
		}
	}
	out.WriteString(")")

	return out.String()
}

type SetExpression struct {
	Token token.Token // The { token
	Set   Node
}

func (se *SetExpression) Children() []Node       { return []Node{se.Set} }
func (se *SetExpression) GetToken() *token.Token { return &se.Token }
func (se *SetExpression) String() string {
	var out bytes.Buffer

	out.WriteString("{")
	out.WriteString(se.Set.String() + " ")
	out.WriteString("}")

	return out.String()
}

type PipingExpression struct {
	Token    token.Token
	Left     Node
	Operator string
	Right    Node
}

func (se *PipingExpression) Children() []Node       { return []Node{se.Left, se.Right} }
func (se *PipingExpression) GetToken() *token.Token { return &se.Token }
func (se *PipingExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(se.Left.String())
	out.WriteString(" " + se.Operator + " ")
	out.WriteString(se.Right.String())
	out.WriteString(")")

	return out.String()
}

type StringLiteral struct {
	Token token.Token
	Value string
}

func (sl *StringLiteral) Children() []Node       { return []Node{} }
func (sl *StringLiteral) GetToken() *token.Token { return &sl.Token }
func (sl *StringLiteral) String() string         { return "\"" + sl.Token.Literal + "\"" }

type StructExpression struct {
	Token token.Token
	Sig   signature.Signature
}

func (st *StructExpression) Children() []Node       { return []Node{} }
func (st *StructExpression) GetToken() *token.Token { return &st.Token }
func (st *StructExpression) String() string         { return "struct " + st.Sig.String() }

type SuffixExpression struct {
	Token    token.Token
	Operator string
	Args     []Node
}

func (se *SuffixExpression) Children() []Node       { return se.Args }
func (se *SuffixExpression) GetArgs() []Node        { return se.Args }
func (se *SuffixExpression) GetToken() *token.Token { return &se.Token }
func (se *SuffixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	for i, v := range se.Args {
		out.WriteString(v.String())
		if i < (len(se.Args)-1) && !(reflect.TypeOf(v) == reflect.TypeOf(&Bling{})) &&
			!(reflect.TypeOf(se.Args[i+1]) == reflect.TypeOf(&Bling{})) {
			out.WriteString(",")
		}
		if i < (len(se.Args) - 1) {
			out.WriteString(" ")
		}
	}
	out.WriteString(") ")
	out.WriteString(se.Operator)
	return out.String()
}

type TryExpression struct {
	Token   token.Token
	VarName string
	Right   Node
}

func (t *TryExpression) Children() []Node       { return []Node{t.Right} }
func (t *TryExpression) GetToken() *token.Token { return &t.Token }
func (t *TryExpression) String() string {
	if t.VarName != "" {
		return "try"
	} else {
		return "try " + t.VarName
	}

}

type TypeLiteral struct {
	Token token.Token
	Value string
}

func (t *TypeLiteral) Children() []Node       { return []Node{} }
func (t *TypeLiteral) GetToken() *token.Token { return &t.Token }
func (t *TypeLiteral) String() string         { return t.Value }

type UnfixExpression struct {
	Token    token.Token
	Operator string
}

func (uf *UnfixExpression) Children() []Node       { return []Node{} }
func (uf *UnfixExpression) GetToken() *token.Token { return &uf.Token }
func (uf *UnfixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(uf.Operator)
	out.WriteString(")")

	return out.String()
}

func (uf *UnfixExpression) GetArgs() []Node { return []Node{} }

// And other useful stuff.

func GetLhsOfAssignments(n Node) set.Set[string] {
	switch n := n.(type) {
	case *AssignmentExpression:
		return GetVariableNames(n.Left)
	case *LazyInfixExpression:
		result := GetLhsOfAssignments(n.Left)
		result.AddSet(GetLhsOfAssignments(n.Right))
		return result
	default:
		return set.Set[string]{}
	}
}

func GetVariableNames(n Node) set.Set[string] {
	result := set.Set[string]{}
	children := recursiveChildren(n)
	for _, v := range children {
		switch ident := v.(type) {
		case *Identifier:
			result.Add(ident.Value)
		}
	}
	return result
}

func recursiveChildren(n Node) []Node {
	result := n.Children()
	for _, v := range n.Children() {
		result = append(result, recursiveChildren(v)...)
	}
	return result
}

type Function = struct {
	Sig     signature.Signature
	Rets    signature.Signature
	Body    Node
	Given   Node
	Cmd     bool
	Private bool
	Number  uint32
}

type FunctionGroup = struct { // Contains the start of a function tree plus the things all the functions with the same name have in common.
	Tree     *FnTreeNode
	RefCount int
}

type FnTreeNode struct {
	Fn     *Function
	Branch []*TypeNodePair
}

type TypeNodePair struct { // This exists because we need an *ordered* collection of type-node pairs.
	TypeName string
	Node     *FnTreeNode
}

func (tree FnTreeNode) String() string {
	result := "["
	for i, v := range tree.Branch {
		result = result + v.TypeName
		if v.Node.Fn != nil {
			result = result + "func " + v.Node.Fn.Sig.String()
		} else {
			result = result + v.Node.String()
		}
		if i < len(tree.Branch)-1 {
			result = result + ", "
		}
	}
	return result + "]"
}

func (tree FnTreeNode) IndentString(indent string) string {
	result := ""
	for _, v := range tree.Branch {
		result = result + "\n" + indent + v.TypeName
		if v.Node.Fn != nil {
			result = result + "func " + v.Node.Fn.Sig.String()
		} else {
			result = result + v.Node.IndentString(indent+"    ")
		}
	}
	return result
}
