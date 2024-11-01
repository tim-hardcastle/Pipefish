package ast

import (
	"bytes"
	"reflect"
	"strconv"

	"pipefish/source/dtypes"
	"pipefish/source/token"
	"pipefish/source/values"
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

func IsBling(node Node) bool {
	_, ok := node.(*Bling)
	return ok
}

// Nodes in alphabetical order. Other structures and functions are in a separate section at the bottom.

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
	Token     token.Token
	Value     string
	Namespace []string
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
	token.Token
	Name string
}

func (bi *BuiltInExpression) Children() []Node       { return []Node{} }
func (bi *BuiltInExpression) GetToken() *token.Token { return &bi.Token }
func (bi *BuiltInExpression) String() string         { return "builtin \"" + bi.Name + "\"" }

type FloatLiteral struct {
	Token token.Token
	Value float64
}

func (fl *FloatLiteral) Children() []Node       { return []Node{} }
func (fl *FloatLiteral) GetToken() *token.Token { return &fl.Token }
func (fl *FloatLiteral) String() string         { return fl.Token.Literal }

type ForExpression struct {
	Token            token.Token
	BoundVariables   Node
	Initializer      Node
	ConditionOrRange Node
	Update           Node
	Body             Node
	Given            Node
}

func (fe *ForExpression) Children() []Node {
	return []Node{fe.BoundVariables, fe.Initializer, fe.ConditionOrRange, fe.Update, fe.Body, fe.Given}
}
func (fe *ForExpression) GetToken() *token.Token { return &fe.Token }
func (fe *ForExpression) String() string {
	var out bytes.Buffer
	if fe.BoundVariables != nil {
		out.WriteString("from ")
		out.WriteString(fe.BoundVariables.String())
		out.WriteString(" ")
	}
	out.WriteString("for ")
	if fe.Initializer != nil {
		out.WriteString(fe.Initializer.String())
		out.WriteString("; ")
	}
	if fe.ConditionOrRange != nil {
		out.WriteString(fe.ConditionOrRange.String())
		if fe.Update != nil {
			out.WriteString("; ")
		}
	}
	if fe.Update != nil {
		out.WriteString(fe.Update.String())
	}
	out.WriteString(" : ")
	out.WriteString(fe.Body.String())
	return out.String()
}

type FuncExpression struct {
	Token token.Token
	PrsrFunction
}

func (fe *FuncExpression) Children() []Node       { return []Node{fe.Body, fe.Given} }
func (fe *FuncExpression) GetToken() *token.Token { return &fe.Token }
func (fe *FuncExpression) String() string {
	result := "func " + fe.NameSig.String() + " : " + fe.Body.String()
	if fe.Given != nil {
		result = "(" + result + ")" + " given : " + "(" + fe.Given.String() + ")"
	}

	return result
}

type GolangExpression struct {
	Token       token.Token
	ObjectCode  func(args ...any) any
	Raw         []bool
	Sig         AstSig
	ReturnTypes AstSig
}

func (ge *GolangExpression) Children() []Node       { return []Node{} }
func (ge *GolangExpression) GetToken() *token.Token { return &ge.Token }
func (ge *GolangExpression) String() string         { return ge.Token.Literal }

type Identifier struct {
	Token     token.Token
	Value     string
	Namespace []string
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
	Token     token.Token
	Operator  string
	Args      []Node
	Namespace []string
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

type Nothing struct {
	Token token.Token
}

func (ne *Nothing) Children() []Node       { return []Node{} }
func (ne *Nothing) GetToken() *token.Token { return &ne.Token }
func (ne *Nothing) String() string         { return "" }

type PrefixExpression struct {
	Token     token.Token
	Operator  string
	Args      []Node
	Namespace []string
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

type RuneLiteral struct {
	Token token.Token
	Value rune
}

func (rl *RuneLiteral) Children() []Node       { return []Node{} }
func (rl *RuneLiteral) GetToken() *token.Token { return &rl.Token }
func (rl *RuneLiteral) String() string         { return strconv.QuoteRune(rl.Value) }

type StringLiteral struct {
	Token token.Token
	Value string
}

func (sl *StringLiteral) Children() []Node       { return []Node{} }
func (sl *StringLiteral) GetToken() *token.Token { return &sl.Token }
func (sl *StringLiteral) String() string         { return "\"" + sl.Token.Literal + "\"" }

type StructExpression struct {
	Token token.Token
	Sig   AstSig
}

func (st *StructExpression) Children() []Node       { return []Node{} }
func (st *StructExpression) GetToken() *token.Token { return &st.Token }
func (st *StructExpression) String() string         { return "struct " + st.Sig.String() }

type SuffixExpression struct {
	Token     token.Token
	Operator  string
	Args      []Node
	Namespace []string
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
	out.WriteString(" ")
	out.WriteString(se.Operator)
	out.WriteString(")")
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
	Token     token.Token
	Value     string
	Namespace []string
}

func (t *TypeLiteral) Children() []Node       { return []Node{} }
func (t *TypeLiteral) GetToken() *token.Token { return &t.Token }
func (t *TypeLiteral) String() string         { return t.Value }

type UnfixExpression struct {
	Token     token.Token
	Operator  string
	Namespace []string
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

func GetVariablesFromLhsAndRhsOfAssignments(n Node) (dtypes.Set[string], dtypes.Set[string]) { // This slurps the variable usage out of `given` blocks specifically.
	switch n := n.(type) {
	case *AssignmentExpression:
		return GetVariableNames(n.Left), GetVariableNames(n.Right)
	case *LazyInfixExpression: // I.e. the ';' dividing asignments in a 'given' block.
		lhs1, rhs1 := GetVariablesFromLhsAndRhsOfAssignments(n.Left)
		lhs2, rhs2 := GetVariablesFromLhsAndRhsOfAssignments(n.Right)
		lhs1.AddSet(lhs2)
		rhs1.AddSet(rhs2)
		return lhs1, lhs2
	default:
		return dtypes.Set[string]{}, dtypes.Set[string]{}
	}
}

// We want to extract the variables used in a given node. However, the parameters of a lambda, or its locals, don't count as "used", only its captures.
func GetVariableNames(n Node) dtypes.Set[string] {
	result := dtypes.Set[string]{}
	if n == nil {
		return result
	}
	switch n := n.(type) {
	case *Identifier:
		return result.Add(n.Value)
	case *FuncExpression:
		params := dtypes.Set[string]{}
		for _, pair := range n.Sig {
			params.Add(pair.VarName)
		}
		// We find all the identifiers that we declare in the 'given' block.
		locals, rhs := GetVariablesFromLhsAndRhsOfAssignments(n.Given)
		// Find all the variable names in the body.
		bodyNames := GetVariableNames(n.Body)
		rhs.AddSet(bodyNames)
		return rhs.SubtractSet(params).SubtractSet(locals)
	default:
		for _, v := range n.Children() {
			result.AddSet(GetVariableNames(v))
		}
		return result
	}
}

// This works with the following function to find dependencies in a 'given' block.
func GetPrefixesFromLhsAndRhsOfAssignments(n Node) (dtypes.Set[string], dtypes.Set[string]) {
	switch n := n.(type) {
	case *AssignmentExpression:
		return GetVariableNames(n.Left), GetPrefixes(n.Right)
	case *LazyInfixExpression:
		lhs1, rhs1 := GetPrefixesFromLhsAndRhsOfAssignments(n.Left)
		lhs2, rhs2 := GetPrefixesFromLhsAndRhsOfAssignments(n.Right)
		lhs1.AddSet(lhs2)
		rhs1.AddSet(rhs2)
		return lhs1, lhs2
	default:
		return dtypes.Set[string]{}, dtypes.Set[string]{}
	}
}

// We want to extract the prefixes used in a 'given' node.
func GetPrefixes(n Node) dtypes.Set[string] {
	result := dtypes.Set[string]{}
	switch n := n.(type) {
	case *PrefixExpression:
		for _, v := range n.Children() {
			result.AddSet(GetPrefixes(v))
		}
		return result.Add(n.Operator)
	case *FuncExpression:
		params := dtypes.Set[string]{}
		for _, pair := range n.Sig {
			params.Add(pair.VarName)
		}
		// We find all the identifiers that we declare in the 'given' block of the lambda.
		locals, rhs := GetPrefixesFromLhsAndRhsOfAssignments(n.Given)
		// Find all the variable names in the body.
		bodyNames := GetPrefixes(n.Body)
		rhs.AddSet(bodyNames)
		return rhs.SubtractSet(params).SubtractSet(locals)
	default:
		for _, v := range n.Children() {
			result.AddSet(GetPrefixes(v))
		}
		return result
	}
}

// TODO --- this is unDRYly like the above functions and could be coalesced with them by making the types of nodes to be captured parameters of the function.
func ExtractAllNames(node Node) dtypes.Set[string] {
	result := dtypes.Set[string]{}
	switch n := node.(type) {
	case nil:
		return result
	case *PrefixExpression:
		for _, v := range n.Children() {
			result.AddSet(ExtractAllNames(v))
		}
		return result.Add(n.Operator)
	case *InfixExpression:
		for _, v := range n.Children() {
			result.AddSet(ExtractAllNames(v))
		}
		return result.Add(n.Operator)
	case *SuffixExpression:
		for _, v := range n.Children() {
			result.AddSet(ExtractAllNames(v))
		}
		return result.Add(n.Operator)
	case *UnfixExpression:
		return result.Add(n.Operator)
	case *GolangExpression:
		return result.Add(n.GetToken().Literal)
	case *Identifier:
		return result.Add(n.Value)
	case *FuncExpression:
		params := dtypes.Set[string]{}
		for _, pair := range n.Sig {
			params.Add(pair.VarName)
		}
		// We find all the identifiers that we declare in the 'given' block of the lambda.
		locals, rhs := ExtractNamesFromLhsAndRhsOfGivenBlock(n.Given)
		// Find all the variable names in the body.
		bodyNames := ExtractAllNames(n.Body)
		rhs.AddSet(bodyNames)
		return rhs.SubtractSet(params).SubtractSet(locals)
	default:
		for _, v := range n.Children() {
			result.AddSet(ExtractAllNames(v))
		}
		return result
	}
}

func ExtractNamesFromLhsAndRhsOfGivenBlock(n Node) (dtypes.Set[string], dtypes.Set[string]) {
	switch n := n.(type) {
	case *AssignmentExpression:
		return GetVariableNames(n.Left), ExtractAllNames(n.Right)
	case *LazyInfixExpression:
		lhs1, rhs1 := ExtractNamesFromLhsAndRhsOfGivenBlock(n.Left)
		lhs2, rhs2 := ExtractNamesFromLhsAndRhsOfGivenBlock(n.Right)
		lhs1.AddSet(lhs2)
		rhs1.AddSet(rhs2)
		return lhs1, rhs1
	default:
		return dtypes.Set[string]{}, dtypes.Set[string]{}
	}
}

type PrsrFunction struct {
	Sig      ParserSig    // The signature of the function, represented in a form where the types are given as abstract types.
	NameSig  AstSig       // The same, but with names of types instead of abstract types.
	RetsSig  ParserSig    // The return types: nil if not supplied.
	NameRets AstSig       // The same, but with names of types instead of abstract types.
	Body     Node         // The body of the function.
	Given    Node         // The 'given' block: nil if there isn't one.
	Cmd      bool         // Whether it's a command or not.
	Private  bool         // Whether it's private or not.
	Number   uint32       // The order in which the function was compiled by the vmMaker. Initialized as DUMMY.
	Position uint32       // PREFIX, INFIX, SUFFIX.
	Tok      *token.Token // Where it was declared.
}

type FunctionTree = struct { // Contains the start of a function tree plus the things all the functions with the same name have in common.
	Tree     *FnTreeNode
	RefCount int // For reasons, the reference variables in a function's sig must (a) come at the start (b) be of the same number for each of the overloaded variants of the function.
}

type FnTreeNode struct {
	Fn     *PrsrFunction
	Branch []*TypeNodePair
}

type TypeNodePair struct { // This exists because we need an *ordered* collection of type-node pairs.
	Type     values.AbstractType
	IsVararg bool
	Node     *FnTreeNode
}

func (tree FnTreeNode) String() string {
	result := "["
	for i, v := range tree.Branch {
		result = result + v.Type.String()
		if v.Node.Fn != nil {
			result = result + "func " + v.Node.Fn.NameSig.String()
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
		result = result + "\n" + indent + v.Type.String()
		if v.Node.Fn != nil {
			result = result + "func" + v.Node.Fn.NameSig.String()
		} else {
			result = result + v.Node.IndentString(indent+"    ")
		}
	}
	return result
}
