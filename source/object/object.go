package object

import (
	"bytes"
	"fmt"
	"hash/fnv"
	"strings"

	"charm/source/ast"
	"charm/source/signature"
	"charm/source/text"
	"charm/source/token"
)

////////////////////////////
// Constants
////////////////////////////

type View int

const (
	ViewStdOut View = iota
	ViewCharmLiteral
)

type ObjectType string

const (
	BLING_OBJ       = "bling"
	BOOLEAN_OBJ     = "bool"
	BUILTIN_OBJ     = "BUILTIN"
	CODE_OBJ        = "code"
	ERROR_OBJ       = "error"
	FLOAT_OBJ       = "float64"
	FUNC_OBJ        = "func"
	HASH_OBJ        = "map"
	INTEGER_OBJ     = "int"
	LABEL_OBJ       = "label"
	LIST_OBJ        = "list"
	PAIR_OBJ        = "pair"
	RETURN_OBJ      = "return"
	SET_OBJ         = "set"
	STRING_OBJ      = "string"
	STRUCT_OBJ      = "struct"
	STRUCTDEF_OBJ   = "structdef"
	SUCCESSFUL_OBJ  = "successful assignment"
	TUPLE_OBJ       = "tuple"
	TYPE_OBJ        = "type"
	UNSATISFIED_OBJ = "unsatisfied conditional"
)

///////////////////////////
// Interfaces
///////////////////////////

type HashKey struct {
	Type  ObjectType
	Value uint64
}

type Hashable interface {
	HashKey() HashKey
	Inspect(view View) string
	Type() ObjectType
}

type Object interface {
	DeepCopy() Object
	Type() ObjectType
	Inspect(view View) string
}

///////////////////////////
// Charm object types
///////////////////////////

// Objects are alphabetized, as in the constants section above.

// The "bling" type is internal: it is the type assigned to those parts of a Fancy Function
// Definition that are decorative: forefixes, midfixes, endfixes.

// The DeepCopy method is a stopgap until I have persistent data structures. Its name is often a lie in terms of its
// implementation, in that it's only necessary to actually do a deep copy, or indeed a copy of any sort, if we have operations
// returning an updated version of the object. Most often we don't, and so 'DeepCopy' can in fact just return the object. We need to
// perform an actual deep copy for container types, however, because of 'with' and the slice operator.

type Bling struct {
	Value string
}
func (b *Bling) DeepCopy() Object {return b}
func (b *Bling) Type() ObjectType         { return BLING_OBJ }
func (b *Bling) Inspect(view View) string { return b.Value }

// This just wraps around Go's bool type, plus makes it hashable.
type Boolean struct {
	Value bool
}

func (b *Boolean) DeepCopy() Object {return b}
func (b *Boolean) Type() ObjectType         { return BOOLEAN_OBJ }
func (b *Boolean) Inspect(view View) string { return fmt.Sprintf("%t", b.Value) }
func (b *Boolean) HashKey() HashKey {
	var value uint64

	if b.Value {
		value = 1
	} else {
		value = 0
	}

	return HashKey{Type: b.Type(), Value: value}
}

// Exists only to be passed out of builtin.go and then tranformed into a regular erro.
// TODO --- find out why/if it was necessary to decorate the errors from the builtins like this instead
// of them generating normal errors.
type BuiltError struct {
	Ident string
	Info []any
}

// The 'code' type contains an AST. Charm's 'eval' function, when applied to it, evaluates the AST.
type Code struct {
	Value ast.Node
}

func (c *Code) DeepCopy() Object {return c}
func (c *Code) Type() ObjectType { return CODE_OBJ }
func (c *Code) Inspect(view View) string {
	return fmt.Sprintf("code %s", c.Value.String())
}

// The 'error' type.
type Error struct {
	ErrorId string
	Message string
	Values  []Object
	Trace   []token.Token
	Token   token.Token
}

func (e *Error) DeepCopy() Object {return e}
func (e *Error) Type() ObjectType { return ERROR_OBJ }
func (e *Error) Inspect(view View) string {
	if view == ViewStdOut {
		if len(e.Trace) == 0 {
			return text.ERROR + e.Message + text.DescribePos(e.Token) + "."
		} else {
			return text.RT_ERROR + e.Message + text.DescribePos(e.Token) + "."
		}
	}
	return "error " + text.ToEscapedText(e.Message)
}

// The 'float64' type
type Float struct {
	Value float64
}

func (f *Float) DeepCopy() Object {return f}
func (f *Float) Type() ObjectType         { return FLOAT_OBJ }
func (f *Float) Inspect(view View) string { return fmt.Sprintf("%f", f.Value) }
func (f *Float) HashKey() HashKey {
	return HashKey{Type: f.Type(), Value: uint64(f.Value)}
}

// A lambda object.
type Func struct {
	ast.Function
	Env *Environment
}

func (fn *Func) DeepCopy() Object {return fn} 
func (fn *Func) Type() ObjectType { return FUNC_OBJ }
func (fn *Func) Inspect(view View) string {
	result := "func " + fn.Sig.String() + " : " + fn.Body.String()
	if fn.Given != nil {
		result = "(" + result + ")" + " given : " + "(" + fn.Given.String() + ")"
	}
	return result
}

// Charm's 'map' type. Temporary, will be refactored.
type Hash struct {
	Pairs map[HashKey]HashPair
}

func (h *Hash) DeepCopy() Object {
	newHash := &Hash{Pairs:  make(map[HashKey]HashPair)}
	for hashKey, pair := range h.Pairs {
		newPair := HashPair{Key: pair.Key, Value: pair.Value.DeepCopy()}
		newHash.Pairs[hashKey] = newPair 
	}
	return newHash
}
func (h *Hash) Type() ObjectType { return HASH_OBJ }
func (h *Hash) Inspect(view View) string {
	var out bytes.Buffer

	pairs := []string{}
	out.WriteString("map ")
	out.WriteString("(")
	for _, pair := range h.Pairs {
		pairs = append(pairs, fmt.Sprintf("%s :: %s",
			pair.Key.Inspect(view), pair.Value.Inspect(view)))
	}

	out.WriteString(strings.Join(pairs, ", "))

	out.WriteString(")")

	return out.String()
}

type HashPair struct {
	Key   Object
	Value Object
}

// Charm 'int' wraps around Go 'int' and so is machine-dependent.
type Integer struct {
	Value int
}

func (i *Integer) DeepCopy() Object {return i}
func (i *Integer) Type() ObjectType         { return INTEGER_OBJ }
func (i *Integer) Inspect(view View) string { return fmt.Sprintf("%d", i.Value) }
func (i *Integer) HashKey() HashKey {
	return HashKey{Type: i.Type(), Value: uint64(i.Value)}
}

// The 'label' type includes both fields of structs and members of enums.
// The 'Name' field will contain either 'field' if it's a field, or the name of the enum
// otherwise.
type Label struct {
	Value string
	Name  string
}

func (la *Label) DeepCopy() Object {return la}
func (la *Label) Type() ObjectType         { return LABEL_OBJ }
func (la *Label) Inspect(view View) string { return la.Value }
func (la *Label) HashKey() HashKey {
	h := fnv.New64a()
	h.Write([]byte(la.Value))

	return HashKey{Type: la.Type(), Value: h.Sum64()}
}

// Eventually to be refactored to use persistent data structures.
type List struct {
	Elements []Object
}

func (lo *List) DeepCopy() Object {
	newList := make([]Object, len(lo.Elements))
		for i := 0; i < len(lo.Elements); i++ {
			newList[i] = lo.Elements[i].DeepCopy()
		}
	return &List{Elements: newList}
}
func (lo *List) DeepSlice(start, end int) Object {
	newList := make([]Object, end - start)
		for i := start; i < end; i++ {
			newList[i - start] = lo.Elements[i].DeepCopy()
		}
	return &List{Elements: newList}
}
func (lo *List) Type() ObjectType { return LIST_OBJ }
func (lo *List) Inspect(view View) string {
	var out bytes.Buffer

	elements := []string{}
	for _, e := range lo.Elements {
		elements = append(elements, e.Inspect(view))
	}

	out.WriteString("[")
	out.WriteString(strings.Join(elements, ", "))
	out.WriteString("]")

	return out.String()
}

// The 'pair' type, formed with the :: operator.
type Pair struct {
	Left  Object
	Right Object
}

func (p *Pair) DeepCopy() Object {return &Pair{Left: p.Left.DeepCopy(), Right: p.Right.DeepCopy()}}
func (p *Pair) Type() ObjectType { return PAIR_OBJ }
func (p *Pair) Inspect(view View) string {
	return fmt.Sprintf("%s :: %s", p.Left.Inspect(view), p.Right.Inspect(view))
}

// The 'return' type wraps around values returned from the cmd section and should be pretty much
// invisible to users and end-users.
type Return struct {
	Elements []Object
}

func (r *Return) DeepCopy() Object {return r}
func (r *Return) Type() ObjectType { return RETURN_OBJ }
func (r *Return) Inspect(view View) string {
	var out bytes.Buffer

	elements := []string{}
	for _, e := range r.Elements {
		elements = append(elements, e.Inspect(view))
	}
	out.WriteString(strings.Join(elements, ", "))

	return out.String()
}

// A 'set' type. Appalingly implemented, because it will be replaced by persistent data structures
// anyway.
type Set struct {
	Elements []Object
}

func (so *Set) DeepCopy() Object {
	newSet := make([]Object, len(so.Elements))
		for i := 0; i < len(so.Elements); i++ {
			newSet[i] = so.Elements[i].DeepCopy()
		}
	return &Set{Elements: newSet}
}
func (so *Set) Type() ObjectType { return SET_OBJ }
func (so *Set) Inspect(view View) string {
	var out bytes.Buffer
	elements := []string{}
	for _, element := range so.Elements {
		elements = append(elements, element.Inspect(view))
	}

	out.WriteString("{")
	out.WriteString(strings.Join(elements, ", "))
	out.WriteString("}")

	return out.String()
}

// Just your basic string type.
type String struct {
	Value string
}

func (s *String) DeepCopy() Object {return s}
func (s *String) Type() ObjectType { return STRING_OBJ }
func (s *String) Inspect(view View) string {
	if view == ViewStdOut {
		return s.Value
	}
	return text.ToEscapedText(s.Value)
}
func (s *String) HashKey() HashKey {
	h := fnv.New64a()
	h.Write([]byte(s.Value))

	return HashKey{Type: s.Type(), Value: h.Sum64()}
}

// An object for containing structs. The 'Name' filed contains the type of the concrete struct.
type Struct struct {
	Labels []string
	Value  map[string]Object
	Name   string
}
func (s *Struct) DeepCopy() Object {
	newStruct := &Struct{Labels: s.Labels, Value: make(map[string]Object), Name: s.Name}
	for k, v := range(s.Value) {
		newStruct.Value[k] = v.DeepCopy() 
	}
	return newStruct
}
func (st *Struct) Type() ObjectType { return STRUCT_OBJ }
func (st *Struct) Inspect(view View) string {
	var out bytes.Buffer

	if st.Name == "nil" {
		return "NIL"
	}

	elements := []string{}
	for _, e := range st.Labels {
		elements = append(elements, e+" :: "+st.Value[e].Inspect(view))
	}
	if view == ViewCharmLiteral {
		out.WriteString(st.Name)
		out.WriteString(" with ")
	}
	out.WriteString("(")
	out.WriteString(strings.Join(elements, ", "))
	out.WriteString(")")
	return out.String()
}

// For reasons I can't remember, it was a good idea to process the definitions of structs via
// the evaluator as well as the parser. Hence this purely internal type.
type StructDef struct {
	Sig signature.Signature
}

func (st *StructDef) DeepCopy() Object { return st }
func (st *StructDef) Type() ObjectType         { return STRUCTDEF_OBJ }
func (st *StructDef) Inspect(view View) string { return st.Sig.String() }

// Since everything is an expression and must return something, this is what a valid assignment
// returns.
type SuccessfulAssignment struct{}

func (s *SuccessfulAssignment) DeepCopy() Object { return s }
func (s *SuccessfulAssignment) Type() ObjectType         { return SUCCESSFUL_OBJ }
func (s *SuccessfulAssignment) Inspect(view View) string { return text.OK }

// The 'tuple' type.
type Tuple struct {
	Elements []Object
}

func (to *Tuple) DeepCopy() Object {
	newList := make([]Object, len(to.Elements))
		for i := 0; i < len(to.Elements); i++ {
			newList[i] = to.Elements[i].DeepCopy()
		}
	return &Tuple{Elements: newList}
}
func (to *Tuple) DeepSlice(start, end int) Object {
	newTuple := make([]Object, end - start)
		for i := start; i < end; i++ {
			newTuple[i - start] = to.Elements[i].DeepCopy()
		}
	return &Tuple{Elements: newTuple}
}
func (to *Tuple) Type() ObjectType { return TUPLE_OBJ }
func (to *Tuple) Inspect(view View) string {
	var out bytes.Buffer
	elements := []string{}
	if len(to.Elements) == 1 {
		out.WriteString("tuple ")
	}
	if len(to.Elements) <= 1 {
		out.WriteString("(")
	}
	for _, e := range to.Elements {
		elements = append(elements, e.Inspect(view))
	}
	out.WriteString(strings.Join(elements, ", "))
	if len(to.Elements) <= 1 {
		out.WriteString(")")
	}
	return out.String()
}
func (to *Tuple) Len() int {
	return len((*to).Elements)
}

// The 'type' type.
type Type struct {
	Value string
}

func (t *Type) DeepCopy() Object {return t}
func (t *Type) Type() ObjectType { return TYPE_OBJ }
func (t *Type) Inspect(view View) string {
	return t.Value
}

// An 'unsatisfied conditional' is returned by anything of the form x : y where x is not true.
// This is an internal type in that before any user gets to see it it will be turned into
// a unsatisfied conditional error.
type UnsatisfiedConditional struct{}

func (u *UnsatisfiedConditional) DeepCopy() Object {return u}
func (u *UnsatisfiedConditional) Type() ObjectType         { return UNSATISFIED_OBJ }
func (u *UnsatisfiedConditional) Inspect(view View) string { return "unsatisfied conditional" }

///////////////////////////////////////////////////////////////
// Functions for pretending the 'set' type is actually a set.
///////////////////////////////////////////////////////////////

// This is gross but it will all be removed when I do persistent data structures so I feel no obligation to do better.

func (so *Set) Contains(ob Object) bool {
	return contains(so.Elements, ob)
}

func (so *Set) AddElement(ob Object) {
	if !so.Contains(ob) {
		so.Elements = append(so.Elements, ob)
	}
}

func (so *Set) Copy() *Set {
	var result = Set{Elements: []Object{}}
	for _, v := range so.Elements {
		result.AddElement(v)
	}
	return &result
}

func contains(elements []Object, ob Object) bool {
	for _, v := range elements {
		if Equals(v, ob) {
			return true
		}
	}
	return false
}

func SetFromSlice(slice []Object) *Set {
	var result = Set{Elements: []Object{}}
	for _, v := range slice {
		result.AddElement(v)
	}
	return &result
}

//////////////////////////
// Helper functions etc
//////////////////////////

func (t *Type) HashKey() HashKey {
	h := fnv.New64a()
	h.Write([]byte(t.Value))
	return HashKey{Type: t.Type(), Value: h.Sum64()}
}

func Equals(lhs, rhs Object) bool {
	if TrueType(lhs) != TrueType(rhs) {
		return false
	}
	if lhs == rhs {
		return true
	}
	switch lhs.Type() {
	case INTEGER_OBJ:
		return lhs.(*Integer).Value == rhs.(*Integer).Value
	case LABEL_OBJ:
		return lhs.(*Label).Value == rhs.(*Label).Value
	case FLOAT_OBJ:
		return lhs.(*Float).Value == rhs.(*Float).Value
	case STRING_OBJ:
		return lhs.(*String).Value == rhs.(*String).Value
	case BOOLEAN_OBJ:
		return lhs == rhs
	case TYPE_OBJ:
		return lhs.(*Type).Value == rhs.(*Type).Value
	case LIST_OBJ:
		if len(lhs.(*List).Elements) != len(rhs.(*List).Elements) {
			return false
		}
		for k, v := range lhs.(*List).Elements {
			if !Equals(v, rhs.(*List).Elements[k]) {
				return false
			}
		}
		return true
	case STRUCT_OBJ:
		for k, v := range lhs.(*Struct).Value {
			w, ok := rhs.(*Struct).Value[k]
			if !ok {
				return false
			}
			if !Equals(v, w) {
				return false
			}
		}
		return true
	case TUPLE_OBJ:
		if len(lhs.(*Tuple).Elements) != len(rhs.(*Tuple).Elements) {
			return false
		}
		for k, v := range lhs.(*Tuple).Elements {
			if !Equals(v, rhs.(*Tuple).Elements[k]) {
				return false
			}
		}
		return true
	case SET_OBJ:
		if len(lhs.(*Set).Elements) != len(rhs.(*Set).Elements) {
			return false
		} // Yes this is terrible.
		for _, v := range rhs.(*Set).Elements {
			if !lhs.(*Set).Contains(v) {
				return false
			}
		}
		for _, v := range lhs.(*Set).Elements {
			if !rhs.(*Set).Contains(v) {
				return false
			}
		}
		return true
	case PAIR_OBJ:
		return Equals(lhs.(*Pair).Left, rhs.(*Pair).Left) && Equals(lhs.(*Pair).Right, rhs.(*Pair).Right)
	default:
		panic("You're trying to compare something for which == hasn't been implemented. Find out why and make it stop.")
	}
}

func MakeBool(input bool) *Boolean {
	if input {
		return TRUE
	}
	return FALSE
}

func MakeInverseBool(input bool) *Boolean {
	if input {
		return FALSE
	}
	return TRUE
}

var (
	TRUE  = &Boolean{Value: true}
	FALSE = &Boolean{Value: false}
	NIL   = &Struct{Labels: []string{},
		Value: make(map[string]Object),
		Name:  "nil"}
)

func DescribeSomeParams(params []Object, incomplete bool) string {
	out := "'" + describeParamsWithoutQuotes(params)
	if incomplete { out = out + " ..."}
	return out + "'"
}

func DescribeParams(params []Object) string {
	return "'" + describeParamsWithoutQuotes(params) + "'"
}

func DescribeSomeObjects(params []Object, incomplete bool) string {
	s := ""
	for k, v := range params {
		if TrueType(v) == "bling" {
			s = s + v.(*Bling).Value
		} else {
			s = s + (v.Inspect(ViewCharmLiteral))
		}
		if k < len(params)-1 && !(TrueType(v) == "bling") && !(TrueType(params[k+1]) == "bling") {
			s = s + ","
		}
		if k < len(params)-1 {
			s = s + " "
		}
	}
	if incomplete { s = s + " ..."}
	if (len(params) == 1 && params[0].Type() == STRING_OBJ) && ! incomplete {
		return s
	}
	return "'" + s + "'"
}

func describeParamsWithoutQuotes(params []Object) string {
	s := ""
	for k, v := range params {
		if TrueType(v) == "bling" {
			s = s + v.(*Bling).Value
		} else {
			s = s + "<" + TrueType(v) + ">"
		}
		if k < len(params)-1 && !(TrueType(v) == "bling") && !(TrueType(params[k+1]) == "bling") {
			s = s + ","
		}
		if k < len(params)-1 {
			s = s + " "
		}
	}
	return s
}

type GoReturn struct {
	Elements []any
}

func TrueType(o Object) string {
	if o.Type() == STRUCT_OBJ {
		return o.(*Struct).Name
	}
	if o.Type() == LABEL_OBJ {
		return o.(*Label).Name
	}
	return string(o.Type())
}

func TypeOrBling(o Object) string {
	if o.Type() == BLING_OBJ {
		return o.(*Bling).Value
	}
	return TrueType(o)
}

func EmphType(o Object) string {
	return "<" + TrueType(o) + ">"
}

func EmphValue(o Object) string {
	if o.Type() == STRING_OBJ {
		return text.Cyan(o.Inspect(ViewCharmLiteral))
	}
	return text.Emph(o.Inspect(ViewCharmLiteral))
}
