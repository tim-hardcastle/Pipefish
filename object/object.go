package object

import (
	"bytes"
	"fmt"
	"hash/fnv"
	"strings"

	"charm/ast"
	"charm/signature"
	"charm/text"
	"charm/token"
)

type View int

const (
	ViewStdOut =  iota
	ViewCharmLiteral
)

type ObjectType string

const (

	ERROR_OBJ = "error"
	UNSATISFIED_OBJ = "unsatisfied conditional"
	SUCCESSFUL_OBJ = "successful assignment"
	BLING_OBJ = "bling"

	INTEGER_OBJ = "int"
	FLOAT_OBJ = "float"
	BOOLEAN_OBJ = "bool"
	STRING_OBJ  = "string"
	TYPE_OBJ = "type"

	FUNC_OBJ = "func"
	BUILTIN_OBJ  = "BUILTIN"

	LIST_OBJ = "list"
	SET_OBJ = "set"
	TUPLE_OBJ = "tuple"
	RETURN_OBJ = "return"
	STRUCTDEF_OBJ = "structdef"
	PAIR_OBJ = "pair"
	HASH_OBJ  = "map"
	LABEL_OBJ = "label"
	STRUCT_OBJ = "struct"
)

func TrueType(o Object) string {
	if o.Type() != STRUCT_OBJ {
		return string(o.Type())
	}
	return o.(*Struct).Name
}

func EmphType(o Object) string {
	return "<" + TrueType(o) + ">"
}

func EmphValue(o Object) string {
	if o.Type() == STRING_OBJ { return text.Cyan(o.Inspect(ViewCharmLiteral)) }
	return text.Emph(o.Inspect(ViewCharmLiteral))
}

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
	Type() ObjectType
	Inspect(view View) string
}

type Type struct {
	Value string
}

func (t *Type) Type() ObjectType { return TYPE_OBJ }
func (t *Type) Inspect(view View) string  { 
	if view == ViewStdOut { return fmt.Sprintf("%s", t.Value) }
	return fmt.Sprintf("type %s", t.Value)
}

func (t *Type) HashKey() HashKey {
	h := fnv.New64a()
	h.Write([]byte(t.Value))
	return HashKey{Type: t.Type(), Value: h.Sum64()}
}

type Return struct {
	Elements []Object
}

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

type Func struct {
	ast.Function
	Env *Environment
}

func (fn *Func) Type() ObjectType { return FUNC_OBJ }
func (fn *Func) Inspect(view View) string  { 
	result := "func " + fn.Sig.String() + " : " + fn.Body.String()
	if fn.Given != nil {
		result = "(" + result + ")" + " given : " + "(" + fn.Given.String() + ")"
	}
	return result
}

type Label struct {
	Value string
}

func (la *Label) Type() ObjectType { return LABEL_OBJ }
func (la *Label) Inspect(view View) string  { return la.Value }
func (la *Label) HashKey() HashKey {
	h := fnv.New64a()
	h.Write([]byte(la.Value))

	return HashKey{Type: la.Type(), Value: h.Sum64()}
}

type Struct struct {
	Labels []string
	Value map[string] Object
	Name string
}

func (st *Struct) Type() ObjectType { return STRUCT_OBJ }
func (st *Struct) Inspect(view View) string  { 
	var out bytes.Buffer

	if st.Name == "nil" { return "NIL" }

	elements := []string{}
	for _, e := range st.Labels {
		elements = append(elements, e + " :: " + st.Value[e].Inspect(view))
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

 type StructDef struct {
	 Sig signature.Signature
 }

 func (st *StructDef) Type() ObjectType { return STRUCTDEF_OBJ }
 func (st *StructDef) Inspect(view View) string  { return st.Sig.String() }

type Bling struct {
	Value string
}

func (b *Bling) Type() ObjectType { return BLING_OBJ }
func (b *Bling) Inspect(view View) string  { return b.Value }

type Integer struct {
	Value int64
}

func (i *Integer) Type() ObjectType { return INTEGER_OBJ }
func (i *Integer) Inspect(view View) string  { return fmt.Sprintf("%d", i.Value) }
func (i *Integer) HashKey() HashKey {
	return HashKey{Type: i.Type(), Value: uint64(i.Value)}
}

type Pair struct {
	Left Object
	Right Object
}

func (p *Pair) Type() ObjectType { return PAIR_OBJ }
func (p *Pair) Inspect(view View) string  { return fmt.Sprintf("%s :: %s", p.Left.Inspect(view), p.Right.Inspect(view)) }


type Float struct {
	Value float64
}

func (f *Float) Type() ObjectType { return FLOAT_OBJ }
func (f *Float) Inspect(view View) string  { return fmt.Sprintf("%f", f.Value) }
func (f *Float) HashKey() HashKey {
	return HashKey{Type: f.Type(), Value: uint64(f.Value)}
}

type Boolean struct {
	Value bool
}

func (b *Boolean) Type() ObjectType { return BOOLEAN_OBJ }
func (b *Boolean) Inspect(view View) string  { return fmt.Sprintf("%t", b.Value) }
func (b *Boolean) HashKey() HashKey {
	var value uint64

	if b.Value {
		value = 1
	} else {
		value = 0
	}

	return HashKey{Type: b.Type(), Value: value}
}

type UnsatisfiedConditional struct{}

func (u *UnsatisfiedConditional) Type() ObjectType { return UNSATISFIED_OBJ }
func (u *UnsatisfiedConditional) Inspect(view View) string  { return "unsatisfied conditional" }
// As you should never ever see one of these, what it looks like is irrelevant

type SuccessfulAssignment struct{}

func (s *SuccessfulAssignment) Type() ObjectType { return SUCCESSFUL_OBJ }
func (s *SuccessfulAssignment) Inspect(view View) string  { return text.OK }

type Error struct {
	ErrorId string
	Message string
	Info []any
	Trace []token.Token
	Token token.Token
}

func (e *Error) Type() ObjectType { return ERROR_OBJ }
func (e *Error) Inspect(view View) string  { 
	if view == ViewStdOut {
			if len(e.Trace) == 0 {
				return text.ERROR + e.Message + text.DescribePos(e.Token) + "."
			} else {
				return text.RT_ERROR + e.Message + text.DescribePos(e.Token) + "."
			}
	}
	return "error " + text.ToEscapedText(e.Message) 
}

type String struct {
	Value string
}

func (s *String) Type() ObjectType { return STRING_OBJ }
func (s *String) Inspect(view View) string {
	if view == ViewStdOut {return s.Value }
	return text.ToEscapedText(s.Value) 
}
func (s *String) HashKey() HashKey {
	h := fnv.New64a()
	h.Write([]byte(s.Value))

	return HashKey{Type: s.Type(), Value: h.Sum64()}
}

type List struct {
	Elements []Object
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



type Set struct {
	Elements []Object
}

func (so *Set) Type() ObjectType { return SET_OBJ }
func (so *Set) Inspect(view View) string {
	var out bytes.Buffer
	elements := []string{}
	for _, element := range so.Elements {
		elements = append(elements, fmt.Sprintf("%s",
			element.Inspect(view)))
	}

	out.WriteString("{")
	out.WriteString(strings.Join(elements, ", "))
	out.WriteString("}")

	return out.String()
}

func (so *Set) Contains(ob Object) bool {
	return contains(so.Elements, ob)
}

func (so *Set) AddElement(ob Object) {
	if !so.Contains(ob) {
		so.Elements = append(so.Elements, ob)		
	}
}

func (so *Set) Copy() *Set {
	var result = Set{Elements : []Object{}}
    for _, v := range so.Elements {
        result.AddElement(v)
    }
    return &result
}

func contains(elements []Object, ob Object) bool {
	for _, v := range elements {
		if Equals(v, ob) { return true }
	}
	return false
}

func SetFromSlice(slice []Object) (*Set) {
	var result = Set{Elements : []Object{}}
    for _, v := range slice {
        result.AddElement(v)
    }
    return &result
}

type Tuple struct {
	Elements []Object
}
func (to *Tuple) Type() ObjectType { return TUPLE_OBJ }
func (to *Tuple) Inspect(view View) string {
	var out bytes.Buffer
	elements := []string{}
	if len(to.Elements) == 1 { out.WriteString("tuple ") }
	if len(to.Elements) <= 1 { out.WriteString("(") }
	for _, e := range to.Elements {
		elements = append(elements, e.Inspect(view))
	}
	out.WriteString(strings.Join(elements, ", "))
	if len(to.Elements) <= 1 { out.WriteString(")") }
	return out.String()
}
func (to *Tuple) Len() int {
	return len((*to).Elements)
}


type HashPair struct {
	Key   Object
	Value Object
}

type Hash struct {
	Pairs map[HashKey]HashPair
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

func Equals(lhs, rhs Object) bool {
	if TrueType(lhs) != TrueType(rhs) {
		return false
	}
	if lhs == rhs {
		return true
	}
	switch lhs.Type() {
	case INTEGER_OBJ :
		return lhs.(*Integer).Value == rhs.(*Integer).Value
	case LABEL_OBJ :
		return lhs.(*Label).Value == rhs.(*Label).Value
	case FLOAT_OBJ :
		return lhs.(*Float).Value == rhs.(*Float).Value	
	case STRING_OBJ :
		return lhs.(*String).Value == rhs.(*String).Value
	case BOOLEAN_OBJ :
		return lhs == rhs
	case TYPE_OBJ :
		return lhs.(*Type).Value == rhs.(*Type).Value
	case LIST_OBJ :
		if len(lhs.(*List).Elements) != len(rhs.(*List).Elements) {
			return false
		}
		for k, v := range lhs.(*List).Elements {
			if !Equals(v, rhs.(*List).Elements[k]) {
				return false
			}
		}
		return true
	case STRUCT_OBJ :
		for k, v := range lhs.(*Struct).Value {
			w, ok := rhs.(*Struct).Value[k]
			if ! ok { return false }
			if !Equals(v, w) {
				return false
			}
		}
		return true
	case TUPLE_OBJ :
		if len(lhs.(*Tuple).Elements) != len(rhs.(*Tuple).Elements) {
			return false
		}
		for k, v := range lhs.(*Tuple).Elements {
			if !Equals(v, rhs.(*Tuple).Elements[k]) {
				return false
			}
		}
		return true
	case SET_OBJ :
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
	case PAIR_OBJ :
		return Equals(lhs.(*Pair).Left, rhs.(*Pair).Left) && Equals(lhs.(*Pair).Right, rhs.(*Pair).Right) 
	default :
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
	NIL = &Struct{Labels: []string{},
		Value: make(map[string] Object),
		Name: "nil"}
)

func DescribeParams(params []Object) string {
	s := ""
	for k, v := range params {
		if TrueType(v) == "bling" {
			s = s + v.(*Bling).Value 
		} else {
			s = s + "<" + TrueType(v) + ">"
		}
		if k < len(params) - 1 && !(TrueType(v) == "bling") && !(TrueType(params[k + 1]) == "bling") {
			s = s + ","
		}
		if k < len(params) - 1  {
			s = s + " "
		}
	}
	return "'" + s + "'"
}