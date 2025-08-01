package values

import (
	"strconv"
)

type ValueType uint32

const ( // Cross-reference with typeNames in BlankVm()

	// The types from UNDEFINED VALUE to BREAK inclusive are internal types which should never actually be seen by the user.
	// In some cases, e.g. CREATED_LOCAL_CONSTANT, they are also not instantiated: they are there to
	// return in a TypeScheme object when the compiled code doesn't create a value.

	UNDEFINED_TYPE          ValueType = iota // For debugging purposes, it is useful to have the zero value be something it should never actually be.
	INT_ARRAY                                // V is an array of Golang uint32. TODO --- its only current use is a three-value enum.
	THUNK                                    // V is a ThunkValue which contains the address to call to evaluate the thunk and the memory location where the result ends up.
	CREATED_THUNK_OR_CONST                   // Returned by the compiler in the TypeScheme when we compile a thunk.
	COMPILE_TIME_ERROR                       // For when we have to return a type, but what we have is a compile time error.
	BLING                                    // Values representing e.g. the `troz` in `foo (x) troz (y)`.
	UNSATISFIED_CONDITIONAL                  // An unsatisfied conditional, i.e. what <condition> : <expression> returns if <condition> isn't true.
	REF                                      // A reference variable. This is always dereferenced when used, so the type is invisible.
	ITERATOR                                 // V is an Iterator interface as defined in iterator.go in this folder.

	// The SUCCESSFUL_VALUE is visible to the user only in the REPL, it's not first-class.

	SUCCESSFUL_VALUE

	// And now we have types visible to the user.

	TUPLE              // V : []values.Value
	ERROR              // V : *object.Error
	NULL               // V : nil
	INT                // V : int
	BOOL               // V : bool
	STRING             // V : string
	RUNE               // V : string
	FLOAT              // V : float
	TYPE               // V : abstractType
	FUNC               // V : vm.Lambda
	PAIR               // V : []values.Value // TODO --- this should be [2]values.Value just for neatness.
	LIST               // V : vector.Vector
	MAP                // V : *values.Map
	SET                // V : values.Set
	LABEL              // V : int
	SNIPPET            // V : Snippet struct{Data []values.Value, Bindle *SnippetBindle}
	SECRET             // V : SecretContainer
	FIRST_DEFINED_TYPE // I.e the first of the enums.
)

const DUMMY = 4294967295

type Value struct {
	T ValueType
	V any
}

type Snippet struct {
	Data   []Value
	Bindle *SnippetBindle
}

// A grouping of all the things a snippet from a given snippet factory have in common.
type SnippetBindle struct {
	CodeLoc   uint32   // Where to find the code to compute the object string and the values.
	ValueLocs []uint32 // The locations where we put the computed values to inject into SQL or HTML snippets.
}

func (v Value) compare(w Value) bool { // To implement the set and hash structures. It doesn't really matter which order
	if v.T < w.T { // these things are in, so long as there is one.
		return true
	}
	if w.T < v.T {
		return false
	}
	switch v.T {
	case BOOL:
		return (!v.V.(bool)) && w.V.(bool)
	case FLOAT:
		return v.V.(float64) < w.V.(float64)
	case INT:
		return v.V.(int) < w.V.(int)
	case NULL:
		return false
	case RUNE:
		return v.V.(rune) < w.V.(rune)
	case STRING:
		return v.V.(string) < w.V.(string)
	case TUPLE:
		if len(v.V.([]Value)) == len(w.V.([]Value)) {
			for i, vEl := range v.V.([]Value) {
				if vEl.compare(w.V.([]Value)[i]) {
					return true
				}
			}
			return false
		}
		return len(v.V.([]Value)) < len(w.V.([]Value))
	case TYPE:
		lhs := v.V.(AbstractType)
		rhs := w.V.(AbstractType)
		if len(lhs.Types) == len(rhs.Types) {
			for i, ty := range lhs.Types {
				if ty < rhs.Types[i] {
					return true
				}
				if ty > rhs.Types[i] {
					return false
				}
			}
			return false
		} else {
			return len(lhs.Types) < len(rhs.Types)
		}
	}
	// So we're going to assume that it's an enum and that this has been checked elsewhere.
	// TODO --- structs, maybe lists?
	return v.V.(int) < w.V.(int)
}

// Cross-reference with CONSTANTS in vm.go.
var (
	UNDEF = Value{UNDEFINED_TYPE, nil}
	FALSE = Value{BOOL, false}
	TRUE  = Value{BOOL, true}
	U_OBJ = Value{T: UNSATISFIED_CONDITIONAL}
	ONE   = Value{INT, 1}
	BLNG  = Value{BLING, "bling"}
	OK    = Value{SUCCESSFUL_VALUE, nil}
	EMPTY = Value{TUPLE, []Value{}}
)

const (
	C_UNDEF = iota
	C_FALSE
	C_TRUE
	C_U_OBJ
	C_ONE
	C_BLING
	C_OK
	C_EMPTY_TUPLE
)

func (a AbstractType) String() string {
	result := "["
	sep := ""
	for _, t := range a.Types {
		result = result + sep + strconv.Itoa(int(t))
		sep = ", "
	}
	result = result + "]"
	return result
}

type AbstractType struct {
	Types   []ValueType
}

func MakeAbstractType(args ...ValueType) AbstractType {
	result := AbstractType{[]ValueType{}}
	for _, t := range args {
		result = result.Insert(t)
	}
	return result
}

func (lhs AbstractType) Union(rhs AbstractType) AbstractType {
	i := 0
	j := 0
	result := make([]ValueType, 0, len(lhs.Types)+len(rhs.Types))
	for i < len(lhs.Types) || j < len(rhs.Types) {
		switch {
		case i == len(lhs.Types):
			result = append(result, rhs.Types[j])
			j++
		case j == len(rhs.Types):
			result = append(result, lhs.Types[i])
			i++
		case lhs.Types[i] == rhs.Types[j]:
			result = append(result, lhs.Types[i])
			i++
			j++
		case lhs.Types[i] < rhs.Types[j]:
			result = append(result, lhs.Types[i])
			i++
		case rhs.Types[j] < lhs.Types[i]:
			result = append(result, rhs.Types[j])
			j++
		}
	}
	return AbstractType{result}
}

func (a AbstractType) Insert(v ValueType) AbstractType {
	if len(a.Types) == 0 {
		return AbstractType{[]ValueType{v}}
	}
	for i, t := range a.Types {
		if v == t {
			return a
		}
		if v < t {
			lhs := make([]ValueType, i)
			rhs := make([]ValueType, len(a.Types)-i)
			copy(lhs, a.Types[:i])
			copy(rhs, a.Types[i:])
			lhs = append(lhs, v)
			return AbstractType{append(lhs, rhs...)}
		}
	}
	return AbstractType{append(a.Types, v)}
}

// Because AbstractTypes are ordered we could use binary search for this and there is a threshold beyond which
// it would be quicker, but this must be determined empirically and I haven't done that yet. TODO.
func (a AbstractType) Contains(v ValueType) bool {
	for _, w := range a.Types {
		if v == w {
			return true
		}
	}
	return false
}

func (a AbstractType) Len() int {
	return len(a.Types)
}

func (a AbstractType) Equals(b AbstractType) bool {
	if len(a.Types) != len(b.Types) {
		return false
	}
	for i, v := range a.Types {
		if v != b.Types[i] {
			return false
		}
	}
	return true
}

func (a AbstractType) IsSubtypeOf(b AbstractType) bool {
	if len(a.Types) > len(b.Types) {
		return false
	}
	i := 0
	for _, t := range a.Types {
		for ; i < len(b.Types) && b.Types[i] < t; i++ {
		}
		if i >= len(b.Types) {
			return false
		}
		if t != b.Types[i] {
			return false
		}
	}
	return true
}

func (a AbstractType) IsProperSubtypeOf(b AbstractType) bool {
	if len(a.Types) > len(b.Types) || len(a.Types) == len(b.Types) {
		return false
	}
	i := 0
	for _, t := range a.Types {
		for ; i < len(b.Types) && b.Types[i] < t; i++ {
		}
		if i >= len(b.Types) {
			return false
		}
		if t != b.Types[i] {
			return false
		}
	}
	return true
}

func (vL AbstractType) Intersect(wL AbstractType) AbstractType {
	result := AbstractType{[]ValueType{}}
	var vix, wix int
	for vix < vL.Len() && wix < wL.Len() {
		if vL.Types[vix] == wL.Types[wix] {
			result.Types = append(result.Types, vL.Types[vix])
			vix++
			wix++
			continue
		}
		if vL.Types[vix] < wL.Types[wix] {
			vix++
			continue
		}
		wix++
	}
	return result
}

func (a AbstractType) PartlyIntersects(b AbstractType) bool {
	intersectionSize := a.Intersect(b).Len()
	return !(a.Len() == intersectionSize || b.Len() == intersectionSize || intersectionSize == 0)
}

func (a AbstractType) Without(b AbstractType) AbstractType {
	rTypes := make([]ValueType, 0, len(a.Types))
	i := 0
	for _, t := range a.Types {
		for ; i < len(b.Types) && b.Types[i] < t; i++ {
		}
		if i >= len(b.Types) {
			rTypes = append(rTypes, t)
			continue
		}
		if (t != b.Types[i]) {
			rTypes = append(rTypes, t)
		}
	}
	return AbstractType{rTypes}
}

type AbstractTypeInfo struct {
	Name string
	Path string
	AT   AbstractType
	IsMI bool
}

func (aT AbstractTypeInfo) IsMandatoryImport() bool {
	return aT.IsMI
}

// This is the data that goes inside a THUNK value.
type ThunkValue struct {
	MLoc  uint32 // The place in memory where the result of the thunk ends up when you unthunk it.
	CAddr uint32 // The code address to call to unthunk the thunk.
}
