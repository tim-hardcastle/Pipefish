package values

type ValueType uint32

const ( // Cross-reference with typeNames in BlankVm()

	// The types from UNDEFINED VALUE to BREAK inclusive are internal types which should never actually be seen by the user.
	// In some cases, e.g. CREATED_LOCAL_CONSTANT, they are also not instantiated: they are there to
	// return in a typeScheme object when the compiled code doesn't create a value.

	UNDEFINED_VALUE         ValueType = iota // For debugging purposes, it is useful to have the zero value be something it should never actually be.
	INT_ARRAY                                // V is an array of Golang uint32. TODO --- its only current use is a three-value enum.
	SNIPPET_DATA                             // V is SnippetData. This is attached as an invisible field to a snippet struct to carry around things that can be deduced at compile time.
	THUNK                                    // V is a ThunkValue which contains the address to call to evaluate the thunk and the memory location where the result ends up.
	CREATED_THUNK_OR_CONST                   // Returned by the compiler in the typeScheme when we compile a thunk.
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
	PAIR               // V : []values.Value
	LIST               // V : vector.Vector
	MAP                // V : *values.Map
	SET                // V : values.Set
	LABEL              // V : int
	FIRST_DEFINED_TYPE // I.e the first of the enums.
)

const DUMMY = 4294967295

type Value struct {
	T ValueType
	V any
}

func (v Value) compare(w Value) bool { // To implement the set and hash structures. It doesn't really matter which order
	if v.T < w.T { // these things are in, so long as there is one.
		return true
	}
	if w.T < v.T {
		return false
	}
	switch v.T {
	case NULL:
		return false
	case INT:
		return v.V.(int) < w.V.(int)
	case BOOL:
		return (!v.V.(bool)) && w.V.(bool)
	case STRING:
		return v.V.(string) < w.V.(string)
	case FLOAT:
		return v.V.(float64) < w.V.(float64)
	case TYPE:
		return v.V.(ValueType) < w.V.(ValueType)
	}
	// So we're going to assume that it's an enum and that this has been checked elsewhere.
	return v.V.(int) < w.V.(int)
}

// Cross-reference with CONSTANTS in vm.go.
var (
	UNDEF = Value{UNDEFINED_VALUE, nil}
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

// AbstractTypes are constructed from the altTypes in the compiler and so are assumed to be ordered.
type AbstractType struct {
	Types   []ValueType
	Varchar uint32 // A kludge. if this has the maximum value (DUMMY), and there's a string in the types, then it's a string; otherwise it's a varchar with that value.
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

func (a AbstractType) IsVarchar() bool {
	return len(a.Types) == 1 && a.Types[0] == STRING && a.Varchar < DUMMY
}

func (a AbstractType) IsVarcharOrNull() bool {
	return len(a.Types) == 1 && a.Types[0] == STRING && a.Types[1] == STRING && a.Varchar < DUMMY
}

func (a AbstractType) Equals(b AbstractType) bool {
	if len(a.Types) != len(b.Types) || a.Varchar != b.Varchar {
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
	if len(a.Types) > len(b.Types) || a.Varchar > b.Varchar {
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

func (a AbstractType) Without(b AbstractType) AbstractType {
	rTypes := make([]ValueType, 0, len(a.Types))
	varCharFlag := false
	i := 0
	for _, t := range a.Types {
		for ; i < len(b.Types) && b.Types[i] < t; i++ {
		}
		if i >= len(b.Types) {
			rTypes = append(rTypes, t)
			continue
		}
		if (t != b.Types[i]) || (t == STRING && a.Varchar != b.Varchar) {
			rTypes = append(rTypes, t)
			varCharFlag = (t == STRING && a.Varchar == b.Varchar)
		}
	}
	if varCharFlag { // Then we've removed a varchar(n) type.
		return AbstractType{rTypes, 0}
	}
	return AbstractType{rTypes, a.Varchar}
}

type AbstractTypeInfo struct {
	Name string
	Path string
	AT   AbstractType
}
