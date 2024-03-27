package values

type ValueType uint32

const ( // Cross-reference with typeNames in BlankVm()

	// The types from UNDEFINED VALUE to REF inclusive are internal types which should never actually be seen by the user.
	// In some cases, e.g. CREATED_LOCAL_CONSTANT, they are also not instantiated: they are there to
	// return in a typeScheme object when the compiled code doesn't create a value.

	UNDEFINED_VALUE        ValueType = iota // For debugging purposes, it is useful to have the zero value be something it should never actually be.
	INT_ARRAY                               // V is an array of Golang integers.
	THUNK                                   // Contains what we need to evaluate inner variables.
	CREATED_LOCAL_CONSTANT                  // Returned by the compiler in the typeScheme when we compile a thunk.
	COMPILE_TIME_ERROR                      // For when we have to return a type, but what we have is a compile time error.
	BLING                                   // Values representing e.g. the `troz` in `foo (x) troz (y)`.
	UNSAT                                   // An unsatisfied conditional, i.e. what <condition> : <expression> returns if <condition> isn't true.
	REF                                     // A reference variable. This is always dereferenced when used, so the type is invisible.
	BREAK                                   // The value of the `break` statement.

	// The SUCCESSFUL_VALUE is visible to the user only in the REPL, it's not first-class.

	SUCCESSFUL_VALUE

	// And now we have types visible to the user.

	TUPLE    // V : []values.Value
	ERROR    // V : *object.Error
	NULL     // V : nil
	INT      // V : int
	BOOL     // V : bool
	STRING   // V : string
	FLOAT    // V : float
	TYPE     // V : --- currently ValueType but should be something else.
	FUNC     // V : vm.Lambda
	PAIR     // V : []values.Value
	LIST     // V : vector.Vector
	MAP      // V : *values.Map
	SET      // V : values.Set
	LABEL    // V : int
	LB_ENUMS // I.e the first of the enums.
)

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
	U_OBJ = Value{T: UNSAT}
	ONE   = Value{INT, 1}
	BLNG  = Value{BLING, "bling"}
	OK    = Value{SUCCESSFUL_VALUE, nil}
	BRK   = Value{BREAK, nil}
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
	C_BREAK
	C_EMPTY_TUPLE
)
