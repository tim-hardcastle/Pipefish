package values

type ValueType uint32

const ( // Cross-reference with typeNames in blankVm()
	UNDEFINED_VALUE ValueType = iota // For debugging purposes, it is useful to have the zero value something it should never actually be.
	INT_ARRAY                        // For internal use only
	THUNK
	CREATED_LOCAL_CONSTANT
	TUPLE
	ERROR
	UNSAT
	REF
	NULL
	INT
	BOOL
	STRING
	FLOAT
	TYPE
	FUNC
	PAIR
	LIST
	MAP
	SET
	LABEL
	LB_ENUMS // I.e the first of the enums.
)

type Value struct {
	T ValueType
	V any
}

func (v Value) compare(w Value) bool {
	if v.T < w.T {
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

var (
	FALSE = Value{T: BOOL, V: false}
	TRUE  = Value{T: BOOL, V: true}
	U_OBJ = Value{T: UNSAT}
)

const (
	C_FALSE = iota
	C_TRUE
	C_U_OBJ
)
