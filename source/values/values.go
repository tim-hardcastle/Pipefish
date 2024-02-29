package values

type ValueType uint32

const ( // Cross-reference with typeNames in blankVm()
	UNDEFINED_VALUE ValueType = iota // For debugging purposes, it is useful to have the null value something it should never actually be.
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
	LIST
	LB_ENUMS // I.e the first of the enums.
)

type Value struct {
	T ValueType
	V any
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
