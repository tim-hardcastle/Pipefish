package compiler

// How the compiler keeps track of where in memory the variables are stored, and what the access to them is like.

import "github.com/tim-hardcastle/Pipefish/source/dtypes"

type VarAccess int

const (
	GLOBAL_CONSTANT_PUBLIC VarAccess = iota
	GLOBAL_VARIABLE_PUBLIC
	GLOBAL_CONSTANT_PRIVATE
	GLOBAL_VARIABLE_PRIVATE
	LOCAL_VARIABLE
	FUNCTION_ARGUMENT
	LOCAL_VARIABLE_THUNK
	LOCAL_FUNCTION_THUNK
	LOCAL_FUNCTION_CONSTANT
	LOCAL_CONSTANT
	REFERENCE_VARIABLE
	VERY_LOCAL_CONSTANT     // i.e. 'that' when constant
	VERY_LOCAL_VARIABLE     // i.e. 'that' when variable
	UNDEFINED_THIS_VARIABLE // When 'this' hasn't been defined because we haven't finished compiling the lambda.
	FOR_LOOP_BOUND_VARIABLE
	FOR_LOOP_INDEX_VARIABLE
)

// Update with:
var ALL_CONSTANT_ACCESS = dtypes.MakeFromSlice([]VarAccess{GLOBAL_CONSTANT_PUBLIC, GLOBAL_CONSTANT_PRIVATE, LOCAL_CONSTANT, VERY_LOCAL_CONSTANT})
var ALL_PRIVATE_ACCESS = dtypes.MakeFromSlice([]VarAccess{GLOBAL_CONSTANT_PRIVATE, GLOBAL_VARIABLE_PRIVATE})

type Variable struct {
	MLoc   uint32
	Access VarAccess
	Types  AlternateType
}

type Environment struct {
	Data map[string]Variable
	Ext  *Environment
}

func NewEnvironment() *Environment {
	return &Environment{Data: make(map[string]Variable), Ext: nil}
}

func (env *Environment) GetVar(name string) (*Variable, bool) {
	if env == nil {
		return nil, false
	}
	v, ok := env.Data[name]
	if ok {
		return &v, true
	}
	return env.Ext.GetVar(name)
}
