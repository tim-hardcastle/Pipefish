package vm

import "pipefish/source/set"

type varAccess int

const (
	GLOBAL_CONSTANT_PUBLIC varAccess = iota
	GLOBAL_VARIABLE_PUBLIC
	FUNCTION_ARGUMENT
	LOCAL_CONSTANT_THUNK
	LOCAL_TRUE_CONSTANT
	REFERENCE_VARIABLE
	VERY_LOCAL_CONSTANT // i.e. 'that' when constant
	VERY_LOCAL_VARIABLE // i.e. 'that' when variable
)

// Update with:
var ALL_CONST_ACCESS = set.MakeFromSlice[varAccess]([]varAccess{GLOBAL_CONSTANT_PUBLIC, LOCAL_TRUE_CONSTANT, VERY_LOCAL_CONSTANT})

type variable struct {
	mLoc   uint32
	access varAccess
	types  alternateType
}

type environment struct {
	data map[string]variable
	ext  *environment
}

func newEnvironment() *environment {
	return &environment{data: make(map[string]variable), ext: nil}
}

func (env *environment) getVar(name string) (*variable, bool) {
	if env == nil {
		return nil, false
	}
	v, ok := env.data[name]
	if ok {
		return &v, true
	}
	return env.ext.getVar(name)
}
