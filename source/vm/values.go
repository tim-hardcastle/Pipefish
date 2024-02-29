package vm

import (
	"charm/source/set"
)

const ( // Cross-reference with typeNames in blankVm()
	UNDEFINED_VALUE simpleType = iota // For debugging purposes, it is useful to have the null value something it should never actually be.
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

// TODO, think about this one.
var ANY_TYPE = alternateType{NULL, INT, BOOL, STRING, FLOAT, TYPE, FUNC, typedTupleType{alternateType{NULL, INT, BOOL, STRING, FLOAT, TYPE, FUNC}}}

type Value struct {
	T simpleType
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

type typeScheme interface {
	compare(u typeScheme) int
}

// Finds all the possible lengths of tuples in a typeScheme. (Single values have length 1. Non-finite tuples have length -1.)
// This allows us to figure out if we need to generate a check on the length of a tuple or whether we can take it for granted
// at compile time.
func lengths(t typeScheme) set.Set[int] {
	result := make(set.Set[int])
	switch t := t.(type) {
	case simpleType:
		result.Add(1)
		return result
	case typedTupleType:
		result.Add(-1)
		return result
	case alternateType:
		for _, v := range t {
			newSet := lengths(v)
			result.AddSet(newSet)
			if result.Contains(-1) {
				return result
			}
		}
		return result
	case finiteTupleType:
		if len(t) == 0 {
			result.Add(0)
			return result
		}
		thisColumnLengths := lengths((t)[0])
		remainingColumnLengths := lengths((t)[1:])
		for j := range thisColumnLengths {
			for k := range remainingColumnLengths {
				result.Add(j + k)
			}
		}
		return result
	}
	panic("We shouldn't be here!")
}

func maxLengthsOrMinusOne(s set.Set[int]) int {
	max := 0
	for k := range s {
		if k == -1 {
			return -1
		}
		if k > max {
			max = k
		}
	}
	return max
}

// This finds all the possible ix-th elements in a typeScheme.
func typesAtIndex(t typeScheme, ix int) alternateType {
	result, _ := recursiveTypesAtIndex(t, ix)
	return result
}

// TODO: This is somewhat wasteful because there ought to be some sensible way to fix it so the algorithm uses data from computing this for
// i - 1. But let's get the VM working first and optimise the compiler later.
func recursiveTypesAtIndex(t typeScheme, ix int) (alternateType, set.Set[int]) {
	resultTypes := alternateType{}
	resultSet := make(set.Set[int])
	switch t := t.(type) {
	case simpleType:
		if ix == 0 {
			resultTypes = resultTypes.union(alternateType{t})
		}
		resultSet.Add(1)
		return resultTypes, resultSet
	case typedTupleType:
		resultTypes = resultTypes.union(t.t)
		resultSet.Add(ix)
		return resultTypes, resultSet
	case alternateType:
		for _, v := range t {
			newTypes, newSet := recursiveTypesAtIndex(v, ix)
			resultTypes = resultTypes.union(newTypes)
			resultSet.AddSet(newSet)
		}
		return resultTypes, resultSet
	case finiteTupleType:
		if len(t) == 0 {
			return resultTypes, resultSet
		}
		resultTypes, resultSet = recursiveTypesAtIndex(t[0], ix)
		for jx := range resultSet {
			newTypes, newSet := recursiveTypesAtIndex(t[1:], ix-jx)
			resultTypes = resultTypes.union(newTypes)
			resultSet.AddSet(newSet)
		}
		return resultTypes, resultSet
	}
	panic("We shouldn't be here!")
}

type simpleType uint32

func (t simpleType) compare(u typeScheme) int {
	switch u := u.(type) {
	case simpleType:
		return int(t) - int(u)
	default:
		return -1
	}
}

type alternateType []typeScheme

func (vL alternateType) intersect(wL alternateType) alternateType {
	x := alternateType{}
	var vix, wix int
	for vix < len(vL) && wix < len(wL) {
		comp := vL[vix].compare(wL[wix])
		if comp == 0 {
			x = append(x, vL[vix])
			vix++
			wix++
			continue
		}
		if comp < 0 {
			vix++
			continue
		}
		wix++
	}
	return x
}

func (vL alternateType) union(wL alternateType) alternateType {
	x := alternateType{}
	var vix, wix int
	for vix < len(vL) || wix < len(wL) {
		if vix == len(vL) {
			x = append(x, wL[wix])
			wix++
			continue
		}
		if wix == len(wL) {
			x = append(x, vL[vix])
			vix++
			continue
		}
		comp := vL[vix].compare(wL[wix])
		if comp == 0 {
			x = append(x, vL[vix])
			vix++
			wix++
			continue
		}
		if comp < 0 {
			x = append(x, vL[vix])
			vix++
			continue
		}
		x = append(x, wL[wix])
		wix++
	}
	return x
}

func (vL alternateType) without(t typeScheme) alternateType {
	x := alternateType{}
	for _, v := range vL {
		if v.compare(t) != 0 {
			x = append(x, v)
		}
	}
	return x
}

func (alternateType alternateType) only(t simpleType) bool {
	if len(alternateType) == 1 {
		switch el := alternateType[0].(type) {
		case simpleType:
			return el == t
		default:
			return false
		}
	}
	return false
}

func (alternateType alternateType) contains(t simpleType) bool {
	for _, ty := range alternateType {
		switch el := ty.(type) {
		case simpleType:
			return (el) == t
		}
	}
	return false
}

func (t alternateType) compare(u typeScheme) int {
	switch u := u.(type) {
	case simpleType:
		return 1
	case alternateType:
		diff := len(t) - len(u)
		if diff != 0 {
			return diff
		}
		for i := 0; i < len(t); i++ {
			diff := (t)[i].compare((u)[i])
			if diff != 0 {
				return diff
			}
		}
		return 0
	default:
		return -1
	}
}

type finiteTupleType []typeScheme // "Finite" meaning that we know its size at compile time.

func (t finiteTupleType) compare(u typeScheme) int {
	switch u := u.(type) {
	case simpleType, alternateType:
		return 1
	case finiteTupleType:
		diff := len(t) - len(u)
		if diff != 0 {
			return diff
		}
		for i := 0; i < len(t); i++ {
			diff := (t)[i].compare((u)[i])
			if diff != 0 {
				return diff
			}
		}
		return 0
	default:
		return -1
	}
}

type typedTupleType struct { // We don't know how long it is but we know what its elements are. (or we can say 'single?' if we don't.)
	t alternateType
}

func (t typedTupleType) compare(u typeScheme) int {
	switch u := u.(type) {
	case simpleType, finiteTupleType:
		return 1
	case typedTupleType:
		return t.t.compare(u.t)
	default:
		return -1
	}
}

type blingType struct {
	tag string
}

func (t blingType) compare(u typeScheme) int {
	switch u := u.(type) {
	case simpleType, finiteTupleType, typedTupleType:
		return 1
	case blingType:
		if t.tag < u.tag {
			return -1
		}
		if t.tag == u.tag {
			return 0
		}
		return 1
	default:
		return -1
	}
}

type listType []typeScheme

func (t listType) compare(u typeScheme) int {
	switch u := u.(type) {
	case simpleType, finiteTupleType, typedTupleType, blingType:
		return 1
	case listType:
		diff := len(t) - len(u)
		if diff != 0 {
			return diff
		}
		for i := 0; i < len(t); i++ {
			diff := (t)[i].compare((u)[i])
			if diff != 0 {
				return diff
			}
		}
		return 0
	default:
		return -1
	}
}

func singleType(t simpleType) alternateType {
	return alternateType{t}
}
