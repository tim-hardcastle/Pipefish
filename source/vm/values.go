package vm

import "strconv"

const ( // Cross-reference with typeNames in
	ERROR uint32 = iota // Some code may depend on the order of early elements.
	NULL
	INT
	BOOL
	STRING
	FLOAT
	UNSAT
	TYPE_ERROR
	LB_ENUMS // I.e the first of the enums.
)

type Value struct {
	T uint32 // Which is clearly too many, but it's nice to have all operands be uint32
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
	UB_OF_PREDEFINED_CONSTS
)

type varAccess int

const (
	V_PUBLIC varAccess = iota
)

type variable struct {
	mLoc   uint32
	access varAccess
	types  typeList
}

type environment struct {
	data map[string]variable
	ext  *environment
}

func newEnvironment() *environment {
	return &environment{data: make(map[string]variable), ext: nil}
}

func (env *environment) exists(name string) bool {
	_, ok := env.data[name]
	return ok
}

// For taliking about inferred types. Lists of valTypes are to be kept in numerical order.
type valType interface {
	concreteType() uint32
	compare(u valType) int
}

type typeList []valType

func (vL typeList) intersect(wL typeList) typeList {
	x := typeList{}
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

func (vL typeList) union(wL typeList) typeList {
	x := typeList{}
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

func (vL typeList) without(t valType) typeList {
	x := typeList{}
	for _, v := range vL {
		if v.compare(t) != 0 {
			x = append(x, v)
		}
	}
	return x
}

func (typeList typeList) only(t uint32) bool {
	return len(typeList) == 1 && typeList[0].concreteType() == t
}

func (typeList typeList) contains(t uint32) bool {
	for _, ty := range typeList {
		if ty.concreteType() == t {
			return true
		}
	}
	return false
}

type simpleType struct {
	t uint32
}

func (t *simpleType) concreteType() uint32 {
	return t.t
}

func (t *simpleType) compare(u valType) int {
	switch u := u.(type) {
	case *simpleType:
		return int(t.t - u.t)
	default:
		return -1
	}
}

func (v *Value) describe() string {
	switch v.T {
	case INT:
		return strconv.Itoa(v.V.(int))
	case STRING:
		return "\"" + v.V.(string) + "\""
	case BOOL:
		if v.V.(bool) {
			return "true"
		} else {
			return "false"
		}
	case FLOAT:
		return strconv.FormatFloat(v.V.(float64), 'g', 8, 64)
	case UNSAT:
		return "unsatisfied conditional"
	case NULL:
		return "null"
	}

	panic("can't describe value")
}
