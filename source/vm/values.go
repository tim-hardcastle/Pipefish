package vm

import "strconv"

const (
	ERROR uint32 = iota // Some code depends on ERROR being first and this should not be changed.
	INT
	BOOL
	STRING
	FLOAT
	TYPE_ERROR
	USER // I.e the first of the enums.
)

var VALUE_MAP = map[uint32]string{
	ERROR:  "ERROR",
	INT:    "INT",
	BOOL:   "BOOL",
	STRING: "STRING",
	FLOAT:  "FLOAT",
}

type Value struct {
	T uint32 // Which is clearly too many, but it's nice to have all operands be uint32
	V any
}

var TRUE = Value{T: BOOL, V: true}
var FALSE = Value{T: BOOL, V: false}

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
		}
		if comp < 0 {
			vix++
		}
		wix++
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
	}

	panic("can't describe value")
}
