package vm

import "strconv"

const (
	ERROR uint32 = iota
	INT
	BOOL
	STRING
	FLOAT
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
