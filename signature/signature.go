package signature

import (
	"charm/set"
)

type NameTypePair = struct {
	VarName string
	VarType string
}

type Signature []NameTypePair

func (ns Signature) String() (result string) {
	for _, v := range(ns) {
		if result != "" {
			result = result + ", "
		}
		result = result + v.VarName + " " + v.VarType
	}
	result = "(" + result + ")"
	return
}

func (ns Signature) NameSet() (result set.Set[string]) {
	for _, v := range(ns) {
		result.Add(v.VarName)
	}
	return
}



