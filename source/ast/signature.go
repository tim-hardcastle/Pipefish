package ast

import (
	"pipefish/source/dtypes"
)

// This exists because we have too many ways of representing types. There I said it.
type NameTypePair interface {
	GetName() string
	GetType() any // TODO --- actually some sort of interface over all the type representations might be quite useful if I could figure out what it should do.
}

type NameTypenamePair struct {
	VarName string
	VarType string
}

func (ntp NameTypenamePair) GetName() string {
	return ntp.VarName
}

func (ntp NameTypenamePair) GetType() any {
	return ntp.VarType
}

func (ntp NameTypenamePair) TypeOrBling() string {
	if ntp.VarType == "bling" {
		return ntp.VarName
	}
	return ntp.VarType
}

type AstSig []NameTypenamePair

func (s AstSig) Len() int {
	return len(s)
}

func (s AstSig) GetVarType(i int) any {
	return s[i].GetType()
}

func (s AstSig) GetVarName(i int) string {
	return s[i].VarName
}

func (ns AstSig) String() (result string) {
	for _, v := range ns {
		if result != "" {
			result = result + ", "
		}
		result = result + v.VarName + " " + v.VarType
	}
	result = "(" + result + ")"
	return
}

func (ns AstSig) NameSet() (result dtypes.Set[string]) {
	for _, v := range ns {
		result.Add(v.VarName)
	}
	return
}
