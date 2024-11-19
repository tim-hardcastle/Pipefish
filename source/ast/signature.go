package ast

import (
	"pipefish/source/dtypes"
	"pipefish/source/values"
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

type StringSig []NameTypenamePair

type NameAbstractTypePair struct {
	VarName string
	VarType values.AbstractType
}

func (natp NameAbstractTypePair) GetName() string {
	return natp.VarName
}

func (natp NameAbstractTypePair) GetType() any {
	return natp.VarType
}

func (m NameAbstractTypePair) IsBling() bool {
	return m.VarType.Equals(values.AbstractType{[]values.ValueType{values.BLING}, 0})
}

func (m NameAbstractTypePair) Matches(n NameAbstractTypePair) bool {
	if m.IsBling() && n.IsBling() {
		return n.VarName == m.VarName
	}
	return m.VarType.Equals(n.VarType)
}

type AbstractSig []NameAbstractTypePair

func (p AbstractSig) String() string {
	result := ""
	sep := ""
	for _, pair := range p {
		result = sep + result + pair.VarName + " " + pair.VarType.String()
	}
	return result + ")"
}

func (s StringSig) Len() int {
	return len(s)
}

func (s StringSig) GetVarType(i int) any {
	return s[i].GetType()
}

func (s StringSig) GetVarName(i int) string {
	return s[i].VarName
}

func (ns StringSig) String() (result string) {
	for _, v := range ns {
		if result != "" {
			result = result + ", "
		}
		result = result + v.VarName + " " + v.VarType
	}
	result = "(" + result + ")"
	return
}

func (ns StringSig) NameSet() (result dtypes.Set[string]) {
	for _, v := range ns {
		result.Add(v.VarName)
	}
	return
}
