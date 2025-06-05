package ast

import (
	"github.com/tim-hardcastle/Pipefish/source/dtypes"
	"github.com/tim-hardcastle/Pipefish/source/values"
)

// This exists because we have too many ways of representing types. There I said it.
type NameTypePair interface {
	GetName() string
	GetType() any // TODO --- actually some sort of interface over all the type representations might be quite useful if I could figure out what it should do.
}

type NameTypeAstPair struct {
	VarName string
	VarType TypeNode
}

func (ntp NameTypeAstPair) GetName() string {
	return ntp.VarName
}

func (ntp NameTypeAstPair) GetType() any {
	return ntp.VarType
}

type AstSig []NameTypeAstPair

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
	return m.VarType.Equals(values.MakeAbstractType(values.BLING))
}

func (m NameAbstractTypePair) Matches(n NameAbstractTypePair) bool {
	if m.IsBling() && n.IsBling() {
		return n.VarName == m.VarName
	}
	return m.VarType.Equals(n.VarType)
}

type AbstractSig []NameAbstractTypePair

func (p AbstractSig) String() string {
	result := "("
	sep := ""
	for _, pair := range p {
		result = result + sep + pair.VarName + " " + pair.VarType.String()
		sep = ", "
	}
	return result + ")"
}

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
	if ns == nil {
		return "nil sig ast"
	}
	for _, v := range ns {
		if result != "" {
			result = result + ", "
		}
		if v.VarType == nil {
			result = result + v.VarName + " " + "no type"
		} else {
			result = result + v.VarName + " " + v.VarType.String()
		}
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
