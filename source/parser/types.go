package parser

import (
	"strconv"

	"github.com/tim-hardcastle/Pipefish/source/ast"
	"github.com/tim-hardcastle/Pipefish/source/values"
)

var baseTypes = map[string]values.ValueType{
	"ok":     values.SUCCESSFUL_VALUE,
	"int":    values.INT,
	"string": values.STRING,
	"rune":   values.RUNE,
	"bool":   values.BOOL,
	"float":  values.FLOAT,
	"error":  values.ERROR,
	"type":   values.TYPE,
	"pair":   values.PAIR,
	"list":   values.LIST,
	"map":    values.MAP,
	"set":    values.SET,
	"label":  values.LABEL,
	"func":   values.FUNC,
	"null":   values.NULL,
	"snippet": values.SNIPPET,
	"secret": values.SECRET,
}

func NewCommonTypeMap() TypeSys {
	result := TypeSys{}
	anyType := values.MakeAbstractType()
	for k, v := range baseTypes {
		result[k] = values.MakeAbstractType(v)
		if v != values.SUCCESSFUL_VALUE && v != values.NULL {
			anyType = anyType.Insert(v)
			result[k+"?"] = values.MakeAbstractType(values.NULL, v)
		}
	}
	singleAndNull := anyType.Insert(values.NULL)
	result["any"] = anyType
	result["any?"] = singleAndNull
	for _, abType := range []string{"enum", "struct"} {
		result[abType] = values.MakeAbstractType()
		result[abType+"?"] = values.MakeAbstractType(values.NULL)
	}
	for name, baseType := range ClonableTypes {
		result[name+"like"] = values.MakeAbstractType(baseType)
		result[name+"like?"] = values.MakeAbstractType(values.NULL, baseType)
	}
	result["tuple"] = values.MakeAbstractType(values.TUPLE)
	result["ref"] = values.MakeAbstractType(values.REF)
	result["self"] = values.MakeAbstractType(values.UNDEFINED_TYPE)
	result["bling"] = values.MakeAbstractType(values.BLING)
	return result
}

var ClonableTypes = map[string]values.ValueType{"float": values.FLOAT, "int": values.INT, "list": values.LIST, "map": values.MAP, "pair": values.PAIR, "rune": values.RUNE, "set": values.SET, "snippet": values.SNIPPET, "string": values.STRING}

var AbstractTypesOtherThanSingle = []string{"struct", "enum"}

func IsMoreSpecific(sigA, sigB ast.AbstractSig) (result bool, ok bool) {
	if len(sigB) == 0 {
		result = true
		ok = true
		return
	}
	if len(sigA) == 0 {
		result = false
		ok = true
		return
	}
	var aIsMoreSpecific, bIsMoreSpecific bool
	max := len(sigA)
	if len(sigB) > max {
		max = len(sigB)
	}
	for i := 0; i < max; i++ {
		if i < len(sigA) && i < len(sigB) && sigA[i].IsBling() && sigB[i].IsBling() {
			if sigA[i].VarName == sigB[i].VarName {
				continue
			} else {
				return false, true
			}
		}
		if i >= len(sigB) || i >= len(sigA) {
			return aIsMoreSpecific, true
		}
		aType := sigA[i].VarType
		bType := sigB[i].VarType
		if aType.PartlyIntersects(bType) {
			return false, false
		}
		asubb := aType.IsProperSubtypeOf(bType)
		bsuba := bType.IsProperSubtypeOf(aType)
		aIsMoreSpecific = aIsMoreSpecific || asubb
		bIsMoreSpecific = bIsMoreSpecific || bsuba
		if aIsMoreSpecific && bIsMoreSpecific {
			return false, false
		}
		if (i == len(sigA)-1) && (i == len(sigB)-1) && sigA[i].Matches(sigB[i]) {
			if !(aIsMoreSpecific || bIsMoreSpecific) {
				return false, false
			} else {
				return aIsMoreSpecific, true
			}
		}
		if !(asubb || bsuba || sigA[i].VarType.Equals(sigB[i].VarType)) {
			return false, true
		}
	}
	if !(aIsMoreSpecific || bIsMoreSpecific) {
		return false, false
	}
	return aIsMoreSpecific, true
}

func (p *Parser) IsSameTypeOrSubtype(maybeSub, maybeSuper string) bool {
	subLen, ok := GetLengthFromType(maybeSub)
	if ok {
		if maybeSuper == "string" || maybeSuper == "any" || maybeSuper == "tuple" {
			return true
		}
		superLen, ok := GetLengthFromType(maybeSuper)
		return ok && subLen <= superLen
	}

	return maybeSub == maybeSuper || p.GetAbstractType(maybeSub).IsSubtypeOf(p.GetAbstractType(maybeSuper))
}

func GetLengthFromType(maybeVarchar string) (int, bool) {
	if len(maybeVarchar) >= 10 {
		if maybeVarchar[0:8] == "varchar(" {
			vcLen, _ := strconv.Atoi(maybeVarchar[8 : len(maybeVarchar)-1])
			return vcLen, true
		}
	}
	return 0, false
}

func GetNullabilityFromType(maybeNullable string) bool {
	return maybeNullable == "null" || (len(maybeNullable)-1) == '?' || len(maybeNullable) > 9 && maybeNullable[0:9] == "varchar?("
}

func TypeIsStringlike(maybeString string) bool {
	return maybeString == "string" || maybeString == "string?" || (len(maybeString) >= 10 &&
		(maybeString[0:8] == "varchar("))
}

func UnnullType(maybeNulled string) string {
	if maybeNulled[len(maybeNulled)-1] == '?' {
		return maybeNulled[0 : len(maybeNulled)-1]
	} else {
		return maybeNulled
	}
}



