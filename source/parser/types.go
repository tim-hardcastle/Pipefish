package parser

import (
	"strconv"

	"pipefish/source/ast"
	"pipefish/source/dtypes"
)

type TypeSystem = *dtypes.Digraph[string]

func NewTypeSystem() TypeSystem {
	T := make(dtypes.Digraph[string])
	T.AddTransitiveArrow("single", "tuple")
	for _, t := range BaseTypes {
		T.AddTransitiveArrow(t, "single")
		T.AddTransitiveArrow(t, "single?")
		if t != "null" {
			T.AddTransitiveArrow(t, t+"?")
			T.AddTransitiveArrow("null", t+"?")
		}
	}
	T.AddTransitiveArrow("single", "single?")
	T.AddTransitiveArrow("struct", "single")
	T.AddTransitiveArrow("struct?", "single?")
	T.AddTransitiveArrow("label", "label?")
	T.AddTransitiveArrow("enum", "label")
	T.AddTransitiveArrow("field", "label")
	T.AddTransitiveArrow("enum", "enum?")
	T.AddTransitiveArrow("field", "field?")
	T.AddTransitiveArrow("null", "single?")
	T.AddTransitiveArrow("null", "label?")
	T.AddTransitiveArrow("null", "enum?")
	T.AddTransitiveArrow("null", "field?")
	T.AddTransitiveArrow("null", "struct?")
	T.AddTransitiveArrow("snippet", "snippet?")
	T.AddTransitiveArrow("null", "snippet?")
	T.AddTransitiveArrow("snippet", "struct")
	T.AddTransitiveArrow("snippet?", "struct?")
	T.AddTransitiveArrow("outer function", "func")

	T.AddTransitiveArrow("ref", "dummy value")
	T.AddTransitiveArrow("ok", "dummy value")
	return &T
}

func TypeExists(s string, t TypeSystem) bool {
	_, ok := (*t)[s]
	return ok
}

var BaseTypes = []string{"int", "float", "bool", "string", "error", "type", "list",
	"pair", "set", "map", "func", "struct", "label", "null"}

func IsMoreSpecific(typesystem TypeSystem, sigA, sigB ast.Signature) (result bool, ok bool) {
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
		if i < len(sigA) && i < len(sigB) && sigA[i].VarType == "bling" && sigB[i].VarType == "bling" && sigA[i].VarName != sigB[i].VarName {
			return aIsMoreSpecific, true
		}
		if i >= len(sigB) || i >= len(sigA) {
			return aIsMoreSpecific, true
		}
		asubb := typesystem.PointsTo(sigA[i].VarType, sigB[i].VarType)
		bsuba := typesystem.PointsTo(sigB[i].VarType, sigA[i].VarType)
		aIsMoreSpecific = aIsMoreSpecific || asubb
		bIsMoreSpecific = bIsMoreSpecific || bsuba
		if aIsMoreSpecific && bIsMoreSpecific {
			return false, false
		}
		if (i == len(sigA)-1) && (i == len(sigB)-1) && sigA[i].TypeOrBling() == sigB[i].TypeOrBling() {
			if !(aIsMoreSpecific || bIsMoreSpecific) {
				return false, false
			} else {
				return aIsMoreSpecific, true
			}
		}
		if !(asubb || bsuba || sigA[i].VarType == sigB[i].VarType) {
			return false, true
		}
	}
	if !(aIsMoreSpecific || bIsMoreSpecific) {
		return false, false
	}
	return aIsMoreSpecific, true
}

func IsSameTypeOrSubtype(T TypeSystem, maybeSub, maybeSuper string) bool {
	subLen, ok := GetLengthFromType(maybeSub)
	if ok {
		if maybeSuper == "string" || maybeSuper == "single" || maybeSuper == "tuple" {
			return true
		}
		superLen, ok := GetLengthFromType(maybeSuper)
		return ok && subLen <= superLen
	}

	return maybeSub == maybeSuper || T.PointsTo(maybeSub, maybeSuper)
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

func insert(a []ast.Function, value ast.Function, index int) []ast.Function {
	if len(a) == index { // nil or empty slice or after last element
		return append(a, value)
	}
	a = append(a[:index+1], a[index:]...) // index < len(a)
	a[index] = value
	return a
}

func AddInOrder(T TypeSystem, S []ast.Function, f ast.Function) ([]ast.Function, bool) {
	for i := 0; i < len(S); i++ {
		yes, ok := IsMoreSpecific(T, f.Sig, S[i].Sig)
		if !ok {
			return []ast.Function{}, false
		}
		if yes {
			S = insert(S, f, i)
			return S, true
		}
	}
	S = append(S, f)
	return S, true
}
