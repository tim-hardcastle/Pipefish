package parser

import (
	"strconv"

	"charm/source/ast"
	"charm/source/digraph"
	"charm/source/object"
	"charm/source/signature"
)

type TypeSystem = *digraph.Digraph[string]

func NewTypeSystem() TypeSystem {
	T := make(digraph.Digraph[string])
	T.AddTransitiveArrow("single", "tuple")
	for _, t := range BaseTypes {
		T.AddTransitiveArrow(t, "single")
		T.AddTransitiveArrow(t, "single?")
		if t != "null" {
			T.AddTransitiveArrow(t, t + "?")
			T.AddTransitiveArrow("null", t + "?")
		}
	}
	T.AddTransitiveArrow("single", "single?")
	T.AddTransitiveArrow("label", "label?")
	T.AddTransitiveArrow("enum", "label")
	T.AddTransitiveArrow("field", "label")
	T.AddTransitiveArrow("enum", "enum?")
	T.AddTransitiveArrow("field", "field?")
	return &T
}

func TypeExists(s string, t TypeSystem) bool {
	_, ok := (*t)[s]
	return ok
}

var BaseTypes = []string{"int", "float64", "bool", "string", "error", "type", "list",
	"pair", "set", "map", "func", "struct", "label", "table", "code", "null"}

func IsMoreSpecific(typesystem TypeSystem, sigA, sigB signature.Signature) (result bool, ok bool) {
	if len(sigA) > len(sigB) {
		if len(sigB) == 0 || sigB[len(sigB)-1].VarType != "tuple" {
			result = false
			ok = true
			return
		}
	}
	if len(sigB) > len(sigA) {
		if len(sigA) == 0 || sigA[len(sigA)-1].VarType != "tuple" {
			result = false
			ok = true
			return
		}
	}
	var aIsMoreSpecific, bIsMoreSpecific bool
	for i := 0; i < len(sigA); i++ {
		if sigA[i].VarType == "bling" && sigB[i].VarType == "bling" && sigA[i].VarName != sigB[i].VarName {
			result = false
			ok = true
			return
		}
		asubb := typesystem.PointsTo(sigA[i].VarType, sigB[i].VarType)
		bsuba := typesystem.PointsTo(sigB[i].VarType, sigA[i].VarType)
		aIsMoreSpecific = aIsMoreSpecific || asubb
		bIsMoreSpecific = bIsMoreSpecific || bsuba
		if aIsMoreSpecific && bIsMoreSpecific {
			result = false
			ok = false
			return
		}
		if ((i == len(sigA) - 1) && (i == len(sigB) - 1) && sigA[i].TypeOrBling() == sigB[i].TypeOrBling()) {
			if !(aIsMoreSpecific || bIsMoreSpecific) {
				result = false
				ok = false
				return
			} else {
				result = aIsMoreSpecific
				ok = true
				return
			}
		}
		if !(asubb || bsuba || sigA[i].VarType == sigB[i].VarType) {
			result = false
			ok = true
			return
		}
	}
	if !(aIsMoreSpecific || bIsMoreSpecific) {
		result = false
		ok = false
		return
	}
	result = aIsMoreSpecific
	ok = true
	return
}

func IsObjectInType(typesystem TypeSystem, obj object.Object, ty string) bool {
	return IsSameTypeOrSubtype(typesystem, object.InnerType(obj), ty)
}

func IsSameTypeOrSubtype(T TypeSystem, maybeSub, maybeSuper string) bool {
	subLen, ok := getLengthFromType(maybeSub)
	if ok {
		if maybeSuper == "string" || maybeSuper == "single" {
			return true
		}
		superLen, ok := getLengthFromType(maybeSuper)
		return ok && subLen <= superLen 
	}

	return maybeSub == maybeSuper || T.PointsTo(maybeSub, maybeSuper)
}

func getLengthFromType(maybeVarchar string) (int, bool) {
	if len(maybeVarchar) < 9 {
		return 0, false
	}
	if maybeVarchar[0:8] != "varchar(" {
		return 0, false
	}
	varcharLen, _ := strconv.Atoi(maybeVarchar[8:len(maybeVarchar) - 1])
	return varcharLen, true
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
