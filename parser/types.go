package parser

import (
	"charm/ast"
	"charm/digraph"
	"charm/signature"
)

type TypeSystem = *digraph.Digraph[string]

func NewTypeSystem() TypeSystem {
	T := make(digraph.Digraph[string])
	T.AddTransitiveArrow("tuple", "any")
	T.AddTransitiveArrow("single", "tuple")
	for _, t := range BaseTypes {
		T.AddTransitiveArrow(t, "single")
	}
	T.AddTransitiveArrow("nil", "struct")
	return &T
}

func TypeExists(s string, t TypeSystem) bool {
	_, ok := (*t)[s]
	return ok
}

var BaseTypes = []string{"int", "float", "bool", "string", "error", "type", "list", 
/**/ "pair", "set", "map", "func", "struct", "label"}


func IsMoreSpecific(typesystem TypeSystem, sigA, sigB signature.NamedSignature) (result bool, ok bool) {
	if len(sigA) > len(sigB) {
		if sigB[len(sigB) - 1].VarType != "tuple" && sigB[len(sigB) - 1].VarType != "any" {
			result = false; ok = true; return 
		}
	}
	if len(sigB) > len(sigA) {
		if sigA[len(sigA) - 1].VarType != "tuple" && sigA[len(sigA) - 1].VarType != "any" {
			result = false; ok = true; return 
		}
	}
	var aIsMoreSpecific, bIsMoreSpecific bool
	for i := 0; i < len(sigA); i++ {
		asubb := typesystem.PointsTo(sigA[i].VarType, sigB[i].VarType)
		bsuba := typesystem.PointsTo(sigB[i].VarType, sigA[i].VarType)
		aIsMoreSpecific = aIsMoreSpecific || asubb
		bIsMoreSpecific = bIsMoreSpecific || bsuba
		if aIsMoreSpecific && bIsMoreSpecific {
			result = false; ok = false; return
		}
		if !(asubb || bsuba || sigA[i].VarType == sigB[i].VarType) {
			result = false; ok = true; return
		}
	}
	if !(aIsMoreSpecific || bIsMoreSpecific) {
		result = false; ok = false; return 
	}
	result = aIsMoreSpecific; ok = true; return
}

func IsSameTypeOrSubtype(T TypeSystem, maybeSub, maybeSuper string) bool {
	return maybeSub == maybeSuper || T.PointsTo(maybeSub, maybeSuper)
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
	for i := 0 ; i < len(S) ; i++ {
		yes, ok := IsMoreSpecific(T, f.Sig, S[i].Sig)
		if !ok {
			return []ast.Function{}, false}
		if yes {
			S = insert(S, f, i)
			return S, true
		}
	}
	S = append(S, f)
	return S, true
}

func min(a, b int) int {
    if a < b {
        return a
    }
    return b
}