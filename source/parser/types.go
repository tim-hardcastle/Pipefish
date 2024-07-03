package parser

import (
	"strconv"

	"pipefish/source/ast"
	"pipefish/source/dtypes"
)

type TypeSystem = dtypes.Digraph[string]

func NewTypeSystem() TypeSystem {
	T := make(dtypes.Digraph[string])
	// We put in the top and bottom of our type system by hand.
	T.AddTransitiveArrow("single", "single?")
	T.AddTransitiveArrow("single", "...single")
	T.AddTransitiveArrow("single?", "...single?")
	T.AddTransitiveArrow("null", "...null")
	T.AddTransitiveArrow("null", "single?")
	T.AddTransitiveArrow("single?", "any")
	T.AddTransitiveArrow("tuple", "any")

	// Then for the other built-in concrete and abstract types there is generic stuff we can do.
	for _, t := range BaseTypesOtherThanNull {
		AddType(T, t)
	}
	for _, t := range AbstractTypesOtherThanSingle {
		AddType(T, t)
	}

	// Kludges.
	T.AddTransitiveArrow("outer function", "func")
	T.AddTransitiveArrow("ref", "dummy value")
	T.AddTransitiveArrow("ok", "dummy value")
	return T
}

func TypeExists(s string, t TypeSystem) bool {
	_, ok := t[s]
	return ok
}

// Supertypes includes containing types other than 'single', 'single?', and 'any', which are taken for granted.
func AddType(T TypeSystem, t string, supertypes ...string) {
	T.AddTransitiveArrow(t, t+"?")
	T.AddTransitiveArrow("null", t+"?")
	T.AddTransitiveArrow(t, "..."+t)
	supertypes = append(supertypes, "single")
	for _, st := range supertypes {
		T.AddTransitiveArrow(t, st)
		T.AddTransitiveArrow("..."+t, "..."+st)
	}
}

var BaseTypesOtherThanNull = []string{"int", "float", "bool", "string", "rune", "error", "type", "list", "label",
	"pair", "set", "map", "func", "struct", "label"}

var AbstractTypesOtherThanSingle = []string{"struct", "snippet", "enum"}

func IsMoreSpecific(typesystem TypeSystem, sigA, sigB ast.AstSig) (result bool, ok bool) {
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

func insert(a []*ast.PrsrFunction, value *ast.PrsrFunction, index int) []*ast.PrsrFunction {
	if len(a) == index { // nil or empty slice or after last element
		return append(a, value)
	}
	a = append(a[:index+1], a[index:]...) // index < len(a)
	a[index] = value
	return a
}

func AddInOrder(T TypeSystem, S []*ast.PrsrFunction, f *ast.PrsrFunction) ([]*ast.PrsrFunction, bool) {
	for i := 0; i < len(S); i++ {
		yes, ok := IsMoreSpecific(T, f.Sig, S[i].Sig)
		if !ok {
			return []*ast.PrsrFunction{}, false
		}
		if yes {
			S = insert(S, f, i)
			return S, true
		}
	}
	S = append(S, f)
	return S, true
}
