package compiler

import (
	"reflect"

	"github.com/tim-hardcastle/Pipefish/source/ast"
	"github.com/tim-hardcastle/Pipefish/source/values"
)

var BASE_TYPES = map[string]values.ValueType{
	"ok":      values.SUCCESSFUL_VALUE,
	"int":     values.INT,
	"string":  values.STRING,
	"rune":    values.RUNE,
	"bool":    values.BOOL,
	"float":   values.FLOAT,
	"error":   values.ERROR,
	"type":    values.TYPE,
	"pair":    values.PAIR,
	"list":    values.LIST,
	"map":     values.MAP,
	"set":     values.SET,
	"label":   values.LABEL,
	"func":    values.FUNC,
	"null":    values.NULL,
	"snippet": values.SNIPPET,
	"secret":  values.SECRET,
}

func NewCommonTypeMap() TypeSys {
	result := TypeSys{}
	anyType := values.MakeAbstractType()
	for k, v := range BASE_TYPES {
		result[k] = values.MakeAbstractType(v)
		if v != values.SUCCESSFUL_VALUE && v != values.NULL {
			anyType = anyType.Insert(v)
		}
	}
	result["any"] = anyType
	for _, abType := range []string{"enum", "struct"} {
		result[abType] = values.MakeAbstractType()
	}
	for name, baseType := range ClonableTypes {
		result["clones{"+name+"}"] = values.MakeAbstractType(baseType)
	}
	result["tuple"] = values.MakeAbstractType(values.TUPLE)
	result["ref"] = values.MakeAbstractType(values.REF)
	result["self"] = values.MakeAbstractType(values.UNDEFINED_TYPE)
	result["bling"] = values.MakeAbstractType(values.BLING)
	return result
}

var ClonableTypes = map[string]values.ValueType{"float": values.FLOAT, "int": values.INT, "list": values.LIST, "map": values.MAP, "pair": values.PAIR, "rune": values.RUNE, "set": values.SET, "snippet": values.SNIPPET, "string": values.STRING}

var AbstractTypesOtherThanAny = []string{"struct", "enum"}

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

func (cp *Compiler) IsSameTypeOrSubtype(maybeSub, maybeSuper ast.TypeNode) bool {
	return cp.GetAbstractTypeFromAstType(maybeSub).IsSubtypeOf(cp.GetAbstractTypeFromAstType(maybeSuper))
}

type TypeSys map[string]values.AbstractType

func (ts TypeSys) String() string {
	result := ""
	for k, v := range ts {
		result = result + "* " + k + " : " + v.String() + "\n"
	}
	return result + "\n"
}

func (cp *Compiler) MakeAbstractSigFromStringSig(sig ast.AstSig) ast.AbstractSig {
	result := make(ast.AbstractSig, sig.Len())
	for i, pair := range sig {
		typename := pair.VarType
		typeToUse := typename
		if typename, ok := typename.(*ast.TypeDotDotDot); ok {
			typeToUse = typename.Right
		}
		abType := cp.GetAbstractTypeFromAstType(typeToUse)
		result[i] = ast.NameAbstractTypePair{pair.VarName, abType}
	}
	return result
}

func (cp *Compiler) TypeExists(name string) bool {
	// Check if it's a shared type: 'int', 'struct', 'like{list}', 'any?' etc.
	if _, ok := cp.Common.Types[name]; ok {
		return true
	}
	// ... or the result should just be in the parser's own type map.
	_, ok := cp.TypeMap[name]
	return ok
}

func (cp *Compiler) GetAbstractTypeFromAstType(typeNode ast.TypeNode) values.AbstractType {
	if typeNode == nil { // This can mark an absence of return types.
		return values.AbstractType{}
	}
	switch typeNode := typeNode.(type) {
	case *ast.TypeWithName:
		return cp.GetAbstractTypeFromTypeName(typeNode.OperatorName)
	case *ast.TypeWithArguments:
		return cp.GetAbstractTypeFromTypeName(typeNode.String())
	case *ast.TypeInfix:
		LHS := cp.GetAbstractTypeFromAstType(typeNode.Left)
		RHS := cp.GetAbstractTypeFromAstType(typeNode.Right)
		if typeNode.Operator == "/" {
			return LHS.Union(RHS)
		}
		if typeNode.Operator == "&" {
			return LHS.Intersect(RHS)
		}
	case *ast.TypeSuffix:
		LHS := cp.GetAbstractTypeFromAstType(typeNode.Left)
		if typeNode.Operator == "?" {
			return LHS.Insert(values.NULL)
		}
		if typeNode.Operator == "!" {
			return LHS.Insert(values.ERROR)
		}
	case *ast.TypeBling:
		return values.AbstractType{[]values.ValueType{values.BLING}}
	case *ast.TypeDotDotDot:
		return cp.GetAbstractTypeFromAstType(typeNode.Right)
	case *ast.TypeWithParameters:
		return cp.GetAbstractTypeFromTypeName(typeNode.Blank().String())
	case *ast.TypeExpression:
		if typeNode.TypeArgs == nil {
			return cp.GetAbstractTypeFromTypeName(typeNode.Operator)
		} else {
			return cp.P.ParTypes[typeNode.Operator].PossibleReturnTypes
		}
	}
	panic("Can't compile type node " + typeNode.String() + " with type " + reflect.TypeOf(typeNode).String())
}

func (cp *Compiler) GetAbstractTypeFromTypeName(name string) values.AbstractType {
	// Check if it's a shared type: 'int', 'struct', 'clones{list}', 'any?' etc.
	if result, ok := cp.Common.Types[name]; ok {
		return result
	}
	// ... or the result should just be in the parser's own type map.
	result := cp.TypeMap[name]
	return result
}
