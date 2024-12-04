// Some small functions which answer the question "Given datum x, how do I find y?"

package service

import (
	"strings"

	"pipefish/source/ast"
	"pipefish/source/values"
)

func (cp *Compiler) getAbstractType(name string) (values.AbstractType, bool) {
	return cp.P.SafeGetAbstractType(name)
}

func (cp *Compiler) getTypeNameFromNumber(typeNumber values.ValueType) string {
	return cp.Vm.concreteTypeInfo[typeNumber].getName(DEFAULT)
}

func (cp *Compiler) getConcreteType(name string) (values.ValueType, bool) {
	abstractType, ok := cp.getAbstractType(name)
	if !ok || abstractType.Len() != 1 {
		return values.UNDEFINED_VALUE, false
	}
	return abstractType.Types[0], true
}

func (cp *Compiler) ConcreteTypeNow(name string) values.ValueType {
	abstractType, _ := cp.getAbstractType(name)
	return abstractType.Types[0]
}

func (cp *Compiler) typeInfoNow(name string) typeInformation {
	concreteType, _ := cp.getConcreteType(name)
	return cp.Vm.concreteTypeInfo[concreteType]
}

func (cp *Compiler) getTypeInformation(name string) (typeInformation, bool) {
	concreteType, ok := cp.getConcreteType(name)
	if !ok {
		return nil, false
	}
	return cp.Vm.concreteTypeInfo[concreteType], true
}

func (cp *Compiler) isBuiltin(name string) bool {
	typeInfo, ok := cp.getTypeInformation(name)
	if !ok {
		return false
	} else {
		_, ok := typeInfo.(builtinType)
		return ok
	}
}

func (cp *Compiler) isEnum(name string) bool {
	typeInfo, ok := cp.getTypeInformation(name)
	if !ok {
		return false
	} else {
		_, ok := typeInfo.(enumType)
		return ok
	}
}

func (cp *Compiler) isClone(name string) bool {
	typeInfo, ok := cp.getTypeInformation(name)
	if !ok {
		return false
	} else {
		_, ok := typeInfo.(cloneType)
		return ok
	}
}

func (cp *Compiler) isStruct(name string) bool {
	typeInfo, ok := cp.getTypeInformation(name)
	if !ok {
		return false
	} else {
		_, ok := typeInfo.(structType)
		return ok
	}
}

func (cp *Compiler) isPrivate(a values.AbstractType) bool {
	for _, w := range a.Types {
		if cp.Vm.concreteTypeInfo[w].isPrivate() {
			return true
		}
	}
	return false
}

func (cp *Compiler) typeNumberIsStruct(T values.ValueType) bool {
	_, ok := cp.Vm.concreteTypeInfo[T].(structType)
	return ok
}

func (cp *Compiler) alternateTypeIsOnlyStruct(aT AlternateType) (values.ValueType, bool) {
	if len(aT) == 1 {
		switch el := aT[0].(type) {
		case simpleType:
			if cp.typeNumberIsStruct(values.ValueType(el)) {
				return values.ValueType(el), true
			}
		default:
			return values.UNDEFINED_VALUE, false
		}
	}
	return values.UNDEFINED_VALUE, false
}

func (cp *Compiler) alternateTypeIsOnlyAssortedStructs(aT AlternateType) bool {
	for _, el := range aT {
		switch el := el.(type) {
		case simpleType:
			if !cp.typeNumberIsStruct(values.ValueType(el)) {
				return false
			}
		default:
			return false
		}
	}
	return true
}

func (cp *Compiler) returnSigToAlternateType(sig ast.StringSig) finiteTupleType {
	if sig == nil {
		return nil
	}
	ftt := finiteTupleType{}
	for _, pair := range sig {
		ftt = append(ftt, cp.getAlternateTypeFromTypeName(pair.VarType))
	}
	return ftt
}

func (cp *Compiler) rtnTypesToTypeScheme(rtnSig ast.AbstractSig) AlternateType {
	if len(rtnSig) == 0 {
		return cp.Vm.AnyTypeScheme
	}
	if len(rtnSig) == 1 {
		return AbstractTypeToAlternateType(rtnSig[0].VarType)
	}
	tup := finiteTupleType{}
	for _, v := range rtnSig {
		tup = append(tup, AbstractTypeToAlternateType(v.VarType))
	}
	return AlternateType{tup}
}

// Either we already have an AlternateType, and can return it, or we have a type in the form of a string and
// can transform it into one.
func (cp *Compiler) getTypes(s signature, i int) AlternateType {
	typeRep := s.GetVarType(i)
	switch typeRep := typeRep.(type) {
	case string:
		return cp.getAlternateTypeFromTypeName(typeRep)
	case AlternateType:
		return typeRep
	default:
		panic("Tim, you messed up.")
	}
}

type NameAlternateTypePair struct {
	VarName string
	VarType AlternateType
}

func (ntp NameAlternateTypePair) GetName() string {
	return ntp.VarName
}

func (ntp NameAlternateTypePair) GetType() any {
	return ntp.VarType
}

func getVarNames(sig signature) string {
	names := []string{}
	for i := 0; i < sig.Len(); i++ {
		names = append(names, sig.GetVarName(i))
	}
	return strings.Join(names, ", ")
}

func (cp *Compiler) getAlternateTypeFromTypeName(typename string) AlternateType {
	varargs := len(typename) >= 3 && typename[:3] == "..."
	if varargs {
		typename = typename[3:]
	}
	result, ok := cp.typeNameToTypeScheme[typename]
	if ok {
		if varargs {
			return AlternateType{TypedTupleType{result}}
		}
		return result
	}
	result, ok = cp.Vm.sharedTypenameToTypeList[typename]
	if ok {
		if varargs {
			return AlternateType{TypedTupleType{result}}
		}
		return result
	}
	return AlternateType{}
}

func val(T values.ValueType, V any) values.Value {
	return values.Value{T: T, V: V}
}
