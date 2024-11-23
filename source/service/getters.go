// Some small functions which answer the question "Given datum x, how do I find y?"

package service

import "pipefish/source/values"

func (cp *Compiler) getAbstractType(name string) (values.AbstractType, bool) {
	return cp.P.SafeGetAbstractType(name)
}

func(cp *Compiler) getTypeNameFromNumber(typeNumber values.ValueType) string {
	return cp.Vm.concreteTypeInfo[typeNumber].getName(DEFAULT)
}

func (cp *Compiler) getConcreteType(name string) (values.ValueType, bool) {
	abstractType, ok := cp.getAbstractType(name)
	if !ok || abstractType.Len() != 1 {
		return values.UNDEFINED_VALUE, false
	}
	return abstractType.Types[0], true
}

func (cp *Compiler) concreteTypeNow(name string) (values.ValueType) {
	abstractType, _ := cp.getAbstractType(name)
	return abstractType.Types[0]
}

func (cp *Compiler) typeInfoNow(name string) (typeInformation) {
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
