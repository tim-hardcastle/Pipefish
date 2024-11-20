// Some small functions which answer the question "Given datum x, how do I find y?"

package service

import "pipefish/source/values"

func (cp *Compiler) getAbstractType(name string) (values.AbstractType, bool) {
	return cp.P.SafeGetAbstractType(name)
}

func (cp *Compiler) getConcreteType(name string) (values.ValueType, bool) {
	abstractType, ok := cp.getAbstractType(name)
	if !ok || abstractType.Len() != 1 {
		return values.UNDEFINED_VALUE, false
	}
	return abstractType.Types[0], true
}

func (cp *Compiler) getTypeInformation(name string) (typeInformation, bool) {
	concreteType, ok := cp.getConcreteType(name)
	if !ok {
		return nil, false
	}
	return cp.Vm.concreteTypeInfo[concreteType], true
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
