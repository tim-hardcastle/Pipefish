// Some small functions which answer the question "Given datum x, how do I find y?"

package compiler

import (
	"os"
	"strings"

	"github.com/tim-hardcastle/Pipefish/source/ast"
	"github.com/tim-hardcastle/Pipefish/source/text"
	"github.com/tim-hardcastle/Pipefish/source/values"
)

func (cp *Compiler) getAbstractType(name string) (values.AbstractType, bool) {
	return cp.P.SafeGetAbstractType(name)
}

func (cp *Compiler) GetTypeNameFromNumber(typeNumber values.ValueType) string {
	return cp.Vm.ConcreteTypeInfo[typeNumber].GetName(DEFAULT)
}

func (cp *Compiler) GetConcreteType(name string) (values.ValueType, bool) {
	abstractType, ok := cp.getAbstractType(name)
	if !ok || abstractType.Len() != 1 {
		return values.UNDEFINED_TYPE, false
	}
	return abstractType.Types[0], true
}

func (cp *Compiler) ConcreteTypeNow(name string) values.ValueType {
	abstractType, _ := cp.getAbstractType(name)
	return abstractType.Types[0]
}

func (cp *Compiler) TypeInfoNow(name string) typeInformation {
	concreteType, _ := cp.GetConcreteType(name)
	return cp.Vm.ConcreteTypeInfo[concreteType]
}

func (cp *Compiler) getTypeInformation(name string) (typeInformation, bool) {
	concreteType, ok := cp.GetConcreteType(name)
	if !ok {
		return nil, false
	}
	return cp.Vm.ConcreteTypeInfo[concreteType], true
}

func (cp *Compiler) IsBuiltin(name string) bool {
	typeInfo, ok := cp.getTypeInformation(name)
	if !ok {
		return false
	} else {
		_, ok := typeInfo.(BuiltinType)
		return ok
	}
}

func (cp *Compiler) isEnum(name string) bool {
	typeInfo, ok := cp.getTypeInformation(name)
	if !ok {
		return false
	} else {
		_, ok := typeInfo.(EnumType)
		return ok
	}
}

func (cp *Compiler) isClone(name string) bool {
	typeInfo, ok := cp.getTypeInformation(name)
	if !ok {
		return false
	} else {
		_, ok := typeInfo.(CloneType)
		return ok
	}
}

func (cp *Compiler) IsStruct(name string) bool {
	typeInfo, ok := cp.getTypeInformation(name)
	if !ok {
		return false
	} else {
		_, ok := typeInfo.(StructType)
		return ok
	}
}

func (cp *Compiler) IsPrivate(a values.AbstractType) bool {
	for _, w := range a.Types {
		if cp.Vm.ConcreteTypeInfo[w].IsPrivate() {
			return true
		}
	}
	return false
}

func (cp *Compiler) typeNumberIsStruct(T values.ValueType) bool {
	_, ok := cp.Vm.ConcreteTypeInfo[T].(StructType)
	return ok
}

func (cp *Compiler) alternateTypeIsOnlyStruct(aT AlternateType) (values.ValueType, bool) {
	if len(aT) == 1 {
		switch el := aT[0].(type) {
		case SimpleType:
			if cp.typeNumberIsStruct(values.ValueType(el)) {
				return values.ValueType(el), true
			}
		default:
			return values.UNDEFINED_TYPE, false
		}
	}
	return values.UNDEFINED_TYPE, false
}

func (cp *Compiler) alternateTypeIsOnlyAssortedStructs(aT AlternateType) bool {
	for _, el := range aT {
		switch el := el.(type) {
		case SimpleType:
			if !cp.typeNumberIsStruct(values.ValueType(el)) {
				return false
			}
		default:
			return false
		}
	}
	return true
}

func (cp *Compiler) ReturnSigToAlternateType(sig ast.StringSig) FiniteTupleType {
	if sig == nil {
		return nil
	}
	ftt := FiniteTupleType{}
	for _, pair := range sig {
		ftt = append(ftt, cp.GetAlternateTypeFromTypeName(pair.VarType))
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
	tup := FiniteTupleType{}
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
		return cp.GetAlternateTypeFromTypeName(typeRep)
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

func (cp *Compiler) GetAlternateTypeFromTypeName(typename string) AlternateType {
	varargs := len(typename) >= 3 && typename[:3] == "..."
	if varargs {
		typename = typename[3:]
	}
	result, ok := cp.TypeNameToTypeScheme[typename]
	if ok {
		if varargs {
			return AlternateType{TypedTupleType{result}}
		}
		return result
	}
	result, ok = cp.Vm.SharedTypenameToTypeList[typename]
	if ok {
		if varargs {
			return AlternateType{TypedTupleType{result}}
		}
		return result
	}
	return AlternateType{}
}

// Manufactures a value.
func val(T values.ValueType, V any) values.Value {
	return values.Value{T: T, V: V}
}

func GetSourceCode(scriptFilepath string) (string, error) {
	var sourcebytes []byte
	var err error
	if scriptFilepath != "" { // In which case we're making a blank VM.
		if len(scriptFilepath) >= 11 && scriptFilepath[:11] == "test-files/" {
			sourcebytes, err = TestFolder.ReadFile(scriptFilepath)
		} else {
			sourcebytes, err = os.ReadFile(text.MakeFilepath(scriptFilepath))
		}
		if err != nil {
			return "", err
		}
	}
	sourcebytes = append(sourcebytes, '\n')
	return string(sourcebytes), nil
}
