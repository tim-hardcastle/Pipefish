// Some small functions which answer the question "Given datum x, how do I find y?"

package compiler

import (
	"os"
	"reflect"
	"strings"

	"github.com/tim-hardcastle/Pipefish/source/ast"
	"github.com/tim-hardcastle/Pipefish/source/text"
	"github.com/tim-hardcastle/Pipefish/source/values"
	"github.com/tim-hardcastle/Pipefish/source/vm"
)

func (cp *Compiler) getAbstractType(name string) (values.AbstractType) {
	return cp.P.GetAbstractTypeFromTypeSys(name)
}

func (cp *Compiler) GetTypeNameFromNumber(typeNumber values.ValueType) string {
	return cp.Vm.ConcreteTypeInfo[typeNumber].GetName(vm.DEFAULT)
}

func (cp *Compiler) GetConcreteType(name string) (values.ValueType, bool) {
	abstractType := cp.getAbstractType(name)
	if abstractType.Len() != 1 {
		return values.UNDEFINED_TYPE, false
	}
	return abstractType.Types[0], true
}

func (cp *Compiler) ConcreteTypeNow(name string) values.ValueType {
	abstractType := cp.getAbstractType(name)
	return abstractType.Types[0]
}

func (cp *Compiler) TypeInfoNow(name string) vm.TypeInformation {
	concreteType, _ := cp.GetConcreteType(name)
	return cp.Vm.ConcreteTypeInfo[concreteType]
}

func (cp *Compiler) getTypeInformation(name string) (vm.TypeInformation, bool) {
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
		_, ok := typeInfo.(vm.BuiltinType)
		return ok
	}
}

func (cp *Compiler) isEnum(name string) bool {
	typeInfo, ok := cp.getTypeInformation(name)
	if !ok {
		return false
	} else {
		_, ok := typeInfo.(vm.EnumType)
		return ok
	}
}

func (cp *Compiler) isClone(name string) bool {
	typeInfo, ok := cp.getTypeInformation(name)
	if !ok {
		return false
	} else {
		_, ok := typeInfo.(vm.CloneType)
		return ok
	}
}

func (cp *Compiler) IsStruct(name string) bool {
	typeInfo, ok := cp.getTypeInformation(name)
	if !ok {
		return false
	} else {
		_, ok := typeInfo.(vm.StructType)
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
	_, ok := cp.Vm.ConcreteTypeInfo[T].(vm.StructType)
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

func (cp *Compiler) ReturnSigToAlternateType(sig ast.AstSig) FiniteTupleType {
	if sig == nil {
		return nil
	}
	ftt := FiniteTupleType{}
	for _, pair := range sig {
		ftt = append(ftt, cp.GetAlternateTypeFromTypeAst(pair.VarType))
	}
	return ftt
}

func (cp *Compiler) rtnTypesToTypeScheme(rtnSig ast.AbstractSig) AlternateType {
	if len(rtnSig) == 0 {
		return cp.Common.AnyTypeScheme
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
	if typeRep == nil {
		return AltType()
	}
	switch typeRep := typeRep.(type) {
	case ast.TypeNode:
		return cp.GetAlternateTypeFromTypeAst(typeRep)
	case AlternateType:
		return typeRep
	default:
		panic("Found unexpected type " + reflect.TypeOf(typeRep).String())
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

func (cp *Compiler) GetAlternateTypeFromTypeAst(typeNode ast.TypeNode) AlternateType {
	if typeNode, ok := typeNode.(*ast.TypeDotDotDot); ok {
		return AlternateType{TypedTupleType{cp.GetAlternateTypeFromTypeAst(typeNode.Right)}}
	}
	abType := cp.P.GetAbstractType(typeNode)
	return AbstractTypeToAlternateType(abType)
}

func (cp *Compiler) GetAlternateTypeFromConcreteTypeName(name string) AlternateType {
	return AlternateType{SimpleType(cp.ConcreteTypeNow(name))}
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
