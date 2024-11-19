package service

// Converts values from Pipefish to Go and back for the vm.

import (
	"pipefish/source/dtypes"
	"pipefish/source/report"
	"pipefish/source/token"
	"pipefish/source/values"
)

// How the vm performs conversion at runtime.
func (vm *Vm) pipefishToGo(v values.Value, converter func(uint32, []any) any) any {
	typeInfo := vm.concreteTypes[v.T]
	if typeInfo.isStruct() {
		pVals := v.V.([]values.Value)
		gVals := make([]any, 0, len(pVals))
		for _, v := range pVals {
			gVals = append(gVals, vm.pipefishToGo(v, converter))
		}
		return converter(uint32(v.T), gVals)
	}
	if typeInfo.isEnum() {
		return v.V.(int)
	}
	switch v.T {
	case values.BOOL:
		return v.V.(bool)
	case values.FLOAT:
		return v.V.(float64)
	case values.INT:
		return v.V.(int)
	case values.RUNE:
		return v.V.(rune)
	case values.STRING:
		return v.V.(string)
	default:
		panic("Can't convert Pipefish value.")
	}
}

// How the VM performs conversion at runtime.
func (vm *Vm) goToPipefish(v any, structConverter func(any) (uint32, []any, bool), enumConverter func(any) (uint32, int)) values.Value {
	switch v := v.(type) {
	case *values.GoReturn:
		result := make([]values.Value, 0, len(v.Elements))
		for _, el := range v.Elements {
			result = append(result, vm.goToPipefish(el, structConverter, enumConverter))
		}
		return values.Value{values.TUPLE, result}
	case bool:
		return values.Value{values.BOOL, v}
	case error:
		return values.Value{values.ERROR, report.Error{Message: v.Error()}}
	case float64:
		return values.Value{values.FLOAT, v}
	case int:
		return values.Value{values.INT, v}
	case nil:
		return values.Value{values.NULL, v}
	case rune:
		return values.Value{values.RUNE, v}
	case string:
		return values.Value{values.STRING, v}
	case values.Value:
		return v
	}
	structType, gVals, ok := structConverter(v)
	if ok {
		pVals := make([]values.Value, 0, len(gVals))
		for _, gVal := range gVals {
			pVals = append(pVals, vm.goToPipefish(gVal, structConverter, enumConverter))
		}
		return values.Value{values.ValueType(structType), pVals}
	}
	enumType, val := enumConverter(v)
	if enumType != 0 { // Which is never used for anything, and so as may well be a sentinel.
		return values.Value{values.ValueType(enumType), val}
	}

	return values.Value{values.ERROR, &report.Error{ErrorId: "golang/conv/a", Token: &token.Token{Source: "golang conversion function"}}}
}

func (cp *Compiler) CloseTypeDeclarations(goHandler *GoHandler) {
	structsToCheck := goHandler.StructNames
	for newStructsToCheck := make(dtypes.Set[string]) ; len(structsToCheck) > 0; {
		for structName := range structsToCheck {
			structTypeNumber := cp.StructNameToTypeNumber[structName]
			for _, fieldType := range cp.Vm.concreteTypes[structTypeNumber].(structType).abstractStructFields {
				if fieldType.Len() != 1 {
					cp.Throw("golang/type/concrete/a", token.Token{Source: "golang interop"}, cp.Vm.DescribeAbstractType(fieldType, LITERAL))
				}
				typeOfField := fieldType.Types[0]
				switch fieldData := cp.Vm.concreteTypes[typeOfField].(type) {
				case cloneType : 
					goHandler.CloneNames.Add(fieldData.name)
				case enumType :
					goHandler.EnumNames.Add(fieldData.name)
				case structType :
					if !goHandler.StructNames.Contains(fieldData.name) {
						newStructsToCheck.Add(fieldData.name)
						goHandler.StructNames.Add(fieldData.name)
					}
				default :
					// As other type-checking needs to be done for things that are not in structs anyway,
					// it would be superfluous to do it here.	
				}
			}
		}
		structsToCheck = newStructsToCheck
	}
}

