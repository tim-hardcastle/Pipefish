package service

// Converts values from Pipefish to Go and back for the vm.

import (
	"pipefish/source/err"
	"pipefish/source/values"
	"reflect"

	"src.elv.sh/pkg/persistent/vector"
)

// How the vm performs conversion at runtime.
func (vm *Vm) pipefishToGo(v values.Value, cloneConverter (func(uint32, any) any), enumConverter (func(uint32, int) any), structConverter (func(uint32, []any) any)) (any, bool) {
	switch v.T {
	case values.BOOL:
		return v.V.(bool), true
	case values.FLOAT:
		return v.V.(float64), true
	case values.INT:
		return v.V.(int), true
	case values.RUNE:
		return v.V.(rune), true
	case values.STRING:
		return v.V.(string), true
	}
	typeInfo := vm.concreteTypeInfo[v.T]
	switch typeInfo.(type) {
	case enumType :
		return enumConverter(uint32(v.T), v.V.(int)), true
	case structType :
		pVals := v.V.([]values.Value)
		gVals := make([]any, 0, len(pVals))
		for _, v := range pVals {
			newGVal , ok := vm.pipefishToGo(v, cloneConverter, enumConverter, structConverter)
			if !ok {
				return newGVal, false     // 'false' meaning, this is the culprit.
			}
			gVals = append(gVals, newGVal)
		}
		return structConverter(uint32(v.T), gVals), true
	case cloneType :
		return cloneConverter(uint32(v.T), v.V), true
	}
	return nil, false
}

func (vm *Vm) goToPipefish(goValue any, goToPipefishConverter func(any) (uint32, any), errorLoc uint32) values.Value {
	
	switch goValue := goValue.(type) {

	// So one of several things may be happening.
	
	// (1) Either it's returning us a Go value which is sufficiently like a Pipefish value that we can 
	// just tie a type to it and call it one.
	case bool:
		return values.Value{values.BOOL, goValue}
	case error:
		return values.Value{values.ERROR, err.Error{ErrorId: "vm/go/runtime", Message: goValue.Error()}}
	case float64:
		return values.Value{values.FLOAT, goValue}
	case int:
		return values.Value{values.INT, goValue}
	case nil:
		return values.Value{values.NULL, goValue}
	case rune:
		return values.Value{values.RUNE, goValue}
	case string:
		return values.Value{values.STRING, goValue}
	// (2) Go returned a tuple which we immediately wrapped up in the goTuple type to mark it as one, 
	// and which we can now unpack recursively.
	case goTuple:
		result := make([]values.Value, 0, len(goValue))
		for _, el := range goValue {
			result = append(result, vm.goToPipefish(el, goToPipefishConverter, errorLoc))
		}
		return values.Value{values.TUPLE, result}
	}
	// (3) We have been returned one of the user-defined Pipefish types that the Go code has access to.
	uint32Type, someValue := goToPipefishConverter(goValue)
	if uint32Type != 0 { // Then the jig is up. We return the goValue so that the VM can return an error.
		
		switch typeInfo := vm.concreteTypeInfo[uint32Type].(type) {
		case enumType:
			return values.Value{values.ValueType(uint32Type), someValue.(int)}
		case structType:
			goValues := someValue.([]any)
			pipefishValues := make([]values.Value, 0, len(goValues))
			for _, goElement := range goValues {
				pipefishValues = append(pipefishValues, vm.goToPipefish(goElement, goToPipefishConverter, errorLoc))
			}
			return values.Value{values.ValueType(uint32Type), pipefishValues}
		case cloneType:
			switch typeInfo.parent {
			case values.INT, values.FLOAT, values.STRING :
				return values.Value{values.ValueType(uint32Type), someValue}
			case values.LIST:
				vec := vector.Empty
				for _, goElement := range someValue.([]any) {
					pipefishElement := vm.goToPipefish(goElement, goToPipefishConverter, errorLoc)
					if pipefishElement.T == values.UNDEFINED_VALUE {
						return pipefishElement
					}
					vec = vec.Conj(pipefishElement)
				}
				return values.Value{values.ValueType(uint32Type), vec}
			case values.PAIR :
				goPair := someValue.([]any)
				if len(goPair) != 2 {
					return values.Value{values.UNDEFINED_VALUE, []any{"vm/go/pair", uint32Type, len(goPair)}}
				}
				leftEl := vm.goToPipefish(goPair[0], goToPipefishConverter, errorLoc)
				if leftEl.T == values.UNDEFINED_VALUE {
					return leftEl
				}
				rightEl := vm.goToPipefish(goPair[1], goToPipefishConverter, errorLoc)
				if rightEl.T == values.UNDEFINED_VALUE {
					return rightEl
				}
				return values.Value{values.ValueType(uint32Type), []any{leftEl, rightEl}}
			case values.SET :
				pfSet := values.Set{}
				for _, el := range someValue.([]any) {
					pfEl := vm.goToPipefish(el, goToPipefishConverter, errorLoc)
					if pfEl.T == values.UNDEFINED_VALUE {
						return pfEl
					}
					pfSet = pfSet.Add(pfEl)
				}
				return values.Value{values.ValueType(uint32Type), pfSet}
			}
		}
	}
	return values.Value{values.UNDEFINED_VALUE, []any{"vm/go/type", reflect.TypeOf(goValue)}}
}

// Because the above function will be applied recursively, we apply the goTuple type to the values
// we first get back from Go so as to distinguish a tuple from a list.

type goTuple []any 
