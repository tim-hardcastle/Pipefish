package service

// Converts values from Pipefish to Go and back for the vm.

import (
	"pipefish/source/err"
	"pipefish/source/values"
	"reflect"

	"src.elv.sh/pkg/persistent/vector"
)

// How the vm performs conversion at runtime.
func (vm *Vm) pipefishToGo(v values.Value) (any, bool) {
	typeInfo := vm.concreteTypeInfo[v.T]
	constructor := typeInfo.getGoConverter()
	switch typeInfo.(type) {
	case builtinType :
		return constructor(uint32(v.T), v.V), true
	case enumType :
		println("Making enum.")
		return constructor(uint32(v.T), v.V.(int)), true
	case structType :
		println("Making struct.")
		pVals := v.V.([]values.Value)
		gVals := make([]any, 0, len(pVals))
		for _, w := range pVals {
			newGVal, ok := vm.pipefishToGo(w)
			if !ok {
				return newGVal, false     // 'false' meaning, this is the culprit.
			}
			gVals = append(gVals, newGVal)
		}
		return constructor(uint32(v.T), gVals), true
	case cloneType :
		println("Making clone.")
		return constructor(uint32(v.T), v.V), true
	}
	return v, false  // So if it comes back false, we know which Pipefish value was the culprit.
}

func (vm *Vm) goToPipefish(goValue reflect.Value) values.Value {
	someValue := goValue.Interface()
	uint32Type, ok := vm.goToPipefishTypes[goValue.Type()]
	if ok {
		pipefishType := values.ValueType(uint32Type)
		switch typeInfo := vm.concreteTypeInfo[uint32Type].(type) {
		case builtinType :
			switch pipefishType {
			case values.BOOL:
				return values.Value{values.BOOL, someValue.(bool)}
			case values.FLOAT:
				return values.Value{values.FLOAT, someValue.(float64)}
			case values.INT:
				return values.Value{values.INT, someValue.(int)}
			case values.RUNE:
				return values.Value{values.RUNE, someValue.(rune)}
			case values.STRING:
				return values.Value{values.BOOL, someValue.(string)}
			}
		case enumType :
			return values.Value{pipefishType, someValue.(int)}
		case structType :
			goValues := someValue.([]any)
			pipefishValues := make([]values.Value, 0, len(goValues))
			for _, goElement := range goValues {
				pipefishValues = append(pipefishValues, vm.goToPipefish(reflect.ValueOf(goElement)))
			}
		case cloneType :
		switch typeInfo.parent {
		case values.INT, values.FLOAT, values.STRING :
			return values.Value{values.ValueType(uint32Type), someValue}
		case values.LIST:
			vec := vector.Empty
			for _, goElement := range someValue.([]any) {
				pipefishElement := vm.goToPipefish(reflect.ValueOf(goElement))
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
			leftEl := vm.goToPipefish(reflect.ValueOf(goPair[0]))
			if leftEl.T == values.UNDEFINED_VALUE {
				return leftEl
			}
			rightEl := vm.goToPipefish(reflect.ValueOf(goPair[1]))
			if rightEl.T == values.UNDEFINED_VALUE {
				return rightEl
			}
			return values.Value{values.ValueType(uint32Type), []any{leftEl, rightEl}}
		case values.SET :
			pfSet := values.Set{}
			for _, el := range someValue.([]any) {
				pfEl := vm.goToPipefish(reflect.ValueOf(el))
				if pfEl.T == values.UNDEFINED_VALUE {
					return pfEl
				}
				pfSet = pfSet.Add(pfEl)
			}
			return values.Value{values.ValueType(uint32Type), pfSet}
		}
	}
	}
	switch someValue := someValue.(type) {
	case error:
		return values.Value{values.ERROR, err.Error{ErrorId: "vm/go/runtime", Message: someValue.Error()}}
	case goTuple:
		result := make([]values.Value, 0, len(someValue))
		for _, el := range someValue {
			result = append(result, vm.goToPipefish(reflect.ValueOf(el)))
		}
		return values.Value{values.TUPLE, result}
	}
	return values.Value{values.UNDEFINED_VALUE, []any{"vm/go/type", reflect.TypeOf(goValue).String()}}
}
	
	

// Because the above function will be applied recursively, we apply the goTuple type to the values
// we first get back from Go so as to distinguish a tuple from a list.

type goTuple []any 
