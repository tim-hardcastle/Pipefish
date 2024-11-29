package service

// Converts values from Pipefish to Go and back for the vm.

import (
	"pipefish/source/err"
	"pipefish/source/values"
	"reflect"

	"src.elv.sh/pkg/persistent/vector"
)

func (vm *Vm) pipefishToGo(v values.Value) (any, bool) {
	switch v.T {
	case values.LIST:
		result := []any{}
		for it := v.V.(vector.Vector).Iterator(); it.HasElem(); it.Next() {
			pfEl := it.Elem()
			goEl, ok := vm.pipefishToGo(pfEl.(values.Value))
			if !ok {
				return goEl, false
			}
			result = append(result, goEl)
		}
		return result, true
	case values.MAP:
		result := map[any]any{}
		mapAsSlice := v.V.(*values.Map).AsSlice()
		for _, pair := range mapAsSlice {
			goKey, ok := vm.pipefishToGo(pair.Key)
			if !ok {
				return goKey, false
			}
			goVal, ok := vm.pipefishToGo(pair.Val)
			if !ok {
				return goVal, false
			}
			result[goKey] = goVal
		}
		return result, true
	case values.PAIR:
		leftPf := v.V.([]values.Value)[0]
		leftGo, ok := vm.pipefishToGo(leftPf)
		if !ok {
			return leftGo, false
		}
		rightPf := v.V.([]values.Value)[1]
		rightGo, ok := vm.pipefishToGo(rightPf)
		if !ok {
			return rightGo, false
		}
		return [2]any{leftGo, rightGo}, true
	case values.SET:
		result := map[any]struct{}{}
		setAsSlice := v.V.(values.Set).AsSlice()
		for _, pfEl := range setAsSlice {
			goEl, ok := vm.pipefishToGo(pfEl)
			if !ok {
				return goEl, false
			}
			result[goEl] = struct{}{}
		}
		return result, true
	case values.TUPLE:
		result := []any{}
		for _, pfEl := range v.V.([]values.Value) {
			goEl, ok := vm.pipefishToGo(pfEl)
			if !ok {
				return goEl, false
			}
			result = append(result, goEl)
		}
		return result, true
	}
	constructor := vm.goConverter[v.T]
	if constructor != nil {
		typeInfo := vm.concreteTypeInfo[v.T]
		switch typeInfo := typeInfo.(type) {
		case builtinType :
			return constructor(uint32(v.T), v.V), true
		case enumType :
			return constructor(uint32(v.T), v.V.(int)), true
		case structType :
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
			switch typeInfo.parent {
			case values.FLOAT, values.INT, values.RUNE, values.STRING:
				return constructor(uint32(v.T), v.V), true
			case values.LIST:
				result := []any{}
				for it := v.V.(vector.Vector).Iterator(); it.HasElem(); it.Next() {
					pfEl := it.Elem()
					goEl, ok := vm.pipefishToGo(pfEl.(values.Value))
					if !ok {
						return goEl, false
					}
					result = append(result, goEl)
				}
				return constructor(uint32(v.T), result), true
			case values.MAP :
				result := map[any]any{}
				mapAsSlice := v.V.(*values.Map).AsSlice()
				for _, pair := range mapAsSlice {
					goKey, ok := vm.pipefishToGo(pair.Key)
					if !ok {
						return goKey, false
					}
					goVal, ok := vm.pipefishToGo(pair.Val)
					if !ok {
						return goVal, false
					}
					result[goKey] = goVal
				}
				return constructor(uint32(v.T), result), true
			case values.PAIR :
				leftPf := v.V.([]values.Value)[0]
				leftGo, ok := vm.pipefishToGo(leftPf)
				if !ok {
					return leftGo, false
				}
				rightPf := v.V.([]values.Value)[1]
				rightGo, ok := vm.pipefishToGo(rightPf)
				if !ok {
					return rightGo, false
				}
				return constructor(uint32(v.T), [2]any{leftGo, rightGo}), true
			case values.SET:
				result := map[any]struct{}{}
				setAsSlice := v.V.(values.Set).AsSlice()
				for _, pfEl := range setAsSlice {
					goEl, ok := vm.pipefishToGo(pfEl)
					if !ok {
						return goEl, false
					}
					result[goEl] = struct{}{}
				}
				return constructor(uint32(v.T), result), true
			}

		}
	}
	return v, false  // So if it comes back false, we know which Pipefish value was the culprit.
}

func (vm *Vm) goToPipefish(goValue reflect.Value) values.Value {
	someGoDatum := goValue.Interface()
	uint32Type, ok := vm.goToPipefishTypes[reflect.TypeOf(someGoDatum)]
	if ok {
		pipefishType := values.ValueType(uint32Type)
		switch typeInfo := vm.concreteTypeInfo[uint32Type].(type) {
		case builtinType :
			switch pipefishType {
			case values.BOOL:
				return values.Value{values.BOOL, someGoDatum.(bool)}
			case values.FLOAT:
				return values.Value{values.FLOAT, someGoDatum.(float64)}
			case values.INT:
				return values.Value{values.INT, someGoDatum.(int)}
			case values.RUNE:
				return values.Value{values.RUNE, someGoDatum.(rune)}
			case values.STRING:
				return values.Value{values.STRING, someGoDatum.(string)}
			}
		case enumType :
			return values.Value{pipefishType, int(reflect.ValueOf(someGoDatum).Int())}
		case structType :
			// At this point someValue must contain a struct of type Foo where uint32
			goValue := reflect.ValueOf(someGoDatum)
			pipefishValues := make([]values.Value, 0, goValue.NumField())
			for i := 0; i < goValue.NumField(); i++ {
				pipefishValues = append(pipefishValues, vm.goToPipefish(goValue.FieldByIndex([]int{i})))
			}
			return values.Value{values.ValueType(uint32Type), pipefishValues}
		case cloneType :
			switch typeInfo.parent {
			case values.INT :
				return values.Value{values.ValueType(uint32Type), int(reflect.ValueOf(someGoDatum).Int())}
			case values.FLOAT :
				return values.Value{values.ValueType(uint32Type), reflect.ValueOf(someGoDatum).Float()}
			case values.STRING :
				return values.Value{values.ValueType(uint32Type), reflect.ValueOf(someGoDatum).String()}
			case values.RUNE :
				return values.Value{values.ValueType(uint32Type), rune(reflect.ValueOf(someGoDatum).Int())}
			case values.LIST:
				vec := vector.Empty
				goList := goValue.Convert(reflect.TypeFor[[]any]()).Interface().([]any)
				for _, goElement := range goList {
					pipefishElement := vm.goToPipefish(reflect.ValueOf(goElement))
					if pipefishElement.T == values.UNDEFINED_VALUE || pipefishElement.T == values.ERROR {
						return pipefishElement
					}
					vec = vec.Conj(pipefishElement)
				}
				return values.Value{values.ValueType(uint32Type), vec}
			case values.MAP :
				goMap := goValue.Convert(reflect.TypeFor[map[any]any]()).Interface().(map[any]any)
				pfMap := &values.Map{}
				for goKey, goEl := range goMap {
					pfKey := vm.goToPipefish(reflect.ValueOf(goKey))
					if pfKey.T == values.UNDEFINED_VALUE || pfKey.T == values.ERROR {
						return pfKey
					}
					pfEl := vm.goToPipefish(reflect.ValueOf(goEl))
					if pfEl.T == values.UNDEFINED_VALUE || pfEl.T == values.ERROR {
						return pfEl
					}
					pfMap = pfMap.Set(pfKey, pfEl)
				}
			return values.Value{values.ValueType(uint32Type), pfMap}
			case values.PAIR :
				goPair := goValue.Convert(reflect.TypeFor[[2]any]()).Interface().([2]any)
				leftEl := vm.goToPipefish(reflect.ValueOf(goPair[0]))
				if leftEl.T == values.UNDEFINED_VALUE || leftEl.T == values.ERROR {
					return leftEl
				}
				rightEl := vm.goToPipefish(reflect.ValueOf(goPair[1]))
				if rightEl.T == values.UNDEFINED_VALUE || rightEl.T == values.ERROR {
					return rightEl
				}
				return values.Value{values.ValueType(uint32Type), []values.Value{leftEl, rightEl}}
			case values.SET :
				pfSet := values.Set{}
				goSet := goValue.Convert(reflect.TypeFor[map[any]struct{}]()).Interface().(map[any]struct{})
				for el := range goSet {
					pfEl := vm.goToPipefish(reflect.ValueOf(el))
					if pfEl.T == values.UNDEFINED_VALUE || pfEl.T == values.ERROR {
						return pfEl
					}
					pfSet = pfSet.Add(pfEl)
				}
				return values.Value{values.ValueType(uint32Type), pfSet}
			}
		}
	}
	switch someValue := someGoDatum.(type) {
	case error:
		if someValue == nil {
			return values.Value{values.SUCCESSFUL_VALUE, nil}
		}
		return values.Value{values.ERROR, err.Error{ErrorId: "vm/go/runtime", Message: someValue.Error()}}
	case goTuple:
		result := make([]values.Value, 0, len(someValue))
		for _, el := range someValue {
			result = append(result, vm.goToPipefish(reflect.ValueOf(el)))
		}
		return values.Value{values.TUPLE, result}
	}
	switch  {
	case goValue.IsNil() :
		return values.Value{values.NULL, nil}
	case goValue.Kind() == reflect.Slice || goValue.Kind() == reflect.Array && goValue.Len() != 2 : // 2 is pairs.
		vec := vector.Empty
		for i := 0; i < goValue.Len(); i++ {
			goElement := goValue.Index(i)
			pfElement := vm.goToPipefish(goElement)
			if pfElement.T == values.UNDEFINED_VALUE || pfElement.T == values.ERROR {
				return pfElement
			}
			vec = vec.Conj(pfElement)
		}
		return values.Value{values.LIST, vec}
	case goValue.Kind() == reflect.Array && goValue.Len() == 2 :
		pair := []values.Value{}
		for i := 0; i < goValue.Len(); i++ {
			goElement := goValue.Index(i)
			pfElement := vm.goToPipefish(goElement)
			if pfElement.T == values.UNDEFINED_VALUE || pfElement.T == values.ERROR {
				return pfElement
			}
			pair = append(pair, pfElement)
		}
		return values.Value{values.PAIR, pair}
	case goValue.Kind() == reflect.Map :
		rangeType := goValue.Type().Elem()
		if rangeType.Kind() == reflect.Struct && rangeType.NumField() == 0 { // Then we have a set.
			iterator := goValue.MapRange()
			pfSet := values.Set{}
			for iterator.Next() {
				goEl := iterator.Key()
				pfEl := vm.goToPipefish(goEl)
				if pfEl.T == values.UNDEFINED_VALUE || pfEl.T == values.ERROR {
					return pfEl
				}
				pfSet = pfSet.Add(pfEl)
			}
			return values.Value{values.SET, pfSet}
		} else { // We have a map.
			iterator := goValue.MapRange()
			pfMap := &values.Map{}
			for iterator.Next() {
				goKey := iterator.Key()
				pfKey := vm.goToPipefish(goKey)
				if pfKey.T == values.UNDEFINED_VALUE ||  pfKey.T == values.ERROR {
					return pfKey
				}
				goEl := iterator.Value()
				pfEl := vm.goToPipefish(goEl)
				if pfEl.T == values.UNDEFINED_VALUE || pfEl.T == values.ERROR {
					return pfEl
				}
				pfMap = pfMap.Set(pfKey, pfEl)
			}
			return values.Value{values.MAP, pfMap}
		}
	case goValue.Kind() == reflect.Struct :
		if goValue.NumField() == 0 { // We use struct{}{} to represent OK.
			return values.OK
		}
	case goValue.CanFloat() :
		return values.Value{values.FLOAT, goValue.Float()}
	case goValue.CanInt() :
		return values.Value{values.INT, int(goValue.Int())}
	case goValue.CanUint() :
		return values.Value{values.INT, int(goValue.Uint())}
	}
	return values.Value{values.UNDEFINED_VALUE, []any{"vm/go/type", reflect.TypeOf(someGoDatum).String()}}
}

// Because the above function will be applied recursively, we apply the goTuple type to the values
// we first get back from Go so as to distinguish a tuple from a list.
type goTuple []any 
