package service

// Converts values from Pipefish to Go and back for the vm.

import (
	"errors"
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
	case values.FUNC:
		lambda := v.V.(Lambda)
		goLambda := func(args ...any) []any { // Note that this largely repeats the logic of the VM's Dofn operation, and any changes needed here will probably need to be reflected there.
			if len(args) != len(lambda.sig) { // TODO: variadics.
				return []any{errors.New("wrong number of arguments for function")}
			}
			pfArgs := []values.Value{}
			for _, goArg := range args {
				pfArgs = append(pfArgs, vm.goToPipefish(reflect.ValueOf(goArg)))
			}
			for i := 0; i < int(lambda.capturesEnd-lambda.capturesStart); i++ {
				vm.Mem[int(lambda.capturesStart)+i] = lambda.captures[i]
			}
			for i := 0; i < int(lambda.parametersEnd-lambda.capturesEnd); i++ {
				vm.Mem[int(lambda.capturesEnd)+i] = pfArgs[i]
			}
			success := true
			if lambda.sig != nil {
				for i, abType := range lambda.sig { // TODO --- as with other such cases there will be a threshold at which linear search becomes inferior to binary search and we should find out what it is.
					success = false
					if abType.Types == nil {
						success = true
						continue
					} else {
						for _, ty := range abType.Types {
							if ty == vm.Mem[int(lambda.capturesEnd)+i].T {
								success = true
								if vm.Mem[int(lambda.capturesEnd)+i].T == values.STRING && len(vm.Mem[int(lambda.capturesEnd)+i].V.(string)) > abType.Len() {
									success = false
								}
							}
						}
					}
					if !success {
						return []any{errors.New("arguments of wrong type for sig")}
					}
				}
			}
			vm.Run(lambda.addressToCall)
			result := vm.Mem[lambda.resultLocation]
			if result.T == values.TUPLE {
				results := []any{}
				for _, pfVal := range result.V.([]values.Value) {
					goVal, ok := vm.pipefishToGo(pfVal)
					if !ok {
						return []any{errors.New("can't convert Pipefish return value")}
					}
					results = append(results, goVal)
				}
				return results
			}
			goResult, ok := vm.pipefishToGo(result)
			if !ok {
				return []any{errors.New("can't convert Pipefish return value")}
			}
			return []any{goResult}
		}
		return goLambda, true
	}

	constructor := vm.GoConverter[v.T]
	if constructor != nil {
		typeInfo := vm.ConcreteTypeInfo[v.T]
		switch typeInfo := typeInfo.(type) {
		case BuiltinType:
			return constructor(uint32(v.T), v.V), true
		case EnumType:
			return constructor(uint32(v.T), v.V.(int)), true
		case StructType:
			pVals := v.V.([]values.Value)
			gVals := make([]any, 0, len(pVals))
			for _, w := range pVals {
				newGVal, ok := vm.pipefishToGo(w)
				if !ok {
					return newGVal, false // 'false' meaning, this is the culprit.
				}
				gVals = append(gVals, newGVal)
			}
			return constructor(uint32(v.T), gVals), true
		case CloneType:
			switch typeInfo.Parent {
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
				return constructor(uint32(v.T), result), true
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
	return v, false // So if it comes back false, we know which Pipefish value was the culprit.
}

func (vm *Vm) goToPipefish(goValue reflect.Value) values.Value {
	if goValue.Kind() == reflect.Invalid { // We returned 'nil'.
		return values.Value{values.NULL, nil}
	}
	someGoDatum := goValue.Interface()
	uint32Type, ok := vm.GoToPipefishTypes[reflect.TypeOf(someGoDatum)]
	if ok {
		pipefishType := values.ValueType(uint32Type)
		switch typeInfo := vm.ConcreteTypeInfo[uint32Type].(type) {
		case BuiltinType:
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
		case EnumType:
			return values.Value{pipefishType, int(reflect.ValueOf(someGoDatum).Int())}
		case StructType:
			// At this point someValue must contain a struct of type Foo where uint32
			goValue := reflect.ValueOf(someGoDatum)
			pipefishValues := make([]values.Value, 0, goValue.NumField())
			for i := 0; i < goValue.NumField(); i++ {
				pipefishValues = append(pipefishValues, vm.goToPipefish(goValue.FieldByIndex([]int{i})))
			}
			return values.Value{values.ValueType(uint32Type), pipefishValues}
		case CloneType:
			switch typeInfo.Parent {
			case values.INT:
				return values.Value{values.ValueType(uint32Type), int(reflect.ValueOf(someGoDatum).Int())}
			case values.FLOAT:
				return values.Value{values.ValueType(uint32Type), reflect.ValueOf(someGoDatum).Float()}
			case values.STRING:
				return values.Value{values.ValueType(uint32Type), reflect.ValueOf(someGoDatum).String()}
			case values.RUNE:
				return values.Value{values.ValueType(uint32Type), rune(reflect.ValueOf(someGoDatum).Int())}
			case values.LIST:
				vec := vector.Empty
				goList := goValue.Convert(reflect.TypeFor[[]any]()).Interface().([]any)
				for _, goElement := range goList {
					pipefishElement := vm.goToPipefish(reflect.ValueOf(goElement))
					if pipefishElement.T == values.UNDEFINED_TYPE || pipefishElement.T == values.ERROR {
						return pipefishElement
					}
					vec = vec.Conj(pipefishElement)
				}
				return values.Value{values.ValueType(uint32Type), vec}
			case values.MAP:
				goMap := goValue.Convert(reflect.TypeFor[map[any]any]()).Interface().(map[any]any)
				pfMap := &values.Map{}
				for goKey, goEl := range goMap {
					pfKey := vm.goToPipefish(reflect.ValueOf(goKey))
					if pfKey.T == values.UNDEFINED_TYPE || pfKey.T == values.ERROR {
						return pfKey
					}
					pfEl := vm.goToPipefish(reflect.ValueOf(goEl))
					if pfEl.T == values.UNDEFINED_TYPE || pfEl.T == values.ERROR {
						return pfEl
					}
					pfMap = pfMap.Set(pfKey, pfEl)
				}
				return values.Value{values.ValueType(uint32Type), pfMap}
			case values.PAIR:
				goPair := goValue.Convert(reflect.TypeFor[[2]any]()).Interface().([2]any)
				leftEl := vm.goToPipefish(reflect.ValueOf(goPair[0]))
				if leftEl.T == values.UNDEFINED_TYPE || leftEl.T == values.ERROR {
					return leftEl
				}
				rightEl := vm.goToPipefish(reflect.ValueOf(goPair[1]))
				if rightEl.T == values.UNDEFINED_TYPE || rightEl.T == values.ERROR {
					return rightEl
				}
				return values.Value{values.ValueType(uint32Type), []values.Value{leftEl, rightEl}}
			case values.SET:
				pfSet := values.Set{}
				goSet := goValue.Convert(reflect.TypeFor[map[any]struct{}]()).Interface().(map[any]struct{})
				for el := range goSet {
					pfEl := vm.goToPipefish(reflect.ValueOf(el))
					if pfEl.T == values.UNDEFINED_TYPE || pfEl.T == values.ERROR {
						return pfEl
					}
					pfSet = pfSet.Add(pfEl)
				}
				return values.Value{values.ValueType(uint32Type), pfSet}
			}
		}
	}
	switch someValue := someGoDatum.(type) {
	case goTuple:
		result := make([]values.Value, 0, len(someValue))
		for _, el := range someValue {
			result = append(result, vm.goToPipefish(reflect.ValueOf(el)))
		}
		return values.Value{values.TUPLE, result}
	}
	switch {
	case goValue.Kind() == reflect.Slice || goValue.Kind() == reflect.Array && goValue.Len() != 2: // 2 is pairs.
		vec := vector.Empty
		for i := 0; i < goValue.Len(); i++ {
			goElement := goValue.Index(i)
			pfElement := vm.goToPipefish(goElement)
			if pfElement.T == values.UNDEFINED_TYPE || pfElement.T == values.ERROR {
				return pfElement
			}
			vec = vec.Conj(pfElement)
		}
		return values.Value{values.LIST, vec}
	case goValue.Kind() == reflect.Array && goValue.Len() == 2:
		pair := []values.Value{}
		for i := 0; i < goValue.Len(); i++ {
			goElement := goValue.Index(i)
			pfElement := vm.goToPipefish(goElement)
			if pfElement.T == values.UNDEFINED_TYPE || pfElement.T == values.ERROR {
				return pfElement
			}
			pair = append(pair, pfElement)
		}
		return values.Value{values.PAIR, pair}
	case goValue.Kind() == reflect.Map:
		rangeType := goValue.Type().Elem()
		if rangeType.Kind() == reflect.Struct && rangeType.NumField() == 0 { // Then we have a set.
			iterator := goValue.MapRange()
			pfSet := values.Set{}
			for iterator.Next() {
				goEl := iterator.Key()
				pfEl := vm.goToPipefish(goEl)
				if pfEl.T == values.UNDEFINED_TYPE || pfEl.T == values.ERROR {
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
				if pfKey.T == values.UNDEFINED_TYPE || pfKey.T == values.ERROR {
					return pfKey
				}
				goEl := iterator.Value()
				pfEl := vm.goToPipefish(goEl)
				if pfEl.T == values.UNDEFINED_TYPE || pfEl.T == values.ERROR {
					return pfEl
				}
				pfMap = pfMap.Set(pfKey, pfEl)
			}
			return values.Value{values.MAP, pfMap}
		}
	case goValue.Kind() == reflect.Struct:
		if goValue.NumField() == 0 { // We use struct{}{} to represent OK.
			return values.OK
		}
	case goValue.CanFloat():
		return values.Value{values.FLOAT, goValue.Float()}
	case goValue.CanInt():
		return values.Value{values.INT, int(goValue.Int())}
	case goValue.CanUint():
		return values.Value{values.INT, int(goValue.Uint())}
	case goValue.Kind() == reflect.Func:
		sig := []values.AbstractType{}
		for i := 0; i < goValue.Type().NumIn(); i++ {
			goType := goValue.Type().In(i)
			uint32Type, ok := vm.GoToPipefishTypes[goType]
			pfType := values.ValueType(uint32Type)
			if !ok {
				switch {
				case goType.Kind() == reflect.Array && goType.Len() == 2:
					pfType = values.PAIR
				case goType.Kind() == reflect.Slice || goType.Kind() == reflect.Array:
					pfType = values.LIST
				case goType.Kind() == reflect.Map: // TODO --- if you can't put this information i goTPipefishTypes you could put it in another map.
					rangeType := goValue.Type().Elem()
					if rangeType.Kind() == reflect.Struct && rangeType.NumField() == 0 {
						pfType = values.SET
					} else {
						pfType = values.MAP
					}
				default:
					return values.Value{values.UNDEFINED_TYPE, []any{"vm/go/type", reflect.TypeOf(someGoDatum).String()}}
				}
			}
			sig = append(sig, values.AbstractType{Types: []values.ValueType{pfType}})
		}
		pfLambda := Lambda{gocode: &goValue, sig: sig}
		return values.Value{values.FUNC, pfLambda}
	}
	return values.Value{values.UNDEFINED_TYPE, []any{"vm/go/type", reflect.TypeOf(someGoDatum).String()}}
}

// Because the above function will be applied recursively, we apply the goTuple type to the values
// we first get back from Go so as to distinguish a tuple from a list.
type goTuple []any
