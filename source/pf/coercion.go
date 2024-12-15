package pf

import (
	"errors"
	"reflect"

	"github.com/tim-hardcastle/Pipefish/source/compiler"
)

// Tries to turn a given Pipefish value into a given Go type. As it necessarily has return
// type `any` (plus an error if the coercion is impossible) the value returned will then
// still need downcasting to the type it was coerced to.
//
// E.g:
// func TwoPlusTwo() int {
//     v, _ := fooService.Do(`2 + 2`)                   // `v` has type `Value`.
//     i := fooService.ToGo(v, reflect.TypeFor[int]())  // `i` has type `any`.
//     return i.(int)                                   // We return an integer as required.
//}
//
// The error will be non-nil if the coercion is impossible.
func (sv *Service) ToGo (pfValue Value, goType reflect.Type) (any, error) {
	pfTypeInfo := sv.cp.Vm.ConcreteTypeInfo[pfValue.T]
	pfTypeName := pfTypeInfo.GetName(compiler.LITERAL)
	goTypeName := goType.String()
	myError := errors.New("cannot coerce Pipefish value of type '" + pfTypeName + 
	                       "' to Go value of type '" + goTypeName + "'")
	if goType.Kind() == reflect.Pointer {
		goDatum, e := sv.ToGo(pfValue, goType.Elem())
		if e != nil {
			return nil, e
		}
		return &goDatum, nil
	}
	if goType == reflect.TypeFor[any]() {
		var ok bool
		goType, ok = DEFAULT_TYPE_FOR[pfValue.T]
		if !ok {
			return nil, myError
		}
	}
	var goDatum any
	if structInfo, ok := pfTypeInfo.(compiler.StructType); ok {
		if goType.Kind() != reflect.Struct || structInfo.Len() != goType.NumField() {
			return nil, myError
		}
		goStruct := reflect.New(goType).Elem()
		for i, pfFieldValue := range pfValue.V.([]Value) {
			goFieldDatum, e := sv.ToGo(pfFieldValue, goType.FieldByIndex([]int{i}).Type)
			if e != nil {
				return nil, e
			}
			goStruct.Field(i).Set(reflect.ValueOf(goFieldDatum))
		}
		return goStruct.Interface(), nil
	}
	pfBaseType := UNDEFINED_TYPE
	if _, ok := pfTypeInfo.(compiler.BuiltinType); ok {
		pfBaseType = pfValue.T
	}
	if cloneType, ok := pfTypeInfo.(compiler.CloneType); ok {
		pfBaseType = cloneType.Parent
	}
	if _, ok := pfTypeInfo.(compiler.EnumType); ok {
		pfBaseType = INT
	}
	switch pfBaseType {
	case UNDEFINED_TYPE : // Then we didn't find anything we could do with it.
		return nil, myError
	case INT:
		switch goType.Kind() {
		case reflect.Int :
			goDatum = pfValue.V.(int)
		case reflect.Int8 :
			goDatum = int8(pfValue.V.(int))
		case reflect.Int16 :
			goDatum = int16(pfValue.V.(int))
		case reflect.Int32 :
			goDatum = int32(pfValue.V.(int))
		case reflect.Int64 :
			goDatum = int64(pfValue.V.(int))
		case reflect.Uint :
			goDatum = uint(pfValue.V.(int))
		case reflect.Uint8 :
			goDatum = uint8(pfValue.V.(int))
		case reflect.Uint16 :
			goDatum = uint16(pfValue.V.(int))
		case reflect.Uint32 :
			goDatum = uint32(pfValue.V.(int))
		case reflect.Uint64 :
			goDatum = uint64(pfValue.V.(int))
		default :
			return nil, myError	
		}
	case BOOL:
		if goType.Kind() != reflect.Bool {
			return nil, myError	
		}
		goDatum = pfValue.V.(bool)
	case STRING:
		if goType.Kind() != reflect.String {
			return nil, myError	
		}
		goDatum = pfValue.V.(bool)
	case RUNE:
		if goType.Kind() != reflect.Int32 {
			return nil, myError	
		}
		goDatum = pfValue.V.(rune)
	case FLOAT:
		switch goType.Kind() {
		case reflect.Float32 :
			goDatum = float32(pfValue.V.(float64))
		case reflect.Float64 :
			goDatum = pfValue.V.(float64)
		default :
			return nil, myError	
		}
	case TUPLE, PAIR, LIST :
		var pfValues []Value 
		if pfValue.T == LIST {
			vec := pfValue.V.(List)
			for i := 0; i <= vec.Len() ; i++ {
				pfElement, _ := vec.Index(i)
				pfValues = append(pfValues, pfElement.(Value))
			}
		} else {
			pfValues = pfValue.V.([]Value)
		}
		goElements := pfValue.V.([]Value)
		switch goType.Kind() {
		case reflect.Array :
			if goType.Len() != len(goElements) {
				return nil, myError
			}
			goArray := reflect.New(goType).Elem()
			for i, pfElement := range pfValues {
				goFieldDatum, e := sv.ToGo(pfElement, goType.Elem())
				if e != nil {
					return nil, e
				}
				goArray.Index(i).Set(reflect.ValueOf(goFieldDatum))
			}
			goDatum = goArray.Interface()
		case reflect.Slice :
			goSlice := reflect.New(goType).Elem()
			goSlice.SetCap(len(goElements))
			goSlice.SetLen(len(goElements))
			for i, pfElement := range pfValue.V.([]Value) {
				goElement, e := sv.ToGo(pfElement, goType.Elem())
				if e != nil {
					return nil, e
				}
				goSlice.Index(i).Set(reflect.ValueOf(goElement))
			}
			goDatum = goSlice.Interface()
		}
	case MAP:
		if goType.Kind() != reflect.Map {
			return nil, myError	
		}
		pfMap := pfValue.V.(Map)
		goMap := reflect.MakeMap(goType)
		for _, pfKeyValuePair := range pfMap.AsSlice() {
			goKeyDatum, keyError := sv.ToGo(pfKeyValuePair.Key, goType.Key())
			if keyError != nil {
				return nil, keyError
			}
			goValDatum, valError := sv.ToGo(pfKeyValuePair.Val, goType.Elem())
			if valError != nil {
				return nil, valError
			}
			goMap.SetMapIndex(reflect.ValueOf(goKeyDatum), reflect.ValueOf(goValDatum))
		}
		goDatum = goMap.Interface()
	case SET:
		if goType.Kind() != reflect.Map || goType.Elem() != reflect.TypeFor[struct{}]() {
			return nil, myError	
		}
		pfSet := pfValue.V.(Set)
		goSet := reflect.MakeMap(goType)
		for _, pfElement := range pfSet.AsSlice() {
			goElementDatum, elementError := sv.ToGo(pfElement, goType.Key())
			if elementError != nil {
				return nil, elementError
			}
			goSet.SetMapIndex(reflect.ValueOf(goElementDatum), reflect.ValueOf(struct{}{}))
		}
		goDatum = goSet.Interface()
	default :
		return nil, myError
	}
	// This takes care of the assigned types.
	return reflect.ValueOf(goDatum).Convert(goType).Interface(), nil
} 

// What we convert to when we convert to `any`.
var DEFAULT_TYPE_FOR = map[Type]reflect.Type{
	INT: reflect.TypeFor[int](),
	BOOL: reflect.TypeFor[bool](),
	STRING: reflect.TypeFor[string](),
	FLOAT: reflect.TypeFor[float64](),
	LIST: reflect.TypeFor[[]any](),
	PAIR: reflect.TypeFor[[2]any](),
	MAP: reflect.TypeFor[map[any]any](),
	SET: reflect.TypeFor[map[any]struct{}](),
	TUPLE: reflect.TypeFor[[]any](),
}