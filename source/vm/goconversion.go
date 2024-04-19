package vm

import (
	"pipefish/source/report"
	"pipefish/source/token"
	"pipefish/source/values"
)

func (vm *Vm) pipefishToGo(v values.Value, converter func(uint32, []any) any) any {
	if v.T >= vm.Ub_enums {
		pVals := v.V.([]values.Value)
		gVals := make([]any, 0, len(pVals))
		for _, v := range pVals {
			gVals = append(gVals, vm.pipefishToGo(v, converter))
		}
		return converter(uint32(v.T), gVals)
	}
	switch v.T {
	case values.INT:
		return v.V.(int)
	case values.FLOAT:
		return v.V.(int)
	case values.STRING:
		return v.V.(string)
	case values.BOOL:
		return v.V.(bool)
	default:
		panic("Can't convert Pipefish value.")
	}
}

func (vm *Vm) goToPipefish(v any, converter func(any) (uint32, []any, bool)) values.Value {
	switch v := v.(type) {
	case *values.GoReturn:
		result := make([]values.Value, 0, len(v.Elements))
		for el := range v.Elements {
			result = append(result, vm.goToPipefish(el, converter))
		}
		return values.Value{values.TUPLE, result}
	case int:
		return values.Value{values.INT, v}
	case float64:
		return values.Value{values.FLOAT, v}
	case string:
		return values.Value{values.STRING, v}
	case bool:
		return values.Value{values.BOOL, v}
	case nil:
		return values.Value{values.NULL, v}
	case error:
		return values.Value{values.ERROR, report.Error{Message: v.Error()}}
	case values.Value:
		return v
	}
	structType, gVals, ok := converter(v)
	if ok {
		pVals := make([]values.Value, 0, len(gVals))
		for _, gVal := range gVals {
			pVals = append(pVals, vm.goToPipefish(gVal, converter))
		}
		return values.Value{values.ValueType(structType), pVals}
	}

	return values.Value{values.ERROR, &report.Error{ErrorId: "vm/go/conv", Token: &token.Token{Source: "golang conversion function"}}}
}
