package service

// Converts values from Pipefish to Go and back for the vm.

import (
	"pipefish/source/err"
	"pipefish/source/values"

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
	case enumType :
		return enumConverter(uint32(v.T), v.V.(int)), true
	case cloneType :
		return cloneConverter(uint32(v.T), v.V), true
	}
	return nil, false
}

// We will pass this as the value of an UNDEFINED_VALUE because it seems appropriate.
type conversionProblem = struct {
	presumedType values.ValueType
	goValue any
}

func convError(t values.ValueType, v any) values.Value {
	return values.Value{values.UNDEFINED_VALUE, conversionProblem{t, v}}
}

func (vm *Vm) goToPipefish(v any, structConverter, enumConverter, cloneConverter func(any) (uint32, any), errorLoc uint32) values.Value {
	
	switch v := v.(type) {

	// So one of several things may be happening.
	
	// (1) Either it's returning us a Go value which is sufficiently like a Pipefish value that we can 
	// just tie a type to it and call it one.
	case bool:
		return values.Value{values.BOOL, v}
	case error:
		return values.Value{values.ERROR, err.Error{ErrorId: "vm/go/runtime/a", Message: v.Error()}}
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
	// (2) The return doctor has turned a multiple return from a Go function into a single Pipefish value
	// of the GoReturn type, which we can now unpack recursively.
	case goTuple:
		result := make([]values.Value, 0, len(v))
		for _, el := range v {
			result = append(result, vm.goToPipefish(el, structConverter, enumConverter, cloneConverter, errorLoc))
		}
		return values.Value{values.TUPLE, result}

	// (3) The writer of the Go function has voluntarily used the values library to emit a Pipefish value
	// of their own, which they presumably intend us to pass on.
	case values.Value: // TODO --- we should check that it does in fact contain a suitable value or we could
		return v       // get some really weird bugs.
	}
	// (4) The struct converter recognizes it as one of Pipefish's own struct types which we Go-ified.
	// It returns the struct type and the field values which we can then turn recursively into Pipefish values.
	structType, fVals := structConverter(v)
	gVals := fVals.([]any)
	if structType != 0 {
		pVals := make([]values.Value, 0, len(gVals))
		for _, gVal := range gVals {
			pVals = append(pVals, vm.goToPipefish(gVal, structConverter, enumConverter, cloneConverter, errorLoc))
		}
		return values.Value{values.ValueType(structType), pVals}
	}
	// (5) The enum converter recognizes the type as a Go-ified Pipefish enum. The converter also returns 
	// the index of the element, so we produce the value and send it on its way.,
	eType, num := enumConverter(v)
	if eType != 0 { // Which is never used for anything, and so as may well be a sentinel.
		return values.Value{values.ValueType(eType), num.(int)}
	}
	// (6) The clone converter recognizes the goification of a clone and returns the clone's type 
	// and its contents. If it's a float, integer or string clone then this is no problem. But:
	//
	//     (a) If it's a list clone then its elements may not be Pipefishable.
	//     (b) If it's a set clone, then the constituent elements may also not be Pipefishable,
	//         or if they are, they may not be allowed to be elements of sets.
    //     (c) If it's a pair, then besides the usual problem it may not be two elements long.

	cType, val := cloneConverter(v)
	if cType != 0 { // The sentinel value.
		switch vm.concreteTypeInfo[cType].(cloneType).parent {
		case values.INT, values.FLOAT, values.STRING :
			return values.Value{values.ValueType(cType), val}
		case values.LIST:
			vec := vector.Empty
			for _, el := range val.([]any) {
				pfEl := vm.goToPipefish(el, structConverter, enumConverter, cloneConverter, errorLoc)
				if pfEl.T == values.UNDEFINED_VALUE {
					return pfEl
				}
				vec = vec.Conj(pfEl)
			}
			return values.Value{values.ValueType(cType), vec}
		case values.PAIR :
			goPair := val.([]any)
			if len(goPair) != 2 {
				return convError(values.ValueType(cType), val)
			}
			leftEl := vm.goToPipefish(goPair[0], structConverter, enumConverter, cloneConverter, errorLoc)
			if leftEl.T == values.UNDEFINED_VALUE {
				return leftEl
			}
			rightEl := vm.goToPipefish(goPair[1], structConverter, enumConverter, cloneConverter, errorLoc)
			if rightEl.T == values.UNDEFINED_VALUE {
				return rightEl
			}
			return values.Value{values.ValueType(cType), []any{leftEl, rightEl}}
		case values.SET :
			pfSet := values.Set{}
			for _, el := range val.([]any) {
				pfEl := vm.goToPipefish(el, structConverter, enumConverter, cloneConverter, errorLoc)
				if pfEl.T == values.UNDEFINED_VALUE {
					return pfEl
				}
				pfSet = pfSet.Add(pfEl)
			}
			return values.Value{values.ValueType(cType), pfSet}
		}
	}
    // (7) We're beat and we're out.
	return convError(values.UNDEFINED_VALUE, v)
}

// Because the above function will be applied recursively, we apply the goTuple type to the values
// we first get back from Go so as to distinguish a tuple from a list.

type goTuple []any 
