package compiler

import (
	"fmt"
	"pipefish/source/dtypes"
	"pipefish/source/values"
	"pipefish/source/vm"
	"strings"
)

type typeScheme interface {
	compare(u typeScheme) int
	describe(mc *vm.Vm) string
}

// Finds all the possible lengths of tuples in a typeScheme. (Single values have length 1. Non-finite tuples have length -1.)
// This allows us to figure out if we need to generate a check on the length of a tuple or whether we can take it for granted
// at compile time.
func lengths(t typeScheme) dtypes.Set[int] {
	result := make(dtypes.Set[int])
	switch t := t.(type) {
	case simpleType:
		result.Add(1)
		return result
	case typedTupleType:
		result.Add(-1)
		return result
	case alternateType:
		for _, v := range t {
			newSet := lengths(v)
			result.AddSet(newSet)
			if result.Contains(-1) {
				return result
			}
		}
		return result
	case finiteTupleType:
		if len(t) == 0 {
			result.Add(0)
			return result
		}
		thisColumnLengths := lengths((t)[0])
		remainingColumnLengths := lengths((t)[1:])
		for j := range thisColumnLengths {
			for k := range remainingColumnLengths {
				result.Add(j + k)
			}
		}
		return result
	}
	panic("We shouldn't be here!")
}

func maxLengthsOrMinusOne(s dtypes.Set[int]) int {
	max := 0
	for k := range s {
		if k == -1 {
			return -1
		}
		if k > max {
			max = k
		}
	}
	return max
}

// This finds all the possible ix-th elements in a typeScheme.
func typesAtIndex(t typeScheme, ix int) alternateType {
	result, _ := recursiveTypesAtIndex(t, ix)
	return result
}

// TODO: This is somewhat wasteful because there ought to be some sensible way to fix it so the algorithm uses data from computing this for
// i - 1. But let's get the VM working first and optimise the compiler later.
func recursiveTypesAtIndex(t typeScheme, ix int) (alternateType, dtypes.Set[int]) {
	resultTypes := alternateType{}
	resultSet := make(dtypes.Set[int])
	switch t := t.(type) {
	case simpleType:
		if ix == 0 {
			resultTypes = resultTypes.union(alternateType{t})
		}
		resultSet.Add(1)
		return resultTypes, resultSet
	case typedTupleType:
		resultTypes = resultTypes.union(t.t)
		resultSet.Add(ix)
		return resultTypes, resultSet
	case alternateType:
		for _, v := range t {
			newTypes, newSet := recursiveTypesAtIndex(v, ix)
			resultTypes = resultTypes.union(newTypes)
			resultSet.AddSet(newSet)
		}
		return resultTypes, resultSet
	case finiteTupleType:
		if len(t) == 0 {
			return resultTypes, resultSet
		}
		resultTypes, resultSet = recursiveTypesAtIndex(t[0], ix)
		for jx := range resultSet {
			newTypes, newSet := recursiveTypesAtIndex(t[1:], ix-jx)
			resultTypes = resultTypes.union(newTypes)
			resultSet.AddSet(newSet)
		}
		return resultTypes, resultSet
	}
	panic("We shouldn't be here!")
}

type simpleType uint32

func (t simpleType) compare(u typeScheme) int {
	switch u := u.(type) {
	case simpleType:
		return int(t) - int(u)
	default:
		return -1
	}
}

func (t simpleType) describe(mc *vm.Vm) string {
	return mc.DescribeType(values.ValueType(t))
}

type alternateType []typeScheme

// This assumes that the alternateType came from the typeNameToTypeList array and therefore contains only elements of
// type simpleType.
func (aT alternateType) ToAbstractType() values.AbstractType {
	result := make(values.AbstractType, 0, len(aT))
	for _, v := range aT {
		result = append(result, values.ValueType(v.(simpleType)))
	}
	return result
}

func (vL alternateType) intersect(wL alternateType) alternateType {
	x := alternateType{}
	var vix, wix int
	for vix < len(vL) && wix < len(wL) {
		comp := vL[vix].compare(wL[wix])
		if comp == 0 {
			x = append(x, vL[vix])
			vix++
			wix++
			continue
		}
		if comp < 0 {
			vix++
			continue
		}
		wix++
	}
	return x
}

func (aT alternateType) describe(mc *vm.Vm) string {
	var buf strings.Builder
	var sep string
	for _, v := range aT {
		fmt.Fprintf(&buf, "%s%s", sep, v.describe(mc))
		sep = "/"
	}
	return buf.String()
}

func (vL alternateType) union(wL alternateType) alternateType {
	x := alternateType{}
	var vix, wix int
	for vix < len(vL) || wix < len(wL) {
		if vix == len(vL) {
			x = append(x, wL[wix])
			wix++
			continue
		}
		if wix == len(wL) {
			x = append(x, vL[vix])
			vix++
			continue
		}
		comp := vL[vix].compare(wL[wix])
		if comp == 0 {
			x = append(x, vL[vix])
			vix++
			wix++
			continue
		}
		if comp < 0 {
			x = append(x, vL[vix])
			vix++
			continue
		}
		x = append(x, wL[wix])
		wix++
	}
	return x
}

func (vL alternateType) without(t typeScheme) alternateType {
	x := alternateType{}
	for _, v := range vL {
		if v.compare(t) != 0 {
			x = append(x, v)
		}
	}
	return x
}

func (aT alternateType) isOnly(vt values.ValueType) bool {
	t := simpleType(vt)
	if len(aT) == 1 {
		switch el := aT[0].(type) {
		case simpleType:
			return el == t
		default:
			return false
		}
	}
	return false
}

func (aT alternateType) isOnlyStruct(ub int) (values.ValueType, bool) {
	if len(aT) == 1 {
		switch el := aT[0].(type) {
		case simpleType:
			if ub <= int(el) {
				return values.ValueType(el), true
			}
		default:
			return values.UNDEFINED_VALUE, false
		}
	}
	return values.UNDEFINED_VALUE, false
}

func (aT alternateType) isOnlyAssortedStructs(ub int) bool {
	for _, el := range aT {
		switch el := el.(type) {
		case simpleType:
			if ub > int(el) {
				return false
			}
		default:
			return false
		}
	}
	return true
}

func (aT alternateType) hasSideEffects() bool {
	for _, u := range aT {
		switch u := u.(type) {
		case simpleType:
			if u == simpleType(values.SUCCESSFUL_VALUE) || u == simpleType(values.BREAK) {
				return true
			}
		}
	}
	return false
}

func (aT alternateType) isLegalCmdReturn() bool {
	for _, u := range aT {
		switch u := u.(type) {
		case simpleType:
			if u != simpleType(values.SUCCESSFUL_VALUE) && u != simpleType(values.ERROR) &&
				u != simpleType(values.BREAK) && u != simpleType(values.UNSAT) {
				return false
			}
		default:
			return false
		}
	}
	return true
}

func (aT alternateType) isLegalReturnFromLoopBody() bool {
	for _, u := range aT {
		switch u := u.(type) {
		case simpleType:
			if u != simpleType(values.SUCCESSFUL_VALUE) && u != simpleType(values.ERROR) &&
				u != simpleType(values.BREAK) {
				return false
			}
		default:
			return false
		}
	}
	return true
}

func (aT alternateType) isLegalDefReturn() bool {
	for _, u := range aT {
		switch u := u.(type) {
		case simpleType:
			if u == simpleType(values.SUCCESSFUL_VALUE) {
				return false
			}
		}
	}
	return true
}

func (aT alternateType) contains(vt values.ValueType) bool {
	t := simpleType(vt)
	if vt == values.TUPLE { // Special-case tuples.
		for _, ty := range aT {
			switch ty.(type) {
			case finiteTupleType:
				return true
			case typedTupleType:
				return true
			}
		}
		return false
	}
	for _, ty := range aT {
		switch el := ty.(type) {
		case simpleType:
			if (el) == t {
				return true
			}
		}
	}
	return false
}

func (aT alternateType) containsOnlyTuples() bool {
	for _, ty := range aT {
		switch el := ty.(type) {
		case simpleType:
			return false
		case alternateType:
			if !el.containsOnlyTuples() {
				return false
			}
		case blingType: //, listType
			return false
		}
	}
	return true
}

func (aT alternateType) isNoneOf(vts ...values.ValueType) bool {
	for _, vt := range vts {
		if aT.contains(vt) {
			return false
		}
	}
	return true
}

func (t alternateType) compare(u typeScheme) int {
	switch u := u.(type) {
	case simpleType:
		return 1
	case alternateType:
		diff := len(t) - len(u)
		if diff != 0 {
			return diff
		}
		for i := 0; i < len(t); i++ {
			diff := (t)[i].compare((u)[i])
			if diff != 0 {
				return diff
			}
		}
		return 0
	default:
		return -1
	}
}

type finiteTupleType []typeScheme // "Finite" meaning that we know its size at compile time.

func (t finiteTupleType) compare(u typeScheme) int {
	switch u := u.(type) {
	case simpleType, alternateType:
		return 1
	case finiteTupleType:
		diff := len(t) - len(u)
		if diff != 0 {
			return diff
		}
		for i := 0; i < len(t); i++ {
			diff := (t)[i].compare((u)[i])
			if diff != 0 {
				return diff
			}
		}
		return 0
	default:
		return -1
	}
}

func (fT finiteTupleType) describe(mc *vm.Vm) string {
	var buf strings.Builder
	buf.WriteString("tuple[")
	var sep string
	for _, v := range fT {
		fmt.Fprintf(&buf, "%s%s", sep, v.describe(mc))
		sep = ", "
	}
	buf.WriteString("]")
	return buf.String()
}

type typedTupleType struct { // We don't know how long it is but we know what its elements are. (or we can say 'single?' if we don't.)
	t alternateType
}

func (t typedTupleType) compare(u typeScheme) int {
	switch u := u.(type) {
	case simpleType, finiteTupleType:
		return 1
	case typedTupleType:
		return t.t.compare(u.t)
	default:
		return -1
	}
}

func (aT typedTupleType) describe(mc *vm.Vm) string {
	var buf strings.Builder
	buf.WriteString("tuple[")
	buf.WriteString(aT.t.describe(mc))
	buf.WriteString("... ]")
	return buf.String()
}

type blingType struct {
	tag string
}

func (t blingType) compare(u typeScheme) int {
	switch u := u.(type) {
	case simpleType, finiteTupleType, typedTupleType:
		return 1
	case blingType:
		if t.tag < u.tag {
			return -1
		}
		if t.tag == u.tag {
			return 0
		}
		return 1
	default:
		return -1
	}
}

func (bT blingType) describe(mc *vm.Vm) string {
	return bT.tag
}

// type listType []typeScheme

// func (t listType) compare(u typeScheme) int {
// 	switch u := u.(type) {
// 	case simpleType, finiteTupleType, typedTupleType, blingType:
// 		return 1
// 	case listType:
// 		diff := len(t) - len(u)
// 		if diff != 0 {
// 			return diff
// 		}
// 		for i := 0; i < len(t); i++ {
// 			diff := (t)[i].compare((u)[i])
// 			if diff != 0 {
// 				return diff
// 			}
// 		}
// 		return 0
// 	default:
// 		return -1
// 	}
// }

func altType(t ...values.ValueType) alternateType {
	result := make(alternateType, len(t))
	for i, v := range t {
		result[i] = simpleType(v)
	}
	return result
}

func tp(t values.ValueType) simpleType {
	return simpleType(t)
}
