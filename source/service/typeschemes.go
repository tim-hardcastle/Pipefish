package service

import (
	"fmt"
	"pipefish/source/dtypes"
	"pipefish/source/values"
	"reflect"
	"strings"
)

type typeScheme interface {
	compare(u typeScheme) int
	describe(mc *Vm) string
	isPrivate(mc *Vm) bool
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
	case TypedTupleType:
		result.Add(-1)
		return result
	case AlternateType:
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

func Equals(t, u typeScheme) bool {
	if reflect.TypeOf(t) != reflect.TypeOf(u) {
		return false
	}
	switch t := t.(type) {
	case simpleType:
		return t == u
	case TypedTupleType:
		if len(t.T) != len(u.(TypedTupleType).T) {
			return false
		}
		for i, w := range t.T {
			if !Equals(w, u.(TypedTupleType).T[i]) {
				return false
			}
		}
		return true
	case AlternateType:
		if len(t) != len(u.(AlternateType)) {
			return false
		}
		for i, w := range t {
			if !Equals(w, u.(AlternateType)[i]) {
				return false
			}
		}
		return true
	case finiteTupleType:
		if len(t) != len(u.(finiteTupleType)) {
			return false
		}
		for i, w := range t {
			if !Equals(w, u.(finiteTupleType)[i]) {
				return false
			}
		}
		return true
	}
	panic("We shouldn't be here!")
}

func recursivelyContains(t typeScheme, u simpleType) bool {
	switch t := t.(type) {
	case simpleType:
		return t == u
	case TypedTupleType:
		for _, w := range t.T {
			if recursivelyContains(w, u) {
				return true
			}
		}
		return false
	case AlternateType:
		for _, w := range t {
			if recursivelyContains(w, u) {
				return true
			}
		}
		return false
	case finiteTupleType:
		for _, w := range t {
			if recursivelyContains(w, u) {
				return true
			}
		}
		return false
	}
	panic("We shouldn't be here!")
}

// This finds all the possible ix-th elements in a typeScheme.
func typesAtIndex(t typeScheme, ix int) AlternateType {
	result, _ := recursiveTypesAtIndex(t, ix)
	return result
}

// TODO: This is somewhat wasteful because there ought to be some sensible way to fix it so the algorithm uses data from computing this for
// i - 1. But let's get the VM working first and optimise the compiler later.
func recursiveTypesAtIndex(t typeScheme, ix int) (AlternateType, dtypes.Set[int]) {
	resultTypes := AlternateType{}
	resultSet := make(dtypes.Set[int])
	switch t := t.(type) {
	case simpleType:
		if ix == 0 {
			resultTypes = resultTypes.Union(AlternateType{t})
		}
		resultSet.Add(1)
		return resultTypes, resultSet
	case TypedTupleType:
		resultTypes = resultTypes.Union(t.T)
		resultSet.Add(ix)
		return resultTypes, resultSet
	case AlternateType:
		for _, v := range t {
			newTypes, newSet := recursiveTypesAtIndex(v, ix)
			resultTypes = resultTypes.Union(newTypes)
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
			resultTypes = resultTypes.Union(newTypes)
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

func (t simpleType) describe(mc *Vm) string {
	return mc.DescribeType(values.ValueType(t))
}

func (t simpleType) isPrivate(mc *Vm) bool {
	return mc.concreteTypes[t].isPrivate()
}

type AlternateType []typeScheme

// This assumes that the alternateType came from the typeNameToTypeList array and therefore contains only elements of
// type simpleType.
func (aT AlternateType) ToAbstractType() values.AbstractType {
	result := make([]values.ValueType, 0, len(aT))
	for _, v := range aT {
		result = append(result, values.ValueType(v.(simpleType)))
	}
	return values.AbstractType{result, 0}
}

func (vL AlternateType) intersect(wL AlternateType) AlternateType {
	x := AlternateType{}
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

func (aT AlternateType) describe(mc *Vm) string {
	var buf strings.Builder
	var sep string
	for _, v := range aT {
		fmt.Fprintf(&buf, "%s%s", sep, v.describe(mc))
		sep = "/"
	}
	return buf.String()
}

func (vL AlternateType) Union(wL AlternateType) AlternateType {
	x := AlternateType{}
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

func (vL AlternateType) without(t typeScheme) AlternateType {
	x := AlternateType{}
	for _, v := range vL {
		if v.compare(t) != 0 {
			x = append(x, v)
		}
	}
	return x
}

func (aT AlternateType) isOnly(vt values.ValueType) bool {
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

func (aT AlternateType) isPrivate(mc *Vm) bool {
	for _, el := range aT {
		if el.isPrivate(mc) {
			return true
		}
	}
	return false
}

func (aT AlternateType) splitSinglesAndTuples() (AlternateType, AlternateType) {
	singles := AlternateType{}
	tuples := AlternateType{}
	for _, t := range aT {
		switch t := t.(type) {
		case simpleType:
			singles = append(singles, t)
		case AlternateType:
			recSng, recTup := t.splitSinglesAndTuples()
			singles = append(singles, recSng)
			tuples = append(tuples, recTup)
		default:
			tuples = append(tuples, t)
		}
	}
	return singles, tuples
}

func (aT AlternateType) hasSideEffects() bool {
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

func (aT AlternateType) IsLegalCmdReturn() bool {
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

func (aT AlternateType) isLegalReturnFromLoopBody() bool {
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

func (aT AlternateType) IsLegalDefReturn() bool {
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

func (aT AlternateType) containsAnyOf(vts ...values.ValueType) bool {
	for _, vt := range vts {
		if aT.Contains(vt) {
			return true
		}
	}
	return false
}

func (aT AlternateType) Contains(vt values.ValueType) bool { // TODO --- make recursive on typedTupleTyp etc or there will be corner cases.
	t := simpleType(vt)
	if vt == values.TUPLE { // Special-case tuples.
		for _, ty := range aT {
			switch ty.(type) {
			case finiteTupleType:
				return true
			case TypedTupleType:
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

func (aT AlternateType) containsOnlyTuples() bool {
	for _, ty := range aT {
		switch el := ty.(type) {
		case simpleType:
			return false
		case AlternateType:
			if !el.containsOnlyTuples() {
				return false
			}
		case blingType: //, listType
			return false
		}
	}
	return true
}

func (aT AlternateType) isNoneOf(vts ...values.ValueType) bool {
	for _, vt := range vts {
		if aT.Contains(vt) {
			return false
		}
	}
	return true
}

func (t AlternateType) compare(u typeScheme) int {
	switch u := u.(type) {
	case simpleType:
		return 1
	case AlternateType:
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
	case simpleType, AlternateType:
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

func (fT finiteTupleType) describe(mc *Vm) string {
	var buf strings.Builder
	var sep string
	for _, v := range fT {
		fmt.Fprintf(&buf, "%s%s", sep, v.describe(mc))
		sep = ", "
	}
	return buf.String()
}

func (fT finiteTupleType) isPrivate(mc *Vm) bool {
	for _, el := range fT {
		if el.isPrivate(mc) {
			return true
		}
	}
	return false
}

type TypedTupleType struct { // We don't know how long it is but we know what its elements are (or we can say 'single?' if we don't.)
	T AlternateType
}

func (t TypedTupleType) compare(u typeScheme) int {
	switch u := u.(type) {
	case simpleType, finiteTupleType:
		return 1
	case TypedTupleType:
		return t.T.compare(u.T)
	default:
		return -1
	}
}

func (tT TypedTupleType) describe(mc *Vm) string {
	var buf strings.Builder
	buf.WriteString(tT.T.describe(mc))
	buf.WriteString("... ")
	return buf.String()
}

func (tT TypedTupleType) isPrivate(mc *Vm) bool {
	for _, el := range tT.T {
		if el.isPrivate(mc) {
			return true
		}
	}
	return false
}

type blingType struct {
	tag string
}

func (t blingType) compare(u typeScheme) int {
	switch u := u.(type) {
	case simpleType, finiteTupleType, TypedTupleType:
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

func (bT blingType) describe(mc *Vm) string {
	return bT.tag
}

func (bT blingType) isPrivate(mc *Vm) bool {
	return false // TODO --- not covered this yet.
}

func AltType(t ...values.ValueType) AlternateType {
	result := make(AlternateType, len(t))
	for i, v := range t {
		result[i] = simpleType(v)
	}
	return result
}

func tp(t values.ValueType) simpleType {
	return simpleType(t)
}
