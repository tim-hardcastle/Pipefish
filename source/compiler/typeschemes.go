package compiler

import (
	"fmt"
	"reflect"
	"strings"

	"github.com/tim-hardcastle/Pipefish/source/dtypes"
	"github.com/tim-hardcastle/Pipefish/source/values"
	"github.com/tim-hardcastle/Pipefish/source/vm"
)

// The Pipefish compiler has a rather richer view of the type system than the Pipefish language and its
// runtime, in that the compiler can to some extent keep track of the arity of tuples and/or the types of
// their individual elements. We refer to this information as a "typescheme" to distinguish it from the
// types of the languge itself. In this file we declare the assorted typescheme structs and their
// supporting methods.

// Typeschemes implement the `TypeScheme` interface. Those typeschemes that contain other typeschemes can
// be ordered to speed things up, hence the `compare` method in the interface.

type TypeScheme interface {
	compare(u TypeScheme) int
	describe(mc *vm.Vm) string
	IsPrivate(mc *vm.Vm) bool
}

// What every compiler starts off with. The `AlternateType` typescheme is the cannonical form for storing
// them: it expresses a range of potential types which can of course be a singleton.
var INITIAL_TYPE_SCHEMES = map[string]AlternateType{
	"ok":       AltType(values.SUCCESSFUL_VALUE),
	"int":      AltType(values.INT),
	"string":   AltType(values.STRING),
	"rune":     AltType(values.RUNE),
	"bool":     AltType(values.BOOL),
	"float":    AltType(values.FLOAT),
	"error":    AltType(values.ERROR),
	"type":     AltType(values.TYPE),
	"pair":     AltType(values.PAIR),
	"list":     AltType(values.LIST),
	"map":      AltType(values.MAP),
	"set":      AltType(values.SET),
	"label":    AltType(values.LABEL),
	"snippet":  AltType(values.SNIPPET),
	"func":     AltType(values.FUNC),
	"int?":     AltType(values.NULL, values.INT),
	"string?":  AltType(values.NULL, values.STRING),
	"bool?":    AltType(values.NULL, values.BOOL),
	"float64?": AltType(values.NULL, values.FLOAT),
	"type?":    AltType(values.NULL, values.TYPE),
	"pair?":    AltType(values.NULL, values.PAIR),
	"list?":    AltType(values.NULL, values.LIST),
	"map?":     AltType(values.NULL, values.MAP),
	"set?":     AltType(values.NULL, values.SET),
	"label?":   AltType(values.NULL, values.LABEL),
	"func?":    AltType(values.NULL, values.FUNC),
	"snippet?": AltType(values.NULL, values.SNIPPET),
	"null":     AltType(values.NULL),
}

// Finds all the possible lengths of tuples in a TypeScheme. (Single values have length 1. Non-finite tuples have length -1.)
// This allows us to figure out if we need to generate a check on the length of a tuple or whether we can take it for granted
// at compile time.
func lengths(t TypeScheme) dtypes.Set[int] {
	result := make(dtypes.Set[int])
	switch t := t.(type) {
	case SimpleType:
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
	case FiniteTupleType:
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

func Equals(t, u TypeScheme) bool {
	if reflect.TypeOf(t) != reflect.TypeOf(u) {
		return false
	}
	switch t := t.(type) {
	case SimpleType:
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
	case FiniteTupleType:
		if len(t) != len(u.(FiniteTupleType)) {
			return false
		}
		for i, w := range t {
			if !Equals(w, u.(FiniteTupleType)[i]) {
				return false
			}
		}
		return true
	}
	panic("We shouldn't be here!")
}

func recursivelyContains(t TypeScheme, u SimpleType) bool {
	switch t := t.(type) {
	case SimpleType:
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
	case FiniteTupleType:
		for _, w := range t {
			if recursivelyContains(w, u) {
				return true
			}
		}
		return false
	}
	panic("We shouldn't be here!")
}

// This finds all the possible ix-th elements in a TypeScheme.
func typesAtIndex(t TypeScheme, ix int) AlternateType {
	result, _ := recursiveTypesAtIndex(t, ix)
	return result
}

// TODO: This is somewhat wasteful because there ought to be some sensible way to fix it so the algorithm uses data from computing this for
// i - 1. But let's get the VM working first and optimise the compiler later.
func recursiveTypesAtIndex(t TypeScheme, ix int) (AlternateType, dtypes.Set[int]) {
	resultTypes := AlternateType{}
	resultSet := make(dtypes.Set[int])
	switch t := t.(type) {
	case SimpleType:
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
	case FiniteTupleType:
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

type SimpleType uint32

func (t SimpleType) compare(u TypeScheme) int {
	switch u := u.(type) {
	case SimpleType:
		return int(t) - int(u)
	default:
		return -1
	}
}

func (t SimpleType) describe(mc *vm.Vm) string {
	return mc.DescribeType(values.ValueType(t), vm.LITERAL)
}

func (t SimpleType) IsPrivate(mc *vm.Vm) bool {
	return mc.ConcreteTypeInfo[t].IsPrivate()
}

type AlternateType []TypeScheme

func AbstractTypeToAlternateType(abType values.AbstractType) AlternateType {
	result := AlternateType{}
	for _, ty := range abType.Types {
		result = append(result, SimpleType(ty))
	}
	return result
}

// This assumes that the alternateType came from the typeNameToTypeList array and therefore contains only elements of
// type SimpleType. TODO: this probably shouldn't exist. Truth is meant to flow from abstract types to alternate types.
func (aT AlternateType) ToAbstractType() values.AbstractType {
	result := make([]values.ValueType, 0, len(aT))
	for _, v := range aT {
		result = append(result, values.ValueType(v.(SimpleType)))
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

func (aT AlternateType) describe(mc *vm.Vm) string {
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

func (vL AlternateType) without(t TypeScheme) AlternateType {
	x := AlternateType{}
	for _, v := range vL {
		if v.compare(t) != 0 {
			x = append(x, v)
		}
	}
	return x
}

func (aT AlternateType) isOnly(vt values.ValueType) bool {
	t := SimpleType(vt)
	if len(aT) == 1 {
		switch el := aT[0].(type) {
		case SimpleType:
			return el == t
		default:
			return false
		}
	}
	return false
}

func (aT AlternateType) isOnlyCloneOf(mc *vm.Vm, vts ...values.ValueType) bool {
	types := dtypes.MakeFromSlice(vts)
	for _, el := range aT {
		switch el := el.(type) {
		case SimpleType:
			typeNumber := values.ValueType(el)
			switch typeInfo := mc.ConcreteTypeInfo[typeNumber].(type) {
			case vm.BuiltinType :
				if !types.Contains(typeNumber) {
					return false
				}
			case vm.CloneType :
				if !types.Contains(typeInfo.Parent) {
					return false
				}
			default :
				return false
			}
			
		default:
			return false
		}
	}
	return true
}

func (aT AlternateType) cannotBeACloneOf(mc *vm.Vm, vts ...values.ValueType) bool {
	singles, _ := aT.splitSinglesAndTuples()
	types := dtypes.MakeFromSlice(vts)
	for _, el := range singles {
		switch el := el.(type) {
		case SimpleType:
			typeNumber := values.ValueType(el)
			switch typeInfo := mc.ConcreteTypeInfo[typeNumber].(type) {
			case vm.BuiltinType :
				if types.Contains(typeNumber) {
					return false
				}
			case vm.CloneType :
				if types.Contains(typeInfo.Parent) {
					return false
				}
			}
		}
	}
	return true
	
}

func (aT AlternateType) IsPrivate(mc *vm.Vm) bool {
	for _, el := range aT {
		if el.IsPrivate(mc) {
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
		case SimpleType:
			singles = append(singles, t)
		case AlternateType:
			recSng, recTup := t.splitSinglesAndTuples()
			singles = append(singles, recSng...)
			tuples = append(tuples, recTup...)
		default:
			tuples = append(tuples, t)
		}
	}
	return singles, tuples
}

func (aT AlternateType) hasSideEffects() bool {
	for _, u := range aT {
		switch u := u.(type) {
		case SimpleType:
			if u == SimpleType(values.SUCCESSFUL_VALUE) {
				return true
			}
		}
	}
	return false
}

func (aT AlternateType) IsLegalCmdReturn() bool {
	for _, u := range aT {
		switch u := u.(type) {
		case SimpleType:
			if u != SimpleType(values.SUCCESSFUL_VALUE) && u != SimpleType(values.ERROR) &&
				u != SimpleType(values.COMPILE_TIME_ERROR) && u != SimpleType(values.UNSATISFIED_CONDITIONAL) {
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
		case SimpleType:
			if u == SimpleType(values.SUCCESSFUL_VALUE) {
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
	t := SimpleType(vt)
	if vt == values.TUPLE { // Special-case tuples.
		for _, ty := range aT {
			switch ty.(type) {
			case FiniteTupleType:
				return true
			case TypedTupleType:
				return true
			}
		}
		return false
	}
	for _, ty := range aT {
		switch el := ty.(type) {
		case SimpleType:
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
		case SimpleType:
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

func (aT AlternateType) IsNoneOf(vts ...values.ValueType) bool {
	for _, vt := range vts {
		if aT.Contains(vt) {
			return false
		}
	}
	return true
}

func (t AlternateType) compare(u TypeScheme) int {
	switch u := u.(type) {
	case SimpleType:
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

type FiniteTupleType []TypeScheme // "Finite" meaning that we know its size at compile time.

func (t FiniteTupleType) compare(u TypeScheme) int {
	switch u := u.(type) {
	case SimpleType, AlternateType:
		return 1
	case FiniteTupleType:
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

func (fT FiniteTupleType) describe(mc *vm.Vm) string {
	var buf strings.Builder
	lastWasBling := true // Which is a lie, but stops us from putting a comma right at the start.
	for i, v := range fT {
		_, thisIsBling := v.(blingType)
		if !(lastWasBling || thisIsBling) {
			fmt.Fprintf(&buf, ",")
		}
		if i > 0 {
			fmt.Fprintf(&buf, " ")
		}
		fmt.Fprintf(&buf, v.describe(mc))
		lastWasBling = thisIsBling
	}
	return buf.String()
}

// The 'infix' field will usually be "" but if it is populated then it will single that piece of bling out for
// special treatment.
// The sole use of this is to produce good error essages from bad function calls.
func (fT FiniteTupleType) describeWithPotentialInfix(mc *vm.Vm, infix string) string {
	var buf strings.Builder
	fmt.Fprintf(&buf, "'")
	specialBlingHasHappened := false
	specialBlingJustHappened := false
	lastWasBling := true // Which is a lie, but stops us from putting a comma right at the start.
	for i, u := range fT {
		v := u.(AlternateType) // This is horrible, but because of the way we got this we know it's true.
		var bling blingType
		var thisIsBling bool
		if len(v) == 1 {
			bling, thisIsBling = v[0].(blingType)
		}
		if !(lastWasBling || thisIsBling) {
			fmt.Fprintf(&buf, ",")
		}
		if thisIsBling && bling.tag == infix {
			specialBlingHasHappened = true
			fmt.Fprintf(&buf, "' on the left of it and '") // The ' characters will sneakily interact with emph in the errorfile.
		} else {
			if i > 0 && !specialBlingJustHappened {
				fmt.Fprintf(&buf, " ")
			}
			fmt.Fprintf(&buf, v.describe(mc))
		}
		lastWasBling = thisIsBling
		specialBlingJustHappened = thisIsBling && bling.tag == infix
	}
	fmt.Fprintf(&buf, "'")
	if specialBlingHasHappened {
		fmt.Fprintf(&buf, " on the right")
	}
	return buf.String()
}

func (fT FiniteTupleType) IsPrivate(mc *vm.Vm) bool {
	for _, el := range fT {
		if el.IsPrivate(mc) {
			return true
		}
	}
	return false
}

type TypedTupleType struct { // We don't know how long it is but we know what its elements are (or we can say 'any?' if we don't.)
	T AlternateType
}

func (t TypedTupleType) compare(u TypeScheme) int {
	switch u := u.(type) {
	case SimpleType, FiniteTupleType:
		return 1
	case TypedTupleType:
		return t.T.compare(u.T)
	default:
		return -1
	}
}

func (tT TypedTupleType) describe(mc *vm.Vm) string {
	var buf strings.Builder
	buf.WriteString(tT.T.describe(mc))
	buf.WriteString("... ")
	return buf.String()
}

func (tT TypedTupleType) IsPrivate(mc *vm.Vm) bool {
	for _, el := range tT.T {
		if el.IsPrivate(mc) {
			return true
		}
	}
	return false
}

type blingType struct {
	tag string
}

func (t blingType) compare(u TypeScheme) int {
	switch u := u.(type) {
	case SimpleType, FiniteTupleType, TypedTupleType:
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

func (bT blingType) IsPrivate(mc *vm.Vm) bool {
	return false // TODO --- not covered this yet.
}

func AltType(t ...values.ValueType) AlternateType {
	result := make(AlternateType, len(t))
	for i, v := range t {
		result[i] = SimpleType(v)
	}
	return result
}

func tp(t values.ValueType) SimpleType {
	return SimpleType(t)
}

func getAllTypes(ts TypeScheme) AlternateType {
	result := AlternateType{}
	switch ts := ts.(type) {
	case AlternateType:
		for _, v := range ts {
			result = result.Union(getAllTypes(v))
		}
	case TypedTupleType:
		result = ts.T
	case FiniteTupleType:
		for _, v := range ts {
			result = result.Union(getAllTypes(v))
		}
	case SimpleType:
		result = AlternateType{ts}
	default:
		panic("We shouldn't be here!")
	}
	return result
}

func (ts AlternateType) reduce() TypeScheme { // Turns alternative types with only on option into their contents.
	if len(ts) == 1 {
		return ts[0]
	}
	return ts
}

func (t AlternateType) mustBeSingleOrTuple() (bool, bool) {
	s, T := true, true
	for _, v := range t {
		switch v.(type) {
		case SimpleType:
			T = false
		default:
			s = false
		}
	}
	return s, T
}
