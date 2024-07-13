package values

import (
	"src.elv.sh/pkg/persistent/vector"
)

type Iterator interface {
	Unfinished() bool
	NextKey() Value
	NextValue() Value
	NextKeyValuePair() (Value, Value)
}

type DecIterator struct { // For an 'x::y' range, going down.
	Min int
	Pos int
	val int
}

func (it *DecIterator) Unfinished() bool {
	return it.val >= it.Min
}

func (it *DecIterator) NextKey() Value {
	keyResult := Value{INT, it.Pos}
	it.Pos++
	it.val--
	return keyResult
}

func (it *DecIterator) NextValue() Value {
	valResult := Value{INT, it.val}
	it.Pos++
	it.val--
	return valResult
}

func (it *DecIterator) NextKeyValuePair() (Value, Value) {
	keyResult := Value{INT, it.Pos}
	valResult := Value{INT, it.val}
	it.Pos++
	it.val--
	return keyResult, valResult
}

type IncIterator struct { // For an 'x::y' range, going up.
	Max int
	Pos int
	val int
}

func (it *IncIterator) Unfinished() bool {
	return it.Pos < it.Max
}

func (it *IncIterator) NextKey() Value {
	keyResult := Value{INT, it.Pos}
	it.Pos++
	it.val++
	return keyResult
}

func (it *IncIterator) NextValue() Value {
	valResult := Value{INT, it.val}
	it.Pos++
	it.val++
	return valResult
}

func (it *IncIterator) NextKeyValuePair() (Value, Value) {
	keyResult := Value{INT, it.Pos}
	valResult := Value{INT, it.val}
	it.Pos++
	it.val++
	return keyResult, valResult
}

// This is for the case when we ask to range over the key only of something which has an integer key.
type KeyIncIterator struct {
	Max int
	pos int
}

func (it *KeyIncIterator) Unfinished() bool {
	return it.pos < it.Max
}

func (it *KeyIncIterator) NextKey() Value {
	keyResult := Value{INT, it.pos}
	it.pos++
	return keyResult
}

func (it *KeyIncIterator) NextValue() Value {
	panic("KeyIncIterator returns only keys.")
}

func (it *KeyIncIterator) NextKeyValuePair() (Value, Value) {
	panic("KeyIncIterator returns only keys.")
}

type ListIterator struct {
	VecIt vector.Iterator
	pos   int
}

func (it *ListIterator) Unfinished() bool {
	return it.VecIt.HasElem()
}

func (it *ListIterator) NextKey() Value {
	keyResult := Value{INT, it.pos}
	it.pos++
	it.VecIt.Next()
	return keyResult
}

func (it *ListIterator) NextValue() Value {
	valResult := it.VecIt.Elem().(Value)
	it.pos++
	it.VecIt.Next()
	return valResult
}

func (it *ListIterator) NextKeyValuePair() (Value, Value) {
	keyResult := Value{INT, it.pos}
	valResult := it.VecIt.Elem().(Value)
	it.pos++
	it.VecIt.Next()
	return keyResult, valResult
}

type StringIterator struct {
	Str string
	pos int
}

func (it *StringIterator) Unfinished() bool {
	return it.pos < len(it.Str)
}

func (it *StringIterator) NextKey() Value {
	keyResult := Value{INT, it.pos}
	it.pos++
	return keyResult
}

func (it *StringIterator) NextValue() Value {
	valResult := Value{RUNE, it.Str[it.pos]}
	it.pos++
	return valResult
}

func (it *StringIterator) NextKeyValuePair() (Value, Value) {
	keyResult := Value{INT, it.pos}
	valResult := Value{RUNE, it.Str[it.pos]}
	it.pos++
	return keyResult, valResult
}
