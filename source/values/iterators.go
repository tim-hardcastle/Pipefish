package values

import (
	"src.elv.sh/pkg/persistent/vector"
)

type Iterator interface {
	Unfinished() bool
	GetKey() Value
	GetValue() Value
	GetKeyValuePair() (Value, Value)
	Reset()
}

type DecIterator struct { // For an 'x::y' range, going down.
	StartVal int
	Val      int
	MinVal   int
	pos      int
}

func (it *DecIterator) Unfinished() bool {
	return it.Val >= it.MinVal
}

func (it *DecIterator) GetKey() Value {
	keyResult := Value{INT, it.pos}
	it.pos++
	it.Val--
	return keyResult
}

func (it *DecIterator) GetValue() Value {
	valResult := Value{INT, it.Val}
	it.pos++
	it.Val--
	return valResult
}

func (it *DecIterator) GetKeyValuePair() (Value, Value) {
	keyResult := Value{INT, it.pos}
	valResult := Value{INT, it.Val}
	it.pos++
	it.Val--
	return keyResult, valResult
}

func (it *DecIterator) Reset() {
	it.pos = 0
	it.Val = it.StartVal
}

type EnumIterator struct { // For an 'x::y' range, going down.
	Type ValueType
	Max  int
	pos  int
}

func (it *EnumIterator) Unfinished() bool {
	return it.pos < it.Max
}

func (it *EnumIterator) GetKey() Value {
	keyResult := Value{INT, it.pos}
	it.pos++
	return keyResult
}

func (it *EnumIterator) GetValue() Value {
	valResult := Value{it.Type, it.pos}
	it.pos++
	return valResult
}

func (it *EnumIterator) GetKeyValuePair() (Value, Value) {
	keyResult := Value{INT, it.pos}
	valResult := Value{it.Type, it.pos}
	it.pos++
	return keyResult, valResult
}

func (it *EnumIterator) Reset() {
	it.pos = 0
}

type IncIterator struct { // For an 'x::y' range, going up.
	StartVal int
	Val      int
	MaxVal   int
	pos      int
}

func (it *IncIterator) Unfinished() bool {
	return it.Val < it.MaxVal
}

func (it *IncIterator) GetKey() Value {
	keyResult := Value{INT, it.pos}
	it.pos++
	it.Val++
	return keyResult
}

func (it *IncIterator) GetValue() Value {
	valResult := Value{INT, it.Val}
	it.pos++
	it.Val++
	return valResult
}

func (it *IncIterator) GetKeyValuePair() (Value, Value) {
	keyResult := Value{INT, it.pos}
	valResult := Value{INT, it.Val}
	it.pos++
	it.Val++
	return keyResult, valResult
}

func (it *IncIterator) Reset() {
	it.pos = 0
	it.Val = it.StartVal
}

// This is for the case when we ask to range over the key only of something which has an integer key.
type KeyIncIterator struct {
	Max int
	pos int
}

func (it *KeyIncIterator) Unfinished() bool {
	return it.pos < it.Max
}

func (it *KeyIncIterator) GetKey() Value {
	keyResult := Value{INT, it.pos}
	it.pos++
	return keyResult
}

func (it *KeyIncIterator) GetValue() Value {
	panic("KeyIncIterator returns only keys.")
}

func (it *KeyIncIterator) GetKeyValuePair() (Value, Value) {
	panic("KeyIncIterator returns only keys.")
}

func (it *KeyIncIterator) Reset() {
	it.pos = 0
}

type ListIterator struct {
	VecIt vector.Iterator
	pos   int
}

func (it *ListIterator) Unfinished() bool {
	return it.VecIt.HasElem()
}

func (it *ListIterator) GetKey() Value {
	keyResult := Value{INT, it.pos}
	it.pos++
	it.VecIt.Next()
	return keyResult
}

func (it *ListIterator) GetValue() Value {
	valResult := it.VecIt.Elem().(Value)
	it.pos++
	it.VecIt.Next()
	return valResult
}

func (it *ListIterator) GetKeyValuePair() (Value, Value) {
	keyResult := Value{INT, it.pos}
	valResult := it.VecIt.Elem().(Value)
	it.pos++
	it.VecIt.Next()
	return keyResult, valResult
}

func (it *ListIterator) Reset() {
	it.pos = 0
}

type MapIterator struct { // TODO --- write actual iterator forr Map for this to wrap around.
	KVPairs []MapPair
	Len     int
	pos     int
}

func (it *MapIterator) Unfinished() bool {
	return it.pos < it.Len
}

func (it *MapIterator) GetKey() Value {
	keyResult := it.KVPairs[it.pos].Key
	it.pos++
	return keyResult
}

func (it *MapIterator) GetValue() Value {
	valResult := it.KVPairs[it.pos].Val
	it.pos++
	return valResult
}

func (it *MapIterator) GetKeyValuePair() (Value, Value) {
	keyResult := it.KVPairs[it.pos].Key
	valResult := it.KVPairs[it.pos].Val
	it.pos++
	return keyResult, valResult
}

func (it *MapIterator) Reset() {
	it.pos = 0
}

type SetIterator struct { // TODO --- write actual iterator for Set for this to wrap around.
	Elements []Value
	Len      int
	pos      int
}

func (it *SetIterator) Unfinished() bool {
	return it.pos < it.Len
}

func (it *SetIterator) GetKey() Value {
	keyResult := it.Elements[it.pos]
	it.pos++
	return keyResult
}

func (it *SetIterator) GetValue() Value {
	valResult := it.Elements[it.pos]
	it.pos++
	return valResult
}

func (it *SetIterator) GetKeyValuePair() (Value, Value) {
	keyResult := it.Elements[it.pos]
	valResult := it.Elements[it.pos]
	it.pos++
	return keyResult, valResult
}

func (it *SetIterator) Reset() {
	it.pos = 0
}

type StringIterator struct {
	Str string
	pos int
}

func (it *StringIterator) Unfinished() bool {
	return it.pos < len(it.Str)
}

func (it *StringIterator) GetKey() Value {
	keyResult := Value{INT, it.pos}
	it.pos++
	return keyResult
}

func (it *StringIterator) GetValue() Value {
	valResult := Value{RUNE, int32(it.Str[it.pos])}
	it.pos++
	return valResult
}

func (it *StringIterator) GetKeyValuePair() (Value, Value) {
	keyResult := Value{INT, it.pos}
	valResult := Value{RUNE, int32(it.Str[it.pos])}
	it.pos++
	return keyResult, valResult
}

func (it *StringIterator) Reset() {
	it.pos = 0
}

type TupleIterator struct { // TODO --- write actual iterator for Map for this to wrap around.
	Elements []Value
	Len      int
	pos      int
}

func (it *TupleIterator) Unfinished() bool {
	return it.pos < it.Len
}

func (it *TupleIterator) GetKey() Value {
	keyResult := Value{INT, it.pos}
	it.pos++
	return keyResult
}

func (it *TupleIterator) GetValue() Value {
	valResult := it.Elements[it.pos]
	it.pos++
	return valResult
}

func (it *TupleIterator) GetKeyValuePair() (Value, Value) {
	keyResult := Value{INT, it.pos}
	valResult := it.Elements[it.pos]
	it.pos++
	return keyResult, valResult
}

func (it *TupleIterator) Reset() {
	it.pos = 0
}
