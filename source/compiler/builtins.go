package compiler

import (
	"pipefish/source/token"
	"pipefish/source/values"
	"pipefish/source/vm"
)

type functionAndReturnType struct {
	f func(cp *Compiler, mc *vm.Vm, tok *token.Token, dest uint32, args []uint32)
	t alternateType
}

var BUILTINS = map[string]functionAndReturnType{
	"add_floats":        {(*Compiler).btAddFloats, altType(values.FLOAT)},
	"add_integers":      {(*Compiler).btAddIntegers, altType(values.INT)},
	"add_lists":         {(*Compiler).btAddLists, altType(values.LIST)},
	"add_sets":          {(*Compiler).btAddSets, altType(values.SET)},
	"add_strings":       {(*Compiler).btAddStrings, altType(values.STRING)},
	"divide_floats":     {(*Compiler).btDivideFloats, altType(values.ERROR, values.FLOAT)},
	"divide_integers":   {(*Compiler).btDivideIntegers, altType(values.ERROR, values.INT)},
	"float_of_int":      {(*Compiler).btFloatOfInt, altType(values.FLOAT)},
	"float_of_string":   {(*Compiler).btFloatOfString, altType(values.ERROR, values.FLOAT)},
	"gt_floats":         {(*Compiler).btGtFloats, altType(values.BOOL)},
	"gte_floats":        {(*Compiler).btGteFloats, altType(values.BOOL)},
	"gt_ints":           {(*Compiler).btGtInts, altType(values.BOOL)},
	"gte_ints":          {(*Compiler).btGteInts, altType(values.BOOL)},
	"identity":          {(*Compiler).btIdentity, altType(values.TUPLE)},
	"int_of_float":      {(*Compiler).btIntOfFloat, altType(values.INT)},
	"int_of_string":     {(*Compiler).btIntOfString, altType(values.ERROR, values.INT)},
	"keys_of_map":       {(*Compiler).btKeysOfMap, altType(values.LIST)},
	"keys_of_struct":    {(*Compiler).btKeysOfStruct, altType(values.LIST)},
	"len_list":          {(*Compiler).btLenList, altType(values.INT)},
	"len_map":           {(*Compiler).btLenMap, altType(values.INT)},
	"len_set":           {(*Compiler).btLenSet, altType(values.INT)},
	"len_string":        {(*Compiler).btLenString, altType(values.INT)},
	"len_tuple":         {(*Compiler).btLenTuple, altType(values.INT)},
	"literal":           {(*Compiler).btLiteral, altType(values.STRING)},
	"lt_floats":         {(*Compiler).btLtFloats, altType(values.BOOL)},
	"lte_floats":        {(*Compiler).btLteFloats, altType(values.BOOL)},
	"lt_ints":           {(*Compiler).btLtInts, altType(values.BOOL)},
	"lte_ints":          {(*Compiler).btLteInts, altType(values.BOOL)},
	"make_error":        {(*Compiler).btMakeError, altType(values.ERROR)},
	"make_map":          {(*Compiler).btMakeMap, altType(values.MAP)},
	"make_pair":         {(*Compiler).btMakePair, altType(values.PAIR)},
	"make_set":          {(*Compiler).btMakeSet, altType(values.SET)},
	"modulo_integers":   {(*Compiler).btModuloIntegers, altType(values.ERROR, values.INT)},
	"multiply_floats":   {(*Compiler).btMultiplyFloats, altType(values.FLOAT)},
	"multiply_integers": {(*Compiler).btMultiplyIntegers, altType(values.INT)},
	"negate_float":      {(*Compiler).btNegateFloat, altType(values.FLOAT)},
	"negate_integer":    {(*Compiler).btNegateInteger, altType(values.INT)},
	"string":            {(*Compiler).btString, altType(values.STRING)},
	"single_in_list":    {(*Compiler).btSingleInList, altType(values.BOOL)},
	"single_in_set":     {(*Compiler).btSingleInSet, altType(values.BOOL)},
	"single_in_tuple":   {(*Compiler).btSingleInTuple, altType(values.BOOL)},
	"single_in_type":    {(*Compiler).btSingleInType, altType(values.BOOL)},
	"subtract_floats":   {(*Compiler).btSubtractFloats, altType(values.FLOAT)},
	"subtract_integers": {(*Compiler).btSubtractIntegers, altType(values.INT)},
	"tuple_of_single?":  {(*Compiler).btTupleOfSingle, altType()}, // Since we can't know the typeschemes in advance, these are kludged in by the seekFunctionCall method.
	"tuple_of_tuple":    {(*Compiler).btTupleOfTuple, altType()},  //
	"tuplify_list":      {(*Compiler).btTuplifyList, altType()},   // We know that this should be alternateType{typedTupleType{single?}}, but we don't know what single? is yet.
	"type":              {(*Compiler).btType, altType(values.TYPE)},
	"type_of_tuple":     {(*Compiler).btTypeOfTuple, altType(values.TYPE)},
}

func (cp *Compiler) btAddFloats(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Addf, dest, args[0], args[2])
}

func (cp *Compiler) btAddIntegers(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Addi, dest, args[0], args[2])
}

func (cp *Compiler) btAddLists(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.AddL, dest, args[0], args[2])
}

func (cp *Compiler) btAddSets(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.AddS, dest, args[0], args[2])
}

func (cp *Compiler) btAddStrings(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Adds, dest, args[0], args[2])
}

func (cp *Compiler) btDivideFloats(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.reserve(mc, values.FLOAT, 0.0)
	cp.put(mc, vm.Equf, args[2], mc.That())
	cp.emit(mc, vm.Qtru, mc.That(), mc.CodeTop()+3)
	cp.reserveError(mc, "built/div/float", tok, []any{})
	cp.emit(mc, vm.Asgm, dest, mc.That())
	cp.emit(mc, vm.Jmp, mc.CodeTop()+2)
	cp.emit(mc, vm.Divf, dest, args[0], args[2])
}

func (cp *Compiler) btDivideIntegers(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.reserve(mc, values.INT, 0)
	cp.put(mc, vm.Equi, args[2], mc.That())
	cp.emit(mc, vm.Qtru, mc.That(), mc.CodeTop()+3)
	cp.reserveError(mc, "built/div/int", tok, []any{})
	cp.emit(mc, vm.Asgm, dest, mc.That())
	cp.emit(mc, vm.Jmp, mc.CodeTop()+2)
	cp.emit(mc, vm.Divi, dest, args[0], args[2])
}

func (cp *Compiler) btFloatOfInt(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Flti, dest, args[0])
}

func (cp *Compiler) btFloatOfString(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Flts, dest, args[0])
}

func (cp *Compiler) btGtFloats(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Gthf, dest, args[0], args[2])
}

func (cp *Compiler) btGteFloats(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Gtef, dest, args[0], args[2])
}

func (cp *Compiler) btGtInts(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Gthi, dest, args[0], args[2])
}

func (cp *Compiler) btGteInts(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Gtei, dest, args[0], args[2])
}

func (cp *Compiler) btIdentity(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Idfn, dest, args[0])
}

func (cp *Compiler) btIntOfFloat(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Intf, dest, args[0])
}

func (cp *Compiler) btIntOfString(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Ints, dest, args[0])
}

func (cp *Compiler) btKeysOfMap(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.KeyM, dest, args[0])
}

func (cp *Compiler) btKeysOfStruct(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.KeyZ, dest, args[0])
}

func (cp *Compiler) btLenList(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.LenL, dest, args[0])
}

func (cp *Compiler) btLenMap(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.LenM, dest, args[0])
}

func (cp *Compiler) btLenSet(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.LenS, dest, args[0])
}

func (cp *Compiler) btLenString(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Lens, dest, args[0])
}

func (cp *Compiler) btLenTuple(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.LenT, dest, args[0])
}

func (cp *Compiler) btLiteral(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Litx, dest, args[0])
}

func (cp *Compiler) btLtFloats(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Gthf, dest, args[2], args[0])
}

func (cp *Compiler) btLteFloats(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Gtef, dest, args[2], args[0])
}

func (cp *Compiler) btLtInts(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Gthi, dest, args[2], args[0])
}

func (cp *Compiler) btLteInts(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Gtei, dest, args[2], args[0])
}

func (cp *Compiler) btMakeError(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Mker, dest, args[0], cp.reserveToken(mc, tok))
}

func (cp *Compiler) btMakeMap(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.reserveError(mc, "built/map/pair", tok, []any{})
	cp.reserveError(mc, "built/map/type", tok, []any{})
	cp.emit(mc, vm.Mkmp, dest, args[0])
}

func (cp *Compiler) btMakePair(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Mkpr, dest, args[0], args[2])
}

func (cp *Compiler) btMakeSet(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.reserveError(mc, "built/set/type", tok, []any{})
	cp.emit(mc, vm.Mkst, dest, args[0])
}

func (cp *Compiler) btModuloIntegers(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.reserve(mc, values.INT, 0)
	cp.put(mc, vm.Equi, args[2], mc.That())
	cp.emit(mc, vm.Qtru, mc.That(), mc.CodeTop()+3)
	cp.reserveError(mc, "built/mod", tok, []any{})
	cp.emit(mc, vm.Asgm, dest, mc.That())
	cp.emit(mc, vm.Jmp, mc.CodeTop()+2)
	cp.emit(mc, vm.Modi, dest, args[0], args[2])
}

func (cp *Compiler) btMultiplyFloats(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Mulf, dest, args[0], args[2])
}

func (cp *Compiler) btMultiplyIntegers(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Muli, dest, args[0], args[2])
}

func (cp *Compiler) btNegateFloat(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Negf, dest, args[0])
}

func (cp *Compiler) btNegateInteger(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Negi, dest, args[0])
}

func (cp *Compiler) btSingleInList(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.InxL, dest, args[0], args[2])
}

func (cp *Compiler) btSingleInSet(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.InxS, dest, args[0], args[2])
}

func (cp *Compiler) btSingleInTuple(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.InxT, dest, args[0], args[1])
}

func (cp *Compiler) btSingleInType(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Inxt, dest, args[0], args[2])
}

func (cp *Compiler) btString(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Strx, dest, args[0])
}

func (cp *Compiler) btSubtractFloats(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Subf, dest, args[0], args[2])
}

func (cp *Compiler) btSubtractIntegers(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Subi, dest, args[0], args[2])
}

func (cp *Compiler) btType(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Typx, dest, args[0])
}

func (cp *Compiler) btTupleOfSingle(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) { // TODO --- do we need this or not?
	cp.emit(mc, vm.Cv1T, dest, args[0])
}

func (cp *Compiler) btTuplifyList(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.TupL, dest, args[0])
}

func (cp *Compiler) btTupleOfTuple(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	destWithArgs := append([]uint32{dest}, args...)
	cp.emit(mc, vm.CvTT, destWithArgs...)
}

func (cp *Compiler) btTypeOfTuple(mc *vm.Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, vm.Asgm, dest, cp.tupleType)
}
