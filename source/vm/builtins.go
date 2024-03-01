package vm

import (
	"pipefish/source/token"
	"pipefish/source/values"
)

type functionAndReturnType struct {
	f func(cp *Compiler, mc *Vm, tok *token.Token, dest uint32, args []uint32)
	t alternateType
}

var BUILTINS = map[string]functionAndReturnType{
	"add_floats":        {(*Compiler).btAddFloats, altType(values.FLOAT)},
	"add_integers":      {(*Compiler).btAddIntegers, altType(values.INT)},
	"add_strings":       {(*Compiler).btAddStrings, altType(values.STRING)},
	"divide_floats":     {(*Compiler).btDivideFloats, altType(values.ERROR, values.FLOAT)},
	"divide_integers":   {(*Compiler).btDivideIntegers, altType(values.ERROR, values.INT)},
	"float_of_int":      {(*Compiler).btFloatOfInt, altType(values.FLOAT)},
	"float_of_string":   {(*Compiler).btFloatOfString, altType(values.ERROR, values.FLOAT)},
	"gt_floats":         {(*Compiler).btGtFloats, altType(values.BOOL)},
	"gte_floats":        {(*Compiler).btGteFloats, altType(values.BOOL)},
	"gt_ints":           {(*Compiler).btGtInts, altType(values.BOOL)},
	"gte_ints":          {(*Compiler).btGteInts, altType(values.BOOL)},
	"int_of_float":      {(*Compiler).btIntOfFloat, altType(values.INT)},
	"int_of_string":     {(*Compiler).btIntOfString, altType(values.ERROR, values.INT)},
	"len_string":        {(*Compiler).btLenString, altType(values.INT)},
	"literal":           {(*Compiler).btLiteral, altType(values.STRING)},
	"lt_floats":         {(*Compiler).btLtFloats, altType(values.BOOL)},
	"lte_floats":        {(*Compiler).btLteFloats, altType(values.BOOL)},
	"lt_ints":           {(*Compiler).btLtInts, altType(values.BOOL)},
	"lte_ints":          {(*Compiler).btLteInts, altType(values.BOOL)},
	"make_error":        {(*Compiler).btMakeError, altType(values.ERROR)},
	"modulo_integers":   {(*Compiler).btModuloIntegers, altType(values.ERROR, values.INT)},
	"multiply_floats":   {(*Compiler).btMultiplyFloats, altType(values.FLOAT)},
	"multiply_integers": {(*Compiler).btMultiplyIntegers, altType(values.INT)},
	"negate_float":      {(*Compiler).btNegateFloat, altType(values.FLOAT)},
	"negate_integer":    {(*Compiler).btNegateInteger, altType(values.INT)},
	"string":            {(*Compiler).btString, altType(values.STRING)},
	"subtract_floats":   {(*Compiler).btSubtractFloats, altType(values.FLOAT)},
	"subtract_integers": {(*Compiler).btSubtractIntegers, altType(values.INT)},
	"tuple_of_single?":  {(*Compiler).btTupleOfSingle, alternateType{finiteTupleType{}}},
	"tuple_of_tuple":    {(*Compiler).btTupleOfTuple, alternateType{finiteTupleType{}}},
	"type":              {(*Compiler).btType, altType(values.TYPE)},
	"type_of_tuple":     {(*Compiler).btTypeOfTuple, altType(values.TYPE)},
}

func (cp *Compiler) btAddFloats(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Addf, dest, args[0], args[2])
}

func (cp *Compiler) btAddIntegers(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Addi, dest, args[0], args[2])
}

func (cp *Compiler) btAddStrings(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Adds, dest, args[0], args[2])
}

func (cp *Compiler) btDivideFloats(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.reserve(vm, values.FLOAT, 0.0)
	cp.put(vm, Equf, args[2], vm.that())
	cp.emit(vm, Qtru, vm.that(), vm.codeTop()+3)
	cp.reserveError(vm, "built/div/float", tok, []any{})
	cp.emit(vm, Asgm, dest, vm.that())
	cp.emit(vm, Jmp, vm.codeTop()+2)
	cp.emit(vm, Divf, dest, args[0], args[2])
}

func (cp *Compiler) btDivideIntegers(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.reserve(vm, values.INT, 0)
	cp.put(vm, Equi, args[2], vm.that())
	cp.emit(vm, Qtru, vm.that(), vm.codeTop()+3)
	cp.reserveError(vm, "built/div/int", tok, []any{})
	cp.emit(vm, Asgm, dest, vm.that())
	cp.emit(vm, Jmp, vm.codeTop()+2)
	cp.emit(vm, Divi, dest, args[0], args[2])
}

func (cp *Compiler) btFloatOfInt(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Flti, dest, args[0])
}

func (cp *Compiler) btFloatOfString(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Flts, dest, args[0])
}

func (cp *Compiler) btGtFloats(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Gthf, dest, args[0], args[2])
}

func (cp *Compiler) btGteFloats(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Gtef, dest, args[0], args[2])
}

func (cp *Compiler) btGtInts(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Gthi, dest, args[0], args[2])
}

func (cp *Compiler) btGteInts(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Gtei, dest, args[0], args[2])
}

func (cp *Compiler) btIntOfFloat(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Intf, dest, args[0])
}

func (cp *Compiler) btIntOfString(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Ints, dest, args[0])
}

func (cp *Compiler) btLenString(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Lens, dest, args[0])
}

func (cp *Compiler) btLiteral(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Litx, dest, args[0])
}

func (cp *Compiler) btLtFloats(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Gthf, dest, args[2], args[0])
}

func (cp *Compiler) btLteFloats(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Gtef, dest, args[2], args[0])
}

func (cp *Compiler) btLtInts(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Gthi, dest, args[2], args[0])
}

func (cp *Compiler) btLteInts(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Gtei, dest, args[2], args[0])
}

func (cp *Compiler) btMakeError(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Mker, dest, args[0], cp.reserveToken(vm, tok))
}

func (cp *Compiler) btModuloIntegers(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.reserve(vm, values.INT, 0)
	cp.put(vm, Equi, args[2], vm.that())
	cp.emit(vm, Qtru, vm.that(), vm.codeTop()+3)
	cp.reserveError(vm, "built/mod", tok, []any{})
	cp.emit(vm, Asgm, dest, vm.that())
	cp.emit(vm, Jmp, vm.codeTop()+2)
	cp.emit(vm, Modi, dest, args[0], args[2])
}

func (cp *Compiler) btMultiplyFloats(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Mulf, dest, args[0], args[2])
}

func (cp *Compiler) btMultiplyIntegers(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Muli, dest, args[0], args[2])
}

func (cp *Compiler) btNegateFloat(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Negf, dest, args[0])
}

func (cp *Compiler) btNegateInteger(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Negi, dest, args[0])
}

func (cp *Compiler) btSubtractFloats(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Subf, dest, args[0], args[2])
}

func (cp *Compiler) btString(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Strx, dest, args[0])
}

func (cp *Compiler) btSubtractIntegers(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Subi, dest, args[0], args[2])
}

func (cp *Compiler) btType(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Typx, dest, args[0])
}

func (cp *Compiler) btTupleOfSingle(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Cv1T, dest, args[0])
}

func (cp *Compiler) btTupleOfTuple(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Asgm, dest, args[0])
}

func (cp *Compiler) btTypeOfTuple(vm *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(vm, Asgm, dest, cp.tupleType)
}
