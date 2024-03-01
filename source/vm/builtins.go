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

func (cp *Compiler) btAddFloats(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Addf, dest, args[0], args[2])
}

func (cp *Compiler) btAddIntegers(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Addi, dest, args[0], args[2])
}

func (cp *Compiler) btAddStrings(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Adds, dest, args[0], args[2])
}

func (cp *Compiler) btDivideFloats(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.reserve(mc, values.FLOAT, 0.0)
	cp.put(mc, Equf, args[2], mc.that())
	cp.emit(mc, Qtru, mc.that(), mc.codeTop()+3)
	cp.reserveError(mc, "built/div/float", tok, []any{})
	cp.emit(mc, Asgm, dest, mc.that())
	cp.emit(mc, Jmp, mc.codeTop()+2)
	cp.emit(mc, Divf, dest, args[0], args[2])
}

func (cp *Compiler) btDivideIntegers(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.reserve(mc, values.INT, 0)
	cp.put(mc, Equi, args[2], mc.that())
	cp.emit(mc, Qtru, mc.that(), mc.codeTop()+3)
	cp.reserveError(mc, "built/div/int", tok, []any{})
	cp.emit(mc, Asgm, dest, mc.that())
	cp.emit(mc, Jmp, mc.codeTop()+2)
	cp.emit(mc, Divi, dest, args[0], args[2])
}

func (cp *Compiler) btFloatOfInt(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Flti, dest, args[0])
}

func (cp *Compiler) btFloatOfString(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Flts, dest, args[0])
}

func (cp *Compiler) btGtFloats(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Gthf, dest, args[0], args[2])
}

func (cp *Compiler) btGteFloats(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Gtef, dest, args[0], args[2])
}

func (cp *Compiler) btGtInts(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Gthi, dest, args[0], args[2])
}

func (cp *Compiler) btGteInts(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Gtei, dest, args[0], args[2])
}

func (cp *Compiler) btIntOfFloat(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Intf, dest, args[0])
}

func (cp *Compiler) btIntOfString(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Ints, dest, args[0])
}

func (cp *Compiler) btLenString(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Lens, dest, args[0])
}

func (cp *Compiler) btLiteral(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Litx, dest, args[0])
}

func (cp *Compiler) btLtFloats(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Gthf, dest, args[2], args[0])
}

func (cp *Compiler) btLteFloats(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Gtef, dest, args[2], args[0])
}

func (cp *Compiler) btLtInts(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Gthi, dest, args[2], args[0])
}

func (cp *Compiler) btLteInts(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Gtei, dest, args[2], args[0])
}

func (cp *Compiler) btMakeError(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Mker, dest, args[0], cp.reserveToken(mc, tok))
}

func (cp *Compiler) btModuloIntegers(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.reserve(mc, values.INT, 0)
	cp.put(mc, Equi, args[2], mc.that())
	cp.emit(mc, Qtru, mc.that(), mc.codeTop()+3)
	cp.reserveError(mc, "built/mod", tok, []any{})
	cp.emit(mc, Asgm, dest, mc.that())
	cp.emit(mc, Jmp, mc.codeTop()+2)
	cp.emit(mc, Modi, dest, args[0], args[2])
}

func (cp *Compiler) btMultiplyFloats(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Mulf, dest, args[0], args[2])
}

func (cp *Compiler) btMultiplyIntegers(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Muli, dest, args[0], args[2])
}

func (cp *Compiler) btNegateFloat(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Negf, dest, args[0])
}

func (cp *Compiler) btNegateInteger(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Negi, dest, args[0])
}

func (cp *Compiler) btSubtractFloats(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Subf, dest, args[0], args[2])
}

func (cp *Compiler) btString(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Strx, dest, args[0])
}

func (cp *Compiler) btSubtractIntegers(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Subi, dest, args[0], args[2])
}

func (cp *Compiler) btType(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Typx, dest, args[0])
}

func (cp *Compiler) btTupleOfSingle(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Cv1T, dest, args[0])
}

func (cp *Compiler) btTupleOfTuple(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Asgm, dest, args[0])
}

func (cp *Compiler) btTypeOfTuple(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.emit(mc, Asgm, dest, cp.tupleType)
}
