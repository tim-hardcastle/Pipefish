package service

import (
	"pipefish/source/token"
	"pipefish/source/values"
)

type functionAndReturnType struct {
	f func(cp *Compiler, mc *Vm, tok *token.Token, dest uint32, args []uint32)
	T AlternateType
}

var BUILTINS = map[string]functionAndReturnType{
	"add_floats":        {(*Compiler).btAddFloats, AltType(values.FLOAT)},
	"add_integers":      {(*Compiler).btAddIntegers, AltType(values.INT)},
	"add_lists":         {(*Compiler).btAddLists, AltType(values.LIST)},
	"add_sets":          {(*Compiler).btAddSets, AltType(values.SET)},
	"add_strings":       {(*Compiler).btAddStrings, AltType(values.STRING)},
	"divide_floats":     {(*Compiler).btDivideFloats, AltType(values.ERROR, values.FLOAT)},
	"divide_integers":   {(*Compiler).btDivideIntegers, AltType(values.ERROR, values.INT)},
	"float_of_int":      {(*Compiler).btFloatOfInt, AltType(values.FLOAT)},
	"float_of_string":   {(*Compiler).btFloatOfString, AltType(values.ERROR, values.FLOAT)},
	"get_from_contact":  {(*Compiler).btGetFromSpecialSnippet, AltType(values.SUCCESSFUL_VALUE, values.ERROR)},
	"get_from_input":    {(*Compiler).btGetFromInput, AltType(values.SUCCESSFUL_VALUE)},
	"get_from_SQL":      {(*Compiler).btGetFromSpecialSnippet, AltType(values.SUCCESSFUL_VALUE, values.ERROR)},
	"gt_floats":         {(*Compiler).btGtFloats, AltType(values.BOOL)},
	"gte_floats":        {(*Compiler).btGteFloats, AltType(values.BOOL)},
	"gt_ints":           {(*Compiler).btGtInts, AltType(values.BOOL)},
	"gte_ints":          {(*Compiler).btGteInts, AltType(values.BOOL)},
	"identity":          {(*Compiler).btIdentity, AltType(values.TUPLE)},
	"int_of_float":      {(*Compiler).btIntOfFloat, AltType(values.INT)},
	"int_of_string":     {(*Compiler).btIntOfString, AltType(values.ERROR, values.INT)},
	"keys_of_map":       {(*Compiler).btKeysOfMap, AltType(values.LIST)},
	"keys_of_struct":    {(*Compiler).btKeysOfStruct, AltType(values.LIST)},
	"len_list":          {(*Compiler).btLenList, AltType(values.INT)},
	"len_map":           {(*Compiler).btLenMap, AltType(values.INT)},
	"len_set":           {(*Compiler).btLenSet, AltType(values.INT)},
	"len_string":        {(*Compiler).btLenString, AltType(values.INT)},
	"len_tuple":         {(*Compiler).btLenTuple, AltType(values.INT)},
	"list_with":         {(*Compiler).btListWith, AltType(values.LIST)},
	"literal":           {(*Compiler).btLiteral, AltType(values.STRING)},
	"lt_floats":         {(*Compiler).btLtFloats, AltType(values.BOOL)},
	"lte_floats":        {(*Compiler).btLteFloats, AltType(values.BOOL)},
	"lt_ints":           {(*Compiler).btLtInts, AltType(values.BOOL)},
	"lte_ints":          {(*Compiler).btLteInts, AltType(values.BOOL)},
	"make_error":        {(*Compiler).btMakeError, AltType(values.ERROR)},
	"make_map":          {(*Compiler).btMakeMap, AltType(values.MAP)},
	"make_pair":         {(*Compiler).btMakePair, AltType(values.PAIR)},
	"make_set":          {(*Compiler).btMakeSet, AltType(values.SET)},
	"map_with":          {(*Compiler).btMapWith, AltType(values.MAP)},
	"map_without":       {(*Compiler).btMapWithout, AltType(values.MAP)},
	"modulo_integers":   {(*Compiler).btModuloIntegers, AltType(values.ERROR, values.INT)},
	"multiply_floats":   {(*Compiler).btMultiplyFloats, AltType(values.FLOAT)},
	"multiply_integers": {(*Compiler).btMultiplyIntegers, AltType(values.INT)},
	"negate_float":      {(*Compiler).btNegateFloat, AltType(values.FLOAT)},
	"negate_integer":    {(*Compiler).btNegateInteger, AltType(values.INT)},
	"post_contact":      {(*Compiler).btPostSpecialSnippet, AltType(values.SUCCESSFUL_VALUE, values.ERROR)},
	"post_html":         {(*Compiler).btPostSpecialSnippet, AltType(values.SUCCESSFUL_VALUE, values.ERROR)},
	"post_to_output":    {(*Compiler).btPostToOutput, AltType(values.SUCCESSFUL_VALUE)},
	"post_sql":          {(*Compiler).btPostSpecialSnippet, AltType(values.SUCCESSFUL_VALUE, values.ERROR)},
	"post_to_terminal":  {(*Compiler).btPostToTerminal, AltType(values.SUCCESSFUL_VALUE)},
	"string":            {(*Compiler).btString, AltType(values.STRING)},
	"single_in_list":    {(*Compiler).btSingleInList, AltType(values.BOOL)},
	"single_in_set":     {(*Compiler).btSingleInSet, AltType(values.BOOL)},
	"single_in_tuple":   {(*Compiler).btSingleInTuple, AltType(values.BOOL)},
	"single_in_type":    {(*Compiler).btSingleInType, AltType(values.BOOL)},
	"subtract_floats":   {(*Compiler).btSubtractFloats, AltType(values.FLOAT)},
	"struct_with":       {(*Compiler).btStructWith, AltType()},
	"subtract_integers": {(*Compiler).btSubtractIntegers, AltType(values.INT)},
	"tuple_of_single?":  {(*Compiler).btTupleOfSingle, AltType()}, // Since we can't know the typeschemes in advance, these are kludged in by the seekFunctionCall method.
	"tuple_of_tuple":    {(*Compiler).btTupleOfTuple, AltType()},  //
	"tuplify_list":      {(*Compiler).btTuplifyList, AltType()},   // This needs to be anyTypeScheme, so the compiler adds it in durin intialization.
	"type":              {(*Compiler).btType, AltType(values.TYPE)},
	"type_with":         {(*Compiler).btTypeWith, AltType()},
	"type_of_tuple":     {(*Compiler).btTypeOfTuple, AltType(values.TYPE)},
	"type_union":        {(*Compiler).btTypeUnion, AltType(values.TYPE)},
}

func (cp *Compiler) btAddFloats(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Addf, dest, args[0], args[2])
}

func (cp *Compiler) btAddIntegers(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Addi, dest, args[0], args[2])
}

func (cp *Compiler) btAddLists(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, AddL, dest, args[0], args[2])
}

func (cp *Compiler) btAddSets(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, AddS, dest, args[0], args[2])
}

func (cp *Compiler) btAddStrings(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Adds, dest, args[0], args[2])
}

func (cp *Compiler) btDivideFloats(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Reserve(mc, values.FLOAT, 0.0)
	cp.put(mc, Equf, args[2], mc.That())
	cp.Emit(mc, Qtru, mc.That(), mc.CodeTop()+3)
	cp.reserveError(mc, "built/div/float", tok, []any{})
	cp.Emit(mc, Asgm, dest, mc.That())
	cp.Emit(mc, Jmp, mc.CodeTop()+2)
	cp.Emit(mc, Divf, dest, args[0], args[2])
}

func (cp *Compiler) btDivideIntegers(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Reserve(mc, values.INT, 0)
	cp.put(mc, Equi, args[2], mc.That())
	cp.Emit(mc, Qtru, mc.That(), mc.CodeTop()+3)
	cp.reserveError(mc, "built/div/int", tok, []any{})
	cp.Emit(mc, Asgm, dest, mc.That())
	cp.Emit(mc, Jmp, mc.CodeTop()+2)
	cp.Emit(mc, Divi, dest, args[0], args[2])
}

func (cp *Compiler) btFloatOfInt(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Flti, dest, args[0])
}

func (cp *Compiler) btFloatOfString(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Flts, dest, args[0])
}

func (cp *Compiler) btGetFromSpecialSnippet(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Gsnp, mc.Mem[args[0]].V.(uint32), args[2])
	cp.Emit(mc, Qtyp, mc.Mem[args[0]].V.(uint32), uint32(values.ERROR), mc.CodeTop()+3)
	cp.Emit(mc, Asgm, dest, mc.Mem[args[0]].V.(uint32))
	cp.Emit(mc, Jmp, mc.CodeTop()+2)
	cp.Emit(mc, Asgm, dest, values.C_OK)
}

func (cp *Compiler) btGetFromInput(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Inpt, mc.Mem[args[0]].V.(uint32), args[2])
	cp.Emit(mc, Asgm, dest, values.C_OK)
}

func (cp *Compiler) btGtFloats(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Gthf, dest, args[0], args[2])
}

func (cp *Compiler) btGteFloats(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Gtef, dest, args[0], args[2])
}

func (cp *Compiler) btGtInts(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Gthi, dest, args[0], args[2])
}

func (cp *Compiler) btGteInts(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Gtei, dest, args[0], args[2])
}

func (cp *Compiler) btIdentity(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Idfn, dest, args[0])
}

func (cp *Compiler) btIntOfFloat(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Intf, dest, args[0])
}

func (cp *Compiler) btIntOfString(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Ints, dest, args[0])
}

func (cp *Compiler) btKeysOfMap(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, KeyM, dest, args[0])
}

func (cp *Compiler) btKeysOfStruct(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, KeyZ, dest, args[0])
}

func (cp *Compiler) btLenList(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, LenL, dest, args[0])
}

func (cp *Compiler) btLenMap(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, LenM, dest, args[0])
}

func (cp *Compiler) btLenSet(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, LenS, dest, args[0])
}

func (cp *Compiler) btLenString(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Lens, dest, args[0])
}

func (cp *Compiler) btLenTuple(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, LenT, dest, args[0])
}

func (cp *Compiler) btListWith(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.reserveError(mc, "built/with/list", tok, []any{})
	cp.Emit(mc, WthL, dest, args[0], args[1], mc.That())
}

func (cp *Compiler) btLiteral(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Litx, dest, args[0])
}

func (cp *Compiler) btLtFloats(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Gthf, dest, args[2], args[0])
}

func (cp *Compiler) btLteFloats(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Gtef, dest, args[2], args[0])
}

func (cp *Compiler) btLtInts(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Gthi, dest, args[2], args[0])
}

func (cp *Compiler) btLteInts(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Gtei, dest, args[2], args[0])
}

func (cp *Compiler) btMakeError(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Mker, dest, args[0], cp.reserveToken(mc, tok))
}

func (cp *Compiler) btMakeMap(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.reserveError(mc, "built/map/pair", tok, []any{})
	cp.reserveError(mc, "built/map/type", tok, []any{})
	cp.Emit(mc, Mkmp, dest, args[0])
}

func (cp *Compiler) btMakePair(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Mkpr, dest, args[0], args[2])
}

func (cp *Compiler) btMakeSet(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.reserveError(mc, "built/set/type", tok, []any{})
	cp.Emit(mc, Mkst, dest, args[0])
}

func (cp *Compiler) btMapWith(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.reserveError(mc, "built/with/map", tok, []any{})
	cp.Emit(mc, WthM, dest, args[0], args[1], mc.That())
}

func (cp *Compiler) btMapWithout(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.reserveError(mc, "built/without/map", tok, []any{})
	cp.Emit(mc, WtoM, dest, args[0], args[1], mc.That())
}

func (cp *Compiler) btModuloIntegers(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Reserve(mc, values.INT, 0)
	cp.put(mc, Equi, args[2], mc.That())
	cp.Emit(mc, Qtru, mc.That(), mc.CodeTop()+3)
	cp.reserveError(mc, "built/mod", tok, []any{})
	cp.Emit(mc, Asgm, dest, mc.That())
	cp.Emit(mc, Jmp, mc.CodeTop()+2)
	cp.Emit(mc, Modi, dest, args[0], args[2])
}

func (cp *Compiler) btMultiplyFloats(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Mulf, dest, args[0], args[2])
}

func (cp *Compiler) btMultiplyIntegers(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Muli, dest, args[0], args[2])
}

func (cp *Compiler) btNegateFloat(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Negf, dest, args[0])
}

func (cp *Compiler) btNegateInteger(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Negi, dest, args[0])
}

func (cp *Compiler) btPostToOutput(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Outp, args[0])
	cp.Emit(mc, Asgm, dest, values.C_OK)
}

func (cp *Compiler) btPostSpecialSnippet(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Psnp, dest, args[0])
	cp.Emit(mc, Asgm, dest, values.C_OK)
}

func (cp *Compiler) btPostToTerminal(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Outt, args[0])
	cp.Emit(mc, Asgm, dest, values.C_OK)
}

func (cp *Compiler) btSingleInList(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, InxL, dest, args[0], args[2])
}

func (cp *Compiler) btSingleInSet(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, InxS, dest, args[0], args[2])
}

func (cp *Compiler) btSingleInTuple(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, InxT, dest, args[0], args[1])
}

func (cp *Compiler) btSingleInType(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Inxt, dest, args[0], args[2])
}

func (cp *Compiler) btString(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Strx, dest, args[0])
}

func (cp *Compiler) btStructWith(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.reserveError(mc, "built/with/struct", tok, []any{})
	cp.Emit(mc, WthZ, dest, args[0], args[1], mc.That())
}

func (cp *Compiler) btSubtractFloats(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Subf, dest, args[0], args[2])
}

func (cp *Compiler) btSubtractIntegers(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Subi, dest, args[0], args[2])
}

func (cp *Compiler) btTupleOfSingle(mc *Vm, tok *token.Token, dest uint32, args []uint32) { // TODO --- do we need this or not?
	cp.Emit(mc, Cv1T, dest, args[0])
}

func (cp *Compiler) btTuplifyList(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, TupL, dest, args[0])
}

func (cp *Compiler) btTupleOfTuple(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	destWithArgs := append([]uint32{dest}, args...)
	cp.Emit(mc, CvTT, destWithArgs...)
}

func (cp *Compiler) btType(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Typx, dest, args[0])
}

func (cp *Compiler) btTypeOfTuple(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Asgm, dest, cp.TupleType)
}

func (cp *Compiler) btTypeUnion(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(mc, Typu, dest, args[0], args[2])
}

func (cp *Compiler) btTypeWith(mc *Vm, tok *token.Token, dest uint32, args []uint32) {
	cp.reserveError(mc, "built/with/type", tok, []any{})
	cp.Emit(mc, Wtht, dest, args[0], args[1], mc.That())
}
