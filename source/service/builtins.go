package service

import (
	"pipefish/source/token"
	"pipefish/source/values"
)

type functionAndReturnType struct {
	f func(cp *Compiler, tok *token.Token, dest uint32, args []uint32)
	T AlternateType
}

var BUILTINS = map[string]functionAndReturnType{
	"add_floats":         {(*Compiler).btAddFloats, AltType(values.FLOAT)},
	"add_integers":       {(*Compiler).btAddIntegers, AltType(values.INT)},
	"add_lists":          {(*Compiler).btAddLists, AltType(values.LIST)},
	"add_rune_to_rune":   {(*Compiler).btAddRuneToRune, AltType(values.STRING)},
	"add_rune_to_string": {(*Compiler).btAddRuneToString, AltType(values.STRING)},
	"add_sets":           {(*Compiler).btAddSets, AltType(values.SET)},
	"add_string_to_rune": {(*Compiler).btAddStringToRune, AltType(values.STRING)},
	"add_strings":        {(*Compiler).btAddStrings, AltType(values.STRING)},
	"codepoint":          {(*Compiler).btCodepoint, AltType(values.INT)},
	"divide_floats":      {(*Compiler).btDivideFloats, AltType(values.ERROR, values.FLOAT)},
	"divide_integers":    {(*Compiler).btDivideIntegers, AltType(values.ERROR, values.INT)},
	"first_in_tuple":     {(*Compiler).btFirstInTuple, AltType()}, // Types need to be added by the caller..
	"float_of_int":       {(*Compiler).btFloatOfInt, AltType(values.FLOAT)},
	"float_of_string":    {(*Compiler).btFloatOfString, AltType(values.ERROR, values.FLOAT)},
	"get_from_external":  {(*Compiler).btGetFromSpecialSnippet, AltType(values.SUCCESSFUL_VALUE, values.ERROR)},
	"get_from_input":     {(*Compiler).btGetFromInput, AltType(values.SUCCESSFUL_VALUE)},
	"get_from_SQL":       {(*Compiler).btGetFromSpecialSnippet, AltType(values.SUCCESSFUL_VALUE, values.ERROR)},
	"gt_floats":          {(*Compiler).btGtFloats, AltType(values.BOOL)},
	"gte_floats":         {(*Compiler).btGteFloats, AltType(values.BOOL)},
	"gt_ints":            {(*Compiler).btGtInts, AltType(values.BOOL)},
	"gte_ints":           {(*Compiler).btGteInts, AltType(values.BOOL)},
	"identity":           {(*Compiler).btIdentity, AltType(values.TUPLE)},
	"int_of_float":       {(*Compiler).btIntOfFloat, AltType(values.INT)},
	"int_of_string":      {(*Compiler).btIntOfString, AltType(values.ERROR, values.INT)},
	"keys_of_map":        {(*Compiler).btKeysOfMap, AltType(values.LIST)},
	"keys_of_struct":     {(*Compiler).btKeysOfStruct, AltType(values.LIST)},
	"label_of_string":    {(*Compiler).btLabelOfString, AltType(values.LABEL)},
	"last_in_tuple":      {(*Compiler).btLastInTuple, AltType()},
	"len_list":           {(*Compiler).btLenList, AltType(values.INT)},
	"len_map":            {(*Compiler).btLenMap, AltType(values.INT)},
	"len_set":            {(*Compiler).btLenSet, AltType(values.INT)},
	"len_string":         {(*Compiler).btLenString, AltType(values.INT)},
	"len_tuple":          {(*Compiler).btLenTuple, AltType(values.INT)},
	"list_with":          {(*Compiler).btListWith, AltType(values.LIST)},
	"literal":            {(*Compiler).btLiteral, AltType(values.STRING)},
	"lt_floats":          {(*Compiler).btLtFloats, AltType(values.BOOL)},
	"lte_floats":         {(*Compiler).btLteFloats, AltType(values.BOOL)},
	"lt_ints":            {(*Compiler).btLtInts, AltType(values.BOOL)},
	"lte_ints":           {(*Compiler).btLteInts, AltType(values.BOOL)},
	"make_error":         {(*Compiler).btMakeError, AltType(values.ERROR)},
	"make_map":           {(*Compiler).btMakeMap, AltType(values.MAP)},
	"make_pair":          {(*Compiler).btMakePair, AltType(values.PAIR)},
	"make_set":           {(*Compiler).btMakeSet, AltType(values.SET)},
	"map_with":           {(*Compiler).btMapWith, AltType(values.MAP)},
	"map_without":        {(*Compiler).btMapWithout, AltType(values.MAP)},
	"modulo_integers":    {(*Compiler).btModuloIntegers, AltType(values.ERROR, values.INT)},
	"multiply_floats":    {(*Compiler).btMultiplyFloats, AltType(values.FLOAT)},
	"multiply_integers":  {(*Compiler).btMultiplyIntegers, AltType(values.INT)},
	"negate_float":       {(*Compiler).btNegateFloat, AltType(values.FLOAT)},
	"negate_integer":     {(*Compiler).btNegateInteger, AltType(values.INT)},
	"post_html":          {(*Compiler).btPostSpecialSnippet, AltType(values.SUCCESSFUL_VALUE, values.ERROR)},
	"post_to_output":     {(*Compiler).btPostToOutput, AltType(values.SUCCESSFUL_VALUE)},
	"post_sql":           {(*Compiler).btPostSpecialSnippet, AltType(values.SUCCESSFUL_VALUE, values.ERROR)},
	"post_to_terminal":   {(*Compiler).btPostToTerminal, AltType(values.SUCCESSFUL_VALUE)},
	"rune":               {(*Compiler).btRune, AltType(values.RUNE)},
	"string":             {(*Compiler).btString, AltType(values.STRING)},
	"single_in_list":     {(*Compiler).btSingleInList, AltType(values.BOOL)},
	"single_in_set":      {(*Compiler).btSingleInSet, AltType(values.BOOL)},
	"single_in_tuple":    {(*Compiler).btSingleInTuple, AltType(values.BOOL)},
	"single_in_type":     {(*Compiler).btSingleInType, AltType(values.BOOL)},
	"subtract_floats":    {(*Compiler).btSubtractFloats, AltType(values.FLOAT)},
	"struct_with":        {(*Compiler).btStructWith, AltType()},
	"subtract_integers":  {(*Compiler).btSubtractIntegers, AltType(values.INT)},
	"tuple_of_tuple":     {(*Compiler).btTupleOfTuple, AltType()}, // Since we can't know the typeschemes in advance, these are kludged in by the seekFunctionCall method.
	"tuple_of_varargs":   {(*Compiler).btTupleOfVarargs, AltType()},
	"tuplify_list":       {(*Compiler).btTuplifyList, AltType()}, // 			"
	"type":               {(*Compiler).btType, AltType(values.TYPE)},
	"type_with":          {(*Compiler).btTypeWith, AltType()},
	"type_of_tuple":      {(*Compiler).btTypeOfTuple, AltType(values.TYPE)},
	"type_union":         {(*Compiler).btTypeUnion, AltType(values.TYPE)},
	"varchar":            {(*Compiler).btVarchar, AltType(values.TYPE, values.ERROR)},
}

func (cp *Compiler) btAddFloats(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Addf, dest, args[0], args[2])
}

func (cp *Compiler) btAddIntegers(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Addi, dest, args[0], args[2])
}

func (cp *Compiler) btAddLists(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(AddL, dest, args[0], args[2])
}

func (cp *Compiler) btAddRuneToRune(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Adrr, dest, args[0], args[2])
}

func (cp *Compiler) btAddRuneToString(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Adrs, dest, args[0], args[2])
}

func (cp *Compiler) btAddSets(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(AddS, dest, args[0], args[2])
}

func (cp *Compiler) btAddStrings(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Adds, dest, args[0], args[2])
}

func (cp *Compiler) btAddStringToRune(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Adsr, dest, args[0], args[2])
}

func (cp *Compiler) btCodepoint(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Cpnt, dest, args[0])
}

func (cp *Compiler) btDivideFloats(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Divf, dest, args[0], args[2], cp.reserveToken(tok))
}

func (cp *Compiler) btDivideIntegers(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Divi, dest, args[0], args[2], cp.reserveToken(tok))
}

func (cp *Compiler) btFirstInTuple(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Tplf, dest, args[0], cp.reserveToken(tok))
}

func (cp *Compiler) btFloatOfInt(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Flti, dest, args[0])
}

func (cp *Compiler) btFloatOfString(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Flts, dest, args[0])
}

func (cp *Compiler) btGetFromSpecialSnippet(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Gsnp, cp.vm.Mem[args[0]].V.(uint32), args[2])
	cp.Emit(Qtyp, cp.vm.Mem[args[0]].V.(uint32), uint32(values.ERROR), cp.CodeTop()+3)
	cp.Emit(Asgm, dest, cp.vm.Mem[args[0]].V.(uint32))
	cp.Emit(Jmp, cp.CodeTop()+2)
	cp.Emit(Asgm, dest, values.C_OK)
}

func (cp *Compiler) btGetFromInput(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Inpt, cp.vm.Mem[args[0]].V.(uint32), args[2])
	cp.Emit(Asgm, dest, values.C_OK)
}

func (cp *Compiler) btGtFloats(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Gthf, dest, args[0], args[2])
}

func (cp *Compiler) btGteFloats(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Gtef, dest, args[0], args[2])
}

func (cp *Compiler) btGtInts(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Gthi, dest, args[0], args[2])
}

func (cp *Compiler) btGteInts(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Gtei, dest, args[0], args[2])
}

func (cp *Compiler) btIdentity(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Idfn, dest, args[0])
}

func (cp *Compiler) btIntOfFloat(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Intf, dest, args[0])
}

func (cp *Compiler) btIntOfString(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Ints, dest, args[0])
}

func (cp *Compiler) btKeysOfMap(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(KeyM, dest, args[0])
}

func (cp *Compiler) btKeysOfStruct(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(KeyZ, dest, args[0])
}

func (cp *Compiler) btLabelOfString(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Lbls, dest, args[0], cp.reserveToken(tok))
}

func (cp *Compiler) btLastInTuple(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Tpll, dest, args[0], cp.reserveToken(tok))
}

func (cp *Compiler) btLenList(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(LenL, dest, args[0])
}

func (cp *Compiler) btLenMap(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(LenM, dest, args[0])
}

func (cp *Compiler) btLenSet(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(LenS, dest, args[0])
}

func (cp *Compiler) btLenString(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Lens, dest, args[0])
}

func (cp *Compiler) btLenTuple(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(LenT, dest, args[0])
}

func (cp *Compiler) btListWith(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(WthL, dest, args[0], args[1], cp.reserveToken(tok))
}

func (cp *Compiler) btLiteral(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Litx, dest, args[0])
}

func (cp *Compiler) btLtFloats(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Gthf, dest, args[2], args[0])
}

func (cp *Compiler) btLteFloats(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Gtef, dest, args[2], args[0])
}

func (cp *Compiler) btLtInts(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Gthi, dest, args[2], args[0])
}

func (cp *Compiler) btLteInts(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Gtei, dest, args[2], args[0])
}

func (cp *Compiler) btMakeError(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Mker, dest, args[0], cp.reserveToken(tok))
}

func (cp *Compiler) btMakeMap(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Mkmp, dest, args[0], cp.reserveToken(tok))
}

func (cp *Compiler) btMakePair(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Mkpr, dest, args[0], args[2])
}

func (cp *Compiler) btMakeSet(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Mkst, dest, args[0], cp.reserveToken(tok))
}

func (cp *Compiler) btMapWith(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(WthM, dest, args[0], args[1], cp.reserveToken(tok))
}

func (cp *Compiler) btMapWithout(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(WtoM, dest, args[0], args[1], cp.reserveToken(tok))
}

func (cp *Compiler) btModuloIntegers(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Modi, dest, args[0], args[2], cp.reserveToken(tok))
}

func (cp *Compiler) btMultiplyFloats(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Mulf, dest, args[0], args[2])
}

func (cp *Compiler) btMultiplyIntegers(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Muli, dest, args[0], args[2])
}

func (cp *Compiler) btNegateFloat(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Negf, dest, args[0])
}

func (cp *Compiler) btNegateInteger(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Negi, dest, args[0])
}

func (cp *Compiler) btPostToOutput(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Outp, args[0])
	cp.Emit(Asgm, dest, values.C_OK)
}

func (cp *Compiler) btPostSpecialSnippet(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Psnp, dest, args[0])
}

func (cp *Compiler) btPostToTerminal(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Outt, args[0])
	cp.Emit(Asgm, dest, values.C_OK)
}

func (cp *Compiler) btRune(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Itor, dest, args[0])
}

func (cp *Compiler) btSingleInList(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(InxL, dest, args[0], args[2])
}

func (cp *Compiler) btSingleInSet(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(InxS, dest, args[0], args[2])
}

func (cp *Compiler) btSingleInTuple(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(InxT, dest, args[0], args[2])
}

func (cp *Compiler) btSingleInType(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Inxt, dest, args[0], args[2])
}

func (cp *Compiler) btString(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Strx, dest, args[0])
}

func (cp *Compiler) btStructWith(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(WthZ, dest, args[0], args[1], cp.reserveToken(tok))
}

func (cp *Compiler) btSubtractFloats(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Subf, dest, args[0], args[2])
}

func (cp *Compiler) btSubtractIntegers(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Subi, dest, args[0], args[2])
}

func (cp *Compiler) btTuplifyList(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(TupL, dest, args[0])
}

func (cp *Compiler) btTupleOfTuple(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Asgm, dest, args[0])
}

func (cp *Compiler) btTupleOfVarargs(tok *token.Token, dest uint32, args []uint32) {
	destWithArgs := append([]uint32{dest}, args...)
	cp.Emit(CvTT, destWithArgs...)
}

func (cp *Compiler) btType(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Typx, dest, args[0])
}

func (cp *Compiler) btTypeOfTuple(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Asgm, dest, cp.TupleType)
}

func (cp *Compiler) btTypeUnion(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Typu, dest, args[0], args[2])
}

func (cp *Compiler) btTypeWith(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Wtht, dest, args[0], args[1], cp.reserveToken(tok))
}

func (cp *Compiler) btVarchar(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(Varc, dest, args[0], cp.reserveToken(tok))
}
