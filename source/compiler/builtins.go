package compiler

import (
	"github.com/tim-hardcastle/Pipefish/source/token"
	"github.com/tim-hardcastle/Pipefish/source/values"
	"github.com/tim-hardcastle/Pipefish/source/vm"
)

type functionAndReturnType struct {
	f func(cp *Compiler, tok *token.Token, dest uint32, args []uint32)
	T AlternateType
}

var BUILTINS = map[string]functionAndReturnType{
	"add_floats":                {(*Compiler).btAddFloats, AltType(values.FLOAT)},
	"add_integers":              {(*Compiler).btAddIntegers, AltType(values.INT)},
	"add_lists":                 {(*Compiler).btAddLists, AltType(values.LIST)},
	"add_rune_to_rune":          {(*Compiler).btAddRuneToRune, AltType(values.STRING)},
	"add_rune_to_string":        {(*Compiler).btAddRuneToString, AltType(values.STRING)},
	"add_sets":                  {(*Compiler).btAddSets, AltType(values.SET)},
	"add_string_to_rune":        {(*Compiler).btAddStringToRune, AltType(values.STRING)},
	"add_strings":               {(*Compiler).btAddStrings, AltType(values.STRING)},
	"cast":                      {(*Compiler).btCast, AltType()}, // Types have to be figured out at call site.
	"cast_to_float":             {(*Compiler).btCastToFloat, AltType(values.FLOAT)},
	"cast_to_int":               {(*Compiler).btCastToInt, AltType(values.INT)},
	"cast_to_list":              {(*Compiler).btCastToList, AltType(values.LIST)},
	"cast_to_map":               {(*Compiler).btCastToMap, AltType(values.MAP)},
	"cast_to_pair":              {(*Compiler).btCastToPair, AltType(values.PAIR)},
	"cast_to_set":               {(*Compiler).btCastToSet, AltType(values.SET)},
	"cast_to_snippet":           {(*Compiler).btCastToSnippet, AltType(values.SNIPPET)},
	"cast_to_string":            {(*Compiler).btCastToString, AltType(values.STRING)},
	"codepoint":                 {(*Compiler).btCodepoint, AltType(values.INT)},
	"divide_floats":             {(*Compiler).btDivideFloats, AltType(values.ERROR, values.FLOAT)},
	"divide_float_by_integer":   {(*Compiler).btDivideFloatByInteger, AltType(values.ERROR, values.FLOAT)},
	"divide_integers":           {(*Compiler).btDivideIntegers, AltType(values.ERROR, values.INT)},
	"divide_integer_by_float":   {(*Compiler).btDivideIntegerByFloat, AltType(values.ERROR, values.FLOAT)},
	"divide_integers_to_float":  {(*Compiler).btDivideIntegersToFloat, AltType(values.ERROR, values.FLOAT)},
	"first_in_tuple":            {(*Compiler).btFirstInTuple, AltType()}, // Types need to be added by the caller.
	"float_of_int":              {(*Compiler).btFloatOfInt, AltType(values.FLOAT)},
	"float_of_string":           {(*Compiler).btFloatOfString, AltType(values.ERROR, values.FLOAT)},
	"get_from_input":            {(*Compiler).btGetFromInput, AltType(values.SUCCESSFUL_VALUE)},
	"get_sql":                   {(*Compiler).btGetFromSQL, AltType(values.SUCCESSFUL_VALUE, values.ERROR)},
	"gt_floats":                 {(*Compiler).btGtFloats, AltType(values.BOOL)},
	"gte_floats":                {(*Compiler).btGteFloats, AltType(values.BOOL)},
	"gt_ints":                   {(*Compiler).btGtInts, AltType(values.BOOL)},
	"gte_ints":                  {(*Compiler).btGteInts, AltType(values.BOOL)},
	"intersect_sets":            {(*Compiler).btIntersectSets, AltType(values.SET)},
	"int_of_enum":               {(*Compiler).btIntOfEnum, AltType(values.INT)},
	"int_of_float":              {(*Compiler).btIntOfFloat, AltType(values.INT)},
	"int_of_string":             {(*Compiler).btIntOfString, AltType(values.ERROR, values.INT)},
	"keys_of_map":               {(*Compiler).btKeysOfMap, AltType(values.LIST)},
	"keys_of_struct":            {(*Compiler).btKeysOfStruct, AltType(values.LIST)},
	"label_of_string":           {(*Compiler).btLabelOfString, AltType(values.LABEL)},
	"last_in_tuple":             {(*Compiler).btLastInTuple, AltType()},
	"len_list":                  {(*Compiler).btLenList, AltType(values.INT)},
	"len_map":                   {(*Compiler).btLenMap, AltType(values.INT)},
	"len_set":                   {(*Compiler).btLenSet, AltType(values.INT)},
	"len_snippet":               {(*Compiler).btLenSnippet, AltType(values.INT)},
	"len_string":                {(*Compiler).btLenString, AltType(values.INT)},
	"len_tuple":                 {(*Compiler).btLenTuple, AltType(values.INT)},
	"list_with":                 {(*Compiler).btListWith, AltType(values.LIST)},
	"literal":                   {(*Compiler).btLiteral, AltType(values.STRING)},
	"lt_floats":                 {(*Compiler).btLtFloats, AltType(values.BOOL)},
	"lte_floats":                {(*Compiler).btLteFloats, AltType(values.BOOL)},
	"lt_ints":                   {(*Compiler).btLtInts, AltType(values.BOOL)},
	"lte_ints":                  {(*Compiler).btLteInts, AltType(values.BOOL)},
	"make_error":                {(*Compiler).btMakeError, AltType(values.ERROR)},
	"make_map":                  {(*Compiler).btMakeMap, AltType(values.MAP)},
	"make_pair":                 {(*Compiler).btMakePair, AltType(values.PAIR)},
	"make_set":                  {(*Compiler).btMakeSet, AltType(values.SET)},
	"make_snippet":              {(*Compiler).btMakeSnippet, AltType(values.SNIPPET)},
	"map_with":                  {(*Compiler).btMapWith, AltType(values.MAP)},
	"map_without":               {(*Compiler).btMapWithout, AltType(values.MAP)},
	"modulo_integers":           {(*Compiler).btModuloIntegers, AltType(values.ERROR, values.INT)},
	"multiply_floats":           {(*Compiler).btMultiplyFloats, AltType(values.FLOAT)},
	"multiply_float_by_integer": {(*Compiler).btMultiplyFloatByInteger, AltType(values.FLOAT)},
	"multiply_integers":         {(*Compiler).btMultiplyIntegers, AltType(values.INT)},
	"multiply_integer_by_float": {(*Compiler).btMultiplyIntegerByFloat, AltType(values.FLOAT)},
	"negate_float":              {(*Compiler).btNegateFloat, AltType(values.FLOAT)},
	"negate_integer":            {(*Compiler).btNegateInteger, AltType(values.INT)},
	"post_to_output":            {(*Compiler).btPostToOutput, AltType(values.SUCCESSFUL_VALUE)},
	"post_sql":                  {(*Compiler).btPostToSQL, AltType(values.SUCCESSFUL_VALUE, values.ERROR)},
	"post_to_terminal":          {(*Compiler).btPostToTerminal, AltType(values.SUCCESSFUL_VALUE)},
	"rune":                      {(*Compiler).btRune, AltType(values.RUNE)},
	"secret":                    {(*Compiler).btSecret, AltType(values.SECRET)},
	"single_in_list":            {(*Compiler).btSingleInList, AltType(values.BOOL)},
	"single_in_set":             {(*Compiler).btSingleInSet, AltType(values.BOOL)},
	"single_in_tuple":           {(*Compiler).btSingleInTuple, AltType(values.BOOL)},
	"single_in_type":            {(*Compiler).btSingleInType, AltType(values.BOOL)},
	"subtract_floats":           {(*Compiler).btSubtractFloats, AltType(values.FLOAT)},
	"string":                    {(*Compiler).btString, AltType(values.STRING)},
	"struct_with":               {(*Compiler).btStructWith, AltType()},
	"subtract_integers":         {(*Compiler).btSubtractIntegers, AltType(values.INT)},
	"subtract_sets":             {(*Compiler).btSubtractSets, AltType(values.SET)},
	"tuple_of_tuple":            {(*Compiler).btTupleOfTuple, AltType()}, // Since we can't know the typeschemes in advance, these are kludged in by the seekFunctionCall method.
	"tuple_of_varargs":          {(*Compiler).btTupleOfVarargs, AltType()},
	"type":                      {(*Compiler).btType, AltType(values.TYPE)},
	"type_with":                 {(*Compiler).btTypeWith, AltType()},
	"type_of_tuple":             {(*Compiler).btTypeOfTuple, AltType(values.TYPE)},
	"type_union":                {(*Compiler).btTypeUnion, AltType(values.TYPE)},
	"types_of_type":             {(*Compiler).btTypesOfType, AltType(values.SET)},
	"varchar":                   {(*Compiler).btVarchar, AltType(values.TYPE, values.ERROR)},
}

func (cp *Compiler) btAddFloats(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Addf, dest, args[0], args[2])
}

func (cp *Compiler) btAddIntegers(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Addi, dest, args[0], args[2])
}

func (cp *Compiler) btAddLists(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.AddL, dest, args[0], args[2])
}

func (cp *Compiler) btAddRuneToRune(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Adrr, dest, args[0], args[2])
}

func (cp *Compiler) btAddRuneToString(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Adrs, dest, args[0], args[2])
}

func (cp *Compiler) btAddSets(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.AddS, dest, args[0], args[2])
}

func (cp *Compiler) btAddStrings(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Adds, dest, args[0], args[2])
}

func (cp *Compiler) btAddStringToRune(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Adsr, dest, args[0], args[2])
}

func (cp *Compiler) btCast(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Casx, dest, args[0], args[1], cp.ReserveToken(tok))
}

func (cp *Compiler) btCastToFloat(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Cast, dest, args[0], uint32(values.FLOAT))
}

func (cp *Compiler) btCastToInt(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Cast, dest, args[0], uint32(values.INT))
}

func (cp *Compiler) btCastToList(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Cast, dest, args[0], uint32(values.LIST))
}

func (cp *Compiler) btCastToMap(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Cast, dest, args[0], uint32(values.MAP))
}

func (cp *Compiler) btCastToPair(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Cast, dest, args[0], uint32(values.PAIR))
}

func (cp *Compiler) btCastToSet(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Cast, dest, args[0], uint32(values.SET))
}

func (cp *Compiler) btCastToSnippet(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Cast, dest, args[0], uint32(values.SNIPPET))
}

func (cp *Compiler) btCastToString(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Cast, dest, args[0], uint32(values.STRING))
}

func (cp *Compiler) btCodepoint(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Cpnt, dest, args[0])
}

func (cp *Compiler) btDivideFloats(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Divf, dest, args[0], args[2], cp.ReserveToken(tok))
}

func (cp *Compiler) btDivideFloatByInteger(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Dvfi, dest, args[0], args[2], cp.ReserveToken(tok))
}

func (cp *Compiler) btDivideIntegers(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Divi, dest, args[0], args[2], cp.ReserveToken(tok))
}

func (cp *Compiler) btDivideIntegerByFloat(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Dvif, dest, args[0], args[2], cp.ReserveToken(tok))
}

func (cp *Compiler) btDivideIntegersToFloat(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Diif, dest, args[0], args[2], cp.ReserveToken(tok))
}

func (cp *Compiler) btFirstInTuple(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Tplf, dest, args[0], cp.ReserveToken(tok))
}

func (cp *Compiler) btFloatOfInt(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Flti, dest, args[0])
}

func (cp *Compiler) btFloatOfString(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Flts, dest, args[0])
}

func (cp *Compiler) btGetFromInput(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Inpt, cp.Vm.Mem[args[0]].V.(uint32), args[2])
	cp.Emit(vm.Asgm, dest, values.C_OK)
}

func (cp *Compiler) btGetFromSQL(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Gsql, dest, cp.Vm.Mem[args[0]].V.(uint32), args[2], args[4], args[5], cp.ReserveToken(tok))
}

func (cp *Compiler) btGtFloats(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Gthf, dest, args[0], args[2])
}

func (cp *Compiler) btGteFloats(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Gtef, dest, args[0], args[2])
}

func (cp *Compiler) btGtInts(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Gthi, dest, args[0], args[2])
}

func (cp *Compiler) btGteInts(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Gtei, dest, args[0], args[2])
}

func (cp *Compiler) btIntersectSets(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.IctS, dest, args[0], args[2])
}

func (cp *Compiler) btIntOfEnum(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Inte, dest, args[0])
}

func (cp *Compiler) btIntOfFloat(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Intf, dest, args[0])
}

func (cp *Compiler) btIntOfString(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Ints, dest, args[0])
}

func (cp *Compiler) btKeysOfMap(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.KeyM, dest, args[0])
}

func (cp *Compiler) btKeysOfStruct(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.KeyZ, dest, args[0])
}

func (cp *Compiler) btLabelOfString(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Lbls, dest, args[0], cp.ReserveToken(tok))
}

func (cp *Compiler) btLastInTuple(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Tpll, dest, args[0], cp.ReserveToken(tok))
}

func (cp *Compiler) btLenList(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.LenL, dest, args[0])
}

func (cp *Compiler) btLenMap(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.LenM, dest, args[0])
}

func (cp *Compiler) btLenSet(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.LenS, dest, args[0])
}

func (cp *Compiler) btLenSnippet(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.LnSn, dest, args[0])
}

func (cp *Compiler) btLenString(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Lens, dest, args[0])
}

func (cp *Compiler) btLenTuple(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.LenT, dest, args[0])
}

func (cp *Compiler) btListWith(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.WthL, dest, args[0], args[2], cp.ReserveToken(tok))
}

func (cp *Compiler) btLiteral(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Litx, dest, args[0])
}

func (cp *Compiler) btLtFloats(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Gthf, dest, args[2], args[0])
}

func (cp *Compiler) btLteFloats(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Gtef, dest, args[2], args[0])
}

func (cp *Compiler) btLtInts(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Gthi, dest, args[2], args[0])
}

func (cp *Compiler) btLteInts(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Gtei, dest, args[2], args[0])
}

func (cp *Compiler) btMakeError(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Mker, dest, args[0], cp.ReserveToken(tok))
}

func (cp *Compiler) btMakeMap(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Mkmp, dest, args[0], cp.ReserveToken(tok))
}

func (cp *Compiler) btMakePair(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Mkpr, dest, args[0], args[2])
}

func (cp *Compiler) btMakeSnippet(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.CoSn, dest, args[0], cp.ReserveToken(tok))
}

func (cp *Compiler) btMakeSet(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Mkst, dest, args[0], cp.ReserveToken(tok))
}

func (cp *Compiler) btMapWith(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.WthM, dest, args[0], args[2], cp.ReserveToken(tok))
}

func (cp *Compiler) btMapWithout(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.WtoM, dest, args[0], args[2], cp.ReserveToken(tok))
}

func (cp *Compiler) btModuloIntegers(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Modi, dest, args[0], args[2], cp.ReserveToken(tok))
}

func (cp *Compiler) btMultiplyFloatByInteger(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Mlfi, dest, args[0], args[2])
}

func (cp *Compiler) btMultiplyFloats(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Mulf, dest, args[0], args[2])
}

func (cp *Compiler) btMultiplyIntegerByFloat(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Mlfi, dest, args[2], args[0])
}

func (cp *Compiler) btMultiplyIntegers(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Muli, dest, args[0], args[2])
}

func (cp *Compiler) btNegateFloat(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Negf, dest, args[0])
}

func (cp *Compiler) btNegateInteger(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Negi, dest, args[0])
}

func (cp *Compiler) btPostToOutput(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Outp, args[0])
	cp.Emit(vm.Asgm, dest, values.C_OK)
}

func (cp *Compiler) btPostToSQL(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Psql, dest, args[1], args[2], cp.ReserveToken(tok))
}

func (cp *Compiler) btPostToTerminal(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Outt, args[0])
	cp.Emit(vm.Asgm, dest, values.C_OK)
}

func (cp *Compiler) btRune(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Itor, dest, args[0])
}

func (cp *Compiler) btSecret(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.MkSc, dest, args[0])
}

func (cp *Compiler) btSingleInList(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.InxL, dest, args[0], args[2])
}

func (cp *Compiler) btSingleInSet(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.InxS, dest, args[0], args[2])
}

func (cp *Compiler) btSingleInTuple(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.InxT, dest, args[0], args[2])
}

func (cp *Compiler) btSingleInType(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Inxt, dest, args[0], args[2])
}

func (cp *Compiler) btString(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Strx, dest, args[0])
}

func (cp *Compiler) btStructWith(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.WthZ, dest, args[0], args[2], cp.ReserveToken(tok))
}

func (cp *Compiler) btSubtractFloats(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Subf, dest, args[0], args[2])
}

func (cp *Compiler) btSubtractIntegers(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Subi, dest, args[0], args[2])
}

func (cp *Compiler) btSubtractSets(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.SubS, dest, args[0], args[2])
}

func (cp *Compiler) btTupleOfTuple(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Asgm, dest, args[0])
}

func (cp *Compiler) btTupleOfVarargs(tok *token.Token, dest uint32, args []uint32) {
	destWithArgs := append([]uint32{dest}, args...)
	cp.Emit(vm.CvTT, destWithArgs...)
}

func (cp *Compiler) btType(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Typx, dest, args[0])
}

func (cp *Compiler) btTypeOfTuple(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Asgm, dest, cp.TupleType)
}

func (cp *Compiler) btTypeUnion(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Typu, dest, args[0], args[2])
}

func (cp *Compiler) btTypesOfType(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Typs, dest, args[0])
}

func (cp *Compiler) btTypeWith(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Wtht, dest, args[0], args[1], cp.ReserveToken(tok))
}

func (cp *Compiler) btVarchar(tok *token.Token, dest uint32, args []uint32) {
	cp.Emit(vm.Varc, dest, args[0], cp.ReserveToken(tok))
}
