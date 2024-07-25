package service

import (
	"os"
	"path/filepath"
	"pipefish/source/text"
	"strconv"
	"testing"
)

func TestLiterals(t *testing.T) {
	tests := []testItem{
		{`"foo"`, `"foo"`},
		{"`foo`", `"foo"`},
		{`'q'`, `'q'`},
		{`true`, `true`},
		{`false`, `false`},
		{`42.0`, `42.00000000`},
		{`42`, `42`},
		{`0b101010`, `42`},
		{`0o52`, `42`},
		{`0x2A`, `42`},
		{`NULL`, `NULL`},
		{`OK`, `OK`},
	}
	runTest(t, "", tests, testValues)
}
func TestHardwiredOps(t *testing.T) {
	tests := []testItem{
		{`5.0 == 2.0`, `false`},
		{`5.0 != 2.0`, `true`},
		{`5 == 2`, `false`},
		{`5 != 2`, `true`},
		{`true != false`, `true`},
		{`"foo" == "foo"`, `true`},
		{`int == int`, `true`},
		{`struct == struct`, `true`},
		{`not true`, `false`},
		{`not false`, `true`},
		{`false and false`, `false`},
		{`true and false`, `false`},
		{`false and true`, `false`},
		{`true and true`, `true`},
		{`false or false`, `false`},
		{`true or false`, `true`},
		{`false or true`, `true`},
		{`true or true`, `true`},
		{`1, (2, 3)`, `(1, 2, 3)`},
	}
	runTest(t, "", tests, testValues)
}
func TestBuiltins(t *testing.T) {
	tests := []testItem{
		{`5.0 + 2.0`, `7.00000000`},
		{`5 + 2`, `7`},
		{`[1, 2] + [3, 4]`, `[1, 2, 3, 4]`},
		{`len(set(1, 2) + set(3, 4))`, `4`}, // We don't necessarily know which order the set will be serialized in.
		{`'h' + 'i'`, `"hi"`},
		{`'j' + "ello"`, `"jello"`},
		{`"jell" + 'o'`, `"jello"`},
		{`"jel" + "lo"`, `"jello"`},
		{`5.0 / 2.0`, `2.50000000`},
		{`5 / 2`, `2`},
		{`5.0 > 2.0`, `true`},
		{`5.0 >= 2.0`, `true`},
		{`5 > 2`, `true`},
		{`5 >= 2`, `true`},
		{`5.0 < 2.0`, `false`},
		{`5.0 <= 2.0`, `false`},
		{`5 < 2`, `false`},
		{`5 <= 2`, `false`},
		{`"foo"::2`, `"foo"::2`},
		{`5 % 2`, `1`},
		{`5.0 * 2.0`, `10.00000000`},
		{`5 * 2`, `10`},
		{`-5.0`, `-5.00000000`},
		{`-5`, `-5`},
		{`5.0 - 2.0`, `3.00000000`},
		{`5 - 2`, `3`},
		{`int/string`, `int/string`},

		{`codepoint 'A'`, `65`},
		{`first (tuple 1, 2, 3, 4, 5)`, `1`},
		{`float 5`, `5.00000000`},
		{`float "5"`, `5.00000000`},
		{`5 in [1, 2, 3]`, `false`},
		{`5 in [1, 2, 3, 4, 5]`, `true`},
		{`5 in set 1, 2, 3`, `false`},
		{`5 in set 1, 2, 3, 4, 5`, `true`},
		{`5 in tuple 1, 2, 3`, `false`},
		{`5 in tuple 1, 2, 3, 4, 5`, `true`},
		{`5 in string`, `false`},
		{`5 in struct`, `false`},
		{`5 in int`, `true`},
		{`5 in int?`, `true`},
		{`int 5.2`, `5`},
		{`int "5"`, `5`},
		{`last (tuple 1, 2, 3, 4, 5)`, `5`},
		{`len [1, 2, 3]`, `3`},
		{`len (map "a"::1, "b"::2, "c"::3)`, `3`},
		{`len set 1, 2, 3`, `3`},
		{`len "Angela"`, `6`},
		{`len tuple 1, 2, 3`, `3`},
		{`literal 3`, `"3"`},
		{`literal "foo"`, `"\"foo\""`},
		{`literal 'q'`, `"'q'"`},
		{`rune 65`, `'A'`},
		{`map "a"::1, "b"::2`, `map("a"::1, "b"::2)`},
		{`set 1, 2, 3`, `set(1, 2, 3)`},
		{`string 4.0`, `"4.00000000"`},
		{`string 4`, `"4"`},
		{`tuple 1`, `tuple(1)`},
		{`type true`, `bool`},
		{`type bool`, `type`},
		{`varchar(32)`, `varchar(32)`},
	}
	runTest(t, "", tests, testValues)
}
func TestIndexing(t *testing.T) {
	tests := []testItem{
		{`Color[4]`, `BLUE`},
		{`myType[5]`, `PURPLE`},
		{`DARK_BLUE[shade]`, `DARK`},
		{`myColor[shade]`, `LIGHT`},
		{`DARK_BLUE[KEY]`, `DARK`},
		{`myColor[KEY]`, `LIGHT`},
		{`DARK_BLUE[key]`, `DARK`},
		{`myColor[key]`, `LIGHT`},
		{`"Angela"[3]`, `'e'`},
		{`"Angela"[2::5]`, `"gel"`},
		{`myWord[2::5]`, `"gel"`},
		{`myList[2]`, `[5, 6]`},
		{`myList[myNumber]`, `[5, 6]`},
		{`Color[myNumber]`, `YELLOW`},
		{`myType[myNumber]`, `YELLOW`},
		{`myList[0::2]`, `[[1, 2], [3, 4]]`},
		{`myList[myIntPair]`, `[[1, 2], [3, 4]]`},
		{`("a", "b", "c", "d")[2]`, `"c"`},
		{`("a", "b", "c", "d")[myIntPair]`, `("a", "b")`},
		{`"Angela"[myIntPair]`, `"An"`},
		{`myWord[myIntPair]`, `"An"`},
		{`myPair[0]`, `"foo"`},
		{`myMap["a"]`, `[1, 2]`},
		{`foo myType, myNumber`, `YELLOW`},
		{`foo myMap, myIndex`, `[1, 2]`},
		{`foo myList, myNumber`, `[5, 6]`},
		{`foo myColor, key`, `LIGHT`},
		{`foo myPair, myOtherNumber`, `"bar"`},
		{`foo myWord, myNumber`, `'g'`},
	}
	runTest(t, "index_test.pf", tests, testValues)
}
func TestFunctionSyntaxCalls(t *testing.T) {
	tests := []testItem{
		{`foo "bing"`, `"foo bing"`},
		{`"bing" zort`, `"bing zort"`},
		{`"bing" troz "bong"`, `"bing troz bong"`},
		{`moo "bing" goo`, `"moo bing goo"`},
		{`flerp "bing" blerp "bong"`, `"flerp bing blerp bong"`},
		{`qux`, `"qux"`},
	}
	runTest(t, "function_syntax_test.pf", tests, testValues)
}
func TestVariablesAndConsts(t *testing.T) {
	tests := []testItem{
		{`A`, `42`},
		{`getB`, `99`},
		{`changeZ`, `OK`},
		{`v`, `true`},
		{`w`, `42`},
		{`y = NULL`, "OK"},
	}
	runTest(t, "variables_test.pf", tests, testValues)
}
func TestVariableAccessErrors(t *testing.T) {
	tests := []testItem{
		{`B`, `comp/ident/private`},
		{`A = 43`, `comp/assign/const`},
		{`z`, `comp/ident/private`},
		{`secretB`, `comp/private`},
		{`secretZ`, `comp/private`},
	}
	runTest(t, "variables_test.pf", tests, testCompilerErrors)
}
func TestUserDefinedTypes(t *testing.T) {
	tests := []testItem{
		{`Color[4]`, `BLUE`},
		{`DARK_BLUE`, `Tone with (shade::DARK, color::BLUE)`},
		{`type DARK_BLUE`, `Tone`},
		{`type RED`, `Color`},
		{`keys DARK_BLUE`, `[shade, color]`},
		{`DARK_BLUE[shade]`, `DARK`},
		{`DARK_BLUE[color]`, `BLUE`},
		{`GREEN == GREEN`, `true`},
		{`GREEN == ORANGE`, `false`},
		{`GREEN != GREEN`, `false`},
		{`GREEN != ORANGE`, `true`},
		{`PURPLE in MyType`, `true`},
		{`Tone/Shade/Color`, `MyType`},
		{`Tone(LIGHT, GREEN)`, `Tone with (shade::LIGHT, color::GREEN)`},
		{`Tone(LIGHT, GREEN) == DARK_BLUE`, `false`},
		{`Tone(LIGHT, GREEN) != DARK_BLUE`, `true`},
		{`troz DARK_BLUE`, `Tone with (shade::DARK, color::BLUE)`},
		{`foo 3, 5`, `8`},
	}
	runTest(t, "user_types_test.pf", tests, testValues)
}
func TestTypeAccessErrors(t *testing.T) {
	tests := []testItem{
		{`Pair 1, 2`, `comp/private`},
		{`Suit`, `comp/private/type`},
		{`HEARTS`, `comp/private/enum`},
		{`one`, `comp/private/label`},
	}
	runTest(t, "user_types_test.pf", tests, testCompilerErrors)
}
func TestOverloading(t *testing.T) {
	tests := []testItem{
		{`foo 42`, `"int"`},
		{`foo "zort"`, `"string"`},
		{`foo 42, true`, `"single?, bool"`},
		{`foo 42.0, true`, `"single?, bool"`},
		{`foo true, true`, `"bool, bool"`},
	}
	runTest(t, "overloading_test.pf", tests, testValues)
}
func TestPiping(t *testing.T) {
	tests := []testItem{
		{`["fee", "fie", "fo", "fum"] -> len`, `4`},
		{`["fee", "fie", "fo", "fum"] >> len`, `[3, 3, 2, 3]`},
		{`["fee", "fie", "fo", "fum"] -> that + ["foo"]`, `["fee", "fie", "fo", "fum", "foo"]`},
		{`["fee", "fie", "fo", "fum"] >> that + "!"`, `["fee!", "fie!", "fo!", "fum!"]`},
		{`[1, 2, 3, 4] ?> that % 2 == 0`, `[2, 4]`},
	}
	runTest(t, "", tests, testValues)
}
func TestForLoops(t *testing.T) {
	tests := []testItem{
		{`fib 8`, `21`},
		{`collatzA 42`, `1`},
		{`collatzB 42`, `1`},
		{`evens Color`, `[RED, YELLOW, BLUE]`},
		{`evens "Angela"`, `['A', 'g', 'l']`},
		{`evens myList`, `[PURPLE, GREEN, ORANGE]`},
		{`find GREEN, Color`, `3`},
		{`find GREEN, myList`, `2`},
		{`find GREEN, myMap`, `"c"`},
		{`allKeys myList`, `[0, 1, 2, 3, 4, 5]`},
		{`allKeys "Angela"`, `[0, 1, 2, 3, 4, 5]`},
		{`allValues myList`, `[PURPLE, BLUE, GREEN, YELLOW, ORANGE, RED]`},
		{`showRange 3, 8`, `[0::3, 1::4, 2::5, 3::6, 4::7]`},
		{`showRangeKeys 3, 8`, `[0, 1, 2, 3, 4]`},
		{`showRangeValues 3, 8`, `[3, 4, 5, 6, 7]`},
		{`showRange 8, 3`, `[0::7, 1::6, 2::5, 3::4, 4::3]`},
		{`showRangeKeys 8, 3`, `[0, 1, 2, 3, 4]`},
		{`showRangeValues 8, 3 `, `[7, 6, 5, 4, 3]`},
	}
	runTest(t, "for_loop_test.pf", tests, testValues)
}
func TestInnerFunctionsAndVariables(t *testing.T) {
	tests := []testItem{
		{`foo 42`, `42`},
		{`zort 3, 5`, `(25, 15)`},
		{`troz 2`, `2200`},
	}
	runTest(t, "inner_test.pf", tests, testValues)
}
func TestRecursion(t *testing.T) {
	tests := []testItem{
		{`fac 5`, `120`},
		{`power 3, 4`, `81`},
		{`inFac 5`, `120`},
	}
	runTest(t, "recursion_test.pf", tests, testValues)
}
func TestGocode(t *testing.T) {
	tests := []testItem{
		{`boo true`, `false`},
		{`foo 4.2`, `4.20000000`},
		{`ioo 42`, `84`},
		{`noo()`, `NULL`},
		{`roo 42`, `42`},
		{`soo "aardvark"`, `"aardvark"`},
		{`constructPerson "Doug", 42`, `Person with (name::"Doug", age::42)`},
		{`deconstructPerson Person "Doug", 42`, `("Doug", 42)`},
	}
	runTest(t, "gocode_test.pf", tests, testValues)
	// Tear down the .go and .so files.
	nameOfTestFile := "gocode_test.pf"
	currentDirectory, _ := os.Getwd()
	locationOfGocode, _ := filepath.Abs(currentDirectory + "/../../gocode 1.go")
	os.Remove(locationOfGocode)
	absolutePathToGoTestFile, _ := filepath.Abs(currentDirectory + "/../../rsc/go/")
	absoluteLocationOfPipefishTestFile, _ := filepath.Abs(currentDirectory + "/test-files/" + nameOfTestFile)
	file, _ := os.Stat(absoluteLocationOfPipefishTestFile)
	timestamp := file.ModTime().UnixMilli()
	goTestFile := absolutePathToGoTestFile + "/" + text.Flatten(absoluteLocationOfPipefishTestFile) + "_" + strconv.Itoa(int(timestamp)) + ".so"
	os.Remove(goTestFile)
}
func TestImports(t *testing.T) {
	tests := []testItem{
		{`qux.square 5`, `25`},
		{`type qux.Color`, `type`},
		{`qux.RED`, `qux.RED`},        // TODO --- this will break on improving literals.
		{`type qux.RED`, `qux.Color`}, //                "
		{`qux.RED in qux.Color`, `true`},
		{`qux.Color[4]`, `qux.BLUE`},
		{`qux.Person "John", 22`, `qux.Person with (name::"John", age::22)`},
		{`qux.Color[4]`, `qux.BLUE`},
		{`qux.Tone LIGHT, BLUE`, `qux.Tone with (shade::qux.LIGHT, color::qux.BLUE)`},
		{`qux.Time`, `Time`},
		{`troz.sumOfSquares 3, 4`, `25`},
	}
	runTest(t, "import_test.pf", tests, testValues)
}
func TestRef(t *testing.T) {
	tests := []testItem{
		{`x ++`, `OK`},
	}
	runTest(t, "ref_test.pf", tests, testValues)
}

func TestClones(t *testing.T) {
	tests := []testItem{
		{`5 apples + 3 apples`, `apples(8)`},
	}
	runTest(t, "ref_test.pf", tests, testValues)
}
