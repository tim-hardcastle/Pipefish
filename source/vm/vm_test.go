package vm_test

import (
	"os"
	"path/filepath"
	"runtime"
	"strconv"
	"testing"

	"github.com/tim-hardcastle/pipefish/source/test_helper"
	"github.com/tim-hardcastle/pipefish/source/text"
)

func TestEof(t *testing.T) {
	tests := []test_helper.TestItem{
		{`troz 42`, `42`},
		{`zort 42`, `42`},
	}
	test_helper.RunTest(t, "eof_test.pf", tests, test_helper.TestValues)
}
func TestLiterals(t *testing.T) {
	tests := []test_helper.TestItem{
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
	test_helper.RunTest(t, "", tests, test_helper.TestValues)
}
func TestHardwiredOps(t *testing.T) {
	tests := []test_helper.TestItem{
		{`5.0 == 2.0`, `false`},
		{`5.0 != 2.0`, `true`},
		{`5 == 2`, `false`},
		{`5 != 2`, `true`},
		{`true != false`, `true`},
		{`"foo" == "foo"`, `true`},
		{`int == int`, `true`},
		{`struct == struct`, `true`},
		{`[1, 2, 3] == [1, 2, 3]`, `true`},
		{`[1, 2, 4] == [1, 2, 3]`, `false`},
		{`[1, 2, 3, 4] == [1, 2, 3]`, `false`},
		{`[1, 2, 3] == [1, 2, 3, 4]`, `false`},
		{`set(1, 2, 3) == set(1, 2, 3)`, `true`},
		{`set(1, 2, 4) == set(1, 2, 3)`, `false`},
		{`set(1, 2, 3, 4) == set(1, 2, 3)`, `false`},
		{`set(1, 2, 3) == set(1, 2, 3, 4)`, `false`},
		{`1::2 == 1::2`, `true`},
		{`1::2 == 2::2`, `false`},
		{`1::2 == 1::1`, `false`},
		{`map(1::2, 3::4) == map(1::2, 3::4)`, `true`},
		{`map(1::2, 3::4) == map(1::2, 4::4)`, `false`},
		{`map(1::2, 3::4) == map(1::2, 3::5)`, `false`},
		{`map(1::2, 3::4) == map(1::2, 3::4, 5::6)`, `false`},
		{`map(1::2, 3::4, 5::6) == map(1::2, 3::4)`, `false`},
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
	test_helper.RunTest(t, "", tests, test_helper.TestValues)
}

func TestConditionals(t *testing.T) {
	tests := []test_helper.TestItem{
		{`true : 5; else : 6`, `5`},
		{`false : 5; else : 6`, `6`},
		{`1 == 1 : 5; else : 6`, `5`},
		{`1 == 2 : 5; else : 6`, `6`},
	}
	test_helper.RunTest(t, "", tests, test_helper.TestValues)
}
func TestBuiltins(t *testing.T) {
	tests := []test_helper.TestItem{
		{`5.0 + 2.0`, `7.00000000`},
		{`5 + 2`, `7`},
		{`[1, 2] + [3, 4]`, `[1, 2, 3, 4]`},
		{`set(1, 2) + set(3, 4) == set(1, 2, 3, 4)`, `true`},
		{`'h' + 'i'`, `"hi"`},
		{`'j' + "ello"`, `"jello"`},
		{`"jell" + 'o'`, `"jello"`},
		{`"jel" + "lo"`, `"jello"`},
		{`5.0 / 2.0`, `2.50000000`},
		{`5 / 2`, `2.50000000`},
		{`5 / 2.0`, `2.50000000`},
		{`5.0 / 2`, `2.50000000`},
		{`5.0 > 2.0`, `true`},
		{`5.0 >= 2.0`, `true`},
		{`5 > 2`, `true`},
		{`5 >= 2`, `true`},
		{`5.0 < 2.0`, `false`},
		{`5.0 <= 2.0`, `false`},
		{`5 < 2`, `false`},
		{`5 <= 2`, `false`},
		{`"foo"::2`, `"foo"::2`},
		{`5 mod 2`, `1`},
		{`5.0 * 2.0`, `10.00000000`},
		{`5.0 * 2`, `10.00000000`},
		{`5 * 2.0`, `10.00000000`},
		{`5 * 2`, `10`},
		{`-5.0`, `-5.00000000`},
		{`-5`, `-5`},
		{`5.0 - 2.0`, `3.00000000`},
		{`5 - 2`, `3`},
		{`int/string`, `int/string`},
		{`[1, 2, 3] ...`, `(1, 2, 3)`},
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
	}
	test_helper.RunTest(t, "", tests, test_helper.TestValues)
}

func TestTuples(t *testing.T) {
	tests := []test_helper.TestItem{
		{`(1, 2), 3`, `(1, 2, 3)`},
		{`1, (2, 3)`, `(1, 2, 3)`},
		{`(1, 2), (3, 4)`, `(1, 2, 3, 4)`},
		{`()`, `()`},
		{`type tuple "foo", "bar"`, `tuple`},
		{`len tuple "foo", "bar"`, `2`},
		{`1 in tuple(1, 2)`, `true`},
		{`string(X)`, `"(2, 3)"`},
		//{`len tuple 1, X`, `3`},
		{`foo 1, X`, `3`},
		{`foo X, 1`, `3`},
		{`foo W, Z`, `8`},
	}
	test_helper.RunTest(t, "tuples_test.pf", tests, test_helper.TestValues)
}

func TestIndexing(t *testing.T) {
	tests := []test_helper.TestItem{
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
		{`myList[0::2]`, `[[1, 2], [3, 4]]`},
		{`myList[myIntPair]`, `[[1, 2], [3, 4]]`},
		{`("a", "b", "c", "d")[2]`, `"c"`},
		{`("a", "b", "c", "d")[myIntPair]`, `("a", "b")`},
		{`"Angela"[myIntPair]`, `"An"`},
		{`myWord[myIntPair]`, `"An"`},
		{`myPair[0]`, `"foo"`},
		{`myMap["a"]`, `[1, 2]`},
		{`foo myMap, myIndex`, `[1, 2]`},
		{`foo myList, myNumber`, `[5, 6]`},
		{`foo myColor, key`, `LIGHT`},
		{`foo myPair, myOtherNumber`, `"bar"`},
		{`foo myWord, myNumber`, `'g'`},
	}
	test_helper.RunTest(t, "index_test.pf", tests, test_helper.TestValues)
}
func TestFunctionSyntaxCalls(t *testing.T) {
	tests := []test_helper.TestItem{
		{`foo "bing"`, `"foo bing"`},
		{`"bing" zort`, `"bing zort"`},
		{`"bing" troz "bong"`, `"bing troz bong"`},
		{`moo "bing" goo`, `"moo bing goo"`},
		{`flerp "bing" blerp "bong"`, `"flerp bing blerp bong"`},
		{`qux`, `"qux"`},
		{`foo p 7`, `"foo p"`},
		{`foo q 7`, `"foo q"`},
	}
	test_helper.RunTest(t, "function_call_test.pf", tests, test_helper.TestValues)
}
func TestVariablesAndConsts(t *testing.T) {
	tests := []test_helper.TestItem{
		{`A`, `42`},
		{`getB`, `99`},
		{`changeZ`, `OK`},
		{`v`, `true`},
		{`w`, `42`},
		{`y = NULL`, "OK"},
	}
	test_helper.RunTest(t, "variables_test.pf", tests, test_helper.TestValues)
}
func TestVariableAccessErrors(t *testing.T) {
	tests := []test_helper.TestItem{
		{`B`, `comp/ident/private`},
		{`A = 43`, `comp/assign/const`},
		{`z`, `comp/ident/private`},
		{`secretB`, `comp/private`},
		{`secretZ`, `comp/private`},
	}
	test_helper.RunTest(t, "variables_test.pf", tests, test_helper.TestCompilerErrors)
}
func TestUserDefinedTypes(t *testing.T) {
	tests := []test_helper.TestItem{
		{`Tone with (shade::LIGHT, color::RED)`, `Tone with (shade::LIGHT, color::RED)`},
		{`Color(4)`, `BLUE`},
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
	test_helper.RunTest(t, "user_types_test.pf", tests, test_helper.TestValues)
}
func TestTypeAccessErrors(t *testing.T) {
	tests := []test_helper.TestItem{
		{`Pair 1, 2`, `comp/private`},
		{`Suit`, `comp/private/type`},
		{`HEARTS`, `comp/ident/private`},
		{`one`, `comp/ident/private`},
	}
	test_helper.RunTest(t, "user_types_test.pf", tests, test_helper.TestCompilerErrors)
}
func TestOverloading(t *testing.T) {
	tests := []test_helper.TestItem{
		{`foo 42`, `"int"`},
		{`foo "zort"`, `"string"`},
		{`foo 42, true`, `"any?, bool"`},
		{`foo 42.0, true`, `"any?, bool"`},
		{`foo true, true`, `"bool, bool"`},
	}
	test_helper.RunTest(t, "overloading_test.pf", tests, test_helper.TestValues)
}
func TestPiping(t *testing.T) {
	tests := []test_helper.TestItem{
		{`["fee", "fie", "fo", "fum"] -> len`, `4`},
		{`["fee", "fie", "fo", "fum"] >> len`, `[3, 3, 2, 3]`},
		{`["fee", "fie", "fo", "fum"] -> that + ["foo"]`, `["fee", "fie", "fo", "fum", "foo"]`},
		{`["fee", "fie", "fo", "fum"] >> that + "!"`, `["fee!", "fie!", "fo!", "fum!"]`},
		{`[1, 2, 3, 4] ?> that mod 2 == 0`, `[2, 4]`},
	}
	test_helper.RunTest(t, "", tests, test_helper.TestValues)
}
func TestForLoops(t *testing.T) {
	tests := []test_helper.TestItem{
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
		{`x`, `10`},
	}
	test_helper.RunTest(t, "for_loop_test.pf", tests, test_helper.TestValues)
}
func TestInnerFunctionsAndVariables(t *testing.T) {
	tests := []test_helper.TestItem{
		{`foo 42`, `42`},
		{`zort 3, 5`, `(25, 15)`},
		{`troz 2`, `2200`},
	}
	test_helper.RunTest(t, "inner_test.pf", tests, test_helper.TestValues)
}
func TestRecursion(t *testing.T) {
	tests := []test_helper.TestItem{
		{`fac 5`, `120`},
		{`power 3, 4`, `81`},
		{`inFac 5`, `120`},
	}
	test_helper.RunTest(t, "recursion_test.pf", tests, test_helper.TestValues)
}
func TestImports(t *testing.T) {
	tests := []test_helper.TestItem{
		{`qux.square 5`, `25`},
		{`type qux.Color`, `type`},
		{`qux.RED`, `qux.RED`},
		{`type qux.RED`, `qux.Color`},
		{`qux.RED in qux.Color`, `true`},
		{`qux.Color(4)`, `qux.BLUE`},
		{`qux.Person "John", 22`, `qux.Person with (name::"John", age::22)`},
		{`qux.Tone LIGHT, BLUE`, `qux.Tone with (shade::qux.LIGHT, color::qux.BLUE)`},
		{`qux.Time`, `Time`},
		{`troz.sumOfSquares 3, 4`, `25`},
	}
	test_helper.RunTest(t, "import_test.pf", tests, test_helper.TestValues)
}
func TestExternals(t *testing.T) {
	tests := []test_helper.TestItem{
		{`zort.square 5`, `25`},
		{`type zort.Color`, `type`},
		{`zort.RED`, `zort.RED`},
		{`type zort.RED`, `zort.Color`},
		{`zort.RED in zort.Color`, `true`},
		{`zort.Color(4)`, `zort.BLUE`},
		{`zort.Person "John", 22`, `zort.Person with (name::"John", age::22)`},
		{`zort.Tone LIGHT, BLUE`, `zort.Tone with (shade::zort.LIGHT, color::zort.BLUE)`},
		{`zort.Qux 5`, `zort.Qux(5)`},
		{`zort.Time`, `Time`},
	}
	test_helper.RunTest(t, "external_test.pf", tests, test_helper.TestValues)
}

func TestRef(t *testing.T) {
	tests := []test_helper.TestItem{
		{`x ++`, `OK`},
	}
	test_helper.RunTest(t, "ref_test.pf", tests, test_helper.TestValues)
}
func TestClones(t *testing.T) {
	tests := []test_helper.TestItem{
		{`FloatClone(4.2) == FloatClone(4.2)`, `true`},
		{`FloatClone(4.2) == FloatClone(9.9)`, `false`},
		{`IntClone(42) == IntClone(42)`, `true`},
		{`IntClone(42) == IntClone(99)`, `false`},
		{`ListClone([1, 2]) == ListClone([1, 2])`, `true`},
		{`ListClone([1, 2]) == ListClone([1, 3])`, `false`},
		{`ListClone([1, 2]) == ListClone([1, 2, 3])`, `false`},
		{`MapClone(map(1::2, 3::4)) == MapClone(map(3::4, 1::2))`, `true`},
		{`MapClone(map(1::2, 3::4)) == MapClone(map(1::2, 3::5))`, `false`},
		{`MapClone(map(1::2, 3::4)) == MapClone(map(1::2, 3::4, 5::6))`, `false`},
		{`PairClone(1::2) == PairClone(1::2)`, `true`},
		{`PairClone(1::2) == PairClone(2::2)`, `false`},
		{`PairClone(1::2) == PairClone(1::1)`, `false`},
		{`RuneClone('a') == RuneClone('a')`, `true`},
		{`RuneClone('a') == RuneClone('z')`, `false`},
		{`SetClone(set(1, 2)) == SetClone(set(1, 2))`, `true`},
		{`SetClone(set(1, 2)) == SetClone(set(1, 3))`, `false`},
		{`SetClone(set(1, 2)) == SetClone(set(1, 2, 3))`, `false`},
		{`StringClone("aardvark") == StringClone("aardvark")`, `true`},
		{`StringClone("aardvark") == StringClone("zebra")`, `false`},
		{`5 apples + 3 apples`, `apples(8)`},
	}
	test_helper.RunTest(t, "clone_test.pf", tests, test_helper.TestValues)
}
func TestSnippet(t *testing.T) {
	tests := []test_helper.TestItem{
		{`(qux 5)[0]`, `"foo "`},
		{`(qux 5)[1]`, `10`},
		{`(qux 5)[2]`, `" bar"`},
	}
	test_helper.RunTest(t, "snippets_test.pf", tests, test_helper.TestValues)
}
func TestInterface(t *testing.T) {
	tests := []test_helper.TestItem{
		{`BLERP in Addable`, `true`},
		{`Fnug(5) in Addable`, `true`},
		{`ZORT in Foobarable`, `true`},
		{`true in Addable`, `false`},
		{`Fnug(5) in Foobarable`, `false`},
		{`Grunt(1, Derp(5))`, `Grunt with (flerp::1, glerp::Derp with (blerp::5))`},
		{`Derp(5) in Zort`, `true`},
		{`Derp(5) in Spoitable`, `true`},
		{`xuq Derp(5)`, `Derp with (blerp::5)`},
		{`respoit Derp(5)`, `Derp with (blerp::5)`},
	}
	test_helper.RunTest(t, "interface_test.pf", tests, test_helper.TestValues)
}
func TestFunctionSharing(t *testing.T) {
	tests := []test_helper.TestItem{
		{`C(1, 2) in Addable`, `true`},
		{`C(1, 2) in summer.Addable`, `true`},
		{`C(1, 2) in summer.Rotatable`, `true`},
		{`summer.sum [C(1, 2), C(3, 4), C(5, 6)]`, `C with (real::9, imaginary::12)`},
		{`summer.rotAll [C(1, 2), C(3, 4)]`, `[C with (real::-2, imaginary::1), C with (real::-4, imaginary::3)]`},
	}
	test_helper.RunTest(t, "function_sharing_test.pf", tests, test_helper.TestValues)
}
func TestImperative(t *testing.T) {
	tests := []test_helper.TestItem{
		{`zort false`, `7`},
		{`zort true`, `6`},
		{`qux false`, `5`},
		{`qux true`, `6`},
	}
	test_helper.RunTest(t, "imperative_test.pf", tests, test_helper.TestOutput)
}
func TestRuntimeTypecheck(t *testing.T) {
	tests := []test_helper.TestItem{
		{`EvenNumber 2`, `EvenNumber(2)`},
		{`EvenNumber 3`, `vm/typecheck/fail`},
		{`Person "Doug", 42`, `Person with (name::"Doug", age::42)`},
		{`Person "", 42`, "vm/typecheck/fail"},
		{`Person "Doug", -99`, "vm/typecheck/fail"},
	}
	test_helper.RunTest(t, "runtime_typecheck_test.pf", tests, test_helper.TestValues)
}
func TestParameterizedTypes(t *testing.T) {
	tests := []test_helper.TestItem{
		{`Z{12}`, `Z{12}`},
		{`Z{5} == Z{12}`, `false`},
		{`Z{5}(3) + Z{5}(4)`, `Z{5}(2)`},
		{`Vec{3}[1, 2, 3] + Vec{3}[4, 5, 6]`, `Vec{3}[5, 7, 9]`},
		{`Money{USD} == Money{EURO}`, `false`},
		{`list{int}[1, 2]`, `list{int}[1, 2]`},
		{`list{int}[1, 2] + list{int}[3, 4]`, `list{int}[1, 2, 3, 4]`},
		{`Z{5}(4) in Z{5}`, `true`},
		{`Z{5}(4) in Z{12}`, `false`},
		{`clones{int}`, `clones{int}`},
	}
	test_helper.RunTest(t, "parameterized_type_test.pf", tests, test_helper.TestValues)
}
func TestTypeInstances(t *testing.T) {
	tests := []test_helper.TestItem{
		{`Z{3}(2) in Z{3}`, `true`},
		{`Z{5}(2) in Z{5}`, `true`},
		{`Z{7}(2) in Z{7}`, `true`},
		{`Z{12}(2) in Z{12}`, `true`},
	}
	test_helper.RunTest(t, "type_instances_test.pf", tests, test_helper.TestValues)
}
func TestReflection(t *testing.T) {
	tests := []test_helper.TestItem{
		{`reflect.isStruct Varchar{8}`, `false`},
		{`reflect.isClone Varchar{8}`, `true`},
		{`reflect.parent Varchar{8}`, `string`},
		{`reflect.parameterTypes Varchar{8}`, `[int]`},
		{`reflect.parameterValues Varchar{8}`, `[8]`},
	}
	test_helper.RunTest(t, "reflect_test.pf", tests, test_helper.TestValues)
}
func TestGocode(t *testing.T) {
	if runtime.GOOS == "windows" { // Windows can't use the plugin package.
		return
	}
	tests := []test_helper.TestItem{
		{`anyTest 42`, `42`},
		{`variadicAnyTest 2, 42, true, "foo", 9.9`, `"foo"`},
		{`boolTest true`, `false`},
		{`float 4.2`, `4.20000000`},
		{`intTest 42`, `84`},
		{`listTest([1, 2])`, `[1, 2]`},
		{`mapTest(map(1::2, 3::4)) == map(1::2, 3::4)`, `true`},
		{`pairTest(1::2) == 1::2`, `true`},
		{`runeTest('q') == 'q'`, `true`},
		{`setTest(set(1, 2)) == set(1, 2)`, `true`},
		{`stringTest "aardvark"`, `"aardvark"`},
		{`tupleTest(tuple(1, 2)) == [1, 2]`, `true`},
		{`variadicTest(2, "fee", "fie", "fo", "fum") == "fo"`, `true`},
		{`enumTest BLUE`, `BLUE`},
		{`intCloneTest IntClone(5)`, `IntClone(5)`},
		{`constructPerson "Doug", 42`, `Person with (name::"Doug", age::42)`},
		{`deconstructPerson Person "Doug", 42`, `("Doug", 42)`},
		{`floatCloneTest(FloatClone(4.2)) == FloatClone(4.2)`, `true`},
		{`intCloneTest(IntClone(42)) == IntClone(42)`, `true`},
		{`listCloneTest(ListClone([1, 2])) == ListClone([1, 2])`, `true`},
		{`mapCloneTest(MapClone(map(1::2, 3::4))) == MapClone(map(1::2, 3::4))`, `true`},
		{`pairCloneTest(PairClone(1::2)) == PairClone(1::2)`, `true`},
		{`runeCloneTest(RuneClone('q')) == RuneClone('q')`, `true`},
		{`setCloneTest(SetClone(set(1, 2))) == SetClone(set(1, 2))`, `true`},
		{`stringCloneTest(StringClone("zort")) == StringClone("zort")`, `true`},
		{`commandTest`, `OK`},
		{`applyFunction(2, (func(i int) : 2 * i))`, `4`},
		{`type(multiplyBy(3))`, `func`},
		{`getType(3)`, `func`},
		{`multiply 2, 3`, `6`},
	}
	currentDirectory, _ := os.Getwd()
	absolutePathToRscGo, _ := filepath.Abs(currentDirectory + "/../../pipefish-rsc/")
	locationOfGoTimes := absolutePathToRscGo + "/gotimes.dat"
	temp, err := os.ReadFile(locationOfGoTimes)
	if err != nil {
		println("Couldn't read gotimes")
		println("Error was", err.Error())
		panic("That's all folks!")
	}
	test_helper.RunTest(t, "gocode_test.pf", tests, test_helper.TestValues)
	// Tear down the .go and .so files.
	nameOfTestFile := "gocode_test.pf"
	locationOfGocode, _ := filepath.Abs(currentDirectory + "/../../golang 1.go")
	os.Remove(locationOfGocode)
	absoluteLocationOfPipefishTestFile, _ := filepath.Abs(currentDirectory + "/../compiler/test-files/" + nameOfTestFile)
	file, _ := os.Stat(absoluteLocationOfPipefishTestFile)
	timestamp := file.ModTime().UnixMilli()
	goTestFile := absolutePathToRscGo + "/" + text.Flatten(absoluteLocationOfPipefishTestFile) + "_" + strconv.Itoa(int(timestamp)) + ".so"
	os.Remove(goTestFile)
	os.WriteFile(locationOfGoTimes, temp, 0644)
}

func TestLogging(t *testing.T) {
	tests := []test_helper.TestItem{
		{`foo 8`, test_helper.Foo8Result},
		{`foo 13`, test_helper.Foo13Result},
		{`qux 8`, test_helper.Qux8Result},
		{`qux 13`, test_helper.Qux13Result},
	}
	test_helper.RunTest(t, "logging_test.pf", tests, test_helper.TestOutput)
}
