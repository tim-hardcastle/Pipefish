package parser_test

import (
	"errors"
	"testing"

	"github.com/tim-hardcastle/Pipefish/source/compiler"
	"github.com/tim-hardcastle/Pipefish/source/test_helper"
)
func TestParser(t *testing.T) {
	tests := []test_helper.TestItem{
		{`2 + 2`, `(2 + 2)`},
		{`2 + 3 * 4`, `(2 + (3 * 4))`},
		{`2 * 3 + 4`, `((2 * 3) + 4)`},
		{`-5`, `(- 5)`},
		{`-5 + 3`, `((- 5) + 3)`},
		{`a + b + c`, `((a + b) + c)`},
		{`a + b - c`, `((a + b) - c)`},
		{`a * b * c`, `((a * b) * c)`},
		{`a * b / c`, `((a * b) / c)`},
		{`a + b / c`, `(a + (b / c))`},
		{`a + b[c]`, `(a + (b[c]))`},
		{`-a * b`, `((- a) * b)`},
		{`true or true and true`, `(true or (true and true))`},
		{`true and true or true`, `((true and true) or true)`},
		{`not x and not y`, `((not x) and (not y))`},
		{`1 + 2, 3 + 4`, `((1 + 2) , (3 + 4))`},
		{`1 < 2 == 3 < 4`, `((1 < 2) == (3 < 4))`},
		{`1 == 2 and 3 == 4`, `((1 == 2) and (3 == 4))`},
		{`1 == 2 or 3 == 4`, `((1 == 2) or (3 == 4))`},
		{`1 < 2 != 3 < 4`, `((1 < 2) != (3 < 4))`},
		{`1 != 2 and 3 <= 4`, `((1 != 2) and (3 <= 4))`},
		{`1 >= 2 or 3 > 4`, `((1 >= 2) or (3 > 4))`},
		{`2 + 2 == 4 and true`, `(((2 + 2) == 4) and true)`},
		{`1 + 2 < 3 + 4`, `((1 + 2) < (3 + 4))`},
		{`1 * 2 > 3 mod 4`, `((1 * 2) > (3 mod 4))`},
		{`x = func(y) : y * y`, `(x = func (y any?) : (y * y))`},
		{`from a for i = 1; i < n; i + 1 : a + i`, `from a for (i = 1); (i < n); (i + 1) : (a + i)`},
		{`len x`, `(len x)`},
		{`len x, y`, `(len x, y)`},
		{`len(x), y`, `((len x) , y)`},
		{`x in Y, Z`, `(x in Y, Z)`},
		{`v + w :: x + y`, `((v + w) :: (x + y))`},
		{`x in int`, `(x in int)`},
		{`x -> y`, `(x -> y)`},
		{`[1, 2, 3]`, `[((1 , 2) , 3) ]`},
		{`'q'`, `'q'`},
		{`0.42`, `0.42`},
		{`valid(x)`, `(valid x)`},
		{`unwrap(x)`, `(unwrap x)`},
		{`break`, `break`},
		{`break 42`, `(break 42)`},
		{`continue`, `continue`},
		{`true : 42 ; else : "moo!"`, `((true : 42) ; (else : "moo!"))`},
	}
	test_helper.RunTest(t, "", tests, testParserOutput)
}
func TestFunctionSyntax(t *testing.T) {
	tests := []test_helper.TestItem{
		{`foo x`, `(foo x)`},
		{`x zort`, `(x zort)`},
		{`x troz y`, `(x troz y)`},
		{`moo x goo`, `(moo x goo)`},
		{`flerp x blerp y`, `(flerp x blerp y)`},
		{`qux`, `(qux)`},
	}
	test_helper.RunTest(t, "function_syntax_test.pf", tests, testParserOutput)
}
func TestTypeParser(t *testing.T) {
	tests := []test_helper.TestItem{
		{`string/int`, `(string / int)`},
		{`string&int`, `(string & int)`},
		{`string`, `string`},
		{`int?`, `int?`},
		{`int!`, `int!`},
		{`string{42}`, `string{42}`},
		{`string{42, 43}`, `string{42, 43}`},
		{`string{true}`, `string{true}`},
		{`string{4.2}`, `string{4.2}`},
		{`string{"foo"}`, `string{"foo"}`},
		{`string{'q'}`, `string{'q'}`},
		{`list{T type}`, `list{T type}`},
		{`pair{K, V type}`, `pair{K type, V type}`},
		{`list{string}`, `list{string}`},
		{`list{list{string}}`, `list{list{string}}`},
	}
	test_helper.RunTest(t, "", tests, testTypeParserOutput)
}

func TestChunkCallSignatures(t *testing.T) {
	tests := []test_helper.TestItem{
		{`qux (a int) :`, `qux (a int)`},
		{`qux (a int) -> int :`, `qux (a int) -> int`},
		{`qux (a int) -> int, string :`, `qux (a int) -> int, string`},
		{`qux (a int) -> int?, string :`, `qux (a int) -> int ?, string`},
		{`qux (a int, b string) :`, `qux (a int, b string)`},
		{`qux (a, b int) :`, `qux (a int, b int)`},
		{`qux (a, b) :`, `qux (a any ?, b any ?)`},
		{`qux (a) :`, `qux (a any ?)`},
		{`qux (a any ?) :`, `qux (a any ?)`},
		{`qux (a Z{5}) :`, `qux (a Z { 5 })`},
		{`qux (a Z{5, 6}) :`, `qux (a Z { 5 , 6 })`},
		{`qux (a int/string) :`, `qux (a int / string)`},
		{`qux (a Z{5, 6}, b int) :`, `qux (a Z { 5 , 6 }, b int)`},
		{`qux (a int/string, b int) :`, `qux (a int / string, b int)`},
		{`qux foo :`, `qux foo`},
		{`qux foo (a int) :`, `qux foo (a int)`},
		{`(a int) qux (b string) :`, `(a int) qux (b string)`},
		{`(a) qux (b string) :`, `(a any ?) qux (b string)`},
		{`(a int) qux (b) :`, `(a int) qux (b any ?)`},
		{`(a int) qux:`, `(a int) qux`},
		{`(a int, b string) qux :`, `(a int, b string) qux`},
		{`(a, b) qux :`, `(a any ?, b any ?) qux`},
		{`qux (a int) foo (b string) :`, `qux (a int) foo (b string)`},
		{`qux (a) foo (b string) :`, `qux (a any ?) foo (b string)`},
		{`qux (a int) foo (b) :`, `qux (a int) foo (b any ?)`},
		{`qux (a int) foo:`, `qux (a int) foo`},
		{`qux (a int, b string) foo :`, `qux (a int, b string) foo`},
		{`qux (a, b) foo :`, `qux (a any ?, b any ?) foo`},
	}
	test_helper.RunTest(t, "", tests, testChunkingSignatures)
}

func TestParserErrors(t *testing.T) {
	tests := []test_helper.TestItem{
		{`2 +`, `parse/prefix`},
		{`1 + )`, `parse/prefix`},
		{`1 + ]`, `parse/prefix`},
		{`len 1,`, `parse/prefix`},
		{`len(`, `parse/prefix`},
		{`len(1`, `parse/line`},
		{`troz.foo`, `parse/namespace/exist`},
		{`2 "aardvark"`, `parse/before/a`},
		{`func(x) wut`, `parse/colon`},
		{`from 1`, `parse/from`},
		{`(1))`, `parse/expected`},
	}
	test_helper.RunTest(t, "", tests, testParserErrors)
}

// The helper functions for testing the parser.

func testParserOutput(cp *compiler.Compiler, s string) (string, error) {
	astOfLine := cp.P.ParseLine("test", s)
	if cp.P.ErrorsExist() {
		return "", errors.New("compilation error")
	}
	return astOfLine.String(), nil
}

func testTypeParserOutput(cp *compiler.Compiler, s string) (string, error) {
	astOfLine := cp.P.ParseTypeFromString(s)
	if cp.P.ErrorsExist() {
		return "", errors.New("compilation error")
	}
	if astOfLine == nil {
		return "nil", nil
	}
	return astOfLine.String(), nil
}

func testChunkingSignatures(cp *compiler.Compiler, s string) (string, error) {
	result := cp.P.ChunkFunctionDeclarationFromString(s)
	if cp.P.ErrorsExist() {
		return cp.P.Common.Errors[0].ErrorId + " : " + cp.P.Common.Errors[0].Message, errors.New("compilation error")
	}
	return result, nil
}

func testParserErrors(cp *compiler.Compiler, s string) (string, error) {
	cp.P.ParseLine("test", s)
	if cp.P.ErrorsExist() {
		return cp.P.Common.Errors[0].ErrorId, nil
	} else {
		return "", errors.New("unexpected successful parsing")
	}
}
