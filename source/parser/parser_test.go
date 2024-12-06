package parser_test

import (
    "testing"

	"pipefish/source/parser"
	"pipefish/source/service"
	"pipefish/source/test_helper"
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
		{`1 * 2 > 3 % 4`, `((1 * 2) > (3 % 4))`},
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
func testParserOutput(cp *service.Compiler, s string) string {
	p := cp.P
	if p.ErrorsExist() {
		panic("That shouldn't happen.")
	}
	return cp.P.ParseLine("test", s).String()
}

func testParserErrors(cp *service.Compiler, s string) string {
	cp.P.ParseLine("test", s)
	if !cp.P.ErrorsExist() {
		return "unexpected successful parsing"
	} else {
		return cp.P.Common.Errors[0].ErrorId
	}
}

func dummyFunction(p parser.Parser) {

}
