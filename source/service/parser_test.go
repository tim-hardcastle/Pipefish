package service

// NOTE --- since even the most basic operations are now uploaded from 'builtins.pf',
// it's no longer possible to test the parser without starting up a pipefish service,
// and so the unit tests for the parser are here in the 'service' package.

import (
	"testing"
)

func TestParser(t *testing.T) {
	tests := []testItem{
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
		{`x = func(y) : y * y`, `(x = func (y single?) : (y * y))`},
		{`from a for i = 1; i < n; i + 1 : a + i`, `from a for (i = 1); (i < n); (i + 1) : (a + i)`},
		{`len x`, `(len x)`},
		{`len x, y`, `(len x, y)`},
		{`len(x), y`, `((len x) , y)`},
		{`x in Y, Z`, `(x in Y, Z)`},
		{`v + w :: x + y`, `((v + w) :: (x + y))`},
		{`1 + 2 zort`, `(1 + (2 zort))`},
	}
	runTest(t, "parser_test.pf", tests,
		func(cp *Compiler, s string) string {
			return cp.P.ParseLine("test", s).String()
		})
}

func TestParserErrors(t *testing.T) {
	tests := []testItem{
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
	runTest(t, "parser_test.pf", tests,
		func(cp *Compiler, s string) string {
			cp.P.ParseLine("test", s)
			if !cp.P.ErrorsExist() {
				return "unexpected successful parsing"
			} else {
				return cp.P.Errors[0].ErrorId
			}
		})
}
