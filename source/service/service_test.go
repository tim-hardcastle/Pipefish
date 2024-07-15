package service

import (
	"testing"
)

func TestHardwiredOps(t *testing.T) {
	tests := []testItem{
		{`5.0 == 2.0`, `false`},
		{`5.0 != 2.0`, `true`},
		{`5 == 2`, `false`},
		{`5 != 2`, `true`},
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
	}
	runTest(t, "parser_test.pf", tests,
		func(cp *Compiler, s string) string {
			return cp.Describe(cp.Do(s))
		})
}
func TestBuiltins(t *testing.T) {
	tests := []testItem{
		{`5.0 + 2.0`, `7.00000000`},
		{`5 + 2`, `7`},
		{`[1, 2] + [3, 4]`, `[1, 2, 3, 4]`},
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
		{`"foo"::2`, `foo::2`}, // TODO --- this will need changing to "foo"::2 when you improve the Literal function.
		{`5 % 2`, `1`},
		{`5.0 * 2.0`, `10.00000000`},
		{`5 * 2`, `10`},
		{`-5.0`, `-5.00000000`},
		{`-5`, `-5`},
		{`5.0 - 2.0`, `3.00000000`},
		{`5 - 2`, `3`},
		{`int/string`, `int/string`},

		{`codepoint 'A'`, `65`},
		{`float 5`, `5.00000000`},
		{`float "5"`, `5.00000000`},
		{`5 in [1, 2, 3]`, `false`},
		{`5 in [1, 2, 3, 4, 5]`, `true`},
		{`5 in set 1, 2, 3`, `false`},
		{`5 in set 1, 2, 3, 4, 5`, `true`},
		{`5 in tuple 1, 2, 3`, `false`},
		{`5 in tuple 1, 2, 3, 4, 5`, `true`},
		{`5 in string`, `false`},
		{`5 in int`, `true`},
		{`len [1, 2, 3]`, `3`},
		{`len TEST_MAP`, `3`},
		{`len "Angela"`, `6`},
		{`rune 65`, `'A'`},
		{`map "a"::1, "b"::2`, `map(a::1, b::2)`},
		{`set 1, 2, 3`, `set(1, 2, 3)`},
		{`string 4.0`, `"4.00000000"`},
		{`string 4`, `"4"`},
		{`type true`, `bool`},
		{`varchar(32)`, `varchar(32)`},
	}
	runTest(t, "parser_test.pf", tests,
		func(cp *Compiler, s string) string {
			return cp.Describe(cp.Do(s))
		})
}
