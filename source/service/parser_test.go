package service

// NOTE --- since even the most basic operations are now uploaded from 'builtins.pf',
// it's no longer possible to test the parser without starting up a pipefish service,
// and so the unit tests for the parser are in the 'service' package.

import (
	"pipefish/source/text"

	"os"
	"testing"
)

func TestParse(t *testing.T) {
	tests := []struct {
		input string
		want  string
	}{
		{"2 + 2", "(2 + 2)"},
		{"len x", "(len x)"},
		{"2 + 3 * 4", "(2 + (3 * 4))"},
		{"2 * 3 + 4", "((2 * 3) + 4)"},
		{"x in Y, Z", "(x in Y, Z)"},
		{"-5", "(- 5)"},
		{"-5 + 3", "((- 5) + 3)"},
		{"true or true and true", "(true or (true and true))"},
		{"true and true or true", "((true and true) or true)"},
		{"1 + 2, 3 + 4", "((1 + 2) , (3 + 4))"},
		{"1 == 2 and 3 == 4", "((1 == 2) and (3 == 4))"},
		{"1 == 2 or 3 == 4", "((1 == 2) or (3 == 4))"},
		{"2 + 2 == 4 and true", "(((2 + 2) == 4) and true)"},
		{"x = func(y) : y * y", "(x = func (y single?) : (y * y))"},
	}
	mc := BlankVm(nil, nil)
	wd, _ := os.Getwd() // The working directory is the directory containing the package being tested.
	cp, uP := initializeFromFilepath(mc, "", text.Trim(wd))
	if uP.Parser.ErrorsExist() {
		println("There were errors initializing the service : \n" + uP.Parser.ReturnErrors() + "\n")
	}
	for _, test := range tests {
		got := cp.P.ParseLine("", test.input).String()
		if !(test.want == got) {
			if uP.Parser.ErrorsExist() {
				println("There were errors parsing the line: \n" + uP.Parser.ReturnErrors() + "\n")
			}
			t.Fatalf(`Test failed with input %s | Wanted : %s | Got : %s.`, test.input, test.want, got)
		}
	}
}
