package service

import (
	"os"
	"pipefish/source/settings"
	"pipefish/source/text"
	"testing"
)

type testItem struct {
	input string
	want  string
}

func testValues(cp *Compiler, s string) string {
	return cp.Describe(cp.Do(s))
}

func testParserOutput(cp *Compiler, s string) string {
	return cp.P.ParseLine("test", s).String()
}

func testParserErrors(cp *Compiler, s string) string {
	cp.P.ParseLine("test", s)
	if !cp.P.ErrorsExist() {
		return "unexpected successful parsing"
	} else {
		return cp.P.Errors[0].ErrorId
	}
}

func testCompilerErrors(cp *Compiler, s string) string {
	val := cp.Do(s)
	if !cp.P.ErrorsExist() {
		return "unexpected successful evaluation returned " + text.Emph(cp.vm.Describe(val))
	} else {
		return cp.P.Errors[0].ErrorId
	}
}

func runTest(t *testing.T, filename string, tests []testItem, F func(cp *Compiler, s string) string) {
	wd, _ := os.Getwd() // The working directory is the directory containing the package being tested.
	for _, test := range tests {
		if settings.SHOW_TESTS {
			println(text.BULLET + "Running test " + text.Emph(test.input))
		}
		mc := BlankVm(nil, nil)
		var cp *Compiler
		var uP *Initializer
		if filename == "" {
			cp, uP = initializeFromFilepath(mc, "", text.Trim(wd))
		} else {
			cp, uP = initializeFromFilepath(mc, wd+"/test-files/"+filename, text.Trim(wd))
		}
		if uP.Parser.ErrorsExist() {
			println("There were errors initializing the service : \n" + uP.Parser.ReturnErrors() + "\n")
		}
		got := F(cp, test.input)
		if !(test.want == got) {
			if uP.Parser.ErrorsExist() {
				println("There were errors parsing the line: \n" + uP.Parser.ReturnErrors() + "\n")
			}
			t.Fatalf(`Test failed with input %s | Wanted : %s | Got : %s.`, test.input, test.want, got)
		}
	}
}
