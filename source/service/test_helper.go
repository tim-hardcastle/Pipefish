package service

import (
	"os"
	"pipefish/source/settings"
	"pipefish/source/text"
	"testing"
)

type TestItem struct {
	Input string
	Want  string
}

func testValues(cp *Compiler, s string) string {
	return cp.Describe(cp.Do(s))
}

func testCompilerErrors(cp *Compiler, s string) string {
	val := cp.Do(s)
	if !cp.P.ErrorsExist() {
		return "unexpected successful evaluation returned " + text.Emph(cp.vm.DefaultDescription(val))
	} else {
		return cp.P.Errors[0].ErrorId
	}
}

func RunTest(t *testing.T, filename string, tests []TestItem, F func(cp *Compiler, s string) string) {
	wd, _ := os.Getwd() // The working directory is the directory containing the package being tested.
	for _, test := range tests {
		if settings.SHOW_TESTS {
			println(text.BULLET + "Running test " + text.Emph(test.Input))
		}
		mc := BlankVm(nil, nil)
		var cp *Compiler
		var uP *Initializer
		if filename == "" {
			cp, uP = initializeFromFilepath(mc, "", text.Trim(wd), "", LF_NONE)
		} else {
			cp, uP = initializeFromFilepath(mc, wd+"/test-files/"+filename, text.Trim(wd), "", LF_NONE)
		}
		if uP.Parser.ErrorsExist() {
			println(uP.Parser.Errors[0].ErrorId)
			println("There were errors initializing the service : \n" + uP.Parser.ReturnErrors())

		}
		got := F(cp, test.Input)
		if !(test.Want == got) {
			if uP.Parser.ErrorsExist() {
				println("There were errors parsing the line: \n" + uP.Parser.ReturnErrors() + "\n")
			}
			t.Fatalf(`Test failed with input %s | Wanted : %s | Got : %s.`, test.Input, test.Want, got)
		}
	}
}
