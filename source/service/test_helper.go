package service

import (
	"os"
	"pipefish/source/parser"
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
		return "unexpected successful evaluation returned " + text.Emph(cp.Vm.DefaultDescription(val))
	} else {
		return cp.P.Common.Errors[0].ErrorId
	}
}

func RunTest(t *testing.T, filename string, tests []TestItem, F func(cp *Compiler, s string) string) {
	wd, _ := os.Getwd() // The working directory is the directory containing the package being tested.
	for _, test := range tests {
		if settings.SHOW_TESTS {
			println(text.BULLET + "Running test " + text.Emph(test.Input))
		}
		mc := BlankVm(nil, nil)
		common := parser.NewCommonParserBindle()
		iz := newInitializer()
		iz.common = newCommonInitializerBindle()
		var cp *Compiler
		if filename == "" {
			cp = iz.initializeFromFilepath(mc, common, "", "")
		} else {
			cp = iz.initializeFromFilepath(mc, common, wd+"/test-files/"+filename, "")
		}
		if iz.ErrorsExist() {
			t.Fatalf("There were errors initializing the service : \n" + cp.P.ReturnErrors())
		}
		iz.makeFunctionTableAndGoMods()
		if iz.ErrorsExist() {
			t.Fatalf("There were errors initializing the service : \n" + cp.P.ReturnErrors())
		}
		iz.populateAbstractTypesAndMakeFunctionTrees()
		if iz.ErrorsExist() {
			t.Fatalf("There were errors initializing the service : \n" + cp.P.ReturnErrors())
		}
		iz.compileEverything()
		if iz.ErrorsExist() {
			t.Fatalf("There were errors initializing the service : \n" + cp.P.ReturnErrors())
		}
		iz.ResolveInterfaceBacktracks()
		got := F(cp, test.Input)
		if !(test.Want == got) {
			if iz.ErrorsExist() {
				println(text.Red(test.Input))
				println("There were errors parsing the line: \n" + cp.P.ReturnErrors() + "\n")
			}
			t.Fatalf(`Test failed with input %s | Wanted : %s | Got : %s.`, test.Input, test.Want, got)
		}
	}
}
