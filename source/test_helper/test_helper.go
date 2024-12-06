package test_helper

import (
	"os"
	"testing"

	"pipefish/source/initializer"
	"pipefish/source/parser"
	"pipefish/source/service"
	"pipefish/source/settings"
	"pipefish/source/text"
)

// Auxiliary types and functions for testing the parser and compiler.

type TestItem struct {
	Input string
	Want  string
}

func RunTest(t *testing.T, filename string, tests []TestItem, F func(cp *service.Compiler, s string) string) {
	wd, _ := os.Getwd() // The working directory is the directory containing the package being tested.
	for _, test := range tests {
		if settings.SHOW_TESTS {
			println(text.BULLET + "Running test " + text.Emph(test.Input))
		}
		mc := service.BlankVm(nil, nil)
		common := parser.NewCommonParserBindle()
		iz := initializer.NewInitializer()
		iz.Common = initializer.NewCommonInitializerBindle()
		var cp *service.Compiler
		if filename == "" {
			cp = iz.InitializeFromFilepath(mc, common, "", "")
		} else {
			cp = iz.InitializeFromFilepath(mc, common, wd+"/test-files/"+filename, "")
		}
		if iz.ErrorsExist() {
			t.Fatalf("There were errors initializing the service : \n" + cp.P.ReturnErrors())
		}
		iz.MakeFunctionTableAndGoMods()
		if iz.ErrorsExist() {
			t.Fatalf("There were errors initializing the service : \n" + cp.P.ReturnErrors())
		}
		iz.PopulateAbstractTypesAndMakeFunctionTrees()
		if iz.ErrorsExist() {
			t.Fatalf("There were errors initializing the service : \n" + cp.P.ReturnErrors())
		}
		iz.CompileEverything()
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
