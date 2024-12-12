package test_helper

import (
	"os"
	"testing"

	"github.com/tim-hardcastle/Pipefish/source/compiler"
	"github.com/tim-hardcastle/Pipefish/source/initializer"
	"github.com/tim-hardcastle/Pipefish/source/settings"
	"github.com/tim-hardcastle/Pipefish/source/text"
)

// Auxiliary types and functions for testing the parser and compiler.

type TestItem struct {
	Input string
	Want  string
}

func RunTest(t *testing.T, filename string, tests []TestItem, F func(cp *compiler.Compiler, s string) (string, error)) {
	wd, _ := os.Getwd() // The working directory is the directory containing the package being tested.
	for _, test := range tests {
		if settings.SHOW_TESTS {
			println(text.BULLET + "Running test " + text.Emph(test.Input))
		}
		var cp *compiler.Compiler
		if filename == "" {
			cp, _ = initializer.StartCompilerFromFilepath(filename, nil, map[string]*compiler.Compiler{})
		} else {
			cp, _ = initializer.StartCompilerFromFilepath(wd+"/test-files/"+filename, nil, map[string]*compiler.Compiler{})
		}
		if cp.P.Common.IsBroken {
			r := cp.P.ReturnErrors()
			t.Fatalf("There were errors initializing the service : \n" + r)
		}
		got, e := F(cp, test.Input)
		if e != nil {
			println(text.Red(test.Input))
			r := cp.P.ReturnErrors()
			println("There were errors parsing the line: \n" + r + "\n")
		}
		if !(test.Want == got) {
			t.Fatalf(`Test failed with input %s | Wanted : %s | Got : %s.`, test.Input, test.Want, got)
		}
	}
}
