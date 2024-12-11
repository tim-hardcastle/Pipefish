package test_helper

import (
	"os"
	"testing"

	"pipefish/source/pf"
	"pipefish/source/settings"
	"pipefish/source/text"
)

// Auxiliary types and functions for testing the parser and compiler.

type TestItem struct {
	Input string
	Want  string
}

func RunTest(t *testing.T, filename string, tests []TestItem, F func(cp *pf.Service, s string) (string, error)) {
	wd, _ := os.Getwd() // The working directory is the directory containing the package being tested.
	for _, test := range tests {
		if settings.SHOW_TESTS {
			println(text.BULLET + "Running test " + text.Emph(test.Input))
		}
		sv := pf.NewService()
		if filename == "" {
			sv.InitializeFromFilepath("")
		} else {
			sv.InitializeFromFilepath(wd+"/test-files/"+filename)
		}
		if sv.IsBroken() {
			r, _ := sv.GetErrorReport()
			t.Fatalf("There were errors initializing the service : \n" + r)
		}
		got, e := F(sv, test.Input)
		if e != nil {
			println(text.Red(test.Input))
			r, _ := sv.GetErrorReport()
			println("There were errors parsing the line: \n" + r + "\n")
		}
		if !(test.Want == got) {
			t.Fatalf(`Test failed with input %s | Wanted : %s | Got : %s.`, test.Input, test.Want, got)
		}
	}
}
