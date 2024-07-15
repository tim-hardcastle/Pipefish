package service

import (
	"os"
	"pipefish/source/text"
	"testing"
)

type testItem struct {
	input string
	want  string
}

func runTest(t *testing.T, filename string, tests []testItem, F func(cp *Compiler, s string) string) {
	wd, _ := os.Getwd() // The working directory is the directory containing the package being tested.
	for _, test := range tests {
		mc := BlankVm(nil, nil)
		cp, uP := initializeFromFilepath(mc, wd+"/test-files/"+filename, text.Trim(wd))
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
