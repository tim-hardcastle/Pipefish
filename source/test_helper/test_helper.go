package test_helper

import (
	"errors"
	"os"
	"path/filepath"
	"testing"

	"github.com/tim-hardcastle/Pipefish/source/compiler"
	"github.com/tim-hardcastle/Pipefish/source/err"
	"github.com/tim-hardcastle/Pipefish/source/initializer"
	"github.com/tim-hardcastle/Pipefish/source/settings"
	"github.com/tim-hardcastle/Pipefish/source/text"
	"github.com/tim-hardcastle/Pipefish/source/values"
	"github.com/tim-hardcastle/Pipefish/source/vm"
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
			cp, _ = initializer.StartCompilerFromFilepath(filepath.Join(wd, "../compiler/test-files/", filename), nil, map[string]*compiler.Compiler{})
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

// These functions say in what to extract information from a compiler, given
// a line to put in: do we want to look at the returned value; or what was posted
// to output; or the errors in the compiler.

func TestValues(cp *compiler.Compiler, s string) (string, error) {
	v := cp.Do(s)
	if cp.ErrorsExist() {
		return "", errors.New("failed to compile with code " + cp.P.Common.Errors[0].ErrorId)
	}
	return cp.Vm.Literal(v), nil
}

func TestOutput(cp *compiler.Compiler, s string) (string, error) {
	cp.Vm.OutHandle = vm.MakeCapturingOutHandler(cp.Vm)
	ok := cp.Do(s)
	if ok.T == values.ERROR {
		return "", errors.New("runtime error with code " + ok.V.(*err.Error).ErrorId)
	} 
	if cp.ErrorsExist() {
		return "", errors.New("failed to compile with code " + cp.P.Common.Errors[0].ErrorId)
	}
	return cp.Vm.OutHandle.(*vm.CapturingOutHandler).Dump(), nil
}

func TestCompilerErrors(cp *compiler.Compiler, s string) (string, error) {
	v := cp.Do(s)
	if !cp.ErrorsExist() {
		return "", errors.New("unexpected successful evaluation returned " + text.Emph(cp.Vm.Literal(v)))
	} else {
		return cp.P.Common.Errors[0].ErrorId, nil
	}
}

var Foo8Result = `We [0mcalled [0mfunction [0m[36m'foo'[0m [0m— [0mdefined [0mat [33mline 7 [0m— [0mwith [0m[36m'i = 8'[0m.
At [33mline 8 [0mwe [0mevaluated [0mthe [0mcondition [0m[36m'i mod 2 == 0'[0m. [0m
The [0mcondition [0msucceeded.
At [33mline 9 [0mfunction [0m[36m'foo'[0m [0mreturned [0m[36m"even"[0m.
`

var Foo13Result = `We [0mcalled [0mfunction [0m[36m'foo'[0m [0m— [0mdefined [0mat [33mline 7 [0m— [0mwith [0m[36m'i = 13'[0m.
At [33mline 8 [0mwe [0mevaluated [0mthe [0mcondition [0m[36m'i mod 2 == 0'[0m. [0m
The [0mcondition [0mfailed.
At [33mline 10 [0mwe [0mtook [0mthe [0m[36m'else'[0m [0mbranch.
At [33mline 11 [0mfunction [0m[36m'foo'[0m [0mreturned [0m[36m"odd"[0m.
`
