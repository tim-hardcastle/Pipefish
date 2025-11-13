package test_helper

import (
	"errors"
	"os"
	"path/filepath"
	"testing"

	"github.com/tim-hardcastle/Pipefish/source/compiler"
	"github.com/tim-hardcastle/Pipefish/source/err"
	"github.com/tim-hardcastle/Pipefish/source/initializer"
	"github.com/tim-hardcastle/Pipefish/source/parser"
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
			cp, _ = initializer.StartCompilerFromFilepath(filename, map[string]*compiler.Compiler{}, &values.Map{})
		} else {
			cp, _ = initializer.StartCompilerFromFilepath(filepath.Join(wd, "../compiler/test-files/", filename), map[string]*compiler.Compiler{}, &values.Map{})
		}
		if cp.P.Common.IsBroken {
			r := cp.P.ReturnErrors()
			t.Fatal("There were errors initializing the service : \n" + r)
		}
		got, e := F(cp, test.Input)
		if e != nil {
			println(text.Red(test.Input))
			r := cp.P.ReturnErrors()
			println("There were errors parsing the line: \n" + r + "\n")
		}
		if !(test.Want == got) {
			// if len(test.Want) != len(got) {
			// 	for i, ch := range test.Want {
			// 		println(ch, string(ch), got[i], string(got[i]))
			// 	}
			// }
			t.Fatalf("Test failed with input %s \nExp :\n%s\nGot :\n%s", test.Input, test.Want, got)
		}
	}
}

// NOTE: this is here to test some internal workings of the initializer. It only initializes
// a blank service.
func RunInitializerTest(t *testing.T, tests []TestItem, F func(iz *initializer.Initializer, s string) string) {
	iz := initializer.NewInitializer(initializer.NewCommonInitializerBindle(&values.Map{}, map[string]*compiler.Compiler{}))
	iz.ParseEverythingFromSourcecode(vm.BlankVm(), parser.NewCommonParserBindle(), compiler.NewCommonCompilerBindle(), "", "", "")
	for _, test := range tests {
		if settings.SHOW_TESTS {
			println(text.BULLET + "Running test " + text.Emph(test.Input))
		}
		got := F(iz, test.Input)
		if !(test.Want == got) {
			t.Fatalf("Test failed with input %s \nExp :\n%s\nGot :\n%s", test.Input, test.Want, got)
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
	if v.T == values.ERROR {
		return v.V.(*err.Error).ErrorId, nil
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
	return text.StripColors(cp.Vm.OutHandle.(*vm.CapturingOutHandler).Dump()), nil
}

func TestCompilerErrors(cp *compiler.Compiler, s string) (string, error) {
	v := cp.Do(s)
	if !cp.ErrorsExist() {
		return "", errors.New("unexpected successful evaluation returned " + text.Emph(cp.Vm.Literal(v)))
	} else {
		return cp.P.Common.Errors[0].ErrorId, nil
	}
}

// These functions test the internal workings of the initializer.
func TestSigChunking(iz *initializer.Initializer, s string) string {
	iz.P.PrimeWithString("test", s)
	sig, ok := iz.ChunkFunctionSignature()
	if !ok {
		return "Couldn't parse sig."
	}
	return sig.SigAsString()
}

func TestFunctionChunking(iz *initializer.Initializer, s string) string {
	iz.P.PrimeWithString("test", s)
	fn, ok := iz.ChunkFunction(false, false, "")
	if !ok {
		return "Couldn't parse function."
	}
	return initializer.SummaryString(fn)
}

func TestTypeChunking(iz *initializer.Initializer, s string) string {
	iz.P.PrimeWithString("test", s)
	ty, ok := iz.ChunkTypeDeclaration(false, "")
	if !ok {
		return "Couldn't parse type."
	}
	return initializer.SummaryString(ty)
}

func TestConstOrVarChunking(iz *initializer.Initializer, s string) string {
	iz.P.PrimeWithString("test", s)
	ty, ok := iz.ChunkConstOrVarDeclaration(false , false, "")
	if !ok {
		return "Couldn't parse assignment."
	}
	return initializer.SummaryString(ty)
}

func TestExternalOrImportChunking(iz *initializer.Initializer, s string) string {
	iz.P.PrimeWithString("test", s)
	ty, ok := iz.ChunkImportOrExternalDeclaration(false , false, "")
	if !ok {
		return "Couldn't parse import/external declaration."
	}
	return initializer.SummaryString(ty)
}

var Foo8Result = "We called function `foo` - defined at line 13 - with `i` = `8`.\n" +
        "At line 14 we evaluated the condition `i mod 2 == 0`. \n" + 
        "The condition succeeded.\n" +
        "At line 15 function `foo` returned \"even\".\n"

var Foo13Result = "We called function `foo` - defined at line 13 - with `i` = `13`.\n" +
        "At line 14 we evaluated the condition `i mod 2 == 0`. \n" +
        "The condition failed.\n" +
        "At line 16 we took the `else` branch.\n" +
        "At line 17 function `foo` returned \"odd\".\n"

var Qux8Result = "Log at line 7 : We're here.\n" +
        "Log at line 8 : We test to see if i (8) is even, which is true.\n" +
        "Log at line 9 : We return \"even\", because 8 is even.\n"

var Qux13Result = "Log at line 7 : We're here.\n" +
        "Log at line 8 : We test to see if i (13) is even, which is false.\n" +
        "Log at line 10 : Guess we're taking the 'else' branch.\n" +
        "Log at line 11 : And we return \"odd\".\n"

