package initializer_test

// This largely repeats the tests in `vm_test.go` and `vm_compiler.go`, but only 
// running one or twwo tests for each file initialized, since the initializer is 
// what we're meant to be testing.

// This repetition is not entirely superfluous, as while the tests in those other 
// packages would still ensure that the bits of the initialzer being used were 
// working, this doesn't show up as line coverage in VSCode and so I wouldn't know 
// which those bits are.

import (
	"errors"
	"os"
	"path/filepath"
	"runtime"
	"strconv"
	"testing"

	"github.com/tim-hardcastle/Pipefish/source/compiler"
	"github.com/tim-hardcastle/Pipefish/source/test_helper"
	"github.com/tim-hardcastle/Pipefish/source/text"
)

func TestIndexing(t *testing.T) {
	tests := []test_helper.TestItem{
		{`Color[4]`, `BLUE`},
		{`myType[5]`, `PURPLE`},
		{`DARK_BLUE[shade]`, `DARK`},
	}
	test_helper.RunTest(t, "index_test.pf", tests, testValues)
}
func TestFunctionSyntaxCalls(t *testing.T) {
	tests := []test_helper.TestItem{
		{`foo "bing"`, `"foo bing"`},
	}
	test_helper.RunTest(t, "function_call_test.pf", tests, testValues)
}
func TestVariablesAndConsts(t *testing.T) {
	tests := []test_helper.TestItem{
		{`A`, `42`},
	}
	test_helper.RunTest(t, "variables_test.pf", tests, testValues)
}
func TestUserDefinedTypes(t *testing.T) {
	tests := []test_helper.TestItem{
		{`Color[4]`, `BLUE`},
		{`DARK_BLUE`, `Tone with (shade::DARK, color::BLUE)`},
	}
	test_helper.RunTest(t, "user_types_test.pf", tests, testValues)
}
func TestOverloading(t *testing.T) {
	tests := []test_helper.TestItem{
		{`foo 42`, `"int"`},
	}
	test_helper.RunTest(t, "overloading_test.pf", tests, testValues)
}
func TestForLoops(t *testing.T) {
	tests := []test_helper.TestItem{
		{`fib 8`, `21`},
		{`x`, `10`},
	}
	test_helper.RunTest(t, "for_loop_test.pf", tests, testValues)
}
func TestInnerFunctionsAndVariables(t *testing.T) {
	tests := []test_helper.TestItem{
		{`foo 42`, `42`},
		{`zort 3, 5`, `(25, 15)`},
		{`troz 2`, `2200`},
	}
	test_helper.RunTest(t, "inner_test.pf", tests, testValues)
}
func TestRecursion(t *testing.T) {
	tests := []test_helper.TestItem{
		{`fac 5`, `120`},
		{`power 3, 4`, `81`},
		{`inFac 5`, `120`},
	}
	test_helper.RunTest(t, "recursion_test.pf", tests, testValues)
}
func TestImports(t *testing.T) {
	tests := []test_helper.TestItem{
		{`qux.square 5`, `25`},
		{`troz.sumOfSquares 3, 4`, `25`},
	}
	test_helper.RunTest(t, "import_test.pf", tests, testValues)
}
func TestExternals(t *testing.T) {
	tests := []test_helper.TestItem{
		{`zort.square 5`, `25`},
		{`zort.Time`, `Time`},
	}
	test_helper.RunTest(t, "external_test.pf", tests, testValues)
}
func TestRef(t *testing.T) {
	tests := []test_helper.TestItem{
		{`x ++`, `OK`},
	}
	test_helper.RunTest(t, "ref_test.pf", tests, testValues)
}
func TestClones(t *testing.T) {
	tests := []test_helper.TestItem{
		{`FloatClone(4.2) == FloatClone(4.2)`, `true`},
		{`5 apples + 3 apples`, `apples(8)`},
	}
	test_helper.RunTest(t, "clone_test.pf", tests, testValues)
}
func TestSnippet(t *testing.T) {
	tests := []test_helper.TestItem{
		{`makeSn 42`, `Foo with (text::"zort |x| troz", data::["zort ", 42, " troz"])`},
		{`post HTML --- zort |2 + 2| troz`, `OK`},
	}
	test_helper.RunTest(t, "snippets_test.pf", tests, testValues)
}
func TestInterface(t *testing.T) {
	tests := []test_helper.TestItem{
		{`BLERP in Addable`, `true`},
		{`Fnug(5) in Foobarable`, `false`},
	}
	test_helper.RunTest(t, "interface_test.pf", tests, testValues)
}
func TestFunctionSharing(t *testing.T) {
	tests := []test_helper.TestItem{
		{`C(1, 2) in Addable`, `true`},
	}
	test_helper.RunTest(t, "function_sharing_test.pf", tests, testValues)
}
func TestGocode(t *testing.T) {
	if runtime.GOOS == "windows" { // Windows can't use the plugin package.
		return
	}
	tests := []test_helper.TestItem{
		{`anyTest 42`, `42`},
		{`multiply 2, 3`, `6`},
	}
	currentDirectory, _ := os.Getwd()
	absolutePathToRscGo, _ := filepath.Abs(currentDirectory + "/../../pipefish-rsc/")
	locationOfGoTimes := absolutePathToRscGo + "/gotimes.dat"
	temp, err := os.ReadFile(locationOfGoTimes)
	if err != nil {
		println("Couldn't read gotimes")
		println("Error was", err.Error())
		panic("That's all folks!")
	}
	test_helper.RunTest(t, "gocode_test.pf", tests, testValues)
	// Tear down the .go and .so files.
	nameOfTestFile := "gocode_test.pf"
	locationOfGocode, _ := filepath.Abs(currentDirectory + "/../../golang 1.go")
	os.Remove(locationOfGocode)
	absoluteLocationOfPipefishTestFile, _ := filepath.Abs(currentDirectory + "/test-files/" + nameOfTestFile)
	file, _ := os.Stat(absoluteLocationOfPipefishTestFile)
	timestamp := file.ModTime().UnixMilli()
	goTestFile := absolutePathToRscGo + "/" + text.Flatten(absoluteLocationOfPipefishTestFile) + "_" + strconv.Itoa(int(timestamp)) + ".so"
	os.Remove(goTestFile)
	os.WriteFile(locationOfGoTimes, temp, 0644)
}

func testValues(cp *compiler.Compiler, s string) (string, error) {
	v := cp.Do(s)
	if cp.ErrorsExist() {
		return "", errors.New("failed to compile with code " + cp.P.Common.Errors[0].ErrorId)
	}
	return cp.Vm.Literal(v), nil
}

func testCompilerErrors(cp *compiler.Compiler, s string) (string, error) {
	v := cp.Do(s)
	if !cp.ErrorsExist() {
		return "", errors.New("unexpected successful evaluation returned " + text.Emph(cp.Vm.Literal(v)))
	} else {
		return cp.P.Common.Errors[0].ErrorId, nil
	}
}
