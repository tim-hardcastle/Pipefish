package initializer_test

// This largely repeats the tests in `vm_test.go` and `vm_compiler.go`, but only
// running one or two tests for each file initialized, since the initializer is
// what we're meant to be testing.

// This repetition is not entirely superfluous, as while the tests in those other
// packages would still ensure that the bits of the initializer being used were
// working, this doesn't show up as line coverage in VSCode and so I wouldn't know
// which those bits are.

import (
	"os"
	"path/filepath"
	"runtime"
	"strconv"
	"testing"

	"github.com/tim-hardcastle/Pipefish/source/test_helper"
	"github.com/tim-hardcastle/Pipefish/source/text"
)

func TestIndexing(t *testing.T) {
	tests := []test_helper.TestItem{
		{`DARK_BLUE[shade]`, `DARK`},
	}
	test_helper.RunTest(t, "index_test.pf", tests, test_helper.TestValues)
}
func TestFunctionSyntaxCalls(t *testing.T) {
	tests := []test_helper.TestItem{
		{`foo "bing"`, `"foo bing"`},
	}
	test_helper.RunTest(t, "function_call_test.pf", tests, test_helper.TestValues)
}
func TestVariablesAndConsts(t *testing.T) {
	tests := []test_helper.TestItem{
		{`A`, `42`},
	}
	test_helper.RunTest(t, "variables_test.pf", tests, test_helper.TestValues)
}
func TestUserDefinedTypes(t *testing.T) {
	tests := []test_helper.TestItem{
		{`Color(4)`, `BLUE`},
		{`DARK_BLUE`, `Tone with (shade::DARK, color::BLUE)`},
	}
	test_helper.RunTest(t, "user_types_test.pf", tests, test_helper.TestValues)
}
func TestOverloading(t *testing.T) {
	tests := []test_helper.TestItem{
		{`foo 42`, `"int"`},
	}
	test_helper.RunTest(t, "overloading_test.pf", tests, test_helper.TestValues)
}
func TestForLoops(t *testing.T) {
	tests := []test_helper.TestItem{
		{`fib 8`, `21`},
		{`x`, `10`},
	}
	test_helper.RunTest(t, "for_loop_test.pf", tests, test_helper.TestValues)
}
func TestInnerFunctionsAndVariables(t *testing.T) {
	tests := []test_helper.TestItem{
		{`foo 42`, `42`},
		{`zort 3, 5`, `(25, 15)`},
		{`troz 2`, `2200`},
	}
	test_helper.RunTest(t, "inner_test.pf", tests, test_helper.TestValues)
}
func TestRecursion(t *testing.T) {
	tests := []test_helper.TestItem{
		{`fac 5`, `120`},
		{`power 3, 4`, `81`},
		{`inFac 5`, `120`},
	}
	test_helper.RunTest(t, "recursion_test.pf", tests, test_helper.TestValues)
}
func TestImports(t *testing.T) {
	tests := []test_helper.TestItem{
		{`qux.square 5`, `25`},
		{`troz.sumOfSquares 3, 4`, `25`},
	}
	test_helper.RunTest(t, "import_test.pf", tests, test_helper.TestValues)
}
func TestExternals(t *testing.T) {
	tests := []test_helper.TestItem{
		{`zort.square 5`, `25`},
		{`zort.Time`, `Time`},
	}
	test_helper.RunTest(t, "external_test.pf", tests, test_helper.TestValues)
}
func TestRef(t *testing.T) {
	tests := []test_helper.TestItem{
		{`x ++`, `OK`},
	}
	test_helper.RunTest(t, "ref_test.pf", tests, test_helper.TestValues)
}
func TestClones(t *testing.T) {
	tests := []test_helper.TestItem{
		{`FloatClone(4.2) == FloatClone(4.2)`, `true`},
		{`5 apples + 3 apples`, `apples(8)`},
	}
	test_helper.RunTest(t, "clone_test.pf", tests, test_helper.TestValues)
}
func TestSnippet(t *testing.T) {
	tests := []test_helper.TestItem{}
	test_helper.RunTest(t, "snippets_test.pf", tests, test_helper.TestValues)
}
func TestInterface(t *testing.T) {
	tests := []test_helper.TestItem{
		{`BLERP in Addable`, `true`},
		{`Fnug(5) in Foobarable`, `false`},
	}
	test_helper.RunTest(t, "interface_test.pf", tests, test_helper.TestValues)
}
func TestFunctionSharing(t *testing.T) {
	tests := []test_helper.TestItem{
		{`C(1, 2) in Addable`, `true`},
	}
	test_helper.RunTest(t, "function_sharing_test.pf", tests, test_helper.TestValues)
}
func TestImperative(t *testing.T) {
	tests := []test_helper.TestItem{
		{`zort false`, `7`},
	}
	test_helper.RunTest(t, "imperative_test.pf", tests, test_helper.TestOutput)
}
func TestRuntimeTypecheck(t *testing.T) {
	tests := []test_helper.TestItem{
		{`EvenNumber 2`, `EvenNumber(2)`},
		{`EvenNumber 3`, `vm/typecheck/fail`},
	}
	test_helper.RunTest(t, "runtime_typecheck_test.pf", tests, test_helper.TestValues)
}
func TestParameterizedTypes(t *testing.T) {
	tests := []test_helper.TestItem{
		{`Z{5}(3) + Z{5}(4)`, `Z{5}(2)`},
	}
	test_helper.RunTest(t, "parameterized_type_test.pf", tests, test_helper.TestValues)
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
	test_helper.RunTest(t, "gocode_test.pf", tests, test_helper.TestValues)
	// Tear down the .go and .so files.
	nameOfTestFile := "gocode_test.pf"
	locationOfGocode, _ := filepath.Abs(currentDirectory + "/../../golang 1.go")
	os.Remove(locationOfGocode)
	absoluteLocationOfPipefishTestFile, _ := filepath.Abs(currentDirectory + "/../compiler/test-files/" + nameOfTestFile)
	file, _ := os.Stat(absoluteLocationOfPipefishTestFile)
	timestamp := file.ModTime().UnixMilli()
	goTestFile := absolutePathToRscGo + "/" + text.Flatten(absoluteLocationOfPipefishTestFile) + "_" + strconv.Itoa(int(timestamp)) + ".so"
	os.Remove(goTestFile)
	os.WriteFile(locationOfGoTimes, temp, 0644)
}
func TestLogging(t *testing.T) {
	tests := []test_helper.TestItem{
		{`foo 8`, test_helper.Foo8Result},
	}
	test_helper.RunTest(t, "logging_test.pf", tests, test_helper.TestOutput)
}
