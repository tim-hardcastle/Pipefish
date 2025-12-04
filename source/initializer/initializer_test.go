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

	"github.com/tim-hardcastle/pipefish/source/test_helper"
	"github.com/tim-hardcastle/pipefish/source/text"
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
func TestTypeInstances(t *testing.T) {
	tests := []test_helper.TestItem{
		{`Z{3}(2) in Z{3}`, `true`},
	}
	test_helper.RunTest(t, "type_instances_test.pf", tests, test_helper.TestValues)
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

// While most of the tests are just to establish which lines are covered, and so get the
// initializer to initialize the scripts used by the vm and compiler tests, the following
// tests check the internals of the initializer.
func TestSigChunking(t *testing.T) {
	tests := []test_helper.TestItem{
		{`qux :`, `qux`},
		{`qux () :`, `qux ()`},
		{`qux (i ... int) :`, `qux (i ... int)`},
		{`qux (a int) :`, `qux (a int)`},
		{`qux (a int) -> int :`, `qux (a int) -> int`},
		{`qux (a int) -> int, string :`, `qux (a int) -> int, string`},
		{`qux (a int) -> int?, string :`, `qux (a int) -> int?, string`},
		{`qux (a int, b string) :`, `qux (a int, b string)`},
		{`qux (a, b int) :`, `qux (a int, b int)`},
		{`qux (a, b) :`, `qux (a any?, b any?)`},
		{`qux (a) :`, `qux (a any?)`},
		{`qux (a any?) :`, `qux (a any?)`},
		{`qux (a Z{5}) :`, `qux (a Z{5})`},
		{`qux (a Z{5, 6}) :`, `qux (a Z{5, 6})`},
		{`qux (a int/string) :`, `qux (a int / string)`},
		{`qux (a Z{5, 6}, b int) :`, `qux (a Z{5, 6}, b int)`},
		{`qux (a int/string, b int) :`, `qux (a int / string, b int)`},
		{`qux foo :`, `qux foo`},
		{`qux foo (a int) :`, `qux foo (a int)`},
		{`(a int) qux (b string) :`, `(a int) qux (b string)`},
		{`(a) qux (b string) :`, `(a any?) qux (b string)`},
		{`(a int) qux (b) :`, `(a int) qux (b any?)`},
		{`(a int) qux:`, `(a int) qux`},
		{`(a int, b string) qux :`, `(a int, b string) qux`},
		{`(a, b) qux :`, `(a any?, b any?) qux`},
		{`qux (a int) foo (b string) :`, `qux (a int) foo (b string)`},
		{`qux (a) foo (b string) :`, `qux (a any?) foo (b string)`},
		{`qux (a int) foo (b) :`, `qux (a int) foo (b any?)`},
		{`qux (a int) foo:`, `qux (a int) foo`},
		{`qux (a int, b string) foo :`, `qux (a int, b string) foo`},
		{`qux (a, b) foo :`, `qux (a any?, b any?) foo`},
	}
	test_helper.RunInitializerTest(t, tests, test_helper.TestSigChunking)
}

func TestFunctionChunking(t *testing.T) {
	tests := []test_helper.TestItem{
		{"qux : 2 + 2", `qux : 3 tokens.`},
		{"qux : 2 + 2\n", `qux : 3 tokens.`},
		{"qux : \n\t2 + 2", `qux : 5 tokens.`},
		{"qux : \n\t2 + 2\nfoobar : 42", `qux : 5 tokens.`},
		{"qux : \n\t2 + 2\ngiven : 42", `qux : 5 tokens; given : 1 tokens.`},
		{"qux : \n\t2 + 2\ngiven : 42\n", `qux : 5 tokens; given : 1 tokens.`},
	}
	test_helper.RunInitializerTest(t, tests, test_helper.TestFunctionChunking)
}

func TestTypeChunking(t *testing.T) {
	tests := []test_helper.TestItem{
		{"Number = abstract int/float", `Number = abstract int/float`},
		{"Number = abstract int/float\n", `Number = abstract int/float`},
		{"Foo = abstract int/list{float/int}", `Foo = abstract int/list { float / int }`},
		{"UID = clone int", `UID = clone int`},
		{"UID = clone int\n", `UID = clone int`},
		{"UID = clone{s string} int", `UID = clone{s string} int`},
		{"UID = clone{s string} int : foo bar spong", `UID = clone{s string} int : 3 tokens.`},
		{"UID = clone int : foo bar spong", `UID = clone int : 3 tokens.`},
		{"Color = enum RED, GREEN, BLUE", `Color = enum RED, GREEN, BLUE`},
		{"Color = enum RED, GREEN, BLUE\n", `Color = enum RED, GREEN, BLUE`},
		{"Foo = interface : foo ()", `Foo = interface : 1 sigs.`},
		{"Foo = interface : foo () -> int", `Foo = interface : 1 sigs.`},
		{"Foo = interface : foo ()\n", `Foo = interface : 1 sigs.`},
		{"Foo = interface : foo () -> int\n", `Foo = interface : 1 sigs.`},
		{"Foo = interface : \n\tfoo ()", `Foo = interface : 1 sigs.`},
		{"Foo = interface : \n\tfoo () -> int", `Foo = interface : 1 sigs.`},
		{"Foo = interface : \n\tfoo ()\n", `Foo = interface : 1 sigs.`},
		{"Foo = interface : \n\tfoo () -> int\n", `Foo = interface : 1 sigs.`},
		{"Foo = interface : \n\tfoo ()\n\tbar ()", `Foo = interface : 2 sigs.`},
		{"Foo = interface : \n\tfoo (x int, y string) -> int\n\t(x) + (y) -> self", `Foo = interface : 2 sigs.`},
		{"make foo, bar", `make foo, bar`},
		{"make foo, bar\n", `make foo, bar`},
		{"Person = struct(name string, age int)", `Person = struct(name string, age int)`},
		{"Person = struct{i int}(name string, age int)", `Person = struct{i int}(name string, age int)`},
		{"Person = struct{i int}(name, age)", `Person = struct{i int}(name any?, age any?)`},
		{"Person = struct(name, age) : foo bar spong", `Person = struct(name any?, age any?) : 3 tokens.`},
	}
	test_helper.RunInitializerTest(t, tests, test_helper.TestTypeChunking)
}

func TestConstOrVarChunking(t *testing.T) {
	tests := []test_helper.TestItem{
		{"a int = 2 + 2", `a int = 3 tokens.`},
		{"a int = 2 + 2\n", `a int = 3 tokens.`},
		{"a = 2 + 2", `a = 3 tokens.`},
		{"a, b int = 2 + 2", `a int, b int = 3 tokens.`},
	}
	test_helper.RunInitializerTest(t, tests, test_helper.TestConstOrVarChunking)
}

func TestExternalOrImportChunking(t *testing.T) {
	tests := []test_helper.TestItem{
		{"foo::\"bar\"", `foo::"bar"`},
		{"foo::\"bar\"\n", `foo::"bar"`},
		{"\"bar\"", `"bar"`},
		{"\"bar\"\n", `"bar"`},
	}
	test_helper.RunInitializerTest(t, tests, test_helper.TestExternalOrImportChunking)
}
