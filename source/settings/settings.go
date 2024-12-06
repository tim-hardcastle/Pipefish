// All this does is contain in one place the constants controlling which bits of the inner workings of the
// lexer/parser/compiler/are displayed to me for debugging purposes. In a release they must all be set to false
// except SUPPRESS_BUILTINS which may as well be left as true.

package settings

import (
	"os"
	"path/filepath"
	"runtime"
	"testing"

	"pipefish/source/dtypes"
)

// Note the first of these will be set equal to the second by the 'main' function if we're running under WinOS.
var MandatoryImports = []string{"rsc-pf/builtins.pf", "rsc-pf/world.pf", "rsc-pf/interfaces.pf"}
// And so the result of this function is OS-dependent.
func MandatoryImportSet() dtypes.Set[string] {
	return dtypes.MakeFromSlice(MandatoryImports)
}

var ThingsToIgnore = (dtypes.MakeFromSlice(MandatoryImports)).Add("rsc-pf/hub.pf").Add("Builtin constant").Add("rsc/worldlite.pf")

var StandardLibraries = dtypes.MakeFromSlice([]string{"fmt", "math", "path", "regexp", "strings", "time", "unicode"})

const (
	OMIT_BUILTINS      = false // If true then the file builtins.pf, world.pf, etc, will not be added to the service. Note that this means the hub won't work.
	IGNORE_BOILERPLATE = true  // Should usually be left true. Means that the first five flags below won't show instrumentation when compiling buitins.pf, world.pf, etc.

	FUNCTION_TO_PEEK = ""      // Shows the function table entry and function tree associated with the function named in the string, if non-empty.

	// These do what it sounds like.
	SHOW_LEXER             = false
	SHOW_RELEXER           = false
	SHOW_PARSER            = false // Note that this only applies to the REPL and not to code initialization. Use FUNCTION_TO_PEEK to look at the AST of a function.
	SHOW_VMM               = false
	SHOW_COMPILER          = false
	SHOW_COMPILER_COMMENTS = false
	SHOW_RUNTIME           = false // Note that this will show the hub's runtime too at present 'cos it can't tell the difference. TODO.
	SHOW_RUNTIME_VALUES    = false // Shows the contents of memory locations on the rhs of anything (i.e. not the dest).
	SHOW_XCALLS            = false

	SHOW_TESTS = false // Says whether the tests should say what is being tested, useful if one of them crashes and we don't know which.
)

var PipefishHomeDirectory string

func init() {
	if testing.Testing() {
		currentDirectory, _ := os.Getwd()
		absolutePath, _ := filepath.Abs(currentDirectory + "/../../")
		PipefishHomeDirectory = absolutePath + "/"
	} else {
		appDir, _ := filepath.Abs(filepath.Dir(os.Args[0]))
		PipefishHomeDirectory = appDir + "/"
	}
	if runtime.GOOS == "windows" { // This allows a cut-down version that doesn't require the plugins package.
		MandatoryImports = []string{"rsc-pf/builtins.pf", "rsc-pf/worldlite.pf", "rsc-pf/interfaces.pf"}
	}
}