// All this does is contain in one place the constants controlling which bits of the inner workings of the
// lexer/parser/compiler/are displayed to me for debugging purposes. In a release they must all be set to false
// except SUPPRESS_BUILTINS which may as well be left as true.

package settings

import (
	"os"
	"path/filepath"
	"runtime"
	"testing"

	"github.com/tim-hardcastle/Pipefish/source/dtypes"
)

// This can be changed during initialization.
var MandatoryImports = []string{"rsc-pf/builtins.pf", "rsc-pf/world.pf", "rsc-pf/interfaces.pf"}

// And so this is a function. TODO --- init it instead.
func MandatoryImportSet() dtypes.Set[string] {
	return dtypes.MakeFromSlice(MandatoryImports)
}

var ThingsToIgnore = (dtypes.MakeFromSlice(MandatoryImports)).
Add("rsc-pf/hub.pf").Add("Builtin constant").
Add("rsc-pf/worldlite.pf").Add("user/themes.pf")

// This is replicated in the hub and any changes made here must be reflected there. TODO --- don't.
var StandardLibraries = dtypes.MakeFromSlice([]string{"path/filepath", "fmt", "math", "path", "reflect", "regexp", "sql", "strings", "time", "unicode"})

const (
	OMIT_BUILTINS      = false // If true then the file builtins.pf, world.pf, etc, will not be added to the service. Note that this means the hub won't work.
	IGNORE_BOILERPLATE = true  // Should usually be left true. Means that the flags below won't show instrumentation when compiling buitins.pf, world.pf, etc.

	FUNCTION_TO_PEEK = "" // Shows the function table entry and function tree associated with the function named in the string, if non-empty.

	// These do what it sounds like.
	SHOW_LEXER             = false
	SHOW_RELEXER           = false
	SHOW_PARSER            = false // Note that this only applies to the REPL and not to code initialization. Use FUNCTION_TO_PEEK to look at the AST of a function.
	SHOW_INITIALIZER       = false
	SHOW_COMPILER          = false
	SHOW_COMPILER_COMMENTS = false // Note that SHOW_COMPILER must also be true for this to work.
	SHOW_RUNTIME           = false // Note that this will show the hub's runtime too at present 'cos it can't tell the difference. TODO.
	SHOW_RUNTIME_VALUES    = false // Shows the contents of memory locations on the rhs of anything (i.e. not the dest).
	SHOW_XCALLS            = false
	SHOW_GOLANG            = false
	SHOW_API_SERIALIZATION = false
	SHOW_EXTERNAL_STUBS    = false
	SHOW_TESTS             = true // Says whether the tests should say what is being tested, useful if one of them crashes and we don't know which.
	SHOW_BLING_TREE        = false
	ALLOW_PANICS           = false // If turned on, permits panics instead of turning them into error messages.
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
