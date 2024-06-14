// All this does is contain in one place the constants controlling which bits of the inner workings of the
// lexer/parser/compiler/are displayed to me for debugging purposes. In a release they must all be set to false
// except SUPPRESS_BUILTINS which may as well be left as true.

package settings

import "pipefish/source/dtypes"

var MandatoryImports = []string{"rsc/pipefish/builtins.pf", "rsc/pipefish/world.pf", "rsc/pipefish/timeStruct.pf"}

var MandatoryImportSet = dtypes.MakeFromSlice(MandatoryImports)
var ThingsToIgnore = (dtypes.MakeFromSlice(MandatoryImports)).Add("rsc/pipefish/hub.pf")

const (
	OMIT_BUILTINS      = false // If true then the file builtins.pf, world.pf, etc, will not be added to the service. Note that this means the hub won't work.
	USE_TEST           = false // If true the file rsc/pipefish/test.pf will be added to any service along with the builtins etc.
	IGNORE_BOILERPLATE = true  // Should usually be left true. Means that the first four flags below won't show instrumentation when compiling buitins.pf, world.pf, etc.

	FUNCTION_TO_PEEK = "" // Shows the function table entry and function tree associated with the function named in the string, if non-empty.

	// These do what it sounds like.
	SHOW_LEXER             = false
	SHOW_RELEXER           = false
	SHOW_PARSER            = false // Note that this only applies to the REPL and not to code initialization.
	SHOW_COMPILER          = false
	SHOW_COMPILER_COMMENTS = false
	SHOW_RUNTIME           = false // Note that this will show the hub's runtime too at present 'cos it can't tell the difference. TODO.
	SHOW_RUNTIME_VALUES    = false // The firehose. Shows the contents of memory locations on the rhs of anything (i.e. not the dest).
	SHOW_XCALLS            = false
)
