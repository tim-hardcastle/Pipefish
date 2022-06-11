# lmorg/readline

## Changes

### 3.0.1

This is a bug fix release:

* Nil map panic fixed when using dtx.AppendSuggestions()

* Hint text line proper blanked (this is a fix to a regression bug introduced
  in version 3.0.0)

* Example 01 updated to reflect API changes in 3.0.0

### 3.0.0

This release brings a considerable number of new features and bug fixes
inherited from readline's use in murex (https://github.com/lmorg/murex)

* Wrapped lines finally working (where the input line is longer than the
  terminal width)

* Delayed tab completion - allows asynchronous updates to the tab completion so
  slower suggestions do not halt the user experience

* Delayed syntax timer - allows syntax highlighting to run asynchronously for 
  slower parsers (eg spell checkers)

* Support for GetCursorPos ANSI escape sequence (though I don't have a terminal
  which supports this to test the code on)

* Better support for wrapped hint text lines

* Fixed bug with $EDITOR error handling in Windows and Plan 9

* Code clean up - fewer writes to the terminal

If you just use the exported API end points then your code should still work
verbatim. However if you are working against a fork or custom patch set then
considerable more work may be required to merge the changes.

### 2.1.0

Error returns from `readline` have been created as error a variable, which is
more idiomatic to Go than the err constants that existed previously. Currently
both are still available to use however I will be deprecating the the constants
in a latter release.

**Deprecated constants:**
```go
const (
	// ErrCtrlC is returned when ctrl+c is pressed
	ErrCtrlC = "Ctrl+C"

	// ErrEOF is returned when ctrl+d is pressed
	ErrEOF = "EOF"
)
```

**New error variables:**
```go
var (
	// CtrlC is returned when ctrl+c is pressed
	CtrlC = errors.New("Ctrl+C")

	// EOF is returned when ctrl+d is pressed
	// (this is actually the same value as io.EOF)
	EOF = errors.New("EOF")
)
```