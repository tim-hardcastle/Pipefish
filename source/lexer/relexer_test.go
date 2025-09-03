package lexer

import (
	"testing"

	"github.com/tim-hardcastle/Pipefish/source/token"
)

func TestNextTokenForRelexer(t *testing.T) {
	input :=
		`foo(x):
	x : 1
	else : 2
`

	items := []testItem{
		{token.IDENT, "foo", 1},
		{token.LPAREN, "(", 1},
		{token.IDENT, "x", 1},
		{token.RPAREN, ")", 1},
		{token.COLON, ":", 1},
		{token.LPAREN, "|->", 2},
		{token.IDENT, "x", 2},
		{token.COLON, ":", 2},
		{token.INT, "1", 2},
		{token.NEWLINE, ";", 3},
		{token.ELSE, "else", 3},
		{token.COLON, ":", 3},
		{token.INT, "2", 3},
		{token.RPAREN, "<-|", 4},
		{token.EOF, ";", 4},
	}

	testRelexingString(t, input, items)
}

func TestRlGolang(t *testing.T) {
	input :=

		`golang "qux"

golang {
    foo
}`

	items := []testItem{
		{token.GOLANG, "qux", 1},
		{token.NEWLINE, ";", 2},
		{token.GOLANG, "\n    foo\n", 5},
		{token.EOF, "EOF", 5},
	}
	testRelexingString(t, input, items)
}

func testRelexingString(t *testing.T, input string, items []testItem) {
	rl := NewRelexer("dummy source", input)
	runTest(t, rl, items)
}
