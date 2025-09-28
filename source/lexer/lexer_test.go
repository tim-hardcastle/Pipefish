package lexer

import (
	"testing"

	"github.com/tim-hardcastle/Pipefish/source/token"
)

func TestCommentsAndIndents(t *testing.T) {
	input :=
		`line one
    line two
        line three ..
                   .. line threeandahalf,
		  .. line threeandthreequarters
  //This is a comment
	line four
line five
w :
	x :
		y :
			z : 1
			else : 2
		else :
			v : 3
			else : 4
	else : 5
else : 6
			`
	items := []testItem{
		{token.IDENT, "line", 1},//0
		{token.IDENT, "one", 1},
		{token.NEWLINE, ";", 1},
		{token.BEGIN, "|->", 2},
		{token.IDENT, "line", 2},
		{token.IDENT, "two", 2},
		{token.NEWLINE, ";", 2},
		{token.BEGIN, "|->", 3},
		{token.IDENT, "line", 3},
		{token.IDENT, "three", 3}, 
		{token.IDENT, "line", 4}, //10
		{token.IDENT, "threeandahalf", 4},
		{token.COMMA, ",", 4},
		{token.IDENT, "line", 5},
		{token.IDENT, "threeandthreequarters", 5},
		{token.NEWLINE, ";", 5},
		{token.COMMENT, "This is a comment", 6},
		{token.ILLEGAL, "lex/wsp", 7},
		{token.IDENT, "line", 7}, 
		{token.IDENT, "four", 7}, 
		{token.NEWLINE, ";", 7}, //20
		{token.END, "<-|", 8},
		{token.END, "<-|", 8},
		{token.NEWLINE, ";", 8},
		{token.IDENT, "line", 8},
		{token.IDENT, "five", 8},
		{token.NEWLINE, ";", 8},
		{token.IDENT, "w", 9},
		{token.COLON, ":", 9},
		{token.NEWLINE, ";", 9}, 
		{token.BEGIN, "|->", 10}, 
		{token.IDENT, "x", 10}, // 30
		{token.COLON, ":", 10},
		{token.NEWLINE, ";", 10},
		{token.BEGIN, "|->", 11},
		{token.IDENT, "y", 11},
		{token.COLON, ":", 11},
		{token.NEWLINE, ";", 11},
		{token.BEGIN, "|->", 12},
		{token.IDENT, "z", 12}, 
		{token.COLON, ":", 12}, 
		{token.INT, "1", 12}, // 40
		{token.NEWLINE, ";", 12},
		{token.ELSE, "else", 13},
		{token.COLON, ":", 13},
		{token.INT, "2", 13},
		{token.NEWLINE, ";", 13},
		{token.END, "<-|", 14},
		{token.NEWLINE, ";", 14},
		{token.ELSE, "else", 14}, 
		{token.COLON, ":", 14}, 
		{token.NEWLINE, ";", 14}, // 50
		{token.BEGIN, "|->", 15},
		{token.IDENT, "v", 15},
		{token.COLON, ":", 15},
		{token.INT, "3", 15},
		{token.NEWLINE, ";", 15},
		{token.ELSE, "else", 16},
		{token.COLON, ":", 16}, 
		{token.INT, "4", 16}, 
		{token.NEWLINE, ";", 16}, // 60
		{token.END, "<-|", 17},
		{token.END, "<-|", 17},
		{token.NEWLINE, ";", 17},
		{token.ELSE, "else", 17},
		{token.COLON, ":", 17},
		{token.INT, "5", 17},
		{token.NEWLINE, ";", 17},
		{token.END, "<-|", 18},
		{token.NEWLINE, ";", 18},
		{token.ELSE, "else", 18},
		{token.COLON, ":", 18}, 
		{token.INT, "6", 18}, 
	}
	testLexingString(t, input, items)
}

func TestGolang(t *testing.T) {
	input :=

		`golang "qux"
golang "foo"

golang {
    foo
}`

	items := []testItem{
		{token.GOLANG, "qux", 1},
		{token.NEWLINE, ";", 1},
		{token.GOLANG, "foo", 2},
		{token.NEWLINE, ";", 2},
		{token.NEWLINE, ";", 3},
		{token.GOLANG, "\n    foo\n", 4},
		{token.NEWLINE, ";", 4},
	}
	testLexingString(t, input, items)
}

type testItem struct {
	expectedType    token.TokenType
	expectedLiteral string
	expectedLine    int
}

func testLexingString(t *testing.T, input string, items []testItem) {
	l := NewLexer("dummy source", input)
	mt := chain(l)
	runTest(t, mt, items)
}

func runTest(t *testing.T, ts TokenSupplier, items []testItem) {
	for i, tt := range items {
		tok := ts.NextToken()
		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q with literal %q, got=%q with literal %q",
				i, tt.expectedType, tt.expectedLiteral, tok.Type, tok.Literal)
		}
		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
		if tok.Line != tt.expectedLine {
			t.Fatalf("tests[%d] - line wrong. expected=%d, got=%d",
				i, tt.expectedLine, tok.Line)
		}
	}
}
