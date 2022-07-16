package lexer

import (
	"charm/token"
	"testing"
)

func TestNextToken(t *testing.T) {
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
	tests := []struct {
		expectedType    token.TokenType
		expectedLiteral string
		expectedLine    int
	}{
		{token.NO_INDENT, "|||", 1},
		{token.IDENT, "line", 1},
		{token.IDENT, "one", 1},
		{token.NEWLINE, ";", 1},
		{token.BEGIN, "|->", 2},
		{token.IDENT, "line", 2},
		{token.IDENT, "two", 2},
		{token.NEWLINE, ";", 2},
		{token.BEGIN, "|->", 3},
		{token.IDENT, "line", 3},
		{token.IDENT, "three", 3},
		{token.DOTDOT, "..", 4},
		{token.IDENT, "line", 4},
		{token.IDENT, "threeandahalf", 4},
		{token.COMMA, ",", 5},
		{token.IDENT, "line", 5},
		{token.IDENT, "threeandthreequarters", 5},
		{token.NEWLINE, ";", 5},
		{token.COMMENT, "This is a comment", 6},
		{token.NEWLINE, ";", 6},
		{token.ILLEGAL, "lex/wsp", 7},
		{token.IDENT, "line", 7},
		{token.IDENT, "four", 7},
		{token.NEWLINE, ";", 7},
		{token.END, "2", 8},
		{token.IDENT, "line", 8},
		{token.IDENT, "five", 8},
		{token.NEWLINE, ";", 8},
		{token.NO_INDENT, "|||", 9},
		{token.IDENT, "w", 9},
		{token.COLON, ":", 9},
		{token.NEWLINE, ";", 9},
		{token.BEGIN, "|->", 10},
		{token.IDENT, "x", 10},
		{token.COLON, ":", 10},
		{token.NEWLINE, ";", 10},
		{token.BEGIN, "|->", 11},
		{token.IDENT, "y", 11},
		{token.COLON, ":", 11},
		{token.NEWLINE, ";", 11},
		{token.BEGIN, "|->", 12},
		{token.IDENT, "z", 12},
		{token.COLON, ":", 12},
		{token.INT, "1", 12},
		{token.NEWLINE, ";", 12},
		{token.NO_INDENT, "|||", 13},
		{token.ELSE, "else", 13},
		{token.COLON, ":", 13},
		{token.INT, "2", 13},
		{token.NEWLINE, ";", 13},
		{token.END, "1", 14},
		{token.ELSE, "else", 13},
		{token.COLON, ":", 13},
		{token.NEWLINE, ";", 13},
		{token.BEGIN, "|->", 14},
		{token.IDENT, "v", 14},
		{token.COLON, ":", 14},
		{token.INT, "3", 14},
		{token.NEWLINE, ";", 14},
		{token.NO_INDENT, "|||", 14},
		{token.ELSE, "else", 15},
		{token.COLON, ":", 15},
		{token.INT, "4", 15},
		{token.NEWLINE, ";", 15},
		{token.END, "2", 16},
		{token.ELSE, "else", 15},
		{token.COLON, ":", 15},
		{token.INT, "5", 15},
		{token.NEWLINE, ";", 15},
		{token.END, "1", 16},
		{token.ELSE, "else", 15},
		{token.COLON, ":", 15},
		{token.INT, "6", 15},
	}

	l := New("dummy source", input)

	for i, tt := range tests {

		tok := l.NextToken()

		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}

		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}

// Similar, but without the whitespace error.
func TestNextToken2(t *testing.T) {
	input :=
`line three ..
          .. line threeandahalf,
     .. line threeandthreequarters
line one
    line two
        line three ..
                   .. line threeandahalf,
		  .. line threeandthreequarters
  //This is a comment
line five`

	tests := []struct {
		expectedType    token.TokenType
		expectedLiteral string
		expectedLine    int
	}{	{token.NO_INDENT, "|||", 1},
		{token.IDENT, "line", 3},
		{token.IDENT, "three", 3},
		{token.DOTDOT, "..", 1},
		{token.IDENT, "line", 4},
		{token.IDENT, "threeandahalf", 4},
		{token.COMMA, ",", 5},
		{token.IDENT, "line", 5},
		{token.IDENT, "threeandthreequarters", 5},
		{token.NEWLINE, ";", 5},
		{token.NO_INDENT, "|||", 1},
		{token.IDENT, "line", 1},
		{token.IDENT, "one", 1},
		{token.NEWLINE, ";", 5},	
		{token.BEGIN, "|->", 2},
		{token.IDENT, "line", 2},
		{token.IDENT, "two", 2},
		{token.NEWLINE, ";", 5},
		{token.BEGIN, "|->", 3},
		{token.IDENT, "line", 3},
		{token.IDENT, "three", 3},
		{token.DOTDOT, "..", 1},
		{token.IDENT, "line", 4},
		{token.IDENT, "threeandahalf", 4},
		{token.COMMA, ",", 5},
		{token.IDENT, "line", 5},
		{token.IDENT, "threeandthreequarters", 5},
		{token.NEWLINE, ";", 15},
		{token.COMMENT, "This is a comment", 6},
		{token.NEWLINE, ";", 15},
		{token.END, "2", 5},
		{token.IDENT, "line", 7},
		{token.IDENT, "five", 7},
	}

	l := New("dummy source", input)

	for i, tt := range tests {

		tok := l.NextToken()

		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}

		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}
