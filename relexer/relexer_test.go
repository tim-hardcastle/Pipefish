package relexer

import (
	"testing"
	"charm/token"
)

func TestNextToken(t *testing.T) {
	input :=
`foo(x):
	x : 1
	else : 2
`

	tests := []struct {
		expectedType    token.TokenType
		expectedLiteral string
		expectedLine    int
	}{
		{token.IDENT, "foo", 1},
		{token.LPAREN, "(", 1},
		{token.IDENT, "x", 1},
		{token.RPAREN, ")", 1},
		{token.COLON, ":", 1},
		{token.LPAREN, "(", 2},
		{token.IDENT, "x", 2},
		{token.COLON, ":", 2},
		{token.INT, "1", 2},
		{token.NEWLINE, ";", 2},
		{token.ELSE, "else", 3},
		{token.COLON, ":", 3},
		{token.INT, "2", 3},
		{token.RPAREN, ")", 4},
		{token.NEWLINE, ";", 0},
	}

	rl := New("", input)

	for i, tt := range tests {
		tok := rl.NextToken()

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

