package tokenized_code_chunk

import (
	"charm/token"
	"fmt"
)

type TokenizedCodeChunk struct {
	position int
	code     []token.Token
}

func New() *TokenizedCodeChunk {
	tcc := &TokenizedCodeChunk{
		position: -1,
		code:     []token.Token{},
	}
	return tcc
}

func (tcc *TokenizedCodeChunk) Change(newToken token.Token) {
	tcc.code[tcc.position] = newToken
}

func (tcc *TokenizedCodeChunk) Append(tokenToAppend token.Token) {
	tcc.code = append(tcc.code, tokenToAppend)
}

func (tcc *TokenizedCodeChunk) Length() int {
	return len(tcc.code)
}

func (tcc *TokenizedCodeChunk) NextToken() token.Token {
	if tcc.position+1 < len(tcc.code) {
		tcc.position++
		return tcc.code[tcc.position]
	}
	return token.Token{Type: token.EOF, Literal: "EOF",
		Line: tcc.code[tcc.position].Line, ChStart: tcc.code[tcc.position].ChStart,
		ChEnd: tcc.code[tcc.position].ChEnd, Source: tcc.code[tcc.position].Source}
}

func (tcc *TokenizedCodeChunk) String() string {
	output := ""
	tcc.ToStart()
	for j := 0; j < tcc.Length(); j++ {
		output = output + fmt.Sprintf("%v\n", tcc.NextToken())
	}
	// for j := 0; j < len(tcc.code); j++ {
	// output = output + fmt.Sprintf("%v\n", tcc.code[j])
	// }
	return output + "\n"
}

func (tcc *TokenizedCodeChunk) ToStart() {
	tcc.position = -1
}
