package lexer

import (
	"github.com/tim-hardcastle/Pipefish/source/dtypes"
	"github.com/tim-hardcastle/Pipefish/source/token"
)

// What we have here is a collection of types, each with a single implementation,
// fulfilling the `relexer` interface.

// Each of them tweaks the output of the previous one slightly, so as to form a
// sort of bucket-chain of processing in which each relexer has a clearly-defined
// responsibility.

// The start of the chain is not a relexer but a lexer, since at the start of it we
// need something that consumes the text file with the code in. And the other end is
// a monotokenizer, an object which instead of emiting slices of tokens emits them
// one at a time.

// The chain is assembled by the following function.
func makeChain(ts tokensSupplier) *monotokenizer {
	return chain(
		ts,
		&removeComments{},
		&removeNewlineBefore{},
	)
}

// A relexer for removing comment tokens
type removeComments struct {
	acc *tokenAccessor
}

func (r *removeComments) chain(ts tokensSupplier) {
	r.acc = newAccessor(ts)
}

func (r *removeComments) getTokens() []token.Token {
	for ; r.acc.tok(0).Type == token.COMMENT; r.acc.next() {
	}
	result := []token.Token{r.acc.tok(0)}
	r.acc.next()
	return result
}

// A relexer for removing non-syntactic whitespace.
type removeNewlineBefore struct {
	acc *tokenAccessor
}

func (r *removeNewlineBefore) chain(ts tokensSupplier) {
	r.acc = newAccessor(ts)
}

var REMOVE_NEWLINE_BEFORE = dtypes.From[token.TokenType](
	token.EOF, token.END, token.GIVEN, token.NEWLINE, token.RPAREN,
)

func (r *removeNewlineBefore) getTokens() []token.Token {
	if r.acc.tok(0).Type == token.NEWLINE {
		peekType := r.acc.tok(1).Type
		if REMOVE_NEWLINE_BEFORE.Contains(peekType)  {
			r.acc.next()
		}
	}
	result := []token.Token{r.acc.tok(0)}
	r.acc.next()
	return result
}

// A relexer that just passes the tokens on unaltered, for testing purposes.
// NOTE --- this doesn't work!!!!!!!!
type iotaRelexer struct {
	acc *tokenAccessor
}

func (r *iotaRelexer) chain(ts tokensSupplier) {
	r.acc = newAccessor(ts)
}

func (r *iotaRelexer) getTokens() []token.Token {
	result := r.acc.buffer
	r.acc.buffer = []token.Token{}
	return result
}
