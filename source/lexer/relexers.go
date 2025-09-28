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
		&removeCommentsAndIllegalTokens{},
		&removeColonAndLoggingAfterGiven{},
		&removeNewlineAfter{},
		&removeNewlineBefore{},
	)
}

// A relexer for removing comment tokens and anything marked ILLEGAL,
// since those are just there to check that the lexer knows what it's doing.
// TODO --- in fact ILLEGAL tokens should be passed on to the parser.
type removeCommentsAndIllegalTokens struct {
	acc *tokenAccessor
}

func (r *removeCommentsAndIllegalTokens) chain(ts tokensSupplier) {
	r.acc = newAccessor(ts)
}

func (r *removeCommentsAndIllegalTokens) getTokens() []token.Token {
	for ; r.acc.tok(0).Type == token.COMMENT || r.acc.tok(0).Type == token.ILLEGAL; r.acc.next() {
	}
	result := []token.Token{r.acc.tok(0)}
	r.acc.next()
	return result
}

// A relexer for removing the colon after `given`, so the parser can treat it as an infix; and
// any logging statement after that, since it can't be called. TODO --- having one should be an error.
type removeColonAndLoggingAfterGiven struct {
	acc *tokenAccessor
}

func (r *removeColonAndLoggingAfterGiven) chain(ts tokensSupplier) {
	r.acc = newAccessor(ts)
}

func (r *removeColonAndLoggingAfterGiven) getTokens() []token.Token {
	head := r.acc.tok(0)
	result := []token.Token{head}
	r.acc.next()
	if head.Type == token.GIVEN {
		if r.acc.tok(0).Type == token.COLON {
			r.acc.next()
		}
		if r.acc.tok(0).Type == token.LOG {
			r.acc.next()
		}
	}
	return result
}

// A relexer for removing non-syntactic whitespace.
type removeNewlineAfter struct {
	acc *tokenAccessor
}

func (r *removeNewlineAfter) chain(ts tokensSupplier) {
	r.acc = newAccessor(ts)
}

// Headwords are added in the method.
var REMOVE_NEWLINE_AFTER = dtypes.From[token.TokenType](
	token.COLON, token.GIVEN, token.NEWLINE, token.PRIVATE,
)

func (r *removeNewlineAfter) getTokens() []token.Token {
	head := r.acc.tok(0)
	result := []token.Token{head}
	r.acc.next()
	if REMOVE_NEWLINE_AFTER.Contains(head.Type) || token.TokenTypeIsHeadword(head.Type) {
		for r.acc.tok(0).Type == token.NEWLINE {
			r.acc.next()
		}
	}	
	return result
}

// Another relexer for removing non-syntactic whitespace.
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
