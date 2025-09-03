package lexer

import (
	"fmt"

	"github.com/tim-hardcastle/Pipefish/source/token"
)

// Between the lexer and the parser we "relex" the lexer's output to make it easier for
// the parser to process.

// This is a fiddly process and to keep us from having to do it all in one big outer loop
// we arrange it on the bucket chain principle. We make lots of relexers, each of which
// processes the stream of tokens in simple way.

// This interface allows the parser to get its supply of tokens either from the relexer directly or from
// a `TokenizedCodeChunk`.
type TokenSupplier interface{ NextToken() token.Token }

// Dumps the contents of a `TokenSupplier` into a string.
func String(t TokenSupplier) string {
	result := ""
	for tok := t.NextToken(); tok.Type != "EOF"; tok = t.NextToken() {
		result = result + fmt.Sprintf("%+v\n", tok)
	}
	return result
}

// We may be getting tokens from either the lexer or from another relexer.
type tokensSupplier interface {
	getTokens() []token.Token
}

// The tokenSupplier may supply us with any non-negative number of tokens.
type tokenAccessor struct {
	supplier tokensSupplier
	buffer   []token.Token
}

// Makes a pointer to an accessor with an empty buffer.
func newAccessor(supplier tokensSupplier) *tokenAccessor {
	return &tokenAccessor{supplier, []token.Token{}}
}

// We can ask to look at the ith token, where 0 is the current one. The accessor
// will keep asking the supplier for
func (ta *tokenAccessor) tok(i int) token.Token {
	for len(ta.buffer) <= i {
		ta.buffer = append(ta.buffer, ta.supplier.getTokens()...)
	}
	return ta.buffer[i]
}

func (ta *tokenAccessor) next() {
	ta.tok(0) // To ensure there is something there to discard.
	ta.buffer = ta.buffer[1:]
}

// The relexer needs to be an interface because some of them are going to need to
// have state.
type relexer interface {
	chain(ts tokensSupplier)
	getTokens() []token.Token
}

// Takes the output from a tokenSupplier and produces a stream of individual tokens.
type monotokenizer struct {
	accessor *tokenAccessor
}

func newMonotokenizer(ts tokensSupplier) *monotokenizer {
	return &monotokenizer{newAccessor(ts)}
}

func (mt *monotokenizer) NextToken() token.Token {
	result := mt.accessor.tok(0)
	mt.accessor.next()
	return result
}

func chain(ts tokensSupplier, relexers ...relexer) *monotokenizer {
	sup := ts
	for _, rl := range relexers {
		rl.chain(sup)
		sup = rl
	}
	return newMonotokenizer(sup)
}
