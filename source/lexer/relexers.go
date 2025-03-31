package lexer

import "github.com/tim-hardcastle/Pipefish/source/token"

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
func makeChain(ts tokenSupplier) monotokenizer {
	return chain(ts)
}

// A relexer that just passes the tokens on unaltered, for testing purposes. 
type iotaRelexer struct {
	acc tokenAccessor
}

func (iR *iotaRelexer) chain(acc tokenAccessor) {
	iR.acc = acc 
}

func (iR *iotaRelexer) getTokens() []token.Token {
	result := iR.acc.buffer
	iR.acc.buffer = []token.Token{}
	return result
}