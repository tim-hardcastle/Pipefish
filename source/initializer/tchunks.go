package initializer

import (
	"strconv"

	"github.com/tim-hardcastle/Pipefish/source/parser"
	"github.com/tim-hardcastle/Pipefish/source/token"
)

// When we first scan the stream of tokens, it is convenient to break it down into chunks
// of tokens having their own format. E.g. even before any parsing, we can break a top-level
// function declaration down into its call signature, return signature, and body, and indeed
// analyse the function signatures into variables and types, using only lexical criteria.

// Since different declarations have different contents, we slap an interface over the top.

type tokenizedCode interface {
	getDeclarationType() declarationType
	isPrivate() bool
	indexToken() token.Token
}

type tokenizedFunction struct {
	decType declarationType
	private bool
	op      token.Token
	pos     opPosition
	sig     parser.TokSig
	rets    parser.TokReturns
	body    *token.TokenizedCodeChunk
}

type opPosition int

const (
	prefix opPosition = iota
	infix
	suffix
	unfix
)

func (tc *tokenizedFunction) getDeclarationType() declarationType {
	return tc.decType
}

func (tc *tokenizedFunction) isPrivate() bool {
	return tc.private
}

func (tc *tokenizedFunction) indexToken() token.Token {
	return tc.op
}

func (iz *Initializer) ChunkFunction(cmd, private bool) (*tokenizedFunction, bool) {
	fn, ok := iz.ChunkFunctionSignature()
	if !ok {
		return &tokenizedFunction{}, false
	}
	fn.body = iz.P.SlurpBlock()
	if fn.body.Length() > 0 && fn.body.IndexToken().Type == token.GOCODE {
		fn.decType = golangDeclaration
	} else {
		if cmd {
			fn.decType = commandDeclaration
		} else {
			fn.decType = golangDeclaration
		}
	}
	fn.private = private
	return &fn, true
}

// This wraps around chunkFunctionArguments and extracts the right name.
func (iz *Initializer) ChunkFunctionSignature() (tokenizedFunction, bool) {
	position := prefix
	name := token.Token{Literal: "*dummy*"}
	if iz.P.CurTokenIs(token.IDENT) {
		name = iz.P.CurToken
		iz.P.NextToken()
	}
	if iz.P.CurTokenIs(token.COLON) || iz.P.CurTokenIs(token.PIPE) {
		position = unfix
	}
	sig, rets, ok := iz.P.ChunkFunctionArguments()
	if !ok {
		return tokenizedFunction{}, false
	}
	if name.Literal == "*dummy*" { // Then it's an infix or isSuffix. It will have been processed as bling.
		for i, pair := range sig {
			if pair.Typename[0].Literal == "bling" {
				name = pair.Name
				if i == len(sig)-1 {
					position = suffix
				} else {
					position = infix
				}
			}
		}
		if name.Literal == "*dummy*" { // Then we've found no bling. Disaster!
			iz.Throw("sigs/name", &iz.P.CurToken)
			return tokenizedFunction{}, false
		}
		if position == suffix { // Then the suffix will have been classified as bling, and we need to remove it from the sig.
			sig = sig[:len(sig)-1]
		}
	}
	return tokenizedFunction{op: name, pos: position, sig: sig, rets: rets}, true
}

func (dec *tokenizedFunction) SigAsString() string {
	result := ""
	if dec.pos == prefix || dec.pos == unfix {
		result = dec.op.Literal
	}
	if dec.pos == prefix {
		result = result + " "
	}
	result = result + dec.sig.String()
	if dec.pos == prefix && len(dec.sig) == 0 {
		result = result + "()"
	}
	if len(dec.rets) > 0 {
		result = result + " -> " + dec.rets.String()
	}
	if dec.pos == suffix {
		result = result + " " + dec.op.Literal
	}
	return result
}

// Returns the signature plus how long the body of the function is in
// tokens.
func (iz *Initializer) SummaryString(dec *tokenizedFunction) string {
	return dec.SigAsString() + " : " + strconv.Itoa(dec.body.Length()) + " tokens."
}
