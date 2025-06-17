package parser

import (
	"github.com/tim-hardcastle/Pipefish/source/lexer"
	"github.com/tim-hardcastle/Pipefish/source/token"
)

// The lowest level of signature representation. We just store everything as tokens until
// we find out what to do with them.
type tokPair struct {
	name token.Token
	typename []token.Token
}

type tokSig []tokPair

func (t tokSig) String() string {
	result := ""
	sep := ""
	for _, pair := range t {
		result = result + sep 
		result = result + pair.name.Literal
		for _, tyTok := range pair.typename {
			result = result + " " + tyTok.Literal
		}
		sep = ", "
	}
	return result
}

// Return types as strings.
type tokReturns [][]token.Token

type tokDeclaration struct {
	op  token.Token
	sig tokSig
	rets tokReturns
}

// This wraps around chunkFunctionSignature and extracts the right name.
func (p *Parser) ChunkFunctionDeclaration() (tokDeclaration, bool) {
	name := token.Token{Literal: "*dummy*"}
	if p.curTokenIs(token.IDENT) {
		name = p.CurToken
		p.NextToken()
	}
	sig, rets, ok := p.ChunkFunctionSignature()
	if !ok {
		return tokDeclaration{}, false
	}
	if name.Literal == "*dummy*" { // Then it's an infix or suffix. It will have been processed as bling.
		suffix := false
		for i, pair := range sig {
			if i == len(sig) - 1 {
				suffix = true
			}
			if pair.typename[0].Literal == "bling" {
				name = pair.name
			}
		}
		if name.Literal == "*dummy*" { // Then we've found no bling. Disaster!
			p.Throw("sigs/name", &p.CurToken)
			return tokDeclaration{}, false
		}
		if suffix { // Then the suffix will have been classified as bling, and we need to remove it from the sig.
			sig = sig[:len(sig)-1]
		}
	}
	return tokDeclaration{name, sig, rets}, true
}

func (p *Parser) ChunkFunctionSignature() (tokSig, tokReturns, bool) {
	sig, ok := p.ChunkFunctionCallSignature()
	if !ok {
		return tokSig{}, tokReturns{}, false
	}
	rets := tokReturns{}
	if p.peekTokenIs(token.PIPE) {
		p.NextToken()
		rets, ok = p.ChunkReturns()
	}
	if !ok {
		return tokSig{}, tokReturns{}, false
	}
	return sig, rets, true
}

// If the function signature begins with a function prefix, this will already have
// been read by the ChunkFunctionDeclaration 
// Hence we should now be looking either at an `(` or at a bling identifier. (Or a suffix
// which will be treated as bling for now and remedied by ChunkFunctionDeclaration.)
// It will end when the peekToken is either a COLON or a PIPE. 
func (p *Parser) ChunkFunctionCallSignature() (tokSig, bool) {
	result := tokSig{}
	for {
		switch p.CurToken.Type {
		case token.LPAREN:
			chunk, ok := p.ChunkNameTypePairs(ANY_OR_NULL)
			if !ok {
				return tokSig{}, false
			}
			if !p.peekTokenIs(token.RPAREN) {
				p.Throw("sigs/paren", &p.PeekToken)
				return tokSig{}, false
			}
			result = append(result, chunk...)
		case token.IDENT :
			blingTok := p.CurToken
			blingTok.Literal = "bling"
			result = append(result, tokPair{p.CurToken, []token.Token{blingTok}})
		default :
			p.Throw("sigs/expect", &p.PeekToken)
			return tokSig{}, false
		}
		if p.peekTokenIs(token.COLON) || p.peekTokenIs(token. PIPE) {
			return result, true
		}
		p.NextToken()
	}
}

// Assumes the current token is PIPE.
// Returns when the next token is COLON.
func (p *Parser) ChunkReturns() (tokReturns, bool) {
	result := tokReturns{}
	for {
		if !p.peekTokenIs(token.IDENT) {
			p.Throw("sigs/ident", &p.CurToken)
			return tokReturns{}, false
		}
		ty, ok := p.slurpTypeExpressionAsTokens()
		if !ok {
			return tokReturns{}, false
		}
		result = append(result, ty)
		if p.peekTokenIs(token.COLON) {
			return result, true
		}
		if p.peekTokenIs(token.COMMA) {
			p.NextToken()
			continue
		}
		p.Throw("sigs/ident", &p.CurToken)
		return tokReturns{}, false
	}
}

// Sometimes our type signatures will have `any?` as the default, but in assignment signatures
// they will need some sort of dummy token to indicate type inference.
type DefaultTypeChunk int 

const (
	ANY_OR_NULL DefaultTypeChunk = iota
)

var defaultMap = map[DefaultTypeChunk] func(token.Token)[]token.Token {
	ANY_OR_NULL : func (t token.Token) []token.Token {
		t1 := t 
		t1.Literal = "any"
		t2 := t 
		t2.Literal = "?"
		return []token.Token{t1, t2}
	},
}

// Chunks things of the form e.g. `a, b int` or `x, y` or `b string, c Z{5}` into pairs
// where every variable name has a type given as a list of tokens.
// It expects to begin with the first variable in the signature as the current token.
// It stops when the peekToken is a `)` or `=`.

func (p *Parser) ChunkNameTypePairs(dflt DefaultTypeChunk) (tokSig, bool) {
	sig := tokSig{}
	for {
		if p.curTokenIs(token.IDENT) {
			sig = append(sig, tokPair{p.CurToken, nil})
			if p.peekTokenIs(token.IDENT) {
				typeName, ok := p.slurpTypeExpressionAsTokens() 
				if !ok {
					return tokSig{}, false
				}			
				for i, v := range sig {
					if len(v.typename) == 0 {
						sig[i].typename = typeName
					}
				}
			}
		}
		if p.peekTokenIs(token.RPAREN) || p.peekTokenIs(token.ASSIGN) || 
				p.peekTokenIs(token.GVN_ASSIGN) {
			for i, v := range sig {
				if len(v.typename) == 0 {
					pair := v 
					pair.typename = defaultMap[dflt](v.name)
					sig[i] = pair
				}
			}
			break
		}
		if p.peekTokenIs(token.COMMA) {
			p.NextToken()
			p.NextToken()
			continue
		}
	}
	return sig, true
}

// Exists for testing purposes.
func (p *Parser) ParseInnerChunksFromString(input string) tokSig {
	p.PrimeWithString("test", input)
	result, _ := p.ChunkNameTypePairs(ANY_OR_NULL)
	p.Common.Errors = append(p.TokenizedCode.(*lexer.Relexer).GetErrors(), p.Common.Errors...)
	return result
}

// Gets things of the form `int`, `Z{5}, string/int`, etc as a list of tokens.
// Or indeed anything else, the tokens don't have to be a well-formed description of
// a type at this point.
// Starts with the assumption that the next token is the first token of the type.
// Returns when the next token is a `)` or `=`, or a `,` outside of a `{..}`.
func (p *Parser) slurpTypeExpressionAsTokens() ([]token.Token, bool) {
	braces := 0
	result := []token.Token{}
	for {
		if p.curTokenIs(token.EOF) {
				p.Throw("sigs/unfinished", &p.CurToken)
				return []token.Token{}, false
			}
		if p.peekTokenIs(token.RPAREN) || p.peekTokenIs(token.ASSIGN) || p.peekTokenIs(token.GVN_ASSIGN) ||
		    (p.peekTokenIs(token.COMMA) && braces == 0) {
				return result, true
			}
		p.NextToken()
		result = append(result, p.CurToken)
		if p.curTokenIs(token.LBRACE) {
			braces ++
		}
		if p.curTokenIs(token.RBRACE) {
			braces --
		}
	}
}
