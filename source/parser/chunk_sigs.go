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
	lastWasBling := true
	for i, pair := range t {
		currentIsBling := pair.typename[0].Literal == "bling"
		if !(lastWasBling || currentIsBling) {
			result = result + ", " 
		}
		if lastWasBling && !currentIsBling {
			if i > 0 {
				result = result + " "
			}
			result = result + "("
		}
		if currentIsBling && !lastWasBling {
			result = result + ")"
		}
		if currentIsBling {
			if i > 0 {
				result = result + " "
			}
			result = result + pair.name.Literal
			lastWasBling = true
			continue
		}
		lastWasBling = false
		result = result + pair.name.Literal
		for _, tyTok := range pair.typename {
			result = result + " " + tyTok.Literal
		}
		if i == len(t) - 1 {
			result = result + ")"
		}
	}
	return result
}

// Return types as strings.
type tokReturns [][]token.Token

func (r tokReturns) String() string {
	sep := ""
	result := ""
	for _, ty := range r {
		result = result + sep
		spacer := ""
		for _, tok := range ty {
			result = result + spacer
			result = result + tok.Literal
			spacer = " "
		}
		sep = ", "
	}
	return result
}

type opPosition int 

const (
	prefix opPosition = iota 
	infix 
	suffix 
	unfix
)

type tokDeclaration struct {
	op  token.Token
	pos  opPosition
	sig tokSig
	rets tokReturns
}

func (dec *tokDeclaration) String() string {
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

// This wraps around chunkFunctionSignature and extracts the right name.
func (p *Parser) ChunkFunctionDeclaration() (tokDeclaration, bool) {
	position := prefix
	name := token.Token{Literal: "*dummy*"}
	if p.curTokenIs(token.IDENT) {
		name = p.CurToken
		p.NextToken()
	}
	if p.curTokenIs(token.COLON) || p.curTokenIs(token.PIPE) {
		position = unfix
	}
	sig, rets, ok := p.ChunkFunctionSignature()
	if !ok {
		return tokDeclaration{}, false
	}
	if name.Literal == "*dummy*" { // Then it's an infix or isSuffix. It will have been processed as bling.
		for i, pair := range sig {
			if pair.typename[0].Literal == "bling" {
				name = pair.name
				if i == len(sig) - 1 {
					position = suffix
				} else {
					position = infix
				}
			}
		}
		if name.Literal == "*dummy*" { // Then we've found no bling. Disaster!
			p.Throw("sigs/name", &p.CurToken)
			return tokDeclaration{}, false
		}
		if position == suffix { // Then the suffix will have been classified as bling, and we need to remove it from the sig.
			sig = sig[:len(sig)-1]
		}
	}
	return tokDeclaration{name, position, sig, rets}, true
}

// Exists for testing purposes.
func (p *Parser) ChunkFunctionDeclarationFromString(input string) string {
	p.PrimeWithString("test", input)
	dec, _ := p.ChunkFunctionDeclaration()
	p.Common.Errors = append(p.TokenizedCode.(*lexer.Relexer).GetErrors(), p.Common.Errors...)
	return dec.String()
}

// The previous function wraps around this in various annoying ways.
// This one is relatively simple, and calls the bit that chunks the call signature
// followed optionally by the bit that chunks the return signature.
func (p *Parser) ChunkFunctionSignature() (tokSig, tokReturns, bool) {
	sig, ok := p.ChunkFunctionCallSignature()
	if !ok {
		return tokSig{}, tokReturns{}, false
	}
	rets := tokReturns{}
	if p.curTokenIs(token.PIPE) {
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
// It will end when the current token is either a COLON or a PIPE. 
func (p *Parser) ChunkFunctionCallSignature() (tokSig, bool) {
	result := tokSig{}
	for {
		switch p.CurToken.Type {
		case token.LPAREN:
			var chunk tokSig
			ok := true
			if !p.peekTokenIs((token.RPAREN)) {
				p.NextToken()
				chunk, ok = p.ChunkNameTypePairs(ANY_OR_NULL)
			}
			if !ok {
				return tokSig{}, false
			}
			if !p.peekTokenIs(token.RPAREN) {
				p.Throw("sigs/paren", &p.PeekToken)
				return tokSig{}, false
			}
			result = append(result, chunk...)
			p.NextToken()
			p.NextToken()
			continue
		case token.IDENT :
			blingTok := p.CurToken
			blingTok.Literal = "bling"
			result = append(result, tokPair{p.CurToken, []token.Token{blingTok}})
			p.NextToken()
			continue
		case token.COLON, token.PIPE :
			return result, true
		default :
			p.Throw("sigs/expect", &p.CurToken)
			return tokSig{}, false
		}
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
		p.Throw("sigs/expect/b", &p.CurToken)
		return tokSig{}, false
	}
	return sig, true
}



// Gets things of the form `int`, `Z{5}, string/int`, etc as a list of tokens.
// Or indeed anything else, the tokens don't have to be a well-formed description of
// a type at this point.
// Starts with the assumption that the next token is the first token of the type.
// Returns when the next token is a `)` or `=`, or `or a `,` outside of a `{..}`.
func (p *Parser) slurpTypeExpressionAsTokens() ([]token.Token, bool) {
	braces := 0
	result := []token.Token{}
	for {
		if p.curTokenIs(token.EOF) {
				p.Throw("sigs/unfinished", &p.CurToken)
				return []token.Token{}, false
			}
		if p.peekTokenIs(token.RPAREN) || p.peekTokenIs(token.ASSIGN) || 
		   p.peekTokenIs(token.GVN_ASSIGN) || p.peekTokenIs(token.COLON) ||
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
