package parser

import (
	"github.com/tim-hardcastle/Pipefish/source/settings"
	"github.com/tim-hardcastle/Pipefish/source/text"
	"github.com/tim-hardcastle/Pipefish/source/token"
)

// The lowest level of signature representation. We just store everything as tokens until
// we find out what to do with them.

// Type declarations.

type TokPair struct {
	Name     token.Token
	Typename []token.Token
}

type TokSig []TokPair

// The previous function wraps around this in various annoying ways.
// This one is relatively simple, and calls the bit that chunks the call signature
// followed optionally by the bit that chunks the return signature.
func (p *Parser) ChunkFunctionArguments() (TokSig, TokReturns, bool) {
	sig, ok := p.ChunkFunctionCallSignature()
	if !ok {
		return TokSig{}, TokReturns{}, false
	}
	rets := TokReturns{}
	if p.CurTokenIs(token.PIPE) {
		rets, ok = p.ChunkReturns()
	}
	if !ok {
		return TokSig{}, TokReturns{}, false
	}
	return sig, rets, true
}

// If the function signature begins with a function prefix, this will already have
// been read by the ChunkFunctionDeclaration
// Hence we should now be looking either at an `(` or at a bling identifier. (Or a suffix
// which will be treated as bling for now and remedied by ChunkFunctionDeclaration.)
// It will end when the current token is either a COLON or a PIPE.
func (p *Parser) ChunkFunctionCallSignature() (TokSig, bool) {
	result := TokSig{}
	for {
		switch p.CurToken.Type {
		case token.LPAREN:
			var chunk TokSig
			ok := true
			if !p.peekTokenIs((token.RPAREN)) {
				p.NextToken()
				chunk, ok = p.ChunkNameTypePairs(ANY_OR_NULL)
			}
			if !ok {
				return TokSig{}, false
			}
			if !p.peekTokenIs(token.RPAREN) {
				p.Throw("sigs/paren", &p.PeekToken)
				return TokSig{}, false
			}
			result = append(result, chunk...)
			p.NextToken()
			p.NextToken()
			continue
		case token.IDENT:
			blingTok := p.CurToken
			blingTok.Literal = "bling"
			result = append(result, TokPair{p.CurToken, []token.Token{blingTok}})
			p.NextToken()
			continue
		case token.COLON, token.PIPE:
			return result, true
		default:
			p.Throw("sigs/expect", &p.CurToken)
			return TokSig{}, false
		}
	}
}

// Assumes the current token is PIPE.
// Returns when the current token is COLON.
func (p *Parser) ChunkReturns() (TokReturns, bool) {
	result := TokReturns{}
	for {
		if !p.peekTokenIs(token.IDENT) {
			p.Throw("sigs/ident", &p.CurToken)
			return TokReturns{}, false
		}
		ty, ok := p.slurpTypeExpressionAsTokens()
		if !ok {
			return TokReturns{}, false
		}
		result = append(result, ty)
		if p.peekTokenIs(token.COLON) {
			p.NextToken()
			return result, true
		}
		if p.peekTokenIs(token.COMMA) {
			p.NextToken()
			continue
		}
		p.Throw("sigs/ident", &p.CurToken)
		return TokReturns{}, false
	}
}

// At this point the current token should be the colon that introduced the function body.
func (p *Parser) SlurpBlock() *token.TokenizedCodeChunk {
	if !p.CurTokenIs(token.COLON) {
		panic("Unhandled ill-formed declaration: " + string(p.CurToken.Type) + ", " + p.CurToken.Literal)
	}
	code := []token.Token{}
	indentCount := 0
	p.NextToken()
	for ; ; p.NextToken() {
		tok := p.CurToken
		if tok.Type == token.EOF {
			break
		}
		if settings.SHOW_RELEXER && !(settings.IGNORE_BOILERPLATE && settings.ThingsToIgnore.Contains(tok.Source)) {
			println(text.PURPLE+tok.Type, tok.Literal+text.RESET)
		}
		if tok.Type == token.LPAREN && tok.Literal == "|->" {
			indentCount++
		}
		if tok.Type == token.RPAREN && tok.Literal == "<-|" {
			indentCount--
		}
		if indentCount == 0 && tok.Type == token.NEWLINE {
			break
		}
		println("Appending", tok.Type, tok.Literal)
		code = append(code, tok)
	}
	return token.MakeCodeChunk(code, false)
}

// Sometimes our type signatures will have `any?` as the default, but in assignment signatures
// they will need some sort of dummy token to indicate type inference.
type DefaultTypeChunk int

const (
	ANY_OR_NULL DefaultTypeChunk = iota
)

var defaultMap = map[DefaultTypeChunk]func(token.Token) []token.Token{
	ANY_OR_NULL: func(t token.Token) []token.Token {
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

func (p *Parser) ChunkNameTypePairs(dflt DefaultTypeChunk) (TokSig, bool) {
	sig := TokSig{}
	for {
		if p.CurTokenIs(token.IDENT) {
			sig = append(sig, TokPair{p.CurToken, nil})
			if p.peekTokenIs(token.IDENT) {
				typeName, ok := p.slurpTypeExpressionAsTokens()
				if !ok {
					return TokSig{}, false
				}
				for i, v := range sig {
					if len(v.Typename) == 0 {
						sig[i].Typename = typeName
					}
				}
			}
		}
		if p.peekTokenIs(token.RPAREN) || p.peekTokenIs(token.ASSIGN) ||
			p.peekTokenIs(token.GVN_ASSIGN) {
			for i, v := range sig {
				if len(v.Typename) == 0 {
					pair := v
					pair.Typename = defaultMap[dflt](v.Name)
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
		return TokSig{}, false
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
		if p.CurTokenIs(token.EOF) {
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
		if p.CurTokenIs(token.LBRACE) {
			braces++
		}
		if p.CurTokenIs(token.RBRACE) {
			braces--
		}
	}
}

// String functions, mostly for tessting purposes.

func (t TokSig) String() string {
	result := ""
	lastWasBling := true
	for i, pair := range t {
		currentIsBling := pair.Typename[0].Literal == "bling"
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
			result = result + pair.Name.Literal
			lastWasBling = true
			continue
		}
		lastWasBling = false
		result = result + pair.Name.Literal
		for _, tyTok := range pair.Typename {
			result = result + " " + tyTok.Literal
		}
		if i == len(t)-1 {
			result = result + ")"
		}
	}
	return result
}

// Return types as strings.
type TokReturns [][]token.Token

func (r TokReturns) String() string {
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


