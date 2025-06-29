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

// Calls the bit that chunks the call signature
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
			if !p.PeekTokenIs((token.RPAREN)) {
				p.NextToken()
				chunk, ok = p.ChunkNameTypePairs(ANY_OR_NULL)
			}
			if !ok {
				return TokSig{}, false
			}
			if !p.PeekTokenIs(token.RPAREN) {
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
		if !p.PeekTokenIs(token.IDENT) {
			p.Throw("sigs/ident", &p.CurToken)
			return TokReturns{}, false
		}
		ty, ok := p.slurpTypeExpressionAsTokens()
		if !ok {
			return TokReturns{}, false
		}
		result = append(result, ty)
		if p.PeekTokenIs(token.COLON) {
			p.NextToken()
			return result, true
		}
		if p.PeekTokenIs(token.COMMA) {
			p.NextToken()
			continue
		}
		p.Throw("sigs/ident", &p.CurToken)
		return TokReturns{}, false
	}
}

// At this point the current token should be the colon that introduces the block.
// If we've goven up on processing the block and we're calling this from 'finishChunk' then
// 'safe' is turned on so that we don't produce bracket-matching errors.
func (p *Parser) SlurpBlock(safe bool) (*token.TokenizedCodeChunk, bool) {
	var getToken func()
	if safe {
		getToken = p.SafeNextToken
	} else {
		getToken = p.NextToken
	}
	if !p.CurTokenIs(token.COLON) {
		panic("Unhandled ill-formed declaration: " + string(p.CurToken.Type) + ", " + p.CurToken.Literal)
	}
	indexToken := p.CurToken
	code := []token.Token{}
	indentCount := 0
	getToken()
	for ; ; getToken() {
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
		code = append(code, tok)
	}
	if len(code) == 0 {
		p.Throw("parse/block/empty", &indexToken)
		return token.NewCodeChunk(), false
	}
	return token.MakeCodeChunk(code, false), true
}

// Sometimes our type signatures will have `any?` as the default, but in assignment signatures
// they will need some sort of dummy token to indicate type inference.
type DefaultTypeChunk int

const (
	ANY_OR_NULL        DefaultTypeChunk = iota // For functions etc.
	INFERRED
	MISSING_TYPE_ERROR                         // The parameters of a type should be explicitly stated.
)

var defaultMap = map[DefaultTypeChunk]func(token.Token) []token.Token{
	ANY_OR_NULL: func(t token.Token) []token.Token {
		t1 := t
		t1.Literal = "any"
		t2 := t
		t2.Literal = "?"
		return []token.Token{t1, t2}
	},
	INFERRED: func(t token.Token) []token.Token {
		t1 := t
		t1.Literal = "*inferred*"
		return []token.Token{t1}
	},
	MISSING_TYPE_ERROR: func(t token.Token) []token.Token {
		t1 := t
		t1.Literal = "*error*"
		return []token.Token{t1}
	},
}

// Chunks things of the form e.g. `a, b int` or `x, y` or `b string, c Z{5}` into pairs
// where every variable name has a type given as a list of tokens.
// It expects to begin with the first variable in the signature as the current token.
// It stops when the peekToken is a `)` or `=` or an unmatched '}'.
func (p *Parser) ChunkNameTypePairs(dflt DefaultTypeChunk) (TokSig, bool) {
	sig := TokSig{}
	for {
		if p.CurTokenIs(token.IDENT) {
			sig = append(sig, TokPair{p.CurToken, nil})
			if p.PeekTokenIs(token.IDENT) {
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
		if p.PeekTokenIs(token.RPAREN) || p.PeekTokenIs(token.ASSIGN) ||
			p.PeekTokenIs(token.GVN_ASSIGN) || p.PeekTokenIs(token.RBRACE) {
			for i, v := range sig {
				if len(v.Typename) == 0 {
					pair := v
					pair.Typename = defaultMap[dflt](v.Name)
					sig[i] = pair
				}
			}
			break
		}
		if p.PeekTokenIs(token.COMMA) {
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
		if p.PeekTokenIs(token.RPAREN) || p.PeekTokenIs(token.ASSIGN) ||
			p.PeekTokenIs(token.GVN_ASSIGN) || p.PeekTokenIs(token.COLON) ||
			((p.PeekTokenIs(token.COMMA) || p.PeekTokenIs(token.RBRACE)) && braces == 0) {
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

// Assumes a sig with no bling and emits it without surrounding parentheses.
func (t TokSig) SimpleString() string {
	result := ""
	sep := ""
	for _, pair := range t {
		result = result + sep + pair.Name.Literal
		for _, tyTok := range pair.Typename {
			result = result + " " + tyTok.Literal
		}
		sep = ", "
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
