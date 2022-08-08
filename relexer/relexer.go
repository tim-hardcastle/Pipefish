package relexer

// A general sanitation operation and a bit of a kludge: if I wrote it again
// from scratch I'm sure I could make the lexer and relexer into one thing.
//
// The relexer gets tokens from the lexer, throws away the non-semantic ones,
// expands the END statements, turns BEGIN and END into parentheses. It removes
// superfluous newlines and also the colon after GIVEN, since the lexer will
// treat both of these as infix operators.
//
// We're also going to kludge the func definitions. They need to have "weak commas"
// between the func keyword and the colon. A newline after this colon must be discarded as usual.
//
// For this purpose we have the funcDef flag. Is it not written that flags are a Code
// Smell? It is so written.
//
// To add to the stench, we also need a givenHappened flag. Assignments in the "given"
// section of a function have to be treated differently from everything else, but they
// also have to be treated the same whether they appear in the var section, the def
// section, and the REPL.
//
// To cope with this, we will keep a crude count of the nesting level. After "given", all
// the assignments will be turned into GVN_ASSIGN until the nesting level goes down to 
// zero. 
//

import (
	"charm/lexer"
	"charm/object"
	"charm/token"

	"fmt"
	"strconv"
)

type Relexer struct {
	source    string
	lexer     lexer.Lexer
	preTok, curTok, nexTok token.Token
	givenHappened bool
	lparenMeansInnerFunction bool
	innerFunctionIsHappening bool
	nestingLevel int
	Errors object.Errors
	funcDef bool
	structDef bool
}

func New(source, input string) *Relexer {
    l := *lexer.New(source, input)
	rl := &Relexer{lexer : l,
				   source : source,
		           preTok : l.NewToken(token.NEWLINE, ";"),
				   curTok : l.NextNonCommentToken(),
				   nexTok : l.NextNonCommentToken(),
				   funcDef : false,
				   structDef : false,
				   Errors : []*object.Error{},
				}
	return rl
}

func (rl *Relexer) NextToken() token.Token {
	// So, this is almost all a big case switch on the current token.
	// Depending on what it is, we may return it () as the default, or "burn" it, in which
	// case it disappears so completely it doesn't even become the preTok, the previous token,
	// and we return what we would have gotten did it not exist, or we can insert before it, emitting 
	// a fresh token and making that the preTok.
	//
	// We use this last facility to expand out the END statements.


	if rl.nestingLevel == 0 && (rl.curTok.ChStart == 0 && !(rl.curTok.Type == token.BEGIN)) {
		rl.givenHappened = false
		rl.lparenMeansInnerFunction = false
		rl.innerFunctionIsHappening = false
	}

	if rl.innerFunctionIsHappening && rl.preTok.Type == token.IDENT && rl.curTok.Type == token.IDENT &&
	/**/rl.nexTok.Type == token.COMMA {
		rl.nexTok.Type = token.WEAK_COMMA
	}

	if rl.nexTok.Type == token.BEGIN && 
			 !(rl.curTok.Type == token.GIVEN || rl.curTok.Type == token.COLON ||
			  (rl.curTok.Type == token.NEWLINE && ((rl.preTok.Type == token.COLON) || (rl.preTok.Type == token.MAGIC_COLON)) || (rl.preTok.Type == token.GIVEN)) ) {
				 fmt.Println(rl.curTok)
		rl.Throw("relex/indent", rl.curTok)
	}
	
	switch rl.curTok.Type {
		case token.NO_INDENT :
			return rl.burnToken();
		case token.DOTDOT :
			return rl.burnToken();
		case token.COMMENT :
			return rl.burnToken()
		case token.NEWLINE :
		
			if rl.nexTok.Type == token.NO_INDENT || 
			/**/ rl.nexTok.Type == token.NEWLINE {
				return rl.burnNextToken()
			}

			if rl.preTok.Type == token.NEWLINE ||
			/**/ rl.nexTok.Type == token.GIVEN ||
			/**/ rl.preTok.Type == token.GIVEN ||
			/**/ token.TokenTypeIsHeadword(rl.preTok.Type) ||
			/**/ rl.preTok.Type == token.PRIVATE ||
			/**/ rl.preTok.Type == token.COLON ||
			/**/ rl.preTok.Type == token.MAGIC_COLON ||
			/**/ rl.nexTok.Type == token.END ||
			/**/ rl.nexTok.Type == token.RPAREN {
				return rl.burnToken()
			}
			
		case token.ASSIGN : 
			if rl.givenHappened {
				rl.curTok.Type = token.GVN_ASSIGN
				rl.lparenMeansInnerFunction = false
				rl.innerFunctionIsHappening = false
			}
		case token.IDENT :
			if rl.curTok.Literal == "struct" {
				rl.structDef = true
			}
			if rl.curTok.Literal == "func" {
				rl.funcDef = true
			}
			if rl.preTok.Type == token.IDENT && rl.nexTok.Type == token.COMMA && (rl.funcDef || rl.structDef) {
				rl.nexTok.Type = token.WEAK_COMMA
			}
		case token.ILLEGAL :
			return rl.burnToken()
		case token.COLON :
			if rl.innerFunctionIsHappening {
				rl.innerFunctionIsHappening = false
				rl.lparenMeansInnerFunction = false
				rl.curTok.Type = token.MAGIC_COLON
				rl.curTok.Literal = ":"
			}
			if rl.preTok.Type == token.GIVEN {
				return rl.burnToken()
			}
			if rl.funcDef {
				rl.curTok.Type = token.COLON
				rl.funcDef = false
			}
		case token.BEGIN :
			rl.curTok.Type = token.LPAREN
			rl.curTok.Literal = "|->"
			rl.nestingLevel = rl.nestingLevel + 1
		case token.LPAREN :
			rl.nestingLevel = rl.nestingLevel + 1
			if rl.lparenMeansInnerFunction { 
				rl.innerFunctionIsHappening = true
				rl.lparenMeansInnerFunction = false
			 }
		case token.RPAREN :
			rl.nestingLevel = rl.nestingLevel - 1
		case token.END :
			n, _ := strconv.Atoi(rl.curTok.Literal)	
			switch {
				case n == - 1 :
					return rl.burnToken()
				case n == 0 :
					if rl.nexTok.Type == token.GIVEN {
						rl.givenHappened = true
						rl.lparenMeansInnerFunction = true
						return rl.burnToken()
					}
					rl.curTok.Literal = strconv.Itoa(n - 1)
					return token.Token{Type: token.NEWLINE, Literal: ";", Line: rl.curTok.Line,
					/**/ChStart: 0, ChEnd: 0, Source: rl.curTok.Source}
				default:
					rl.nestingLevel = rl.nestingLevel - 1
					rl.curTok.Literal = strconv.Itoa(n - 1)
					if rl.nestingLevel == 0 && rl.preTok.Type != token.GIVEN {
						rl.givenHappened = false
					}
					return token.Token{Type: token.RPAREN, Literal: "<-|", Line: rl.curTok.Line,
					/**/ChStart: 0, ChEnd: 0, Source: rl.curTok.Source}
			}	
		case token.GIVEN :
			rl.givenHappened = true
			rl.innerFunctionIsHappening = false
			rl.lparenMeansInnerFunction = true
			if rl.nexTok.Type == token.COLON {
				return rl.burnNextToken()
			}
			if rl.preTok.Type == token.NEWLINE {
				rl.getToken();
			}
	}

	if rl.curTok.Type == token.NEWLINE && rl.givenHappened { 
		rl.lparenMeansInnerFunction = true
		rl.innerFunctionIsHappening = false
	}
	
	rl.getToken()  // We shuffle them all along before returning 'cos we sure can't do it afterwards.
	//fmt.Println(rl.preTok, rl.nestingLevel, rl.givenHappened)
	return rl.preTok // Which up until now has been the curTok
}

func (rl *Relexer) getToken() {
	rl.preTok = rl.curTok
	rl.curTok = rl.nexTok
	rl.nexTok = rl.lexer.NextNonCommentToken()
}

func (rl *Relexer) burnToken() token.Token {
	rl.curTok = rl.nexTok
	rl.nexTok = rl.lexer.NextNonCommentToken()
	return rl.NextToken()
}

func (rl *Relexer) burnNextToken() token.Token {
	rl.nexTok = rl.lexer.NextNonCommentToken()
	return rl.NextToken()
}

func (rl *Relexer) insertTokenBeforeCurrentToken(token token.Token) token.Token {
	rl.preTok = token
	return rl.preTok
}

func (rl *Relexer) PeekToken() token.Token {
	return rl.curTok 
}

func RelexDump(input string) {
    fmt.Print("Relexer output: \n\n")
    rl := New("", input)
    for tok := rl.NextToken() ; tok.Type != token.EOF ; tok = rl.NextToken() {
        fmt.Println(tok)
    }
    fmt.Println()
}

func (rl *Relexer) Throw(errorID string, tok token.Token, args ...any) {
	rl.Errors = object.Throw(errorID, rl.Errors, tok, args...)
}

func (rl *Relexer) GetErrors() object.Errors {
	rl.Errors = object.MergeErrors(rl.lexer.Ers, rl.Errors)
	return rl.Errors
}