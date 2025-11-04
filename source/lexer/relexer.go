package lexer

// The original relexer. Most of its functions have been transferred either to the bucket chain in
// trelexers.go in this package; or to the parser and initializer. Eventually it should be possible
// to eliminate it completely by dispersing it in this way.

import (
	"github.com/tim-hardcastle/Pipefish/source/dtypes"
	"github.com/tim-hardcastle/Pipefish/source/err"
	"github.com/tim-hardcastle/Pipefish/source/token"
)

var (
	GIVEN        = 0
	FN_REWRITE   = 1
	FN_REWRITTEN = 2
	ASSIGNMENT   = 3
	FOR_REWRITE  = 4
)

type keepTrack struct {
	state int
	depth int
}

type Relexer struct {
	stack                  *dtypes.Stack[keepTrack]
	source                 string
	lexer                  lexer
	mt                     *monotokenizer
	preTok, curTok, nexTok token.Token
	ifLogHappened          bool
	nestingLevel           int
	Errors                 err.Errors
}

func NewRelexer(source, input string) *Relexer {
	l := *NewLexer(source, input)
	mt := makeChain(&l)
	rl := &Relexer{lexer: l,
		mt:     mt,
		source: source,
		preTok: token.Token{token.NEWLINE, ";", 0, 0, 0, "", ""},
		curTok: mt.NextToken(),
		nexTok: mt.NextToken(),
		Errors: []*err.Error{},
		stack:  dtypes.NewStack[keepTrack](),
	}
	return rl
}

func (rl *Relexer) NextToken() token.Token {
	// In this we call NextSemanticToken, which, as its name implies, returns a stream from which the superfluous
	// whitespace has been stripped, and the comments.
	tok := rl.nextSemanticToken()

	switch tok.Type {
	case token.ASSIGN:
		top, ok := rl.stack.HeadValue()
		if ok && top.state == GIVEN {
			tok.Type = token.GVN_ASSIGN
			rl.stack.Push(keepTrack{state: ASSIGNMENT, depth: rl.nestingLevel})
		}
	case token.FOR:
		rl.stack.Push(keepTrack{state: FOR_REWRITE, depth: rl.nestingLevel})
	case token.COLON:
		top, ok := rl.stack.HeadValue()
		if ok && top.state == FN_REWRITE {
			rl.stack.Pop()
			rl.stack.Push(keepTrack{state: FN_REWRITTEN, depth: rl.nestingLevel})
			tok.Type = token.MAGIC_COLON
		}
		if rl.nexTok.Type == token.LOG {
			rl.nexTok.Type = token.PRELOG
		}
		if ok && top.state == FOR_REWRITE && top.depth == rl.nestingLevel {
			rl.stack.Pop()
		}
	case token.LPAREN:
		if tok.Literal == token.LPAREN {
			top, ok := rl.stack.HeadValue()
			if ok && top.state == GIVEN {
				rl.stack.Push(keepTrack{state: FN_REWRITE, depth: rl.nestingLevel})
			}
		}
	case token.GIVEN:
		rl.stack.Push(keepTrack{GIVEN, rl.nestingLevel})
	case token.SEMICOLON:
		if top, ok := rl.stack.HeadValue(); ok && top.state == FOR_REWRITE && top.depth == rl.nestingLevel {
			tok.Type = token.MAGIC_SEMICOLON
		}
	}
	for {
		top, ok := rl.stack.HeadValue()
		if tok.Type == token.NEWLINE && ok && rl.nestingLevel <= top.depth {
			rl.stack.Pop()
		} else {
			break
		}

	}
	return tok
}

func (rl *Relexer) nextSemanticToken() token.Token {
	// So, this is almost all a big case switch on the current token.
	// Depending on what it is, we may return it (as the default), or "burn" it, in which
	// case it disappears so completely it doesn't even become the preTok.

	if rl.nexTok.Type == token.BEGIN &&
		!(rl.curTok.Type == token.GIVEN || rl.curTok.Type == token.PRELOG || rl.curTok.Type == token.COLON ||
			(rl.curTok.Type == token.NEWLINE && (rl.ifLogHappened || (rl.preTok.Type == token.COLON) || (rl.preTok.Type == token.MAGIC_COLON)) ||
				(rl.preTok.Type == token.GIVEN)) || rl.curTok.Type == token.GOLANG) {
		rl.throw("relex/indent", rl.curTok)
	}

	switch rl.curTok.Type {
	case token.PRELOG:
		if rl.nexTok.Type == token.NEWLINE {
			return rl.burnNextToken()
		}
	case token.NEWLINE:
		rl.ifLogHappened = false
		if rl.preTok.Type == token.IFLOG ||
			rl.preTok.Type == token.PRELOG {
			return rl.burnToken()
		}
	case token.COLON:
		if rl.nexTok.Type == token.LOG {
			if rl.nestingLevel == 0 {
				rl.nexTok.Type = token.PRELOG
			} else {
				top, ok := rl.stack.HeadValue()
				if ok && top.state == FN_REWRITE {
					rl.nexTok.Type = token.PRELOG
				} else {
					rl.nexTok.Type = token.IFLOG
					rl.ifLogHappened = true
					return rl.burnToken()
				}
			}
		}
	case token.BEGIN:
		rl.curTok.Type = token.LPAREN
		rl.nestingLevel = rl.nestingLevel + 1
	case token.END:
		rl.curTok.Type = token.RPAREN
		rl.nestingLevel = rl.nestingLevel - 1
	case token.LPAREN:
		rl.nestingLevel = rl.nestingLevel + 1
	case token.RPAREN:
		rl.nestingLevel = rl.nestingLevel - 1
	case token.LOG:
		if rl.preTok.Type == token.COMMA {
			rl.throw("relex/log", rl.curTok)
		}
	}
	rl.getToken() // We shuffle them all along before returning 'cos we sure can't do it afterwards.
	return rl.preTok // Which up until now has been the curTok
}

func (rl *Relexer) getToken() {
	rl.preTok = rl.curTok
	rl.curTok = rl.nexTok
	rl.nexTok = rl.mt.NextToken()
}

func (rl *Relexer) burnToken() token.Token {
	rl.curTok = rl.nexTok
	rl.nexTok = rl.mt.NextToken()
	return rl.nextSemanticToken()
}

func (rl *Relexer) burnNextToken() token.Token {
	rl.nexTok = rl.mt.NextToken()
	return rl.nextSemanticToken()
}

func (rl *Relexer) throw(errorID string, tok token.Token, args ...any) {
	rl.Errors = err.Throw(errorID, rl.Errors, &tok, args...)
}

func (rl *Relexer) GetErrors() err.Errors {
	rl.Errors = err.MergeErrors(rl.lexer.Ers, rl.Errors)
	return rl.Errors
}
