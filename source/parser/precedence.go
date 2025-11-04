package parser

import (
	"github.com/tim-hardcastle/Pipefish/source/dtypes"
	"github.com/tim-hardcastle/Pipefish/source/token"
)

// Data and fuctions for sorting out the operator precedences.

// NOTE: it may seem weird that the semicolon/newline has a lower precedence than 'given' or the 'magic colon', since these are followed by blocks
// of newline-concatenated expressions. However, these blocks are held together by the indent-outdent bracketing. If the semicolon/newline had
// higher precedence, then something along the lines of:
//
// given :
//     foo(x) :
//         x
//     a = 42
//
// would parse incorrectly because it would try and attach the assignment to the inner function.

const (
	_ int = iota
	LOWEST
	SEMICOLON // semantic newline or ;
	FUNC
	GIVEN           // 'given'
	MAGIC_COLON     // The colon separating the parameters of an inner function from its body.
	WEAK_COLON      // A vile kludge. TODO --- can we get rid of it now?
	GVN_ASSIGN      // Assignments in `given` blocks.
	LOGGING         // Logging statements.
	COLON           // :
	MAGIC_SEMICOLON // For use in headers of `for` blocks.
	ASSIGN          // =
	PIPING          // ->, >>, ?>
	OR              // or
	AND             // and
	NOT             // not
	EQUALS          // == or !=
	LESSGREATER     // > or < or <= or >=
	WEAK_COMMA      // a kludge to let me use Go-like syntax in function definitions --- change to FMIDFIX?
	FPREFIX         // user-defined prefix or function
	FMIDFIX         // user-defined midfix or forefix
	FENDFIX         // user-defined endfix
	COMMA           // ,
	WITH            // with, but ONLY when peeking ahead, otherwise it's an FMIDFIX
	FINFIX          // user-defined infix or ->
	SUM             // + or -
	PRODUCT         // * or / or %
	FSUFFIX         // user-defined suffix, or type in type declaration
	MINUS           //  - as a prefix
	INDEX           // after [
)

var precedences = map[token.TokenType]int{
	token.SEMICOLON: SEMICOLON,
	token.NEWLINE:   SEMICOLON,
	token.GIVEN:     GIVEN,
	// WEAK_COLON
	token.GVN_ASSIGN:      GVN_ASSIGN,
	token.LOG:             LOGGING,
	token.IFLOG:           LOGGING,
	token.PRELOG:          LOGGING,
	token.MAGIC_COLON:     COLON,
	token.COLON:           COLON,
	token.FOR:             GIVEN,
	token.MAGIC_SEMICOLON: MAGIC_SEMICOLON,
	token.ASSIGN:          ASSIGN,
	token.PIPE:            PIPING,
	token.MAPPING:         PIPING,
	token.FILTER:          PIPING,
	token.OR:              OR,
	token.AND:             AND,
	token.NOT:             NOT,
	token.EQ:              EQUALS,
	token.NOT_EQ:          EQUALS,
	// LESSGREATER
	token.VALID:    FPREFIX,
	token.UNWRAP:   FPREFIX,
	token.GLOBAL:   FPREFIX,
	token.EVAL:     FPREFIX,
	token.XCALL:    FPREFIX,
	token.RANGE:    FPREFIX,
	token.CONTINUE: FPREFIX,
	token.BREAK:    FPREFIX,
	// FMIDFIX
	// FENDFIX,
	token.COMMA: COMMA,
	// WITH
	// FINFIX
	// SUM
	// PRODUCT
	token.DOTDOTDOT: FSUFFIX,
	// MINUS     (as prefix)
	token.LBRACK: INDEX,
}

var literals = dtypes.MakeFromSlice([]token.TokenType{token.INT, token.FLOAT, token.STRING, token.RUNE, token.TRUE, token.FALSE, token.ELSE})
var literalsAndLParen = dtypes.MakeFromSlice([]token.TokenType{token.INT, token.FLOAT, token.STRING, token.RUNE, token.TRUE, token.FALSE, token.ELSE,
	token.LPAREN, token.LBRACE, token.EVAL})
var assignmentTokens = dtypes.MakeFromSlice([]token.TokenType{token.ASSIGN, token.GVN_ASSIGN})

func (p *Parser) peekPrecedence() int {
	return p.rightPrecedence(p.PeekToken)
}

func (p *Parser) rightPrecedence(tok token.Token) int {
	if prec, ok := precedences[tok.Type]; ok {
		return prec
	}
	ok, _ := p.CanParse(tok, SUFFIX)
	if ok {
		return FSUFFIX
	}

	return p.leftPrecedence(tok)
}

func (p *Parser) curPrecedence() int {
	return p.leftPrecedence(p.CurToken)
}

func (p *Parser) leftPrecedence(tok token.Token) int {
	if p, ok := precedences[tok.Type]; ok {
		return p
	}
	if tok.Type == token.IDENT {
		ok, _ := p.CanParse(tok, INFIX)
		if ok {
			if tok.Literal == "+" || tok.Literal == "-" {
				return SUM
			}
			if tok.Literal == "mod" {
				return WITH
			}
			if tok.Literal == "*" || tok.Literal == "/" {
				return PRODUCT
			}
			if tok.Literal == "<" || tok.Literal == "<=" || tok.Literal == ">" || tok.Literal == ">=" {
				return LESSGREATER
			}
			if tok.Literal == "in" {
				return EQUALS
			}
			if tok.Literal == "with" || tok.Literal == "without" {
				return FMIDFIX
			}
			return FINFIX
		}
		ok, _ = p.CanParse(tok, PREFIX)
		if ok {
			if tok.Literal == "func" {
				return LOWEST
			}
			return FPREFIX
		}
		if p.didBling(tok.Literal, MIDFIX) {
			return FMIDFIX
		}
		ok, _ = p.CanParse(tok, SUFFIX)
		if ok {
			return FSUFFIX
		}
	}
	return LOWEST
}
