package lexer

import (

	"fmt"
	"strconv"
	"strings"

	"charm/stack"
	"charm/token"
	"charm/text"
)	

type Lexer struct {
	reader          strings.Reader
	input           string
	ch              rune      // current rune under examination
	line		    int       // the line number
	newline         bool      // whether we are at the start of a line and so should be treating whitespace syntactically
	afterWhitespace bool      // whether we are just after the (possible empty) whitespace, so .. is forbidden if not a continuation
	whitespaceStack      stack.Stack  // levels of whitespace to unindent to
}

func New(input string) *Lexer {
	r := *strings.NewReader(input)
	stack := make(stack.Stack, 0)
	stack.Push("")
	l := &Lexer{reader: r,
		        input: input,
				line: 1,
				newline: true, 
				afterWhitespace: true, 
				whitespaceStack: stack,
			}
	l.readChar()
	return l
}


func LexDump(input string) {
	fmt.Print("\nLexer output: \n\n")
	l := New(input)
	for tok := l.NextToken() ; tok.Type != token.EOF ; tok = l.NextToken() {
		fmt.Println(tok)
	}
	fmt.Println()
}

func (l *Lexer) NextNonCommentToken() token.Token {
	for tok := l.NextToken() ; ; tok = l.NextToken() {
		if tok.Type != token.COMMENT {
			return tok
		}
	}

}

func (l *Lexer) NextToken() token.Token {
	var tok token.Token

	if l.newline {
		l.afterWhitespace = true
		return l.interpretWhitespace()
	}

	l.skipWhitespace()

	switch l.ch {
	case '\n':
		tok = l.NewToken(token.NEWLINE, ";")	 
	case '=':
		if l.peekChar() == '=' {
			l.readChar()
			tok = l.NewToken(token.EQ, "==")
		} else {
		    tok = l.NewToken(token.ASSIGN, "=")
		}
	case ';':
		tok = l.NewToken(token.SEMICOLON, ";")
	case ':':
		if l.peekChar() == ':' {
			l.readChar()
			tok = l.NewToken(token.IDENT, "::")
		} else {
			tok = l.NewToken(token.COLON, ":")
		}
	case ',':
		if l.skipWhitespaceAfterPotentialContinuation() {
			tok = l.NewToken(token.COMMA, ",")
		} else {
			tok = l.NewToken(token.ILLEGAL, "a line ending in " + text.Emph(",") +
			   "must be followed by a line beginning with " + text.Emph(".."))
		}
	case '{':
		tok = l.NewToken(token.LBRACE, "{")
	case '}':
		tok = l.NewToken(token.RBRACE, "}")	
	case '[':
		tok = l.NewToken(token.LBRACK, "[")
	case ']':
		tok = l.NewToken(token.RBRACK, "]")
	case '(':
		tok = l.NewToken(token.LPAREN, "(")
	case ')':
		tok = l.NewToken(token.RPAREN, ")")
	case '"':
		tok.Type = token.STRING
		tok.Literal = l.readFormattedString()
	case '`':
		tok.Type = token.STRING
		tok.Literal = l.readPlaintextString()
	case 0:
		tok.Literal = ""
		tok.Type = token.EOF
	case '.':
		if l.peekChar() == '.' {
			l.readChar()
			if l.skipWhitespaceAfterPotentialContinuation() {
				tok = l.NewToken(token.DOTDOT, "..")
			} else {
				tok = l.NewToken(token.ILLEGAL, "A line ending in " + text.Emph("..") + " must be followed by a line beginning with " + text.Emph(".."))
				if l.afterWhitespace {
					tok = l.NewToken(token.ILLEGAL, "A line can never begin with " + text.Emph("..") + " unless it is a continuation")
				}
			}	
		} else {
		    tok = l.NewToken(token.DOT, ".")
		}
	default:
		if l.ch == '/' && l.peekChar() == '/' {
			l.readChar()
			tok = l.NewToken(token.COMMENT, l.readComment())
			l.readChar()
			return tok
		}
		if l.ch == '!' && l.peekChar() == '=' {
			l.readChar()
			l.readChar()
			tok = l.NewToken(token.NOT_EQ, "!=")
			l.afterWhitespace = false
			return tok
		}
		if l.ch == '0' {
			switch l.peekChar() {
			case 'b' : 
				numString := l.readBinaryNumber()
				if num, err := strconv.ParseInt(numString, 2, 64); err == nil {
					tok.Type = token.INT
					tok.Literal = strconv.FormatInt(num, 10)
					tok.Line = l.line
					return tok
				}
				return l.NewToken(token.ILLEGAL, string(numString))
			case 'o' :
				numString := l.readOctalNumber()
				if num, err := strconv.ParseInt(numString, 8, 64); err == nil {
					tok.Type = token.INT
					tok.Literal = strconv.FormatInt(num, 10)
					tok.Line = l.line
					return tok
				}
				return l.NewToken(token.ILLEGAL, string(numString))
			case 'x' : 
				numString := l.readHexNumber()
				if num, err := strconv.ParseInt(numString, 16, 64); err == nil {
					tok.Type = token.INT
					tok.Literal = strconv.FormatInt(num, 10)
					tok.Line = l.line
					return tok
				}
				return l.NewToken(token.ILLEGAL, string(numString))
			}
		}
		if isLegalStart(l.ch) {
			tok.Literal = l.readIdentifier()
			tok.Type = token.LookupIdent(tok.Literal)
			if strings.HasSuffix(tok.Literal, "_") {
				tok.Type = token.ILLEGAL
			}
			tok.Line = l.line
			return tok
		} else if isDigit(l.ch) {

			numString := l.readNumber()
			if _, err := strconv.ParseInt(numString, 0, 64); err == nil {
				tok.Type = token.INT
				tok.Literal = numString
				tok.Line = l.line
				return tok
			}
			if _, err := strconv.ParseFloat(numString, 64); err == nil {
				tok.Type = token.FLOAT
				tok.Literal = numString
				tok.Line = l.line
				return tok
			}
			return l.NewToken(token.ILLEGAL, string(numString))

		} else { // not a digit either
			tok = l.NewToken(token.ILLEGAL, string(l.ch))
		}
	}

	l.readChar()
	l.afterWhitespace = false
	return tok
}


func (l *Lexer) interpretWhitespace() token.Token {
	l.newline = false
	whitespace := ""
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\r' {
		whitespace = whitespace + string(l.ch)
		l.readChar()
	}
	if l.ch == '\n' {
		return l.NewToken(token.DOTDOT, "..")
	}
	if l.ch == '/' && l.peekChar() == '/' {
		l.readChar()
		comment := l.readComment()
		l.readChar()
		return l.NewToken(token.COMMENT, comment)
	}
	previousWhitespace := l.whitespaceStack.HeadValue()
    if whitespace == previousWhitespace {
		return l.NewToken(token.DOTDOT, "..")
	}
	if strings.HasPrefix(whitespace,previousWhitespace) {
		l.whitespaceStack.Push(whitespace)
		return l.NewToken(token.BEGIN, "|->")
	}
	level := l.whitespaceStack.Find(whitespace)
	if level > 0 {
		for i := 0 ; i < level ; i ++ {
			l.whitespaceStack.Pop()
		}
		return l.NewToken(token.END, fmt.Sprint(level))
	}
	return l.NewToken(token.ILLEGAL, 
		fmt.Sprintf("Whitespace in line %d ('%s') is inconsistent with previous indentation levels %s",
		l.line, stack.ExplainWhitespace(whitespace), l.whitespaceStack.ExplainWhitespaceStack()))
}


func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\r'   {
		l.readChar()
	}
}

func (l *Lexer) skipWhitespaceAfterPotentialContinuation() bool {
	for l.peekChar() == ' ' || l.peekChar() == '\t' || l.peekChar() == '\r' {
		l.readChar()
	}
	if l.peekChar() != '\n' {
		return true
	}
	for l.peekChar() == '\n' || l.peekChar() == ' ' || l.peekChar() == '\t' || l.peekChar() == '\r' {
		l.readChar()
	}
	if l.peekChar() != '.' {
		return false
	}
	l.readChar()
	if l.peekChar() != '.' {
		return false
	}
	l.readChar()
	l.newline = false
	return true
}

func (l *Lexer) readChar() {
	if l.ch == '\n' {
		l.line ++
		l.newline = true
	}
	if l.reader.Len() == 0 {
		l.ch = 0
	} else {
		l.ch, _, _ = l.reader.ReadRune() 
	}
}

func (l *Lexer) peekChar() rune {
	if l.reader.Len() == 0 {
		return 0
	} else {
		ru, _, _ := l.reader.ReadRune()
		l.reader.UnreadRune()
		return ru
	}
}

func (l *Lexer) readNumber() string {
	result := ""
	for isDigit(l.ch) || l.ch == '.' {
		result = result + string(l.ch)
		l.readChar()
	}
	return result
}

func (l *Lexer) readBinaryNumber() string {
	result := ""
	l.readChar(); l.readChar()
	for isBinaryDigit(l.ch) {
		result = result + string(l.ch)
		l.readChar()
	}
	return result
}

func (l *Lexer) readOctalNumber() string {
	result := ""
	l.readChar(); l.readChar()
	for isOctalDigit(l.ch) {
		result = result + string(l.ch)
		l.readChar()
	}
	return result
}

func (l *Lexer) readHexNumber() string {
	result := ""
	l.readChar(); l.readChar()
	for isHexDigit(l.ch) {
		result = result + string(l.ch)
		l.readChar()
	}
	return result
}

func (l *Lexer) readComment() string {
	result := ""
	for !(l.peekChar() == '\n' || l.peekChar() == 0) {
		result = result + string(l.peekChar())
		l.readChar()
	}
	return result
}

func (l *Lexer) readFormattedString() string {
	escape := false
	result := ""
	for {
		l.readChar()
		if (l.ch == '"' && !escape) || l.ch == 0 || l.ch == 13 {
			break
		}
		if l.ch == '\\' {
			escape = true
			continue
		}

		charToAdd := l.ch

		if escape {
			escape = false
			switch l.ch {
			case 'n': charToAdd = '\n'
			case 'r': charToAdd = '\r'
			case 't': charToAdd = '\t'
			case '"': charToAdd = '"'
			case '\\': charToAdd = '\\'
			}
		}
		result = result + string(charToAdd)
	}
	return result
}

func (l *Lexer) readPlaintextString() string {
	result := ""
	for {
		l.readChar()
		if l.ch == '`' || l.ch == 0 || l.ch == 13 {
			if l.ch == 13 {result = result + string(l.ch)}
			break
		}
		result = result + string(l.ch)
	}
	return result
}

func (l *Lexer) readIdentifier() string {
	result := ""
	for isLegalNonStart(l.ch) {
		result = result + string(l.ch)
		if (isSymbol(l.ch) != isSymbol(l.peekChar())) && !(isUnderscore(l.ch) || isUnderscore(l.peekChar())) {
			l.readChar()
			break
		}
		l.readChar()
	}
	return result
}

func isLetter(ch rune) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'  || ch == '\'' || ch == '$' || ch == '.'
}

func isUnderscore(ch rune) bool {
	return ch == '_'
}

func isDigit(ch rune) bool {
	return '0' <= ch && ch <= '9'
}

func isBinaryDigit(ch rune) bool {
	return ch == '0' || ch == '1'
}

func isOctalDigit(ch rune) bool {
	return '0' <= ch && ch <= '7'
}

func isHexDigit(ch rune) bool {
	return ('0' <= ch && ch <= '9') || ('a' <= ch && ch <= 'f') || ('A' <= ch && ch <= 'F')
}

func isProtectedPunctuation(ch rune) bool {
	return ch == '(' || ch == ')' || ch == '[' || ch == ']' || ch == '{' || ch == '}' || ch == ' ' || ch == ',' || 
	/**/ ch == ':' || ch == ';' || ch == '\t' || ch == '\n' || ch == 0
}

func isWhitespace(ch rune) bool {
	return ch == 0 || ch == ' ' || ch == '\n' || ch == '\t'
}

func isSymbol(ch rune) bool {
	return !(isLetter(ch) || isDigit(ch) || isProtectedPunctuation(ch))
}

func isLegalStart(ch rune) bool {
	return !(isProtectedPunctuation(ch) || isDigit(ch) || isUnderscore(ch))
}

func isLegalNonStart(ch rune) bool {
	return !(isProtectedPunctuation(ch)) || ch == '$'
}

func (l *Lexer) NewToken(tokenType token.TokenType, st string) token.Token {
	return token.Token{Type: tokenType, Literal: st, Line: l.line}
}

