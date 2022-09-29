package lexer

import (
	"fmt"
	"strconv"
	"strings"

	"charm/object"
	"charm/stack"
	"charm/token"
)	

type Lexer struct {
	reader          strings.Reader
	input           string
	ch              rune      // current rune under examination
	line		    int       // the line number
	char			int 	  // the character number
	tstart			int		  // the value of char at the start of a token
	newline         bool      // whether we are at the start of a line and so should be treating whitespace syntactically
	afterWhitespace bool      // whether we are just after the (possible empty) whitespace, so .. is forbidden if not a continuation
	whitespaceStack      stack.Stack[string]  // levels of whitespace to unindent to
	Ers				object.Errors
	source 			string
}

func New(source, input string) *Lexer {
	r := *strings.NewReader(input)
	stack := stack.NewStack[string]();
	stack.Push("")
	l := &Lexer{reader: r,
		        input: input,
				line: 1,
				char: -1,
				newline: true, 
				afterWhitespace: true, 
				whitespaceStack: *stack,
				Ers: []*object.Error{},
				source: source,
			}
	l.readChar()
	return l
}


func LexDump(input string) {
	fmt.Print("\nLexer output: \n\n")
	l := New("", input)
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

	l.tstart = l.char

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
			tok = l.NewToken(token.ILLEGAL, "lex/comma")
			l.Throw("lex/comma", tok)
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
		tok= l.NewToken(token.STRING, "")
		s, ok := l.readFormattedString()
		tok.Literal = s
		if !ok {
			l.Throw("lex/quote/a", tok)
		}
	case '`':
		tok= l.NewToken(token.STRING, ")")
		s, ok := l.readPlaintextString()
		tok.Literal = s
		if !ok {
			l.Throw("lex/quote/b", tok)
		}
	case 0:
		tok = l.NewToken(token.EOF, "EOF")
	case '.':
		if l.peekChar() == '.' {
			l.readChar()
			if l.skipWhitespaceAfterPotentialContinuation() {
				tok = l.NewToken(token.DOTDOT, "..")
			} else {
				tok = l.NewToken(token.ILLEGAL, "lex/cont/a")
				l.Throw("lex/cont/a", tok)
				if l.afterWhitespace {
					tok = l.NewToken(token.ILLEGAL, "lex/cont/b")
					l.Throw("lex/cont/b", tok)
				}
				tok.Line  = l.line
				tok.ChStart = l.tstart
				tok.ChEnd = l.tstart + 2
				return tok
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
		if l.ch == '-' && l.peekChar() == '>' {
			l.readChar()
			tok = l.NewToken(token.RIGHTARROW, "->")
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
					return l.NewToken(token.INT, strconv.FormatInt(num, 10))
				}
				l.Throw("lex/bin", tok, numString)
				return l.NewToken(token.ILLEGAL, "lex/bin")
			case 'o' :
				numString := l.readOctalNumber()
				if num, err := strconv.ParseInt(numString, 8, 64); err == nil {
					return l.NewToken(token.INT, strconv.FormatInt(num, 10))
				}
				l.Throw("lex/oct", tok, numString)
				return l.NewToken(token.ILLEGAL, "lex/oct")
			case 'x' : 
				numString := l.readHexNumber()
				if num, err := strconv.ParseInt(numString, 16, 64); err == nil {
					return l.NewToken(token.INT, strconv.FormatInt(num, 10))
				}
				l.Throw("lex/hex", tok, numString)
				return l.NewToken(token.ILLEGAL, "lex/hex")
			}
		}
		if isLegalStart(l.ch) {
			tok.Literal = l.readIdentifier()
			tok = l.NewToken(token.LookupIdent(tok.Literal), tok.Literal)
			if tok.Type == token.GOLANG {
				tok.Literal = l.readGolang()
			}
			if strings.HasSuffix(tok.Literal, "_") {
				l.Throw("lex/ident/underscore", tok)
				tok = l.NewToken(token.ILLEGAL, "lex/ident/underscore")
			}
			return tok
		} else if isDigit(l.ch) {

			numString := l.readNumber()
			if _, err := strconv.ParseInt(numString, 0, 64); err == nil {
				return l.NewToken(token.INT, numString)
			}
			if _, err := strconv.ParseFloat(numString, 64); err == nil {
				return l.NewToken(token.FLOAT, numString)
			}
			l.Throw("lex/num", tok, numString)
			return l.NewToken(token.ILLEGAL, "lex/num")

		} else { // not a digit either
			l.Throw("lex/ill", tok, l.ch)
			tok = l.NewToken(token.ILLEGAL, "lex/ill")
		}
	}
	tok.Line = l.line; tok.ChStart = l.tstart; tok.ChEnd = l.char
	l.readChar()
	l.afterWhitespace = false
	return tok
}


func (l *Lexer) interpretWhitespace() token.Token {
	l.newline = false
	whitespace := ""
	for l.ch == ' ' || l.ch == '\t' {
		whitespace = whitespace + string(l.ch)
		l.readChar()
	}
	if l.ch == '\n' {
		return l.NewToken(token.NO_INDENT, "|||")
	}
	if l.ch == '/' && l.peekChar() == '/' {
		l.readChar()
		comment := l.readComment()
		l.readChar()
		return l.NewToken(token.COMMENT, comment)
	}
	if l.ch == '.' && l.peekChar() == '.' {
		l.readChar()
		l.readChar()
		l.Throw("lex/cont/c", l.NewToken(token.ILLEGAL, "lex/cont/c"))
		return l.NewToken(token.ILLEGAL, "lex/cont/c")
	}
	previousWhitespace, _ := l.whitespaceStack.HeadValue()
    if whitespace == previousWhitespace {
		return l.NewToken(token.NO_INDENT, "|||")
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
	l.Throw("lex/wsp", l.NewToken(token.ILLEGAL, "lex/wsp"), describeWhitespace(whitespace))
	return l.NewToken(token.ILLEGAL, "lex/wsp")
}

func describeWhitespace(s string) string {
	result := ""
	cur := '#'
	count := 0
	for i, ch := range(s) {
		if ch != cur || i == len(s) - 1 {
			if cur == ' ' {
				result = result + strconv.Itoa(count) + " " + "space"
				if count > 1 { result = result + "s" }
				if i < len(s) - 1 { result = result + ", " }
				cur = ch
				count = 1
				continue
			}
			if cur == '\t' {
				result = result + strconv.Itoa(count) + " " + "tab"
				if count > 1 { result = result + "s" }
				if i < len(s) - 1 { result = result + ", " }
				cur = ch
				count = 1
				continue
			}
		}
	}
	if result == "" { result = "no indentation" }
	return result
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
	l.char ++
	if l.ch == '\n' {
		l.line ++
		l.newline = true
		l.char = 0
		l.tstart = 0
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

func (l *Lexer) readGolang() string {
	result := ""
	for (l.peekChar() == ' ' || l.peekChar() == '\t') {
		l.readChar()
	}
	// We expect a brace after the golang keyword.
	if l.peekChar() != '{' && l.peekChar() != '"'  && l.peekChar() != '`' {
		l.Throw("lex/char", l.NewToken(token.ILLEGAL, "lex/char"))
		return ""
	}
	if l.peekChar() == '"' {
		l.readChar()
		s, ok := l.readFormattedString()
		if !ok {
			l.Throw("lex/quote/c", l.NewToken(token.ILLEGAL, "lex/quote/c"))
		}
		l.readChar()
		return s
	}
	if l.peekChar() == '`' {
		l.readChar()
		s, ok := l.readPlaintextString()
		if !ok {
			l.Throw("lex/quote/d", l.NewToken(token.ILLEGAL, "lex/quote/d"))
		}
		l.readChar()
		return s
	}
	// So now we look for the closing brace, ignoring those inside strings
	l.readChar()
	braces := 1
	escaped := false
	quote := ' '
	for (braces > 0) {
		l.readChar()
		if l.ch == 0 { break }	
		if !escaped {
			if l.ch == '"' || l.ch == '`' || l.ch == '\'' {
				if quote == ' ' {
					quote = l.ch
				} else {
					if quote == l.ch {
						quote = ' '
					}
				}
			}
			if l.ch == '\\' {
				escaped = true
			}
			if l.ch == '{' && quote == ' ' {
				braces ++
			}
			if l.ch == '}' && quote == ' ' {
				braces --
			}
		}
		result = result + string(l.ch)
	}
	l.readChar()
	return result
}

func (l *Lexer) readFormattedString() (string, bool) {
	escape := false
	result := ""
	for {
		l.readChar()
		if (l.ch == '"' && !escape) || l.ch == 0 || l.ch == 13 || l.ch == 10{
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
	if l.ch == 13  || l.ch == 0 || l.ch == 10 {
		return result, false
	}
	return result, true
}

func (l *Lexer) readPlaintextString() (string, bool) {
	result := ""
	for {
		l.readChar()
		if l.ch == '`' || l.ch == 0 || l.ch == 13 || l.ch == 10 {
			break
		}
		result = result + string(l.ch)
	}
	if l.ch == 13  || l.ch == 0 || l.ch == 10 {
		return result, false
	}
	return result, true
}

func (l *Lexer) readIdentifier() string {
	result := ""
	for isLegalNonStart(l.ch) {
		result = result + string(l.ch)
		if (isSymbol(l.ch) != isSymbol(l.peekChar())) && !(isPeriod(l.ch) || isPeriod(l.peekChar())) {
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

func isPeriod(ch rune) bool {
	return ch == '.'
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
	     ch == ':' || ch == ';' || ch == '\t' || ch == '\n' || ch == 0
}

func isWhitespace(ch rune) bool {
	return ch == 0 || ch == ' ' || ch == '\n' || ch == '\t'
}

func isSymbol(ch rune) bool {
	return !(isLetter(ch) || isDigit(ch) || isProtectedPunctuation(ch))
}

func isLegalStart(ch rune) bool {
	return !(isProtectedPunctuation(ch) || isDigit(ch) || isPeriod(ch))
}

func isLegalNonStart(ch rune) bool {
	return !(isProtectedPunctuation(ch)) || ch == '$'
}

func (l *Lexer) NewToken(tokenType token.TokenType, st string) token.Token {
	return token.Token{Type: tokenType, Literal: st, Source: l.source, Line: l.line, ChStart: l.tstart, ChEnd: l.char}
}

func (l *Lexer) Throw(errorID string, tok token.Token, args ...any) {
	l.Ers = object.Throw(errorID, l.Ers, tok, args...)
}