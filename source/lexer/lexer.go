package lexer

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"

	"github.com/tim-hardcastle/Pipefish/source/dtypes"
	"github.com/tim-hardcastle/Pipefish/source/err"
	"github.com/tim-hardcastle/Pipefish/source/settings"
	"github.com/tim-hardcastle/Pipefish/source/token"
)

type Lexer struct {
	reader            strings.Reader
	input             string
	ch                rune                 // current rune under examination
	line              int                  // the line number
	char              int                  // the character number
	tstart            int                  // the value of char at the start of a token
	newline           bool                 // whether we are at the start of a line and so should be treating whitespace syntactically
	afterWhitespace   bool                 // whether we are just after the (possible empty) whitespace, so .. is forbidden if not a continuation
	whitespaceStack   dtypes.Stack[string] // levels of whitespace to unindent to
	Ers               err.Errors
	source            string
}

func NewLexer(source, input string) *Lexer {
	r := *strings.NewReader(input)
	stack := dtypes.NewStack[string]()
	stack.Push("")
	l := &Lexer{reader: r,
		input:           input,
		line:            1,
		char:            -1,
		whitespaceStack: *stack,
		Ers:             []*err.Error{},
		source:          source,
	}
	return l
}

func (l *Lexer) getTokens() []token.Token {

	if l.newline {
		l.afterWhitespace = true
		l.newline = false
		return l.interpretWhitespace()
	}
	l.newline = false
	l.skipWhitespace()
	l.tstart = l.char

	switch l.ch {
	case 0:
		level := l.whitespaceStack.Find("")
		if level > 0 {
			for i := 0; i < level; i++ {
				l.whitespaceStack.Pop()
			}
			return []token.Token{l.MakeToken(token.END, fmt.Sprint(level))}
		} else {
			return []token.Token{l.NewToken(token.EOF, "EOF")}
		}
	case '\n':
		return []token.Token{l.NewToken(token.NEWLINE, ";")}
	case '\\':
		if l.peekChar() == '\\' {
			l.readChar()
			return []token.Token{l.NewToken(token.LOG, strings.TrimSpace(l.readComment()))}
		}
	case ';':
		return []token.Token{l.NewToken(token.SEMICOLON, ";")}
	case ':':
		if l.peekChar() == ':' {
			l.readChar()
			return []token.Token{l.NewToken(token.IDENT, "::")} // We return []token.Token{this as a regular identifier so we can define the '::' operator as a builtin.
		} else {
			return []token.Token{l.NewToken(token.COLON, ":")}
		}
	case '=':
		if l.peekChar() == '=' {
			l.readChar()
			return []token.Token{l.NewToken(token.EQ, "==")} // We return []token.Token{this as a regular identifier so we can define the '::' operator as a builtin.
		} else {
			return []token.Token{l.NewToken(token.ASSIGN, "=")}
		}
	case '?':
		if l.peekChar() == '>' {
			l.readChar()
			return []token.Token{l.NewToken(token.FILTER, "?>")} // We return []token.Token{this as a regular identifier so we can define the '::' operator as a builtin.
		}
	case ',':
		if l.skipWhitespaceAfterPotentialContinuation() {
			return []token.Token{l.NewToken(token.COMMA, ",")}
		} else {
			return []token.Token{l.Throw("lex/comma")}
		}
	case '{':
		return []token.Token{l.NewToken(token.LBRACE, "{")}
	case '}':
		return []token.Token{l.NewToken(token.RBRACE, "}")}
	case '[':
		return []token.Token{l.NewToken(token.LBRACK, "[")}
	case ']':
		return []token.Token{l.NewToken(token.RBRACK, "]")}
	case '(':
		return []token.Token{l.NewToken(token.LPAREN, "(")}
	case ')':
		return []token.Token{l.NewToken(token.RPAREN, ")")}
	case '"':
		s, ok := l.readFormattedString()
		if !ok {
			return []token.Token{l.Throw("lex/quote/a")}
		}
		return []token.Token{l.NewToken(token.STRING, s)}
	case '`':
		s, ok := l.readPlaintextString()
		if !ok {
			return []token.Token{l.Throw("lex/quote/b")}
		}
		return []token.Token{l.NewToken(token.STRING, s)}
	case '\'':
		r, ok := l.readRune()
		if !ok {
			return []token.Token{l.Throw("lex/quote/rune")}
		}
		return []token.Token{l.NewToken(token.RUNE, r)}
	case '.':
		if l.peekChar() == '.' {
			l.readChar()
			if l.peekChar() == '.' {
				l.readChar()
				return []token.Token{l.NewToken(token.DOTDOTDOT, "...")}
			}
			if l.skipWhitespaceAfterPotentialContinuation() {
				return []token.Token{l.NewToken(token.DOTDOT, "..")}
			} else {
				return []token.Token{l.Throw("lex/cont/a")}
			}
		} else {
			return []token.Token{l.NewToken(token.NAMESPACE_SEPARATOR, ".")}
		}
	}

	// We may have a comment.
	if l.ch == '/' && l.peekChar() == '/' {
		l.readChar()
		return []token.Token{l.NewToken(token.COMMENT, l.readComment())}
	}

	// We may have a binary, octal, or hex literal.
	if l.ch == '0' {
		switch l.peekChar() {
		case 'b':
			numString := l.readBinaryNumber()
			if num, err := strconv.ParseInt(numString, 2, 64); err == nil {
				return []token.Token{l.NewToken(token.INT, strconv.FormatInt(num, 10))}
			}
			return []token.Token{l.Throw("lex/bin", numString)}
		case 'o':
			numString := l.readOctalNumber()
			if num, err := strconv.ParseInt(numString, 8, 64); err == nil {
				return []token.Token{l.NewToken(token.INT, strconv.FormatInt(num, 10))}
			}
			return []token.Token{l.Throw("lex/oct", numString)}
		case 'x':
			numString := l.readHexNumber()
			if num, err := strconv.ParseInt(numString, 16, 64); err == nil {
				return []token.Token{l.NewToken(token.INT, strconv.FormatInt(num, 10))}
			}
			return []token.Token{l.Throw("lex/hex", numString)}
		}
	}

	// We may have a normal base-ten literal.
	if isDigit(l.ch) {
		numString := l.readNumber()
		if _, err := strconv.ParseInt(numString, 0, 64); err == nil {
			return []token.Token{l.NewToken(token.INT, numString)}
		}
		if _, err := strconv.ParseFloat(numString, 64); err == nil {
			return []token.Token{l.NewToken(token.FLOAT, numString)}
		}
		return []token.Token{l.Throw("lex/num", numString)}
	}

	// We may have an identifier, a golang block, or a snippet.
	if isLegalStart(l.ch) {
		lit := l.readIdentifier()
		tType := token.LookupIdent(lit)
		switch tType {
		case token.GOCODE:
			text := l.readGolang()
			return []token.Token{l.NewToken(tType, text), l.NewToken(token.NEWLINE, ";")}
		case token.EMDASH:
			str, outdent := l.readSnippet()
			if outdent <= 0 {
				return []token.Token{l.MakeToken(tType, strings.TrimSpace(str)),
					l.MakeToken(token.NEWLINE, ";")}
			} else {
				return []token.Token{l.MakeToken(tType, strings.TrimSpace(str)), 
					l.MakeToken(token.END, fmt.Sprint(outdent)),
					l.MakeToken(token.NEWLINE, ";")}
			}
		default:
			return []token.Token{l.NewToken(tType, lit)}
		}
	}

	// Or we have nothing recognizable.
	return []token.Token{l.Throw("lex/ill", l.ch)}
}

func (l *Lexer) interpretWhitespace() []token.Token {

	l.newline = false
	whitespace := ""

	for l.ch == ' ' || l.ch == '\t' {
		whitespace = whitespace + string(l.ch)
		l.readChar()
	}
	if l.ch == '\n' {
		return []token.Token{}
	}
	if l.ch == '/' && l.peekChar() == '/' {
		l.readChar()
		comment := l.readComment()
		l.readChar()
		return []token.Token{l.NewToken(token.COMMENT, comment)}
	}
	if l.ch == '.' && l.peekChar() == '.' {
		l.readChar()
		l.readChar()
		return []token.Token{l.Throw("lex/cont/b")}
	}
	previousWhitespace, _ := l.whitespaceStack.HeadValue()
	if whitespace == previousWhitespace {
		return []token.Token{}
	}
	if strings.HasPrefix(whitespace, previousWhitespace) {
		l.whitespaceStack.Push(whitespace)
		return []token.Token{l.MakeToken(token.BEGIN, "|->")}
	}
	level := l.whitespaceStack.Find(whitespace)
	if level > 0 {
		for i := 0; i < level; i++ {
			l.whitespaceStack.Pop()
		}
		return []token.Token{l.MakeToken(token.END, fmt.Sprint(level))}
	}
	return []token.Token{l.Throw("lex/wsp", describeWhitespace(whitespace))}
}

var whitespaceDescriptions = map[rune]string{' ': "space", '\n': "newline", '\t': "tab"}

func describeWhitespace(s string) string {
	if len(s) == 0 {
		return "no indentation"
	}
	if len(s) == 1 {
		return "1 " + whitespaceDescriptions[rune(s[0])]
	}
	result := ""
	cur := rune(s[0]) //
	count := 1
	for i, ch := range s[1:] {
		if ch != cur || i == len(s)-2 {
			if i == len(s)-2 {
				count++
			}
			singular := whitespaceDescriptions[ch]
			result = result + strconv.Itoa(count) + " " + singular
			if count > 1 {
				result = result + "s"
			}
			if i < len(s)-1 {
				result = result + ", "
			}
			cur = ch
			count = 1
		} else {
			count++
		}
	}
	return result
}

func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\r' {
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

	l.char++
	if l.ch == '\n' {
		l.line++
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
	result := string(l.ch)
	for isDigit(l.peekChar()) || l.peekChar() == '.' {
		l.readChar()
		result = result + string(l.ch)
	}
	return result
}

func (l *Lexer) readBinaryNumber() string {
	result := ""
	l.readChar()
	for isBinaryDigit(l.peekChar()) {
		l.readChar()
		result = result + string(l.ch)
	}
	return result
}

func (l *Lexer) readOctalNumber() string {
	result := ""
	l.readChar()
	for isOctalDigit(l.peekChar()) {
		l.readChar()
		result = result + string(l.ch)
	}
	return result
}

func (l *Lexer) readHexNumber() string {
	result := ""
	l.readChar()
	for isHexDigit(l.peekChar()) {
		l.readChar()
		result = result + string(l.ch)
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

func (l *Lexer) readSnippet() (string, int) {
	// Note --- what we return from this is followed by makeToken, which doesn't read a character, rather than NewToken, which does.
	// This is because we may end up in a position where we've just realized that we've unindented. (See other use of MakeChar.)
	result := ""
	for l.peekChar() == ' ' || l.peekChar() == '\t' { // We consume the whitespace between the emdash and either a non-whitespace character or a newline.
		l.readChar()
	}
	// There are two possibilities. Either we found a non-whitespace character, and the whole snippet is on the same line as the
	// `---` token, or we found a newline and the snippet is an indent on the succeeding lines. Just like with a colon.
	if l.peekChar() == '\n' || l.peekChar() == '\r' { // --- then we have to mess with whitespace.
		for l.peekChar() == '\n' || l.peekChar() == '\r' {
			l.readChar()
		}
		langIndent := ""
		stackTop, ok := l.whitespaceStack.HeadValue()
		if !ok {
			stackTop = ""
		}
		for {
			currentWhitespace := ""
			for (l.peekChar() == '\t' || l.peekChar() == ' ') && !(langIndent != "" && currentWhitespace == langIndent) {
				l.readChar()
				currentWhitespace = currentWhitespace + string(l.ch)
			}
			if langIndent == "" { // Then this is the first time around.
				if currentWhitespace == "" {
					l.Throw("lex/emdash/indent/a", l.NewToken(token.ILLEGAL, "bad emdash"))
					return result, -1
				}
				langIndent = currentWhitespace
				if langIndent == stackTop {
					l.Throw("lex/emdash/indent/b", l.NewToken(token.ILLEGAL, "bad emdash"))
					return result, -1
				}
			}
			if strings.HasPrefix(stackTop, currentWhitespace) || currentWhitespace == "\n" || currentWhitespace == "\r" || currentWhitespace == string(rune(0)) { // Then we've unindented. Dobby is free!
				if currentWhitespace == "\n" || currentWhitespace == "\r" || currentWhitespace == string(rune(0)) {
					currentWhitespace = ""
				}
				outdent := l.whitespaceStack.Find(currentWhitespace)
				l.whitespaceStack.Take(outdent)
				return result, outdent
			}
			if !strings.HasPrefix(currentWhitespace, stackTop) && !(currentWhitespace == "\n") {
				l.Throw("lex/emdash/indent/c", l.NewToken(token.ILLEGAL, "bad emdash"))
				return result, -1
			}
			for l.peekChar() != '\n' && l.peekChar() != '\r' && l.peekChar() != 0 {
				l.readChar()
				result = result + string(l.ch)
			}
			if l.peekChar() == 0 {
				l.readChar()
				cstackHeight := l.whitespaceStack.Find("")
				l.whitespaceStack.Take(cstackHeight)
				return result, cstackHeight
			}
			l.readChar()
			result = result + "\n"
		}
	} else {
		for l.peekChar() != '\n' && l.peekChar() != '\r' && l.peekChar() != 0 {
			l.readChar()
			result = result + string(l.ch)
		}
		l.readChar()
		return result, -1
	}
}

func (l *Lexer) readGolang() string {
	result := ""
	for l.peekChar() == ' ' || l.peekChar() == '\t' { // Get rid of the whitespace between 'golang' and whatever follows it.
		l.readChar()
	}
	// We expect a brace or quotes after the golang keyword. (The quotes if it's in the import section, import golang "foo".)
	if l.peekChar() != '{' && l.peekChar() != '"' && l.peekChar() != '`' {
		l.Throw("lex/golang", l.NewToken(token.ILLEGAL, "bad golang"))
		return ""
	}
	if l.peekChar() == '"' {
		l.readChar()
		s, ok := l.readFormattedString()
		if !ok {
			l.Throw("lex/quote/c", l.NewToken(token.ILLEGAL, "bad quote"))
		}
		return s
	}
	if l.peekChar() == '`' {
		l.readChar()
		s, ok := l.readPlaintextString()
		if !ok {
			l.Throw("lex/quote/d", l.NewToken(token.ILLEGAL, "bad quote"))
		}
		return s
	}

	l.readChar() // Skips over the opening brace.

	// We just have to look for a closing brace on the left of a line.
	for !(l.ch == 0) && !(l.ch == '\n' && l.peekChar() == '}') {
		l.readChar()
		result = result + string(l.ch)
	}
	return result
}

func (l *Lexer) readRune() (string, bool) {
	escape := false
	result := ""
	for {
		l.readChar()
		if (l.ch == '\'' && !escape) || l.ch == 0 || l.ch == 13 || l.ch == 10 {
			break
		}
		if l.ch == '\\' && !escape {
			escape = true
			continue
		}

		charToAdd := l.ch

		if escape {
			escape = false
			switch l.ch {
			case 'n':
				charToAdd = '\n'
			case 'r':
				charToAdd = '\r'
			case 't':
				charToAdd = '\t'
			case '\'':
				charToAdd = '\''
			case '\\':
				charToAdd = '\\'
			case 'e':
				charToAdd = '\033'
			}
		}
		result = result + string(charToAdd)
	}
	if l.ch == 13 || l.ch == 0 || l.ch == 10 {
		return result, false
	}
	if utf8.RuneCountInString(result) != 1 {
		return result, false
	}
	return result, true
}

func (l *Lexer) readFormattedString() (string, bool) {
	escape := false
	result := ""
	for {
		l.readChar()
		if (l.ch == '"' && !escape) || l.ch == 0 || l.ch == 13 || l.ch == 10 {
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
			case 'n':
				charToAdd = '\n'
			case 'r':
				charToAdd = '\r'
			case 't':
				charToAdd = '\t'
			case '"':
				charToAdd = '"'
			case '\\':
				charToAdd = '\\'
			case 'e':
				charToAdd = '\033'
			}
		}
		result = result + string(charToAdd)
	}
	if l.ch == 13 || l.ch == 0 || l.ch == 10 {
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
	if l.ch == 13 || l.ch == 0 || l.ch == 10 {
		return result, false
	}
	return result, true
}

func (l *Lexer) readIdentifier() string {
	result := string(l.ch) // i.e. the character that suggested this was an identifier.
	for !l.atBoundary() {
		l.readChar()
		result = result + string(l.ch)
	}
	return result
}

func isLetter(ch rune) bool {
	return unicode.IsLetter(ch) 
}

func isUnderscore(ch rune) bool {
	return ch == '_'
}

func isDigit(ch rune) bool {
	return unicode.IsNumber(ch)
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

func isProtectedPunctuationOrWhitespace(ch rune) bool {
	return ch == '(' || ch == ')' || ch == '[' || ch == ']' || ch == '{' || ch == '}' || ch == ' ' || ch == ',' ||
		ch == ':' || ch == ';' || ch == '.' || ch == '\t' || ch == '\n' || ch == '\r' || ch == 0
}

func isSymbol(ch rune) bool {
	return !(isUnderscore(ch) || isLetter(ch) || isDigit(ch) || isProtectedPunctuationOrWhitespace(ch))
}

func isLegalStart(ch rune) bool {
	return !(isProtectedPunctuationOrWhitespace(ch) || isDigit(ch))
}

// Finds if we're at the end of an identifier.
func (l *Lexer) atBoundary() bool {
	pc := l.peekChar()
	return isProtectedPunctuationOrWhitespace(pc) || 
		isLetter(l.ch) && !(isLetter(pc) || isUnderscore(pc)) ||
		isDigit(l.ch) && !(isDigit(pc) || isUnderscore(pc)) ||
		isSymbol(l.ch) && !(isSymbol(pc) || isUnderscore(pc))
}

func (l *Lexer) NewToken(tokenType token.TokenType, st string) token.Token {
	l.readChar()
	l.afterWhitespace = false
	return l.MakeToken(tokenType, st)
}

func (l *Lexer) MakeToken(tokenType token.TokenType, st string) token.Token {
	if settings.SHOW_LEXER && !(settings.IGNORE_BOILERPLATE && settings.ThingsToIgnore.Contains(l.source)) {
		fmt.Println(tokenType, st)
	}
	return token.Token{Type: tokenType, Literal: st, Source: l.source, Line: l.line, ChStart: l.tstart, ChEnd: l.char}
}

func (l *Lexer) Throw(errorID string, args ...any) token.Token {
	tok := l.MakeToken(token.ILLEGAL, errorID)
	l.Ers = err.Throw(errorID, l.Ers, &tok, args...)
	return tok
}


