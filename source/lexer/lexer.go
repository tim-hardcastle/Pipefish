package lexer

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"

	"github.com/tim-hardcastle/pipefish/source/dtypes"
	"github.com/tim-hardcastle/pipefish/source/err"
	"github.com/tim-hardcastle/pipefish/source/settings"
	"github.com/tim-hardcastle/pipefish/source/token"
)

type lexer struct {
	runes            *RuneSupplier
	reader           strings.Reader
	tstart           int // the value of char at the start of a token
	lineNo           int
	afterWhitespace  bool // whether we are just after the (possible empty) whitespace, so .. is forbidden if not a continuation
	continuation     bool
	whitespaceStack  dtypes.Stack[string] // levels of whitespace to unindent to
	Ers              err.Errors
	source           string
	currentNamespace string
}

func NewLexer(source, input string) *lexer {
	r := *strings.NewReader(input)
	stack := dtypes.NewStack[string]()
	stack.Push("")
	l := &lexer{reader: r,
		runes:           NewRuneSupplier([]rune(input)),
		whitespaceStack: *stack,
		Ers:             []*err.Error{},
		source:          source,
		lineNo:          1,
	}
	return l
}

func (l *lexer) getTokens() []token.Token {
	if l.lineNo < l.runes.lineNo && !l.continuation {
		l.afterWhitespace = true
		l.lineNo = l.runes.lineNo
		return l.interpretWhitespace()
	}
	l.continuation = false
	l.skipWhitespace()
	l.lineNo, l.tstart = l.runes.Position()
	switch l.runes.CurrentRune() {
	case 0:
		level := l.whitespaceStack.Find("")
		if level > 0 {
			for range level {
				l.whitespaceStack.Pop()
			}
			return l.makeEnds(level)
		} else {
			return []token.Token{l.NewToken(token.EOF, "EOF")}
		}
	case '\n':
		return []token.Token{l.NewToken(token.NEWLINE, ";")}
	case '\\':
		if l.runes.PeekRune() == '\\' {
			l.runes.Next()
			return []token.Token{l.NewToken(token.LOG, strings.TrimSpace(l.runes.ReadComment()))}
		}
	case ';':
		return []token.Token{l.NewToken(token.SEMICOLON, ";")}
	case ':':
		if l.runes.PeekRune() == ':' {
			l.runes.Next()
			return []token.Token{l.NewToken(token.IDENT, "::")} // We return []token.Token{this as a regular identifier so we can define the '::' operator as a builtin.
		} else {
			return []token.Token{l.NewToken(token.COLON, ":")}
		}
	case '=':
		if l.runes.PeekRune() == '=' {
			l.runes.Next()
			return []token.Token{l.NewToken(token.EQ, "==")} // We return []token.Token{this as a regular identifier so we can define the '::' operator as a builtin.
		} else {
			return []token.Token{l.NewToken(token.ASSIGN, "=")}
		}
	case '?':
		if l.runes.PeekRune() == '>' {
			l.runes.Next()
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
	case '~':
		if l.runes.PeekRune() == '~' {
			l.runes.Next()
			docString := l.runes.ReadComment()
			l.runes.Next()
			return []token.Token{l.NewToken(token.DOCSTRING, strings.TrimSpace(docString)+"\n")}
		}
	// We may have a formated string.
	case '"':
		s, ok := l.runes.ReadFormattedString()
		if !ok {
			return []token.Token{l.Throw("lex/quote/a")}
		}
		return []token.Token{l.NewToken(token.STRING, s)}
	// Or a plaintext string.
	case '`':
		s, ok := l.runes.ReadPlaintextString()
		if !ok {
			return []token.Token{l.Throw("lex/quote/b")}
		}
		return []token.Token{l.NewToken(token.STRING, s)}
	// Or a rune.
	case '\'':
		r, hasSingleQuote, hasLengthOne := l.runes.ReadRuneLiteral()
		if !(hasSingleQuote && hasLengthOne) {
			return []token.Token{l.Throw("lex/quote/rune")}
		}
		return []token.Token{l.NewToken(token.RUNE, r)}
	// We deal with the `.`, `..` and `...` cases.
	case '.':
		if l.runes.PeekRune() == '.' {
			l.runes.Next()
			if l.runes.PeekRune() == '.' {
				l.runes.Next()
				return []token.Token{l.NewToken(token.DOTDOTDOT, "...")}
			}
			if l.skipWhitespaceAfterPotentialContinuation() {
				l.runes.Next()
				return []token.Token{}
			} else {
				return []token.Token{l.Throw("lex/cont/a")}
			}
		} else {
			return []token.Token{l.Throw("lex/dot")}
		}
	}

	// We may have a comment.
	if l.runes.CurrentRune() == '/' && l.runes.PeekRune() == '/' {
		l.runes.Next()
		return []token.Token{l.NewToken(token.COMMENT, l.runes.ReadComment())}
	}

	// We may have a binary, octal, or hex literal.
	if l.runes.CurrentRune() == '0' {
		switch l.runes.PeekRune() {
		case 'b', 'B':
			numString := l.runes.ReadBinaryNumber()
			if num, err := strconv.ParseInt(numString, 2, 64); err == nil {
				return []token.Token{l.NewToken(token.INT, strconv.FormatInt(num, 10))}
			}
			return []token.Token{l.Throw("lex/bin", numString)}
		case 'o', 'O':
			numString := l.runes.ReadOctalNumber()
			if num, err := strconv.ParseInt(numString, 8, 64); err == nil {
				return []token.Token{l.NewToken(token.INT, strconv.FormatInt(num, 10))}
			}
			return []token.Token{l.Throw("lex/oct", numString)}
		case 'x', 'X':
			numString := l.runes.ReadHexNumber()
			if num, err := strconv.ParseInt(numString, 16, 64); err == nil {
				return []token.Token{l.NewToken(token.INT, strconv.FormatInt(num, 10))}
			}
			return []token.Token{l.Throw("lex/hex", numString)}
		}
	}

	// We may have a normal base-ten literal.
	if IsDigit(l.runes.CurrentRune()) {
		numString := l.runes.ReadNumber()
		if _, err := strconv.ParseInt(numString, 0, 64); err == nil {
			return []token.Token{l.NewToken(token.INT, numString)}
		}
		if _, err := strconv.ParseFloat(numString, 64); err == nil {
			return []token.Token{l.NewToken(token.FLOAT, numString)}
		}
		return []token.Token{l.Throw("lex/num", numString)}
	}

	// We may have an identifier, a golang block, or a snippet.
	if IsLegalStart(l.runes.CurrentRune()) {
		lit := l.runes.ReadIdentifier()
		tType := token.LookupIdent(lit)
		switch tType {
		case token.GOLANG:
			text := l.readGolang()
			return []token.Token{l.NewToken(tType, text), l.NewToken(token.NEWLINE, ";")}
		case token.EMDASH:
			str, outdent := l.readSnippet()
			if outdent <= 0 {
				return []token.Token{l.MakeToken(tType, strings.TrimSpace(str)),
					l.MakeToken(token.NEWLINE, ";")}
			} else {
				result := []token.Token{l.MakeToken(tType, strings.TrimSpace(str))}
				result = append(result, l.makeEnds(outdent)...)
				result = append(result, l.MakeToken(token.NEWLINE, ";"))
				return result
			}
		default:
			if l.runes.PeekRune() == '.' {
				if tType != token.IDENT {
					l.Throw("lex/namespace/left")
				}
				l.currentNamespace = l.currentNamespace + lit + "."
				l.runes.Next()
				l.runes.Next()
				return []token.Token{}
			} else {
				tokenIs := l.NewToken(tType, lit)
				if l.currentNamespace != "" {
					if tType != token.IDENT {
						l.Throw("lex/namespace/right")
					}
					tokenIs.Namespace = l.currentNamespace
					l.currentNamespace = ""
				}
				return []token.Token{tokenIs}
			}
		}
	}

	// Or we have nothing recognizable.
	return []token.Token{l.Throw("lex/ill", l.runes.CurrentRune())}
}

func (l *lexer) interpretWhitespace() []token.Token {
	whitespace := ""
	for l.runes.CurrentRune() == ' ' || l.runes.CurrentRune() == '\t' {
		whitespace = whitespace + string(l.runes.CurrentRune())
		l.runes.Next()
	}
	if l.runes.CurrentRune() == '\n' {
		return []token.Token{}
	}
	if l.runes.CurrentRune() == '/' && l.runes.PeekRune() == '/' {
		l.runes.Next()
		comment := l.runes.ReadComment()
		l.runes.Next()
		return []token.Token{l.NewToken(token.COMMENT, comment)}
	}
	if l.runes.CurrentRune() == '.' && l.runes.PeekRune() == '.' {
		l.runes.Next()
		l.runes.Next()
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
		for range level {
			l.whitespaceStack.Pop()
		}
		result := append(l.makeEnds(level), l.MakeToken(token.NEWLINE, ";"))
		return result
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

func (l *lexer) skipWhitespace() {
	for l.runes.CurrentRune() == ' ' || l.runes.CurrentRune() == '\t' || l.runes.CurrentRune() == '\r' {
		l.runes.Next()
	}
}

func (l *lexer) skipWhitespaceAfterPotentialContinuation() bool {
	for l.runes.PeekRune() == ' ' || l.runes.PeekRune() == '\t' || l.runes.PeekRune() == '\r' {
		l.runes.Next()
	}
	if l.runes.PeekRune() != '\n' {
		l.continuation = true
		return true
	}
	for l.runes.PeekRune() == '\n' || l.runes.PeekRune() == ' ' || l.runes.PeekRune() == '\t' || l.runes.PeekRune() == '\r' {
		l.runes.Next()
	}
	if l.runes.PeekRune() != '.' {
		return false
	}
	l.runes.Next()
	if l.runes.PeekRune() != '.' {
		return false
	}
	l.runes.Next()
	l.continuation = true
	return true
}

func (runes *RuneSupplier) ReadNumber() string {
	result := string(runes.CurrentRune())
	for IsDigit(runes.PeekRune()) || runes.PeekRune() == '.' {
		runes.Next()
		result = result + string(runes.CurrentRune())
	}
	return result
}

func (runes *RuneSupplier) ReadBinaryNumber() string {
	result := ""
	runes.Next()
	for IsBinaryDigit(runes.PeekRune()) {
		runes.Next()
		result = result + string(runes.CurrentRune())
	}
	return result
}

func (runes *RuneSupplier) ReadOctalNumber() string {
	result := ""
	runes.Next()
	for IsOctalDigit(runes.PeekRune()) {
		runes.Next()
		result = result + string(runes.CurrentRune())
	}
	return result
}

func (runes *RuneSupplier) ReadHexNumber() string {
	result := ""
	runes.Next()
	for IsHexDigit(runes.PeekRune()) {
		runes.Next()
		result = result + string(runes.CurrentRune())
	}
	return result
}

func (runes *RuneSupplier) ReadComment() string {
	result := ""
	for !(runes.PeekRune() == '\n' || runes.PeekRune() == 0) {
		result = result + string(runes.PeekRune())
		runes.Next()
	}
	return result
}

func (l *lexer) readSnippet() (string, int) {
	// Note --- what we return from this is followed by makeToken, which doesn't read a character, rather than NewToken, which does.
	// This is because we may end up in a position where we've just realized that we've unindented. (See other use of MakeChar.)
	result := ""
	for l.runes.PeekRune() == ' ' || l.runes.PeekRune() == '\t' { // We consume the whitespace between the emdash and either a non-whitespace character or a newline.
		l.runes.Next()
	}
	// There are two possibilities. Either we found a non-whitespace character, and the whole snippet is on the same line as the
	// `---` token, or we found a newline and the snippet is an indent on the succeeding lines. Just like with a colon.
	if l.runes.PeekRune() == '\n' || l.runes.PeekRune() == '\r' { // --- then we have to mess with whitespace.
		for l.runes.PeekRune() == '\n' || l.runes.PeekRune() == '\r' {
			l.runes.Next()
		}
		langIndent := ""
		stackTop, ok := l.whitespaceStack.HeadValue()
		if !ok {
			stackTop = ""
		}
		for {
			currentWhitespace := ""
			for (l.runes.PeekRune() == '\t' || l.runes.PeekRune() == ' ') && !(langIndent != "" && currentWhitespace == langIndent) {
				l.runes.Next()
				currentWhitespace = currentWhitespace + string(l.runes.CurrentRune())
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
			for l.runes.PeekRune() != '\n' && l.runes.PeekRune() != '\r' && l.runes.PeekRune() != 0 {
				l.runes.Next()
				result = result + string(l.runes.CurrentRune())
			}
			if l.runes.PeekRune() == 0 {
				l.runes.Next()
				cstackHeight := l.whitespaceStack.Find("")
				l.whitespaceStack.Take(cstackHeight)
				return result, cstackHeight
			}
			l.runes.Next()
			result = result + "\n"
		}
	} else {
		for l.runes.PeekRune() != '\n' && l.runes.PeekRune() != '\r' && l.runes.PeekRune() != 0 {
			l.runes.Next()
			result = result + string(l.runes.CurrentRune())
		}
		l.runes.Next()
		return result, -1
	}
}

func (l *lexer) readGolang() string {
	result := ""
	for l.runes.PeekRune() == ' ' || l.runes.PeekRune() == '\t' { // Get rid of the whitespace between 'golang' and whatever follows it.
		l.runes.Next()
	}
	// We expect a brace or quotes after the golang keyword. (The quotes if it's in the import section, import golang "foo".)
	if l.runes.PeekRune() != '{' && l.runes.PeekRune() != '"' && l.runes.PeekRune() != '`' {
		l.Throw("lex/golang", l.NewToken(token.ILLEGAL, "bad golang"))
		return ""
	}
	if l.runes.PeekRune() == '"' {
		l.runes.Next()
		s, ok := l.runes.ReadFormattedString()
		if !ok {
			l.Throw("lex/quote/c", l.NewToken(token.ILLEGAL, "bad quote"))
		}
		return s
	}
	if l.runes.PeekRune() == '`' {
		l.runes.Next()
		s, ok := l.runes.ReadPlaintextString()
		if !ok {
			l.Throw("lex/quote/d", l.NewToken(token.ILLEGAL, "bad quote"))
		}
		return s
	}
	l.runes.Next() // Skips over the opening brace.

	// We just have to look for a closing brace on the left of a line.
	for !(l.runes.CurrentRune() == 0) && !(l.runes.CurrentRune() == '\n' && l.runes.PeekRune() == '}') {
		l.runes.Next()
		result = result + string(l.runes.CurrentRune())
	}
	return result
}

func (runes *RuneSupplier) ReadRuneLiteral() (string, bool, bool) {
	escape := false
	result := ""
	for {
		runes.Next()
		if (runes.CurrentRune() == '\'' && !escape) || runes.CurrentRune() == 0 || runes.CurrentRune() == 13 || runes.CurrentRune() == 10 {
			break
		}
		if runes.CurrentRune() == '\\' && !escape {
			escape = true
			continue
		}
		charToAdd := runes.CurrentRune()
		if escape {
			escape = false
			switch runes.CurrentRune() {
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
	if runes.CurrentRune() == 13 || runes.CurrentRune() == 0 || runes.CurrentRune() == 10 {
		return result + string(runes.CurrentRune()), false, utf8.RuneCountInString(result) == 1
	}
	if utf8.RuneCountInString(result) != 1 {
		return result, true, false
	}
	return result, true, true
}

func (runes *RuneSupplier) ReadFormattedString() (string, bool) {
	escape := false
	result := ""
	for {
		runes.Next()
		if (runes.CurrentRune() == '"' && !escape) || runes.CurrentRune() == 0 || runes.CurrentRune() == 13 || runes.CurrentRune() == 10 {
			break
		}
		if runes.CurrentRune() == '\\' {
			escape = true
			continue
		}

		charToAdd := runes.CurrentRune()

		if escape {
			escape = false
			switch runes.CurrentRune() {
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
	if runes.CurrentRune() == 13 || runes.CurrentRune() == 0 || runes.CurrentRune() == 10 {
		return result, false
	}
	return result, true
}

func (runes *RuneSupplier) ReadPlaintextString() (string, bool) {
	result := ""
	for {
		runes.Next()
		if runes.CurrentRune() == '`' || runes.CurrentRune() == 0 || runes.CurrentRune() == 13 || runes.CurrentRune() == 10 {
			break
		}
		result = result + string(runes.CurrentRune())
	}
	if runes.CurrentRune() == 13 || runes.CurrentRune() == 0 || runes.CurrentRune() == 10 {
		return result, false
	}
	return result, true
}

func (runes *RuneSupplier) ReadIdentifier() string {
	result := string(runes.CurrentRune()) // i.e. the character that suggested this was an identifier.
	for !runes.atBoundary() {
		runes.Next()
		result = result + string(runes.CurrentRune())
	}
	return result
}

func IsLetter(ch rune) bool {
	return unicode.IsLetter(ch)
}

func IsUnderscore(ch rune) bool {
	return ch == '_'
}

func IsDigit(ch rune) bool {
	return unicode.IsNumber(ch)
}

func IsBinaryDigit(ch rune) bool {
	return ch == '0' || ch == '1'
}

func IsOctalDigit(ch rune) bool {
	return '0' <= ch && ch <= '7'
}

func IsHexDigit(ch rune) bool {
	return ('0' <= ch && ch <= '9') || ('a' <= ch && ch <= 'f') || ('A' <= ch && ch <= 'F')
}

func IsProtectedPunctuationBracketOrWhitespace(ch rune) bool {
	return ch == '(' || ch == ')' || ch == '[' || ch == ']' || ch == '{' || ch == '}' || ch == ' ' || ch == ',' ||
		ch == ':' || ch == ';' || ch == '.' || ch == '\t' || ch == '\n' || ch == '\r' || ch == 0
}

func IsProtectedPunctuation(ch rune) bool {
	return ch == ',' || ch == ':' || ch == ';' || ch == '.' || ch == '='
}

func IsWhitespace(ch rune) bool {
	return ch == '\t' || ch == '\n' || ch == '\r' || ch == 0
}

func IsSymbol(ch rune) bool {
	return !(IsUnderscore(ch) || IsLetter(ch) || IsDigit(ch) || IsProtectedPunctuationBracketOrWhitespace(ch))
}

func IsLegalStart(ch rune) bool {
	return !(IsProtectedPunctuationBracketOrWhitespace(ch) || IsDigit(ch))
}

func IsBoundary(ch, pc rune) bool {
	return IsProtectedPunctuationBracketOrWhitespace(pc) ||
		IsLetter(ch) && !(IsLetter(pc) || IsUnderscore(pc)) ||
		IsDigit(ch) && !(IsDigit(pc) || IsUnderscore(pc)) ||
		IsSymbol(ch) && !(IsSymbol(pc) || IsUnderscore(pc))
}

// Finds if we're at the end of an identifier.
func (runes *RuneSupplier) atBoundary() bool {
	return IsBoundary(runes.CurrentRune(), runes.PeekRune())
}

func (l *lexer) NewToken(tokenType token.TokenType, st string) token.Token {
	l.runes.Next()
	l.afterWhitespace = false
	return l.MakeToken(tokenType, st)
}

func (l *lexer) MakeToken(tokenType token.TokenType, st string) token.Token {
	if settings.SHOW_LEXER && !(settings.IGNORE_BOILERPLATE && settings.ThingsToIgnore.Contains(l.source)) {
		fmt.Println(tokenType, st)
	}
	_, chNo := l.runes.Position()
	return token.Token{Type: tokenType, Literal: st, Source: l.source, Line: l.lineNo, ChStart: l.tstart, ChEnd: chNo}
}

func (l *lexer) Throw(errorID string, args ...any) token.Token {
	tok := l.MakeToken(token.ILLEGAL, errorID)
	l.Ers = err.Throw(errorID, l.Ers, &tok, args...)
	return tok
}

func (l *lexer) makeEnds(n int) []token.Token {
	result := []token.Token{}
	for range n {
		result = append(result, l.MakeToken(token.END, "<-|"))
	}
	return result
}
