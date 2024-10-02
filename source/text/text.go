package text

// This consists of a bunch of text utilities to help in generating pretty and meaningful
// help messages, error messages, etc.

import (
	"runtime"
	"strconv"
	"strings"

	"path/filepath"

	"pipefish/source/token"
)

const (
	VERSION        = "0.4.9"
	BULLET         = "  ▪ "
	BULLET_SPACING = "    " // I.e. whitespace the same width as BULLET.
	GOOD_BULLET    = "\033[32m  ▪ \033[0m"
	BROKEN         = "\033[31m  ✖ \033[0m"
	PROMPT         = "→ "
)

func ExtractFileName(s string) string {
	if strings.LastIndex(s, ".") >= 0 {
		s = s[:strings.LastIndex(s, ".")]
	}
	if strings.LastIndex(s, "/") >= 0 {
		s = s[strings.LastIndex(s, "/")+1:]
	}
	return s
}

func ToEscapedText(s string) string {
	result := "\""
	for _, ch := range s {
		switch ch {
		case '\n':
			result = result + "\n"
		case '\r':
			result = result + "\r"
		case '\t':
			result = result + "\t"
		default:
			result = result + string(ch)
		}
	}
	return result + "\""
}

func FlattenedFilename(s string) string {
	base := filepath.Base(s)
	withoutSuffix := strings.TrimSuffix(base, filepath.Ext(base))
	flattened := strings.Replace(withoutSuffix, ".", "_", -1)
	return flattened
}

func Flatten(s string) string {
	s = strings.Replace(s, ".", "_", -1)
	s = strings.Replace(s, "/", "_", -1)
	return s
}

func Cyan(s string) string {
	return CYAN + s + RESET
}

func Emph(s string) string {
	return "'" + s + "'"
}

func EmphType(s string) string {
	return "'" + s + "'"
}

func Red(s string) string {
	return RED + s + RESET
}

func Green(s string) string {
	return GREEN + s + RESET
}

func Yellow(s string) string {
	return YELLOW + s + RESET
}

func Logo() string {
	var padding string
	if len(VERSION)%2 == 1 {
		padding = ","
	}
	titleText := " Pipefish" + padding + " version " + VERSION + " "
	loveHeart := Red("♥")
	leftMargin := "  "
	bar := strings.Repeat("═", len(titleText)/2)
	logoString := "\n" +
		leftMargin + "╔" + bar + loveHeart + bar + "╗\n" +
		leftMargin + "║" + titleText + "║\n" +
		leftMargin + "╚" + bar + loveHeart + bar + "╝\n\n"
	return logoString
}

const HELP = "\nUsage: pipefish [-v | --version] [-h | --help]\n" +
	"                <command> [args]\n\n" +
	"Commands are:\n\n" +
	"  tui           Starts the Pipfish TUI (text user interface).\n" +
	"  run <file>    Runs a Pipefish script if it has a 'main' command, or offers to open it\n" +
	"                in the TUI if it doesn't.\n\n"

func DescribePos(token *token.Token) string {
	prettySource := token.Source
	if prettySource == "" {
		return ""
	}
	if prettySource != "REPL input" {
		prettySource = "'" + prettySource + "'"
	}
	if token.Line > 0 {
		result := strconv.Itoa(token.Line) + ":" + strconv.Itoa(token.ChStart)
		if token.ChStart != token.ChEnd {
			result = result + "-" + strconv.Itoa(token.ChEnd)
		}
		result = " at line" + "@" + result + "@"

		return result + "of " + prettySource
	}
	return " in " + prettySource
}

// Describes a token for the purposes of error messages etc.
func DescribeTok(tok *token.Token) string {
	switch tok.Type {
	case token.LPAREN:
		if tok.Literal == "|->" {
			return "indent"
		}
	case token.RPAREN:
		if tok.Literal == "<-|" {
			return "outdent"
		}
	case token.NEWLINE:
		if tok.Literal == "\n" {
			return "newline"
		}
	case token.EOF:
		return "end of line"
	case token.STRING:
		return "<string>"
	case token.INT:
		return "<int>"
	case token.FLOAT:
		return "<float64>"
	case token.TRUE:
		return "<bool>"
	case token.FALSE:
		return "<bool>"
	case token.IDENT:
		return "'" + tok.Literal + "'"
	}
	return "'" + tok.Literal + "'"
}

func DescribeOpposite(tok *token.Token) string {
	switch tok.Literal {
	case "<-|":
		{
			return "indent"
		}
	case "|->":
		{
			return "indent"
		}
	case ")":
		{
			return "'('"
		}
	case "]":
		{
			return "["
		}
	case "}":
		{
			return "{"
		}
	case "(":
		{
			return "')'"
		}
	case "[":
		{
			return "]"
		}
	case "{":
		{
			return "}"
		}
	}
	return "You goofed, that doesn't have an opposite."
}

var (
	RESET  = "\033[0m"
	UNDERLINE = "\033[3m"
	RED    = "\033[31m"
	GREEN  = "\033[32m"
	YELLOW = "\033[33m"
	BLUE   = "\033[34m"
	PURPLE = "\033[35m"
	CYAN   = "\033[36m"
	GRAY   = "\033[37m"
	WHITE  = "\033[97m"

	ERROR     = "$Error$"
	RT_ERROR  = "$Error$"
	HUB_ERROR = "$Hub error$"
	OK        = Green("OK")
)

func HighlightLine(plainLine string, highlighter rune) (string, rune) {
	// Now we highlight the line. The rules are: anything enclosed in '   ' is code and is
	// therefore highlighted, i.e. 'foo' serves the same function as writing foo in a monotype
	// font would in a textbook or manual.

	// Because it looks kind of odd and redundant to write '"foo"' and '<foo>',  these are also
	// highlighted without requiring '.

	// The ' doesn't trigger the highlighting unless it follows a line beginning or space etc, because it
	// might be an apostrophe.

	highlitLine := ""
	prevCh := ' '
	if highlighter != ' ' {
		highlitLine = CYAN
	}

	for _, ch := range plainLine {
		if highlighter == ' ' && ((prevCh == ' ' || prevCh == '\n' || prevCh == '$') &&
			(ch == '\'' || ch == '"' || ch == '<' || ch == '$') || ch == '@') {
			highlighter = ch
			if highlighter == '<' {
				highlighter = '>'
			}
			if highlighter == '$' {
				highlitLine = highlitLine + RED
				continue
			}
			if highlighter == '@' {
				highlitLine = highlitLine + " " + YELLOW
				continue
			}
			highlitLine = highlitLine + CYAN
		} else {
			if ch == highlighter {
				prevCh = ch
				highlighter = ' '

				if ch == '$' {
					highlitLine = highlitLine + RESET + ": "
					continue
				}
				if ch == '@' {
					highlitLine = highlitLine + " " + RESET
					continue
				}
				highlitLine = highlitLine + string(ch) + RESET
				continue
			}
		}
		prevCh = ch
		highlitLine = highlitLine + string(ch)
	}
	return highlitLine, highlighter
}

var (
	WAS         = Green("was") + ": "
	GOT         = Red("got") + ": "
	TEST_PASSED = Green("Test passed!") + "\n"
)

func Pretty(s string, lMargin, rMargin int) string {
	LENGTH := rMargin - lMargin
	result := ""
	codeWidth := -1
	highlighter := ' '
	for i := 0; i < len(s); {
		result = result + strings.Repeat(" ", lMargin)
		e := i + LENGTH
		j := 0
		if e > len(s) {
			j = len(s) - i
		} else if strings.Contains(s[i:e], "\n") {
			j = strings.Index(s[i:e], "\n")
		} else {
			j = strings.LastIndex(s[i:e], " ")
		}
		if j == -1 {
			j = LENGTH
		}
		if strings.Contains(s[i:i+j], "\n") {
			j = strings.Index(s[i:i+j], "\n")
		}

		plainLine := s[i : i+j]
		if len(plainLine) >= 2 && plainLine[0:2] == "|-" {
			if codeWidth > 0 {
				result = result + (" └──" + strings.Repeat("─", codeWidth) + "┘\n")
				codeWidth = -1
			} else {
				codeWidth = len(plainLine)
				result = result + (" ┌──" + strings.Repeat("─", codeWidth) + "┐\n")
			}
		} else if codeWidth > 0 {
			repeatNo := codeWidth - len(plainLine)
			if repeatNo < 0 {
				repeatNo = 0
			}
			result = result + (" │  " + Cyan(plainLine) + strings.Repeat(" ", repeatNo) + "│\n")
		} else {
			var str string
			str, highlighter = HighlightLine(plainLine, highlighter)
			result = result + (str + "\n")
		}
		i = i + j + 1
	}
	return result
}

func GetTextWithBarsAsList(text string) ([]string, bool) {
	strList := []string{}
	var (
		word string
		exp  bool
	)
	for _, c := range text {
		if c == '|' {
			if exp {
				strList = append(strList, word+"|")
				word = ""
				exp = false
			} else {
				strList = append(strList, word)
				word = "|"
				exp = true
			}
		} else {
			word = word + string(c)
		}
	}
	if exp {
		return nil, false
	}
	strList = append(strList, word)
	return strList, true
}

// Removes the last two folders in a filepath. TODO --- is there a more principled way of doing this?
func Trim(path string) string {
	sep := "/"
	if runtime.GOOS == "windows" {
		sep = "\\"
	}
	lastFS := strings.LastIndex(path, sep)
	path = path[:lastFS]
	lastFS = strings.LastIndex(path, sep)
	path = path[:lastFS]
	path = path + sep
	return path
}
