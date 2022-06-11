package text

import (

	"strconv"
	"strings"

	"path/filepath"

	"charm/token"
)

const (
	VERSION = "0.1"
	BULLET = " ▪ "
	PROMPT = "→ ")

func ToEscapedText(s string) string {
	result := "\""
	for _, ch := range(s) {
		switch ch {
		case '\n' :
			result = result + "\n"
		case '\r' :
			result = result + "\r"
		case '\t' :
			result = result + "\t"
		default : result = result + string(ch)
		}
	}
	return result + "\""
}

func FlattenedFilename(s string) string {
	s = filepath.Base(s)
	s = strings.Replace(s, ".", "_", -1)
	return s
}

func Emph(s string) string {
	return CYAN + "'" + s + "'" + RESET;
}

func Red(s string) string {
	return RED + s + RESET;
}

func Green(s string) string {
	return GREEN + s + RESET;
}

func Yellow(s string) string {
	return YELLOW + s + RESET;
}

func Logo() string {
	var padding string
	if len(VERSION) % 2 == 0 {padding = ","}
	titleText := " Charm" + padding + " version " + VERSION + " "
	loveHeart := Red("♥")
	leftMargin := "  "
	bar := strings.Repeat("═", len(titleText) / 2)
	logoString := "\n" + 
		leftMargin + "╔" + bar + loveHeart + bar + "╗\n" +
		leftMargin + "║"       + titleText +       "║\n" +
	    leftMargin + "╚" + bar + loveHeart + bar + "╝\n\n"
	return logoString
}

func PosDescription(token token.Token) string {
	result := strconv.Itoa(token.Line) + ":" + strconv.Itoa(token.ChStart)
	if token.ChStart != token.ChEnd { result = result + "-" + strconv.Itoa(token.ChEnd) }
	result = " at line " + Yellow(result)
	prettySource := token.Source
	if prettySource != "REPL input" {
		prettySource = Emph(prettySource)
	}
	return result + " of " + prettySource
}

var (
	RESET  = "\033[0m"
	RED    = "\033[31m"
	GREEN  = "\033[32m"
	YELLOW = "\033[33m"
	BLUE  = "\033[34m"
	PURPLE = "\033[35m"
	CYAN   = "\033[36m"
	GRAY   = "\033[37m"
	WHITE  = "\033[97m"
	ERROR = Red("error") + ": " 
	OK = Green("ok")
)

