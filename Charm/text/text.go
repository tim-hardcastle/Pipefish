package text

import (
	"fmt"
	//"runtime"
	"path/filepath"
	"strings"
)

const (
	VERSION = "0.1"
	BULLET = " ▪ "
	PROMPT = "→ ")

type Err struct {
	Msg string
	Line int	
}

type Errors []Err

func (e *Err) String() string {
	if ((*e).Line) > 0 {
		return fmt.Sprintf(ERROR + "%s at line " + Yellow("%d") + "\n", (*e).Msg, (*e).Line)
	}
	return fmt.Sprintf(ERROR + "%s\n", (*e).Msg)
}

func (E *Errors) String() string {
	result := ""
	for _, e := range (*E) {
		result += e.String()
	}
	return result
}

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

// func init() {
// 	if runtime.GOOS == "windows" {
// 		RESET  = ""
// 		RED    = ""
// 		GREEN  = ""
// 		YELLOW = ""
// 		BLUE  = ""
// 		PURPLE = ""
// 		CYAN   = ""
// 		GRAY   = ""
// 		WHITE  = ""
// 		ERROR = "Error: " 
// 		OK = "OK!\n"
// 	}
// }