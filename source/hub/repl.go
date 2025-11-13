package hub

import (
	"io"
	"regexp"
	"strings"

	"github.com/lmorg/readline/v4"
	"github.com/tim-hardcastle/pipefish/source/text"
)

// TODO --- once the highlighting is semantic and not syntactic, we'll
// need a different highlighter for each service.
func StartHub(hub *Hub, in io.Reader, out io.Writer) {
	colonOrEmdash, _ := regexp.Compile(`.*[\w\s]*(:|--)[\s]*$`)
	rline := readline.NewInstance()
	rline.SyntaxHighlighter = func(code []rune) string {
		return hub.services[hub.currentServiceName()].Highlight(code, hub.getFonts())
	}
	for {
		// The hub's CurrentForm setting allows it to ask for information from the user instead of
		// just sitting waiting to be told. If CurrentForm is not nil then it contains a structured
		// request for information which must be completed before returning to the regular REPL.
		// TODO --- this can now be replaced by Pipefish's own facilities for IO.
		if hub.CurrentForm != nil {
			for {
				queryString := hub.CurrentForm.Fields[len(hub.CurrentForm.Result)]
				pos := strings.LastIndex(queryString, "\n")
				rline := readline.NewInstance()
				if pos == -1 {
					rline.SetPrompt(queryString + ": ")
				} else {
					hub.WriteString(queryString[:pos+1])
					rline.SetPrompt(queryString[pos+1:] + ": ")
				}
				line, _ := rline.Readline()
				hub.CurrentForm.Result[hub.CurrentForm.Fields[len(hub.CurrentForm.Result)]] = line
				if len(hub.CurrentForm.Result) == len(hub.CurrentForm.Fields) {
					hub.CurrentForm.Call(hub.CurrentForm)
					break
				}
			}
			continue
		}

		ws := ""
		input := ""
		c := 0
		PAIRS := [][2]string{
			{"(", ")"},
			{"{", "}"},
			{"[", "]"},
			{"\"", "\""},
			{"`", "`"},
			{"|", "|"},
		}
		for _, pair := range PAIRS {
			left := pair[0]
			right := pair[1]
			handler := func(i int, st *readline.EventState) *readline.EventReturn {
				return &readline.EventReturn{
					SetLine: []rune(st.Line[:st.CursorPos] + right + st.Line[st.CursorPos:]),
					Continue: true,
					SetPos: st.CursorPos,
				}
			}
			rline.AddEvent(left, handler)
		}
		for {
			rline.SetPrompt(makePrompt(hub, ws != ""))
			line, err := rline.ReadlineWithDefault(ws)
			if err == readline.ErrCtrlC {
				print("\nQuit Pipefish? [Y/n] ")
				ch := text.ReadChar()
				println(string(ch))
				if ch == 'n' || ch == 'N' {
					println(text.Green("OK"))
				} else {
					hub.Quit()
					return
				}
			}
			c++
			input = input + line + "\n"
			ws = ""
			for _, c := range line {
				if c == ' ' || c == '\t' {
					ws = ws + string(c)
				} else {
					break
				}
			}
			if colonOrEmdash.Match([]byte(line)) {
				ws = ws + "  "
			}
			if ws == "" {
				break
			}
		}
		input = strings.TrimSpace(input)
		sv := hub.services[hub.currentServiceName()]
		sv.SetOutHandler(sv.MakeTerminalOutHandler())
		_, quit := hub.Do(input, hub.Username, hub.Password, hub.currentServiceName(), false)
		if quit {
			break
		}
	}
}

func makePrompt(hub *Hub, indented bool) string {
	symbol := PROMPT
	left := hub.currentServiceName()
	if indented {
		symbol = INDENT_PROMPT
		left = strings.Repeat(" ", len(left))
	}
	if hub.currentServiceName() == "" {
		return symbol
	}
	promptText := text.RESET + text.Cyan(left) + " " + symbol
	if hub.CurrentServiceIsBroken() {
		promptText = text.RESET + text.Red(left) + " " + symbol
	}
	return promptText
}
