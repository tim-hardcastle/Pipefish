package hub

import (
	"io"
	"regexp"
	"strings"

	"github.com/lmorg/readline/v4"
)

func StartHub(hub *Hub, in io.Reader, out io.Writer) {
	colonOrEmdash, _ := regexp.Compile(`.*[\w\s]*(:|--)[\s]*$`)
	rline := readline.NewInstance()
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
		for {
			rline.SetPrompt(makePrompt(hub, ws != ""))
			line, _ := rline.ReadlineWithDefault(ws)
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

		_, quit := hub.Do(input, hub.Username, hub.Password, hub.currentServiceName())
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
	promptText := left + " " + symbol
	if hub.CurrentServiceIsBroken() {
		promptText = Red(promptText)
	}
	return promptText
}


