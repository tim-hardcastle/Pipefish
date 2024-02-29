package repl

import (
	"pipefish/source/hub"
	"pipefish/source/text"

	"io"
	"strings"

	"github.com/lmorg/readline"
)

func Start(hub *hub.Hub, in io.Reader, out io.Writer) {
	rline := readline.NewInstance()
	for {

		// The hub's CurrentForm setting allows it to ask for information from the user instead of
		// just sitting waiting to be told. If CurrentForm is not nil then it contains a structured
		// request for information which must be completed before returnng to the regular REPL.
		if hub.CurrentForm != nil {

			for {
				queryString := hub.CurrentForm.Fields[len(hub.CurrentForm.Result)]
				// A * at the beginning of the query string indicates that the answer should be
				// masked.
				if queryString[0] == '*' {
					queryString = queryString[1:]
					rline.PasswordMask = '▪'
				}

				// The readln utility doesn't like multiline prompts, so we must kludge a little.
				pos := strings.LastIndex(queryString, "\n")
				if pos == -1 {
					rline.SetPrompt(queryString + ": ")
				} else {
					hub.WriteString(queryString[:pos+1])
					rline.SetPrompt(queryString[pos+1:] + ": ")
				}
				line, _ := rline.Readline()
				rline.PasswordMask = 0
				hub.CurrentForm.Result[hub.CurrentForm.Fields[len(hub.CurrentForm.Result)]] = line
				if len(hub.CurrentForm.Result) == len(hub.CurrentForm.Fields) {
					hub.CurrentForm.Call(hub.CurrentForm)
					break
				}
			}
			continue
		}

		rline.SetPrompt(makePrompt(hub))
		line, _ := rline.Readline()

		line = strings.TrimSpace(line)

		if line == "" {
			continue
		}

		_, quitCharm := hub.Do(line, hub.Username, hub.Password, hub.GetCurrentServiceName())
		if quitCharm.QuitHappened {
			break
		}
	}
}

func makePrompt(hub *hub.Hub) string {
	if hub.GetCurrentServiceName() == "" {
		return text.PROMPT
	}
	promptText := hub.GetCurrentServiceName() + " " + text.PROMPT
	if hub.CurrentServiceIsBroken() {
		promptText = text.Red(promptText)
	}
	return promptText
}
