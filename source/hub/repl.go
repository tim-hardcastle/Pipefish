package hub

import (
	"io"
	"strings"

	"github.com/lmorg/readline"
)

func StartHub(hub *Hub, in io.Reader, out io.Writer) {
	rline := readline.NewInstance()
	for {

		// The hub's CurrentForm setting allows it to ask for information from the user instead of
		// just sitting waiting to be told. If CurrentForm is not nil then it contains a structured
		// request for information which must be completed before returnng to the regular REPL.
		// TODO --- this can now be replaced by Pipefish's own facilities for IO.
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

		_, quitCharm := hub.Do(line, hub.Username, hub.Password, hub.currentServiceName())
		if quitCharm {
			break
		}
	}
}

func makePrompt(hub *Hub) string {
	if hub.currentServiceName() == "" {
		return PROMPT
	}
	promptText := hub.currentServiceName() + " " + PROMPT
	if hub.CurrentServiceIsBroken() {
		promptText = Red(promptText)
	}
	return promptText
}
