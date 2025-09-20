package hub

import (
	"io"
	"regexp"
	"strings"

	"github.com/lmorg/readline/v4"
	"github.com/tim-hardcastle/Pipefish/source/text"
)

func StartHub(hub *Hub, in io.Reader, out io.Writer) {
	colonOrEmdash, _ := regexp.Compile(`.*[\w\s]*(:|--)[\s]*$`)
	rline := readline.NewInstance()
	rline.SyntaxHighlighter = hub.makeHighlighter()
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
	promptText := text.RESET + text.Cyan(left) + " " + symbol
	if hub.CurrentServiceIsBroken() {
		promptText = text.RESET + text.Red(left) + " " + symbol
	}
	return promptText
}

func (hub *Hub) makeHighlighter() func([]rune) string {
	findNumbers, _ := regexp.Compile("(^|[^_;0-9])(0[bB][01]+|0[oO][0-7]+|0[xX][0-9A-Fa-f]+|[0-9]+)")
	findComments, _ := regexp.Compile(`//`)
	findStrings, _ := regexp.Compile("(\"[^\"\\\\]*(\\\\.[^\"\\\\]*)*\"|`*`)")
	findTypes, _ := regexp.Compile("(\\b(clone|clones|enum|struct|def|float|string|int|bool|type|secret|snippet|pair|list|map|set|error|null)\\b\\??|\\b[A-Z][a-z][A-Za-z]*\\b\\??)")
	findConstants, _ := regexp.Compile("(\\b[A-Z][A-Z_]+\\b)")
	findControl, _ := regexp.Compile("\\b(break|continue|else|try)\\b")
	findReserved, _ := regexp.Compile("\\b(and|false|given|global|golang|not|or|ref|true|unwrap|valid)\\b")
	findPunctuation, _ := regexp.Compile("(=|==|!=|->|\\?>|\\>>|:| ;|; |::|->|--|,|\\.|\\||\\.\\.\\.)")
	findZ, _ := regexp.Compile(`(//.*)(z)(.*)`)
	return func(r []rune) string {
		result := string(r)
		result = findNumbers.ReplaceAllString(result, "$1" + hub.getColor("number") + "$2" + text.RESET)
		result = findTypes.ReplaceAllString(result, hub.getColor("type") + "$1" + text.RESET)
		result = findConstants.ReplaceAllString(result, hub.getColor("constant") + "$1" + text.RESET)
		result = findControl.ReplaceAllString(result, hub.getColor("control") + "$1" + text.RESET)
		result = findReserved.ReplaceAllString(result, hub.getColor("reserved") + "$1" + text.RESET)
		result = findPunctuation.ReplaceAllString(result, hub.getColor("reserved") + "$1" + text.RESET)
		result = findStrings.ReplaceAllString(result, hub.getColor("string") + "$1" + text.RESET)
		result = findComments.ReplaceAllString(result, hub.getColor("comment") + "//")
		result = findZ.ReplaceAllString(result, "$1$3")
		return result
	}
}
