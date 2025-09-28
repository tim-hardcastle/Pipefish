package compiler

import (
	"github.com/tim-hardcastle/Pipefish/source/text"
	"github.com/tim-hardcastle/Pipefish/source/values"
)

// This supplies the bits and pieces we need to render the API.
// We're doing this here and now rather than at initialization so that in principle
// we could get the font and width from a desktop client.

func (cp *Compiler) Api(fonts *values.Map, width int) string {
	result := "\n"
	for i, items := range cp.ApiDescription {
		if len(items) == 0 {
			continue
		}
		result = result + text.BOLD + headings[i] + text.RESET + "\n\n"
		for _, item := range items {
			result = result + text.BULLET + cp.Highlight(item.Declaration, fonts) + "\n"
		}
		result = result + "\n"
	}
	if result == "\n" {
		return("API is empty.")
	}
	return result
}

type ApiItem struct {
	Declaration []rune
	DocString  string
}

type descriptionType int

const ( // Most of these names are self-explanatory.
	moduleDescription descriptionType = iota
	typeDescription
	constantDescription
	variableDescription
	commandDescription
	functionDescription
)

var headings = []string{"Modules", "Types", "Constants", "Variables", "Commands", "Functions"}

