package compiler

import (
	"strings"

	"github.com/tim-hardcastle/Pipefish/source/text"
	"github.com/tim-hardcastle/Pipefish/source/values"
)

// This supplies the bits and pieces we need to render the API.
// We're doing this here and now rather than at initialization so that in principle
// we could get the font and width from a desktop client.

func (cp *Compiler) Api(fonts *values.Map, width int) string {
	markdowner := text.NewMarkdown("", width, func(s string) string {return cp.Highlight([]rune(s), fonts)})
	hasContents := false
	result := ""
	if cp.DocString != "" {
		result = "\n" + markdowner.Render([]string{"# " + strings.TrimSpace(cp.DocString)}) + "\n"
	}
	for i, items := range cp.ApiDescription {
		if len(items) == 0 {
			continue
		}
		hasContents = true
		result = result + "\n" + markdowner.Render([]string{"### " + headings[i]})
		for _, item := range items {
			stringToRender := "- " + cp.Highlight(item.Declaration, fonts)
			if item.DocString != "" {
				stringToRender = stringToRender + " — "  + item.DocString
			}
			result = result + markdowner.Render([]string{stringToRender})
		}
		result = result + "\n"
	}
	if !hasContents {
		return("API is empty.\n")
	}
	result = result + "\n"
	return result
}

type ApiItem struct {
	Declaration []rune
	DocString  string
}

var headings = []string{"Modules", "Types", "Constants", "Variables", "Commands", "Functions"}

