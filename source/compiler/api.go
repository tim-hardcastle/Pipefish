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
	headliner := text.NewMarkdown("", width, func(s string) string {return cp.Highlight([]rune(s), fonts)})
	markdowner := text.NewMarkdown("    ", width, func(s string) string {return cp.Highlight([]rune(s), fonts)})
	hasContents := false
	result := "\n"
	if cp.DocString != "" {
		result = "\n" + headliner.Render([]string{"# " + strings.TrimSpace(cp.DocString)}) + "\n"
	}
	for i, items := range cp.ApiDescription {
		if len(items) == 0 {
			continue
		}
		hasContents = true
		result = result + headliner.Render([]string{"### " + headings[i]}) + "\n"
		for _, item := range items {
			decString := text.BULLET + cp.Highlight(item.Declaration, fonts)
			if item.DocString != "" {
				decString = decString + " — "
				result = result + markdowner.RenderLeftPad(decString, []string{item.DocString})
			} else {
				result = result + decString 
			}
			result = result + "\n"
		}
		result = result + "\n"
	}
	if !hasContents {
		return("API is empty.")
	}
	return result
}

type ApiItem struct {
	Declaration []rune
	DocString  string
}

var headings = []string{"Modules", "Types", "Constants", "Variables", "Commands", "Functions"}

