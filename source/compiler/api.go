package compiler

import "github.com/tim-hardcastle/Pipefish/source/values"

// This supplies the bits and pieces we need to render the API.
// We're doing this here and now rather than at initialization so that in principle
// we could get the font and width from a desktop client.

func (cp *Compiler) Api(fonts *values.Map, width int) string {
	return "And now we're here."
}

type apiItem struct{definition []rune
                    docString  string}

type descriptionType int

const ( // Most of these names are self-explanatory.
	moduleDescription       descriptionType = iota
	typeDescription      
	constantDescription
	variableDescription
	commandDescription
	functionDescription
)

var headings = []string{"Modules", "Types", "Constants", "Variables", "Commands", "Functions"}


