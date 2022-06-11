package sysvars

import (
	"charm/object"
	"charm/text"
)

type sysvar = struct {
	Dflt object.Object
	Validator func(object.Object) string
}

var Sysvars = map[string] sysvar {
	"$view" : sysvar{
		Dflt : &object.String{Value: "plain"},
		Validator : func(obj object.Object) string {
			switch obj.(type) {
			case *object.String :
				if obj.(*object.String).Value != "charm" && obj.(*object.String).Value != "plain" {
					return "system variable " + text.Emph("$view") + " takes values " + 
					text.Emph("\"charm\"") + " or " + text.Emph("\"plain\"")
				}
				return ""
			default :
				return "system variable " + text.Emph("$view") + " is of type " + text.Emph("string")
			}
		},
	},
}

