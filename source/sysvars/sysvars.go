package sysvars

import (
	"os"
	"path/filepath"

	"charm/source/object"
)

// This was written before variables had types even, and should be rewritten.

type sysvar = struct {
	Dflt      object.Object
	Validator func(object.Object) string
}

var Sysvars = map[string]sysvar{
	"$view": sysvar{
		Dflt: &object.String{Value: "plain"},
		Validator: func(obj object.Object) string {
			switch obj.(type) {
			case *object.String:
				if obj.(*object.String).Value != "charm" && obj.(*object.String).Value != "plain" {
					return "sys/view/vals"
				}
				return ""
			default:
				return "sys/view/string"
			}
		},
	},
	"$logTime": sysvar{
		Dflt: object.FALSE,
		Validator: func(obj object.Object) string {
			switch obj.(type) {
			case *object.Boolean:
				return ""
			default:
				return "sys/logtime/bool"
			}
		},
	},
	"$logPath": sysvar{
		Dflt: &object.String{Value: "stdout"},
		Validator: func(obj object.Object) string {
			switch obj.(type) {
			case *object.String:
				if obj.(*object.String).Value == "stdout" {
					return ""
				} 
				_, err := os.Stat(filepath.Dir(obj.(*object.String).Value))
						if err != nil {
					return "sys/logpath/path"
				}
				return ""
			default:
				return "sys/logpath/string"
			}
		},
	},
}
