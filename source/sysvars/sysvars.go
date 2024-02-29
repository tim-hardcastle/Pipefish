package sysvars

import (
	"os"
	"path/filepath"

	"pipefish/source/object"
)

// This was written before variables had types even, and should be rewritten.

type sysvar = struct {
	Dflt      object.Object
	Validator func(object.Object) string
}

var Sysvars = map[string]sysvar{
	"$view": {
		Dflt: &object.String{Value: "plain"},
		Validator: func(obj object.Object) string {
			switch obj := obj.(type) {
			case *object.String:
				if obj.Value != "" && obj.Value != "plain" {
					return "sys/view/vals"
				}
				return ""
			default:
				return "sys/view/string"
			}
		},
	},
	"$logTime": {
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
	"$logPath": {
		Dflt: &object.String{Value: "stdout"},
		Validator: func(obj object.Object) string {
			switch obj := obj.(type) {
			case *object.String:
				if obj.Value == "stdout" {
					return ""
				}
				_, err := os.Stat(filepath.Dir(obj.Value))
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
