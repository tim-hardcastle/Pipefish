package initializer

// Code for generating golang to be turned into plugins.

// The type declarations are ordinary Go type declartions. The PIPEFISH_CONVERTOR when properly formed
// looks like e.g. this:
//
// var PIPEFISH_CONVERTER = map[string](func(t uint32, v any) any){
// 	"Temperature": func(t uint32, v any) any {return Temperature(v.(int))},
// 	"Color": func(t uint32, v any) any {return Color(v.(int))},
// 	"Dragon": func(t uint32, v any) any {return Dragon{v.([]any)[0], V.([]any)[1], V.([]any)[2]}},
// }
//
// ... given input like this:
//
// Dragon = struct(name string, color Color, temperature Temperature)
// Color = enum RED, GREEN, GOLD, BLACK
// Temperature = clone int

import (
	"fmt"
	"strconv"
	"strings"

	"pipefish/source/ast"
	"pipefish/source/dtypes"
	"pipefish/source/service"
	"pipefish/source/text"
	"pipefish/source/token"
	"pipefish/source/values"
)

func (iz *initializer) generateDeclarations(sb *strings.Builder, userDefinedTypes dtypes.Set[string]) {
	for name := range userDefinedTypes {
		switch typeInfo := iz.cp.TypeInfoNow(name).(type) {
		case service.CloneType:
			goType := cloneConv[typeInfo.Parent]
			fmt.Fprint(sb, "type ", name, " ", goType, "\n\n")
		case service.EnumType:
			firstEnumElement := typeInfo.ElementNames[0]
			fmt.Fprint(sb, "type ", name, " int\n\nconst (\n    ", firstEnumElement, " ", name, " = iota\n")
			for _, element := range typeInfo.ElementNames[1:] {
				fmt.Fprint(sb, "    ", element, "\n")
			}
			fmt.Fprint(sb, ")\n\n")
		case service.StructType:
			fmt.Fprint(sb, "type ", name, " struct {\n")
			for i, lN := range typeInfo.LabelNumbers {
				fmt.Fprint(sb, "\t", (text.Capitalize(iz.cp.Vm.Labels[lN])), " ", iz.convertFieldTypeFromPfToGo(typeInfo.AbstractStructFields[i]), "\n")
			}
			fmt.Fprint(sb, "}\n\n")
		}
	}

	//  Example output:
	//
	//   var PIPEFISH_CONVERTER = map[string](func(t uint32, v any) any){
	// 	    "Temperature": func(t uint32, v any) any {return Temperature(v.(int))},
	// 	    "Color": func(t uint32, v any) any {return Color(v.(int))},
	// 	    "Dragon": func(t uint32, v any) any {return Dragon{v.([]any)[0], V.([]any)[1], V.([]any)[2]}},
	// }
	fmt.Fprint(sb, "var PIPEFISH_FUNCTION_CONVERTER = map[string](func(t uint32, v any) any){\n")
	for name := range userDefinedTypes {
		fmt.Fprint(sb, "    \"", name, "\": func(t uint32, v any) any {return ", name)
		switch typeInfo := iz.cp.TypeInfoNow(name).(type) {
		case service.CloneType:
			fmt.Fprint(sb, "(v.(", cloneConv[typeInfo.Parent], "))},\n")
		case service.EnumType:
			fmt.Fprint(sb, "(v.(int))},\n")
		case service.StructType:
			fmt.Fprint(sb, "{")
			sep := ""
			for i := 0; i < typeInfo.Len(); i++ {
				fmt.Fprint(sb, sep, "v.([]any)[", strconv.Itoa(i), "].(", iz.convertFieldTypeFromPfToGo(typeInfo.AbstractStructFields[i]), ")")
				sep = ", "
			}
			fmt.Fprint(sb, "}},\n")
		}
	}
	fmt.Fprint(sb, "}\n\n")

	// The reason we use the `(*name)(nil)` formula instead of just passing the Type
	// is that then we'd have to import the `reflect` package into everything.
	fmt.Fprint(sb, "var PIPEFISH_VALUE_CONVERTER = map[string]any{\n")
	for name := range userDefinedTypes {
		fmt.Fprint(sb, "    \"", name, "\": (*", name, ")(nil),\n")
	}
	fmt.Fprint(sb, "}\n\n")
}

var cloneConv = map[values.ValueType]string{
	values.FLOAT:  "float64",
	values.FUNC:   "(func(...any)[]any)",
	values.INT:    "int",
	values.LIST:   "[]any",
	values.MAP:    "map[any]any",
	values.PAIR:   "[2]any",
	values.RUNE:   "rune",
	values.SET:    "map[any]struct{}",
	values.STRING: "string",
}

// This is auxillary to 'generateDeclarations'. It produces the names of the
// field types in the struct declarations generated for the Go code.
// This produces the names of the field types in the struct declarations generated for the Go code.
func (iz *initializer) convertFieldTypeFromPfToGo(aT values.AbstractType) string {
	if aT.Len() > 1 {
		iz.Throw("golang/type/concrete", INTEROP_TOKEN)
		return ""
	}
	typeNumber := aT.Types[0]
	typeName := iz.cp.Vm.ConcreteTypeInfo[typeNumber].GetName(service.DEFAULT)
	goType, ok := getGoType(typeName)
	if !ok {
		iz.Throw("golang/type/illegal", INTEROP_TOKEN)
		return ""
	}
	return goType
}

// Since the signatures of each function is written in Pipefish, we must give each one a signature in Go.
func (iz *initializer) generateGoFunctionCode(sb *strings.Builder, function *ast.PrsrFunction) {
	fmt.Fprint(sb, "func ", text.Capitalize(function.FName))
	iz.printSig(sb, function.NameSig, *function.Tok)
	switch len(function.NameRets) {
	case 0:
		fmt.Fprint(sb, "any ")
	case 1:
		fmt.Fprint(sb, function.NameRets[0].VarType, " ")
	default:
		iz.printSig(sb, function.NameRets, *function.Tok)
	}
	fmt.Fprint(sb, "{", function.Body.GetToken().Literal, "\n\n")
}

func (iz *initializer) printSig(sb *strings.Builder, sig ast.StringSig, tok token.Token) {
	fmt.Fprint(sb, "(")
	sep := ""
	for _, param := range sig {
		goType, ok := getGoType(param.VarType)
		if !ok {
			iz.Throw("golang/param", &tok, param.VarType)
		}
		fmt.Fprint(sb, sep, param.VarName)
		if param.VarName != "" { // In which case it would be a return signature.
			fmt.Fprint(sb, " ")
		}
		fmt.Fprint(sb, goType)
		sep = ", "
	}
	fmt.Fprint(sb, ") ")
}

func getGoType(pfType string) (string, bool) {
	dots := ""
	if text.Head(pfType, "...") {
		pfType = pfType[3:]
		dots = "..."
	}
	goType, ok := goTypes[pfType]
	if ok {
		if goType == "!" {
			return "", false
		}
		return dots + goType, true
	}
	return dots + pfType, true
}

var goTypes = map[string]string{
	"any":   "any",
	"float": "float64",
	"func":  "(func(...any)[]any)",
	"label": "!",
	"list":  "[]any",
	"map":   "map[any]any",
	"pair":  "[2]any",
	"set":   "map[any]struct{}",
	"tuple": "[]any",
	"type":  "!",
}

var INTEROP_TOKEN = &token.Token{Source: "golang interop"}
