

package service

// Code for generating golang to be turned into plugins.

import (
	"fmt"
	"strconv"
	"strings"

	"pipefish/source/ast"
	"pipefish/source/text"
	"pipefish/source/token"
	"pipefish/source/values"
)

// We begin with the generation of the type declarations. In order for the Golang interop to work with structs,
// each Go file must declare the structs and enums and clone types but also functions which can at least partially
//  turn the Go types into Pipefish types to return them.

// (In the case of the structs, the Go file also needs to do most of the heavy lifting on its end to turn Pipefish
// structs into Go.)

// So this prepares this Go code as a snippet of text which can be added to the Go source code we're compiling.
func (cp *Compiler) generateDeclarationAndConversionCode(goHandler *GoHandler) string {
	var typeDeclarationString string
	// First the clone types.
	var convPfCloneToGo, convGoCloneToPf string
	if len(goHandler.CloneNames) > 0 {
		// Initialize the converter functions
		convPfCloneToGo = "\nfunc ConvertPipefishCloneToGo(typeNo uint32, v any) any {\n\tswitch typeNo {"
		convGoCloneToPf = "\nfunc ConvertGoCloneToPipefish(v any) (uint32, any) {\n\tswitch v:= v.(type) {"
		for name := range goHandler.CloneNames {
			// We add the type declaration.
			concType, _ := cp.getConcreteType(name)
			typeInfo, _ := cp.getTypeInformation(name)
			cloneInfo := typeInfo.(cloneType)
			goType, ok := cloneConv[cloneInfo.parent]
			if !ok {
				cp.Throw("golang/ungoable/a", token.Token{Source: "golang interop"})
				continue
			}
			typeDeclarationString = typeDeclarationString + "type " + name + " " + goType + "\n\n"
			// And the converter functions each need a case in their switch statements.
			convPfCloneToGo = convPfCloneToGo + "\n\tcase " + strconv.Itoa(int(concType)) + " : \n\t\treturn " + name + "(v.(" + goType + "))"
			convGoCloneToPf = convGoCloneToPf + "\n\tcase " + name + " : \n\t\treturn uint32(" + strconv.Itoa(int(concType)) + "), " + goType + "(v)"
		}
		// We finish off the clone conversion function.
		convPfCloneToGo = convPfCloneToGo + "\n\tdefault:\n\t\tpanic(\"Oh no, we ran out of clones!\")\n\t}\n}\n"
		convGoCloneToPf = convGoCloneToPf + "\n\tdefault:\n\t\treturn uint32(0), 0\n\t}\n}\n"
	}

	// Next the enum declarations and converters. 
	var convPfEnumToGo, convGoEnumToPf string
	if len(goHandler.EnumNames) > 0 {
		//We intitialize the converter code.
		convPfEnumToGo := "\nfunc ConvertPipefishEnumToGo(typeNo uint32, index int) any {\n\tswitch typeNo {"
		convGoEnumToPf := "\nfunc ConvertGoEnumToPipefish(v any) (uint32, int) {\n\tswitch v := v.(type) {"
		// And then each enum needs one declaration; and one case in each converter.
		for name := range goHandler.EnumNames {
			// Now we add the type declaration, in Golang's usual const-iota format.
			concType, _ := cp.getConcreteType(name)
			typeInfo, _ := cp.getTypeInformation(name)
			firstEnumElement := typeInfo.(enumType).elementNames[0]
			typeDeclarationString = typeDeclarationString + "type " + name + " int\n\n const (\n    " + firstEnumElement + " " + name + " = iota\n"
			for _, element := range typeInfo.(enumType).elementNames[1:] {
				typeDeclarationString = typeDeclarationString + "    " + element + "\n"
			}
			typeDeclarationString = typeDeclarationString + ")\n"
			// We add one line to the switch case of each converter.
			convPfEnumToGo = convPfEnumToGo + "\n\tcase " + strconv.Itoa(int(concType)) + " : \n\t\treturn " + name + "(index)"
			convGoEnumToPf = convGoEnumToPf + "\n\tcase " + name + " : \n\t\treturn uint32(" + strconv.Itoa(int(concType)) + "), int(v)"
		}
		// We finish off the conversion functions.
		convPfEnumToGo = convPfEnumToGo + "\n\tdefault:\n\t\tpanic(\"Oh no, we ran out of enums!\")\n\t}\n}\n"
		convGoEnumToPf = convGoEnumToPf + "\n\tdefault:\n\t\treturn uint32(0), 0\n\t}\n}\n"
	}

	// And finally the structs.
	var convGoStructToPf, convPfStructToGo string
	if len(goHandler.StructNames) > 0 {
		convGoStructToPf = "\nfunc ConvertGoStructToPipefish(v any) (uint32, []any, bool) {\n\tswitch v := v.(type) {"
		convPfStructToGo = "\nfunc ConvertPipefishStructToGo(T uint32, args []any) any {\n\tswitch T {"
		// And then we iterate over the structs.
		for name := range goHandler.StructNames {
			structTypeNumber := cp.StructNameToTypeNumber[name]
			// We add the definition of the struct.
			typeDefStr := "\ntype " + name + " struct {\n"
			for i, lN := range cp.Vm.concreteTypeInfo[structTypeNumber].(structType).labelNumbers {
				typeDefStr = typeDefStr + "\t" + (cp.Vm.Labels[lN]) + " " + cp.convertFieldTypeFromPfToGo(cp.Vm.concreteTypeInfo[structTypeNumber].(structType).abstractStructFields[i]) + "\n"
			}
			typeDefStr = typeDefStr + "}\n"
			typeDeclarationString = typeDeclarationString + typeDefStr
			// We add part of a type switch that helps convert a Go struct to Pipefish.
			convGoStructToPf = convGoStructToPf + "\n\tcase " + name + " : \n\t\treturn uint32(" + strconv.Itoa(int(structTypeNumber)) + ")"
			convGoStructToPf = convGoStructToPf + ", []any{"
			sep := ""
			for _, lN := range cp.Vm.concreteTypeInfo[structTypeNumber].(structType).labelNumbers {
				convGoStructToPf = convGoStructToPf + sep + "v." + cp.Vm.Labels[lN]
				sep = ", "
			}
			convGoStructToPf = convGoStructToPf + "}, true\n"
			// We add part of a switch that helps convert a Pipefish struct to Go.
			convPfStructToGo = convPfStructToGo + "\n\tcase " + strconv.Itoa(int(structTypeNumber)) + " : \n\t\treturn " + name + "{"
			sep = ""
			for i, ty := range cp.Vm.concreteTypeInfo[structTypeNumber].(structType).abstractStructFields {
				convPfStructToGo = convPfStructToGo + sep + "args[" + strconv.Itoa(i) + "].(" + cp.convertFieldTypeFromPfToGo(ty) + ")"
				sep = ", "
			}
			convPfStructToGo = convPfStructToGo + "}\n"
		} // And we're done iterating over the structs. We add the ends of the two convertor functions.
		convGoStructToPf = convGoStructToPf + "\tdefault:\n\t\treturn uint32(0), []any{}, false\n\t}\n}\n"
		convPfStructToGo = convPfStructToGo + "\tdefault:\n\t\tpanic(\"Oh no, we ran out of structs!\")\n\t}\n}\n"
	}
	// And then slap them all together as one block of code and send them on their way rejoicing.
	return typeDeclarationString + convPfCloneToGo + convGoCloneToPf + convPfEnumToGo + convGoEnumToPf + convPfStructToGo + convGoStructToPf 
}

var cloneConv = map[values.ValueType]string{
	values.FLOAT:  "float64",
	values.INT:    "int",
	values.LIST:   "[]any",
	values.PAIR:   "[]any",
	values.RUNE:   "rune",
	values.SET:    "[]any",
	values.STRING: "string",
}

// This produces the names of the field types in the struct declarations generated for the Go code.
func (cp *Compiler) convertFieldTypeFromPfToGo(aT values.AbstractType) string {
	if aT.Len() > 1 {
		cp.P.Throw("golang/type/concrete", &token.Token{Source: "golang interop"})
		return ""
	}
	typeNumber := aT.Types[0]
	typeName := cp.Vm.concreteTypeInfo[typeNumber].getName(DEFAULT)
	goType, ok := getGoType(typeName)
	if !ok {
		cp.P.Throw("golang/type/illegal", &token.Token{Source: "golang interop"})
		return ""
	}
	return goType
}

// Since the signatures of each function is written in Pipefish, we must give each one a signature in Go.
func (cp *Compiler) generateGoFunctionCode(gh *GoHandler, fnName string, sig, retSig ast.StringSig, golang *ast.GolangExpression, pfDir string) {
	source := golang.GetToken().Source
	var fn strings.Builder
    fmt.Fprint(&fn, "func ", text.Capitalize(fnName))
	cp.printSig(&fn, sig, golang.Token)
	switch len(retSig) {
	case 0 :
		fmt.Fprint(&fn, "any ")
	case 1 :
		fmt.Fprint(&fn, retSig[0].VarType, " ")
	default :
		cp.printSig(&fn, retSig, golang.Token)
	}
	fmt.Fprint(&fn, "{", golang.Token.Literal, "\n\n")
	gh.Modules[source] = gh.Modules[source] + fn.String()
}

func (cp *Compiler) printSig(fn *strings.Builder, sig ast.StringSig, tok token.Token) {
	fmt.Fprint(fn, "(")
	sep := ""
	for _, param := range sig {
		goType, ok := getGoType(param.VarType)
		if !ok {
			cp.Throw("golang/param", tok, param.VarType)
		}
		fmt.Fprint(fn, sep, param.VarName)
		if param.VarName != "" { // In which case it would be a return signature.
			fmt.Fprint(fn, " ")
		}
		fmt.Fprint(fn, goType)
		sep = ", "
	}
	fmt.Fprint(fn, ") ")
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
	"float": "float64",
	"label": "!",
	"list": "any[]",
	"pair": "any[]",
	"set": "any[]",
	"tuple": "any[]",
	"type": "!",
}




