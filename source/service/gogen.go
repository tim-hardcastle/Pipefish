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
	// This is the string which will store all the generated type declarations.
	decs := ""
	// All the Go-to-Pipefish conversion is carried out by a single converter, initialized like this:
	goToPf := "func ConvertGoToPipefish(v any) (uint32, any) {\n\tswitch v:= v.(type) {"

	// We start with the clones. For conversion the other way, from Pipefish-to-Go, clones have their
	// own converter.
	convPfCloneToGo := "func ConvertPipefishCloneToGo(typeNo uint32, v any) any {\n\tswitch typeNo {"
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
		decs = decs + "type " + name + " " + goType + "\n\n"
		// And the converter functions each need a case in their switch statements.
		convPfCloneToGo = convPfCloneToGo + "\n\tcase " + strconv.Itoa(int(concType)) + " : \n\t\treturn " + name + "(v.(" + goType + "))"
		goToPf = goToPf + "\n\tcase " + name + " : \n\t\treturn uint32(" + strconv.Itoa(int(concType)) + "), " + goType + "(v)"
	}
	// We finish off the clone conversion function.
	convPfCloneToGo = convPfCloneToGo + "\n\tdefault:\n\t\tpanic(\"Oh no, we ran out of clones!\")\n\t}\n}\n\n"
	
	// Now we do the enums. We initialize the Pipefish-to-Go converter.
	convPfEnumToGo := "func ConvertPipefishEnumToGo(typeNo uint32, index int) any {\n\tswitch typeNo {"
	// And then each enum needs one declaration; and one case in each converter.
	for name := range goHandler.EnumNames {
		// Now we add the type declaration, in Golang's usual const-iota format.
		concType, _ := cp.getConcreteType(name)
		typeInfo, _ := cp.getTypeInformation(name)
		firstEnumElement := typeInfo.(enumType).elementNames[0]
		decs = decs + "type " + name + " int\n\nconst (\n    " + firstEnumElement + " " + name + " = iota\n"
		for _, element := range typeInfo.(enumType).elementNames[1:] {
			decs = decs + "    " + element + "\n"
		}
		decs = decs + ")\n\n"
		// We add one line to the switch case of each converter.
		convPfEnumToGo = convPfEnumToGo + "\n\tcase " + strconv.Itoa(int(concType)) + " : \n\t\treturn " + name + "(index)"
		goToPf = goToPf + "\n\tcase " + name + " : \n\t\treturn uint32(" + strconv.Itoa(int(concType)) + "), int(v)"
	}
	// We finish off the enum conversion function.
	convPfEnumToGo = convPfEnumToGo + "\n\tdefault:\n\t\tpanic(\"Oh no, we ran out of enums!\")\n\t}\n}\n\n"
	// And finally the structs. Initialize the converter.
	convPfStructToGo := "func ConvertPipefishStructToGo(T uint32, args []any) any {\n\tswitch T {"
	for name := range goHandler.StructNames {
		structTypeNumber := cp.StructNameToTypeNumber[name]
		// We add the definition of the struct.
		typeDefStr := "type " + name + " struct {\n"
		for i, lN := range cp.Vm.concreteTypeInfo[structTypeNumber].(structType).labelNumbers {
			typeDefStr = typeDefStr + "\t" + (cp.Vm.Labels[lN]) + " " + cp.convertFieldTypeFromPfToGo(cp.Vm.concreteTypeInfo[structTypeNumber].(structType).abstractStructFields[i]) + "\n"
		}
		typeDefStr = typeDefStr + "}\n\n"
		decs = decs + typeDefStr
		// We add part of a type switch that helps convert a Go struct to Pipefish.
		goToPf = goToPf + "\n\tcase " + name + " : \n\t\treturn uint32(" + strconv.Itoa(int(structTypeNumber)) + ")"
		goToPf = goToPf + ", []any{"
		sep := ""
		for _, lN := range cp.Vm.concreteTypeInfo[structTypeNumber].(structType).labelNumbers {
			goToPf = goToPf + sep + "v." + cp.Vm.Labels[lN]
			sep = ", "
		}
		goToPf = goToPf + "}\n"
		// We add part of a switch that helps convert a Pipefish struct to Go.
		convPfStructToGo = convPfStructToGo + "\n\tcase " + strconv.Itoa(int(structTypeNumber)) + " : \n\t\treturn " + name + "{"
		sep = ""
		for i, ty := range cp.Vm.concreteTypeInfo[structTypeNumber].(structType).abstractStructFields {
			convPfStructToGo = convPfStructToGo + sep + "args[" + strconv.Itoa(i) + "].(" + cp.convertFieldTypeFromPfToGo(ty) + ")"
			sep = ", "
		}
		convPfStructToGo = convPfStructToGo + "}\n"
	} // And we're done iterating over the structs.
	// We finish off the PIpefish-to-Go struct converter..
	convPfStructToGo = convPfStructToGo + "\tdefault:\n\t\tpanic(\"Oh no, we ran out of structs!\")\n\t}\n}\n\n"

	// And we can also finish the big Go-to-Pipefish everything-converter:
	goToPf = goToPf + "\tdefault:\n\t\treturn uint32(0), nil\n\t}\n}\n\n"
	// Except that if it has no cases but the default then `v` will be an unused variable so in that case
	// we supply this instead.
	if len(goHandler.CloneNames) + len(goHandler.EnumNames) + len(goHandler.StructNames) == 0 {
		goToPf = "func ConvertGoToPipefish(v any) (uint32, any) {\n\treturn uint32(0), nil\n}\n\n"
	}
	// And then slap them all together as one block of code and send them on their way rejoicing.
	return decs + convPfEnumToGo + convPfCloneToGo + convPfStructToGo + goToPf 
}

var cloneConv = map[values.ValueType]string{
	values.FLOAT:  "float64",
	values.INT:    "int",
	values.LIST:   "[]any",
	values.PAIR:   "[]any",
	values.SET:    "[]any",
	values.STRING: "string",
}

// This is also auxillary to the 'generateDeclarationAndConversionCode'. It produces the names of the
// field types in the struct declarations generated for the Go code.
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
	(cp).generateDeclarationAndConversionCode(gh)
	fmt.Fprint(&fn, gh.TypeDeclarations[source])
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



