package service

// Code for generating golang to be turned into plugins.

import (
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
	decs := "\n"

	// We initialize the converter functions.
	kludge := ""
	if len(goHandler.CloneNames) > 0 {
		kludge = "v := "
	}
	convGoCloneToPf := "\nfunc ConvertGoCloneToPipefish(v any) (uint32, any) {\n\tswitch " + kludge + "v.(type) {"
	for name := range goHandler.CloneNames {
		// As usual in Golang interop only concrete types are allowed. We check.
		concType, ok := cp.getConcreteType(name)
		if !ok {
			cp.Throw("golang/type/concrete/b", token.Token{Source: "golang interop"}, name)
			continue
		}
		// Now we add the type declaration.
		decs = decs + "type " + name + " "

		typeInfo, _ := cp.getTypeInformation(name)
		cloneInfo := typeInfo.(cloneType)
		goType, ok := cloneConv[cloneInfo.parent]
		if !ok {
			cp.Throw("golang/ungoable/a", token.Token{Source: "golang interop"})
			continue
		}
		decs = decs + goType + "\n"
		// And the converter function needs a case in its switch statement.
		convGoCloneToPf = convGoCloneToPf + "\n\tcase " + name + " : \n\t\treturn uint32(" + strconv.Itoa(int(concType)) + "), " + goType + "(v)"
	}
	// We finish off the clone conversion function.
	convGoCloneToPf = convGoCloneToPf + "\n\tdefault:\n\t\treturn uint32(0), 0\n\t}\n}\n\n"

	// Next do the enum declarations and converters. We intitialize the converter code.
	kludge = ""
	if len(goHandler.EnumNames) > 0 {
		kludge = "v := "
	}
	convPfEnumToGo := "\nfunc ConvertPipefishEnumToGo(typeNo uint32, index int) any {\n\tswitch typeNo {"
	convGoEnumToPf := "\nfunc ConvertGoEnumToPipefish(v any) (uint32, int) {\n\tswitch v := v.(type) {"
	// And then each enum needs one declaration; and one case in each converter.
	for name := range goHandler.EnumNames {
		// As usual in Golang interop only concrete types are allowed. We check.
		concType, ok := cp.getConcreteType(name)
		if !ok {
			cp.Throw("golang/type/concrete/b", token.Token{Source: "golang interop"}, name)
			continue
		}
		// Now we add the type declaration, in Golang's usual const-iota format.
		firstEnumElement := cp.Vm.concreteTypeInfo[concType].(enumType).elementNames[0]
		decs = decs + "type " + name + " int\n\n const (\n    " + firstEnumElement + " " + name + " = iota\n"
		for _, element := range cp.Vm.concreteTypeInfo[concType].(enumType).elementNames[1:] {
			decs = decs + "    " + element + "\n"
		}
		decs = decs + ")\n\n"

		// We add one line to the switch case of each converter.
		convPfEnumToGo = convPfEnumToGo + "\n\tcase " + strconv.Itoa(int(concType)) + " : \n\t\treturn " + name + "(index)"
		convGoEnumToPf = convGoEnumToPf + "\n\tcase " + name + " : \n\t\treturn uint32(" + strconv.Itoa(int(concType)) + "), int(v)"
	}
	// We finish off the conversion functions.
	convPfEnumToGo = convPfEnumToGo + "\n\tdefault:\n\t\tpanic(\"We ran out of enums!\")\n\t}\n}\n\n"
	convGoEnumToPf = convGoEnumToPf + "\n\tdefault:\n\t\treturn uint32(0), 0\n\t}\n}\n\n"

	// And finally the structs. Initialize converters.
	kludge = ""
	if len(goHandler.StructNames) > 0 {
		kludge = "v := "
	}
	convGoStructToPfStruct := "\nfunc ConvertGoStructToPipefish(v any) (uint32, []any, bool) {\n\tswitch v := v.(type) {"
	convPfStructToGoStruct := "\nfunc ConvertPipefishStructToGoStruct(T uint32, args []any) any {\n\tswitch T {"
	// And then we iterate over the structs.
	for name := range goHandler.StructNames {
		structTypeNumber := cp.StructNameToTypeNumber[name]
		// We add the definition of the struct.
		typeDefStr := "\ntype " + name + " struct {\n"
		for i, lN := range cp.Vm.concreteTypeInfo[structTypeNumber].(structType).labelNumbers {
			typeDefStr = typeDefStr + "\t" + (cp.Vm.Labels[lN]) + " " + cp.convertFieldTypeFromPfToGo(cp.Vm.concreteTypeInfo[structTypeNumber].(structType).abstractStructFields[i]) + "\n"
		}
		typeDefStr = typeDefStr + "}\n"
		decs = decs + typeDefStr
		// We add part of a type switch that helps convert a Go struct to Pipefish.
		convGoStructToPfStruct = convGoStructToPfStruct + "\n\tcase " + name + " : \n\t\treturn uint32(" + strconv.Itoa(int(structTypeNumber)) + ")"
		convGoStructToPfStruct = convGoStructToPfStruct + ", []any{"
		sep := ""
		for _, lN := range cp.Vm.concreteTypeInfo[structTypeNumber].(structType).labelNumbers {
			convGoStructToPfStruct = convGoStructToPfStruct + sep + "v." + cp.Vm.Labels[lN]
			sep = ", "
		}
		convGoStructToPfStruct = convGoStructToPfStruct + "}, true\n"
		// We add part of a switch that helps convert a Pipefish struct to Go.
		convPfStructToGoStruct = convPfStructToGoStruct + "\n\tcase " + strconv.Itoa(int(structTypeNumber)) + " : \n\t\treturn " + name + "{"
		sep = ""
		for i, ty := range cp.Vm.concreteTypeInfo[structTypeNumber].(structType).abstractStructFields {
			convPfStructToGoStruct = convPfStructToGoStruct + sep + "args[" + strconv.Itoa(i) + "].(" + cp.convertFieldTypeFromPfToGo(ty) + ")"
			sep = ", "
		}
		convPfStructToGoStruct = convPfStructToGoStruct + "}\n"
	} // And we're done iterating over the structs.
	// We add the ends of the two convertor functions.
	convGoStructToPfStruct = convGoStructToPfStruct + "\tdefault:\n\t\treturn uint32(0), []any{}, false\n\t}\n}\n\n"
	convPfStructToGoStruct = convPfStructToGoStruct + "\tdefault:\n\t\tpanic(\"I'm not sure if this error can arise.\")\n\t}\n}\n\n"
	// And then slap them all together as one block of code and send them on their way rejoicing.
	return decs + convGoEnumToPf + convPfEnumToGo + convGoCloneToPf + convGoStructToPfStruct + convPfStructToGoStruct
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
func (cp *Compiler) convertFieldTypeFromPfToGo(aT values.AbstractType) string {
	if aT.Len() > 1 {
		cp.P.Throw("golang/concrete/c", &token.Token{Source: "golang interop"})
	}
	tNo := aT.Types[0]
	if cp.Vm.concreteTypeInfo[tNo].isEnum() || cp.Vm.concreteTypeInfo[tNo].isStruct() ||
		cp.Vm.concreteTypeInfo[tNo].isEnum() {
		return cp.Vm.concreteTypeInfo[tNo].getName(DEFAULT)
	}
	if convStr, ok := fConvert[tNo]; ok {
		return convStr
	}
	cp.P.Throw("golang/conv/c", &token.Token{Source: "golang conversion function"})
	return ""
}

// Used by the previous function.
var fConvert = map[values.ValueType]string{
	values.INT:    "int",
	values.FLOAT:  "float64",
	values.FUNC:   "func(args ...any) any",
	values.LABEL:  "string",
	values.TYPE:   "string",
	values.STRING: "string",
	values.RUNE:   "rune",
	values.LIST:   "[]any",
	values.PAIR:   "[]any",
	values.SET:    "[]any",
	values.TUPLE:  "[]any",
	values.BOOL:   "bool",
}

// The Go functions we call are not the Go functions the user writes. We must take the original
// function and give it a new header.
func (cp *Compiler) generateGoFunctionCode(gh *GoHandler, fnName string, sig, rTypes ast.StringSig, golang *ast.GolangExpression, pfDir string) {

	source := golang.GetToken().Source

	// We generate the signature of a function that can accept any types and accept any values. his
	fnString := "func " + text.Capitalize(fnName) + "(args ...any) any {\n\n"

	for i, v := range sig {
		preconv := ""
		postconv := ""
		ok := false
		if golang.Raw[i] {
			gh.rawHappened = true
			postconv = ".(values.Value)"
		} else {
			preconv, postconv, ok = cp.generateTypeConversionSyntax(gh, v.VarType)
			if !ok {
				cp.P.Throw("golang/type", golang.GetToken(), v.VarType)
				return
			}
		}
		fnString = fnString + "    " + v.VarName + " := " + preconv + "args[" + strconv.Itoa(i) + "]" + postconv + "\n"
	}
	for _, v := range rTypes {
		_, _, ok := cp.generateTypeConversionSyntax(gh, v.VarType) // Note that this will add struct types to the GoHandler's list of them.
		if !ok {
			gh.Prsr.Throw("golang/type/c", golang.GetToken(), v.VarType)
			return
		}
	}
	fnString = fnString + doctorReturns(golang.Token.Literal) + "\n\n"
	gh.Modules[source] = gh.Modules[source] + fnString
}

// This is used by the function above. In order to make this gimcrack system work, we actually declare the
// Go functions as being of type 'func(args ... any) any'. Then we declare all the necessary variables at the
// top of the function, and set them equal to the appropriate element of args cast to the appropriate type.
// This helps out by generating bits of code to put before and after the 'args[x]` bit of that to cast it.
func (cp *Compiler) generateTypeConversionSyntax(gh *GoHandler, pfType string) (string, string, bool) {
	if len(pfType) >= 3 && pfType[:3] == "..." {
		return "", ".([]any)", true // Since whatever the type is, it turns into a tuple which is converted to a slice before being pssed to the Go function.
	} // TODO --- we should flag unconvertable types at compile time but for now it's their own silly fault.
	goTy, ok := typeConv[pfType]
	if ok {
		if pfType == "int" { // TODO --- I forget why I have to do this and should find out if I can stop.
			return "int(", goTy, true
		} else {
			return "", goTy, true
		}
	}
	typeInfo, _ := cp.getTypeInformation(pfType)
	switch typeInfo := typeInfo.(type) {
	case structType:
		gh.StructNames.Add(pfType)
		return "", ".(" + pfType + ")", true
	case enumType:
		gh.EnumNames.Add(pfType)
		return pfType + "(", ".(int))", true
	case cloneType:
		gh.CloneNames.Add(pfType)
		return pfType + "(", ".(" + cloneConv[typeInfo.parent] + "))", true
	}
	return "", "", false
}

// Used by the function above.
var typeConv = map[string]string{"bling": ".(string)",
	"bool":   ".(bool)",
	"error":  ".(error)",
	"float":  ".(float64)",
	"func":   ".(func(args ...any) any)",
	"int":    ".(int))", // Extra parenthesis matches the kludge above.
	"label":  ".(string)",
	"list":   ".([]any)",
	"pair":   ".([]any)",
	"rune":   ".(rune)",
	"set":    ".([]any)",
	"any":    "",
	"string": ".(string)",
	"tuple":  ".([]any)",
	"type":   ".(string)",
}

// TODO --- this would also pick out "return" in quotes, comments,
// you need to do a better one.
func doctorReturns(body string) string {
	output := ""
	for ix := strings.Index(body, "return "); ix != -1; ix = strings.Index(body, "return ") {

		output = output + body[:ix]

		body = body[ix+7:]

		returnBody := ""
		for {
			lineEnd := strings.IndexAny(body, "\n\r")
			ix = lineEnd
			if lineEnd == -1 {
				panic("Tim, you goofed. Lines are meant to have endings.")
			}
			newLine := strings.Trim(body[:lineEnd], "\n\r \t")
			if returnBody != "" {
				returnBody = returnBody + "\n"
			}
			returnBody = returnBody + newLine
			// This also is hacky but will work until I can do a simple Go lexer to do it all properly.
			// Or there's probably a library ... ?
			if lastChar := newLine[len(newLine)-1]; !(lastChar == '{' || lastChar == '(' ||
				lastChar == '|' || lastChar == '&' || lastChar == '+' || lastChar == '-' || lastChar == '*' ||
				lastChar == '/' || lastChar == ',' || lastChar == '=' || lastChar == '!' || lastChar == '<' ||
				lastChar == '>' || lastChar == '.') {
				break
			}

		}
		body = body[ix:]
		if len(returnBody) >= 7 && returnBody[:7] == "golang " {
			output = output + "return " + returnBody[7:]
		} else {
			output = output + "return tuplify(" + returnBody + ")"
		}
	}

	return output + body
}

// This is called directly from the initializer to shove the pure Go blocks into the code.
func (gh *GoHandler) addPureGoBlock(source, code string) {
	gh.Modules[source] = gh.Modules[source] + "\n" + code[:len(code)-2] + "\n\n"
}
