package service

// Code for generating golang to be turned into plugins.

import (
	"os"
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

// In the case of the structs, the Go file also needs to do most of the heavy lifting on its end to turn Pipefish
// structs into Go.

// So this prepares this Go code as a snippet of text which can be added to the Go source code we're compiling.
func (cp *Compiler) MakeTypeDeclarationsForGo(goHandler *GoHandler, source string) string {
	// First let's make sure we really do have all the types we need by recursing thorugh the structs:
	cp.CloseTypeDeclarations(goHandler)

	// This is the string which will store all the generated type declarations.
	decs := "\n"

	// Then we hve another string in which we'll store a single function to convert Go clones to Piefish.
	// convGoCloneToPfClone := "\nfunc ConvertGoCloneToPipefish(v any) (uint32, int) {\n\tswitch v.(type) {\n"

// We do the enum declaations and converters.

	convGoEnumToPfEnum := "\nfunc ConvertGoEnumToPipefish(v any) (uint32, int) {\n\tswitch v.(type) {\n"
	
	
	for name := range goHandler.EnumNames {
		concType, ok := cp.getConcreteType(name)
		if !ok {
			cp.Throw("golang/type/concrete/b", token.Token{Source: "golang interop"}, name)
			continue
		}
		convGoEnumToPfEnum = convGoEnumToPfEnum + "\n\tcase " + name + " : \n\t\treturn uint32(" + strconv.Itoa(int(concType)) + "), int(v.("+name+"))"
		firstEnumElement := cp.Vm.concreteTypes[concType].(enumType).elementNames[0]
		decs = decs + "type " + name + " int\n\n const (\n    " + firstEnumElement + " " + name + " = iota\n"
		for _, element := range cp.Vm.concreteTypes[concType].(enumType).elementNames[1:] {
			decs = decs + "    " + element + "\n"
		}
		decs = decs + ")\n\n"
	}	
	convGoEnumToPfEnum = convGoEnumToPfEnum + "\n\tdefault:\n\t\treturn uint32(0), 0\n\t}\n}\n\n"

	// Now the struct convertors.

	convGoStructToPfStruct := "\nfunc ConvertGoStructHalfwayToPipefish(v any) (uint32, []any, bool) {\n\tswitch v.(type) {"
	makeGoStruct := "\nfunc ConvertPipefishStructToGoStruct(T uint32, args []any) any {\n\tswitch T {"
	// The conversion function.
	for name := range goHandler.StructNames {
		structTypeNumber := cp.StructNameToTypeNumber[name]
		// We add the definition of the struct.
		typeDefStr := "\ntype " + name + " struct {\n"
		for i, lN := range cp.Vm.concreteTypes[structTypeNumber].(structType).labelNumbers {
			typeDefStr = typeDefStr + "\t" + (cp.Vm.Labels[lN]) + " " + cp.ConvertFieldType(cp.Vm.concreteTypes[structTypeNumber].(structType).abstractStructFields[i]) + "\n"
		}
		typeDefStr = typeDefStr + "}\n"
		decs = decs + typeDefStr
		// We add part of a type switch that helps convert a Go struct to Pipefish.
		convGoStructToPfStruct = convGoStructToPfStruct + "\n\tcase " + name + " : \n\t\treturn uint32(" + strconv.Itoa(int(structTypeNumber)) + ")"
		convGoStructToPfStruct = convGoStructToPfStruct + ", []any{"
		sep := ""
		for _, lN := range cp.Vm.concreteTypes[structTypeNumber].(structType).labelNumbers {
			convGoStructToPfStruct = convGoStructToPfStruct + sep + "v.(" + name + ")." + cp.Vm.Labels[lN]
			sep = ", "
		}
		convGoStructToPfStruct = convGoStructToPfStruct + "}, true\n"
		// We add part of a type switch that helps convert a Pipefish struct to Go.
		makeGoStruct = makeGoStruct + "\n\tcase " + strconv.Itoa(int(structTypeNumber)) + " : \n\t\treturn " + name + "{"
		sep = ""
		for i, ty := range cp.Vm.concreteTypes[structTypeNumber].(structType).abstractStructFields {
			makeGoStruct = makeGoStruct + sep + "args[" + strconv.Itoa(i) + "].(" + cp.ConvertFieldType(ty) + ")"
			sep = ", "
		}
		makeGoStruct = makeGoStruct + "}\n"
	}
	convGoStructToPfStruct = convGoStructToPfStruct + "\tdefault:\n\t\treturn uint32(0), []any{}, false\n\t}\n}\n\n"
	makeGoStruct = makeGoStruct + "\tdefault:\n\t\tpanic(\"I'm not sure if this error can arise.\")\n\t}\n}\n\n"
	return decs + convGoEnumToPfEnum + convGoStructToPfStruct + makeGoStruct
}

// Auxilliary to the function above. It produces the names of the types in the struct declarations 
// generated for the Go code.
func (cp *Compiler) ConvertFieldType(aT values.AbstractType) string {
	if aT.Len() > 1 {
		cp.P.Throw("golang/concrete/c", &token.Token{Source: "golang interop"})
	}
	tNo := aT.Types[0]
	if cp.Vm.concreteTypes[tNo].isEnum() || cp.Vm.concreteTypes[tNo].isStruct() {
		return cp.Vm.concreteTypes[tNo].getName(DEFAULT)
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



func (cp *Compiler) MakeFunction(gh *GoHandler, keyword string, sig, rTypes ast.StringSig, golang *ast.GolangExpression, pfDir string) {

	// We check to see whether the source code has been modified.

	source := golang.GetToken().Source
	doctoredFilename := MakeFilepath(source, pfDir)
	_, err := os.Stat(doctoredFilename)
	if err != nil {
		panic("GoHandler MakeFunction " + err.Error())
	}

	// If the source has been modified, we proceed ...

	fnString := "func " + text.Capitalize(keyword) + "(args ...any) any {\n\n"
	for i, v := range sig {
		preconv := ""
		postconv := ""
		ok := false
		if golang.Raw[i] {
			gh.rawHappened = true
			postconv = ".(values.Value)"
		} else {
			preconv, postconv, ok = cp.doTypeConversion(gh, v.VarType)
			if !ok {
				cp.P.Throw("golang/type", golang.GetToken(), v.VarType)
				return
			}
		}
		fnString = fnString + "    " + v.VarName + " := " + preconv + "args[" + strconv.Itoa(i) + "]" + postconv + "\n"
	}

	for _, v := range rTypes {
		_, _, ok := cp.doTypeConversion(gh, v.VarType) // Note that this will add struct types to the GoHandler's list of them.
		if !ok {
			gh.Prsr.Throw("golang/type/c", golang.GetToken(), v.VarType)
			return
		}
	}
	fnString = fnString + doctorReturns(golang.Token.Literal) + "\n\n"
	gh.Modules[source] = gh.Modules[source] + fnString
}

func (gh *GoHandler) AddPureGoBlock(source, code string) {
	gh.Modules[source] = gh.Modules[source] + "\n" + code[:len(code)-2] + "\n\n"
}

// This is used by the function above. In order to make this gimcrack system work, we actually declare the 
// Go functions as being of type 'func(args ... any) any'. Then we declare all the necessary variables at the
// top of the function, and set them equal to the appropriate element of args cast to the appropriate type.
// This helps out by generating bits of code to put before and after the 'args[x]` bit of that to cast it.
func (cp *Compiler) doTypeConversion(gh *GoHandler, pTy string) (string, string, bool) {
	if len(pTy) >= 3 && pTy[:3] == "..." {
		return "", ".([]any)", true // Since whatever the type is, it turns into a tuple which is converted to a slice before being pssed to the Go function.
	} // TODO --- we should flag unconvertable types at compile time but for now it's their own silly fault.
	goTy, ok := typeConv[pTy]
	if ok {
		if pTy == "int" { // TODO --- I forget why I have to do this and should find out if I can stop.
			return "int(", goTy, true
		} else {
			return "", goTy, true
		}
	}

	if gh.Prsr.Structs.Contains(pTy) {
		gh.StructNames.Add(pTy)
		return "", ".(" + pTy + ")", true
	}
	abType := gh.Prsr.GetAbstractType(pTy)
	if abType.IsSubtypeOf(gh.Prsr.Common.Types["enum"]) {
		gh.EnumNames.Add(pTy)
		return pTy + "(", ".(int))", true
	}

	return "", "", false
}

// Used by the function above.
var typeConv = map[string]string{"bling": ".(string)",
	"bool":   ".(bool)",
	"error":  ".(error)",
	"float":  ".(float64)",
	"func":   ".(func(args ...any) any)",
	"int":    ".(int))", // Extra parenthesis matches the kludge below.
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

//
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

