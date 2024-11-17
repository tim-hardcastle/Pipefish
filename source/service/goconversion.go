package service

// Converts values from Pipefish to Go and back for the vm.

import (
	"pipefish/source/ast"
	"pipefish/source/report"
	"pipefish/source/text"
	"pipefish/source/token"
	"pipefish/source/values"
	"strconv"
	"strings"
)

func (vm *Vm) pipefishToGo(v values.Value, converter func(uint32, []any) any) any {
	typeInfo := vm.concreteTypes[v.T]
	if typeInfo.isStruct() {
		pVals := v.V.([]values.Value)
		gVals := make([]any, 0, len(pVals))
		for _, v := range pVals {
			gVals = append(gVals, vm.pipefishToGo(v, converter))
		}
		return converter(uint32(v.T), gVals)
	}
	if typeInfo.isEnum() {
		return v.V.(int)
	}
	switch v.T {
	case values.BOOL:
		return v.V.(bool)
	case values.FLOAT:
		return v.V.(float64)
	case values.INT:
		return v.V.(int)
	case values.RUNE:
		return v.V.(rune)
	case values.STRING:
		return v.V.(string)
	default:
		panic("Can't convert Pipefish value.")
	}
}

func (vm *Vm) goToPipefish(v any, structConverter func(any) (uint32, []any, bool), enumConverter func(any) (uint32, int)) values.Value {
	switch v := v.(type) {
	case *values.GoReturn:
		result := make([]values.Value, 0, len(v.Elements))
		for _, el := range v.Elements {
			result = append(result, vm.goToPipefish(el, structConverter, enumConverter))
		}
		return values.Value{values.TUPLE, result}
	case bool:
		return values.Value{values.BOOL, v}
	case error:
		return values.Value{values.ERROR, report.Error{Message: v.Error()}}
	case float64:
		return values.Value{values.FLOAT, v}
	case int:
		return values.Value{values.INT, v}
	case nil:
		return values.Value{values.NULL, v}
	case rune:
		return values.Value{values.RUNE, v}
	case string:
		return values.Value{values.STRING, v}
	case values.Value:
		return v
	}
	structType, gVals, ok := structConverter(v)
	if ok {
		pVals := make([]values.Value, 0, len(gVals))
		for _, gVal := range gVals {
			pVals = append(pVals, vm.goToPipefish(gVal, structConverter, enumConverter))
		}
		return values.Value{values.ValueType(structType), pVals}
	}
	enumType, val := enumConverter(v)
	if enumType != 0 { // Which is never used for anything, and so as may well be a sentinel.
		return values.Value{values.ValueType(enumType), val}
	}

	return values.Value{values.ERROR, &report.Error{ErrorId: "golang/conv/a", Token: &token.Token{Source: "golang conversion function"}}}
}

// In order for the Golang interop to work with structs, each go file must declare the structs it needs plus
// a function which can convert the Go structs back into Pipefish structs. This function prepares this as a
// snippet of text which can be added to the Go source code we're compiling.
func (cp *Compiler) MakeTypeDeclarationsForGo(goHandler *GoHandler, source string) string {
	convGoEnumToPfEnum := "\nfunc ConvertGoEnumToPipefish(v any) (uint32, int) {\n\tswitch v.(type) {\n"
	decs := "\n" // The type declarations.
	// We do the enum converters.
	for eType := range goHandler.EnumNames[source] {
		bits := strings.Split(eType, ".")
		name := bits[len(bits)-1]
		namespacePath := bits[0 : len(bits)-1]
		resolvingCompiler := cp.getResolvingCompiler(&ast.TypeLiteral{Value: name, Token: token.Token{Source: "function making structs for Go"}}, namespacePath, DEF) // DEF could be anything but REPL, we're not passing it on, we're just checking for encapsulation.
		abType := resolvingCompiler.P.GetAbstractType(name)
		if abType.Len() != 1 {
			cp.Throw("golang/type/concrete", token.Token{Source: "function making structs for Go"}, name)
			continue
		}
		concType := abType.Types[0]
		convGoEnumToPfEnum = "\n\tcase " + name + " : \n\t\treturn uint32(" + strconv.Itoa(int(concType)) + "), int(v.(name))"
		firstEnumElement := cp.Vm.concreteTypes[concType].(enumType).elementNames[0]
		decs = decs + "type " + name + " int\n\n const (" + firstEnumElement + " " + name + " = iota\n"
		for _, element := range cp.Vm.concreteTypes[concType].(enumType).elementNames[1:] {
			decs = decs + "    " + element + "\n"
		}
		decs = decs + ")\n\n"
	}	
	convGoEnumToPfEnum = convGoEnumToPfEnum + "\tdefault:\n\t\treturn uint32(0), 0\n\t}\n}\n\n"

	// Now the struct convertors.
	convGoStructToPfStruct := "\nfunc ConvertGoStructHalfwayToPipefish(v any) (uint32, []any, bool) {\n\tswitch v.(type) {"
	makeGoStruct := "\nfunc ConvertPipefishStructToGoStruct(T uint32, args []any) any {\n\tswitch T {"
	// The conversion function.
	for el := range goHandler.StructNames[source] {
		bits := strings.Split(el, ".")
		name := bits[len(bits)-1]
		goStructName := text.Flatten(el)
		namespacePath := bits[0 : len(bits)-1]
		resolvingCompiler := cp.getResolvingCompiler(&ast.TypeLiteral{Value: name, Token: token.Token{Source: "function making structs for Go"}}, namespacePath, DEF) // DEF could be anything but REPL, we're not passing it on, we're just checking for encapsulation.
		structTypeNumber := resolvingCompiler.StructNameToTypeNumber[name]
		// We add the definition of the struct.
		typeDefStr := "\ntype " + goStructName + " struct {\n"
		for i, lN := range cp.Vm.concreteTypes[structTypeNumber].(structType).labelNumbers {
			typeDefStr = typeDefStr + "\t" + text.Flatten(cp.Vm.Labels[lN]) + " " + cp.ConvertFieldType(cp.Vm.concreteTypes[structTypeNumber].(structType).abstractStructFields[i]) + "\n"
		}
		typeDefStr = typeDefStr + "}\n"
		decs = decs + typeDefStr
		// We add part of a type switch that helps convert a Go struct to Pipefish.
		convGoStructToPfStruct = convGoStructToPfStruct + "\n\tcase " + goStructName + " : \n\t\treturn uint32(" + strconv.Itoa(int(structTypeNumber)) + ")"
		convGoStructToPfStruct = convGoStructToPfStruct + ", []any{"
		sep := ""
		for _, lN := range cp.Vm.concreteTypes[structTypeNumber].(structType).labelNumbers {
			convGoStructToPfStruct = convGoStructToPfStruct + sep + "v.(" + goStructName + ")." + text.Flatten(cp.Vm.Labels[lN])
			sep = ", "
		}
		convGoStructToPfStruct = convGoStructToPfStruct + "}, true\n"
		// We add part of a type switch that helps convert a Pipefish struct to Go.
		makeGoStruct = makeGoStruct + "\n\tcase " + strconv.Itoa(int(structTypeNumber)) + " : \n\t\treturn " + goStructName + "{"
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

func (cp *Compiler) ConvertFieldType(aT values.AbstractType) string {
	if aT.Len() > 1 {
		cp.P.Throw("golang/conv/b", &token.Token{Source: "golang conversion function"})
	}
	tNo := aT.Types[0]
	if cp.Vm.concreteTypes[tNo].isEnum() || cp.Vm.concreteTypes[tNo].isStruct() {
		return text.Flatten(cp.Vm.concreteTypes[tNo].getName(LITERAL))
	}
	if convStr, ok := fConvert[tNo]; ok {
		return convStr
	}
	cp.P.Throw("golang/conv/c", &token.Token{Source: "golang conversion function"})
	return ""
}

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
	values.ERROR:  "error",
}
