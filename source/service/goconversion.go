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
	switch v.T {
	case values.INT:
		return v.V.(int)
	case values.FLOAT:
		return v.V.(int)
	case values.STRING:
		return v.V.(string)
	case values.BOOL:
		return v.V.(bool)
	default:
		panic("Can't convert Pipefish value.")
	}
}

func (vm *Vm) goToPipefish(v any, converter func(any) (uint32, []any, bool)) values.Value {
	switch v := v.(type) {
	case *values.GoReturn:
		result := make([]values.Value, 0, len(v.Elements))
		for el := range v.Elements {
			result = append(result, vm.goToPipefish(el, converter))
		}
		return values.Value{values.TUPLE, result}
	case int:
		return values.Value{values.INT, v}
	case float64:
		return values.Value{values.FLOAT, v}
	case string:
		return values.Value{values.STRING, v}
	case bool:
		return values.Value{values.BOOL, v}
	case nil:
		return values.Value{values.NULL, v}
	case error:
		return values.Value{values.ERROR, report.Error{Message: v.Error()}}
	case values.Value:
		return v
	}
	structType, gVals, ok := converter(v)
	if ok {
		pVals := make([]values.Value, 0, len(gVals))
		for _, gVal := range gVals {
			pVals = append(pVals, vm.goToPipefish(gVal, converter))
		}
		return values.Value{values.ValueType(structType), pVals}
	}

	return values.Value{values.ERROR, &report.Error{ErrorId: "vm/golang/conv/a", Token: &token.Token{Source: "golang conversion function"}}}
}

// In order for the Golang interop to work with structs, each go file must declare the structs it needs plus
// a function which can convert the Go structs back into Pipefish structs. This function prepares this as a
// snippet of text which can be added to the Go source code we're compiling.
func (cp *Compiler) MakeTypeDeclarationsForGo(goHandler *GoHandler, source string) string {
	decs := "\n" // The type declarations.
	convGoTypeToPfType := "\nfunc ConvertGoStructHalfwayToPipefish(v any) (uint32, []any, bool) {\n\tswitch v.(type) {"
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
		for i, lN := range cp.vm.concreteTypes[structTypeNumber].(structType).labelNumbers {
			typeDefStr = typeDefStr + "\t" + text.Flatten(cp.vm.Labels[lN]) + " " + cp.ConvertFieldType(cp.vm.concreteTypes[structTypeNumber].(structType).abstractStructFields[i]) + "\n"
		}
		typeDefStr = typeDefStr + "}\n"
		decs = decs + typeDefStr
		// We add part of a type switch that helps convert a Go struct to Pipefish.
		convGoTypeToPfType = convGoTypeToPfType + "\n\tcase " + goStructName + " : \n\t\treturn uint32(" + strconv.Itoa(int(structTypeNumber)) + ")"
		convGoTypeToPfType = convGoTypeToPfType + ", []any{"
		sep := ""
		for _, lN := range cp.vm.concreteTypes[structTypeNumber].(structType).labelNumbers {
			convGoTypeToPfType = convGoTypeToPfType + sep + "v.(" + goStructName + ")." + text.Flatten(cp.vm.Labels[lN])
			sep = ", "
		}
		convGoTypeToPfType = convGoTypeToPfType + "}, true\n"
		// We add part of a type switch that helps convert a Pipefish struct to Go.
		makeGoStruct = makeGoStruct + "\n\tcase " + strconv.Itoa(int(structTypeNumber)) + " : \n\t\treturn " + goStructName + "{"
		sep = ""
		for i, ty := range cp.vm.concreteTypes[structTypeNumber].(structType).abstractStructFields {
			makeGoStruct = makeGoStruct + sep + "args[" + strconv.Itoa(i) + "].(" + cp.ConvertFieldType(ty) + ")"
			sep = ", "
		}
		makeGoStruct = makeGoStruct + "}\n"
	}
	convGoTypeToPfType = convGoTypeToPfType + "\tdefault:\n\t\treturn uint32(0), []any{}, false\n\t}\n}\n\n"
	makeGoStruct = makeGoStruct + "\tdefault:\n\t\tpanic(\"I'm not sure if this error can arise.\")\n\t}\n}\n\n"
	return decs + convGoTypeToPfType + makeGoStruct
}

func (cp *Compiler) ConvertFieldType(aT values.AbstractType) string {
	if aT.Len() > 1 {
		cp.P.Throw("golang/conv/b", &token.Token{Source: "golang conversion function"})
	}
	tNo := aT.Types[0]
	if tNo >= cp.vm.Ub_enums {
		return text.Flatten(cp.vm.concreteTypes[tNo].getName())
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
	values.LIST:   "[]any",
	values.PAIR:   "[]any",
	values.SET:    "[]any",
	values.TUPLE:  "[]any",
	values.BOOL:   "bool",
	values.ERROR:  "error",
}
