package service

// Converts values from Pipefish to Go and back for the vm.

import (
	"pipefish/source/dtypes"
	"pipefish/source/report"
	"pipefish/source/token"
	"pipefish/source/values"
	"strconv"
)

// How the vm performs conversion at runtime.
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

// How the VM performs conversion at runtime.
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

func (cp *Compiler) CloseTypeDeclarations(goHandler *GoHandler) {
	structsToCheck := goHandler.StructNames
	for newStructsToCheck := make(dtypes.Set[string]) ; len(structsToCheck) > 0; {
		for structName := range structsToCheck {
			structTypeNumber := cp.StructNameToTypeNumber[structName]
			for _, fieldType := range cp.Vm.concreteTypes[structTypeNumber].(structType).abstractStructFields {
				if fieldType.Len() != 1 {
					cp.Throw("golang/type/concrete/a", token.Token{Source: "golang interop"}, cp.Vm.DescribeAbstractType(fieldType, LITERAL))
				}
				typeOfField := fieldType.Types[0]
				switch fieldData := cp.Vm.concreteTypes[typeOfField].(type) {
				case cloneType : 
					goHandler.CloneNames.Add(fieldData.name)
				case enumType :
					goHandler.EnumNames.Add(fieldData.name)
				case structType :
					if !goHandler.StructNames.Contains(fieldData.name) {
						newStructsToCheck.Add(fieldData.name)
						goHandler.StructNames.Add(fieldData.name)
					}
				default :
					// As other type-checking needs to be done for things that are not in structs anyway,
					// it would be superfluous to do it here.	
				}
			}
		}
		structsToCheck = newStructsToCheck
	}
}

// In order for the Golang interop to work with structs, each Go file must declare the structs and enums and
// clone types but also functions which can at least partially turn the Go structs and enums back into 
// Pipefish structs, and enums. (Because it can perform reflecton on its own type system whereas the calling VM
// can know nothing about this.)

// So this prepares this Go code as a snippet of text which can be added to the Go source code we're compiling.
func (cp *Compiler) MakeTypeDeclarationsForGo(goHandler *GoHandler, source string) string {
	// First let's make sure we really do have all the types we need by recursing thorugh the structs:
	cp.CloseTypeDeclarations(goHandler)

	decs := "\n" // The type declarations. These will be shoved into one big chunk of code for all the types. This is where we keep it.
	// Then we hve another string in which we'll store a single function to convert Go clones to Piefish.
	// convGoCloneToPfClone := "\nfunc ConvertGoCloneToPipefish(v any) (uint32, int) {\n\tswitch v.(type) {\n"


	convGoEnumToPfEnum := "\nfunc ConvertGoEnumToPipefish(v any) (uint32, int) {\n\tswitch v.(type) {\n"
	
	// We do the enum converters.
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
