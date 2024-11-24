package service

import (
	"bufio"
	"maps"
	"os"
	"os/exec"
	"path/filepath"
	"plugin"
	"reflect"
	"strconv"
	"strings"

	"pipefish/source/ast"
	"pipefish/source/dtypes"
	"pipefish/source/parser"
	"pipefish/source/text"
	"pipefish/source/token"
)

// The Gohandler stores information gathered during compilation to generate code from which Go plugins
// are then compiled, and then builds the plugins from the generated code. The actual code generation is
// kept in the gogen file in this same package.

// This file is in effect in chronological order of how the various bits are used, i.e. it contains:

// * The declaration of the type.
// * The newGoHandler function.
// * The populateTypeLists function.
// * The addPureGoBlock function.
// * The getGoFunctions function.
// * The transitivelyCloseTypes function.
// * The buildGoModule function.
// * The recordGoTimes function.
// * The getFn function.

var counter int

type GoHandler struct {
	Prsr             *parser.Parser            // The parser, to tell the GoHandler what the types mean.
	timeMap          map[string]int            // When the source code was constructed.
	Plugins          *plugin.Plugin // Knows where the plugins live after they've been generated.
	UserDefinedTypes dtypes.Set[string]        // Set of Pipefish clone/enum/struct types appearing explicitly or implicitly in the sigs of the functions.
	TypeDeclarations string         // A string to put the generated source code for declaring things in.
	output           string
	sources          dtypes.Set[string]
	GoImports       []string
}

func (cp *Compiler) newGoHandler() {

	gh := GoHandler{
		timeMap:           make(map[string]int),
		UserDefinedTypes:  make(dtypes.Set[string]),
		sources:		   make(dtypes.Set[string]),
		GoImports:		   make([]string, 0),
	}

	file, err := os.Open(cp.P.Directory + "rsc/go/gotimes.dat")
	if err != nil {
		panic("Can't open file 'rsc/go/gotimes.dat'.")
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	for i := 0; i < (len(lines) / 2); i++ {
		time, _ := strconv.Atoi(lines[(2*i)+1])
		gh.timeMap[lines[2*i]] = time
	}

	cp.goHandler = &gh
}

// This is called directly from the initializer to shove the pure Go blocks into the code.
func (gh *GoHandler) addPureGoBlock(code string) {
	gh.output = gh.output + "\n" + code[:len(code)-2] + "\n\n"
}

func (cp *Compiler) getGoFunctions() {
	
	if len(cp.goHandler.sources) == 0 {
		return
	}

	cp.transitivelyCloseTypes()
	cp.goHandler.TypeDeclarations = cp.generateDeclarations()
	if cp.P.ErrorsExist() {
		return
	}
	
	cp.buildGoModules()
	if cp.P.ErrorsExist() {
		return
	}
	newGoConverter := make([](func(t uint32, v any) any), len(cp.Vm.concreteTypeInfo))
	copy(newGoConverter, cp.Vm.goConverter)
	
	functionConverterSymbol, _ := cp.goHandler.Plugins.Lookup("PIPEFISH_FUNCTION_CONVERTER")
	functionConverter := *functionConverterSymbol.(*map[string](func(t uint32, v any) any))
	maps.Copy(functionConverter, BUILTIN_FUNCTION_CONVERTER)
	for typeName, constructor := range functionConverter {
		typeNumber := cp.concreteTypeNow(typeName)
		newGoConverter[typeNumber] = constructor
	}
	cp.Vm.goConverter = newGoConverter
	valueConverterSymbol, _ := cp.goHandler.Plugins.Lookup("PIPEFISH_VALUE_CONVERTER")
	valueConverter := *valueConverterSymbol.(*map[string]any)
	maps.Copy(valueConverter, BUILTIN_VALUE_CONVERTER)
	for typeName, goValue := range valueConverter {
		cp.Vm.goToPipefishTypes[reflect.TypeOf(goValue).Elem()] = cp.concreteTypeNow(typeName)
	}
	
	if cp.P.NamespacePath == "" {
		for k, v := range cp.P.Common.Functions {
			if v.Body.GetToken().Type == token.GOCODE {
				result := cp.goHandler.getFn(k.FunctionName, v.Body.GetToken())
				v.Body.(*ast.GolangExpression).GoFunction = reflect.Value(result)
			}
		}
	}
	for functionName, fns := range cp.P.FunctionTable { // TODO --- why are we doing it like this?
		for _, v := range fns {
			if v.Body.GetToken().Type == token.GOCODE {
				result := cp.goHandler.getFn(functionName, v.Body.GetToken())
				v.Body.(*ast.GolangExpression).GoFunction = reflect.Value(result)
			}
		}
	}
	cp.recordGoTimes()
}

var BUILTIN_FUNCTION_CONVERTER = map[string](func(t uint32, v any) any){
    "bool": func(t uint32, v any) any {return v.(bool)},
    "float": func(t uint32, v any) any {return v.(float64)},
    "int": func(t uint32, v any) any {return v.(int)},
    "rune": func(t uint32, v any) any {return v.(rune)},
    "string": func(t uint32, v any) any {return v.(string)},
}

var BUILTIN_VALUE_CONVERTER = map[string]any{
    "bool": (*bool)(nil),
    "float":  (*float64)(nil),
    "int":  (*int)(nil),
    "rune":  (*rune)(nil),
    "string":  (*string)(nil),
}

// This makes sure that if  we're generating declarations for a struct type,
// we're also generating declarations for the types of its fields if need be, and so on recursively. We do
// a traditional non-recursive breadth-first search.
func (cp *Compiler) transitivelyCloseTypes() {
	structsToCheck := dtypes.Set[string]{} 
	for name := range cp.goHandler.UserDefinedTypes {
		if cp.isStruct(name) {
			structsToCheck.Add(name)
		}
	}
	for newStructsToCheck := make(dtypes.Set[string]); len(structsToCheck) > 0; {
		for structName := range structsToCheck {
			for _, fieldType := range cp.typeInfoNow(structName).(structType).abstractStructFields {
				if fieldType.Len() != 1 {
					cp.Throw("golang/type/concrete/a", token.Token{Source: "golang interop"}, cp.Vm.DescribeAbstractType(fieldType, LITERAL))
				}
				typeOfField := cp.getTypeNameFromNumber(fieldType.Types[0])
				switch fieldData := cp.typeInfoNow(typeOfField).(type) {
				case cloneType:
					cp.goHandler.UserDefinedTypes.Add(fieldData.getName(DEFAULT))
				case enumType:
					cp.goHandler.UserDefinedTypes.Add(fieldData.getName(DEFAULT))
				case structType:
					if !cp.goHandler.UserDefinedTypes.Contains(fieldData.name) {
						newStructsToCheck.Add(fieldData.name)
						cp.goHandler.UserDefinedTypes.Add(fieldData.name)
					}
				}
			}
		}
		structsToCheck = newStructsToCheck
	}
}

func (cp *Compiler) buildGoModules() {

	// We find if we need to rebuild the Go.

	// The purpose of putting timestamps on the .so files is not that we ever read the timestamps
	// from the filenames (we look either at the OS metadata of the file or at the 'gotimes' file),
	// but simply because you can't re-use the names of .so files in the same Go runtime and since
	// we're looking up times anyway this is a reasonable way to achieve that.

	needsRebuild := false
	var lastChange int
	var maxLastChange int 
	var maxModifiedTime int64

	for source := range cp.goHandler.sources {

		f, err := os.Stat(MakeFilepath(source, cp.P.Directory))
		if err != nil {
			cp.Throw("go/file", token.Token{Source: "linking Golang"}, err.Error())
			needsRebuild = false
			break
		}
		modifiedTime := f.ModTime().UnixMilli()
		if modifiedTime > maxModifiedTime {
			maxModifiedTime = modifiedTime
		}
		lastChange, ok := cp.goHandler.timeMap[source]
		if lastChange > maxLastChange {
			maxLastChange = lastChange
		}
		if !ok || modifiedTime != int64(lastChange) {
			needsRebuild = true 
			break
		}
	}

	if !needsRebuild {
		soFile := cp.P.Directory + "rsc/go/" + text.Flatten(cp.ScriptFilepath) + ".so"
		plugins, err := plugin.Open(soFile)
		if err == nil {
			cp.goHandler.Plugins = plugins
			return
		}
		println("Error building/using .so file")
		println("Error was", err.Error())
		panic("That's all folks.")
	} else {

		preface := "package main\n\n"

		if len(cp.goHandler.GoImports) > 0 {
			preface = preface + "import (\n"
			for _, v := range cp.goHandler.GoImports {
				preface = preface + "    \"" + v + "\"\n"
			}
			preface = preface + ")\n\n"
		}

		counter++ // The number of the gocode_<counter>.go source file we're going to write.
		soFile := filepath.Join(cp.P.Directory, filepath.FromSlash("rsc/go/"+ text.Flatten(cp.ScriptFilepath) + "_" + strconv.Itoa(int(maxModifiedTime)) + ".so"))
		if lastChange != 0 {
			os.Remove(filepath.Join(cp.P.Directory, filepath.FromSlash("rsc/go/"+ text.Flatten(cp.ScriptFilepath) + "_" + strconv.Itoa(maxLastChange) + ".so")))
		}
		goFile := filepath.Join(cp.P.Directory, "gocode_"+strconv.Itoa(counter)+".go")
		file, _ := os.Create(goFile)
		file.WriteString(preface + cp.goHandler.TypeDeclarations + cp.goHandler.output)
		file.Close()
		cmd := exec.Command("go", "build", "-buildmode=plugin", "-o", soFile, goFile) // Version to use running from terminal.
		// cmd := exec.Command("go", "build", "-gcflags=all=-N -l", "-buildmode=plugin", "-o", soFile, goFile) // Version to use with debugger.
		output, err := cmd.Output()
		if err != nil {
			cp.P.Throw("golang/build", &token.Token{}, err.Error()+": "+string(output))
		}
		cp.goHandler.Plugins, err = plugin.Open(soFile)
		if err != nil {
			cp.P.Throw("golang/open", &token.Token{}, err.Error())
		}
		if err == nil || strings.Contains(err.Error(), "plugin was built with a different version of package") {
			os.Remove("gocode_" + strconv.Itoa(counter) + ".go")
		}
	}
}

func (cp *Compiler) recordGoTimes() {

	// We add the newly compiled modules to the list of times.

	for k := range cp.goHandler.sources {
		filepath := MakeFilepath(k, cp.P.Directory)
		file, err := os.Stat(filepath)
		if err != nil {
			panic("Gohandler cleanup: " + err.Error())
		}
		modifiedTime := file.ModTime().UnixMilli()
		cp.goHandler.timeMap[k] = int(modifiedTime)
	}
	// And then write out the list of times to the .dat file.

	f, err := os.Create(cp.P.Directory + "rsc/go/gotimes.dat")
	if err != nil {
		panic("Can't create file rsc/go/gotimes.dat")
	}
	defer f.Close()
	for k, v := range cp.goHandler.timeMap {
		f.WriteString(k + "\n")
		f.WriteString(strconv.Itoa(v) + "\n")
	}
}

func (gh *GoHandler) getFn(fnName string, tok *token.Token) reflect.Value {
	name := text.Capitalize(fnName)
	fn, _ := gh.Plugins.Lookup(name)
	return reflect.ValueOf(fn)
}
