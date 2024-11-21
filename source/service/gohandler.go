package service

import (
	"bufio"
	"os"
	"os/exec"
	"path/filepath"
	"plugin"
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
	Modules          map[string]string         // Where the source files are.
	timeMap          map[string]int            // When the source code was constructed.
	Plugins          map[string]*plugin.Plugin // Knows where the plugins live after they've been generated.
	CloneNames       dtypes.Set[string]        // Set of Pipefish clone types appearing in the sigs of the functions.
	EnumNames        dtypes.Set[string]        // Set of Pipefish struct types appearing in the sigs of the functions.
	StructNames      dtypes.Set[string]        // Set of Pipefish enum types appearing in the sigs of the functions.
	TypeDeclarations map[string]string         // A string to put the generated source code for declaring things in.
}

func newGoHandler(prsr *parser.Parser) *GoHandler {

	gh := GoHandler{
		Prsr:        prsr,
		timeMap:     make(map[string]int),
		Modules:     make(map[string]string),
		Plugins:     make(map[string]*plugin.Plugin),
		CloneNames:  make(dtypes.Set[string]),
		EnumNames:   make(dtypes.Set[string]),
		StructNames: make(dtypes.Set[string]),
	}

	gh.TypeDeclarations = make(map[string]string)

	file, err := os.Open(gh.Prsr.Directory + "rsc/go/gotimes.dat")
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

	return &gh
}

// Takes a type name (fro a sig) and puts it where it needs to go.
func (cp *Compiler) populateTypeLists (gh *GoHandler, pfType string) {
	typeInfo, _ := cp.getTypeInformation(pfType)
	switch typeInfo.(type) {
	case structType:
		gh.StructNames.Add(pfType)
	case enumType:
		gh.EnumNames.Add(pfType)
	case cloneType:
		gh.CloneNames.Add(pfType)
	}
}

// This is called directly from the initializer to shove the pure Go blocks into the code.
func (gh *GoHandler) addPureGoBlock(source, code string) {
	gh.Modules[source] = gh.Modules[source] + "\n" + code[:len(code)-2] + "\n\n"
}

func (cp *Compiler) getGoFunctions(goHandler *GoHandler) {
	for source := range goHandler.Modules {
		cp.transitivelyCloseTypes(goHandler)
		goHandler.TypeDeclarations[source] = cp.generateDeclarationAndConversionCode(goHandler)
		if cp.P.ErrorsExist() {
			return
		}
	}
	goHandler.buildGoModules()
	if cp.P.ErrorsExist() {
		return
	}
	cp.goToPfStruct = map[string]func(any) (uint32, []any, bool){}
	cp.pfToGoStruct = map[string]func(uint32, []any) any{}
	cp.goToPfEnum = map[string](func(any) (uint32, int)){}
	cp.goToPfClone = map[string](func(any) (uint32, any)){}
	for source := range goHandler.Modules {
		fnSymbol, _ := goHandler.Plugins[source].Lookup("ConvertGoStructToPipefish")
		cp.goToPfStruct[source] = fnSymbol.(func(any) (uint32, []any, bool))
		fnSymbol, _ = goHandler.Plugins[source].Lookup("ConvertPipefishStructToGo")
		cp.pfToGoStruct[source] = fnSymbol.(func(uint32, []any) any)
		fnSymbol, _ = goHandler.Plugins[source].Lookup("ConvertGoEnumToPipefish")
		cp.goToPfEnum[source] = fnSymbol.(func(any) (uint32, int))
		fnSymbol, _ = goHandler.Plugins[source].Lookup("ConvertGoCloneToPipefish")
		cp.goToPfClone[source] = fnSymbol.(func(any) (uint32, any))
	}
	// TODO --- see if this plays nicely with function sharing and modules or if it needs more work.
	if cp.P.NamespacePath == "" {
		for k, v := range cp.P.Common.Functions {
			if v.Body.GetToken().Type == token.GOCODE {
				result := goHandler.getFn(text.Flatten(k.FunctionName), v.Body.GetToken())
				v.Body.(*ast.GolangExpression).ObjectCode = result
			}
		}
	}
	for functionName, fns := range cp.P.FunctionTable { // TODO --- why are we doing it like this?
		for _, v := range fns {
			if v.Body.GetToken().Type == token.GOCODE {
				result := goHandler.getFn(text.Flatten(functionName), v.Body.GetToken())
				v.Body.(*ast.GolangExpression).ObjectCode = result
			}
		}
	}
	goHandler.recordGoTimes()
}

// This makes sure that if  we're generating declarations for a struct type,
// we're also generating declarations for the types of its fields if need be, and so on recursively. We do
// a traditional non-recursive breadth-first search.
func (cp *Compiler) transitivelyCloseTypes(goHandler *GoHandler) {
	structsToCheck := goHandler.StructNames
	for newStructsToCheck := make(dtypes.Set[string]); len(structsToCheck) > 0; {
		for structName := range structsToCheck {
			structTypeNumber := cp.StructNameToTypeNumber[structName]
			for _, fieldType := range cp.Vm.concreteTypeInfo[structTypeNumber].(structType).abstractStructFields {
				if fieldType.Len() != 1 {
					cp.Throw("golang/type/concrete/a", token.Token{Source: "golang interop"}, cp.Vm.DescribeAbstractType(fieldType, LITERAL))
				}
				typeOfField := fieldType.Types[0]
				switch fieldData := cp.Vm.concreteTypeInfo[typeOfField].(type) {
				case cloneType:
					goHandler.CloneNames.Add(fieldData.name)
				case enumType:
					goHandler.EnumNames.Add(fieldData.name)
				case structType:
					if !goHandler.StructNames.Contains(fieldData.name) {
						newStructsToCheck.Add(fieldData.name)
						goHandler.StructNames.Add(fieldData.name)
					}
				default:
					// As other type-checking needs to be done for things that are not in structs anyway,
					// it would be superfluous to do it here.
				}
			}
		}
		structsToCheck = newStructsToCheck
	}
}

func (gh *GoHandler) buildGoModules() {

	for source, functionBodies := range gh.Modules {

		var modifiedTime int64
		f, err := os.Stat(MakeFilepath(source, gh.Prsr.Directory))
		if err == nil {
			modifiedTime = f.ModTime().UnixMilli()
		}

		lastChange, ok := gh.timeMap[source]
		if ok {
			if modifiedTime == int64(lastChange) {
				soFile := gh.Prsr.Directory + "rsc/go/" + text.Flatten(source) + "_" + strconv.Itoa(lastChange) + ".so"
				gh.Plugins[source], err = plugin.Open(soFile)
				if err == nil {
					continue
				}
				println("Error building/using .so file")
				println("Error was", err.Error())
				panic("That's all folks.")
			}
		}

		preface := "package main\n\n"

		if len(gh.Prsr.GoImports[source]) > 0 {
			preface = preface + "import (\n"
			for _, v := range gh.Prsr.GoImports[source] {
				preface = preface + "    \"" + v + "\"\n"
			}
			preface = preface + ")\n\n"
		}

		// You can't reuse the names of shared object files.
		counter++
		soFile := filepath.Join(gh.Prsr.Directory, filepath.FromSlash("rsc/go/"+text.Flatten(source)+"_"+strconv.Itoa(int(modifiedTime))+".so"))
		if lastChange != 0 {
			os.Remove(filepath.Join(gh.Prsr.Directory, filepath.FromSlash("rsc/go/"+text.Flatten(source)+"_"+strconv.Itoa(int(lastChange))+".so")))
		}
		goFile := filepath.Join(gh.Prsr.Directory, "gocode_"+strconv.Itoa(counter)+".go")
		file, _ := os.Create(goFile)
		file.WriteString(preface +  gh.TypeDeclarations[source] + functionBodies)
		file.Close()
		cmd := exec.Command("go", "build", "-buildmode=plugin", "-o", soFile, goFile) // Version to use running from terminal.
		// cmd := exec.Command("go", "build", "-gcflags=all=-N -l", "-buildmode=plugin", "-o", soFile, goFile) // Version to use with debugger.
		output, err := cmd.Output()
		if err != nil {
			gh.Prsr.Throw("golang/build", &token.Token{}, err.Error()+": "+string(output))
		}
		gh.Plugins[source], err = plugin.Open(soFile)
		if err != nil {
			gh.Prsr.Throw("golang/open", &token.Token{}, err.Error())
		}
		if err == nil || strings.Contains(err.Error(), "plugin was built with a different version of package") {
			os.Remove("gocode_" + strconv.Itoa(counter) + ".go")
		}
	}
}

func (gh *GoHandler) recordGoTimes() {

	// We add the newly compiled modules to the list of times.

	for k := range gh.Modules {
		filepath := MakeFilepath(k, gh.Prsr.Directory)
		file, err := os.Stat(filepath)
		if err != nil {
			panic("Gohandler cleanup: " + err.Error())
		}
		modifiedTime := file.ModTime().UnixMilli()
		gh.timeMap[k] = int(modifiedTime)
	}
	// And then write out the list of times to the .dat file.

	f, err := os.Create(gh.Prsr.Directory + "rsc/go/gotimes.dat")
	if err != nil {
		panic("Can't create file rsc/go/gotimes.dat")
	}
	defer f.Close()
	for k, v := range gh.timeMap {
		f.WriteString(k + "\n")
		f.WriteString(strconv.Itoa(v) + "\n")
	}
}

func (gh *GoHandler) getFn(fnName string, tok *token.Token) func(args ...any) any {
	name := text.Capitalize(fnName)
	fn, err := gh.Plugins[tok.Source].Lookup(name)
	if err != nil {
		gh.Prsr.Throw("golang/found", tok, name)
		return nil
	}
	fnToReturn := fn.(func(args ...any) any)
	return fnToReturn
}
