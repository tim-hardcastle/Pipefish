package service

import (
	"bufio"
	"fmt"
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
	"pipefish/source/settings"
	"pipefish/source/text"
	"pipefish/source/token"
)

// This allows the compiler to extract functions and converter data from the relevant `.so` files,
// rebuilding them if necessary.
//
//The code for generating the .go source code when a build/rebuild is necessary is kept in the
// `gogen.go` file in this same `service` package. This file is mainly devoted to housekeeping.
//
// When multiple sources are NULL-imported into a module, we must still treat each source with Go
// in it separately, otherwise for example we would have to produce a separate compilation of the
// `strings` library every time it was null-namespaed. Or `world`, which would be every time you
// run Pipefish.

// While the Pipefish needs to be recompiled each time the app it's in is recompiled, the Go doesn't,
// because the .so file hooks in just the same. Hence we can use the `gotimes.dat` file to keep track
// of when a given file was created. TODO --- this (or the practical equivalent is already metadata.
// The only additional purpose the `gotimes` file serves is that the time gives you a unique way of
// mangling the filename and the the file allows you to save the nae you came up with. But it seems like
// there should be a better way. (Ideally involving Google fixing the problem with .so files but I'm
// not holding my breath.))

var counter int // This variable is used to make a unique filename for each gocode_<counter>.go file.

// This struct type is used to accumulate the various data encountered during parsing that we need to 
// build a `.go` file or files. There is one per compiler. The "sources" are as given in the `Source`
// field of any GOCODE token encountered. The sources may be plural because of NULL-imports.
// The `imports`, `functions`, and `pureGo` maps are indexed by these sources.
type GoBucket struct {
	sources   dtypes.Set[string]
	imports   map[string][]string
	functions map[string][]*ast.PrsrFunction
	pureGo    map[string][]string
}

func (cp *Compiler) newGoBucket() {
	gh := GoBucket{
		sources:   make(dtypes.Set[string]),
		imports:   make(map[string][]string),
		functions: make(map[string][]*ast.PrsrFunction),
		pureGo:    make(map[string][]string),
	}
	cp.goBucket = &gh
}

// This will if necessary compile or recompile the relevant .so files, and will extract from them
// the functions and converter data needed by the compiler and vm and put it into its proper place.
func (cp *Compiler) compileGo() {

	// The purpose of putting timestamps on the .so files is not that we ever read the timestamps
	// from the filenames (we look either at the OS metadata of the file or at the 'gotimes' file),
	// but simply because you can't re-use the names of .so files in the same Go runtime and since
	// we're looking up times anyway this is a reasonable way to achieve that.

	// We get the blocks of pure Go, if any, and put them in the appropriate place in the goBucket.
	for _, golang := range cp.P.TokenizedDeclarations[golangDeclaration] {
		golang.ToStart()
		token := golang.NextToken()
		cp.goBucket.sources.Add(token.Source)
		cp.goBucket.pureGo[token.Source] = append(cp.goBucket.pureGo[token.Source], token.Literal[:len(token.Literal)-2])
	}

	timeMap := cp.getGoTimes() // We slurp a map from sources to times from the `gotimes` file.

	for source := range cp.goBucket.sources {
		f, err := os.Stat(MakeFilepath(source))
		if err != nil {
			cp.Throw("go/file", token.Token{Source: "linking Golang"}, err.Error())
			break
		}
		var plugins *plugin.Plugin
		sourceCodeModified := f.ModTime().UnixMilli()
		objectCodeModified, ok := timeMap[source]
		if !ok || sourceCodeModified != int64(objectCodeModified) {
			plugins = cp.makeNewSoFile(source, sourceCodeModified)
		} else {
			soFile := settings.PipefishHomeDirectory + "rsc/go/" + text.Flatten(source) + "_" + strconv.Itoa(int(sourceCodeModified)) + ".so"
			plugins, err = plugin.Open(soFile)
			if err != nil {
				cp.P.Throw("golang/open/b", &token.Token{}, err.Error())
				return
			}
		}

		// We extract the conversion data from the object code, reformat it, and store the results
		// in the vm.
		newGoConverter := make([](func(t uint32, v any) any), len(cp.Vm.concreteTypeInfo))
		copy(newGoConverter, cp.Vm.goConverter)
		functionConverterSymbol, _ := plugins.Lookup("PIPEFISH_FUNCTION_CONVERTER")
		functionConverter := *functionConverterSymbol.(*map[string](func(t uint32, v any) any))
		maps.Copy(functionConverter, BUILTIN_FUNCTION_CONVERTER)
		for typeName, constructor := range functionConverter {
			typeNumber := cp.concreteTypeNow(typeName)
			newGoConverter[typeNumber] = constructor
		}
		cp.Vm.goConverter = newGoConverter
		valueConverterSymbol, _ := plugins.Lookup("PIPEFISH_VALUE_CONVERTER")
		valueConverter := *valueConverterSymbol.(*map[string]any)
		maps.Copy(valueConverter, BUILTIN_VALUE_CONVERTER)
		for typeName, goValue := range valueConverter {
			cp.Vm.goToPipefishTypes[reflect.TypeOf(goValue).Elem()] = cp.concreteTypeNow(typeName)
			if typeName == "string" {
				println("string added to types")
				if reflect.TypeOf(goValue).Elem().String() == "string" {println("With the right thing pointing at it")}
			}
		}
		//We attach the compiled functions to the (pointers to) the functions, which are
		// also pointed to by the compiler's function table and by the list of common functions
		// in the common parser bindle. I.e. we are returning our result by mutating the
		// functions.
		for _, function := range cp.goBucket.functions[source] {
			goFunction, _ := plugins.Lookup(text.Capitalize(function.FName))
			function.Body.(*ast.GolangExpression).GoFunction = reflect.ValueOf(goFunction)
		}
	}
}

// But list, set, pair, and map can't go in here because of the recursion.
var BUILTIN_FUNCTION_CONVERTER = map[string](func(t uint32, v any) any){
	"bool":   func(t uint32, v any) any { return v.(bool) },
	"float":  func(t uint32, v any) any { return v.(float64) },
	"int":    func(t uint32, v any) any { return v.(int) },
	"rune":   func(t uint32, v any) any { return v.(rune) },
	"string": func(t uint32, v any) any { return v.(string) },
}

var BUILTIN_VALUE_CONVERTER = map[string]any{
	"bool":   (*bool)(nil),
	"float":  (*float64)(nil),
	"int":    (*int)(nil),
	"rune":   (*rune)(nil),
	"string": (*string)(nil),
}

// This makes a new .so file, opens it, and returns the plugins.
// Most of the code generation is in the `gogen.go` file in this same `service` package.
func (cp *Compiler) makeNewSoFile(source string, newTime int64) *plugin.Plugin {

	var StringBuilder strings.Builder
	sb := &StringBuilder

	// We emit the package declaration and builtins.

	fmt.Fprint(sb, "package main\n\n")
	if len(cp.goBucket.imports) > 0 {
		fmt.Fprint(sb, "import (\n")
		for _, v := range cp.goBucket.imports[source] {
			fmt.Fprint(sb, "    \""+v+"\"\n")
		}
		fmt.Fprint(sb, ")\n\n")
	}

	// We extract all the types we're going to need to declare.
	userDefinedTypes := make(dtypes.Set[string])
	for _, function := range cp.goBucket.functions[source] {

		for _, v := range function.NameSig {
			if !cp.isBuiltin(v.VarType) {
				userDefinedTypes.Add(v.VarType)
			}
		}
		for _, v := range function.NameRets {
			if !cp.isBuiltin(v.VarType) {
				userDefinedTypes.Add(v.VarType)
			}
		}
	}
	cp.transitivelyCloseTypes(userDefinedTypes)
	if cp.P.ErrorsExist() {
		return nil
	}

	// We emit the type declarations and converters.
	cp.generateDeclarations(sb, userDefinedTypes)
	// And the functions.
	for _, function := range cp.goBucket.functions[source] {
		cp.generateGoFunctionCode(sb, function)
	}
	// And any blocks of pure Go.
	for _, pureGo := range cp.goBucket.pureGo[source] {
		fmt.Fprint(sb, pureGo)
	}

	counter++ // The number of the gocode_<counter>.go source file we're going to write.
	soFile := filepath.Join(settings.PipefishHomeDirectory, filepath.FromSlash("rsc/go/"+text.Flatten(source)+"_"+strconv.Itoa(int(newTime))+".so"))
	timeMap := cp.getGoTimes()
	if oldTime, ok := timeMap[source]; ok {
		os.Remove(filepath.Join(settings.PipefishHomeDirectory, filepath.FromSlash("rsc/go/"+text.Flatten(source)+"_"+strconv.Itoa(int(oldTime))+".so")))
	}
	goFile := filepath.Join(settings.PipefishHomeDirectory, "gocode_"+strconv.Itoa(counter)+".go")
	file, _ := os.Create(goFile)
	file.WriteString(sb.String())
	file.Close()
	cmd := exec.Command("go", "build", "-buildmode=plugin", "-o", soFile, goFile) // Version to use running from terminal.
	// cmd := exec.Command("go", "build", "-gcflags=all=-N -l", "-buildmode=plugin", "-o", soFile, goFile) // Version to use with debugger.
	output, err := cmd.Output()
	if err != nil {
		cp.P.Throw("golang/build", &token.Token{}, err.Error()+": "+string(output))
		return nil
	}
	plugins, err := plugin.Open(soFile)
	if err != nil {
		cp.P.Throw("golang/open/a", &token.Token{}, err.Error())
		return nil
	}
	if err == nil || strings.Contains(err.Error(), "plugin was built with a different version of package") {
		os.Remove("gocode_" + strconv.Itoa(counter) + ".go")
	}
	timeMap[source] = newTime
	cp.recordGoTimes(timeMap)
	return plugins
}

// This makes sure that if  we're generating declarations for a struct type,
// we're also generating declarations for the types of its fields if need be, and so on recursively. We do
// a traditional non-recursive breadth-first search.
func (cp *Compiler) transitivelyCloseTypes(userDefinedTypes dtypes.Set[string]) {
	structsToCheck := dtypes.Set[string]{}
	for name := range userDefinedTypes {
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
					userDefinedTypes.Add(fieldData.getName(DEFAULT))
				case enumType:
					userDefinedTypes.Add(fieldData.getName(DEFAULT))
				case structType:
					if !userDefinedTypes.Contains(fieldData.name) {
						newStructsToCheck.Add(fieldData.name)
						userDefinedTypes.Add(fieldData.name)
					}
				}
			}
		}
		structsToCheck = newStructsToCheck
	}
}

func (cp *Compiler) recordGoTimes(timeMap map[string]int64) {
	f, err := os.Create(settings.PipefishHomeDirectory + "rsc/go/gotimes.dat")
	if err != nil {
		panic("Can't create file rsc/go/gotimes.dat")
	}
	defer f.Close()
	for k, v := range timeMap {
		f.WriteString(k + "\n")
		f.WriteString(strconv.Itoa(int(v)) + "\n")
	}
}

func (cp *Compiler) getGoTimes() map[string]int64 {
	file, err := os.Open(settings.PipefishHomeDirectory + "rsc/go/gotimes.dat")
	if err != nil {
		panic("Can't open file 'rsc/go/gotimes.dat'.")
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	timeMap := make(map[string]int64)

	for i := 0; i < (len(lines) / 2); i++ {
		time, _ := strconv.Atoi(lines[(2*i)+1])
		timeMap[lines[2*i]] = int64(time)
	}
	return timeMap
}
