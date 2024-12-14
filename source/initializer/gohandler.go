package initializer

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"plugin"
	"reflect"
	"strconv"
	"strings"

	"github.com/tim-hardcastle/Pipefish/source/ast"
	"github.com/tim-hardcastle/Pipefish/source/compiler"
	"github.com/tim-hardcastle/Pipefish/source/dtypes"
	"github.com/tim-hardcastle/Pipefish/source/settings"
	"github.com/tim-hardcastle/Pipefish/source/text"
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

func qux(i int, b bool) int {
	x := 42
	if b {
		x, ok := thing(x)
		if !ok {
			panic("Oops")
		}
		println("x is ", x)
	}
	return x
}

func thing(x int) (int, bool) {
	return 99, true
}

func (iz *initializer) newGoBucket() {
	gb := GoBucket{
		sources:   make(dtypes.Set[string]),
		imports:   make(map[string][]string),
		functions: make(map[string][]*ast.PrsrFunction),
		pureGo:    make(map[string][]string),
	}
	iz.goBucket = &gb
}

// This will if necessary compile or recompile the relevant .so files, and will extract from them
// the functions and converter data needed by the compiler and vm and put it into its proper place.
func (iz *initializer) compileGo() {

	// The purpose of putting timestamps on the .so files is not that we ever read the timestamps
	// from the filenames (we look either at the OS metadata of the file or at the 'gotimes' file),
	// but simply because you can't re-use the names of .so files in the same Go runtime and since
	// we're looking up times anyway this is a reasonable way to achieve that.

	// We get the blocks of pure Go, if any, and put them in the appropriate place in the goBucket.
	for _, golang := range iz.TokenizedDeclarations[golangDeclaration] {
		golang.ToStart()
		token := golang.NextToken()
		iz.goBucket.sources.Add(token.Source)
		iz.goBucket.pureGo[token.Source] = append(iz.goBucket.pureGo[token.Source], token.Literal[:len(token.Literal)-2])
	}

	timeMap := iz.getGoTimes() // We slurp a map from sources to times from the `gotimes` file.

	for source := range iz.goBucket.sources {
		f, err := os.Stat(text.MakeFilepath(source))
		if err != nil {
			iz.Throw("golang/file", INTEROP_TOKEN, source, err.Error())
			break
		}
		var plugins *plugin.Plugin
		sourceCodeModified := f.ModTime().UnixMilli()
		objectCodeModified, ok := timeMap[source]
		if !ok || sourceCodeModified != int64(objectCodeModified) {
			plugins = iz.makeNewSoFile(source, sourceCodeModified)
		} else {
			soFile := settings.PipefishHomeDirectory + "pipefish-rsc/" + text.Flatten(source) + "_" + strconv.Itoa(int(sourceCodeModified)) + ".so"
			plugins, err = plugin.Open(soFile)
			if err != nil {
				iz.Throw("golang/open/b", INTEROP_TOKEN, err.Error())
				return
			}
		}

		// We extract the conversion data from the object code, reformat it, and store the results
		// in the vm.
		newGoConverter := make([](func(t uint32, v any) any), len(iz.cp.Vm.ConcreteTypeInfo))
		copy(newGoConverter, iz.cp.Vm.GoConverter)
		functionConverterSymbol, _ := plugins.Lookup("PIPEFISH_FUNCTION_CONVERTER")
		functionConverter := *functionConverterSymbol.(*map[string](func(t uint32, v any) any))
		for k, v := range BUILTIN_FUNCTION_CONVERTER {
			functionConverter[k] = v
		}
		for typeName, constructor := range functionConverter {
			typeNumber := iz.cp.ConcreteTypeNow(typeName)
			newGoConverter[typeNumber] = constructor
		}
		iz.cp.Vm.GoConverter = newGoConverter
		valueConverterSymbol, _ := plugins.Lookup("PIPEFISH_VALUE_CONVERTER")
		valueConverter := *valueConverterSymbol.(*map[string]any)
		for k, v := range BUILTIN_VALUE_CONVERTER {
			valueConverter[k] = v
		}
		for typeName, goValue := range valueConverter {
			iz.cp.Vm.GoToPipefishTypes[reflect.TypeOf(goValue).Elem()] = iz.cp.ConcreteTypeNow(typeName)
		}
		//We attach the compiled functions to the (pointers to) the functions, which are
		// also pointed to by the compiler's function table and by the list of common functions
		// in the common parser bindle. I.e. we are returning our result by mutating the
		// functions.
		for _, function := range iz.goBucket.functions[source] {
			goFunction, _ := plugins.Lookup(text.Capitalize(function.FName))
			function.Body.(*ast.GolangExpression).GoFunction = reflect.ValueOf(goFunction)
			for i, pair := range function.NameSig {
				if text.Head(pair.VarType, "...") {
					if i < function.NameSig.Len()-1 {
						iz.Throw("go/variadic", function.Tok)
					}
				}
			}
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
func (iz *initializer) makeNewSoFile(source string, newTime int64) *plugin.Plugin {

	var StringBuilder strings.Builder
	sb := &StringBuilder

	// We emit the package declaration and builtins.

	fmt.Fprint(sb, "package main\n\n")
	if len(iz.goBucket.imports) > 0 {
		fmt.Fprint(sb, "import (\n")
		for _, v := range iz.goBucket.imports[source] {
			fmt.Fprint(sb, "    \""+v+"\"\n")
		}
		fmt.Fprint(sb, ")\n\n")
	}

	// We extract all the types we're going to need to declare.
	// TODO --- do we need guards here on the types?
	userDefinedTypes := make(dtypes.Set[string])
	for _, function := range iz.goBucket.functions[source] {
		for _, v := range function.NameSig {
			if !iz.cp.IsBuiltin(text.WithoutDots(v.VarType)) && text.WithoutDots(v.VarType) != "any" && text.WithoutDots(v.VarType) != "any?" {
				userDefinedTypes.Add(text.WithoutDots(v.VarType))
			}
		}
		for _, v := range function.NameRets {
			if !iz.cp.IsBuiltin(v.VarType) {
				userDefinedTypes.Add(v.VarType)
			}
		}
	}
	iz.transitivelyCloseTypes(userDefinedTypes)
	if iz.ErrorsExist() {
		return nil
	}

	// We emit the type declarations and converters.
	iz.generateDeclarations(sb, userDefinedTypes)
	// And the functions.
	for _, function := range iz.goBucket.functions[source] {
		iz.generateGoFunctionCode(sb, function)
	}
	// And any blocks of pure Go.
	for _, pureGo := range iz.goBucket.pureGo[source] {
		fmt.Fprint(sb, pureGo)
	}

	counter++ // The number of the gocode_<counter>.go source file we're going to write.
	soFile := filepath.Join(settings.PipefishHomeDirectory, filepath.FromSlash("pipefish-rsc/"+text.Flatten(source)+"_"+strconv.Itoa(int(newTime))+".so"))
	timeMap := iz.getGoTimes()
	if oldTime, ok := timeMap[source]; ok {
		os.Remove(filepath.Join(settings.PipefishHomeDirectory, filepath.FromSlash("pipefish-rsc/"+text.Flatten(source)+"_"+strconv.Itoa(int(oldTime))+".so")))
	}
	goFile := filepath.Join(settings.PipefishHomeDirectory, "gocode_"+strconv.Itoa(counter)+".go")
	file, _ := os.Create(goFile)
	file.WriteString(sb.String())
	file.Close()
	cmd := exec.Command("go", "build", "-buildmode=plugin", "-o", soFile, goFile) // Version to use running from terminal.
	// cmd := exec.Command("go", "build", "-gcflags=all=-N -l", "-buildmode=plugin", "-o", soFile, goFile) // Version to use with debugger.
	output, err := cmd.Output()
	if err != nil {
		iz.Throw("golang/build", INTEROP_TOKEN, err.Error()+": "+string(output))
		return nil
	}
	plugins, err := plugin.Open(soFile)
	if err != nil {
		iz.Throw("golang/open/a", INTEROP_TOKEN, err.Error())
		return nil
	}
	if err == nil || strings.Contains(err.Error(), "plugin was built with a different version of package") {
		os.Remove("gocode_" + strconv.Itoa(counter) + ".go")
	}
	timeMap[source] = newTime
	iz.recordGoTimes(timeMap)
	return plugins
}

// This makes sure that if  we're generating declarations for a struct type,
// we're also generating declarations for the types of its fields if need be, and so on recursively. We do
// a traditional non-recursive breadth-first search.
func (iz *initializer) transitivelyCloseTypes(userDefinedTypes dtypes.Set[string]) {
	structsToCheck := dtypes.Set[string]{}
	for name := range userDefinedTypes {
		if iz.cp.IsStruct(name) {
			structsToCheck.Add(name)
		}
	}
	for newStructsToCheck := make(dtypes.Set[string]); len(structsToCheck) > 0; {
		for structName := range structsToCheck {
			for _, fieldType := range iz.cp.TypeInfoNow(structName).(compiler.StructType).AbstractStructFields {
				if fieldType.Len() != 1 {
					iz.Throw("golang/type/concrete/a", INTEROP_TOKEN, iz.cp.Vm.DescribeAbstractType(fieldType, compiler.LITERAL))
				}
				typeOfField := iz.cp.GetTypeNameFromNumber(fieldType.Types[0])
				switch fieldData := iz.cp.TypeInfoNow(typeOfField).(type) {
				case compiler.CloneType:
					userDefinedTypes.Add(fieldData.GetName(compiler.DEFAULT))
				case compiler.EnumType:
					userDefinedTypes.Add(fieldData.GetName(compiler.DEFAULT))
				case compiler.StructType:
					if !userDefinedTypes.Contains(fieldData.Name) {
						newStructsToCheck.Add(fieldData.Name)
						userDefinedTypes.Add(fieldData.Name)
					}
				}
			}
		}
		structsToCheck = newStructsToCheck
	}
}

func (iz *initializer) recordGoTimes(timeMap map[string]int64) {
	f, err := os.Create(settings.PipefishHomeDirectory + "pipefish-rsc/gotimes.dat")
	if err != nil {
		panic("Can't create file gotimes.dat")
	}
	defer f.Close()
	for k, v := range timeMap {
		f.WriteString(k + "\n")
		f.WriteString(strconv.Itoa(int(v)) + "\n")
	}
}

func (iz *initializer) getGoTimes() map[string]int64 {
	filepath := settings.PipefishHomeDirectory + "pipefish-rsc/gotimes.dat"
	file, err := os.Open(filepath)
	if err != nil {
		panic("Can't open file '" + filepath + "'.")
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
