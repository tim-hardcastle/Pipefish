package pf

import (
	"crypto/aes"
	"crypto/cipher"
	"crypto/rand"
	"crypto/sha256"
	"errors"
	"fmt"
	"io"
	"os"
	"reflect"
	"strings"

	"golang.org/x/crypto/pbkdf2"

	"github.com/tim-hardcastle/pipefish/source/compiler"
	"github.com/tim-hardcastle/pipefish/source/err"
	"github.com/tim-hardcastle/pipefish/source/filesystem"
	"github.com/tim-hardcastle/pipefish/source/initializer"
	"github.com/tim-hardcastle/pipefish/source/settings"
	"github.com/tim-hardcastle/pipefish/source/text"
	"github.com/tim-hardcastle/pipefish/source/values"
	"github.com/tim-hardcastle/pipefish/source/vm"

	"src.elv.sh/pkg/persistent/vector"
)

type Service struct {
	cp           *compiler.Compiler
	dependencies Dependencies
}

type FileSystem = filesystem.FileSystem

type Dependencies = struct {
	// This is the file system used both by the runtime and by the compiler: by the runtime via
	// the `files` standard library, and by the compiler to locate dependencies.
	FileSystem FileSystem

	// Where `get x from Terminal("prompt")` will get input from.
	InHandler InHandler

	// Where `post x` / `post x to Output()` will post to.
	OutHandler OutHandler

	// Where `post x to Terminal()` will post to.
	// Terminal              ***

	// The contents of `$_env`.
	Environment Map

	// Map of names to services, to be compiled as external services with the names as namespaces.
	ExternalServices map[string]*Service

	// A function which knows how to render markdown in the TUI of the service. This is going to
	// be different according to whether it's running in a Linux terminal and we're using the
	// terminal control codes; or running in the browser and using HTML: or other cases not yet
	// encountered.
	MarkdownRenderer func(string) string
}

func (sv *Service) Inject(d Dependencies) *Service {
	sv.dependencies = d
	if sv.cp != nil {
		sv.cp.Common.Dependencies = convertDependencies(sv.dependencies)
		sv.cp.InjectDependencies()
	}
	return sv
}

func (sv *Service) Update(d Dependencies) *Service {
	result := sv.dependencies
	if d.Environment.Len() != 0 {
		result.Environment = d.Environment
	}
	if len(d.ExternalServices) != 0 {
		result.ExternalServices = d.ExternalServices
	}
	if d.FileSystem != nil {
		result.FileSystem = d.FileSystem
	}
	if d.InHandler != nil {
		result.InHandler = d.InHandler
	}
	if d.MarkdownRenderer != nil {
		result.MarkdownRenderer = d.MarkdownRenderer
	}
	if d.OutHandler != nil {
		result.OutHandler = d.OutHandler
	}
	sv.dependencies = result
	if sv.cp != nil {
		sv.cp.Common.Dependencies = convertDependencies(sv.dependencies)
		sv.cp.InjectDependencies()
	}
	return sv
}

func convertDependencies(d Dependencies) *compiler.Dependencies {
	compilerMap := map[string]*compiler.Compiler{}
	for k, v := range d.ExternalServices {
		compilerMap[k] = v.cp
	}
	return &compiler.Dependencies{
		FileSystem:       d.FileSystem,
		InHandler:        d.InHandler,
		OutHandler:       d.OutHandler,
		Environment:      d.Environment,
		ExternalServices: compilerMap,
		MarkdownRenderer: d.MarkdownRenderer,
	}
}

// Returns a new service.
func NewService() *Service {
	return &Service{
		cp:           nil,
		dependencies: getDefaultDependencies(),
	}
}

func getDefaultDependencies() Dependencies {
	d := initializer.GetDefaultDependencies()
	return Dependencies{
		FileSystem:       d.FileSystem,
		InHandler:        d.InHandler,
		OutHandler:       d.OutHandler,
		Environment:      d.Environment,
		ExternalServices: map[string]*Service{},
		MarkdownRenderer: d.MarkdownRenderer,
	}
}

// Initializes the service with the source code supplied in the file indicated by the filepath.
func (sv *Service) InitializeFromFilepath(scriptFilepath string) error {
	sourcecode, e := initializer.GetSourceCode(sv.dependencies.FileSystem, scriptFilepath)
	if e != nil {
		return e
	}
	return sv.initialize(scriptFilepath, sourcecode)
}

// Initializes the service with the source code supplied in the string.
func (sv *Service) InitializeFromCode(code string) error {
	path, _ := sv.GetFilepath()
	return sv.initialize(path, code)
}

// Initializes the service on behalf of both the previous methods. As the
// compiler can't see the service class, the other services visible to a
// service have to be supplied as raw compilers. We pass them in, and then we
// yoink them out at the end because the service might have started up a new
// external service.
func (sv *Service) initialize(scriptFilepath, sourcecode string) error {
	sv.cp = initializer.StartCompiler(scriptFilepath, sourcecode, convertDependencies(sv.dependencies))
	if sv.IsBroken() {
		return errors.New("compilation error")
	}
	return nil
}

// A struct representing a Pipefish value, consisting of fields `T“, a `Type`, and
// V, the payload, of type `any`.
type Value = values.Value

// The type of a Pipefish value.

type Type = values.ValueType

// An interface with one method, `Get()`, which supplies a string when the Pipefish
// code does `get x from Input()`.
type InHandler = vm.InHandler

// An interface with two methods (1) `Out(v Value)` which takes the value supplied when
// the Pipefish code says `post x to Output()` and does something with it, presumably
// serializing it in some way and writing it somewhere; (2) a method `Write(s string)`
// which if necessary allows the user to write a string to the same place.
type OutHandler = vm.OutHandler

// An InHandler which just gets an input from an io.Reader supplied at its construction.
type SimpleInHandler = vm.SimpleInHandler

// An OutHandler which stringifies/serializes the given value and writes it to an io.Writer supplied
// at its construction.
type SimpleOutHandler = vm.SimpleOutHandler

// An OutHandler which serializes the given value and writes it to an io.Writer supplied
// at its construction.
type LiteralOutHandler = vm.LiteralOutHandler

// An InHandler which supplies a prompt and then gets its input from the terminal.
type TerminalInHandler = vm.StandardInHandler

// The representation of a Pipefish list in the `V` field of a `Value` with `T` = `LIST`.
type List = vector.Vector

// The representation of a Pipefish map in the `V` field of a `Value` with `T` = `MAP`.
type Map = values.Map

// The representation of a Pipefish set in the `V` field of a `Value` with `T` = `SET`.
type Set = values.Set

// The representation of a Pipefish set in the `V` field of a `Value` with `T` = `ERROR`.
type Error = err.Error

// Constants representing Pipefish types.
const (
	UNDEFINED_TYPE          Type = values.UNDEFINED_TYPE
	BLING                   Type = values.BLING
	UNSATISFIED_CONDITIONAL Type = values.UNSATISFIED_CONDITIONAL
	OK                      Type = values.SUCCESSFUL_VALUE
	TUPLE                   Type = values.TUPLE
	ERROR                   Type = values.ERROR
	NULL                    Type = values.NULL
	INT                     Type = values.INT
	BOOL                    Type = values.BOOL
	STRING                  Type = values.STRING
	RUNE                    Type = values.RUNE
	FLOAT                   Type = values.FLOAT
	TYPE                    Type = values.TYPE
	FUNC                    Type = values.FUNC
	PAIR                    Type = values.PAIR
	LIST                    Type = values.LIST
	MAP                     Type = values.MAP
	SET                     Type = values.SET
	LABEL                   Type = values.LABEL
	SNIPPET                 Type = values.SNIPPET
)

// Makes an InHandler which does nothing but get a string from terminal
// input and return it.
func MakeSimpleInHandler(in io.Reader) *SimpleInHandler {
	return vm.MakeSimpleInHandler(in)
}

// Method makes an `OutHandler` which applies Pipefish's `literal` or `string`
// function to the value and then writes the result to the supplied `io.Writer`.
func (sv *Service) MakeWritingOutHandler(out io.Writer) *SimpleOutHandler {
	return vm.MakeSimpleOutHandler(out, sv.cp.Vm)
}

// An outhandler which always serializes the value, for use by external services.
func (sv *Service) MakeLiteralOutHandler(out io.Writer) *LiteralOutHandler {
	return vm.MakeLiteralOutHandler(out, sv.cp.Vm)
}

// Method makes an `OutHandler` which applies Pipefish's `literal` or `string`
// function to the value and then writes the result to Stdout.
func (sv *Service) MakeTerminalOutHandler() *SimpleOutHandler {
	return vm.MakeSimpleOutHandler(os.Stdout, sv.cp.Vm)
}

// Makes a `CapturingOutHandler` which will store the output until requested with its "dump"
// method.
func (sv *Service) MakeCapturingOutHandler() *vm.CapturingOutHandler {
	return vm.MakeCapturingOutHandler(sv.cp.Vm)
}

// Makes an `InHandler` which will get input from the terminal using the string supplied
// to prompt the end user.
func MakeTerminalInHandler(prompt string, cancel chan os.Signal) *TerminalInHandler {
	return vm.MakeStandardInHandler(prompt, cancel)
}

// Outputs a value via the outhandler.
func (sv *Service) Output(v Value) {
	sv.cp.Vm.World.OutHandle.Out(v)
}

func (sv *Service) Dump() (string, error) {
	if sv.cp == nil {
		return "", errors.New("service is uninitialized")
	}
	if sv.IsBroken() {
		return "", errors.New("service is broken")
	}
	if oH, ok := sv.cp.Vm.World.OutHandle.(*vm.CapturingOutHandler); ok {
		return oH.Dump(), nil
	} else {
		return "", errors.New("wrong OutHandler")
	}
}

// Once the service is initialized, will interpret the string supplied as though
// it had been entered into the REPL of the service. The error field will be non-nil
// in the case of a compile-time error. In the case of a runtime error, it will be
// nil, and the error will be returned as the `Value`.
func (sv *Service) Do(line string) (Value, error) {
	if sv.cp == nil {
		return Value{}, errors.New("service is uninitialized")
	}
	if sv.IsBroken() {
		errs, _ := sv.GetErrorReport()
		return Value{}, errors.New("Do: service is broken" + "\n\n" + errs)
	}
	sv.cp.P.ResetAfterError()
	sv.cp.Vm.LiveTracking = make([]vm.TrackingData, 0)
	state := sv.cp.GetState()
	cT := sv.cp.CodeTop()
	node := sv.cp.P.ParseLine("REPL input", line)
	if settings.SHOW_PARSER {
		if node == nil {
			println("Parsing failed, node is nil.")
		} else {
			fmt.Println("Parsed line:", node.String())
		}
	}
	if sv.cp.P.ErrorsExist() {
		return Value{}, errors.New("error parsing input")
	}
	ctxt := compiler.Context{Env: sv.cp.GlobalVars, Access: compiler.REPL, LowMem: compiler.DUMMY, TrackingFlavor: compiler.LF_NONE}
	sv.cp.CompileNode(node, ctxt)
	if sv.cp.P.ErrorsExist() {
		return Value{}, errors.New("error compiling input")
	}
	sv.cp.Emit(vm.Ret)
	sv.cp.Cm("Calling RunRoot from Do.", node.GetToken())
	sv.cp.Vm.PostHappened = false
	sv.cp.Vm.Run(cT)
	result := sv.cp.Vm.Mem[sv.cp.That()]
	sv.cp.Rollback(state, node.GetToken())
	return result, nil
}

// Gets the value of a global variable given its name. Unlike using `Do` for the
// same purpose, this can get the value of private variables.
func (sv *Service) GetVariable(vname string) (values.Value, error) {
	if sv.cp == nil {
		return Value{}, errors.New("service is uninitialized")
	}
	if sv.IsBroken() {
		return Value{}, errors.New("GetVariable: service is broken")
	}
	v, ok := sv.cp.GlobalVars.GetVar(vname)
	if !ok {
		return Value{}, errors.New("variable does not exist")
	}
	return sv.cp.Vm.Mem[v.MLoc], nil
}

func (sv *Service) DumpCode(functionName string, showMemory bool) string {
	return sv.cp.DumpFunction(functionName, showMemory)
}

// Sets the value of a global variable given its name. Unlike using `Do` for the
// same purpose, this can set the value of private variables.
func (sv *Service) SetVariable(vname string, ty values.ValueType, v any) error {
	if sv.cp == nil {
		return errors.New("service is uninitialized")
	}
	if sv.IsBroken() {
		return errors.New("SetVariable: service is broken")
	}
	_, ok := sv.cp.GlobalVars.GetVar(vname)
	if !ok {
		return errors.New("variable does not exist")
	}
	sv.cp.Vm.Mem[sv.cp.GlobalVars.Data[vname].MLoc] = values.Value{ty, v}
	return nil
}

// Calls the `main` function.
func (s *Service) CallMain() (Value, error) {
	return s.cp.CallIfExists("main")
}

// Checks whether the source code for a service has been changed since it was
// initialized.
func (sv *Service) NeedsUpdate() (bool, error) {
	if sv.cp == nil {
		return false, errors.New("service is uninitialized")
	}
	return needsUpdate(sv.cp)
}

func needsUpdate(cp *compiler.Compiler) (bool, error) {
	if len(cp.ScriptFilepath) >= 5 && cp.ScriptFilepath[0:5] == "http:" || len(cp.ScriptFilepath) >= 11 && cp.ScriptFilepath[0:11] == "test-files/" {
		return false, nil
	}
	for fname, timestamp := range cp.Sources {
		file, _ := os.Stat(fname)
		if file != nil { // Exempts things like the builtins.
			currentTimeStamp := file.ModTime().UnixMilli()
			if timestamp != currentTimeStamp {
				return true, nil
			}
		}
	}
	for _, importedCp := range cp.Modules {
		impNeedsUpdate, impError := needsUpdate(importedCp)
		if impNeedsUpdate || impError != nil {
			return impNeedsUpdate, impError
		}
	}
	return false, nil
}

// Returns `true` if the last thing the service did produced errors, whether runtime
// or compile time.
func (sv *Service) ErrorsExist() (bool, error) {
	if sv == nil || sv.cp == nil {
		return false, errors.New("service is uninitialized")
	}
	return sv.cp.P.ErrorsExist(), nil
}

// Gets the source code of the service as a map from filenames to lists of strings (i.e.
// lines of source code).
func (sv *Service) GetSources() (map[string][]string, error) {
	if sv.cp == nil {
		return nil, errors.New("service is uninitialized")
	}
	return sv.cp.P.Common.Sources, nil
}

// Gets a summary of the errors produced by the last thing the service did, in the form of a
// string that can be passed to the `PrettyString` function for highlighting.
func (sv *Service) GetErrorReport() (string, error) {
	if sv.cp == nil {
		return "", errors.New("service is uninitialized")
	}
	return sv.cp.P.ReturnErrors(), nil
}

// Gets the trace report of a runtime error, in the form of a string that can be passed
// to the `PrettyString` function for highlighting.
func GetTraceReport(e *err.Error) string {
	result := text.RT_ERROR + e.Message + "\n"
	for i := len(e.Trace) - 1; i >= 0; i-- {
		result = result + "- From: " + err.DescribeTok(e.Trace[i]) + err.DescribePos(e.Trace[i]) + ".\n"
	}
	return result + "\n"
}

// Provides the answer to `hub why <n>`.
func ExplainError(es []*Error, i int) (string, error) { // TODO --- at this point the explanation will be in the error when it's created.
	if i >= len(es) {
		return "", errors.New("index too big for list")
	}
	if es[i].Explanation != "" {
		return es[i].Explanation, nil
	}
	ec, ok := err.GetErrorCreator(es[i].ErrorId)
	if !ok {
		return "no explanation has been supplied for this error", nil
	}
	return (ec.Explanation(es[i].Token, es[i].Args...)), nil
}

// GetFilepath to the root file of the service.
func (sv *Service) GetFilepath() (string, error) {
	if sv.cp == nil {
		return "", errors.New("service is uninitialized")
	}
	return sv.cp.ScriptFilepath, nil
}

// Returns `true` if the service is uninitialized or failed to compile.
func (sv *Service) IsBroken() bool {
	return sv.cp == nil || sv.cp.P.Common.IsBroken
}

// Returns `true` if the service is initialized.
func (sv *Service) IsInitialized() bool {
	return sv.cp != nil
}

// Returns the errors produced by the last thing the service did, as a list of things
// of type `*Error`.
func (sv *Service) GetErrors() []*Error {
	if sv.cp == nil {
		return []*Error{}
	}
	return sv.cp.P.Common.Errors
}

// Converts a `Value` to a string using Pipefish's `literal` function.
func (sv *Service) ToLiteral(v Value) string {
	return sv.cp.Vm.Literal(v, 0)
}

// Converts a `Value` to a string using Pipefish's `string` function.
func (sv *Service) ToString(v Value) string {
	return sv.cp.Vm.DefaultDescription(v)
}

// Returns `true` if the `Value` is a clone.
func (sv *Service) IsClone(v Value) bool {
	return sv.cp.Vm.ConcreteTypeInfo[v.T].IsClone()
}

// Returns `true` if the `Value` is an enum.
func (sv *Service) IsEnum(v Value) bool {
	return sv.cp.Vm.ConcreteTypeInfo[v.T].IsEnum()
}

// Returns `true` if the `Value` is a struct.
func (sv *Service) IsStruct(v Value) bool {
	return sv.cp.Vm.ConcreteTypeInfo[v.T].IsStruct()
}

// Returns the underlying `Type` of a `Value`, i.e. its parent type if it's a clone, otherwise its own type.
func (sv *Service) UnderlyingType(v Value) Type {
	if clone, ok := sv.cp.Vm.ConcreteTypeInfo[v.T].(vm.CloneType); ok {
		return clone.Parent
	}
	return v.T
}

// Returns the `Type` associated with a given type name.
func (sv *Service) TypeNameToType(s string) (Type, error) {
	if sv.cp == nil {
		return values.UNDEFINED_TYPE, errors.New("service is uninitialized")
	}
	if sv.IsBroken() {
		return values.UNDEFINED_TYPE, errors.New("service is broken")
	}
	t, ok := sv.cp.GetConcreteType(s)
	if !ok {
		return values.UNDEFINED_TYPE, errors.New("no concrete type of that name exists")
	}
	return t, nil
}

// Returns the type name associated with a given `Type`.
func (sv *Service) TypeToTypeName(t Type) (string, error) {
	if sv.cp == nil {
		return "", errors.New("service is uninitialized")
	}
	if sv.IsBroken() {
		return "", errors.New("TypeToTypename: service is broken")
	}
	if int(t) >= len(sv.cp.Vm.ConcreteTypeInfo) {
		return "", errors.New("type does not exist")
	}
	s := sv.cp.Vm.DescribeType(t, vm.DEFAULT, 0)
	return s, nil
}

// Gets a report of the tracking if any, in the form of a string which can be passed to
// `PrettyString` for highlighting.
func (sv *Service) GetTrackingReport() (string, error) {
	if sv.cp == nil {
		return "", errors.New("service is uninitialized")
	}
	if sv.IsBroken() {
		return "", errors.New("GetTrackingReport: service is broken")
	}
	return sv.cp.Vm.TrackingToString(sv.cp.Vm.LiveTracking), nil
}

// Says whether post happened when we called Do.
func (sv *Service) PostHappened() bool {
	return sv.cp.Vm.PostHappened
}

func (sv *Service) SetPostHappened() {
	sv.cp.Vm.PostHappened = true
}

// Gets markdown with appropriate highlighting.
func (sv *Service) Render(s string) string {
	return sv.cp.Common.Dependencies.MarkdownRenderer(s)
}

// Wraps ToGoWithType for better ergonomics.
func ToGo[T any](sv *Service, pfValue Value) (T, error) {
	goVal, e := sv.ToGoWithType(pfValue, reflect.TypeFor[T]())
	if e != nil {
		var zero T
		return zero, e
	}
	return goVal.(T), nil
}

// Tries to turn a given Pipefish value into a given Go type. As it necessarily has return
// type `any` (plus an error if the coercion is impossible) the value returned will then
// still need downcasting to the type it was coerced to.
//
// E.g:
//
//	func TwoPlusTwo() int {
//	    v, _ := fooService.Do(`2 + 2`)                   // `v` has type `Value`.
//	    i := fooService.ToGoWithType(v, reflect.TypeFor[int]())  // `i` has type `any`.
//	    return i.(int)                                   // We return an integer as required.
//	}
//
// The error will be non-nil if the coercion is impossible.
func (sv *Service) ToGoWithType(pfValue Value, goType reflect.Type) (any, error) {
	pfTypeInfo := sv.cp.Vm.ConcreteTypeInfo[pfValue.T]
	pfTypeName := pfTypeInfo.GetName(vm.LITERAL)
	goTypeName := goType.String()
	myError := errors.New("cannot coerce Pipefish value of type '" + pfTypeName +
		"' to Go value of type '" + goTypeName + "'")
	if goType.Kind() == reflect.Pointer {
		goDatum, e := sv.ToGoWithType(pfValue, goType.Elem())
		if e != nil {
			return nil, e
		}
		ptr := reflect.New(goType.Elem())
		ptr.Elem().Set(reflect.ValueOf(goDatum))
		return ptr.Interface(), nil
	}
	if goType == reflect.TypeFor[any]() {
		var ok bool
		goType, ok = DEFAULT_TYPE_FOR[pfValue.T]
		if !ok {
			return nil, myError
		}
	}
	if structInfo, ok := pfTypeInfo.(vm.StructType); ok {
		if goType.Kind() != reflect.Struct || structInfo.Len() != goType.NumField() {
			return nil, myError
		}
		goStruct := reflect.New(goType).Elem()
		for i, pfFieldValue := range pfValue.V.([]Value) {
			goFieldDatum, e := sv.ToGoWithType(pfFieldValue, goType.FieldByIndex([]int{i}).Type)
			if e != nil {
				return nil, e
			}
			goStruct.Field(i).Set(reflect.ValueOf(goFieldDatum))
		}
		return goStruct.Interface(), nil
	}
	if _, ok := pfTypeInfo.(vm.WrapperType); ok && goType == reflect.TypeOf(pfValue.V) {
		return pfValue.V, nil
	}
	pfBaseType := UNDEFINED_TYPE
	if _, ok := pfTypeInfo.(vm.BuiltinType); ok {
		pfBaseType = pfValue.T
	}
	if cloneType, ok := pfTypeInfo.(vm.CloneType); ok {
		pfBaseType = cloneType.Parent
	}
	if _, ok := pfTypeInfo.(vm.EnumType); ok {
		pfBaseType = INT
	}
	var goDatum any
	switch pfBaseType {
	case UNDEFINED_TYPE: // Then we didn't find anything we could do with it.
		return nil, myError
	case INT:
		switch goType.Kind() {
		case reflect.Int:
			goDatum = pfValue.V.(int)
		case reflect.Int8:
			goDatum = int8(pfValue.V.(int))
		case reflect.Int16:
			goDatum = int16(pfValue.V.(int))
		case reflect.Int32:
			goDatum = int32(pfValue.V.(int))
		case reflect.Int64:
			goDatum = int64(pfValue.V.(int))
		case reflect.Uint:
			goDatum = uint(pfValue.V.(int))
		case reflect.Uint8:
			goDatum = uint8(pfValue.V.(int))
		case reflect.Uint16:
			goDatum = uint16(pfValue.V.(int))
		case reflect.Uint32:
			goDatum = uint32(pfValue.V.(int))
		case reflect.Uint64:
			goDatum = uint64(pfValue.V.(int))
		default:
			return nil, myError
		}
	case BOOL:
		if goType.Kind() != reflect.Bool {
			return nil, myError
		}
		goDatum = pfValue.V.(bool)
	case STRING:
		if goType.Kind() != reflect.String {
			return nil, myError
		}
		goDatum = pfValue.V.(string)
	case RUNE:
		if goType.Kind() != reflect.Int32 {
			return nil, myError
		}
		goDatum = pfValue.V.(rune)
	case FLOAT:
		switch goType.Kind() {
		case reflect.Float32:
			goDatum = float32(pfValue.V.(float64))
		case reflect.Float64:
			goDatum = pfValue.V.(float64)
		default:
			return nil, myError
		}
	case TUPLE, PAIR, LIST:
		var pfValues []Value
		if pfValue.T == LIST {
			vec := pfValue.V.(List)
			for i := 0; i < vec.Len(); i++ {
				pfElement, _ := vec.Index(i)
				pfValues = append(pfValues, pfElement.(Value))
			}
		} else {
			pfValues = pfValue.V.([]Value)
		}
		switch goType.Kind() {
		case reflect.Array:
			if goType.Len() != len(pfValues) {
				return nil, myError
			}
			goArray := reflect.New(goType).Elem()
			for i, pfElement := range pfValues {
				goFieldDatum, e := sv.ToGoWithType(pfElement, goType.Elem())
				if e != nil {
					return nil, e
				}
				goArray.Index(i).Set(reflect.ValueOf(goFieldDatum))
			}
			goDatum = goArray.Interface()
		case reflect.Slice:
			goSlice := reflect.MakeSlice(goType, len(pfValues), len(pfValues))
			for i, pfElement := range pfValues {
				goElement, e := sv.ToGoWithType(pfElement, goType.Elem())
				if e != nil {
					return nil, e
				}
				goSlice.Index(i).Set(reflect.ValueOf(goElement))
			}
			goDatum = goSlice.Interface()
		}
	case MAP:
		if goType.Kind() != reflect.Map {
			return nil, myError
		}
		pfMap := pfValue.V.(Map)
		goMap := reflect.MakeMap(goType)
		for _, pfKeyValuePair := range pfMap.AsSlice() {
			goKeyDatum, keyError := sv.ToGoWithType(pfKeyValuePair.Key, goType.Key())
			if keyError != nil {
				return nil, keyError
			}
			goValDatum, valError := sv.ToGoWithType(pfKeyValuePair.Val, goType.Elem())
			if valError != nil {
				return nil, valError
			}
			goMap.SetMapIndex(reflect.ValueOf(goKeyDatum), reflect.ValueOf(goValDatum))
		}
		goDatum = goMap.Interface()
	case SET:
		if goType.Kind() != reflect.Map || goType.Elem() != reflect.TypeFor[struct{}]() {
			return nil, myError
		}
		pfSet := pfValue.V.(Set)
		goSet := reflect.MakeMap(goType)
		for _, pfElement := range pfSet.AsSlice() {
			goElementDatum, elementError := sv.ToGoWithType(pfElement, goType.Key())
			if elementError != nil {
				return nil, elementError
			}
			goSet.SetMapIndex(reflect.ValueOf(goElementDatum), reflect.ValueOf(struct{}{}))
		}
		goDatum = goSet.Interface()
	default:
		return nil, myError
	}
	// This takes care of the assigned types.
	return reflect.ValueOf(goDatum).Convert(goType).Interface(), nil
}

// What we convert to when we convert to `any`.
var DEFAULT_TYPE_FOR = map[Type]reflect.Type{
	INT:    reflect.TypeFor[int](),
	BOOL:   reflect.TypeFor[bool](),
	STRING: reflect.TypeFor[string](),
	FLOAT:  reflect.TypeFor[float64](),
	LIST:   reflect.TypeFor[[]any](),
	PAIR:   reflect.TypeFor[[2]any](),
	MAP:    reflect.TypeFor[map[any]any](),
	SET:    reflect.TypeFor[map[any]struct{}](),
	TUPLE:  reflect.TypeFor[[]any](),
}

// Serializes a Map of Values into newline-separated key-value pairs, encrypting if the
// password is non-empty, and heading the result with "PLAINTEXT\n" if the password is empty.
func (sv *Service) WriteSecret(store values.Map, password string) string {
	var plaintext strings.Builder
	plaintext.WriteString("PLAINTEXT\n")
	for _, pair := range store.AsSlice() {
		plaintext.WriteString(sv.cp.Vm.ToString(pair.Key, vm.LITERAL, 0))
		plaintext.WriteString("::")
		plaintext.WriteString(sv.cp.Vm.ToString(pair.Val, vm.LITERAL, 0))
		plaintext.WriteString("\n")
	}
	if password == "" {
		return plaintext.String()
	}
	plaintext.WriteString(strings.Repeat(" ", aes.BlockSize-plaintext.Len()%aes.BlockSize))
	salt := make([]byte, 32)
	rand.Read(salt)
	key := pbkdf2.Key([]byte(password), salt, 65536, 32, sha256.New) // sha256 has nothing to do with it but the API is stupid.
	block, err := aes.NewCipher(key)
	if err != nil {
		panic(err)
	}
	// We also use salt for the AES cypher (the salt being called `iv` below because the people
	// I copied the code from have a sense of humor).
	ciphertext := make([]byte, aes.BlockSize+plaintext.Len())
	iv := ciphertext[:aes.BlockSize]
	rand.Read(iv)
	mode := cipher.NewCBCEncrypter(block, iv)
	mode.CryptBlocks(ciphertext[aes.BlockSize:], []byte(plaintext.String()))
	return string(salt) + string(ciphertext)
}

// This highlights the given string on the assumption that it's Pipefish
// code, and that the `fonts` map is a theme like in `user/themes.pf“.
func (sv *Service) Highlight(code []rune, fonts Map) string {
	return sv.cp.Highlight(code, fonts)
}

// Returns a description of the API in rendered text.
// The `fonts` field is only used for rendering Pipefish code in code blocks, and
// so may be omitted if you don't want to do that.
func (sv *Service) Api(title string, path []string, fonts values.Map, width int) string {
	return sv.cp.Api(title, path, fonts, width)
}

func (sv *Service) Wiki(path []string) string {
	return sv.cp.Wiki(path)
}

// Describes the API of a service in a custom reverse polish notation format.
//
// TODO, WARNING! This is extremely unstable, it contains things it doesn't need to contain
// and omits things it should contain.
func (sv *Service) SerializeApi() string {
	return sv.cp.API
}
