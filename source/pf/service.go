package pf

import (
	"database/sql"
	"errors"
	"fmt"
	"io"
	"os"
	"reflect"

	"github.com/tim-hardcastle/Pipefish/source/compiler"
	"github.com/tim-hardcastle/Pipefish/source/err"
	"github.com/tim-hardcastle/Pipefish/source/initializer"
	"github.com/tim-hardcastle/Pipefish/source/settings"
	"github.com/tim-hardcastle/Pipefish/source/text"
	"github.com/tim-hardcastle/Pipefish/source/values"
	"github.com/tim-hardcastle/Pipefish/source/vm"

	"src.elv.sh/pkg/persistent/vector"
)

type Service struct {
	cp             *compiler.Compiler
	localExternals map[string]*Service
	db             *sql.DB
}

// Returns a new service.
func NewService() *Service {
	return &Service{cp: nil,
		localExternals: make(map[string]*Service),
		db:             nil,
	}
}

// Initializes the service with the source code supplied in the string.
func (sv *Service) InitializeFromCode(code string) error {
	return sv.initialize("InitializeFromCode", code, &values.Map{})
}

// Initializes the service with the source code supplied in the file indicated by the filepath.
func (sv *Service) InitializeFromFilepath(scriptFilepath string) error {
	sourcecode, e := compiler.GetSourceCode(scriptFilepath)
	if e != nil {
		return e
	}
	return sv.initialize(scriptFilepath, sourcecode, &values.Map{})
}

//The same as the previous two functions, except that we pass in a map of values to initialize
// $store.
// Initializes the service with the source code supplied in the string.
func (sv *Service) InitializeFromCodeWithStore(code string, store Map) error {
	return sv.initialize("InitializeFromCode", code, store)
}

// Initializes the service with the source code supplied in the file indicated by the filepath.
func (sv *Service) InitializeFromFilepathWithStore(scriptFilepath string, store Map) error {
	sourcecode, e := compiler.GetSourceCode(scriptFilepath)
	if e != nil {
		return e
	}
	return sv.initialize(scriptFilepath, sourcecode, store)
}

// Initializes the service on behalf of both the previous methods. As the
// compiler can't see the service class, the other services visible to a
// service have to be supplied as raw compilers. We pass them in, and then we
// yoink them out at the end because the service might have started up a new
// external service.
func (sv *Service) initialize(scriptFilepath, sourcecode string, store Map) error {
	compilerMap := make(map[string]*compiler.Compiler)
	for k, v := range sv.localExternals {
		compilerMap[k] = v.cp
	}
	cp := initializer.StartCompiler(scriptFilepath, sourcecode, compilerMap, store)
	sv.cp = cp
	for k, v := range compilerMap {
		sv.localExternals[k] = &Service{v, sv.localExternals, sv.db}
	}
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

// An OutHandler which serializes the given value and writes it to an io.Writer supplied
// at its construction.
type SimpleOutHandler = vm.SimpleOutHandler

// An InHandler which supplies a prompt and then gets its input from the terminal.
type TerminalInHandler = vm.StandardInHandler

// The representation of a Pipefish list in the `V` field of a `Value` with `T` = `LIST`.
type List = vector.Vector

// The representation of a Pipefish map in the `V` field of a `Value` with `T` = `MAP`.
type Map = *values.Map

// The representation of a Pipefish set in the `V` field of a `Value` with `T` = `SET`.
type Set = values.Set

// The representation of a Pipefish set in the `V` field of a `Value` with `T` = `ERROR`.
type Error = err.Error

// Constants representing Pipefish types.
const (
	UNDEFINED_TYPE Type = values.UNDEFINED_TYPE
	BLING          Type = values.BLING
	OK             Type = values.SUCCESSFUL_VALUE
	TUPLE          Type = values.TUPLE
	ERROR          Type = values.ERROR
	NULL           Type = values.NULL
	INT            Type = values.INT
	BOOL           Type = values.BOOL
	STRING         Type = values.STRING
	RUNE           Type = values.RUNE
	FLOAT          Type = values.FLOAT
	TYPE           Type = values.TYPE
	FUNC           Type = values.FUNC
	PAIR           Type = values.PAIR
	LIST           Type = values.LIST
	MAP            Type = values.MAP
	SET            Type = values.SET
	LABEL          Type = values.LABEL
	SECRET         Type = values.SECRET
	SNIPPET        Type = values.SNIPPET
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

// Method makes an `OutHandler` which applies Pipefish's `literal` or `string`
// function to the value and then writes the result to the supplied `io.Writer`.
func (sv *Service) MakeTerminalOutHandler(out io.Writer) *SimpleOutHandler {
	return vm.MakeSimpleOutHandler(os.Stdout, sv.cp.Vm)
}

// Makes an `InHandler` which will get input from the terminal using the string supplied
// to prompt the end user.
func MakeTerminalInHandler(prompt string) *TerminalInHandler {
	return vm.MakeStandardInHandler(prompt)
}

// Outputs a value via the outhandler.
func (sv *Service) Output(v Value) {
	sv.cp.Vm.OutHandle.Out(v)
}

// Makes other services visible to the service, as though they were running
// on the same hub: their `external` declarations can then allow them to use one
// another as external services.
func (sv *Service) SetLocalExternalServices(svs map[string]*Service) {
	sv.localExternals = svs
}

// Sets an InHandler, i.e. the thing that decides what happens when you do
// `get x from Input()`.
func (sv *Service) SetInHandler(in InHandler) error {
	if sv.cp == nil {
		return errors.New("service is uninitialized")
	}
	if sv.IsBroken() {
		return errors.New("service is broken")
	}
	sv.cp.Vm.InHandle = in
	return nil
}

// Sets an OutHandler, i.e. the thing that decides what happens when you do
// `post x to Output()`.
func (sv *Service) SetOutHandler(out vm.OutHandler) error {
	if sv.cp == nil {
		return errors.New("service is uninitialized")
	}
	if sv.IsBroken() {
		return errors.New("service is broken")
	}
	sv.cp.Vm.OutHandle = out
	return nil
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
		return Value{}, errors.New("service is broken")
	}
	sv.cp.P.ResetAfterError()
	sv.cp.Vm.LiveTracking = make([]vm.TrackingData, 0)
	state := sv.cp.GetState()
	cT := sv.cp.CodeTop()
	node := sv.cp.P.ParseLine("REPL input", line)
	if settings.SHOW_PARSER {
		fmt.Println("Parsed line:", node.String())
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
	sv.cp.Cm("Calling Run from Do.", node.GetToken())
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
		return Value{}, errors.New("service is broken")
	}
	v, ok := sv.cp.GlobalVars.GetVar(vname)
	if !ok {
		return Value{}, errors.New("Variable does not exist")
	}
	return sv.cp.Vm.Mem[v.MLoc], nil
}

// Sets the value of a global variable given its name. Unlike using `Do` for the
// same purpose, this can get the value of private variables.
func (sv *Service) SetVariable(vname string, ty values.ValueType, v any) error {
	if sv.cp == nil {
		return errors.New("service is uninitialized")
	}
	if sv.IsBroken() {
		return errors.New("service is broken")
	}
	_, ok := sv.cp.GlobalVars.GetVar(vname)
	if !ok {
		return errors.New("Variable does not exist")
	}
	sv.cp.Vm.Mem[sv.cp.GlobalVars.Data[vname].MLoc] = values.Value{ty, v}
	return nil
}

func (sv *Service) Store(k, v Value) error {
	if sv.cp == nil {
		return errors.New("service is uninitialized")
	}
	if sv.IsBroken() {
		return errors.New("service is broken")
	}
	sv.cp.Store(k, v)
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
	return sv.cp.NeedsUpdate()
}

// Returns `true` if the last thing the service did produced errors, whether runtime
// or compile time.
func (sv *Service) ErrorsExist() (bool, error) {
	if sv.cp == nil {
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
	result := text.RT_ERROR + e.Message + "\n\n"
	for i := len(e.Trace) - 1; i >= 0; i-- {
		result = result + "  From: " + text.DescribeTok(e.Trace[i]) + text.DescribePos(e.Trace[i]) + "."
	}
	return result + "\n"
}

// Provie the answer to `hub why <n>`.
func ExplainError(es []*Error, i int) (string, error) {
	if i >= len(es) {
		return "", errors.New("index too big for list")
	}
	return (err.ErrorCreatorMap[es[i].ErrorId].Explanation(es, i, es[i].Token, es[i].Args...)), nil
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
	return sv.cp.Vm.Literal(v)
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
		return "", errors.New("service is broken")
	}
	if int(t) >= len(sv.cp.Vm.ConcreteTypeInfo) {
		return "", errors.New("type does not exist")
	}
	s := sv.cp.Vm.DescribeType(t, vm.DEFAULT)
	return s, nil
}

// Gets a report of the tracking if any, in the form of a string which can be passed to
// `PrettyString` for highlighting.
func (sv *Service) GetTrackingReport() (string, error) {
	if sv.cp == nil {
		return "", errors.New("service is uninitialized")
	}
	if sv.IsBroken() {
		return "", errors.New("service is broken")
	}
	return sv.cp.Vm.TrackingToString(sv.cp.Vm.LiveTracking), nil
}

// Says whether pot happened when we called Do.
func (sv *Service) PostHappened() bool {
	return sv.cp.Vm.PostHappened
}

// Turns the markup Pipefish uses internally into highlighting.
func PrettyString(s string, left, right int) string {
	return text.Pretty(s, left, right)
}

// Tries to turn a given Pipefish value into a given Go type. As it necessarily has return
// type `any` (plus an error if the coercion is impossible) the value returned will then
// still need downcasting to the type it was coerced to.
//
// E.g:
//
//	func TwoPlusTwo() int {
//	    v, _ := fooService.Do(`2 + 2`)                   // `v` has type `Value`.
//	    i := fooService.ToGo(v, reflect.TypeFor[int]())  // `i` has type `any`.
//	    return i.(int)                                   // We return an integer as required.
//	}
//
// The error will be non-nil if the coercion is impossible.
func (sv *Service) ToGo(pfValue Value, goType reflect.Type) (any, error) {
	pfTypeInfo := sv.cp.Vm.ConcreteTypeInfo[pfValue.T]
	pfTypeName := pfTypeInfo.GetName(vm.LITERAL)
	goTypeName := goType.String()
	myError := errors.New("cannot coerce Pipefish value of type '" + pfTypeName +
		"' to Go value of type '" + goTypeName + "'")
	if goType.Kind() == reflect.Pointer {
		goDatum, e := sv.ToGo(pfValue, goType.Elem())
		if e != nil {
			return nil, e
		}
		return &goDatum, nil
	}
	if goType == reflect.TypeFor[any]() {
		var ok bool
		goType, ok = DEFAULT_TYPE_FOR[pfValue.T]
		if !ok {
			return nil, myError
		}
	}
	var goDatum any
	if structInfo, ok := pfTypeInfo.(vm.StructType); ok {
		if goType.Kind() != reflect.Struct || structInfo.Len() != goType.NumField() {
			return nil, myError
		}
		goStruct := reflect.New(goType).Elem()
		for i, pfFieldValue := range pfValue.V.([]Value) {
			goFieldDatum, e := sv.ToGo(pfFieldValue, goType.FieldByIndex([]int{i}).Type)
			if e != nil {
				return nil, e
			}
			goStruct.Field(i).Set(reflect.ValueOf(goFieldDatum))
		}
		return goStruct.Interface(), nil
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
		goDatum = pfValue.V.(bool)
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
			for i := 0; i <= vec.Len(); i++ {
				pfElement, _ := vec.Index(i)
				pfValues = append(pfValues, pfElement.(Value))
			}
		} else {
			pfValues = pfValue.V.([]Value)
		}
		goElements := pfValue.V.([]Value)
		switch goType.Kind() {
		case reflect.Array:
			if goType.Len() != len(goElements) {
				return nil, myError
			}
			goArray := reflect.New(goType).Elem()
			for i, pfElement := range pfValues {
				goFieldDatum, e := sv.ToGo(pfElement, goType.Elem())
				if e != nil {
					return nil, e
				}
				goArray.Index(i).Set(reflect.ValueOf(goFieldDatum))
			}
			goDatum = goArray.Interface()
		case reflect.Slice:
			goSlice := reflect.New(goType).Elem()
			goSlice.SetCap(len(goElements))
			goSlice.SetLen(len(goElements))
			for i, pfElement := range pfValue.V.([]Value) {
				goElement, e := sv.ToGo(pfElement, goType.Elem())
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
			goKeyDatum, keyError := sv.ToGo(pfKeyValuePair.Key, goType.Key())
			if keyError != nil {
				return nil, keyError
			}
			goValDatum, valError := sv.ToGo(pfKeyValuePair.Val, goType.Elem())
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
			goElementDatum, elementError := sv.ToGo(pfElement, goType.Key())
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
// This is the only thing that can serialize the `secret` type, exposing its contents. Hence
// if you encrypt it at the same time, then you haven't exposed its contents.
func (sv *Service) WriteSecret(store values.Map, password string) string {
	return sv.cp.Vm.DumpStore(store, password)
}
