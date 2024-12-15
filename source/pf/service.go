package pf

import (
	"database/sql"
	"errors"
	"fmt"
	"io"
	"os"

	"github.com/tim-hardcastle/Pipefish/source/compiler"
	"github.com/tim-hardcastle/Pipefish/source/err"
	"github.com/tim-hardcastle/Pipefish/source/initializer"
	"github.com/tim-hardcastle/Pipefish/source/settings"
	"github.com/tim-hardcastle/Pipefish/source/text"
	"github.com/tim-hardcastle/Pipefish/source/values"

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
	return sv.initialize("InitializeFromCode", code)
}

// Initializes the service with the source code supplied in the file indicated by the filepath.
func (sv *Service) InitializeFromFilepath(scriptFilepath string) error {
	sourcecode, e := compiler.GetSourceCode(scriptFilepath)
	if e != nil {
		return e
	}
	return sv.initialize(scriptFilepath, sourcecode)
}

// Initializes the service on behalf of both the previous methods. As the
// compiler can't see the service class, the other services visible to a
// service have to be supplied as raw compilers.
func (sv *Service) initialize(scriptFilepath, sourcecode string) error {
	compilerMap := make(map[string]*compiler.Compiler)
	for k, v := range sv.localExternals {
		compilerMap[k] = v.cp
	}
	cp := initializer.StartCompiler(scriptFilepath, sourcecode, sv.db, compilerMap)
	sv.cp = cp
	for k, v := range compilerMap {
		sv.localExternals[k].cp = v
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
type InHandler = compiler.InHandler

// An interface with two methods (1) `Out(v Value)` which takes the value supplied when
// the Pipefish code says `post x to Output()` and does something with it, presumably
// serializing it in some way and writing it somewhere; (2) a method `Write(s string)`
// which if necessary allows the user to write a string to the same place.
type OutHandler = compiler.OutHandler

// An InHandler which just gets an input from an io.Reader supplied at its construction.
type SimpleInHandler = compiler.SimpleInHandler

// An OutHandler which serializes the given value and writes it to an io.Writer supplied
// at its construction.
type SimpleOutHandler = compiler.SimpleOutHandler

// An InHandler which supplies a prompt and then gets its input from the terminal.
type StandardInHandler = compiler.StandardInHandler

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
)

// Makes an InHandler which does nothing but get a string from terminal
// input and return it.
func MakeSimpleInHandler(in io.Reader) *SimpleInHandler {
	return compiler.MakeSimpleInHandler(in)
}

// Method makes an `OutHandler` which applies Pipefish's `literal` function
// to the value and then writes the result to the supplied `io.Writer`.
func (sv *Service) MakeLiteralWritingOutHandler(out io.Writer) *SimpleOutHandler {
	return compiler.MakeSimpleOutHandler(out, sv.cp.Vm, true)
}

// Makes an `OutHandler` which applies Pipefish's `string` function
// to the value and then writes the result to the supplied `io.Writer`.
func (sv *Service) MakeStringWritingOutHandler(out io.Writer) *SimpleOutHandler {
	return compiler.MakeSimpleOutHandler(out, sv.cp.Vm, false)
}
// Makes an `OutHandler` which applies Pipefish's `literal` function
// to the value and then writes the result to the terminal.
func (sv *Service) MakeStandardLiteralOutHandler(out io.Writer) *SimpleOutHandler {
	return compiler.MakeSimpleOutHandler(os.Stdout, sv.cp.Vm, false)
}

// Makes an `OutHandler` which applies Pipefish's `string` function to the value and 
// then writes the result to the terminal.
func (sv *Service) MakeStandardStringOutHandler(out io.Writer) *SimpleOutHandler {
	return compiler.MakeSimpleOutHandler(os.Stdout, sv.cp.Vm, false)
}

// Makes an `InHandler` which will get input from the terminal using the string supplied
// to prompt the end user.
func MakeStandardInHandler(prompt string) *StandardInHandler {
	return compiler.MakeStandardInHandler(prompt)
}

// Makes other services visible to the service, as though they were running
// on the same hub: their `external` declarations can then allow them to use one
// another as external services.
func (sv *Service) SetLocalExternalServices(svs map[string]*Service) {
	sv.localExternals = svs
}

// Sets an InHandler, i.e. the thing that decides what happens when you do
// `get x from Input()`.
func (sv *Service) SetInHandler(in compiler.InHandler) error {
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
func (sv *Service) SetOutHandler(out compiler.OutHandler) error {
	if sv.cp == nil {
		return errors.New("service is uninitialized")
	}
	if sv.IsBroken() {
		return errors.New("service is broken")
	}
	sv.cp.Vm.OutHandle = out
	return nil
}

// Sets the database to be used by the service.
func (sv *Service) SetDatabase(db *sql.DB) {
	sv.db = db
	if sv.cp != nil {
		sv.cp.Vm.Database = db
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
		return Value{}, errors.New("service is broken")
	}
	sv.cp.P.ResetAfterError()
	sv.cp.Vm.LiveTracking = make([]compiler.TrackingData, 0)
	state := sv.cp.GetState()
	cT := sv.cp.CodeTop()
	node := sv.cp.P.ParseLine("REPL input", line)
	if settings.SHOW_PARSER {
		fmt.Println("Parsed line:", node.String())
	}
	if sv.cp.P.ErrorsExist() {
		return Value{}, errors.New("error parsing input")
	}
	ctxt := compiler.Context{Env: sv.cp.GlobalVars, Access: compiler.REPL, LowMem: compiler.DUMMY, LogFlavor: compiler.LF_NONE}
	sv.cp.CompileNode(node, ctxt)
	if sv.cp.P.ErrorsExist() {
		return Value{}, errors.New("error compiling input")
	}
	sv.cp.Emit(compiler.Ret)
	sv.cp.Cm("Calling Run from Do.", node.GetToken())
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

// Calls the `main` function.
func (s *Service) CallMain() (values.Value, error) {
	return s.cp.CallIfExists("main")
}

// Checks whether the source code for a service has been changed since it was
// initialized.
func (sv *Service) NeedsUpdate() (bool, error) {
	if sv.cp == nil {
		return false, errors.New("service is uninitialized")
	}
	if sv.IsBroken() {
		return false, errors.New("service is broken")
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

// Gets the source code of the service as a map from filenames to lists of strings (i.e.)
// lines of source code.
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

// Filepath to the root file of the service.
func (sv *Service) Filepath() (string, error) {
	if sv.cp == nil {
		return "", errors.New("service is uninitialized")
	}
	return sv.cp.ScriptFilepath, nil
}

// Returns `true` if the service is uninitialized or failed to compile.
func (sv *Service) IsBroken() bool {
	return sv.cp == nil || sv.cp.P.Common.IsBroken
}

// Returns the errors produced by the last thing the service did, as a list of things
// of type `*Error`.
func (sv *Service) GetErrors() []*Error {
	return sv.cp.P.Common.Errors
}

// Converts a `Value` to a string using Pipefish's `literal` function.
func (sv *Service) ToLiteral(v Value) string {
	return sv.cp.Vm.Literal(v)
}

// Converts a `Value` to a string using Pipefish's `string` function.
func (sv *Service) ToString(v Value) string {
	return sv.cp.Vm.Literal(v)
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

// Gets a report of the tracking if any, in the form of a string which can be passed to
// `PrettyString` for highlighting.
func (sv *Service) GetTrackingReport() (string, error) {
	if sv.cp == nil {
		return "", errors.New("service is uninitialized")
	}
	if sv.IsBroken() {
		return "", errors.New("service is broken")
	}
	return sv.cp.Vm.TrackingToString(), nil
}

// Turns the markup Pipefish uses internally into highlighting.
func PrettyString(s string, left, right int) string {
	return text.Pretty(s, left, right)
}
