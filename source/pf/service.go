package pf

import (
	"database/sql"
	"errors"
	"fmt"
	"io"

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

func NewService() *Service {
	return &Service{cp: nil,
		localExternals: make(map[string]*Service),
		db:             nil,
	}
}

func (sv *Service) InitializeFromCode(code string) error {
	return sv.Initialize("InitializeFromCode", code)
}

func (sv *Service) InitializeFromFilepath(scriptFilepath string) error {
	sourcecode, e := compiler.GetSourceCode(scriptFilepath)
	if e != nil {
		return e
	}
	return sv.Initialize(scriptFilepath, sourcecode)
}

func (sv *Service) Initialize(scriptFilepath, sourcecode string) error {
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

type Value = values.Value
type Type = values.ValueType
type InHandler = compiler.InHandler
type OutHandler = compiler.OutHandler
type SimpleInHandler = compiler.SimpleInHandler
type SimpleOutHandler = compiler.SimpleOutHandler
type StandardInHandler = compiler.StandardInHandler
type List = vector.Vector
type Map = *values.Map
type Set = values.Set

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

var (
	UNDEFINED_VALUE = Value{}
)

func MakeSimpleInHandler(in io.Reader) *SimpleInHandler {
	return compiler.MakeSimpleInHandler(in)
}

func (sv *Service) MakeLiteralWritingOutHandler(out io.Writer) *SimpleOutHandler {
	return compiler.MakeSimpleOutHandler(out, sv.cp.Vm, true)
}

func (sv *Service) MakeStringWritingOutHandler(out io.Writer) *SimpleOutHandler {
	return compiler.MakeSimpleOutHandler(out, sv.cp.Vm, false)
}

func (sv *Service) MakeStandardLiteralOutHandler(out io.Writer) *SimpleOutHandler {
	return compiler.MakeSimpleOutHandler(out, sv.cp.Vm, false)
}

func (sv *Service) MakeStandardStringOutHandler(out io.Writer) *SimpleOutHandler {
	return compiler.MakeSimpleOutHandler(out, sv.cp.Vm, false)
}

func MakeStandardInHandler(prompt string) *StandardInHandler {
	return compiler.MakeStandardInHandler(prompt)
}

func (sv *Service) SetLocalExternalServices(svs map[string]*Service) {
	sv.localExternals = svs
}

func (sv *Service) SetInHandler(in compiler.InHandler) error {
	if sv.cp == nil {
		return errors.New("service is uninitialized.")
	}
	if sv.IsBroken() {
		return errors.New("service is broken.")
	}
	sv.cp.Vm.InHandle = in
	return nil
}

func (sv *Service) SetOutHandler(out compiler.OutHandler) error {
	if sv.cp == nil {
		return errors.New("service is uninitialized.")
	}
	if sv.IsBroken() {
		return errors.New("service is broken.")
	}
	sv.cp.Vm.OutHandle = out
	return nil
}

func (sv *Service) SetDatabase(db *sql.DB) {
	sv.db = db
	if sv.cp != nil {
		sv.cp.Vm.Database = db
	}
}

func (sv *Service) Do(line string) (values.Value, error) {
	if sv.cp == nil {
		return UNDEFINED_VALUE, errors.New("service is uninitialized.")
	}
	if sv.IsBroken() {
		return UNDEFINED_VALUE, errors.New("service is broken.")
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
		return UNDEFINED_VALUE, errors.New("error parsing input.")
	}
	ctxt := compiler.Context{Env: sv.cp.GlobalVars, Access: compiler.REPL, LowMem: compiler.DUMMY, LogFlavor: compiler.LF_NONE}
	sv.cp.CompileNode(node, ctxt)
	if sv.cp.P.ErrorsExist() {
		return UNDEFINED_VALUE, errors.New("error compiling input.")
	}
	sv.cp.Emit(compiler.Ret)
	sv.cp.Cm("Calling Run from Do.", node.GetToken())
	sv.cp.Vm.Run(cT)
	result := sv.cp.Vm.Mem[sv.cp.That()]
	sv.cp.Rollback(state, node.GetToken())
	return result, nil
}

func (sv *Service) GetVariable(vname string) (values.Value, error) {
	if sv.cp == nil {
		return UNDEFINED_VALUE, errors.New("service is uninitialized.")
	}
	if sv.IsBroken() {
		return UNDEFINED_VALUE, errors.New("service is broken.")
	}
	v, ok := sv.cp.GlobalVars.GetVar(vname)
	if !ok {
		return UNDEFINED_VALUE, errors.New("Variable does not exist.")
	}
	return sv.cp.Vm.Mem[v.MLoc], nil
}

func (sv *Service) SetVariable(vname string, ty values.ValueType, v any) error {
	if sv.cp == nil {
		return errors.New("service is uninitialized.")
	}
	if sv.IsBroken() {
		return errors.New("service is broken.")
	}
	_, ok := sv.cp.GlobalVars.GetVar(vname)
	if !ok {
		return errors.New("Variable does not exist.")
	}
	sv.cp.Vm.Mem[sv.cp.GlobalVars.Data[vname].MLoc] = values.Value{ty, v}
	return nil
}

func (s *Service) CallMain() (values.Value, error) {
	return s.cp.CallIfExists("main")
}

func (sv *Service) NeedsUpdate() (bool, error) {
	if sv.cp == nil {
		return false, errors.New("service is uninitialized.")
	}
	if sv.IsBroken() {
		return false, errors.New("service is broken.")
	}
	return sv.cp.NeedsUpdate()
}

func (sv *Service) ErrorsExist() (bool, error) {
	if sv.cp == nil {
		return false, errors.New("service is uninitialized.")
	}
	return sv.cp.P.ErrorsExist(), nil
}

func (sv *Service) GetSources() (map[string][]string, error) {
	if sv.cp == nil {
		return nil, errors.New("service is uninitialized.")
	}
	return sv.cp.P.Common.Sources, nil
}

func (sv *Service) GetErrorReport() (string, error) {
	if sv.cp == nil {
		return "", errors.New("service is uninitialized.")
	}
	return sv.cp.P.ReturnErrors(), nil
}

func GetTraceReport(e *err.Error) string {
	result := text.RT_ERROR + e.Message + "\n\n"
	for i := len(e.Trace) - 1; i >= 0; i-- {
		result = result + "  From: " + text.DescribeTok(e.Trace[i]) + text.DescribePos(e.Trace[i]) + "."
	}
	return result + "\n"
}

func ExplainError(es []*Error, i int) (string, error) {
	if i >= len(es) {
		return "", errors.New("index too big for list")
	}
	return (err.ErrorCreatorMap[es[i].ErrorId].Explanation(es, i, es[i].Token, es[i].Args...)), nil
}

func (sv *Service) Filepath() (string, error) {
	if sv.cp == nil {
		return "", errors.New("service is uninitialized.")
	}
	return sv.cp.ScriptFilepath, nil
}

func (sv *Service) IsBroken() bool {
	return sv.cp == nil || sv.cp.P.Common.IsBroken
}

type Error = err.Error

func (sv *Service) GetErrors() []*Error {
	return sv.cp.P.Common.Errors
}

func (sv *Service) Literal(v Value) string {
	return sv.cp.Vm.Literal(v)
}

func (sv *Service) String(v Value) string {
	return sv.cp.Vm.Literal(v)
}

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

func (sv *Service) GetTracking() (string, error) {
	if sv.cp == nil {
		return "", errors.New("service is uninitialized")
	}
	if sv.IsBroken() {
		return "", errors.New("service is broken")
	}
	return sv.cp.Vm.TrackingToString(), nil
}

func PrettyString(s string, left, right int) string {
	return text.Pretty(s, left, right)
}
