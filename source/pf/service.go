package pf

import (
	"database/sql"
	"errors"
	"fmt"
	"os"

	"pipefish/source/err"
	"pipefish/source/initializer"
	"pipefish/source/service"
	"pipefish/source/settings"
	"pipefish/source/values"
)

type Value = values.Value
type Type = values.ValueType

var (
	STANDARD_INPUT = &service.ReadlnInHandler{}
	STANDARD_OUTPUT = &service.WriteOutHandler{os.Stdout}
	UNDEFINED_VALUE = Value{}
)

type Service struct {
	Cp             *service.Compiler 
	localExternals map[string]*Service 
	in             service.InHandler
	out            service.OutHandler
	db             *sql.DB
}

func NewService() *Service {
	return &Service{Cp: nil,
					localExternals: make(map[string]*Service),
					in:             STANDARD_INPUT,
					out:            STANDARD_OUTPUT,
					db:             nil,
	}
}

func (sv *Service) InitializeFromFilepath(scriptFilepath string) error {
	compilerMap := make(map[string]*service.Compiler)
	for k, v := range sv.localExternals {
		compilerMap[k] = v.Cp
	}
	cp := initializer.StartService(scriptFilepath, sv.db, compilerMap, sv.in, sv.out)
	sv.Cp = cp
	if sv.IsBroken() {
		return errors.New("compilation error")
	}
	return nil
}


func (sv *Service) SetLocalExternalServices(svs map[string]*Service) {
	sv.localExternals = svs
}

func (sv *Service) SetInput(in service.InHandler) {
	sv.in = in
	if sv.Cp != nil {
		sv.Cp.Vm.IoHandle.InHandle = in
	}
}

func (sv *Service) SetOutput(out service.OutHandler) {
	sv.out = out
	if sv.Cp != nil {
		sv.Cp.Vm.IoHandle.OutHandle = out
	}
}

func (sv *Service) SetDatabase(db *sql.DB) {
	sv.db = db
	if sv.Cp != nil {
		sv.Cp.Vm.Database = db
	}
}

func (sv *Service) Do(line string) (values.Value, error) {
	if sv.Cp == nil {
		return UNDEFINED_VALUE, errors.New("Service is uninitialized.")
	}
	if sv.IsBroken() {
		return UNDEFINED_VALUE, errors.New("Service is broken.")
	}
	state := sv.Cp.GetState()
	cT := sv.Cp.CodeTop()
	node := sv.Cp.P.ParseLine("REPL input", line)
	if settings.SHOW_PARSER {
		fmt.Println("Parsed line:", node.String())
	}
	if sv.Cp.P.ErrorsExist() {
		return UNDEFINED_VALUE, errors.New("Error parsing input.")
	}
	ctxt := service.Context{Env: sv.Cp.GlobalVars, Access: service.REPL, LowMem: service.DUMMY, LogFlavor: service.LF_NONE}
	sv.Cp.CompileNode(node, ctxt)
	if sv.Cp.P.ErrorsExist() {
		return UNDEFINED_VALUE, errors.New("Error compiling input.")
	}
	sv.Cp.Emit(service.Ret)
	sv.Cp.Cm("Calling Run from Do.", node.GetToken())
	sv.Cp.Vm.Run(cT)
	result := sv.Cp.Vm.Mem[sv.Cp.That()]
	sv.Cp.Rollback(state, node.GetToken())
	return result, nil
}

func (sv *Service) GetVariable(vname string) (values.Value, error) {
	if sv.Cp == nil {
		return UNDEFINED_VALUE, errors.New("Service is uninitialized.")
	}
	if sv.IsBroken() {
		return UNDEFINED_VALUE, errors.New("Service is broken.")
	}
	v, ok := sv.Cp.GlobalVars.GetVar(vname)
	if !ok {
		return UNDEFINED_VALUE, errors.New("Variable does not exist.")
	}
	return sv.Cp.Vm.Mem[v.MLoc], nil
}

func (sv *Service) SetVariable(vname string, ty values.ValueType, v any) error {
	if sv.Cp == nil {
		return errors.New("Service is uninitialized.")
	}
	if sv.IsBroken() {
		return errors.New("Service is broken.")
	}
	_, ok := sv.Cp.GlobalVars.GetVar(vname)
	if !ok {
		return errors.New("Variable does not exist.")
	}
	sv.Cp.Vm.Mem[sv.Cp.GlobalVars.Data[vname].MLoc] = values.Value{ty, v}
	return nil
}

func (s *Service) CallMain() (values.Value, error) {
	return s.Cp.CallIfExists("main")
}

func (sv *Service) NeedsUpdate() (bool, error) {
	if sv.Cp == nil {
		return false, errors.New("Service is uninitialized.")
	}
	if sv.IsBroken() {
		return false, errors.New("Service is broken.")
	}
	return sv.Cp.NeedsUpdate()
}

func (sv *Service) IsBroken() bool {
	return sv.Cp == nil || sv.Cp.P.Common.IsBroken
}

type Error = err.Error

func (sv *Service) GetErrors() []*Error {
	return sv.Cp.P.Common.Errors
}

func (sv *Service) Parse(line string) (string, error) {
	astOfLine := sv.Cp.P.ParseLine("test", line)
	if sv.Cp.P.ErrorsExist() {
		return "", errors.New("compilation error")
	}
	return astOfLine.String(), nil
}