package service

import (
	"pipefish/source/values"
)

type Service struct {
	Cp      *Compiler 
	Visited bool              // TODO --- should be stored in the hub not the service if we need it at all.
}

func (service *Service) NeedsUpdate() (bool, error) {
	return service.Cp.NeedsUpdate()
}

func (service *Service) IsBroken() bool {
	return service.Cp.P.Common.IsBroken
}

func (service *Service) GetVariable(vname string) values.Value {
	v, _ := service.Cp.GlobalVars.getVar(vname)
	return service.Cp.Vm.Mem[v.mLoc]
}

func (service *Service) SetVariable(name string, ty values.ValueType, v any) {
	service.Cp.Vm.Mem[service.Cp.GlobalVars.data[name].mLoc] = values.Value{ty, v}
}

func (s *Service) CallMain() values.Value {
	return s.Cp.CallIfExists("main")
}