package service

import (
	"pipefish/source/parser"
	"pipefish/source/report"
	"pipefish/source/token"
	"pipefish/source/values"
)

// This is what initialization constructs: a vm and a compiler that between them can evaluate a line of Pipefish.
type VmService struct {
	Mc      *Vm
	Cp      *Compiler // This also contains all the metadata about the top-level source code.
	Broken  bool
	Visited bool
}

func (service *VmService) NeedsUpdate() (bool, error) {
	return service.Cp.NeedsUpdate()
}

// We have two types of external service, defined below: one for services on the same hub, one for services on
// a different hub. Eventually we will need a third class of things on a different hub of the same instance of
// Pipefish, but we haven't implemented that in general yet.
type externalService interface {
	evaluate(mc *Vm, line string) values.Value
	getResolvingParser() *parser.Parser
	problem() *report.Error
}

type externalServiceOnSameHub struct {
	externalService *VmService
}

// There is a somewhat faster way of doing this when the services are on the same hub, since we would just need
// to change the type numbers. TODO. Until then, this serves as a good test bed for the external services on other hubs.
func (ex externalServiceOnSameHub) evaluate(mc *Vm, line string) values.Value {
	exVal := ex.externalService.Cp.Do(ex.externalService.Mc, line)
	serialize := ex.externalService.Mc.Literal(exVal)
	return mc.OwnService.Cp.Do(mc, serialize)
}

func (ex externalServiceOnSameHub) getResolvingParser() *parser.Parser {
	return ex.externalService.Cp.P
}

func (es externalServiceOnSameHub) problem() *report.Error {
	if es.externalService.Broken {
		return report.CreateErr("ext/broken", &token.Token{})
	}
	return nil
}

type externalServiceOnDifferentHub struct {
	username string
	password string
}

func (ex externalServiceOnDifferentHub) evaluate(mc *Vm, line string) values.Value {
	return values.Value{values.NULL, nil}
}

func (es externalServiceOnDifferentHub) getResolvingParser() *parser.Parser {
	return nil
}

func (es externalServiceOnDifferentHub) problem() *report.Error {
	return nil
}
