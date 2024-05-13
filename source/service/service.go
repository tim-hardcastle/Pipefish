package service

import (
	"pipefish/source/parser"
	"pipefish/source/report"
	"pipefish/source/token"
	"pipefish/source/values"
	"strconv"
	"strings"
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

// For a description of the file format, see README-api-serialization.md
func (service VmService) serializeApi() string {
	var buf strings.Builder
	for i := values.LB_ENUMS; i < service.Mc.Ub_enums; i++ {
		if service.Cp.typeAccess[i] == PUBLIC {
			buf.WriteString("ENUM | ")
			buf.WriteString(service.Mc.concreteTypeNames[i])
			for _, el := range service.Mc.Enums[i-values.LB_ENUMS] {
				buf.WriteString(" | ")
				buf.WriteString(el)
			}
			buf.WriteString("\n")
		}
	}

	for i := service.Mc.Ub_enums; i < service.Mc.Lb_snippets; i++ {
		if service.Cp.typeAccess[i] == PUBLIC {
			buf.WriteString("STRUCT | ")
			buf.WriteString(service.Mc.concreteTypeNames[i])
			structOrdinal := i - service.Mc.Ub_enums
			labels := service.Mc.StructLabels[structOrdinal]
			for i, lb := range labels { // We iterate by the label and not by the value so that we can have hidden fields in the structs, as we do for efficiency when making a compilable snippet.
				buf.WriteString(" | ")
				buf.WriteString(service.Mc.Labels[lb])
				buf.WriteString(" ")
				buf.WriteString(service.serializeAbstractType(service.Mc.StructFields[structOrdinal][i]))
			}
			buf.WriteString("\n")
		}
	}

	for i := len(nativeAbstractTypes); i < len(service.Mc.AbstractTypes); i++ {
		ty := service.Mc.AbstractTypes[i]
		if !(service.isPrivate(ty.AT)) {
			buf.WriteString("ABSTRACT | ")
			buf.WriteString(ty.Name)
			buf.WriteString(" | ")
			buf.WriteString(service.serializeAbstractType(ty.AT))
		}
		buf.WriteString("\n")
	}

	for name, fns := range service.Cp.P.FunctionTable {
		for _, fn := range fns {
			if fn.Private {
				continue
			}
			if fn.Cmd {
				buf.WriteString("COMMAND | ")
			} else {
				buf.WriteString("FUNCTION | ")
			}
			buf.WriteString(name)
			buf.WriteString(" | ")
			for _, ntp := range fn.Sig {
				buf.WriteString(" | ")
				buf.WriteString(ntp.VarName)
				buf.WriteString(" ")
				buf.WriteString(ntp.VarType)
			}
			buf.WriteString(" | ")
			buf.WriteString(service.serializeTypescheme(service.Cp.Fns[fn.Number].Types))
		}
	}
	return buf.String()
}

func (service *VmService) isPrivate(a values.AbstractType) bool { // TODO --- obviously this only needs claculating once and sticking in the compiler.
	for _, w := range a.Types {
		if service.Cp.typeAccess[w] == PRIVATE {
			return true
		}
	}
	return false
}

func (service *VmService) serializeAbstractType(ty values.AbstractType) string {
	return strings.ReplaceAll(service.Mc.DescribeAbstractType(ty), "/", " ")
}

// The compiler infers more about the return types of a function than is expressed in the code or
// indeed expressible in Pipefish. We will turn the typescheme into a serialized description in Reverse
// Polish notation.
func (service *VmService) serializeTypescheme(t typeScheme) string {
	switch t := t.(type) {
	case simpleType:
		return service.Mc.concreteTypeNames[t]
	case TypedTupleType:
		acc := ""
		for _, u := range t.T {
			acc = acc + service.serializeTypescheme(u) + " "
		}
		acc = acc + "*TT " + strconv.Itoa(len(t.T))
		return acc
	case AlternateType:
		acc := ""
		for _, u := range t {
			acc = acc + service.serializeTypescheme(u) + " "
		}
		acc = acc + "*AT " + strconv.Itoa(len(t))
		return acc
	case finiteTupleType:
		acc := ""
		for _, u := range t {
			acc = acc + service.serializeTypescheme(u) + " "
		}
		acc = acc + "*FT " + strconv.Itoa(len(t))
		return acc
	}
	panic("Unhandled type scheme!")
}
