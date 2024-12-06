package service

import (
	"pipefish/source/err"
	"pipefish/source/p2p"
	"pipefish/source/settings"
	"pipefish/source/token"
	"pipefish/source/values"
	"strconv"
	"strings"
)

// We have two types of external service, defined below: one for services on the same hub, one for services on
// a different hub.
type ExternalCallHandler interface {
	evaluate(mc *Vm, line string) values.Value

	problem() *err.Error
	GetAPI() string
}

type ExternalCallToHubHandler struct {
	ExternalService *Service
}

// There is a somewhat faster way of doing this when the services are on the same hub, since we would just need
// to change the type numbers. TODO. Until then, this serves as a good test bed for the external services on other hubs.
func (ex ExternalCallToHubHandler) evaluate(mc *Vm, line string) values.Value {
	exVal := ex.ExternalService.Cp.Do(line)
	serialize := ex.ExternalService.Cp.Vm.Literal(exVal)
	return mc.OwnService.Cp.Do(serialize)
}

func (es ExternalCallToHubHandler) problem() *err.Error {
	if es.ExternalService.Cp.P.ErrorsExist() {
		return err.CreateErr("ext/broken", &token.Token{Source: "Pipefish builder"})
	}
	return nil
}

func (es ExternalCallToHubHandler) GetAPI() string {
	return es.ExternalService.SerializeApi()
}

type ExternalHttpCallHandler struct {
	Host     string
	Service  string
	Username string
	Password string
}

func (es ExternalHttpCallHandler) evaluate(mc *Vm, line string) values.Value {
	if settings.SHOW_XCALLS {
		println("Line is", line)
	}
	exValAsString := p2p.Do(es.Host, line, es.Username, es.Password)
	if settings.SHOW_XCALLS {
		println("Returned string is", exValAsString)
	}
	val := mc.OwnService.Cp.Do(exValAsString)
	if settings.SHOW_XCALLS {
		println("Value is", mc.DefaultDescription(val))
	}
	return val
}

func (es ExternalHttpCallHandler) problem() *err.Error {
	return nil
}

func (es ExternalHttpCallHandler) GetAPI() string {
	return p2p.Do(es.Host, "hub serialize \""+es.Service+"\"", es.Username, es.Password)
}

// For a description of the file format, see README-api-serialization.md
func (service Service) SerializeApi() string {
	var buf strings.Builder
	for i := int(values.FIRST_DEFINED_TYPE); i < len(service.Cp.Vm.ConcreteTypeInfo); i++ {
		if !service.Cp.Vm.ConcreteTypeInfo[i].isEnum() {
			continue
		}
		if !service.Cp.Vm.ConcreteTypeInfo[i].IsPrivate() && !service.Cp.Vm.ConcreteTypeInfo[i].isMandatoryImport() {
			buf.WriteString("ENUM | ")
			buf.WriteString(service.Cp.Vm.ConcreteTypeInfo[i].GetName(DEFAULT))
			for _, el := range service.Cp.Vm.ConcreteTypeInfo[i].(EnumType).ElementNames {
				buf.WriteString(" | ")
				buf.WriteString(el)
			}
			buf.WriteString("\n")
		}
	}

	for ty := int(values.FIRST_DEFINED_TYPE); ty < len(service.Cp.Vm.ConcreteTypeInfo); ty++ {
		if !service.Cp.Vm.ConcreteTypeInfo[ty].IsStruct() {
			continue
		}
		if !service.Cp.Vm.ConcreteTypeInfo[ty].IsPrivate() && !service.Cp.Vm.ConcreteTypeInfo[ty].isMandatoryImport() {
			buf.WriteString("STRUCT | ")
			buf.WriteString(service.Cp.Vm.ConcreteTypeInfo[ty].GetName(DEFAULT))
			labels := service.Cp.Vm.ConcreteTypeInfo[ty].(StructType).LabelNumbers
			for i, lb := range labels { // We iterate by the label and not by the value so that we can have hidden fields in the structs, as we do for efficiency when making a compilable snippet.
				buf.WriteString(" | ")
				buf.WriteString(service.Cp.Vm.Labels[lb])
				buf.WriteString(" ")
				buf.WriteString(service.serializeAbstractType(service.Cp.Vm.ConcreteTypeInfo[ty].(StructType).AbstractStructFields[i]))
			}
			buf.WriteString("\n")
		}
	}

	var nativeAbstractTypes = []string{"any", "struct", "snippet"}

	for i := len(nativeAbstractTypes); i < len(service.Cp.Vm.AbstractTypes); i++ {
		ty := service.Cp.Vm.AbstractTypes[i]
		if !(service.IsPrivate(ty.AT)) && !ty.IsMandatoryImport() {
			buf.WriteString("ABSTRACT | ")
			buf.WriteString(ty.Name)
			buf.WriteString(" | ")
			buf.WriteString(service.serializeAbstractType(ty.AT))
		}
		buf.WriteString("\n")
	}

	for name, fns := range service.Cp.P.FunctionTable {
		for defOrCmd := 0; defOrCmd < 2; defOrCmd++ { // In the function table the commands and functions are all jumbled up. But we want the commands first, for neatness, so we'll do two passes.
			for _, fn := range fns {
				if fn.Private || settings.MandatoryImportSet().Contains(fn.Body.GetToken().Source) {
					continue
				}
				if fn.Cmd {
					if defOrCmd == 1 {
						continue
					}
					buf.WriteString("COMMAND | ")
				} else {
					if defOrCmd == 0 {
						continue
					}
					buf.WriteString("FUNCTION | ")
				}
				buf.WriteString(name)
				buf.WriteString(" | ")
				buf.WriteString(strconv.Itoa(int(fn.Position)))
				for _, ntp := range fn.NameSig {
					buf.WriteString(" | ")
					buf.WriteString(ntp.VarName)
					buf.WriteString(" ")
					buf.WriteString(ntp.VarType)
				}
				buf.WriteString(" | ")
				buf.WriteString(service.serializeTypescheme(service.Cp.Fns[fn.Number].RtnTypes))
				buf.WriteString("\n")
			}
		}
	}
	return buf.String()
}

func (service *Service) IsPrivate(a values.AbstractType) bool { // TODO --- obviously this only needs calculating once and sticking in the compiler.
	for _, w := range a.Types {
		if service.Cp.Vm.ConcreteTypeInfo[w].IsPrivate() {
			return true
		}
	}
	return false
}

func (service *Service) serializeAbstractType(ty values.AbstractType) string {
	return strings.ReplaceAll(service.Cp.Vm.DescribeAbstractType(ty, LITERAL), "/", " ")
}

// The compiler infers more about the return types of a function than is expressed in the code or
// indeed expressible in Pipefish. We will turn the typescheme into a serialized description in Reverse
// Polish notation.
func (service *Service) serializeTypescheme(t TypeScheme) string {
	switch t := t.(type) {
	case SimpleType:
		return service.Cp.Vm.ConcreteTypeInfo[t].GetName(DEFAULT)
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
	case FiniteTupleType:
		acc := ""
		for _, u := range t {
			acc = acc + service.serializeTypescheme(u) + " "
		}
		acc = acc + "*FT " + strconv.Itoa(len(t))
		return acc
	}
	panic("Unhandled type scheme!")
}
