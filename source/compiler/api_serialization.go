package compiler

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
	ExternalServiceCp *Compiler
}

// There is a somewhat faster way of doing this when the services are on the same hub, since we would just need
// to change the type numbers. TODO. Until then, this serves as a good test bed for the external services on other hubs.
func (ex ExternalCallToHubHandler) evaluate(mc *Vm, line string) values.Value {
	exVal := ex.ExternalServiceCp.Do(line)
	serialize := ex.ExternalServiceCp.Vm.Literal(exVal)
	return mc.OwningCompiler.Do(serialize)
}

func (es ExternalCallToHubHandler) problem() *err.Error {
	if es.ExternalServiceCp.P.ErrorsExist() {
		return err.CreateErr("ext/broken", &token.Token{Source: "Pipefish builder"})
	}
	return nil
}

func (es ExternalCallToHubHandler) GetAPI() string {
	return es.ExternalServiceCp.SerializeApi()
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
	val := mc.OwningCompiler.Do(exValAsString)
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
func (cp *Compiler) SerializeApi() string {
	var buf strings.Builder
	for i := int(values.FIRST_DEFINED_TYPE); i < len(cp.Vm.ConcreteTypeInfo); i++ {
		if !cp.Vm.ConcreteTypeInfo[i].isEnum() {
			continue
		}
		if !cp.Vm.ConcreteTypeInfo[i].IsPrivate() && !cp.Vm.ConcreteTypeInfo[i].isMandatoryImport() {
			buf.WriteString("ENUM | ")
			buf.WriteString(cp.Vm.ConcreteTypeInfo[i].GetName(DEFAULT))
			for _, el := range cp.Vm.ConcreteTypeInfo[i].(EnumType).ElementNames {
				buf.WriteString(" | ")
				buf.WriteString(el)
			}
			buf.WriteString("\n")
		}
	}

	for ty := int(values.FIRST_DEFINED_TYPE); ty < len(cp.Vm.ConcreteTypeInfo); ty++ {
		if !cp.Vm.ConcreteTypeInfo[ty].IsStruct() {
			continue
		}
		if !cp.Vm.ConcreteTypeInfo[ty].IsPrivate() && !cp.Vm.ConcreteTypeInfo[ty].isMandatoryImport() {
			buf.WriteString("STRUCT | ")
			buf.WriteString(cp.Vm.ConcreteTypeInfo[ty].GetName(DEFAULT))
			labels := cp.Vm.ConcreteTypeInfo[ty].(StructType).LabelNumbers
			for i, lb := range labels { // We iterate by the label and not by the value so that we can have hidden fields in the structs, as we do for efficiency when making a compilable snippet.
				buf.WriteString(" | ")
				buf.WriteString(cp.Vm.Labels[lb])
				buf.WriteString(" ")
				buf.WriteString(cp.serializeAbstractType(cp.Vm.ConcreteTypeInfo[ty].(StructType).AbstractStructFields[i]))
			}
			buf.WriteString("\n")
		}
	}

	var nativeAbstractTypes = []string{"any", "struct", "snippet"}

	for i := len(nativeAbstractTypes); i < len(cp.Vm.AbstractTypes); i++ {
		ty := cp.Vm.AbstractTypes[i]
		if !(cp.IsPrivate(ty.AT)) && !ty.IsMandatoryImport() {
			buf.WriteString("ABSTRACT | ")
			buf.WriteString(ty.Name)
			buf.WriteString(" | ")
			buf.WriteString(cp.serializeAbstractType(ty.AT))
		}
		buf.WriteString("\n")
	}

	for name, fns := range cp.P.FunctionTable {
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
				buf.WriteString(cp.serializeTypescheme(cp.Fns[fn.Number].RtnTypes))
				buf.WriteString("\n")
			}
		}
	}
	return buf.String()
}

func (cp *Compiler) serializeAbstractType(ty values.AbstractType) string {
	return strings.ReplaceAll(cp.Vm.DescribeAbstractType(ty, LITERAL), "/", " ")
}

// The compiler infers more about the return types of a function than is expressed in the code or
// indeed expressible in Pipefish. We will turn the typescheme into a serialized description in Reverse
// Polish notation.
func (cp *Compiler) serializeTypescheme(t TypeScheme) string {
	switch t := t.(type) {
	case SimpleType:
		return cp.Vm.ConcreteTypeInfo[t].GetName(DEFAULT)
	case TypedTupleType:
		acc := ""
		for _, u := range t.T {
			acc = acc + cp.serializeTypescheme(u) + " "
		}
		acc = acc + "*TT " + strconv.Itoa(len(t.T))
		return acc
	case AlternateType:
		acc := ""
		for _, u := range t {
			acc = acc + cp.serializeTypescheme(u) + " "
		}
		acc = acc + "*AT " + strconv.Itoa(len(t))
		return acc
	case FiniteTupleType:
		acc := ""
		for _, u := range t {
			acc = acc + cp.serializeTypescheme(u) + " "
		}
		acc = acc + "*FT " + strconv.Itoa(len(t))
		return acc
	}
	panic("Unhandled type scheme!")
}
