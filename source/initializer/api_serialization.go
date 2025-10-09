package initializer

import (
	"strconv"
	"strings"

	"github.com/tim-hardcastle/Pipefish/source/ast"
	"github.com/tim-hardcastle/Pipefish/source/compiler"
	"github.com/tim-hardcastle/Pipefish/source/err"
	"github.com/tim-hardcastle/Pipefish/source/settings"
	"github.com/tim-hardcastle/Pipefish/source/token"
	"github.com/tim-hardcastle/Pipefish/source/values"
	"github.com/tim-hardcastle/Pipefish/source/vm"
)

// We have two types of external service, defined below: one for services on the same hub, one for services on
// a different hub.

type ExternalCallToHubHandler struct {
	Evaluator    func(line string) values.Value
	ProblemFn    func() bool
	SerializeApi func() string
}

// There is a somewhat faster way of doing this when the services are on the same hub, since we would just need
// to change the type numbers. TODO. Until then, this serves as a good test bed for the external services on other hubs.
func (ex ExternalCallToHubHandler) Evaluate(line string) values.Value {
	return ex.Evaluator(line)
}

func (es ExternalCallToHubHandler) Problem() *err.Error {
	if es.ProblemFn() {
		return err.CreateErr("ext/broken", &token.Token{Source: "Pipefish builder"})
	}
	return nil
}

func (es ExternalCallToHubHandler) GetAPI() string {
	return es.SerializeApi()
}

type ExternalHttpCallHandler struct {
	Host         string
	Service      string
	Username     string
	Password     string
	Deserializer func(valAsString string) values.Value
}

func (es ExternalHttpCallHandler) Evaluate(line string) values.Value {
	if settings.SHOW_XCALLS {
		println("Line is", line)
	}
	exValAsString := Do(es.Host, es.Service, line, es.Username, es.Password)
	val := es.Deserializer(exValAsString)
	return val
}

func (es ExternalHttpCallHandler) Problem() *err.Error {
	return nil
}

func (es ExternalHttpCallHandler) GetAPI() string {
	return Do(es.Host, "", "hub serialize \""+es.Service+"\"", es.Username, es.Password)
}

// TODO --- the serializer doesn't send details of the sources, and until it does, 
// we can't tell if two types are meant to be the same type.

// For a description of the file format, see README-api-serialization.md
func (iz *Initializer) SerializeApi() string {
	
	var buf strings.Builder
	for i := int(values.FIRST_DEFINED_TYPE); i < len(iz.cp.Vm.ConcreteTypeInfo); i++ {
		if !iz.cp.Vm.ConcreteTypeInfo[i].IsEnum() {
			continue
		}
		if !iz.cp.Vm.ConcreteTypeInfo[i].IsPrivate() && !iz.cp.Vm.ConcreteTypeInfo[i].IsMandatoryImport() {
			buf.WriteString("ENUM | ")
			buf.WriteString(iz.cp.Vm.ConcreteTypeInfo[i].GetName(vm.DEFAULT))
			for _, el := range iz.cp.Vm.ConcreteTypeInfo[i].(vm.EnumType).ElementNames {
				buf.WriteString(" | ")
				buf.WriteString(el)
			}
			buf.WriteString("\n")
		}
	}

	for i := int(values.FIRST_DEFINED_TYPE); i < len(iz.cp.Vm.ConcreteTypeInfo); i++ {
		info, ok := iz.cp.Vm.ConcreteTypeInfo[i].(vm.CloneType)
		if !ok {
			continue
		}
		if !info.IsPrivate() && !info.IsMandatoryImport() {
			buf.WriteString("CLONE | ")
			buf.WriteString(info.GetName(vm.DEFAULT))
			buf.WriteString(" | ")
			buf.WriteString(iz.cp.Vm.ConcreteTypeInfo[info.Parent].GetName(vm.DEFAULT))
			for _, el := range info.Using {
				buf.WriteString(" | ")
				buf.WriteString(el.Literal)
			}
			buf.WriteString("\n")
		}
	}

	for ty := int(values.FIRST_DEFINED_TYPE); ty < len(iz.cp.Vm.ConcreteTypeInfo); ty++ {
		// TODO --- this is a temporary and vile kludge to stop us from exporting imports
		// until we know how to do this.
		if iz.cp.Vm.ConcreteTypeInfo[ty].GetName(vm.LITERAL) != iz.cp.Vm.ConcreteTypeInfo[ty].GetName(vm.DEFAULT) {
			continue
		}
		if !iz.cp.Vm.ConcreteTypeInfo[ty].IsStruct() { 
			continue
		}
		if !iz.cp.Vm.ConcreteTypeInfo[ty].IsPrivate() && !iz.cp.Vm.ConcreteTypeInfo[ty].IsMandatoryImport() {
			buf.WriteString("STRUCT | ")
			buf.WriteString(iz.cp.Vm.ConcreteTypeInfo[ty].GetName(vm.DEFAULT))
			labels := iz.cp.Vm.ConcreteTypeInfo[ty].(vm.StructType).LabelNumbers
			for i, lb := range labels { // We iterate by the label and not by the value so that we can have hidden fields in the structs, as we do for efficiency when making a compilable snippet.
				buf.WriteString(" | ")
				buf.WriteString(iz.cp.Vm.Labels[lb])
				buf.WriteString(" ")
				buf.WriteString(iz.serializeAbstractType(iz.cp.Vm.ConcreteTypeInfo[ty].(vm.StructType).AbstractStructFields[i]))
			}
			buf.WriteString("\n")
		}
	}

	for i := 0; i < len(iz.cp.Vm.AbstractTypes); i++ {
		ty := iz.cp.Vm.AbstractTypes[i]
		if !(iz.cp.IsPrivate(ty.AT)) && !iz.IsMandatoryImport(ty) && !iz.cp.GeneratedAbstractTypes.Contains(ty.Name) &&
				!((len(ty.AT.Types) == 1) && (iz.cp.Vm.ConcreteTypeInfo[ty.AT.Types[0]].GetName(vm.DEFAULT) == ty.Name)) { // It may be the abstract type containing the concrete type of the same name, in which case we don't want to declare it twice.
			buf.WriteString("ABSTRACT | ")
			buf.WriteString(ty.Name)
			buf.WriteString(" | ")
			buf.WriteString(iz.serializeAbstractType(ty.AT))
			buf.WriteString("\n")
		}
	}

	for name, fns := range iz.functionTable {
		for defOrCmd := 0; defOrCmd < 2; defOrCmd++ { // In the function table the commands and functions are all jumbled up. But we want the commands first, for neatness, so we'll do two passes.
			for _, fn := range fns {
				_, ok := fn.body.(*ast.BuiltInExpression) // Which includes the constructors, which don't need exporting.
				if fn.private || settings.MandatoryImportSet().Contains(fn.op.Source) || ok {
					continue
				}
				if fn.decType == commandDeclaration {
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
				buf.WriteString(strconv.Itoa(int(fn.pos)))
				for _, ntp := range fn.sig {
					buf.WriteString(" | ")
					buf.WriteString(ntp.VarName)
					buf.WriteString(" ")
					buf.WriteString(ntp.VarType.String())
				}
				buf.WriteString(" | ")
				buf.WriteString(iz.serializeTypescheme(iz.cp.Fns[fn.callInfo.Number].RtnTypes))
				buf.WriteString("\n")
			}
		}
	}
	if settings.SHOW_API_SERIALIZATION {
		println("Api serialization:\n\n" + buf.String() + "\n")
	}
	return buf.String()
}

func (iz *Initializer) serializeAbstractType(ty values.AbstractType) string {
	return strings.ReplaceAll(iz.cp.Vm.DescribeAbstractType(ty, vm.LITERAL), "/", " ")
}

func (iz *Initializer) IsMandatoryImport(aT values.AbstractTypeInfo) bool {
	if aT.IsMandatoryImport() {
		return true
	}
	for _, ty := range aT.AT.Types {
		if iz.cp.Vm.ConcreteTypeInfo[ty].IsMandatoryImport() {
			return true
		}
	}
	return false
}

// The compiler infers more about the return types of a function than is expressed in the code or
// indeed expressible in Pipefish. We will turn the typescheme into a serialized description in Reverse
// Polish notation.
func (iz *Initializer) serializeTypescheme(t compiler.TypeScheme) string {
	switch t := t.(type) {
	case compiler.SimpleType:
		return iz.cp.Vm.ConcreteTypeInfo[t].GetName(vm.DEFAULT)
	case compiler.TypedTupleType:
		acc := ""
		for _, u := range t.T {
			acc = acc + iz.serializeTypescheme(u) + " "
		}
		acc = acc + "*TT " + strconv.Itoa(len(t.T))
		return acc
	case compiler.AlternateType:
		acc := ""
		for _, u := range t {
			acc = acc + iz.serializeTypescheme(u) + " "
		}
		acc = acc + "*AT " + strconv.Itoa(len(t))
		return acc
	case compiler.FiniteTupleType:
		acc := ""
		for _, u := range t {
			acc = acc + iz.serializeTypescheme(u) + " "
		}
		acc = acc + "*FT " + strconv.Itoa(len(t))
		return acc
	}
	panic("Unhandled type scheme!")
}

