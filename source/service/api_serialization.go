package service

import (
	"pipefish/source/dtypes"
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
type externalCallHandler interface {
	evaluate(mc *Vm, line string) values.Value

	problem() *err.Error
	getAPI() string
}

type externalCallToHubHandler struct {
	externalService *Service
}

// There is a somewhat faster way of doing this when the services are on the same hub, since we would just need
// to change the type numbers. TODO. Until then, this serves as a good test bed for the external services on other hubs.
func (ex externalCallToHubHandler) evaluate(mc *Vm, line string) values.Value {
	exVal := ex.externalService.Cp.Do(line)
	serialize := ex.externalService.Cp.Vm.Literal(exVal)
	return mc.OwnService.Cp.Do(serialize)
}

func (es externalCallToHubHandler) problem() *err.Error {
	if es.externalService.Cp.P.ErrorsExist() {
		return err.CreateErr("ext/broken", &token.Token{Source: "Pipefish builder"})
	}
	return nil
}

func (es externalCallToHubHandler) getAPI() string {
	return es.externalService.SerializeApi()
}

type externalHttpCallHandler struct {
	host     string
	service  string
	username string
	password string
}

func (es externalHttpCallHandler) evaluate(mc *Vm, line string) values.Value {
	if settings.SHOW_XCALLS {
		println("Line is", line)
	}
	exValAsString := p2p.Do(es.host, line, es.username, es.password)
	if settings.SHOW_XCALLS {
		println("Returned string is", exValAsString)
	}
	val := mc.OwnService.Cp.Do(exValAsString)
	if settings.SHOW_XCALLS {
		println("Value is", mc.DefaultDescription(val))
	}
	return val
}

func (es externalHttpCallHandler) problem() *err.Error {
	return nil
}

func (es externalHttpCallHandler) getAPI() string {
	return p2p.Do(es.host, "hub serialize \""+es.service+"\"", es.username, es.password)
}

// For a description of the file format, see README-api-serialization.md
func (service Service) SerializeApi() string {
	var buf strings.Builder
	for i := int(values.FIRST_DEFINED_TYPE); i < len(service.Cp.Vm.concreteTypeInfo); i++ {
		if !service.Cp.Vm.concreteTypeInfo[i].isEnum() {
			continue
		}
		if !service.Cp.Vm.concreteTypeInfo[i].isPrivate() && !service.Cp.Vm.concreteTypeInfo[i].isMandatoryImport() {
			buf.WriteString("ENUM | ")
			buf.WriteString(service.Cp.Vm.concreteTypeInfo[i].getName(DEFAULT))
			for _, el := range service.Cp.Vm.concreteTypeInfo[i].(enumType).elementNames {
				buf.WriteString(" | ")
				buf.WriteString(el)
			}
			buf.WriteString("\n")
		}
	}

	for ty := int(values.FIRST_DEFINED_TYPE); ty < len(service.Cp.Vm.concreteTypeInfo); ty++ {
		if !service.Cp.Vm.concreteTypeInfo[ty].isStruct() {
			continue
		}
		if !service.Cp.Vm.concreteTypeInfo[ty].isPrivate() && !service.Cp.Vm.concreteTypeInfo[ty].isMandatoryImport() {
			buf.WriteString("STRUCT | ")
			buf.WriteString(service.Cp.Vm.concreteTypeInfo[ty].getName(DEFAULT))
			labels := service.Cp.Vm.concreteTypeInfo[ty].(structType).labelNumbers
			for i, lb := range labels { // We iterate by the label and not by the value so that we can have hidden fields in the structs, as we do for efficiency when making a compilable snippet.
				buf.WriteString(" | ")
				buf.WriteString(service.Cp.Vm.Labels[lb])
				buf.WriteString(" ")
				buf.WriteString(service.serializeAbstractType(service.Cp.Vm.concreteTypeInfo[ty].(structType).abstractStructFields[i]))
			}
			buf.WriteString("\n")
		}
	}

	for i := len(nativeAbstractTypes); i < len(service.Cp.Vm.AbstractTypes); i++ {
		ty := service.Cp.Vm.AbstractTypes[i]
		if !(service.isPrivate(ty.AT)) && !ty.IsMandatoryImport() {
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

func (service *Service) isPrivate(a values.AbstractType) bool { // TODO --- obviously this only needs calculating once and sticking in the compiler.
	for _, w := range a.Types {
		if service.Cp.Vm.concreteTypeInfo[w].isPrivate() {
			return true
		}
	}
	return false
}

// And then we need a way to turn a serialized API back into a set of declarations.
// xserve is the external service number: set to DUMMY it will indicate that we're just doing this for human readers and
// can therefore leave off the 'xcall' hooks.
func SerializedAPIToDeclarations(serializedAPI string, xserve uint32) string {
	var buf strings.Builder
	lines := strings.Split(strings.TrimRight(serializedAPI, "\n"), "\n")
	lineNo := 0
	hasHappened := map[string]bool{"ENUM": false, "STRUCT": false, "ABSTRACT": false, "COMMAND": false, "FUNCTION": false}
	for lineNo < len(lines) {
		line := lines[lineNo]
		parts := strings.Split(line, " | ")
		switch parts[0] {
		case "ENUM":
			if !hasHappened["ENUM"] {
				buf.WriteString("newtype\n\n")
			}
			buf.WriteString(parts[1])
			buf.WriteString(" = enum ")
			buf.WriteString(strings.Join(strings.Split(parts[2], " "), ", "))
			buf.WriteString("\n")
			lineNo++
		case "STRUCT":
			if hasHappened["ENUM"] && !hasHappened["STRUCT"] {
				buf.WriteString("\n")
			}
			if !hasHappened["ENUM"] && !hasHappened["STRUCT"] {
				buf.WriteString("newtype\n\n")
			}
			buf.WriteString(parts[1])
			buf.WriteString(" = struct (")
			sep := ""
			for _, param := range parts[2:] {
				buf.WriteString(sep)
				fieldNameAndTypes := strings.Split(param, " ")
				buf.WriteString(fieldNameAndTypes[0])
				buf.WriteString(" ")
				buf.WriteString(strings.Join(fieldNameAndTypes[1:], "/"))
				sep = ", "
			}
			buf.WriteString(")\n")
			lineNo++
		case "ABSTRACT":
			if (hasHappened["ENUM"] || hasHappened["STRUCT"]) && !hasHappened["ABSTRACT"] {
				buf.WriteString("\n")
			}
			if !hasHappened["ENUM"] && !hasHappened["STRUCT"] && !hasHappened["ABSTRACT"] {
				buf.WriteString("newtype\n\n")
			}
			buf.WriteString(parts[1])
			buf.WriteString(" = abstract ")
			buf.WriteString(strings.Join(strings.Split(parts[2], " "), "/"))
			lineNo++
		case "COMMAND":
			if !hasHappened["CMD"] {
				buf.WriteString("\ncmd\n\n")
			}
			buf.WriteString(makeCommandOrFunctionDeclarationFromParts(parts[1:], xserve))
			lineNo++
		case "FUNCTION":
			if !hasHappened["DEF"] {
				buf.WriteString("\ndef\n\n")
			}
			buf.WriteString(makeCommandOrFunctionDeclarationFromParts(parts[1:], xserve))
			lineNo++
		case "":
			lineNo++
		default:
			panic("Oops, found" + parts[0] + "instead. Drat.")
		}
		hasHappened[parts[0]] = true
	}
	return buf.String()
}

func makeCommandOrFunctionDeclarationFromParts(parts []string, xserve uint32) string {
	var buf strings.Builder
	// We have snipped off the part saying "FUNCTION" or "COMMAND", so the list of parts now looks like this:
	// functionName | 0, 1, 2, 3 for prefix/infix/suffix/unfix | parameterName1 type1 | parameterName2 type2 | serialization of typescheme
	// We can't use the serialization of the typescheme here, so we can break it down into parts:
	functionName := parts[0]
	posInt, _ := strconv.Atoi(parts[1])
	position := uint32(posInt)
	params := parts[2 : len(parts)-1]
	if position == UNFIX {
		return functionName
	}
	if position == PREFIX {
		buf.WriteString(functionName)
		buf.WriteString(" ")
	}
	buf.WriteString("(")
	lastWasBling := false
	for i, param := range params {
		bits := strings.Split(param, " ")
		if bits[1] == "bling" {
			if !lastWasBling {
				buf.WriteString(")")
			}
			buf.WriteString(" ")
			buf.WriteString(bits[0])
			lastWasBling = true
			continue
		}
		// So it's non-bling
		if lastWasBling {
			buf.WriteString(" (")
		} else {
			if i > 0 {
				buf.WriteString(", ")
			}
		}
		buf.WriteString(bits[0])
		buf.WriteString(" ")
		buf.WriteString(bits[1])
	}
	buf.WriteString(")")
	if position == SUFFIX {
		buf.WriteString(" ")
		buf.WriteString(functionName)
	}
	if xserve != DUMMY { // Then we need to insert the hook.
		buf.WriteString(" : xcall ")
		buf.WriteString(strconv.Itoa(int(xserve)))
		buf.WriteString(", ")
		buf.WriteString("\"")
		buf.WriteString(functionName)
		buf.WriteString("\"")
		buf.WriteString(", ")
		buf.WriteString(strconv.Itoa(int(position)))
		buf.WriteString(", ")
		buf.WriteString("\"")
		buf.WriteString(parts[len(parts)-1])
		buf.WriteString("\"")
	}
	buf.WriteString("\n")
	return buf.String()
}

func (service *Service) serializeAbstractType(ty values.AbstractType) string {
	return strings.ReplaceAll(service.Cp.Vm.DescribeAbstractType(ty, LITERAL), "/", " ")
}

// The compiler infers more about the return types of a function than is expressed in the code or
// indeed expressible in Pipefish. We will turn the typescheme into a serialized description in Reverse
// Polish notation.
func (service *Service) serializeTypescheme(t typeScheme) string {
	switch t := t.(type) {
	case simpleType:
		return service.Cp.Vm.concreteTypeInfo[t].getName(DEFAULT)
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

// And then the deserialization. We can't trust the dependency, so we must check at every step that the description is well-formed.
func (cp *Compiler) deserializeTypescheme(s string) AlternateType { // If it is well-formed we know we must have been passed an AlternateType because all the function return information is stored in that form.
	stack := dtypes.Stack[typeScheme]{}
	words := strings.Split(s, " ")
	ix := 0
	for ix < len(words) {
		word := words[ix]
		ix++
		if word[0] == '*' { // Then we have a constructor *TT, *AT, or *FT.
			if ix == len(words) { // Then the number we were expecting to find after the constructor can't be there.
				cp.P.Throw("ext/deserialize/a", &token.Token{Source: "Pipefish builder"})
				return nil
			}
			numAsString := words[ix]
			ix++
			num, err := strconv.Atoi(numAsString)
			if err != nil {
				cp.P.Throw("ext/deserialize/b", &token.Token{Source: "Pipefish builder"})
				return nil
			}
			types, ok := stack.Take(num) // We try and take that many things off the stack.
			if !ok {
				cp.P.Throw("ext/deserialize/c", &token.Token{Source: "Pipefish builder"})
				return nil
			}
			// If we've gotten this far, then it's well-formed so far, and we can construct a compound type and stick it on the stack.
			switch word {
			case "*AT":
				res := altType()
				for _, ty := range types {
					res = append(res, ty)
				}
				stack.Push(res)
			case "*FT":
				res := finiteTupleType{}
				for _, ty := range types {
					res = append(res, ty)
				}
				stack.Push(res)
			case "*TT":
				res := altType()
				for _, ty := range types {
					res = append(res, ty)
				}
				stack.Push(TypedTupleType{res})
			default:
				cp.P.Throw("ext/deserialize/d", &token.Token{Source: "Pipefish builder"})
				return nil
			}
		} else { // Otherwise we have a word denoting a simpleType
			aT := cp.getAlternateTypeFromTypeName(word) // TODO --- is this really the only way to convert a concrete type name to its type number?
			if len(aT) != 1 {
				cp.P.Throw("ext/deserialize/e", &token.Token{Source: "Pipefish builder"})
				return nil
			}
			ty := aT[0]
			switch ty := ty.(type) {
			case simpleType:
				stack.Push(ty)
			default:
				cp.P.Throw("ext/deserialize/e", &token.Token{Source: "Pipefish builder"})
			}
		}
	}
	// We're done.
	result, ok := stack.Pop() // We should have one thing left on the stack, which is the answer.
	if !ok {
		cp.P.Throw("ext/deserialize/f", &token.Token{Source: "Pipefish builder"})
		return nil
	}
	switch result := result.(type) { // And it should be an AlternateType.
	case AlternateType:
		return result
	default:
		cp.P.Throw("ext/deserialize/g", &token.Token{Source: "Pipefish builder"})
		return nil

	}
}