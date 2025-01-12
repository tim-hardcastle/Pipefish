package initializer

import (
	"strconv"
	"strings"

	"github.com/tim-hardcastle/Pipefish/source/compiler"
	"github.com/tim-hardcastle/Pipefish/source/dtypes"
	"github.com/tim-hardcastle/Pipefish/source/token"
	"github.com/tim-hardcastle/Pipefish/source/vm"

)

// The serialization of the API belongs to the compiler, since this will if at all be demanded of the
// service at runtime. But the deserialization belongs to the initializer, since this is done during
// initialization.

// This turns a serialized API back into a set of declarations.
// xserve is the external service number: set to DUMMY it will indicate that we're just doing this for human readers and
// can therefore leave off the 'xcall' hooks.

// TODO --- malformed data would crash the deserializer with e.g. indexing errors.
// We need to make this a method of the initializer so it can Throw.
func SerializedAPIToDeclarations(serializedAPI string, xserve uint32) string {
	var buf strings.Builder
	lines := strings.Split(strings.TrimRight(serializedAPI, "\n"), "\n")
	lineNo := 0
	hasHappened := map[string]bool{"ENUM": false, "STRUCT": false, "ABSTRACT": false, "COMMAND": false, "FUNCTION": false}
	types := dtypes.MakeFromSlice([]string{"ENUM", "CLONE", "STRUCT", "ABSTRACT"})
	for lineNo < len(lines) {
		line := lines[lineNo]
		parts := strings.Split(line, " | ")
		if types.Contains(parts[0]) && !hasHappened["ENUM"] && !hasHappened["CLONE"] && 
						!hasHappened["STRUCT"] && !hasHappened["ABSTRACT"] {
				buf.WriteString("newtype\n\n")
		}
		switch parts[0] {
		case "ENUM":
			buf.WriteString(parts[1])
			buf.WriteString(" = enum ")
			buf.WriteString(strings.Join(parts[2:], ", "))
			buf.WriteString("\n\n")
			lineNo++
		case "CLONE":
			buf.WriteString(parts[1])
			buf.WriteString(" = clone ")
			buf.WriteString(parts[2])
			if len(parts) > 3 {
				buf.WriteString(" using ")
				buf.WriteString(strings.Join(parts[3:], ", "))
			}
			buf.WriteString("\n\n")
			lineNo++
		case "STRUCT":
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
			buf.WriteString(")\n\n")
			lineNo++
		case "ABSTRACT":
			buf.WriteString(parts[1])
			buf.WriteString(" = abstract ")
			buf.WriteString(strings.Join(strings.Split(parts[2], " "), "/"))
			buf.WriteString("\n\n")
			lineNo++
		case "COMMAND":
			if !hasHappened["COMMAND"] {
				buf.WriteString("\ncmd\n\n")
			}
			buf.WriteString(makeCommandOrFunctionDeclarationFromParts(parts[1:], xserve))
			lineNo++
		case "FUNCTION":
			if !hasHappened["FUNCTION"] {
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
	if position == vm.UNFIX {
		return functionName
	}
	if position == vm.PREFIX {
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
	if position == vm.SUFFIX {
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

// Deserializes the type schemes. We can't trust the dependency, so we must check at every step that the description is well-formed.
func (iz *initializer) deserializeTypescheme(s string) compiler.AlternateType { // If it is well-formed we know we must have been passed an AlternateType because all the function return information is stored in that form.
	stack := dtypes.Stack[compiler.TypeScheme]{}
	words := strings.Split(s, " ")
	ix := 0
	for ix < len(words) {
		word := words[ix]
		ix++
		if word[0] == '*' { // Then we have a constructor *TT, *AT, or *FT.
			if ix == len(words) { // Then the number we were expecting to find after the constructor can't be there.
				iz.Throw("ext/deserialize/a", &token.Token{Source: "Pipefish builder"})
				return nil
			}
			numAsString := words[ix]
			ix++
			num, err := strconv.Atoi(numAsString)
			if err != nil {
				iz.Throw("ext/deserialize/b", &token.Token{Source: "Pipefish builder"})
				return nil
			}
			types, ok := stack.Take(num) // We try and take that many things off the stack.
			if !ok {
				iz.Throw("ext/deserialize/c", &token.Token{Source: "Pipefish builder"})
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
				res := compiler.FiniteTupleType{}
				for _, ty := range types {
					res = append(res, ty)
				}
				stack.Push(res)
			case "*TT":
				res := altType()
				for _, ty := range types {
					res = append(res, ty)
				}
				stack.Push(compiler.TypedTupleType{res})
			default:
				iz.Throw("ext/deserialize/d", &token.Token{Source: "Pipefish builder"})
				return nil
			}
		} else { // Otherwise we have a word denoting a SimpleType
			aT := iz.cp.GetAlternateTypeFromTypeName(word) // TODO --- is this really the only way to convert a concrete type name to its type number?
			if len(aT) != 1 {
				iz.Throw("ext/deserialize/e", &token.Token{Source: "Pipefish builder"})
				return nil
			}
			ty := aT[0]
			switch ty := ty.(type) {
			case compiler.SimpleType:
				stack.Push(ty)
			default:
				iz.Throw("ext/deserialize/e", &token.Token{Source: "Pipefish builder"})
			}
		}
	}
	// We're done.
	result, ok := stack.Pop() // We should have one thing left on the stack, which is the answer.
	if !ok {
		iz.Throw("ext/deserialize/f", &token.Token{Source: "Pipefish builder"})
		return nil
	}
	switch result := result.(type) { // And it should be an AlternateType.
	case compiler.AlternateType:
		return result
	default:
		iz.Throw("ext/deserialize/g", &token.Token{Source: "Pipefish builder"})
		return nil

	}
}
