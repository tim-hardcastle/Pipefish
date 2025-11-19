// Some small functions which answer the question "Given datum x, how do I find y?"

package compiler

import (
	"bytes"
	"os"
	"reflect"
	"regexp"
	"strconv"
	"strings"

	"github.com/tim-hardcastle/pipefish/source/ast"
	"github.com/tim-hardcastle/pipefish/source/dtypes"
	"github.com/tim-hardcastle/pipefish/source/lexer"
	"github.com/tim-hardcastle/pipefish/source/text"
	"github.com/tim-hardcastle/pipefish/source/values"
	"github.com/tim-hardcastle/pipefish/source/vm"
	"src.elv.sh/pkg/persistent/vector"
)

func (cp *Compiler) getAbstractType(name string) values.AbstractType {
	return cp.GetAbstractTypeFromTypeName(name)
}

func (cp *Compiler) GetTypeNameFromNumber(typeNumber values.ValueType) string {
	return cp.Vm.ConcreteTypeInfo[typeNumber].GetName(vm.DEFAULT)
}

func (cp *Compiler) GetConcreteType(name string) (values.ValueType, bool) {
	abstractType := cp.getAbstractType(name)
	if abstractType.Len() != 1 {
		return values.UNDEFINED_TYPE, false
	}
	return abstractType.Types[0], true
}

func (cp *Compiler) ConcreteTypeNow(name string) values.ValueType {
	abstractType := cp.getAbstractType(name)
	if len(abstractType.Types) == 0 {
		panic("Name is " + name)
	}
	return abstractType.Types[0]
}

func (cp *Compiler) TypeInfoNow(name string) vm.TypeInformation {
	concreteType, _ := cp.GetConcreteType(name)
	return cp.Vm.ConcreteTypeInfo[concreteType]
}

func (cp *Compiler) getTypeInformation(name string) (vm.TypeInformation, bool) {
	concreteType, ok := cp.GetConcreteType(name)
	if !ok {
		return nil, false
	}
	return cp.Vm.ConcreteTypeInfo[concreteType], true
}

func (cp *Compiler) IsBuiltin(name string) bool {
	typeInfo, ok := cp.getTypeInformation(name)
	if !ok {
		return false
	} else {
		_, ok := typeInfo.(vm.BuiltinType)
		return ok
	}
}

func (cp *Compiler) isEnum(name string) bool {
	typeInfo, ok := cp.getTypeInformation(name)
	if !ok {
		return false
	} else {
		_, ok := typeInfo.(vm.EnumType)
		return ok
	}
}

func (cp *Compiler) isClone(name string) bool {
	typeInfo, ok := cp.getTypeInformation(name)
	if !ok {
		return false
	} else {
		_, ok := typeInfo.(vm.CloneType)
		return ok
	}
}

func (cp *Compiler) IsStruct(name string) bool {
	typeInfo, ok := cp.getTypeInformation(name)
	if !ok {
		return false
	} else {
		_, ok := typeInfo.(vm.StructType)
		return ok
	}
}

func (cp *Compiler) IsPrivate(a values.AbstractType) bool {
	for _, w := range a.Types {
		if cp.Vm.ConcreteTypeInfo[w].IsPrivate() {
			return true
		}
	}
	return false
}

func (cp *Compiler) typeNumberIsStruct(T values.ValueType) bool {
	_, ok := cp.Vm.ConcreteTypeInfo[T].(vm.StructType)
	return ok
}

func (cp *Compiler) alternateTypeIsOnlyStruct(aT AlternateType) (values.ValueType, bool) {
	if len(aT) == 1 {
		switch el := aT[0].(type) {
		case SimpleType:
			if cp.typeNumberIsStruct(values.ValueType(el)) {
				return values.ValueType(el), true
			}
		default:
			return values.UNDEFINED_TYPE, false
		}
	}
	return values.UNDEFINED_TYPE, false
}

func (cp *Compiler) alternateTypeIsOnlyAssortedStructs(aT AlternateType) bool {
	for _, el := range aT {
		switch el := el.(type) {
		case SimpleType:
			if !cp.typeNumberIsStruct(values.ValueType(el)) {
				return false
			}
		default:
			return false
		}
	}
	return true
}

func (cp *Compiler) ReturnSigToAlternateType(sig ast.AstSig) FiniteTupleType {
	if sig == nil {
		return nil
	}
	ftt := FiniteTupleType{}
	for _, pair := range sig {
		ftt = append(ftt, cp.GetAlternateTypeFromTypeAst(pair.VarType))
	}
	return ftt
}

func (cp *Compiler) rtnTypesToTypeScheme(rtnSig ast.AbstractSig) AlternateType {
	if len(rtnSig) == 0 {
		return cp.Common.AnyTypeScheme
	}
	if len(rtnSig) == 1 {
		return AbstractTypeToAlternateType(rtnSig[0].VarType)
	}
	tup := FiniteTupleType{}
	for _, v := range rtnSig {
		tup = append(tup, AbstractTypeToAlternateType(v.VarType))
	}
	return AlternateType{tup}
}

// Either we already have an AlternateType, and can return it, or we have a type in the form of a string and
// can transform it into one.
func (cp *Compiler) getTypes(s signature, i int) AlternateType {
	typeRep := s.GetVarType(i)
	if typeRep == nil {
		return AltType()
	}
	switch typeRep := typeRep.(type) {
	case ast.TypeNode:
		return cp.GetAlternateTypeFromTypeAst(typeRep)
	case AlternateType:
		return typeRep
	default:
		panic("Found unexpected type " + reflect.TypeOf(typeRep).String())
	}
}

type NameAlternateTypePair struct {
	VarName string
	VarType AlternateType
}

func (ntp NameAlternateTypePair) GetName() string {
	return ntp.VarName
}

func (ntp NameAlternateTypePair) GetType() any {
	return ntp.VarType
}

func getVarNames(sig signature) string {
	names := []string{}
	for i := 0; i < sig.Len(); i++ {
		names = append(names, sig.GetVarName(i))
	}
	return strings.Join(names, ", ")
}

func (cp *Compiler) GetAlternateTypeFromTypeAst(typeNode ast.TypeNode) AlternateType {
	if typeNode, ok := typeNode.(*ast.TypeDotDotDot); ok {
		return AlternateType{TypedTupleType{cp.GetAlternateTypeFromTypeAst(typeNode.Right)}}
	}
	abType := cp.GetAbstractTypeFromAstType(typeNode)
	return AbstractTypeToAlternateType(abType)
}

func (cp *Compiler) GetAlternateTypeFromConcreteTypeName(name string) AlternateType {
	return AlternateType{SimpleType(cp.ConcreteTypeNow(name))}
}

// Manufactures a value.
func val(T values.ValueType, V any) values.Value {
	return values.Value{T: T, V: V}
}

func GetSourceCode(scriptFilepath string) (string, error) {
	var sourcebytes []byte
	var err error
	if scriptFilepath != "" { // In which case we're making a blank VM.
		if len(scriptFilepath) >= 11 && scriptFilepath[:11] == "test-files/" {
			sourcebytes, err = TestFolder.ReadFile(scriptFilepath)
		} else {
			sourcebytes, err = os.ReadFile(text.MakeFilepath(scriptFilepath))
		}
		if err != nil {
			return "", err
		}
	}
	sourcebytes = append(sourcebytes, '\n')
	return string(sourcebytes), nil
}

var (
	nondecimalIndicators = dtypes.MakeFromSlice([]rune{'b', 'B', 'o', 'O', 'x', 'X'})
	control              = dtypes.MakeFromSlice([]string{"break", "continue", "else", "try"})
	reserved             = dtypes.MakeFromSlice([]string{"and", "false", "given", "not", "or", "true", "->", ">>", "?>", "--"})
	illegalInRepl        = dtypes.MakeFromSlice([]string{"cmd", "const", "def", "external", "global", "golang", "import", "newtype", "private", "var", "\\\\", "~~"})
	// Used by the syntax highlighter; should not be used by anything else without much forethought.
	// TODO --- there must be some principled way to generate this from something else.
	nativeTypes          = dtypes.MakeFromSlice([]string{"ok", "int", "string", "rune", "bool", "float", "error", "type", "pair", "list", "map", "set", "label", "func", "null", "snippet", "secret", "clone", "clones", "enum", "struct", "any", "ref", "tuple"})
	enumlike, _          = regexp.Compile(`^[A-Z][A-Z_]+$`)
	typelike, _          = regexp.Compile(`^[A-Z][A-Z]*[a-z]+[A-Za-z]*$`)
	bracketMatch         = map[rune]rune{'(': ')', '[': ']', '{': '}'}
	leftBrackets         = dtypes.MakeFromSlice([]rune{'(', '[', '{'})
	rightBrackets        = dtypes.MakeFromSlice([]rune{')', ']', '}'})
)

// We can't just lex it beause we need the whitespace intact. But we can
// use the lexer logic.
func (cp *Compiler) Highlight(code []rune, fonts *values.Map) string {
	var out bytes.Buffer
	brackets := []rune{}
	runes := lexer.NewRuneSupplier(code)
	for runes.CurrentRune() != '\n' && runes.CurrentRune() != 0 {
		switch {
		// First we deal with the brackets, which have their own rules.
		case leftBrackets.Contains(runes.CurrentRune()):
			font := getFontForBrackets(len(brackets), fonts)
			brackets = append(brackets, runes.CurrentRune())
			out.WriteString(font)
			out.WriteRune(runes.CurrentRune())
			out.WriteString(text.RESET)
		case rightBrackets.Contains(runes.CurrentRune()):
			font := text.BAD_RED + text.UNDERLINE
			if len(brackets) > 0 && bracketMatch[brackets[len(brackets)-1]] == runes.CurrentRune() {
				brackets = brackets[:len(brackets)-1]
				font = getFontForBrackets(len(brackets), fonts)
			}
			out.WriteString(font)
			out.WriteRune(runes.CurrentRune())
			out.WriteString(text.RESET)
		// We could be looking at protected punctuation.
		case lexer.IsProtectedPunctuation(runes.CurrentRune()) ||
			(runes.CurrentRune() == '!' && runes.PeekRune() == '='):
			out.WriteString(wrapFont(string(runes.CurrentRune()), "reserved", fonts))
		// A formatted string literal.
		case runes.CurrentRune() == '"':
			result, ok := runes.ReadFormattedString()
			result = `"` + result
			if ok {
				result = result + `"`
			}
			out.WriteString(wrapFont(result, "string", fonts))
		// A plaintext string literal.
		case runes.CurrentRune() == '`':
			result, ok := runes.ReadPlaintextString()
			result = "`" + result
			if ok {
				result = result + "`"
			}
			out.WriteString(wrapFont(result, "string", fonts))
		// A rune literal.
		case runes.CurrentRune() == '\'':
			result, hasSingleQuote, hasLengthOne := runes.ReadRuneLiteral()
			result = "'" + result
			if hasSingleQuote {
				result = result + "'"
			}
			if !hasLengthOne {
				out.WriteString(text.ErrorFont(result))
			} else {
				out.WriteString(wrapFont(result, "string", fonts))
			}
		// A comment.
		case runes.CurrentRune() == '/' && runes.PeekRune() == '/':
			result := "/" + runes.ReadComment()
			out.WriteString(wrapFont(result, "comment", fonts))
		// A nondecimal integer literal.
		case runes.CurrentRune() == '0' && nondecimalIndicators.Contains(runes.PeekRune()):
			result := ""
			indicator := runes.PeekRune()
			switch indicator {
			case 'b', 'B':
				result = runes.ReadBinaryNumber()
			case 'o', 'O':
				result = runes.ReadOctalNumber()
			case 'x', 'X':
				result = runes.ReadHexNumber()
			}
			out.WriteString(wrapFont("0"+string(indicator)+result, "number", fonts))
		// It could be a perfectly ordinary number.
		case lexer.IsDigit(runes.CurrentRune()):
			result := runes.ReadNumber()
			out.WriteString(wrapFont(result, "number", fonts))
		// It could be an identifier (as the lexer defines that term, not the parser, e.g. it includes `else`.)
		case lexer.IsLegalStart(runes.CurrentRune()):
			result := runes.ReadIdentifier()
			switch {
			case control.Contains(result):
				out.WriteString(wrapFont(result, "control", fonts))
			case reserved.Contains(result):
				out.WriteString(wrapFont(result, "reserved", fonts))
			case illegalInRepl.Contains(result):
				out.WriteString(text.ErrorFont(result))
			case enumlike.Match([]byte(result)):
				out.WriteString(wrapFont(result, "constant", fonts))
			case nativeTypes.Contains(result) || typelike.Match([]byte(result)):
				for runes.PeekRune() == '?' || runes.PeekRune() == '!' {
					runes.Next()
					result = result + string(runes.CurrentRune())
				}
				out.WriteString(wrapFont(result, "type", fonts))
			default:
				out.WriteString(result)
			}
		// Or, by default (e.g. if it's whitespace)
		default:
			out.WriteRune(runes.CurrentRune())
		}
		runes.Next()
	}
	return out.String()
}

func (cp *Compiler) GetMarkdowner(leftMargin string, rightMargin int, fonts *values.Map) func(string) string {
	hl := func(s string) string {
		return cp.Highlight([]rune(s), fonts)
	}
	md := text.NewMarkdown(leftMargin, rightMargin, hl)
	return func(s string) string {
		return md.Render([]string{s})
	}
}

func wrapFont(body, tokenIs string, fonts *values.Map) string {
	var out bytes.Buffer
	out.WriteString(getFont(tokenIs, fonts))
	out.WriteString(body)
	out.WriteString(text.RESET)
	return out.String()
}

func getFont(tokenIs string, fonts *values.Map) string {
	if fonts == nil {
		return ""
	}
	pfFont, colorExists := fonts.Get(values.Value{values.STRING, tokenIs})
	if !colorExists {
		return ""
	}
	return fontFromFontValue(pfFont)
}

func getFontForBrackets(depth int, fonts *values.Map) string {
	if fonts == nil {
		return ""
	}
	brackets, bracketsExist := fonts.Get(values.Value{values.STRING, "brackets"})
	if !bracketsExist {
		return ""
	}
	bracketList := brackets.V.(vector.Vector)
	font, _ := bracketList.Index(depth % bracketList.Len())
	pfFont := font.(values.Value)
	return fontFromFontValue(pfFont)
}

func fontFromFontValue(pfFont values.Value) string {
	fontValues := pfFont.V.([]values.Value)
	result := "\033[38;2"
	for i := range 3 {
		result = result + ";" + strconv.Itoa(fontValues[i].V.(int))
	}
	result = result + "m"
	switch fontValues[3].V.(int) {
	case 1:
		result = result + text.BOLD
	case 2:
		result = result + text.ITALIC
	case 3:
		result = result + text.UNDERLINE
	}
	return result
}
