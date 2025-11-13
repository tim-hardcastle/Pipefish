package vm

// Used by the vm to describe Pipefish values and types, and also to describe bytecode for debugging purposes.

import (
	"context"

	"github.com/tim-hardcastle/Pipefish/source/err"
	"github.com/tim-hardcastle/Pipefish/source/text"
	"github.com/tim-hardcastle/Pipefish/source/token"
	"github.com/tim-hardcastle/Pipefish/source/values"

	"crypto/aes"
	"crypto/cipher"
	"crypto/rand"
	"crypto/sha256"
	"fmt"
	"strconv"
	"strings"

	"golang.org/x/crypto/pbkdf2"
	"src.elv.sh/pkg/persistent/vector"
)

// The maximum value of a `uint32`. Used as a dummy/sentinel value when `0` is not appropriate.
const DUMMY = 4294967295

func (vm *Vm) DescribeCode(loc uint32) string {
	prefix := "@" + strconv.Itoa(int(loc)) + " : "
	spaces := strings.Repeat(" ", 8-len(prefix))
	return spaces + prefix + describe(vm.Code[loc])
}

func (vm *Vm) DescribeOperandValues(addr uint32) string {
	op := vm.Code[addr]
	operands := op.Args
	operandTypes := OPERANDS[op.Opcode].or
	result := ""
	for i, operandType := range operandTypes {
		if operandType == mem {
			result = result + text.BULLET + "m" + strconv.Itoa(int(operands[i])) + " = " + vm.DescribeTypeAndValue(vm.Mem[operands[i]], LITERAL) + "\n"
		}
	}
	return text.PURPLE + result + text.RESET
}

func (vm *Vm) DescribeTypeAndValue(v values.Value, flavor descriptionFlavor) string {
	return vm.DescribeType(v.T, flavor) + "::" + vm.toString(v, flavor)
}

func (vm *Vm) DescribeType(t values.ValueType, flavor descriptionFlavor) string {
	return vm.ConcreteTypeInfo[t].GetName(flavor)
}

type descriptionFlavor int

const (
	DEFAULT descriptionFlavor = iota
	LITERAL
	SECRET
)

// TODO --- LITERAL should produce an error on being fed something unserializable.
// Add an EXPLICIT option for LITERAL with fallback to STRING.
func (vm *Vm) toString(v values.Value, flavor descriptionFlavor) string {
	typeInfo := vm.ConcreteTypeInfo[v.T]
	if typeInfo.IsStruct() {
		var buf strings.Builder
		buf.WriteString(typeInfo.GetName(flavor))
		buf.WriteString(" with (")
		var sep string
		vals := v.V.([]values.Value)
		for i, lb := range typeInfo.(StructType).LabelNumbers { // We iterate by the label and not by the value so that we can have hidden fields in the structs, as we do for efficiency when making a compilable snippet.
			fmt.Fprintf(&buf, "%s%s::%s", sep, vm.Labels[lb], vm.StringifyValue(vals[i], flavor))
			sep = ", "
		}
		buf.WriteByte(')')
		return buf.String()
	}
	if typeInfo.IsEnum() {
		var buf strings.Builder
		if flavor == LITERAL || flavor == SECRET {
			buf.WriteString(typeInfo.(EnumType).Path)
		}
		buf.WriteString(typeInfo.(EnumType).ElementNames[v.V.(int)])
		return buf.String()

	}
	if typeInfo.IsClone() {
		if typeInfo.(CloneType).Parent == values.SNIPPET {
			var buf strings.Builder
			buf.WriteString(typeInfo.GetName(flavor))
			buf.WriteByte('(')
			var sep string
			for _, k := range v.V.(values.Snippet).Data {
				fmt.Fprintf(&buf, "%s%s", sep, vm.StringifyValue(k, LITERAL))
				sep = ", "
			}
			buf.WriteByte(')')
			return buf.String()
		}
		var buf strings.Builder
		buf.WriteString(typeInfo.GetName(flavor))
		if typeInfo.(CloneType).Parent != values.LIST {
			buf.WriteByte('(')
		}
		buf.WriteString(vm.StringifyValue(values.Value{typeInfo.(CloneType).Parent, v.V}, flavor))
		if typeInfo.(CloneType).Parent != values.LIST {
			buf.WriteByte(')')
		}
		return buf.String()
	}
	switch v.T {
	case values.BLING:
		return v.V.(string)
	case values.BOOL:
		if v.V.(bool) {
			return "true"
		} else {
			return "false"
		}
	case values.ERROR:
		if v.V == nil { // Can happen when we're running a test and the compiler throws an error. Really should always be the special compiler error type --- TODO.
			return "nil error"
		}
		ob := v.V.(*err.Error)
		if ob.ErrorId != "vm/user" {
			ob = err.CreateErr(ob.ErrorId, ob.Token, ob.Args...)
		}
		return text.Pretty(text.RT_ERROR+ob.Message+text.DescribePos(ob.Token)+".", 0, 80)
	case values.FLOAT:
		return strconv.FormatFloat(v.V.(float64), 'f', 8, 64)
	case values.FUNC:
		return "lambda function"
	case values.INT:
		return strconv.Itoa(v.V.(int))
	case values.INT_ARRAY:
		var buf strings.Builder
		buf.WriteString("INT_ARRAY!(")
		var sep string
		for _, v := range v.V.([]uint32) {
			fmt.Fprintf(&buf, "%s%v", sep, v)
			sep = ", "
		}
		buf.WriteByte(')')
		return buf.String()
	case values.LABEL:
		return vm.Labels[v.V.(int)]
	case values.LIST:
		var buf strings.Builder
		buf.WriteString("[")
		var sep string
		for i := 0; i < v.V.(vector.Vector).Len(); i++ {
			el, _ := v.V.(vector.Vector).Index(i)
			fmt.Fprintf(&buf, "%s%s", sep, vm.StringifyValue(el.(values.Value), flavor))
			sep = ", "
		}
		buf.WriteByte(']')
		return buf.String()
	case values.MAP:
		var buf strings.Builder
		buf.WriteString("map(")
		var sep string
		(v.V.(*values.Map)).Range(func(k, v values.Value) {
			fmt.Fprintf(&buf, "%s%v::%v", sep, vm.StringifyValue(k, flavor), vm.StringifyValue(v, flavor))
			sep = ", "
		})
		buf.WriteByte(')')
		return buf.String()
	case values.NULL:
		return "NULL"
	case values.PAIR:
		vals := v.V.([]values.Value)
		return vm.StringifyValue(vals[0], flavor) + "::" + vm.StringifyValue(vals[1], flavor)
	case values.RUNE:
		if flavor == DEFAULT {
			return fmt.Sprintf("%c", v.V.(rune))
		}
		if flavor == LITERAL || flavor == SECRET {
			return fmt.Sprintf("'%c'", v.V.(rune))
		}
	case values.SECRET:
		if flavor == SECRET {
			var buf strings.Builder
			buf.WriteString("secret(")
			buf.WriteString(vm.toString(v.V.(Secret).secret, SECRET))
			buf.WriteString(")")
			return buf.String()
		} else {
			return "secret(?)"
		}
	case values.SET:
		var buf strings.Builder
		buf.WriteString("set(")
		var sep string
		v.V.(values.Set).Range(func(k values.Value) {
			fmt.Fprintf(&buf, "%s%s", sep, vm.StringifyValue(k, flavor))
			sep = ", "
		})
		buf.WriteByte(')')
		return buf.String()
	case values.SNIPPET:
		var buf strings.Builder
		buf.WriteString("snippet(")
		var sep string
		for _, k := range v.V.(values.Snippet).Data {
			fmt.Fprintf(&buf, "%s%s", sep, vm.StringifyValue(k, LITERAL))
			sep = ", "
		}
		buf.WriteByte(')')
		return buf.String()
	case values.STRING:
		if flavor == DEFAULT {
			return v.V.(string)
		}
		if flavor == LITERAL || flavor == SECRET {
			return strconv.Quote(v.V.(string))
		}
	case values.SUCCESSFUL_VALUE:
		if flavor == DEFAULT {
			return text.GREEN + "OK" + text.RESET
		}
		if flavor == LITERAL || flavor == SECRET {
			return "OK"
		}

	case values.THUNK:
		if v.V == nil {
			return "nil"
		}
		return "m" + strconv.Itoa(int(v.V.(values.ThunkValue).MLoc)) + ", @" + strconv.Itoa(int(v.V.(values.ThunkValue).CAddr))
	case values.TUPLE:
		result := make([]string, len(v.V.([]values.Value)))
		for i, v := range v.V.([]values.Value) {
			result[i] = vm.StringifyValue(v, flavor)
		}
		prefix := "("
		if len(result) == 1 {
			prefix = "tuple("
		}
		return prefix + strings.Join(result, ", ") + ")"
	case values.TYPE:
		return vm.DescribeAbstractType(v.V.(values.AbstractType), flavor)
	case values.UNDEFINED_TYPE:
		return "UNDEFINED VALUE!"
	case values.UNSATISFIED_CONDITIONAL:
		return "UNSATIFIED CONDITIONAL!"
	case values.ITERATOR:
		return "ITERATOR"
	case values.REF:
		return "REFERENCE VARIABLE"
	}
	println("Undescribable value", v.T)
	panic("can't describe value")
}

func (vm *Vm) DescribeAbstractType(aT values.AbstractType, flavor descriptionFlavor) string {
	result := []string{}
	T := aT
	if len(aT.Types) == 0 {
		return "empty"
	}
	if len(aT.Types) == 1 {
		return vm.DescribeType(aT.Types[0], flavor)
	}
	// First we greedily remove the abstract types.
	for {
		var biggestType int
		var sizeOfBiggestType int
		for i, pair := range vm.AbstractTypes {
			if pair.AT.IsSubtypeOf(T) {
				if pair.AT.Len() > sizeOfBiggestType {
					biggestType = i
					sizeOfBiggestType = pair.AT.Len()
				}
			}
		}
		if sizeOfBiggestType == 0 {
			break
		}
		if flavor == LITERAL || flavor == SECRET {
			result = append(result, vm.AbstractTypes[biggestType].Path+vm.AbstractTypes[biggestType].Name)
		} else {
			result = append(result, vm.AbstractTypes[biggestType].Name)
		}
		T = T.Without(vm.AbstractTypes[biggestType].AT)
	}

	// We then add on all the other types except null, which we will represent with a ?
	nullFlag := false
	for _, t := range T.Types {
		if t == values.NULL {
			nullFlag = true
		} else {
			result = append(result, vm.DescribeType(t, flavor))
		}
	}
	// Deal with null
	if nullFlag {
		if len(result) > 0 {
			result[len(result)-1] = result[len(result)-1] + "?"
		} else {
			result = []string{"null"}
		}
	}
	return strings.Join(result, "/")
}

func (vm *Vm) DefaultDescription(v values.Value) string {
	return vm.toString(v, DEFAULT)
}

func (vm *Vm) Literal(v values.Value) string {
	return vm.toString(v, LITERAL)
}

func (vm *Vm) StringifyValue(v values.Value, flavor descriptionFlavor) string {
	if flavor == LITERAL {
		return vm.toString(v, LITERAL)
	}
	if v.T == values.TUPLE || v.T == values.SUCCESSFUL_VALUE || v.T == values.ERROR {
		return vm.toString(v, flavor)
	}
	vm.Mem[vm.StringifyLoReg] = v
	vm.run(vm.StringifyCallTo, context.Background()) // TODO --- does this matter?
	return vm.Mem[vm.StringifyOutReg].V.(string)
}

// To make an error, we need the error code, the number of the token to be attached to it,
// the memory locations of the values that caused the problem, to be appended to the Values
// field of the Error; and anything else we may want to package up with it, to go in the
// Args field. The memory locations and miscellania are passed indifferently to the args field of the
// makeError function, and it is assumed that anything of type uint32 represents a location.
func (vm *Vm) makeError(errCode string, tokenOrdinal uint32, args ...any) values.Value {
	tok := vm.Tokens[tokenOrdinal]
	result := &err.Error{ErrorId: errCode, Token: tok, Trace: []*token.Token{vm.Tokens[tokenOrdinal]}}
	for _, arg := range args {
		switch arg := arg.(type) {
		case uint32:
			result.Values = append(result.Values, vm.Mem[arg])
		case values.Value:
			result.Values = append(result.Values, arg)
		default:
			result.Args = append(result.Args, arg)
		}
	}
	errorCreator, ok := err.ErrorCreatorMap[errCode]
	if !ok {
		return values.Value{values.ERROR, err.CreateErr("err/misdirect", tok, errCode)}
	}
	result.Message = errorCreator.Message(tok, args...)
	return values.Value{values.ERROR, result}
}

// Actually this can dump any Map but why would you want to?
func (vm *Vm) DumpStore(store values.Map, password string) string {
	var plaintext strings.Builder
	plaintext.WriteString("PLAINTEXT\n")
	for _, pair := range store.AsSlice() {
		plaintext.WriteString(vm.toString(pair.Key, SECRET))
		plaintext.WriteString("::")
		plaintext.WriteString(vm.toString(pair.Val, SECRET))
		plaintext.WriteString("\n")
	}
	if password == "" {
		return plaintext.String()
	}
	plaintext.WriteString(strings.Repeat(" ", aes.BlockSize-plaintext.Len()%aes.BlockSize))
	salt := make([]byte, 32)
	rand.Read(salt)
	key := pbkdf2.Key([]byte(password), salt, 65536, 32, sha256.New) // sha256 has nothing to do with it but the API is stupid.
	block, err := aes.NewCipher(key)
	if err != nil {
		panic(err)
	}
	// We also use salt for the AES cypher (the salt being called `iv` below because the people
	// I copied the code from have a sense of humor).
	ciphertext := make([]byte, aes.BlockSize+plaintext.Len())
	iv := ciphertext[:aes.BlockSize]
	rand.Read(iv)
	mode := cipher.NewCBCEncrypter(block, iv)
	mode.CryptBlocks(ciphertext[aes.BlockSize:], []byte(plaintext.String()))
	return string(salt) + string(ciphertext)
}
