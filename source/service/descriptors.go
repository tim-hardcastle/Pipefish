package service

// Used by the vm to describe Pipefish values and types, and also to describe bytecode for debugging purposes.

import (
	"pipefish/source/report"
	"pipefish/source/text"
	"pipefish/source/token"
	"pipefish/source/values"

	"fmt"
	"strconv"
	"strings"

	"src.elv.sh/pkg/persistent/vector"
)

func (vm *Vm) DescribeCode(loc uint32) string {
	prefix := "@" + strconv.Itoa(int(loc)) + " : "
	spaces := strings.Repeat(" ", 8-len(prefix))
	return spaces + prefix + describe(vm.Code[loc])
}

func (vm *Vm) DescribeType(t values.ValueType) string {
	return vm.concreteTypeNames[t]
}

func (vm *Vm) Describe(v values.Value) string {
	if v.T >= vm.Ub_enums { // We have a struct.
		var buf strings.Builder
		buf.WriteString(vm.concreteTypeNames[v.T])
		buf.WriteString(" with (")
		var sep string
		labels := vm.StructLabels[v.T-vm.Ub_enums]
		vals := v.V.([]values.Value)
		for i, lb := range labels { // We iterate by the label and not by the value so that we can have hidden fields in the structs, as we do for efficiency when making a compilable snippet.
			fmt.Fprintf(&buf, "%s%s::%s", sep, vm.Labels[lb], vm.Describe(vals[i]))
			sep = ", "
		}
		buf.WriteByte(')')
		return buf.String()
	}
	if values.LB_ENUMS <= v.T && v.T < values.ValueType(vm.Ub_enums) { // We have an enum.
		return vm.Enums[v.T-values.LB_ENUMS][v.V.(int)]
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
	case values.BREAK:
		return "break"
	case values.ERROR:
		ob := v.V.(*report.Error)
		if ob.ErrorId != "eval/user" {
			ob = report.CreateErr(ob.ErrorId, ob.Token, ob.Args...)
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
			fmt.Fprintf(&buf, "%s%s", sep, vm.Describe(el.(values.Value)))
			sep = ", "
		}
		buf.WriteByte(']')
		return buf.String()
	case values.MAP:
		var buf strings.Builder
		buf.WriteString("map(")
		var sep string
		(v.V.(*values.Map)).Range(func(k, v values.Value) {
			fmt.Fprintf(&buf, "%s%v::%v", sep, vm.Describe(k), vm.Describe(v))
			sep = ", "
		})
		buf.WriteByte(')')
		return buf.String()
	case values.NULL:
		return "NULL"
	case values.PAIR:
		vals := v.V.([]values.Value)
		return vm.Describe(vals[0]) + "::" + vm.Describe(vals[1])
	case values.SET:
		var buf strings.Builder
		buf.WriteString("set(")
		var sep string
		v.V.(values.Set).Range(func(k values.Value) {
			fmt.Fprintf(&buf, "%s%s", sep, vm.Describe(k))
			sep = ", "
		})
		buf.WriteByte(')')
		return buf.String()
	case values.STRING:
		return v.V.(string)
	case values.SUCCESSFUL_VALUE:
		return text.GREEN + "OK" + text.RESET
	case values.THUNK:
		return "THUNK!"
	case values.TUPLE:
		result := make([]string, len(v.V.([]values.Value)))
		for i, v := range v.V.([]values.Value) {
			result[i] = vm.Describe(v)
		}
		prefix := "("
		if len(result) == 1 {
			prefix = "tuple("
		}
		return prefix + strings.Join(result, ", ") + ")"
	case values.TYPE:
		return vm.DescribeAbstractType(v.V.(values.AbstractType))
	case values.UNDEFINED_VALUE:
		return "UNDEFINED VALUE!"
	case values.UNSAT:
		return "UNSATIFIED CONDITIONAL!"
	}
	println("Undescribable value", v.T)
	panic("can't describe value")
}

func (vm *Vm) DescribeAbstractType(aT values.AbstractType) string {
	result := []string{}
	T := aT
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
		result = append(result, vm.AbstractTypes[biggestType].Name)
		T = T.Without(vm.AbstractTypes[biggestType].AT)
	}

	// We then add on all the other types except null, which we will represent with a ?
	nullFlag := false
	for _, t := range T.Types {
		if t == values.NULL {
			nullFlag = true
		} else {
			if t == values.STRING && T.Varchar < DUMMY {
				result = append(result, "varchar("+strconv.Itoa(int(T.Varchar))+")")
			} else {
				result = append(result, vm.DescribeType(t))
			}
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

func (vm *Vm) Literal(v values.Value) string {
	switch v.T {
	case values.STRING:
		return "\"" + v.V.(string) + "\""
	case values.SUCCESSFUL_VALUE:
		return "OK"
	default:
		return vm.Describe(v) // TODO: this won't work, you need a single recursive function with being literal as a parameter.
	}
}

// To make an error, we need the error code, the number of the token to be attached to it,
// the memory locations of the values that caused the problem, to be appended to the Values
// field of the Error; and anything else we may want to package up with it, to go in the
// Args field. The memory locations and miscellania are passed indifferently to the args field of the
// makeError function, and it is assumed that anything of type uint32 represents a location.
func (vm *Vm) makeError(errCode string, tokenOrdinal uint32, args ...any) values.Value {
	result := &report.Error{ErrorId: errCode, Token: vm.Tokens[tokenOrdinal], Trace: []*token.Token{vm.Tokens[tokenOrdinal]}}
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
	return values.Value{values.ERROR, result}
}
