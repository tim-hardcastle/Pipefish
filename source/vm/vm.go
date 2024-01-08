package vm

import (
	"strconv"
	"strings"
)

type Vm struct {
	// Temporary state
	mem       []Value
	callstack []uint32
	code      []*operation

	// Permanent state

	ub_enums  uint32
	typeNames []string
	enums     [][]string
}

func (vm *Vm) memTop() uint32 {
	return uint32(len(vm.mem))
}

func (vm *Vm) that() uint32 {
	return uint32(len(vm.mem) - 1)
}

func (vm *Vm) codeTop() uint32 {
	return uint32(len(vm.code))
}

func (vm *Vm) next() uint32 {
	return uint32(len(vm.code))
}

var OPCODE_LIST []func(vm *Vm, args []uint32)

var CONSTANTS = []Value{FALSE, TRUE, U_OBJ}

func blankVm() *Vm {
	newVm := &Vm{mem: CONSTANTS}
	// Cross-reference with consts in values.go. TODO --- find something less stupidly brittle to do instead.
	newVm.typeNames = []string{"thunk", "created local constant", "tuple", "error", "unsat", "null",
		"int", "bool", "string", "float64", "type"}
	return newVm
}

func (vm *Vm) Run(loc uint32) {
	if SHOW_RUN {
		print("\nRunning:\n\n")
	}
loop:
	for {
		if SHOW_RUN {
			println(vm.describeCode(loc))
		}
		args := vm.code[loc].args
		switch vm.code[loc].opcode {
		case addf:
			vm.mem[args[0]] = Value{FLOAT, vm.mem[args[1]].V.(float64) + vm.mem[args[2]].V.(float64)}
		case addi:
			vm.mem[args[0]] = Value{INT, vm.mem[args[1]].V.(int) + vm.mem[args[2]].V.(int)}
		case adds:
			vm.mem[args[0]] = Value{STRING, vm.mem[args[1]].V.(string) + vm.mem[args[2]].V.(string)}
		case andb:
			vm.mem[args[0]] = Value{BOOL, vm.mem[args[1]].V.(bool) && vm.mem[args[2]].V.(bool)}
		case asgm:
			vm.mem[args[0]] = vm.mem[args[1]]
		case call:
			offset := args[1]
			for i := args[1]; i < args[2]; i++ {
				vm.mem[i] = vm.mem[args[3+i-offset]]
			}
			vm.callstack = append(vm.callstack, loc)
			loc = args[0]
			continue
		case cc11:
			vm.mem[args[0]] = Value{TUPLE, []Value{vm.mem[args[1]], vm.mem[args[2]]}}
		case cc1T:
			vm.mem[args[0]] = Value{TUPLE, append([]Value{vm.mem[args[1]]}, vm.mem[args[2]].V.([]Value)...)}
		case ccT1:
			vm.mem[args[0]] = Value{TUPLE, append(vm.mem[args[1]].V.([]Value), vm.mem[args[2]])}
		case ccTT:
			vm.mem[args[0]] = Value{TUPLE, append(vm.mem[args[1]].V.([]Value), vm.mem[args[2]])}
		case ccxx:
			if vm.mem[args[1]].T == TUPLE {
				if vm.mem[args[2]].T == TUPLE {
					vm.mem[args[0]] = Value{TUPLE, append(vm.mem[args[1]].V.([]Value), vm.mem[args[2]])}
				} else {
					vm.mem[args[0]] = Value{TUPLE, append(vm.mem[args[1]].V.([]Value), vm.mem[args[2]])}
				}
			} else {
				if vm.mem[args[2]].T == TUPLE {
					vm.mem[args[0]] = Value{TUPLE, append([]Value{vm.mem[args[1]]}, vm.mem[args[2]].V.([]Value)...)}
				} else {
					vm.mem[args[0]] = Value{TUPLE, []Value{vm.mem[args[1]], vm.mem[args[2]]}}
				}
			}
		case cv1T:
			vm.mem[args[0]] = Value{TUPLE, []Value{vm.mem[args[1]]}}
		case divf:
			if vm.mem[args[2]].V.(float64) == 0 {
				vm.mem[args[0]] = Value{ERROR, DUMMY}
			} else {
				vm.mem[args[0]] = Value{FLOAT, vm.mem[args[1]].V.(float64) / vm.mem[args[2]].V.(float64)}
			}
		case divi:
			if vm.mem[args[2]].V.(int) == 0 {
				vm.mem[args[0]] = Value{ERROR, "Division by zero"}
			} else {
				vm.mem[args[0]] = Value{INT, vm.mem[args[1]].V.(int) / vm.mem[args[2]].V.(int)}
			}
		case equb:
			vm.mem[args[0]] = Value{BOOL, vm.mem[args[1]].V.(bool) == vm.mem[args[2]].V.(bool)}
		case equf:
			vm.mem[args[0]] = Value{BOOL, vm.mem[args[1]].V.(float64) == vm.mem[args[2]].V.(float64)}
		case equi:
			vm.mem[args[0]] = Value{BOOL, vm.mem[args[1]].V.(int) == vm.mem[args[2]].V.(int)}
		case equs:
			vm.mem[args[0]] = Value{BOOL, vm.mem[args[1]].V.(string) == vm.mem[args[2]].V.(string)}
		case flti:
			vm.mem[args[0]] = Value{FLOAT, float64(vm.mem[args[1]].V.(int))}
		case flts:
			i, err := strconv.ParseFloat(vm.mem[args[1]].V.(string), 64)
			if err != nil {
				vm.mem[args[0]] = Value{ERROR, DUMMY}
			} else {
				vm.mem[args[0]] = Value{FLOAT, i}
			}
		case gtef:
			vm.mem[args[0]] = Value{BOOL, vm.mem[args[1]].V.(float64) >= vm.mem[args[2]].V.(float64)}
		case gtei:
			vm.mem[args[0]] = Value{BOOL, vm.mem[args[1]].V.(int) >= vm.mem[args[2]].V.(int)}
		case gthf:
			vm.mem[args[0]] = Value{BOOL, vm.mem[args[1]].V.(float64) > vm.mem[args[2]].V.(float64)}
		case gthi:
			vm.mem[args[0]] = Value{BOOL, vm.mem[args[1]].V.(int) > vm.mem[args[2]].V.(int)}
		case halt:
			break loop
		case intf:
			vm.mem[args[0]] = Value{INT, int(vm.mem[args[1]].V.(float64))}
		case ints:
			i, err := strconv.Atoi(vm.mem[args[1]].V.(string))
			if err != nil {
				vm.mem[args[0]] = Value{ERROR, DUMMY}
			} else {
				vm.mem[args[0]] = Value{INT, i}
			}
		case idxT:
			vm.mem[args[0]] = vm.mem[args[1]].V.([]Value)[args[2]]
		case jmp:
			loc = args[0]
			continue
		case jsr:
			vm.callstack = append(vm.callstack, loc)
			loc = args[0]
			continue
		case lens:
			vm.mem[args[0]] = Value{INT, len(vm.mem[args[1]].V.(string))}
		case litx:
			vm.mem[args[0]] = Value{STRING, vm.literal(vm.mem[args[1]])}
		case modi:
			if vm.mem[args[2]].V.(int) == 0 {
				vm.mem[args[0]] = Value{ERROR, DUMMY}
			} else {
				vm.mem[args[0]] = Value{INT, vm.mem[args[1]].V.(int) % vm.mem[args[2]].V.(int)}
			}
		case mulf:
			vm.mem[args[0]] = Value{FLOAT, vm.mem[args[1]].V.(float64) * vm.mem[args[2]].V.(float64)}
		case muli:
			vm.mem[args[0]] = Value{INT, vm.mem[args[1]].V.(int) * vm.mem[args[2]].V.(int)}
		case negf:
			vm.mem[args[0]] = Value{FLOAT, -vm.mem[args[1]].V.(float64)}
		case negi:
			vm.mem[args[0]] = Value{INT, -vm.mem[args[1]].V.(int)}
		case notb:
			vm.mem[args[0]] = Value{BOOL, !vm.mem[args[1]].V.(bool)}
		case orb:
			vm.mem[args[0]] = Value{BOOL, (vm.mem[args[1]].V.(bool) || vm.mem[args[2]].V.(bool))}
		case qlnT:
			if len(vm.mem[args[0]].V.([]Value)) == int(args[1]) {
				loc = loc + 1
			} else {
				loc = args[2]
			}
		case qsng:
			if vm.mem[args[0]].T >= INT {
				loc = loc + 1
			} else {
				loc = args[1]
			}
			continue
		case qsnQ:
			if vm.mem[args[0]].T >= NULL {
				loc = loc + 1
			} else {
				loc = args[1]
			}
			continue
		case qtru:
			if vm.mem[args[0]].V.(bool) {
				loc = loc + 1
			} else {
				loc = args[1]
			}
			continue
		case qtyp:
			if vm.mem[args[0]].T == simpleType(args[1]) {
				loc = loc + 1
			} else {
				loc = args[2]
			}
			continue
		case ret:
			if len(vm.callstack) == 0 {
				break loop
			}
			loc = vm.callstack[len(vm.callstack)-1]
			vm.callstack = vm.callstack[0 : len(vm.callstack)-1]
		case strx:
			vm.mem[args[0]] = Value{STRING, vm.describe(vm.mem[args[1]])}
		case subf:
			vm.mem[args[0]] = Value{FLOAT, vm.mem[args[1]].V.(float64) - vm.mem[args[2]].V.(float64)}
		case subi:
			vm.mem[args[0]] = Value{INT, vm.mem[args[1]].V.(int) - vm.mem[args[2]].V.(int)}
		case thnk:
			vm.mem[args[0]].T = THUNK
			vm.mem[args[0]].V = args[1]
		case typx:
			vm.mem[args[0]] = Value{TYPE, vm.mem[args[1]].T}
		case untk:
			if (vm.mem[args[0]].T) == THUNK {
				vm.callstack = append(vm.callstack, loc)
				loc = vm.mem[args[0]].V.(uint32)
				continue
			}
		default:
			panic("Unhandled opcode!")
		}
		loc++
	}
	if SHOW_RUN {
		println()
	}
}

func (vm *Vm) describeCode(loc uint32) string {
	prefix := "@" + strconv.Itoa(int(loc)) + " : "
	spaces := strings.Repeat(" ", 6-len(prefix))
	return spaces + prefix + describe(vm.code[loc])
}

func (vm *Vm) describeType(t typeScheme) string {
	if t == nil {
		return "nil"
	}
	switch t := t.(type) {
	case simpleType:
		return vm.typeNames[t]
	case alternateType:
		if len(t) == 0 {
			return "∅"
		}
		tList := []string{}
		for _, v := range t {
			tList = append(tList, vm.describeType(v))
		}
		return "[" + strings.Join(tList, "/") + "]"
	case finiteTupleType:
		tList := []string{}
		for _, v := range t {
			tList = append(tList, vm.describeType(v))
		}
		return "tuple with (" + strings.Join(tList, ", ") + ")"
	case typedTupleType:
		return "tuple of (" + vm.describeType(t.t) + ")"
	case blingType:
		return t.tag
	}
	panic("unimplemented type")
}

func (vm *Vm) describe(v Value) string {
	switch v.T {
	case INT:
		return strconv.Itoa(v.V.(int))
	case STRING:
		return v.V.(string)
	case TYPE:
		return vm.describeType(v.V.(simpleType))
	case BOOL:
		if v.V.(bool) {
			return "true"
		} else {
			return "false"
		}
	case FLOAT:
		return strconv.FormatFloat(v.V.(float64), 'f', 8, 64)
	case UNSAT:
		return "unsatisfied conditional"
	case NULL:
		return "NULL"
	case THUNK:
		return "thunk"
	case TUPLE:
		result := make([]string, len(v.V.([]Value)))
		for i, v := range v.V.([]Value) {
			result[i] = vm.describe(v)
		}
		prefix := "("
		if len(result) == 1 {
			prefix = "tuple("
		}
		return prefix + strings.Join(result, ", ") + ")"
	case ERROR:
		return "error"
	}

	panic("can't describe value")
}

func (vm *Vm) literal(v Value) string {
	switch v.T {
	case STRING:
		return "\"" + v.V.(string) + "\""
	default:
		return vm.describe(v)
	}
}
