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

var OPCODE_LIST []func(vm *Vm, args []uint32)

var CONSTANTS = []Value{FALSE, TRUE, U_OBJ}

func blankVm() *Vm {
	newVm := &Vm{mem: CONSTANTS}
	// Cross-reference with consts in values.go.
	newVm.typeNames = []string{"thunk", "created local constant", "tuple", "error", "unsat", "null",
		"int", "bool", "string", "float64"}
	return newVm
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
		return strings.Join(tList, "/")
	case finiteTupleType:
		tList := []string{}
		for _, v := range t {
			tList = append(tList, vm.describeType(v))
		}
		return "tuple with ()" + strings.Join(tList, ", ") + ")"
	case typedTupleType:
		return "tuple of (" + vm.describeType(t.t) + ")"
	}
	panic("unimplemented type")
}

const SHOW_RUN = true

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
		case jmp:
			loc = args[0]
			continue
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
		case apnT:
			sliceIs := vm.mem[args[0]].V.([]Value)
			if vm.mem[args[1]].T == TUPLE {
				sliceIs = append(sliceIs, vm.mem[args[1]].V.([]Value)...)
			} else {
				sliceIs = append(sliceIs, vm.mem[args[1]])
			}
		case idxT:
			vm.mem[args[0]] = vm.mem[args[1]].V.([]Value)[args[2]]
		case equi:
			vm.mem[args[0]] = Value{T: BOOL, V: vm.mem[args[1]].V.(int) == vm.mem[args[2]].V.(int)}
		case equs:
			vm.mem[args[0]] = Value{T: BOOL, V: vm.mem[args[1]].V.(string) == vm.mem[args[2]].V.(string)}
		case equb:
			vm.mem[args[0]] = Value{T: BOOL, V: vm.mem[args[1]].V.(bool) == vm.mem[args[2]].V.(bool)}
		case equf:
			vm.mem[args[0]] = Value{T: BOOL, V: vm.mem[args[1]].V.(float64) == vm.mem[args[2]].V.(float64)}
		case notb:
			vm.mem[args[0]] = Value{T: BOOL, V: !vm.mem[args[1]].V.(bool)}
		case orb:
			vm.mem[args[0]] = Value{T: BOOL, V: (vm.mem[args[1]].V.(bool) || vm.mem[args[2]].V.(bool))}
		case andb:
			vm.mem[args[0]] = Value{T: BOOL, V: (vm.mem[args[1]].V.(bool) && vm.mem[args[2]].V.(bool))}
		case qtyp:
			if vm.mem[args[0]].T == simpleType(args[1]) {
				loc = loc + 1
			} else {
				loc = args[2]
			}
			continue
		case qsnQ:
			if vm.mem[args[0]].T >= NULL {
				loc = loc + 1
			} else {
				loc = args[1]
			}
			continue
		case qsng:
			if vm.mem[args[0]].T >= INT {
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
		case qlnT:
			if len(vm.mem[args[0]].V.([]Value)) == int(args[1]) {
				loc = loc + 1
			} else {
				loc = args[2]
			}
		case thnk:
			vm.mem[args[0]].T = THUNK
			vm.mem[args[0]].V = args[1]
		case untk:
			if (vm.mem[args[0]].T) == THUNK {
				vm.callstack = append(vm.callstack, loc)
				loc = vm.mem[args[0]].V.(uint32)
				continue
			}
		case jsr:
			vm.callstack = append(vm.callstack, loc)
			loc = args[0]
			continue
		case ret:
			if len(vm.callstack) == 0 {
				break loop
			}
			loc = vm.callstack[len(vm.callstack)-1]
			vm.callstack = vm.callstack[0 : len(vm.callstack)-1]
		case halt:
			break loop
		case lens:
			vm.mem[args[0]] = Value{INT, len(vm.mem[args[1]].V.(string))}
		case addi:
			vm.mem[args[0]] = Value{INT, vm.mem[args[1]].V.(int) + vm.mem[args[2]].V.(int)}
		case ints:
			i, err := strconv.Atoi(vm.mem[args[1]].V.(string))
			if err != nil {
				vm.mem[args[0]] = Value{ERROR, DUMMY}
			} else {
				vm.mem[args[0]] = Value{INT, i}
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
