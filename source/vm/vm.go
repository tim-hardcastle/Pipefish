package vm

type Vm struct {
	mem       []Value
	con       []Value
	code      []*operation
	callstack []uint32
}

func blankVm() *Vm {
	return &Vm{mem: []Value{{}}}
}

func (vm *Vm) Run(loc uint32) {
	print("\nRunning:\n\n")
loop:
	for {
		println(describe(vm.code[loc]))
		args := vm.code[loc].args
		switch vm.code[loc].opcode {
		case jmp:
			loc = args[0]
			continue
		case asgnc:
			vm.mem[args[0]] = vm.con[args[1]]
		case asgnm:
			vm.mem[args[0]] = vm.mem[args[1]]
		case qtype:
			if vm.mem[args[0]].T != args[1] {
				loc = loc + 2
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
		default:
			panic("Unhandled opcode!")
		}
		loc++
	}
	print("\nOutput : " + vm.mem[0].describe() + "\n\n")
}
