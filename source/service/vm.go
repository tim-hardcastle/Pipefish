package service

import (
	"database/sql"
	"fmt"
	"os"
	"strconv"

	"src.elv.sh/pkg/persistent/vector"

	"pipefish/source/report"
	"pipefish/source/settings"
	"pipefish/source/text"
	"pipefish/source/token"
	"pipefish/source/values"
)

type Vm struct {
	// Temporary state: things we change at runtime.
	Mem       []values.Value
	Code      []*Operation
	callstack []uint32
	logging   bool

	// Permanent state: things established at compile time.

	StructResolve    StructResolver
	Ub_enums         values.ValueType // (Exclusive) upper bound of the enums. Everything above this is a struct.
	Ub_langs         values.ValueType // (Exclusive) upper bound of the languages. Everything above this is a contact.
	Lb_snippets      values.ValueType // (Inclusive) lower bound of the snippets.
	TypeNames        []string
	StructLabels     [][]int // Array from a struct to its label numbers.
	StructFields     [][]values.AbstractType
	Enums            [][]string // Array from the number of the enum to a list of the strings of its elements.
	Labels           []string   // Array from the number of a field label to its name.
	Tokens           []*token.Token
	LambdaFactories  []*LambdaFactory
	SnippetFactories []*SnippetFactory
	GoFns            []GoFn
	IoHandle         IoHandler
	Db               *sql.DB
	AbstractTypes    []values.NameAbstractTypePair
}

// This takes a snapshot of how much code, memory locations, etc, have been added to the respective lists at a given
// point. Then to roll back the vm, we can call the rollback function (below) on the state returned by getState.
type vmState struct {
	mem              int
	code             int
	tokens           int
	lambdaFactories  int
	snippetFactories int
}

// This captures the record.
func (vm *Vm) getState() vmState {
	return vmState{len(vm.Mem), len(vm.Code), len(vm.Tokens), len(vm.LambdaFactories), len(vm.SnippetFactories)}
}

// And this rolls back the machine.
func (vm *Vm) rollback(vms vmState) {
	vm.Code = vm.Code[:vms.code]
	vm.Mem = vm.Mem[:vms.mem]
	vm.Tokens = vm.Tokens[:vms.tokens]
	vm.LambdaFactories = vm.LambdaFactories[:vms.lambdaFactories]
	vm.SnippetFactories = vm.SnippetFactories[:vms.snippetFactories]
}

type GoFn struct {
	Code   func(args ...any) any
	GoToPf func(v any) (uint32, []any, bool)
	PfToGo func(T uint32, args []any) any
	Raw    []bool
}

// All the information we need to make a lambda at a particular point in the code.
type LambdaFactory struct {
	Model  *Lambda  // Copy this to make the lambda.
	ExtMem []uint32 // Then these are the location of the values we're closing over, so we copy them into the lambda.
	Size   uint32   // The size of the memory for a new VM.
}

// All the information we need to make a snippet at a particular point in the code.
type SnippetFactory struct {
	compiledSnippetKind compiledSnippetKind // An enum type saying whether it's uncompiled, a contact, SQL, or HTML.
	snippetType         values.ValueType    // The type of the snippet, adoy.
	sourceEnv           *Environment        // A flattened map of strings to memory locations of variables, used to compute the env field of the snippet at runtime.
	sourceString        string              // The plain text of the snippet before processing.
	bindle              *SnippetBindle      // Points to the structure defined below.
}

// A grouping of all the things a snippet from a given snippet factory have in common.
type SnippetBindle struct {
	varLocsStart    uint32   // Destination of the environment slice.
	codeLoc         uint32   // Where to find the code to compute the object string and the values.
	objectStringLoc uint32   // Where to find the object string.
	valueLocs       []uint32 // The values for SQL or HTML snippets.
}

// Then the SnippetData consists of that and the environment slice, which we can't insert into memory until call
// time because another snippet of the same type might be called first.
type SnippetData struct {
	EnvironmentSlice []values.Value // For similar reasons --- the values referenced may change between making and using the snippet --- we must pass these as values and not locations.
	Bindle           *SnippetBindle
}

type Lambda struct {
	Mc        *Vm
	ExtTop    uint32
	PrmTop    uint32
	Dest      uint32
	LocToCall uint32
	Captures  []values.Value
}

// These inhabit the first few memory addresses of the VM.
var CONSTANTS = []values.Value{values.UNDEF, values.FALSE, values.TRUE, values.U_OBJ, values.ONE, values.BLNG, values.OK, values.BRK, values.EMPTY}

func BlankVm(db *sql.DB) *Vm {
	newVm := &Vm{Mem: make([]values.Value, len(CONSTANTS)), Db: db, Ub_enums: values.LB_ENUMS,
		StructResolve: MapResolver{}, logging: true, IoHandle: MakeStandardIoHandler(os.Stdout)}
	// Cross-reference with consts in values.go. TODO --- find something less stupidly brittle to do instead.
	// Type names in upper case are things the user should never see.
	copy(newVm.Mem, CONSTANTS)
	newVm.TypeNames = []string{"UNDEFINED VALUE", "INT ARRAY", "SNIPPET DATA", "THUNK",
		"CREATED LOCAL CONSTANT", "COMPILE TIME ERROR", "BLING", "UNSATISFIED CONDITIONAL", "REFERENCE VARIABLE",
		"BREAK", "SUCCESSFUL VALUE", "tuple", "error", "null", "int", "bool", "string", "float64", "type", "func",
		"pair", "list", "map", "set", "label"}
	return newVm
}

func (vm *Vm) Run(loc uint32) {
	if settings.SHOW_RUNTIME {
		println()
	}
loop:
	for {
		if settings.SHOW_RUNTIME {
			println(text.GREEN + "    " + vm.DescribeCode(loc) + text.RESET)
		}
		args := vm.Code[loc].Args
	Switch:
		switch vm.Code[loc].Opcode {
		case Addf:
			vm.Mem[args[0]] = values.Value{values.FLOAT, vm.Mem[args[1]].V.(float64) + vm.Mem[args[2]].V.(float64)}
		case Addi:
			vm.Mem[args[0]] = values.Value{values.INT, vm.Mem[args[1]].V.(int) + vm.Mem[args[2]].V.(int)}
		case AddL:
			result := vm.Mem[args[1]].V.(vector.Vector)
			rhs := vm.Mem[args[2]].V.(vector.Vector)
			for i := 0; ; i++ {
				el, ok := rhs.Index(i)
				if !ok {
					break
				}
				result = result.Conj(el)
			}
			vm.Mem[args[0]] = values.Value{values.LIST, result}
		case AddS:
			result := vm.Mem[args[1]].V.(values.Set)
			result.Union(vm.Mem[args[2]].V.(values.Set))
			vm.Mem[args[0]] = values.Value{values.SET, result}
		case Adds:
			vm.Mem[args[0]] = values.Value{values.STRING, vm.Mem[args[1]].V.(string) + vm.Mem[args[2]].V.(string)}
		case Adtk:
			vm.Mem[args[0]] = vm.Mem[args[1]]
			vm.Mem[args[0]].V.(*report.Error).AddToTrace(vm.Tokens[args[2]])
		case Andb:
			vm.Mem[args[0]] = values.Value{values.BOOL, vm.Mem[args[1]].V.(bool) && vm.Mem[args[2]].V.(bool)}
		case Aref:
			vm.Mem[vm.Mem[args[0]].V.(uint32)] = vm.Mem[args[1]]
		case Asgm:
			vm.Mem[args[0]] = vm.Mem[args[1]]
		case Call:
			offset := args[1]
			for i := args[1]; i < args[2]; i++ {
				vm.Mem[i] = vm.Mem[args[3+i-offset]]
			}
			vm.callstack = append(vm.callstack, loc)
			loc = args[0]
			continue
		case CalT:
			offset := int(args[1]) - 3
			var tupleTime bool
			var tplpt int
			tupleList := vm.Mem[args[2]].V.([]uint32) // This is the hireg of the parameters, and (numbering being exclusive) is the reg containing the integer array saying where tuple captures start.
			for j := 3; j < len(args); j++ {
				if tplpt <= len(tupleList) && j-3 == int(tupleList[tplpt]) {
					tupleTime = true
					vm.Mem[args[1]+tupleList[tplpt]] = values.Value{values.TUPLE, make([]values.Value, 0, 10)}
				}
				if vm.Mem[args[j]].T == values.BLING {
					tupleTime = false
				}
				if tupleTime {
					tupleVal := vm.Mem[args[1]+tupleList[tplpt]].V.([]values.Value)
					tupleVal = append(tupleVal, vm.Mem[args[j]])
					vm.Mem[args[1]+tupleList[tplpt]].V = tupleVal
				} else {
					vm.Mem[j+offset] = vm.Mem[args[j]]
				}
			}
			vm.callstack = append(vm.callstack, loc)
			loc = args[0]
			continue
		case Cc11:
			vm.Mem[args[0]] = values.Value{values.TUPLE, []values.Value{vm.Mem[args[1]], vm.Mem[args[2]]}}
		case Cc1T:
			vm.Mem[args[0]] = values.Value{values.TUPLE, append([]values.Value{vm.Mem[args[1]]}, vm.Mem[args[2]].V.([]values.Value)...)}
		case CcT1:
			vm.Mem[args[0]] = values.Value{values.TUPLE, append(vm.Mem[args[1]].V.([]values.Value), vm.Mem[args[2]])}
		case CcTT:
			vm.Mem[args[0]] = values.Value{values.TUPLE, append(vm.Mem[args[1]].V.([]values.Value), vm.Mem[args[2]])}
		case Ccxx:
			if vm.Mem[args[1]].T == values.TUPLE {
				if vm.Mem[args[2]].T == values.TUPLE {
					vm.Mem[args[0]] = values.Value{values.TUPLE, append(vm.Mem[args[1]].V.([]values.Value), vm.Mem[args[2]])}
				} else {
					vm.Mem[args[0]] = values.Value{values.TUPLE, append(vm.Mem[args[1]].V.([]values.Value), vm.Mem[args[2]])}
				}
			} else {
				if vm.Mem[args[2]].T == values.TUPLE {
					vm.Mem[args[0]] = values.Value{values.TUPLE, append([]values.Value{vm.Mem[args[1]]}, vm.Mem[args[2]].V.([]values.Value)...)}
				} else {
					vm.Mem[args[0]] = values.Value{values.TUPLE, []values.Value{vm.Mem[args[1]], vm.Mem[args[2]]}}
				}
			}
		case Cv1T:
			vm.Mem[args[0]] = values.Value{values.TUPLE, []values.Value{vm.Mem[args[1]]}}
		case CvTT:
			slice := make([]values.Value, len(args)-1)
			for i := 0; i < len(slice); i++ {
				slice[i] = vm.Mem[args[i+1]]
			}
			vm.Mem[args[0]] = values.Value{values.TUPLE, slice}
		case Divf:
			vm.Mem[args[0]] = values.Value{values.FLOAT, vm.Mem[args[1]].V.(float64) / vm.Mem[args[2]].V.(float64)}
		case Divi:
			vm.Mem[args[0]] = values.Value{values.INT, vm.Mem[args[1]].V.(int) / vm.Mem[args[2]].V.(int)}
		case Dofn:
			lhs := vm.Mem[args[1]].V.(Lambda)
			for i := 0; i < int(lhs.PrmTop-lhs.ExtTop); i++ {
				lhs.Mc.Mem[int(lhs.ExtTop)+i] = vm.Mem[args[2+i]]
			}
			copy(lhs.Captures, vm.Mem)
			lhs.Mc.Run(lhs.LocToCall)
			vm.Mem[args[0]] = lhs.Mc.Mem[lhs.Dest]
		case Dref:
			vm.Mem[args[0]] = vm.Mem[vm.Mem[args[1]].V.(uint32)]
		case Equb:
			vm.Mem[args[0]] = values.Value{values.BOOL, vm.Mem[args[1]].V.(bool) == vm.Mem[args[2]].V.(bool)}
		case Equf:
			vm.Mem[args[0]] = values.Value{values.BOOL, vm.Mem[args[1]].V.(float64) == vm.Mem[args[2]].V.(float64)}
		case Equi:
			vm.Mem[args[0]] = values.Value{values.BOOL, vm.Mem[args[1]].V.(int) == vm.Mem[args[2]].V.(int)}
		case Equs:
			vm.Mem[args[0]] = values.Value{values.BOOL, vm.Mem[args[1]].V.(string) == vm.Mem[args[2]].V.(string)}
		case Equt:
			vm.Mem[args[0]] = values.Value{values.BOOL, vm.Mem[args[1]].V.(values.AbstractType).Equals(vm.Mem[args[2]].V.(values.AbstractType))}
		case Eqxx:
			if vm.Mem[args[1]].T != vm.Mem[args[2]].T {
				vm.Mem[args[0]] = vm.Mem[args[3]]
			} else {
				vm.Mem[args[0]] = values.Value{values.BOOL, vm.equals(vm.Mem[args[1]], vm.Mem[args[2]])}
			}
		case Flti:
			vm.Mem[args[0]] = values.Value{values.FLOAT, float64(vm.Mem[args[1]].V.(int))}
		case Flts:
			i, err := strconv.ParseFloat(vm.Mem[args[1]].V.(string), 64)
			if err != nil {
				vm.Mem[args[0]] = values.Value{values.ERROR, DUMMY}
			} else {
				vm.Mem[args[0]] = values.Value{values.FLOAT, i}
			}
		case Gofn:
			F := vm.GoFns[args[1]]
			goTpl := make([]any, 0, len(args))
			for i, v := range args[2:] {
				el := vm.Mem[v]
				if F.Raw[i] {
					goTpl = append(goTpl, el)
				} else {
					goTpl = append(goTpl, vm.pipefishToGo(el, F.PfToGo))
				}
			}
			vm.Mem[args[0]] = vm.goToPipefish(F.Code(goTpl...), F.GoToPf)
		case Gtef:
			vm.Mem[args[0]] = values.Value{values.BOOL, vm.Mem[args[1]].V.(float64) >= vm.Mem[args[2]].V.(float64)}
		case Gtei:
			vm.Mem[args[0]] = values.Value{values.BOOL, vm.Mem[args[1]].V.(int) >= vm.Mem[args[2]].V.(int)}
		case Gthf:
			vm.Mem[args[0]] = values.Value{values.BOOL, vm.Mem[args[1]].V.(float64) > vm.Mem[args[2]].V.(float64)}
		case Gthi:
			vm.Mem[args[0]] = values.Value{values.BOOL, vm.Mem[args[1]].V.(int) > vm.Mem[args[2]].V.(int)}
		case Halt:
			break loop
		case Idfn:
			vm.Mem[args[0]] = vm.Mem[args[1]]
		case IdxL:
			vec := vm.Mem[args[1]].V.(vector.Vector)
			ix := vm.Mem[args[2]].V.(int)
			val, ok := vec.Index(ix)
			if !ok {
				vm.Mem[args[0]] = vm.Mem[args[3]]

			} else {
				vm.Mem[args[0]] = val.(values.Value)
			}
		case Idxp:
			pair := vm.Mem[args[1]].V.([]values.Value)
			ix := vm.Mem[args[2]].V.(int)
			ok := ix == 0 || ix == 1
			if ok {
				vm.Mem[args[0]] = pair[ix]
			} else {
				vm.Mem[args[0]] = vm.Mem[args[3]]
			}
		case Idxs:
			str := vm.Mem[args[1]].V.(string)
			ix := vm.Mem[args[2]].V.(int)
			ok := 0 <= ix && ix < len(str)
			if ok {
				val := values.Value{values.STRING, string(str[ix])}
				vm.Mem[args[0]] = val
			} else {
				vm.Mem[args[0]] = vm.Mem[args[3]]
			}
		case Idxt:
			typ := vm.Mem[args[1]].V.(values.ValueType)
			if typ < values.LB_ENUMS || vm.Ub_enums <= typ {
				vm.Mem[args[0]] = vm.Mem[args[3]]
				break
			}
			ix := vm.Mem[args[2]].V.(int)
			ok := 0 <= ix && ix < len(vm.Enums[typ-values.LB_ENUMS])
			if ok {
				vm.Mem[args[0]] = values.Value{typ, ix}
			} else {
				vm.Mem[args[0]] = vm.Mem[args[4]]
			}
		case IdxT:
			tuple := vm.Mem[args[1]].V.([]values.Value)
			ix := vm.Mem[args[2]].V.(int)
			ok := 0 <= ix && ix < len(tuple)
			if ok {
				vm.Mem[args[0]] = tuple[ix]
			} else {
				vm.Mem[args[0]] = vm.Mem[args[3]]
			}
		case Inpt:
			vm.Mem[args[0]] = values.Value{values.STRING, vm.IoHandle.InHandle.Get(vm.Mem[args[1]].V.([]values.Value)[0].V.(string))}
		case Intf:
			vm.Mem[args[0]] = values.Value{values.INT, int(vm.Mem[args[1]].V.(float64))}
		case Ints:
			i, err := strconv.Atoi(vm.Mem[args[1]].V.(string))
			if err != nil {
				vm.Mem[args[0]] = values.Value{values.ERROR, DUMMY}
			} else {
				vm.Mem[args[0]] = values.Value{values.INT, i}
			}
		case InxL:
			x := vm.Mem[args[1]]
			L := vm.Mem[args[2]].V.(vector.Vector)
			i := 0
			vm.Mem[args[0]] = values.Value{values.BOOL, false}
			for el, ok := L.Index(i); ok; {
				if x.T == el.(values.Value).T {
					if vm.equals(x, el.(values.Value)) {
						vm.Mem[args[0]] = values.Value{values.BOOL, true}
						break
					}
				}
				i++
				el, ok = L.Index(i)
			}
		case InxS:
			x := vm.Mem[args[1]]
			S := vm.Mem[args[2]].V.(values.Set)
			if S.Contains(x) {
				vm.Mem[args[0]] = values.Value{values.BOOL, true}
			} else {
				vm.Mem[args[0]] = values.Value{values.BOOL, false}
			}
		case InxT:
			x := vm.Mem[args[1]]
			T := vm.Mem[args[2]].V.([]values.Value)
			vm.Mem[args[0]] = values.Value{values.BOOL, false}
			for _, el := range T {
				if x.T == el.T {
					if vm.equals(x, el) {
						vm.Mem[args[0]] = values.Value{values.BOOL, true}
						break
					}
				}
			}
		case Inxt:
			vm.Mem[args[0]] = values.Value{values.BOOL, false}
			for _, t := range vm.Mem[args[2]].V.(values.AbstractType) {
				if vm.Mem[args[1]].T == t {
					vm.Mem[args[0]] = values.Value{values.BOOL, true}
				}
			}
		case IxTn:
			vm.Mem[args[0]] = vm.Mem[args[1]].V.([]values.Value)[args[2]]
		case IxXx:
			container := vm.Mem[args[1]]
			index := vm.Mem[args[2]]
			if index.T == values.PAIR { // Then we're slicing.
				// We switch on the type of the lhs.
				switch container.T {
				case values.LIST:
					vec := vm.Mem[args[1]].V.(vector.Vector)
					ix := vm.Mem[args[2]].V.([]values.Value)
					if ix[0].T == values.INT && ix[1].T == values.INT && 0 <= ix[0].V.(int) &&
						ix[0].V.(int) <= ix[1].V.(int) && ix[1].V.(int) <= vec.Len() {
						vm.Mem[args[0]] = values.Value{values.LIST, vec.SubVector(ix[0].V.(int), ix[1].V.(int))}
					} else {
						vm.Mem[args[0]] = vm.Mem[args[3]]
					}
				case values.STRING:
					str := container.V.(string)
					ix := index.V.([]values.Value)
					if ix[0].T == values.INT && ix[1].T == values.INT && 0 <= ix[0].V.(int) &&
						ix[0].V.(int) <= ix[1].V.(int) && ix[1].V.(int) <= len(str) {
						vm.Mem[args[0]] = values.Value{values.STRING, str[ix[0].V.(int):ix[1].V.(int)]}
					} else {
						vm.Mem[args[0]] = vm.Mem[args[3]]
					}
				case values.TUPLE:
					tup := container.V.([]values.Value)
					ix := index.V.([]values.Value)
					if ix[0].T == values.INT && ix[1].T == values.INT && 0 <= ix[0].V.(int) &&
						ix[0].V.(int) <= ix[1].V.(int) && ix[1].V.(int) <= len(tup) {
						vm.Mem[args[0]] = values.Value{values.STRING, tup[ix[0].V.(int):ix[1].V.(int)]}
					} else {
						vm.Mem[args[0]] = vm.Mem[args[3]]
					}
				default:
					vm.Mem[args[0]] = vm.Mem[args[3]]
				}
			}
			// Otherwise it's not a slice. We switch on the type of the lhs.
			if vm.Ub_enums <= container.T {
				ix := vm.StructResolve.Resolve(int(vm.Mem[args[1]].T-vm.Ub_enums), vm.Mem[args[2]].V.(int))
				vm.Mem[args[0]] = vm.Mem[args[1]].V.([]values.Value)[ix]
				break
			}
			switch container.T {
			case values.LIST:
				vec := container.V.(vector.Vector)
				ix := index.V.(int)
				val, ok := vec.Index(ix)
				if !ok {
					vm.Mem[args[0]] = vm.Mem[args[3]]
				} else {
					vm.Mem[args[0]] = val.(values.Value)
				}
			case values.PAIR:
				pair := container.V.([]values.Value)
				ix := index.V.(int)
				ok := ix == 0 || ix == 1
				if ok {
					vm.Mem[args[0]] = pair[ix]
				} else {
					vm.Mem[args[0]] = vm.Mem[args[3]]
				}
			case values.STRING:
				str := container.V.(string)
				ix := index.V.(int)
				ok := 0 <= ix && ix < len(str)
				if ok {
					val := values.Value{values.STRING, string(str[ix])}
					vm.Mem[args[0]] = val
				} else {
					vm.Mem[args[0]] = vm.Mem[args[3]]
				}
			case values.TYPE:
				typ := container.V.(values.ValueType)
				if typ < values.LB_ENUMS || vm.Ub_enums <= typ {
					vm.Mem[args[0]] = vm.Mem[args[3]]
					break
				}
				ix := index.V.(int)
				ok := 0 <= ix && ix < len(vm.Enums[typ-values.LB_ENUMS])
				if ok {
					vm.Mem[args[0]] = values.Value{typ, ix}
				} else {
					vm.Mem[args[0]] = vm.Mem[args[4]]
				}
			case values.TUPLE:
				tuple := container.V.([]values.Value)
				ix := index.V.(int)
				ok := 0 <= ix && ix < len(tuple)
				if ok {
					vm.Mem[args[0]] = tuple[ix]
				} else {
					vm.Mem[args[0]] = vm.Mem[args[3]]
				}
			default:
				vm.Mem[args[0]] = vm.Mem[args[3]]
			}
		case IxZl:
			ix := vm.StructResolve.Resolve(int(vm.Mem[args[1]].T-vm.Ub_enums), vm.Mem[args[2]].V.(int))
			vm.Mem[args[0]] = vm.Mem[args[1]].V.([]values.Value)[ix]
		case IxZn:
			vm.Mem[args[0]] = vm.Mem[args[1]].V.([]values.Value)[args[2]]
		case Jmp:
			loc = args[0]
			continue
		case Jsr:
			vm.callstack = append(vm.callstack, loc)
			loc = args[0]
			continue
		case KeyM:
			vm.Mem[args[0]] = values.Value{values.LIST, vm.Mem[args[1]].V.(*values.Map).AsVector()}
		case KeyZ:
			result := vector.Empty
			for _, labelNumber := range vm.StructLabels[vm.Mem[args[1]].T-vm.Ub_enums] {
				result = result.Conj(values.Value{values.LABEL, labelNumber})
			}
			vm.Mem[args[0]] = values.Value{values.LIST, result}
		case LenL:
			vm.Mem[args[0]] = values.Value{values.INT, vm.Mem[args[1]].V.(vector.Vector).Len()}
		case LenM:
			vm.Mem[args[0]] = values.Value{values.INT, vm.Mem[args[1]].V.(*values.Map).Len()}
		case Lens:
			vm.Mem[args[0]] = values.Value{values.INT, len(vm.Mem[args[1]].V.(string))}
		case LenS:
			vm.Mem[args[0]] = values.Value{values.INT, vm.Mem[args[1]].V.(values.Set).Len()}
		case LenT:
			vm.Mem[args[0]] = values.Value{values.INT, len(vm.Mem[args[1]].V.([]values.Value))}
		case List:
			list := vector.Empty
			if vm.Mem[args[1]].T == values.TUPLE {
				for _, v := range vm.Mem[args[1]].V.([]values.Value) {
					list = list.Conj(v)
				}
			} else {
				list = list.Conj(vm.Mem[args[1]])
			}
			vm.Mem[args[0]] = values.Value{values.LIST, list}
		case Litx:
			vm.Mem[args[0]] = values.Value{values.STRING, vm.Literal(vm.Mem[args[1]])}
		case Log:
			fmt.Print(vm.Mem[args[0]].V.(string) + "\n\n")
		case Logn:
			vm.logging = false
		case Logy:
			vm.logging = true
		case Mker:
			vm.Mem[args[0]] = values.Value{values.ERROR, &report.Error{ErrorId: "eval/user", Message: vm.Mem[args[1]].V.(string), Token: vm.Tokens[args[2]]}}
		case Mkfn:
			lf := vm.LambdaFactories[args[1]]
			newLambda := *lf.Model
			newLambda.Captures = make([]values.Value, len(lf.ExtMem))
			for i, v := range lf.ExtMem {
				newLambda.Captures[i] = vm.Mem[v]
			}
			vm.Mem[args[0]] = values.Value{values.FUNC, newLambda}
		case Mkpr:
			vm.Mem[args[0]] = values.Value{values.PAIR, []values.Value{vm.Mem[args[1]], vm.Mem[args[2]]}}
		case Mkst:
			result := values.Set{}
			for _, v := range vm.Mem[args[1]].V.([]values.Value) {
				if !((values.NULL <= v.T && v.T < values.PAIR) || (values.LB_ENUMS <= v.T && v.T < vm.Ub_enums)) {
					vm.Mem[args[0]] = vm.Mem[vm.That()] // I.e. an error created before the mkst call.
				}
				result = result.Add(v)
			}
			vm.Mem[args[0]] = values.Value{values.SET, result}
		case Mkmp:
			result := &values.Map{}
			for _, p := range vm.Mem[args[1]].V.([]values.Value) {
				if p.T != values.PAIR {
					vm.Mem[args[0]] = vm.Mem[vm.That()-1] // I.e. an error created before the mkmp call.
					break
				}
				k := p.V.([]values.Value)[0]
				v := p.V.([]values.Value)[1]
				if !((values.NULL <= v.T && v.T < values.PAIR) || (values.LB_ENUMS <= v.T && v.T < vm.Ub_enums)) {
					vm.Mem[args[0]] = vm.Mem[vm.That()] // I.e. an error created before the mkst call.
				}
				result = result.Set(k, v)
			}
			vm.Mem[args[0]] = values.Value{values.MAP, result}
		case MkSn:
			sFac := vm.SnippetFactories[args[1]]
			result := &values.Map{}
			for k, v := range sFac.sourceEnv.data { // TODO --- check access.
				result = result.Set(values.Value{values.STRING, k}, vm.Mem[v.mLoc])
			}
			vm.Mem[args[0]] = values.Value{values.ValueType(sFac.snippetType),
				[]values.Value{values.Value{values.STRING, sFac.sourceString}, values.Value{values.MAP, result}}}
		case Modi:
			vm.Mem[args[0]] = values.Value{values.INT, vm.Mem[args[1]].V.(int) % vm.Mem[args[2]].V.(int)}
		case Mulf:
			vm.Mem[args[0]] = values.Value{values.FLOAT, vm.Mem[args[1]].V.(float64) * vm.Mem[args[2]].V.(float64)}
		case Muli:
			vm.Mem[args[0]] = values.Value{values.INT, vm.Mem[args[1]].V.(int) * vm.Mem[args[2]].V.(int)}
		case Negf:
			vm.Mem[args[0]] = values.Value{values.FLOAT, -vm.Mem[args[1]].V.(float64)}
		case Negi:
			vm.Mem[args[0]] = values.Value{values.INT, -vm.Mem[args[1]].V.(int)}
		case Notb:
			vm.Mem[args[0]] = values.Value{values.BOOL, !vm.Mem[args[1]].V.(bool)}
		case Orb:
			vm.Mem[args[0]] = values.Value{values.BOOL, (vm.Mem[args[1]].V.(bool) || vm.Mem[args[2]].V.(bool))}
		case Outp:
			vm.IoHandle.OutHandle.Out([]values.Value{vm.Mem[args[0]]}, vm)
		case Outt:
			fmt.Println(vm.Describe(vm.Mem[args[0]]))
		case Qabt:
			for _, t := range args[1 : len(args)-1] {
				if vm.Mem[args[0]].T == values.ValueType(t) {
					loc = loc + 1
					continue loop
				}
			}
			loc = args[len(args)-1]
			continue
		case Qctc:
			if vm.Mem[args[0]].T >= vm.Ub_langs {
				loc = loc + 1
			} else {
				loc = args[1]
			}
			continue
		case Qctq:
			if vm.Mem[args[0]].T >= vm.Ub_langs || vm.Mem[args[0]].T == values.NULL {
				loc = loc + 1
			} else {
				loc = args[1]
			}
			continue
		case QlnT:
			if len(vm.Mem[args[0]].V.([]values.Value)) == int(args[1]) {
				loc = loc + 1
			} else {
				loc = args[2]
			}
			continue
		case Qlog:
			if vm.logging {
				loc = loc + 1
			} else {
				loc = args[0]
			}
			continue
		case Qntp:
			if vm.Mem[args[0]].T != values.ValueType(args[1]) {
				loc = loc + 1
			} else {
				loc = args[2]
			}
			continue
		case Qsat:
			if vm.Mem[args[0]].T != values.UNSAT {
				loc = loc + 1
			} else {
				loc = args[1]
			}
			continue
		case Qsng:
			if vm.Mem[args[0]].T >= values.INT {
				loc = loc + 1
			} else {
				loc = args[1]
			}
			continue
		case Qsnq:
			if vm.Mem[args[0]].T >= values.NULL {
				loc = loc + 1
			} else {
				loc = args[1]
			}
			continue
		case Qspt:
			if vm.Mem[args[0]].T >= vm.Lb_snippets {
				loc = loc + 1
			} else {
				loc = args[1]
			}
			continue
		case Qspq:
			if vm.Mem[args[0]].T >= vm.Lb_snippets || vm.Mem[args[0]].T == values.NULL {
				loc = loc + 1
			} else {
				loc = args[1]
			}
			continue
		case Qstr:
			if vm.Mem[args[0]].T >= vm.Ub_enums {
				loc = loc + 1
			} else {
				loc = args[1]
			}
			continue
		case Qstq:
			if vm.Mem[args[0]].T >= vm.Ub_enums || vm.Mem[args[0]].T == values.NULL {
				loc = loc + 1
			} else {
				loc = args[1]
			}
			continue
		case Qtru:
			if vm.Mem[args[0]].V.(bool) {
				loc = loc + 1
			} else {
				loc = args[1]
			}
			continue
		case Qtyp:
			if vm.Mem[args[0]].T == values.ValueType(args[1]) {
				loc = loc + 1
			} else {
				loc = args[2]
			}
			continue
		case Ret:
			if len(vm.callstack) == 0 {
				break loop
			}
			loc = vm.callstack[len(vm.callstack)-1]
			vm.callstack = vm.callstack[0 : len(vm.callstack)-1]
		case SliL:
			vec := vm.Mem[args[1]].V.(vector.Vector)
			ix := vm.Mem[args[2]].V.([]values.Value)
			if ix[0].T == values.INT && ix[1].T == values.INT && 0 <= ix[0].V.(int) &&
				ix[0].V.(int) <= ix[1].V.(int) && ix[1].V.(int) <= vec.Len() {
				vm.Mem[args[0]] = values.Value{values.LIST, vec.SubVector(ix[0].V.(int), ix[1].V.(int))}
			} else {
				vm.Mem[args[0]] = vm.Mem[args[3]]
			}
		case Slis:
			str := vm.Mem[args[1]].V.(string)
			ix := vm.Mem[args[2]].V.([]values.Value)
			if ix[0].T == values.INT && ix[1].T == values.INT && 0 <= ix[0].V.(int) &&
				ix[0].V.(int) <= ix[1].V.(int) && ix[1].V.(int) <= len(str) {
				vm.Mem[args[0]] = values.Value{values.STRING, str[ix[0].V.(int):ix[1].V.(int)]}
			} else {
				vm.Mem[args[0]] = vm.Mem[args[3]]
			}
		case SliT:
			tup := vm.Mem[args[1]].V.([]values.Value)
			ix := vm.Mem[args[2]].V.([]values.Value)
			if ix[0].T == values.INT && ix[1].T == values.INT && 0 <= ix[0].V.(int) &&
				ix[0].V.(int) <= ix[1].V.(int) && ix[1].V.(int) <= len(tup) {
				vm.Mem[args[0]] = values.Value{values.STRING, tup[ix[0].V.(int):ix[1].V.(int)]}
			} else {
				vm.Mem[args[0]] = vm.Mem[args[3]]
			}
		case Strc:
			fields := make([]values.Value, 0, len(args)-2)
			for _, loc := range args[2:] {
				fields = append(fields, vm.Mem[loc])
			}
			vm.Mem[args[0]] = values.Value{values.ValueType(args[1]), fields}
		case Strx:
			vm.Mem[args[0]] = values.Value{values.STRING, vm.Describe(vm.Mem[args[1]])}
		case Subf:
			vm.Mem[args[0]] = values.Value{values.FLOAT, vm.Mem[args[1]].V.(float64) - vm.Mem[args[2]].V.(float64)}
		case Subi:
			vm.Mem[args[0]] = values.Value{values.INT, vm.Mem[args[1]].V.(int) - vm.Mem[args[2]].V.(int)}
		case Thnk:
			vm.Mem[args[0]].T = values.THUNK
			vm.Mem[args[0]].V = args[1]
		case TupL:
			vector := vm.Mem[args[1]].V.(vector.Vector)
			length := vector.Len()
			slice := make([]values.Value, length)
			for i := 0; i < length; i++ {
				element, _ := vector.Index(i)
				slice[i] = element.(values.Value)
			}
			vm.Mem[args[0]] = values.Value{values.TUPLE, slice}
		case Typu:
			lhs := vm.Mem[args[1]].V.(values.AbstractType)
			rhs := vm.Mem[args[2]].V.(values.AbstractType)
			i := 0
			j := 0
			result := make(values.AbstractType, 0, len(lhs)+len(rhs))
			for i < len(lhs) || j < len(rhs) {
				switch {
				case i == len(lhs):
					result = append(result, rhs[j])
					j++
				case j == len(rhs):
					result = append(result, lhs[i])
					i++
				case lhs[i] == rhs[j]:
					result = append(result, lhs[i])
					i++
					j++
				case lhs[i] < rhs[j]:
					result = append(result, lhs[i])
					i++
				case rhs[j] < lhs[i]:
					result = append(result, rhs[j])
					j++
				}
			}
			vm.Mem[args[0]] = values.Value{values.TYPE, result}
		case Typx:
			vm.Mem[args[0]] = values.Value{values.TYPE, values.AbstractType{vm.Mem[args[1]].T}}
		case Untk:
			if (vm.Mem[args[0]].T) == values.THUNK {
				vm.callstack = append(vm.callstack, loc)
				loc = vm.Mem[args[0]].V.(uint32)
				continue
			}
		case WthL:
			var pairs []values.Value
			if (vm.Mem[args[2]].T) == values.PAIR {
				pairs = []values.Value{vm.Mem[args[2]]}
			} else {
				pairs = vm.Mem[args[2]].V.([]values.Value)
			}
			result := values.Value{values.LIST, vm.Mem[args[1]].V.(vector.Vector)}
			for _, pair := range pairs {
				if pair.T != values.PAIR {
					vm.Mem[args[0]] = vm.Mem[args[3]]
					break
				}
				key := pair.V.([]values.Value)[0]
				val := pair.V.([]values.Value)[1]
				var keys []values.Value
				if key.T == values.LIST {
					vec := key.V.(vector.Vector)
					ln := vec.Len()
					if ln == 0 {
						vm.Mem[args[0]] = vm.Mem[args[3]]
						break
					}
					keys = make([]values.Value, ln)
					for i := 0; i < ln; i++ {
						el, _ := vec.Index(i)
						keys[i] = el.(values.Value)
					}
				} else {
					keys = []values.Value{key}
				}
				result = vm.with(result, keys, val, vm.Mem[args[3]])
				if result.T == values.ERROR {
					break
				}
			}
			vm.Mem[args[0]] = result
		case WthM:
			var pairs []values.Value
			if (vm.Mem[args[2]].T) == values.PAIR {
				pairs = []values.Value{vm.Mem[args[2]]}
			} else {
				pairs = vm.Mem[args[2]].V.([]values.Value)
			}
			result := values.Value{values.MAP, vm.Mem[args[1]].V.(*values.Map)}
			for _, pair := range pairs {
				if pair.T != values.PAIR {
					vm.Mem[args[0]] = vm.Mem[args[3]]
					break
				}
				key := pair.V.([]values.Value)[0]
				val := pair.V.([]values.Value)[1]
				var keys []values.Value
				if key.T == values.LIST {
					vec := key.V.(vector.Vector)
					ln := vec.Len()
					if ln == 0 {
						vm.Mem[args[0]] = vm.Mem[args[3]]
						break
					}
					keys = make([]values.Value, ln)
					for i := 0; i < ln; i++ {
						el, _ := vec.Index(i)
						keys[i] = el.(values.Value)
					}
				} else {
					keys = []values.Value{key}
				}
				result = vm.with(result, keys, val, vm.Mem[args[3]])
				if result.T == values.ERROR {
					break
				}
			}
			vm.Mem[args[0]] = result
		case Wtht:
			typL := vm.Mem[args[1]].V.(values.AbstractType)
			if len(typL) != 1 {
				vm.Mem[args[0]] = vm.Mem[args[3]]
				break Switch
			}
			typ := typL[0]
			if (typ) < vm.Ub_enums {
				vm.Mem[args[0]] = vm.Mem[args[3]]
				break Switch
			}
			typeNumber := typ - vm.Ub_enums
			var pairs []values.Value
			if (vm.Mem[args[2]].T) == values.PAIR {
				pairs = []values.Value{vm.Mem[args[0]]}
			} else {
				pairs = vm.Mem[args[2]].V.([]values.Value)
			}
			outVals := make([]values.Value, len(vm.StructLabels[typeNumber]))
			for _, pair := range pairs {
				if pair.T != values.PAIR {
					vm.Mem[args[0]] = vm.Mem[args[3]]
					break
				}
				key := pair.V.([]values.Value)[0]
				val := pair.V.([]values.Value)[1]
				if key.T != values.LABEL {
					vm.Mem[args[0]] = vm.Mem[args[3]]
					break Switch
				}
				keyNumber := vm.StructResolve.Resolve(int(typeNumber), key.V.(int))
				if keyNumber == -1 {
					vm.Mem[args[0]] = vm.Mem[args[3]]
					break Switch
				}
				if outVals[keyNumber].T != values.UNDEFINED_VALUE {
					vm.Mem[args[0]] = vm.Mem[args[3]]
					break Switch
				}
				outVals[keyNumber] = val
			}
			for _, v := range outVals {
				if v.T == values.UNDEFINED_VALUE {
					vm.Mem[args[0]] = vm.Mem[args[3]]
					break Switch
				}
			}
			vm.Mem[args[0]] = values.Value{typ, outVals}
		case WthZ:
			typ := vm.Mem[args[1]].T
			typeNumber := typ - vm.Ub_enums
			var pairs []values.Value
			if (vm.Mem[args[2]].T) == values.PAIR {
				pairs = []values.Value{vm.Mem[args[2]]}
			} else {
				pairs = vm.Mem[args[2]].V.([]values.Value)
			}
			outVals := make([]values.Value, len(vm.StructLabels[typeNumber]))
			copy(outVals, vm.Mem[args[1]].V.([]values.Value))
			result := values.Value{typ, outVals}
			for _, pair := range pairs {
				if pair.T != values.PAIR {
					vm.Mem[args[0]] = vm.Mem[args[3]]
					break
				}
				key := pair.V.([]values.Value)[0]
				val := pair.V.([]values.Value)[1]
				var keys []values.Value
				if key.T == values.LIST {
					vec := key.V.(vector.Vector)
					ln := vec.Len()
					if ln == 0 {
						vm.Mem[args[0]] = vm.Mem[args[3]]
						break
					}
					keys = make([]values.Value, ln)
					for i := 0; i < ln; i++ {
						el, _ := vec.Index(i)
						keys[i] = el.(values.Value)
					}
				} else {
					keys = []values.Value{key}
				}
				result = vm.with(result, keys, val, vm.Mem[args[3]])
				if result.T == values.ERROR {
					break
				}
			}
			vm.Mem[args[0]] = result
		case WtoM:
			var items []values.Value
			if (vm.Mem[args[2]].T) == values.TUPLE {
				items = vm.Mem[args[2]].V.([]values.Value)
			} else {
				items = []values.Value{vm.Mem[args[2]]}
			}
			mp := vm.Mem[args[1]].V.(*values.Map)
			for _, key := range items {
				if (key.T < values.NULL || key.T >= values.FUNC) && (key.T < values.LABEL || key.T >= vm.Ub_enums) { // Check that the key is orderable.
					vm.Mem[args[0]] = vm.Mem[args[3]]
					break Switch
				}
				mp = (*mp).Delete(key)
			}
			vm.Mem[args[0]] = values.Value{values.MAP, mp}
		case Xcon:
			snippet := vm.Mem[args[1]].V.([]values.Value)[0].V.(string)
			//env := vm.Mem[args[1]].V.([]values.Value)[1].V.(*values.Map)
			println(snippet)
			vm.Mem[args[0]] = values.Value{values.SUCCESSFUL_VALUE, nil}
		case Xsql:
			snippet := vm.Mem[args[1]].V.([]values.Value)[0].V.(string)
			//env := vm.Mem[args[1]].V.([]values.Value)[1].V.(*values.Map)
			println(snippet)
			vm.Mem[args[0]] = values.Value{values.SUCCESSFUL_VALUE, nil}
		default:
			panic("Unhandled opcode '" + OPERANDS[vm.Code[loc].Opcode].oc + "'")
		}
		loc++
	}
	if settings.SHOW_RUNTIME {
		println()
	}
}

// Implements equality-by-value. Assumes that the two values have already been verified to have the same type.
func (mc Vm) equals(v, w values.Value) bool {
	switch v.T {
	case values.NULL:
		return true
	case values.INT:
		return v.V.(int) == w.V.(int)
	case values.BOOL:
		return v.V.(bool) == w.V.(bool)
	case values.STRING:
		return v.V.(string) == w.V.(string)
	case values.FLOAT:
		return v.V.(float64) == w.V.(float64)
	case values.TYPE:
		return v.V.(values.AbstractType).Equals(w.V.(values.AbstractType))
	case values.PAIR:
		return mc.equals(v.V.([]values.Value)[0], w.V.([]values.Value)[0]) &&
			mc.equals(v.V.([]values.Value)[1], w.V.([]values.Value)[1])
	case values.LIST:
		K := v.V.(vector.Vector)
		L := w.V.(vector.Vector)
		lth := K.Len()
		if L.Len() != lth {
			return false
		}
		for i := 0; i < lth; i++ {
			kEl, _ := K.Index(i)
			lEl, _ := L.Index(i)
			if kEl.(values.Value).T != lEl.(values.Value).T {
				return false
			}
			if !mc.equals(kEl.(values.Value), lEl.(values.Value)) {
				return false
			}
		}
		return true
	case values.SET:

	case values.MAP:

	case values.FUNC:
		return false
	}
	if values.LB_ENUMS <= v.T && v.T < mc.Ub_enums {
		return v.V.(int) == w.V.(int)
	}
	if v.T >= mc.Ub_enums {
		for i, v := range v.V.([]values.Value) {
			if !mc.equals(v, w.V.([]values.Value)[i]) {
				return false
			}
		}
		return true
	}
	panic("Wut?")
}

func (vm *Vm) with(container values.Value, keys []values.Value, val values.Value, err values.Value) values.Value {
	key := keys[0]
	switch container.T {
	case values.LIST:
		vec := container.V.(vector.Vector)
		if key.T != values.INT {
			return err
		}
		keyNumber := key.V.(int)
		if keyNumber < 0 || keyNumber >= vec.Len() {
			return err
		}
		if len(keys) == 1 {
			container.V = vec.Assoc(keyNumber, val)
			return container
		}
		el, _ := vec.Index(keyNumber)
		container.V = vec.Assoc(keyNumber, vm.with(el.(values.Value), keys[1:], val, err))
		return container
	case values.MAP:
		mp := container.V.(*values.Map)
		if (key.T < values.NULL || key.T >= values.FUNC) && (key.T < values.LABEL || key.T >= vm.Ub_enums) { // Check that the key is orderable.
			return err
		}
		if len(keys) == 1 {
			mp = mp.Set(key, val)
			return values.Value{values.MAP, mp}
		}
		el, _ := mp.Get(key)
		mp = mp.Set(key, vm.with(el, keys[1:], val, err))
		return values.Value{values.MAP, mp}
	default: // It's a struct.
		fields := make([]values.Value, len(container.V.([]values.Value)))
		clone := values.Value{container.T, fields}
		copy(fields, container.V.([]values.Value))
		typeNumber := container.T - vm.Ub_enums

		if key.T != values.LABEL {
			return err
		}
		fieldNumber := vm.StructResolve.Resolve(int(typeNumber), key.V.(int))
		if fieldNumber == -1 {
			return err
		}
		if len(keys) == 1 {
			fields[fieldNumber] = val
			return clone
		}
		fields[fieldNumber] = vm.with(fields[fieldNumber], keys[1:], val, err)
		return clone
	}
}

type StructResolver interface {
	Add(structNumber int, labels []int) StructResolver // Not the struct type, but its number, i.e. we start at 0.
	Resolve(structNumber int, labelNumber int) int
}

type MapResolver []map[int]int

func (mr MapResolver) Add(structNumber int, labels []int) StructResolver {
	if structNumber != len(mr) {
		panic("That wasn't meant to happen.")
	}
	newMap := make(map[int]int, len(labels))
	for k, v := range labels {
		newMap[v] = k
	}
	mr = append(mr, newMap)
	return mr
}

func (mr MapResolver) Resolve(structNumber int, labelNumber int) int {
	fieldNo, ok := mr[structNumber][labelNumber]
	if ok {
		return fieldNo
	}
	return -1
}

func (vm *Vm) MemTop() uint32 {
	return uint32(len(vm.Mem))
}

func (vm *Vm) That() uint32 {
	return uint32(len(vm.Mem) - 1)
}

func (vm *Vm) ThatToken() uint32 {
	return uint32(len(vm.Tokens) - 1)
}

func (vm *Vm) CodeTop() uint32 {
	return uint32(len(vm.Code))
}

func (vm *Vm) TokenTop() uint32 {
	return uint32(len(vm.Tokens))
}

func (vm *Vm) LfTop() uint32 {
	return uint32(len(vm.LambdaFactories))
}

func (vm *Vm) Next() uint32 {
	return uint32(len(vm.Code))
}
