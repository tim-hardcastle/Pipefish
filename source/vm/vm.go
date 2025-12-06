package vm

import (
	"context"
	"database/sql"
	"fmt"
	"os"
	"os/signal"
	"path/filepath"
	"reflect"
	"strconv"
	"strings"
	"syscall"

	"src.elv.sh/pkg/persistent/vector"

	"github.com/tim-hardcastle/pipefish/source/database"
	"github.com/tim-hardcastle/pipefish/source/err"
	"github.com/tim-hardcastle/pipefish/source/settings"
	"github.com/tim-hardcastle/pipefish/source/text"
	"github.com/tim-hardcastle/pipefish/source/token"
	"github.com/tim-hardcastle/pipefish/source/values"
)

type Vm struct {
	// Temporary state: things we change at runtime.
	Mem            []values.Value
	Code           []*Operation
	callstack      []uint32
	recursionStack []recursionData
	logging        bool
	// TODO --- the LogToLoc field of TrackingData is never used by *live* tracking, which should therefore have its own data type.
	LiveTracking []TrackingData // "Live" tracking data in which the uint32s in the permanent tracking data have been replaced by the corresponding memory registers.
	PostHappened bool

	// Permanent state: things established at compile time.

	ConcreteTypeInfo           []TypeInformation
	Labels                     []string // Array from the number of a field label to its name.
	Tokens                     []*token.Token
	LambdaFactories            []*LambdaFactory
	SnippetFactories           []*SnippetFactory
	GoFns                      []GoFn
	TypeCheckErrors            []*TypeCheckError
	Tracking                   []TrackingData // Data needed by the 'trak' opcode to produce the live tracking data.
	InHandle                   InHandler
	OutHandle                  OutHandler
	AbstractTypes              []values.AbstractTypeInfo
	ExternalCallHandlers       []ExternalCallHandler // The services declared external, whether on the same hub or a different one.
	UsefulTypes                UsefulTypes
	UsefulValues               UsefulValues
	TypeNumberOfUnwrappedError values.ValueType // What it says. When we unwrap an 'error' to an 'Error' struct, the vm needs to know the number of the struct.
	StringifyLoReg             uint32           // |
	StringifyCallTo            uint32           // | These are so the vm knows how to call the stringify function.
	StringifyOutReg            uint32           // |
	GoToPipefishTypes          map[reflect.Type]values.ValueType
	FieldLabelsInMem           map[string]uint32 // Used to turn a string into a label.
	GoConverter                [](func(t uint32, v any) any)
	ParameterizedTypeInfo      []*values.Map // A list of maps from type parameters (as TUPLE values) to types (as TYPE values). The list is itself keyed by a map from type operators to the position in the list, which is stored in the compiler.
}

// In general, the VM can't convert from type names to type numbers, because it doesn't
// need to. And we don't need the whole map of them because only a tiny proportion are
// needed by the runtime, so a struct gives us quick access to what we do need.
type UsefulTypes struct {
	UnwrappedError values.ValueType
	File           values.ValueType
	Terminal       values.ValueType
	Output         values.ValueType
}

// Similarly we need to know where some values are kept, if they have special effects
// on runtime behavior.
type UsefulValues struct {
	OutputAs uint32
}

// Contains a Go function in the form of a reflect.Value, and, currently, nothing else.
type GoFn struct {
	Code reflect.Value
}

// Contains the information to execute a lambda at runtime; i.e. it is the payload of a FUNC type value.
type Lambda struct {
	CapturesStart  uint32
	CapturesEnd    uint32
	ParametersEnd  uint32
	ResultLocation uint32
	AddressToCall  uint32
	Captures       []values.Value
	Sig            []values.AbstractType // To represent the call signature. Unusual in that the types of the AbstractType will be nil in case the type is 'any?'
	RtnSig         []values.AbstractType // The return signature. If empty means ok/error for a command, anything for a function.
	Tok            *token.Token
	Gocode         *reflect.Value // If it's a lambda returned from Go code, this will be non-nil, and most of the other fields will be their zero value except the sig information.
}

// What a thing of type SECRET keeps in its V field.
type Secret struct {
	secret values.Value
}

// Interface wrapping around external calls whether to the same hub or via HTTP.
type ExternalCallHandler interface {
	Evaluate(line string) values.Value
	Problem() *err.Error
	GetAPI() string
}

// All the information we need to make a lambda at a particular point in the code.
type LambdaFactory struct {
	Model            *Lambda  // Copy this to make the lambda.
	CaptureLocations []uint32 // Then these are the location of the values we're closing over, so we copy them into the lambda.
}

// All the information we need to make a snippet at a particular point in the code.
// Currently contains only the bindle but later may contain some secret sauce.
type SnippetFactory struct {
	Bindle *values.SnippetBindle // Points to the structure defined below.
}

// For containing the data needed to manufacture a typechecking error at runtime.
type TypeCheckError struct {
	Tok       *token.Token
	Condition string
	Type      string
	Value     uint32
}

// Container for the data we push when a function might be about to do recursion.
type recursionData struct {
	mems []values.Value
	loc  uint32
}

// Used for injecting data into HTML.
type HTMLInjector struct {
	Data []any
}

// These inhabit the first few memory addresses of the VM.
var CONSTANTS = []values.Value{values.UNDEF, values.FALSE, values.TRUE, values.U_OBJ, values.ONE, values.BLNG, values.OK, values.EMPTY}

// Type names in upper case are things the user should never see.
var nativeTypeNames = []string{"UNDEFINED VALUE", "INT ARRAY", "THUNK", "CREATED LOCAL CONSTANT",
	"COMPILE TIME ERROR", "BLING", "UNSATISFIED CONDITIONAL", "REFERENCE VARIABLE",
	"ITERATOR", "ok", "tuple", "error", "null", "int", "bool", "string", "rune", "float", "type", "func",
	"pair", "list", "map", "set", "label", "snippet", "secret"}

func BlankVm() *Vm {
	vm := &Vm{Mem: make([]values.Value, len(CONSTANTS)),
		logging: true, InHandle: &StandardInHandler{"→ "},
		GoToPipefishTypes: map[reflect.Type]values.ValueType{},
		GoConverter:       [](func(t uint32, v any) any){},
	}
	vm.OutHandle = &SimpleOutHandler{os.Stdout, vm}
	copy(vm.Mem, CONSTANTS)
	for _, name := range nativeTypeNames {
		vm.ConcreteTypeInfo = append(vm.ConcreteTypeInfo, BuiltinType{name: name})
	}
	vm.UsefulTypes.UnwrappedError = DUMMY
	vm.Mem = append(vm.Mem, values.Value{values.SUCCESSFUL_VALUE, nil}) // TODO --- why?
	vm.FieldLabelsInMem = make(map[string]uint32)
	return vm
}

// The `run` function can and occasionally does call itself. If we want to catch a panic
// from the VM, we need to unwind the stack back to where we actually entered. This is what
// `Run` is for: everything calling `run` does so through `Run`, except `run` itself and
// a few other places in the VM itself.
//
// In the same way we catch Ctrl+C interupts and return an appropriate error.
func (vm *Vm) Run(loc uint32) {
	// First the panics.
	if !settings.ALLOW_PANICS {
		defer func() {
			if r := recover(); r != nil {
				e := err.CreateErr("vm/panic", &token.Token{}, fmt.Sprintf("%v", r))
				vm.Mem = append(vm.Mem, values.Value{values.ERROR, e})
			}
		}()
	}
	// Then the ctrl+c interrupts.
	ctx, cancel := context.WithCancel(context.Background())
	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt, syscall.SIGTERM)
	go func() {
		<-c
		cancel()
		e := err.CreateErr("vm/ctrl/c", &token.Token{})
		vm.Mem = append(vm.Mem, values.Value{values.ERROR, e})
	}()
	vm.run(loc, ctx)
}

// The heart of the VM. A big loop around a switch. It will keep going until it hits a `ret`
// and the callstack is the same height as when it was called.
//
// (This condition, rather than just saying "until the callstack is empty" allows `run` to
// call itself under certain rare and harmless conditions.)
func (vm *Vm) run(loc uint32, ctx context.Context) {
	if settings.SHOW_RUNTIME {
		println("Running code from", loc)
	}
	// We exit the loop and this function when we perform a `ret` openeration and `stackHeight``
	// equals the length of the callstack.
	stackHeight := len(vm.callstack)
loop:
	for {
		select {
		case <-ctx.Done():
			vm.callstack = vm.callstack[0:stackHeight]
			return
		default:
			if settings.SHOW_RUNTIME {
				println(text.GREEN + vm.DescribeCode(loc) + text.RESET)
			}
			if settings.SHOW_RUNTIME_VALUES {
				print(vm.DescribeOperandValues(loc))
			}
			args := vm.Code[loc].Args
		Switch:
			switch vm.Code[loc].Opcode {
			case Gsql:
				// Arguments:
				// 0: the destination, which ends up as OK or an error, i.e. it's what the command returns.
				// 1: the address of the reference variable: where we put what we get from SQL.
				// 2: the desired type of the result
				// 3: the database
				// 4: the snippet
				// 5: 0 for `as`, 1 for `like`.
				// 6: the token
				rType := vm.Mem[args[2]].V.(values.AbstractType)
				if rType.Len() != 1 {
					vm.Mem[args[0]] = vm.makeError("vm/sql/abstract/c", args[5], vm.DescribeAbstractType(vm.Mem[args[2]].V.(values.AbstractType), LITERAL))
					break Switch
				}
				cType := rType.Types[0]
				dbValue := vm.Mem[args[3]].V.([]values.Value)
				driverNo := dbValue[0].V.(int)
				host := dbValue[1].V.(string)
				port := dbValue[2].V.(int)
				name := dbValue[3].V.(string)
				user := dbValue[4].V.(Secret).secret.V.(string)
				password := dbValue[5].V.(Secret).secret.V.(string)
				connectionString := fmt.Sprintf("host=%v port=%v dbname=%v user=%v password=%v sslmode=disable",
					host, port, name, user, password)
				sqlObj, connectionError := sql.Open(database.SqlDrivers[driverNo], connectionString)
				if connectionError != nil {
					vm.Mem[args[0]] = vm.makeError("vm/sql/connect/a", args[6], connectionError.Error())
					vm.Mem[args[1]] = vm.Mem[args[0]]
					break Switch
				}
				pingError := sqlObj.Ping()
				if pingError != nil {
					vm.Mem[args[0]] = vm.makeError("vm/sql/ping/a", args[6], pingError.Error())
					vm.Mem[args[1]] = vm.Mem[args[0]]
					break Switch
				}
				snippet := vm.Mem[args[4]].V.(values.Snippet).Data
				buf := strings.Builder{}
				vals := make([]values.Value, 0, len(snippet)/2)
				for i, v := range snippet {
					if i%2 == 0 {
						buf.WriteString(v.V.(string))
					} else {
						vals = append(vals, v)
						buf.WriteString("$")
						buf.WriteString(strconv.Itoa(1 + i/2))
					}
				}
				result := vm.evalGetSQL(sqlObj, cType, buf.String(), vals, args[5], args[6])
				vm.Mem[args[1]] = result
				if result.T == values.ERROR {
					vm.Mem[args[0]] = result
				} else {
					vm.Mem[args[0]] = values.OK
				}
			case Psql:
				dbValue := vm.Mem[args[1]].V.([]values.Value)
				driverNo := dbValue[0].V.(int)
				host := dbValue[1].V.(string)
				port := dbValue[2].V.(int)
				name := dbValue[3].V.(string)
				user := dbValue[4].V.(Secret).secret.V.(string)
				password := dbValue[5].V.(Secret).secret.V.(string)
				connectionString := fmt.Sprintf("host=%v port=%v dbname=%v user=%v password=%v sslmode=disable",
					host, port, name, user, password)
				sqlObj, connectionError := sql.Open(database.SqlDrivers[driverNo], connectionString)
				if connectionError != nil {
					vm.Mem[args[0]] = vm.makeError("vm/sql/connect/b", args[3], connectionError.Error())
					break Switch
				}
				pingError := sqlObj.Ping()
				if pingError != nil {
					vm.Mem[args[0]] = vm.makeError("vm/sql/ping/b", args[3], pingError.Error())
					break Switch
				}
				snippet := vm.Mem[args[2]].V.(values.Snippet).Data
				buf := strings.Builder{}
				vals := make([]values.Value, 0, len(snippet)/2)
				for i, v := range snippet {
					if i%2 == 0 {
						buf.WriteString(v.V.(string))
					} else {
						if v.T == values.TYPE {
							if v.V.(values.AbstractType).Len() != 1 {
								vm.Mem[args[0]] = vm.makeError("vm/sql/abstract/d", args[3], vm.DescribeAbstractType(v.V.(values.AbstractType), LITERAL))
								break Switch
							}
							cType := v.V.(values.AbstractType).Types[0]
							sqlSig, err := vm.getTableSigFromStructType(cType, args[3])
							if err.T == values.ERROR {
								vm.Mem[args[0]] = err
								break Switch
							}
							buf.WriteString(sqlSig)
						} else {
							vals = append(vals, v)
							buf.WriteString("$")
							buf.WriteString(strconv.Itoa(1 + i/2))
						}
					}
				}
				vm.Mem[args[0]] = vm.evalPostSQL(sqlObj, buf.String(), vals, args[3])
			case Addf:
				vm.Mem[args[0]] = values.Value{vm.Mem[args[1]].T, vm.Mem[args[1]].V.(float64) + vm.Mem[args[2]].V.(float64)}
			case Addi:
				vm.Mem[args[0]] = values.Value{vm.Mem[args[1]].T, vm.Mem[args[1]].V.(int) + vm.Mem[args[2]].V.(int)}
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
				vm.Mem[args[0]] = values.Value{vm.Mem[args[1]].T, result}
			case AddS:
				result := vm.Mem[args[1]].V.(values.Set)
				result.Union(vm.Mem[args[2]].V.(values.Set))
				vm.Mem[args[0]] = values.Value{vm.Mem[args[1]].T, result}
			case Adds:
				vm.Mem[args[0]] = values.Value{vm.Mem[args[1]].T, vm.Mem[args[1]].V.(string) + vm.Mem[args[2]].V.(string)}
			case Adrr:
				vm.Mem[args[0]] = values.Value{values.STRING, string(vm.Mem[args[1]].V.(rune)) + string(vm.Mem[args[2]].V.(rune))}
			case Adrs:
				vm.Mem[args[0]] = values.Value{values.STRING, string(vm.Mem[args[1]].V.(rune)) + vm.Mem[args[2]].V.(string)}
			case Adsr:
				vm.Mem[args[0]] = values.Value{values.STRING, vm.Mem[args[1]].V.(string) + string(vm.Mem[args[2]].V.(rune))}
			case Adtk:
				vm.Mem[args[0]] = vm.Mem[args[1]]
				vm.Mem[args[0]].V.(*err.Error).AddToTrace(vm.Tokens[args[2]])
			case Andb:
				vm.Mem[args[0]] = values.Value{values.BOOL, vm.Mem[args[1]].V.(bool) && vm.Mem[args[2]].V.(bool)}
			case Aref:
				vm.Mem[vm.Mem[args[0]].V.(uint32)] = vm.Mem[args[1]]
			case Asgm:
				vm.Mem[args[0]] = vm.Mem[args[1]]
			case Auto:
				if vm.logging {
					staticData := vm.Tracking[args[0]]
					newData := TrackingData{staticData.Flavor, staticData.Tok, DUMMY, make([]any, len(staticData.Args))}
					copy(newData.Args, staticData.Args) // This is because only things of tye uint32 are meant to be replaced.
					for i, v := range newData.Args {
						if v, ok := v.(uint32); ok {
							newData.Args[i] = vm.Mem[v]
						}
					}
					prettyString := vm.TrackingToString([]TrackingData{newData})
					switch vm.Mem[staticData.LogToLoc].T {
					case vm.UsefulTypes.Terminal:
						print(text.Pretty(prettyString, 0, 90))
					case vm.UsefulTypes.Output:
						vm.OutHandle.Write(text.Pretty(prettyString, 0, 90))
					case vm.UsefulTypes.File:
						// TODO --- this is obviously very wasteful. Make $_logTo into a constant?
						filename := vm.Mem[staticData.LogToLoc].V.([]values.Value)[0].V.(string)
						path := filename
						if filepath.IsLocal(path) {
							path = filepath.Join(filepath.Dir(staticData.Tok.Source), path)
						}
						var f *os.File
						if _, err := os.Stat(path); os.IsNotExist(err) {
							f, err = os.Create(path)
							if err != nil {
								panic(err)
							}
						} else {
							f, err = os.OpenFile(path, os.O_RDWR|os.O_APPEND, 0660)
							if err != nil {
								panic(err)
							}
						}
						f.WriteString(text.Pretty(prettyString, 0, 90))
					}
				}
			case Call:
				paramNumber := args[1]
				argNumber := 3
				for paramNumber < args[2] {
					v := vm.Mem[args[argNumber]]
					if v.T == values.TUPLE {
						tup := v.V.([]values.Value)
						for ix := 0; ix < len(tup); ix++ {
							vm.Mem[paramNumber] = tup[ix]
							paramNumber++
						}
						argNumber++
					} else {
						vm.Mem[paramNumber] = v
						paramNumber++
						argNumber++
					}
				}
				vm.callstack = append(vm.callstack, loc)
				loc = args[0]
				continue
			case CalT: // A more complicated version which can do vararg or tuple captures. TODO --- the chances of anything needing to do both are infinitessimally rare so maybe two simpler versions, one for varargs and one for tuples?
				paramNumber := args[1]
				argNumber := 3
				tupleOrVarargsData := vm.Mem[args[2]].V.([]uint32)
				var varargsTime bool
				for paramNumber < args[2] {
					torvIndex := paramNumber - args[1]
					if tupleOrVarargsData[torvIndex] == 1 && !varargsTime {
						vm.Mem[paramNumber] = values.Value{values.TUPLE, []values.Value{}}
						varargsTime = true
					}
					if varargsTime && len(args) <= argNumber { // Then we have no more arguments but may be supplying an empty varargs.
						paramNumber++
						continue
					}
					v := vm.Mem[args[argNumber]]
					if v.T == values.TUPLE && tupleOrVarargsData[torvIndex] != 2 { // Then we're exploding a tuple.
						tup := v.V.([]values.Value)
						if varargsTime { // We may be doing a varargs, in which case we suck the whole tuple up into the vararg.
							vararg := vm.Mem[paramNumber].V.([]values.Value)
							vm.Mem[paramNumber].V = append(vararg, tup...)
						} else { // Otherwise we need to explode it and put it into the parameters one at a time unless and untill we run out of them or we meet a varargs.
							for ix := 0; ix < len(tup); ix++ {
								if tupleOrVarargsData[paramNumber-args[1]] == 1 { // The vararg will slurp up what remains of the tuple.
									varargsTime = true
									vm.Mem[paramNumber] = values.Value{values.TUPLE, tup[ix:]}
									break
								}
								vm.Mem[paramNumber] = tup[ix]
								paramNumber++
							}
						}
						argNumber++
					} else { // Otherwise we're not exploding a tuple.
						if varargsTime {
							for (argNumber < len(args)) && vm.Mem[args[argNumber]].T != values.BLING {
								vararg := vm.Mem[paramNumber].V.([]values.Value)
								if vm.Mem[args[argNumber]].T == values.TUPLE {
									vm.Mem[paramNumber].V = append(vararg, vm.Mem[args[argNumber]].V.([]values.Value)...)
								} else {
									vm.Mem[paramNumber].V = append(vararg, vm.Mem[args[argNumber]])
								}
								argNumber++
							}
							varargsTime = false
							paramNumber++
						} else {
							vm.Mem[paramNumber] = v
							paramNumber++
							argNumber++
						}
					}
				}
				vm.callstack = append(vm.callstack, loc)
				loc = args[0]
				continue
			case CasP:
				typeNo := vm.Mem[args[2]].V.(values.AbstractType).Types[0]
				if typeCheck := vm.ConcreteTypeInfo[typeNo].(CloneType).TypeCheck; typeCheck != nil {
					vm.Mem[typeCheck.TokNumberLoc] = values.Value{values.INT, int(args[1])}
					vm.Mem[typeCheck.InLoc] = vm.Mem[args[3]]
					vm.Mem[typeCheck.ResultLoc] = values.Value{typeNo, vm.Mem[args[3]].V}
					vm.run(typeCheck.CallAddress, ctx)
					vm.Mem[args[0]] = vm.Mem[typeCheck.ResultLoc]
				} else {
					vm.Mem[args[0]] = values.Value{typeNo, vm.Mem[args[3]].V}
				}
			case Cast:
				vm.Mem[args[0]] = values.Value{values.ValueType(args[2]), vm.Mem[args[1]].V}
			case Casx:
				castToAbstract := vm.Mem[args[2]].V.(values.AbstractType)
				if len(castToAbstract.Types) != 1 {
					vm.Mem[args[0]] = vm.makeError("vm/cast/concrete", args[3], args[1], args[2])
					break Switch
				}
				targetType := castToAbstract.Types[0]
				currentType := vm.Mem[args[1]].T
				if targetType == currentType {
					vm.Mem[args[0]] = vm.Mem[args[1]]
					break Switch
				}
				if enumInfo, ok := vm.ConcreteTypeInfo[targetType].(EnumType); ok && currentType == values.INT {
					if vm.Mem[args[1]].V.(int) >= len(enumInfo.ElementNames) || vm.Mem[args[1]].V.(int) < 0 {
						vm.Mem[args[0]] = vm.makeError("vm/cast/enum", args[3], args[1], args[2])
						break Switch
					}
					vm.Mem[args[0]] = values.Value{targetType, vm.Mem[args[1]].V.(int)}
					break Switch
				}
				if structInfo, ok := vm.ConcreteTypeInfo[targetType].(StructType); ok && currentType == values.LIST {
					elements := vm.Mem[args[1]].V.(vector.Vector)
					if elements.Len() != len(structInfo.AbstractStructFields) {
						vm.Mem[args[0]] = vm.makeError("vm/cast/fields", args[3], args[1], args[2])
						break Switch
					}
					fields := make([]values.Value, elements.Len())
					for i := 0; i < elements.Len(); i++ {
						el, _ := elements.Index(i)
						if !structInfo.AbstractStructFields[i].Contains(el.(values.Value).T) {
							vm.Mem[args[0]] = vm.makeError("vm/cast/types", args[3], args[1], args[2])
							break Switch
						}
						fields[i] = el.(values.Value)
					}
					vm.Mem[args[0]] = values.Value{targetType, fields}
					break Switch
				}
				if cloneInfoForCurrentType, ok := vm.ConcreteTypeInfo[currentType].(CloneType); ok {
					if cloneInfoForCurrentType.Parent == targetType {
						vm.Mem[args[0]] = values.Value{targetType, vm.Mem[args[1]].V}
						break Switch
					}
					if cloneInfoForTargetType, ok := vm.ConcreteTypeInfo[currentType].(CloneType); ok && cloneInfoForTargetType.Parent == cloneInfoForCurrentType.Parent {
						vm.Mem[args[0]] = values.Value{targetType, vm.Mem[args[1]].V}
						break Switch
					}
				}
				// Otherwise by elimination the current type is the parent and the target type is a clone, or we have an error.
				if cloneInfoForTargetType, ok := vm.ConcreteTypeInfo[targetType].(CloneType); ok && cloneInfoForTargetType.Parent == currentType {
					vm.Mem[args[0]] = values.Value{targetType, vm.Mem[args[1]].V}
					break Switch
				}
				vm.Mem[args[0]] = vm.makeError("vm/cast", args[3], args[1], args[2])
			case Cc11:
				vm.Mem[args[0]] = values.Value{values.TUPLE, []values.Value{vm.Mem[args[1]], vm.Mem[args[2]]}}
			case Cc1T:
				vm.Mem[args[0]] = values.Value{values.TUPLE, append([]values.Value{vm.Mem[args[1]]}, vm.Mem[args[2]].V.([]values.Value)...)}
			case CcT1:
				vm.Mem[args[0]] = values.Value{values.TUPLE, append(vm.Mem[args[1]].V.([]values.Value), vm.Mem[args[2]])}
			case CcTT:
				vm.Mem[args[0]] = values.Value{values.TUPLE, append(vm.Mem[args[1]].V.([]values.Value), vm.Mem[args[2]].V.([]values.Value)...)}
			case Ccxx:
				if vm.Mem[args[1]].T == values.TUPLE {
					if vm.Mem[args[2]].T == values.TUPLE {
						vm.Mem[args[0]] = values.Value{values.TUPLE, append(vm.Mem[args[1]].V.([]values.Value), vm.Mem[args[2]].V.([]values.Value)...)}
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
			case Chck:
				// Arguments:
				// args[0] : memory location of result.
				// args[1] : evaluation of the type condition, presumptively boolean.
				// args[2] : memory location containing an int which is the number of the
				// token of the calling constructor.
				// args[3] : the ordinal of the data needed to hold the error message.
				switch vm.Mem[args[1]].T {
				case values.BOOL:
					if !(vm.Mem[args[1]].V.(bool)) {
						tokNumber := uint32(vm.Mem[args[2]].V.(int))
						errorInfo := vm.TypeCheckErrors[args[3]]
						vm.Mem[args[0]] = vm.makeError("vm/typecheck/fail", tokNumber,
							errorInfo.Condition, errorInfo.Type, errorInfo.Tok, errorInfo.Value)
						loc = vm.callstack[len(vm.callstack)-1]
						vm.callstack = vm.callstack[0 : len(vm.callstack)-1]
					}
				case values.ERROR:
					vm.Mem[args[0]] = vm.Mem[args[1]]
				default:
					tokNumber := uint32(vm.Mem[args[2]].V.(int))
					errorInfo := vm.TypeCheckErrors[args[3]]
					vm.Mem[args[0]] = vm.makeError("vm/typecheck/bool", tokNumber,
						errorInfo.Condition, errorInfo.Type, errorInfo.Tok,
						vm.DescribeType(vm.Mem[args[1]].T, LITERAL), vm.Mem[args[1]], errorInfo.Tok)
					loc = vm.callstack[len(vm.callstack)-1]
					vm.callstack = vm.callstack[0 : len(vm.callstack)-1]
				}
			case Chrf: // If a reference variable is an error at return time, we need to substitute the error for `OK` as the return value.
				if vm.Mem[vm.Mem[args[1]].V.(uint32)].T == values.ERROR {
					vm.Mem[args[0]] = vm.Mem[vm.Mem[args[1]].V.(uint32)]
				}
			case Clon:
				if vm.Mem[args[1]].T != values.TYPE {
					vm.Mem[args[0]] = vm.makeError("vm/clones/type", args[1])
					break Switch
				}
				abType := values.AbstractType{}
				for _, v := range vm.Mem[args[1]].V.(values.AbstractType).Types {
					clones := vm.ConcreteTypeInfo[v].IsClonedBy()
					abType = abType.Union(clones)
				}
				vm.Mem[args[0]] = values.Value{values.TYPE, abType}
			case CoSn:
				vm.Mem[args[0]] = values.Value{values.SNIPPET, values.Snippet{vm.Mem[args[1]].V.([]values.Value), nil}}
			case Cpnt:
				vm.Mem[args[0]] = values.Value{values.INT, int(vm.Mem[args[1]].V.(rune))}
			case Cv1T:
				vm.Mem[args[0]] = values.Value{values.TUPLE, []values.Value{vm.Mem[args[1]]}}
			case CvTT:
				slice := []values.Value{}
				for i := 1; i < len(args); i++ {
					if vm.Mem[args[i]].T == values.TUPLE {
						slice = append(slice, vm.Mem[args[i]].V.([]values.Value)...)
					} else {
						slice = append(slice, vm.Mem[args[i]])
					}
				}
				vm.Mem[args[0]] = values.Value{values.TUPLE, slice}
			case Diif:
				divisor := vm.Mem[args[2]].V.(int)
				if divisor == 0 {
					vm.Mem[args[0]] = vm.makeError("vm/div/zero/a", args[3])
				} else {
					vm.Mem[args[0]] = values.Value{values.FLOAT, float64(vm.Mem[args[1]].V.(int)) / float64(divisor)}
				}
			case Divf:
				divisor := vm.Mem[args[2]].V.(float64)
				if divisor == 0 {
					vm.Mem[args[0]] = vm.makeError("vm/div/zero/b", args[3])
				} else {
					vm.Mem[args[0]] = values.Value{values.FLOAT, vm.Mem[args[1]].V.(float64) / divisor}
				}
			case Divi:
				divisor := vm.Mem[args[2]].V.(int)
				if divisor == 0 {
					vm.Mem[args[0]] = vm.makeError("vm/div/zero/c", args[3])
				} else {
					vm.Mem[args[0]] = values.Value{values.INT, vm.Mem[args[1]].V.(int) / vm.Mem[args[2]].V.(int)}
				}
			case Dvfi:
				divisor := vm.Mem[args[2]].V.(int)
				if divisor == 0 {
					vm.Mem[args[0]] = vm.makeError("vm/div/zero/d", args[3])
				} else {
					vm.Mem[args[0]] = values.Value{values.FLOAT, vm.Mem[args[1]].V.(float64) / float64(divisor)}
				}
			case Dvif:
				divisor := vm.Mem[args[2]].V.(float64)
				if divisor == 0 {
					vm.Mem[args[0]] = vm.makeError("vm/div/zero/e", args[3])
				} else {
					vm.Mem[args[0]] = values.Value{values.FLOAT, float64(vm.Mem[args[1]].V.(int)) / divisor}
				}
			case Dofn:
				lambda := vm.Mem[args[1]].V.(Lambda)
				// The case where the lambda is from a Go function.
				if lambda.Gocode != nil {
					goArgs := []reflect.Value{}
					for _, pfMemLoc := range args[2:] {
						pfArg := vm.Mem[pfMemLoc]
						goArg, ok := vm.pipefishToGo(pfArg)
						if !ok {
							vm.Mem[args[0]] = values.Value{values.ERROR, err.CreateErr("vm/func/go", lambda.Tok, goArg)} // If the conversion failed, the goArg will be the value it couldn't convert.
							break Switch
						}
						goArgs = append(goArgs, reflect.ValueOf(goArg))
					}
					goResultValues := lambda.Gocode.Call(goArgs)
					var doctoredValues any
					if len(goResultValues) == 1 {
						doctoredValues = goResultValues[0].Interface()
					} else {
						elements := make([]any, 0, len(goResultValues))
						for _, v := range goResultValues {
							elements = append(elements, v.Interface())
						}
						doctoredValues = goTuple(elements)
					}
					val := vm.goToPipefish(reflect.ValueOf(doctoredValues))
					if val.T == 0 {
						payload := val.V.([]any)
						newError := err.CreateErr(payload[0].(string), vm.Mem[args[1]].V.(*err.Error).Token, payload[1:]...)
						vm.Mem[args[0]] = values.Value{values.ERROR, newError}
						break
					}
					if val.T == values.ERROR {
						val.V.(*err.Error).Token = vm.Mem[args[1]].V.(*err.Error).Token
					}
					vm.Mem[args[0]] = val
					break Switch
				}
				// The normal case.
				// The code here is repeated with a few twists in a very non-DRY in `vmgo` and any changes necessary here will probably need to be copied there.
				if len(args)-2 != len(lambda.Sig) { // TODO: variadics.
					vm.Mem[args[0]] = values.Value{values.ERROR, err.CreateErr("vm/func/args", lambda.Tok)}
					break Switch
				}
				for i := 0; i < int(lambda.CapturesEnd-lambda.CapturesStart); i++ {
					vm.Mem[int(lambda.CapturesStart)+i] = lambda.Captures[i]
				}
				for i := 0; i < int(lambda.ParametersEnd-lambda.CapturesEnd); i++ {
					vm.Mem[int(lambda.CapturesEnd)+i] = vm.Mem[args[2+i]]
				}
				success := true
				if lambda.Sig != nil {
					for i, abType := range lambda.Sig { // TODO --- as with other such cases there will be a threshold at which linear search becomes inferior to binary search and we should find out what it is.
						success = false
						if abType.Types == nil { // Used for `any?`.
							success = true
							continue
						} else {
							for _, ty := range abType.Types {
								if ty == vm.Mem[int(lambda.CapturesEnd)+i].T {
									success = true
									if vm.Mem[int(lambda.CapturesEnd)+i].T == values.STRING && len(vm.Mem[int(lambda.CapturesEnd)+i].V.(string)) > abType.Len() {
										success = false
									}
								}
							}
						}
						if !success {
							vm.Mem[args[0]] = values.Value{values.ERROR, err.CreateErr("vm/func/types", lambda.Tok)}
							break Switch
						}
					}
				}
				vm.run(lambda.AddressToCall, ctx)
				vm.Mem[args[0]] = vm.Mem[lambda.ResultLocation]
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
			case Extn:
				externalOrdinal := args[1]
				operatorType := args[2]
				remainingNamespace := vm.Mem[args[3]].V.(string)
				name := vm.Mem[args[4]].V.(string)
				argLocs := args[5:]
				lastWasBling := false
				var buf strings.Builder
				if operatorType == PREFIX || operatorType == UNFIX {
					buf.WriteString(remainingNamespace)
					buf.WriteString(name)
					lastWasBling = true
				}
				if operatorType == PREFIX {
					if len(argLocs) == 0 {
						buf.WriteString("(")
					}
					lastWasBling = len(argLocs) > 0
				}
				if operatorType == INFIX || operatorType == SUFFIX {
					buf.WriteString("(")
				}
				for i, loc := range argLocs {
					serializedValue := vm.Literal(vm.Mem[loc])
					if operatorType == INFIX && vm.Mem[loc].T == values.BLING && serializedValue == name { // Then we need to attach the namespace to the operator.
						buf.WriteString(remainingNamespace)
					}
					if vm.Mem[loc].T == values.BLING {
						if !lastWasBling {
							buf.WriteString(")")
						}
						buf.WriteString(" ")
						buf.WriteString(serializedValue)
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
					lastWasBling = false
					buf.WriteString(serializedValue)
				}
				if !lastWasBling {
					buf.WriteString(")")
				}
				if operatorType == SUFFIX {
					buf.WriteString(remainingNamespace)
					buf.WriteString(name)
				}
				vm.Mem[args[0]] = vm.ExternalCallHandlers[externalOrdinal].Evaluate(buf.String())
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
				F := vm.GoFns[args[2]]
				goTpl := make([]reflect.Value, 0, len(args))
				for _, v := range args[3:] { // TODO --- how can this be right? Surely they should be stored in a TUPLE.
					el := vm.Mem[v]
					goVal, ok := vm.pipefishToGo(el)
					if !ok {
						newError := err.CreateErr("vm/pipefish/type", vm.Mem[args[1]].V.(*err.Error).Token, vm.DescribeType(el.T, LITERAL))
						newError.Values = []values.Value{el}
						vm.Mem[args[0]] = values.Value{values.ERROR, newError}
						break
					}
					goTpl = append(goTpl, reflect.ValueOf(goVal))
				}
				var goResultValues []reflect.Value
				goResultValues = F.Code.Call(goTpl)
				var doctoredValues any
				if len(goResultValues) == 1 {
					doctoredValues = goResultValues[0].Interface()
				} else {
					elements := make([]any, 0, len(goResultValues))
					for _, v := range goResultValues {
						elements = append(elements, v.Interface())
					}
					doctoredValues = goTuple(elements)
				}
				val := vm.goToPipefish(reflect.ValueOf(doctoredValues))
				if val.T == 0 {
					payload := val.V.([]any)
					newError := err.CreateErr(payload[0].(string), vm.Mem[args[1]].V.(*err.Error).Token, payload[1:]...)
					vm.Mem[args[0]] = values.Value{values.ERROR, newError}
					break
				}
				vm.Mem[args[0]] = val
			case Gtef:
				vm.Mem[args[0]] = values.Value{values.BOOL, vm.Mem[args[1]].V.(float64) >= vm.Mem[args[2]].V.(float64)}
			case Gtei:
				vm.Mem[args[0]] = values.Value{values.BOOL, vm.Mem[args[1]].V.(int) >= vm.Mem[args[2]].V.(int)}
			case Gthf:
				vm.Mem[args[0]] = values.Value{values.BOOL, vm.Mem[args[1]].V.(float64) > vm.Mem[args[2]].V.(float64)}
			case Gthi:
				vm.Mem[args[0]] = values.Value{values.BOOL, vm.Mem[args[1]].V.(int) > vm.Mem[args[2]].V.(int)}
			case IctS:
				leftSet := vm.Mem[args[1]].V.(values.Set)
				result := leftSet.Intersect(vm.Mem[args[2]].V.(values.Set))
				vm.Mem[args[0]] = values.Value{vm.Mem[args[1]].T, result}
			case IdxL:
				vec := vm.Mem[args[1]].V.(vector.Vector)
				ix := vm.Mem[args[2]].V.(int)
				val, ok := vec.Index(ix)
				if !ok {
					vm.Mem[args[0]] = vm.makeError("vm/index/list", args[3], ix, vec.Len(), args[1], args[2])
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
					vm.Mem[args[0]] = vm.makeError("vm/index/pair", args[3])
				}
			case Idxs:
				str := vm.Mem[args[1]].V.(string)
				ix := vm.Mem[args[2]].V.(int)
				ok := 0 <= ix && ix < len(str)
				if ok {
					val := values.Value{values.RUNE, rune(str[ix])}
					vm.Mem[args[0]] = val
				} else {
					vm.Mem[args[0]] = vm.makeError("vm/index/string", args[3], ix, len(str), args[1], args[2])
				}
			case IdxT:
				tuple := vm.Mem[args[1]].V.([]values.Value)
				ix := vm.Mem[args[2]].V.(int)
				ok := 0 <= ix && ix < len(tuple)
				if ok {
					vm.Mem[args[0]] = tuple[ix]
				} else {
					vm.Mem[args[0]] = vm.makeError("vm/index/tuple", args[3], ix, len(tuple), args[1], args[2])
				}
			case Inpt:
				if _, ok := vm.InHandle.(*StandardInHandler); ok {
					vm.InHandle = &StandardInHandler{vm.Mem[args[1]].V.([]values.Value)[0].V.(string)}
				}
				vm.Mem[args[0]] = values.Value{values.STRING, vm.InHandle.Get()}
			case Inte:
				vm.Mem[args[0]] = values.Value{values.INT, vm.Mem[args[1]].V.(int)}
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
				for _, t := range vm.Mem[args[2]].V.(values.AbstractType).Types {
					if vm.Mem[args[1]].T == t {
						vm.Mem[args[0]] = values.Value{values.BOOL, true}
					}
				}
			case Itgk:
				vm.Mem[args[0]] = vm.Mem[args[1]].V.(values.Iterator).GetKey()
			case Itkv:
				vm.Mem[args[0]], vm.Mem[args[1]] = vm.Mem[args[2]].V.(values.Iterator).GetKeyValuePair()
			case Itgv:
				vm.Mem[args[0]] = vm.Mem[args[1]].V.(values.Iterator).GetValue()
			case Itor:
				vm.Mem[args[0]] = values.Value{values.RUNE, rune(vm.Mem[args[1]].V.(int))}
			case IxSn:
				vm.Mem[args[0]] = vm.Mem[args[1]].V.(values.Snippet).Data[vm.Mem[args[2]].V.(int)]
			case IxTn:
				vm.Mem[args[0]] = vm.Mem[args[1]].V.([]values.Value)[args[2]]
			case IxXx:
				container := vm.Mem[args[1]]
				if container.T == values.ERROR {
					vm.Mem[args[0]] = container
					break Switch
				}
				index := vm.Mem[args[2]]
				if index.T == values.ERROR {
					vm.Mem[args[0]] = index
					break Switch
				}
				indexType := index.T
				if cloneInfo, ok := vm.ConcreteTypeInfo[indexType].(CloneType); ok {
					indexType = cloneInfo.Parent
				}
				if indexType == values.PAIR { // Then we're slicing.
					containerType := container.T
					if cloneInfo, ok := vm.ConcreteTypeInfo[containerType].(CloneType); ok && cloneInfo.IsSliceable {
						containerType = cloneInfo.Parent
					}
					ix := vm.Mem[args[2]].V.([]values.Value)
					if ix[0].T != values.INT {
						vm.Mem[args[0]] = vm.makeError("vm/index/a", args[3], vm.DescribeType(ix[0].T, LITERAL))
						break Switch
					}
					if ix[1].T != values.INT {
						vm.Mem[args[0]] = vm.makeError("vm/index/b", args[3], vm.DescribeType(ix[1].T, LITERAL))
						break Switch
					}
					if ix[0].V.(int) < 0 {
						vm.Mem[args[0]] = vm.makeError("vm/index/c", args[3], ix[0].V.(int))
						break Switch
					}
					if ix[1].V.(int) < ix[0].V.(int) {
						vm.Mem[args[0]] = vm.makeError("vm/index/d", args[3], ix[0].V.(int), ix[1].V.(int))
						break Switch
					}
					// We switch on the type of the lhs.
					switch container.T {
					case values.LIST:
						vec := vm.Mem[args[1]].V.(vector.Vector)
						if ix[1].V.(int) > vec.Len() {
							vm.Mem[args[0]] = vm.makeError("vm/index/e", args[3], ix[1], vec.Len(), args[1], args[2])
							break Switch
						}
						vm.Mem[args[0]] = values.Value{vm.Mem[args[1]].T, vec.SubVector(ix[0].V.(int), ix[1].V.(int))}
					case values.STRING:
						str := container.V.(string)
						ix := index.V.([]values.Value)
						if ix[1].V.(int) > len(str) {
							vm.Mem[args[0]] = vm.makeError("vm/index/f", args[3], ix[1], len(str), args[1], args[2])
							break Switch
						}
						vm.Mem[args[0]] = values.Value{vm.Mem[args[1]].T, str[ix[0].V.(int):ix[1].V.(int)]}
					case values.TUPLE:
						tup := container.V.([]values.Value)
						if ix[1].V.(int) > len(tup) {
							vm.Mem[args[0]] = vm.makeError("vm/index/r", args[3], ix[1].V.(int))
							break Switch
						}
						vm.Mem[args[0]] = values.Value{values.TUPLE, tup[ix[0].V.(int):ix[1].V.(int)]}
					default:
						vm.Mem[args[0]] = vm.makeError("vm/index/g", args[3])
						break Switch
					}
				} else {
					// Otherwise it's not a slice. We switch on the type of the lhs.
					containerType := container.T
					if cloneInfo, ok := vm.ConcreteTypeInfo[containerType].(CloneType); ok {
						containerType = cloneInfo.Parent
					}
					typeInfo := vm.ConcreteTypeInfo[containerType]
					if typeInfo.IsStruct() {
						ix := typeInfo.(StructType).Resolve(vm.Mem[args[2]].V.(int))
						if ix == -1 {
							vm.Mem[args[0]] = vm.makeError("vm/index/t", args[3], typeInfo.(StructType).Name, vm.Labels[vm.Mem[args[2]].V.(int)])
						} else {
							vm.Mem[args[0]] = vm.Mem[args[1]].V.([]values.Value)[ix]
						}
						break
					}
					if containerType == values.MAP {
						mp := container.V.(*values.Map)
						ix := vm.Mem[args[2]]
						result, ok := mp.Get(ix)
						if !ok {
							vm.Mem[args[0]] = vm.makeError("vm/index/h", args[3], vm.DefaultDescription(vm.Mem[args[2]]), args[1], args[2])
						} else {
							vm.Mem[args[0]] = result
						}
						break
					}
					if indexType != values.INT {
						vm.Mem[args[0]] = vm.makeError("vm/index/i", args[3], vm.DescribeType(vm.Mem[args[1]].T, LITERAL), vm.DescribeType(vm.Mem[args[2]].T, LITERAL), args[1], args[2])
						break
					}
					ty := container.T
					if cloneInfo, ok := vm.ConcreteTypeInfo[container.T].(CloneType); ok {
						ty = cloneInfo.Parent
					}
					switch ty {
					case values.LIST:
						vec := container.V.(vector.Vector)
						ix := index.V.(int)
						val, ok := vec.Index(ix)
						if !ok {
							vm.Mem[args[0]] = vm.makeError("vm/index/j", args[3], ix, vec.Len(), args[1], args[2])
						} else {
							vm.Mem[args[0]] = val.(values.Value)
						}
						break Switch
					case values.PAIR:
						pair := container.V.([]values.Value)
						ix := index.V.(int)
						ok := ix == 0 || ix == 1
						if ok {
							vm.Mem[args[0]] = pair[ix]
						} else {
							vm.Mem[args[0]] = vm.makeError("vm/index/k", args[3], ix)
						}
						break Switch
					case values.SNIPPET:
						snippetData := container.V.(values.Snippet).Data
						ix := index.V.(int)
						ok := 0 <= ix && ix < len(snippetData)
						if ok {
							vm.Mem[args[0]] = snippetData[ix]
						} else {
							vm.Mem[args[0]] = vm.makeError("vm/index/s", args[3], ix, len(snippetData), args[1], args[2])
						}
						break Switch
					case values.STRING:
						str := container.V.(string)
						ix := index.V.(int)
						ok := 0 <= ix && ix < len(str)
						if ok {
							val := values.Value{values.RUNE, rune(str[ix])}
							vm.Mem[args[0]] = val
						} else {
							vm.Mem[args[0]] = vm.makeError("vm/index/l", args[3], ix, len(str), args[1], args[2])
						}
						break Switch
					case values.TUPLE:
						tuple := container.V.([]values.Value)
						ix := index.V.(int)
						ok := 0 <= ix && ix < len(tuple)
						if ok {
							vm.Mem[args[0]] = tuple[ix]
						} else {
							vm.Mem[args[0]] = vm.makeError("vm/index/m", args[3], ix, len(tuple), args[1], args[2])
						}
						break Switch
					default:
						vm.Mem[args[0]] = vm.makeError("vm/index/q", args[3], vm.DescribeType(vm.Mem[args[1]].T, LITERAL), vm.DescribeType(vm.Mem[args[2]].T, LITERAL))
						break Switch
					}
				}
			case IxZl:
				typeInfo := vm.ConcreteTypeInfo[vm.Mem[args[1]].T].(StructType)
				ix := typeInfo.Resolve(vm.Mem[args[2]].V.(int))
				if ix == -1 {
					vm.Mem[args[0]] = vm.makeError("vm/index/u", args[3], vm.DescribeType(vm.Mem[args[1]].T, LITERAL), vm.DefaultDescription(vm.Mem[args[2]]))
					continue
				}
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
				for _, labelNumber := range vm.ConcreteTypeInfo[vm.Mem[args[1]].T].(StructType).LabelNumbers {
					result = result.Conj(values.Value{values.LABEL, labelNumber})
				}
				vm.Mem[args[0]] = values.Value{values.LIST, result}
			case Lbls:
				stringToConvert := vm.Mem[args[1]].V.(string)
				labelNo, ok := vm.FieldLabelsInMem[stringToConvert]
				if ok {
					vm.Mem[args[0]] = vm.Mem[labelNo]
				} else {
					vm.Mem[args[0]] = vm.makeError("vm/label/exist", args[2], stringToConvert)
				}
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
			case LnSn:
				vm.Mem[args[0]] = values.Value{values.INT, len(vm.Mem[args[1]].V.(values.Snippet).Data)}
			case Logn:
				vm.logging = false
			case Logy:
				vm.logging = true
			case MkEn:
				info := vm.ConcreteTypeInfo[args[1]].(EnumType)
				ix := vm.Mem[args[2]].V.(int)
				ok := 0 <= ix && ix < len(info.ElementNames)
				if ok {
					vm.Mem[args[0]] = values.Value{values.ValueType(args[1]), ix}
				} else {
					vm.Mem[args[0]] = vm.makeError("vm/index/p", args[3], info.GetName(LITERAL), ix)
				}
			case Mker:
				vm.Mem[args[0]] = values.Value{values.ERROR, &err.Error{ErrorId: "vm/user", Message: vm.Mem[args[1]].V.(string), Token: vm.Tokens[args[2]]}}
			case Mkfn:
				lf := vm.LambdaFactories[args[1]]
				newLambda := *lf.Model
				newLambda.Captures = make([]values.Value, len(lf.CaptureLocations))
				for i, v := range lf.CaptureLocations {
					val := vm.Mem[v]
					if val.T == values.THUNK {
						vm.run(val.V.(uint32), ctx)
						val = vm.Mem[v]
					}
					newLambda.Captures[i] = val
				}
				vm.Mem[args[0]] = values.Value{values.FUNC, newLambda}
			case Mkit:
				vm.Mem[args[0]] = vm.NewIterator(vm.Mem[args[1]], args[2] == 1, args[3])
			case Mkmp:
				result := &values.Map{}
				for _, p := range vm.Mem[args[1]].V.([]values.Value) {
					if p.T != values.PAIR {
						vm.Mem[args[0]] = vm.makeError("vm/map/pair", args[2], p, vm.DescribeType(p.T, LITERAL))
						break Switch
					}
					k := p.V.([]values.Value)[0]
					v := p.V.([]values.Value)[1]
					if !((values.NULL <= k.T && k.T < values.PAIR) || vm.ConcreteTypeInfo[k.T].IsEnum() || // TODO, we can just have a simple filter and/or a method of the interface.
						vm.ConcreteTypeInfo[k.T].IsStruct()) { // Or hand it off to the Set method to return an error.
						vm.Mem[args[0]] = vm.makeError("vm/map/key", args[2], k, vm.DescribeType(k.T, LITERAL))
						break Switch
					}
					result = result.Set(k, v)
				}
				vm.Mem[args[0]] = values.Value{values.MAP, result}
			case Mkpr:
				vm.Mem[args[0]] = values.Value{values.PAIR, []values.Value{vm.Mem[args[1]], vm.Mem[args[2]]}}
			case MkSc:
				vm.Mem[args[0]] = values.Value{values.SECRET, Secret{vm.Mem[args[1]]}}
			case Mkst:
				result := values.Set{}
				for _, v := range vm.Mem[args[1]].V.([]values.Value) {
					// TODO --- whether a type can be put in a set should be extractable from its concrete type information.
					result = result.Add(v)
				}
				vm.Mem[args[0]] = values.Value{values.SET, result}
			case MkSn:
				sFac := vm.SnippetFactories[args[1]]
				vals := make([]values.Value, len(sFac.Bindle.ValueLocs))
				for i, v := range sFac.Bindle.ValueLocs {
					vals[i] = vm.Mem[v]
				}
				vm.Mem[args[0]] = values.Value{values.SNIPPET, values.Snippet{vals, sFac.Bindle}}
			case Mlfi:
				vm.Mem[args[0]] = values.Value{values.FLOAT, vm.Mem[args[1]].V.(float64) * float64(vm.Mem[args[2]].V.(int))}
			case Modi:
				divisor := vm.Mem[args[2]].V.(int)
				if divisor == 0 {
					vm.Mem[args[0]] = vm.makeError("vm/mod/int", args[3])
				} else {
					vm.Mem[args[0]] = values.Value{vm.Mem[args[1]].T, vm.Mem[args[1]].V.(int) % vm.Mem[args[2]].V.(int)}
				}
			case Mpar:
				vals := []values.Value{}
				for _, loc := range args[3:] {
					vals = append(vals, vm.Mem[loc])
				}
				entry, ok := vm.ParameterizedTypeInfo[args[1]].Get(values.Value{values.TUPLE, vals})
				if !ok {
					argsAsAny := []any{}
					for _, v := range vals {
						argsAsAny = append(argsAsAny, v)
					}
					vm.Mem[args[0]] = vm.makeError("vm/param/exist", args[2], argsAsAny...)
				} else {
					vm.Mem[args[0]] = entry
				}
			case Mulf:
				vm.Mem[args[0]] = values.Value{vm.Mem[args[1]].T, vm.Mem[args[1]].V.(float64) * vm.Mem[args[2]].V.(float64)}
			case Muli:
				vm.Mem[args[0]] = values.Value{vm.Mem[args[1]].T, vm.Mem[args[1]].V.(int) * vm.Mem[args[2]].V.(int)}
			case Negf:
				vm.Mem[args[0]] = values.Value{vm.Mem[args[1]].T, -vm.Mem[args[1]].V.(float64)}
			case Negi:
				vm.Mem[args[0]] = values.Value{vm.Mem[args[1]].T, -vm.Mem[args[1]].V.(int)}
			case Notb:
				vm.Mem[args[0]] = values.Value{values.BOOL, !vm.Mem[args[1]].V.(bool)}
			case Orb:
				vm.Mem[args[0]] = values.Value{values.BOOL, (vm.Mem[args[1]].V.(bool) || vm.Mem[args[2]].V.(bool))}
			case Outp:
				vm.OutHandle.Out(vm.Mem[args[0]])
				vm.PostHappened = true
			case Outt:
				if vm.Mem[vm.UsefulValues.OutputAs].V.(int) == 0 {
					fmt.Println(vm.Literal(vm.Mem[args[0]]))
				} else {
					fmt.Println(vm.DefaultDescription(vm.Mem[args[0]]))
				}
			case Qabt:
				for _, t := range args[1 : len(args)-1] {
					if vm.Mem[args[0]].T == values.ValueType(t) {
						loc = loc + 1
						continue loop
					}
				}
				loc = args[len(args)-1]
				continue
			case Qfls:
				if vm.Mem[args[0]].V.(bool) {
					loc = args[1]
				} else {
					loc = loc + 1
				}
				continue
			case Qitr:
				if vm.Mem[args[0]].V.(values.Iterator).Unfinished() {
					loc = args[1]
				} else {
					loc = loc + 1

				}
				continue
			case QleT:
				if vm.Mem[args[0]].T == values.TUPLE && len(vm.Mem[args[0]].V.([]values.Value)) <= int(args[1]) {
					loc = loc + 1
				} else {
					loc = args[2]
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
				if vm.Mem[args[0]].T != values.UNSATISFIED_CONDITIONAL {
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
			case Qtpt:
				vals := vm.Mem[args[0]].V.([]values.Value)
				slice := []values.Value{}
				if int(args[1]) <= len(vals) {
					slice = vals[args[1]:]
				}
				for _, v := range slice {
					var found bool
					for _, t := range args[2 : len(args)-1] {
						if v.T == values.ValueType(t) {
							found = true
							break
						}
					}
					if !found {
						loc = args[len(args)-1]
						continue loop
					}
				}
				loc = loc + 1
				continue
			case Qtru:
				if vm.Mem[args[0]].V.(bool) {
					loc = loc + 1
				} else {
					loc = args[1]
				}
				continue
			case Qtyl:
				if vm.Mem[args[0]].T == vm.Mem[args[1]].V.(values.AbstractType).Types[0] {
					loc = loc + 1
				} else {
					loc = args[2]
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
				if len(vm.callstack) == stackHeight { // This is so that we can call "Run" when we have things on the stack and it will bottom out at the appropriate time.
					break loop
				}
				loc = vm.callstack[len(vm.callstack)-1]
				vm.callstack = vm.callstack[0 : len(vm.callstack)-1]
			case Rpop:
				rData := vm.recursionStack[len(vm.recursionStack)-1]
				vm.recursionStack = vm.recursionStack[:len(vm.recursionStack)-1]
				copy(vm.Mem[rData.loc:int(rData.loc)+len(rData.mems)], rData.mems)
			case Rpsh:
				lowLoc := args[0]
				highLoc := args[1]
				memToSave := make([]values.Value, highLoc-lowLoc)
				copy(memToSave, vm.Mem[lowLoc:highLoc])
				vm.recursionStack = append(vm.recursionStack, recursionData{memToSave, lowLoc})
			case Rsit:
				vm.Mem[args[0]].V.(values.Iterator).Reset()
			case SliL:
				vec := vm.Mem[args[1]].V.(vector.Vector)
				ix := vm.Mem[args[2]].V.([]values.Value)
				if ix[0].T != values.INT {
					vm.Mem[args[0]] = vm.makeError("vm/slice/list/a", args[3], vm.DescribeType(ix[0].T, LITERAL), args[1], args[2])
					break Switch
				}
				if ix[1].T != values.INT {
					vm.Mem[args[0]] = vm.makeError("vm/slice/list/b", args[3], vm.DescribeType(ix[1].T, LITERAL), args[1], args[2])
					break Switch
				}
				if ix[0].V.(int) < 0 {
					vm.Mem[args[0]] = vm.makeError("vm/slice/list/c", args[3], vec, ix)
					break Switch
				}
				if ix[1].V.(int) < ix[0].V.(int) {
					vm.Mem[args[0]] = vm.makeError("vm/slice/list/d", args[3], ix[0].V.(int), ix[1].V.(int), args[1], args[2])
					break Switch
				}
				if vec.Len() < ix[1].V.(int) {
					vm.Mem[args[0]] = vm.makeError("vm/slice/list/e", args[3], ix[1].V.(int), vec.Len(), args[1], args[2])
					break Switch
				}
				vm.Mem[args[0]] = values.Value{values.LIST, vec.SubVector(ix[0].V.(int), ix[1].V.(int))}
			case Slis:
				str := vm.Mem[args[1]].V.(string)
				ix := vm.Mem[args[2]].V.([]values.Value)
				if ix[0].T != values.INT {
					vm.Mem[args[0]] = vm.makeError("vm/slice/string/a", args[3], vm.DescribeType(ix[0].T, LITERAL), args[1], args[2])
					break Switch
				}
				if ix[1].T != values.INT {
					vm.Mem[args[0]] = vm.makeError("vm/slice/string/b", args[3], vm.DescribeType(ix[1].T, LITERAL), args[1], args[2])
					break Switch
				}
				if ix[0].V.(int) < 0 {
					vm.Mem[args[0]] = vm.makeError("vm/slice/string/c", args[3], args[1], args[2])
					break Switch
				}
				if ix[1].V.(int) < ix[0].V.(int) {
					vm.Mem[args[0]] = vm.makeError("vm/slice/string/d", args[3], ix[0].V.(int), ix[1].V.(int), args[1], args[2])
					break Switch
				}
				if len(str) < ix[1].V.(int) {
					vm.Mem[args[0]] = vm.makeError("vm/slice/string/e", args[3], ix[1].V.(int), len(str), args[1], args[2])
					break Switch
				}
				vm.Mem[args[0]] = values.Value{values.STRING, str[ix[0].V.(int):ix[1].V.(int)]}
			case SliT:
				tup := vm.Mem[args[1]].V.([]values.Value)
				ix := vm.Mem[args[2]].V.([]values.Value)
				if ix[0].T != values.INT {
					vm.Mem[args[0]] = vm.makeError("vm/slice/tuple/a", args[3], vm.DescribeType(ix[0].T, LITERAL), args[1], args[2])
					break Switch
				}
				if ix[1].T != values.INT {
					vm.Mem[args[0]] = vm.makeError("vm/slice/tuple/b", args[3], vm.DescribeType(ix[1].T, LITERAL), args[1], args[2])
					break Switch
				}
				if ix[0].V.(int) < 0 {
					vm.Mem[args[0]] = vm.makeError("vm/slice/tuple/c", args[3], ix[0].V.(int), ix[1].V.(int), args[1], args[2])
					break Switch
				}
				if ix[1].V.(int) < ix[0].V.(int) {
					vm.Mem[args[0]] = vm.makeError("vm/slice/tuple/d", args[3], ix[0].V.(int), ix[1].V.(int), args[1], args[2])
					break Switch
				}
				if len(tup) < ix[1].V.(int) {
					vm.Mem[args[0]] = vm.makeError("vm/slice/tuple/e", args[3], ix[1].V.(int), len(tup), args[1], args[2])
					break Switch
				}
				vm.Mem[args[0]] = values.Value{values.TUPLE, tup[ix[0].V.(int):ix[1].V.(int)]}
			case SlTn:
				vm.Mem[args[0]] = values.Value{values.TUPLE, (vm.Mem[args[1]].V.([]values.Value))[args[2]:]}
			case StrP:
				typeNo := vm.Mem[args[2]].V.(values.AbstractType).Types[0]
				fields := make([]values.Value, 0, len(args)-3)
				for _, loc := range args[3:] {
					fields = append(fields, vm.Mem[loc])
				}
				if typeCheck := vm.ConcreteTypeInfo[typeNo].(StructType).TypeCheck; typeCheck != nil {
					vm.Mem[typeCheck.TokNumberLoc] = values.Value{values.INT, int(args[1])}
					vm.Mem[typeCheck.InLoc] = values.Value{typeNo, fields}
					vm.Mem[typeCheck.ResultLoc] = values.Value{typeNo, fields}
					vm.run(typeCheck.CallAddress, ctx)
					vm.Mem[args[0]] = vm.Mem[typeCheck.ResultLoc]
				} else {
					vm.Mem[args[0]] = values.Value{typeNo, fields}
				}
			case Strc:
				fields := make([]values.Value, 0, len(args)-2)
				for _, loc := range args[2:] {
					fields = append(fields, vm.Mem[loc])
				}
				vm.Mem[args[0]] = values.Value{values.ValueType(args[1]), fields}
			case Strx:
				vm.Mem[args[0]] = values.Value{values.STRING, vm.DefaultDescription(vm.Mem[args[1]])}
			case Subf:
				vm.Mem[args[0]] = values.Value{vm.Mem[args[1]].T, vm.Mem[args[1]].V.(float64) - vm.Mem[args[2]].V.(float64)}
			case Subi:
				vm.Mem[args[0]] = values.Value{vm.Mem[args[1]].T, vm.Mem[args[1]].V.(int) - vm.Mem[args[2]].V.(int)}
			case SubS:
				result := vm.Mem[args[1]].V.(values.Set)
				result.Subtract(vm.Mem[args[2]].V.(values.Set))
				vm.Mem[args[0]] = values.Value{vm.Mem[args[1]].T, result}
			case Thnk:
				vm.Mem[args[0]] = values.Value{values.THUNK, values.ThunkValue{args[1], args[2]}}
			case Tinf:
				// TODO --- as this is permanent state, much of it could be stuck in the ConcreteTypeInfo.
				result := vector.Empty
				ty := vm.Mem[args[1]].V.(values.AbstractType)
				conc := (ty.Len() == 1)
				name := vm.DescribeAbstractType(ty, LITERAL)
				operator := ""
				ix := strings.IndexRune(name, '{')
				if ix == -1 {
					operator = name
				} else {
					operator = name[:ix]
				}
				result = result.Conj(values.Value{values.STRING, name})
				types := values.Set{}
				for _, v := range ty.Types {
					concType := values.AbstractType{[]values.ValueType{v}}
					types = types.Add(values.Value{values.TYPE, concType})
				}
				result = result.Conj(values.Value{values.SET, types})
				result = result.Conj(values.Value{values.BOOL, conc &&
					!(vm.ConcreteTypeInfo[ty.Types[0]].IsClone() ||
						vm.ConcreteTypeInfo[ty.Types[0]].IsEnum() ||
						vm.ConcreteTypeInfo[ty.Types[0]].IsStruct())})
				result = result.Conj(values.Value{values.BOOL, conc && vm.ConcreteTypeInfo[ty.Types[0]].IsClone()})
				result = result.Conj(values.Value{values.BOOL, conc && vm.ConcreteTypeInfo[ty.Types[0]].IsEnum()})
				result = result.Conj(values.Value{values.BOOL, conc && vm.ConcreteTypeInfo[ty.Types[0]].IsStruct()})
				if ct, ok := vm.ConcreteTypeInfo[ty.Types[0]].(CloneType); ok {
					result = result.Conj(values.Value{values.TYPE, values.MakeAbstractType(ct.Parent)})
				} else {
					result = result.Conj(values.Value{values.NULL, nil})
				}
				if et, ok := vm.ConcreteTypeInfo[ty.Types[0]].(EnumType); ok {
					result = result.Conj(et.ElementValues)
				} else {
					result = result.Conj(values.Value{values.NULL, nil})
				}
				if st, ok := vm.ConcreteTypeInfo[ty.Types[0]].(StructType); ok {
					result = result.Conj(st.LabelValues)
					vec := vector.Empty
					for _, ty := range st.AbstractStructFields {
						vec = vec.Conj(values.Value{values.TYPE, ty})
					}
					result = result.Conj(values.Value{values.LIST, vec})
				} else {
					result = result.Conj(values.Value{values.NULL, nil})
					result = result.Conj(values.Value{values.NULL, nil})
				}
				result = result.Conj(values.Value{values.STRING, operator})
				pVals := vector.Empty
				pTypes := vector.Empty
				switch typeIs := vm.ConcreteTypeInfo[ty.Types[0]].(type) {
				case CloneType:
					for _, v := range typeIs.TypeArguments {
						pVals = pVals.Conj(v)
						pTypes = pTypes.Conj(values.Value{values.TYPE, values.MakeAbstractType(v.T)})
					}
					result = result.Conj(values.Value{values.LIST, pVals})
					result = result.Conj(values.Value{values.LIST, pTypes})
				case StructType:
					for _, v := range typeIs.TypeArguments {
						pVals = pVals.Conj(v)
						pTypes = pTypes.Conj(values.Value{values.TYPE, values.MakeAbstractType(v.T)})
					}
					result = result.Conj(values.Value{values.LIST, pVals})
					result = result.Conj(values.Value{values.LIST, pTypes})
				default:
					result = result.Conj(values.Value{values.NULL, nil})
					result = result.Conj(values.Value{values.NULL, nil})
				}
				vm.Mem[args[0]] = values.Value{values.LIST, result}
			case Tplf:
				tup := vm.Mem[args[1]].V.([]values.Value)
				if len(tup) == 0 {
					vm.Mem[args[0]] = vm.makeError("vm/tup/first", args[2])
					break Switch
				}
				vm.Mem[args[0]] = tup[0]
			case Tpll:
				tup := vm.Mem[args[1]].V.([]values.Value)
				if len(tup) == 0 {
					vm.Mem[args[0]] = vm.makeError("vm/tup/last", args[2])
					break Switch
				}
				vm.Mem[args[0]] = tup[len(tup)-1]
			case Trak:
				staticData := vm.Tracking[args[0]]
				newData := TrackingData{staticData.Flavor, staticData.Tok, DUMMY, make([]any, len(staticData.Args))}
				copy(newData.Args, staticData.Args) // This is because only things of tye uint32 are meant to be replaced.
				for i, v := range newData.Args {
					if v, ok := v.(uint32); ok {
						newData.Args[i] = vm.Mem[v]
					}
				}
				vm.LiveTracking = append(vm.LiveTracking, newData)
			case TuLx:
				vector, ok := vm.Mem[args[1]].V.(vector.Vector)
				if !ok {
					vm.Mem[args[0]] = vm.makeError("vm/splat/type", args[2], args[1])
					break Switch
				}
				length := vector.Len()
				slice := make([]values.Value, length)
				for i := 0; i < length; i++ {
					element, _ := vector.Index(i)
					slice[i] = element.(values.Value)
				}
				vm.Mem[args[0]] = values.Value{values.TUPLE, slice}
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
				vm.Mem[args[0]] = values.Value{values.TYPE, lhs.Union(rhs)}
			case Typx:
				vm.Mem[args[0]] = values.Value{values.TYPE, values.AbstractType{[]values.ValueType{vm.Mem[args[1]].T}}}
			case UntE:
				err := vm.Mem[args[0]].V.(*err.Error)
				newArgs := []any{}
				newVals := []values.Value{}
				for _, arg := range err.Args {
					switch arg := arg.(type) {
					case uint32:
						newVals = append(newVals, vm.Mem[arg])
					default:
						newArgs = append(newArgs, arg)
					}
				}
				err.Args = newArgs
				err.Values = newVals
			case Untk:
				if vm.Mem[args[0]].T == values.THUNK {
					resultLoc := vm.Mem[args[0]].V.(values.ThunkValue).MLoc
					codeAddr := vm.Mem[args[0]].V.(values.ThunkValue).CAddr
					vm.run(codeAddr, ctx)
					vm.Mem[args[0]] = vm.Mem[resultLoc]
				}
			case Uwrp:
				if vm.Mem[args[1]].T == values.ERROR {
					wrappedErr := vm.Mem[args[1]].V.(*err.Error)
					errWithMessage := err.CreateErr(wrappedErr.ErrorId, wrappedErr.Token, wrappedErr.Args...)
					vm.Mem[args[0]] = values.Value{vm.UsefulTypes.UnwrappedError, []values.Value{{values.STRING, errWithMessage.ErrorId}, {values.STRING, errWithMessage.Message}}}
				} else {
					vm.Mem[args[0]] = vm.makeError("vm/unwrap", args[2], vm.DescribeType(vm.Mem[args[1]].T, LITERAL))
				}
			case Vlid:
				vm.Mem[args[0]] = values.Value{values.BOOL, vm.Mem[args[1]].T != values.ERROR}
			case WthL:
				result := values.Value{vm.Mem[args[1]].T, vm.Mem[args[1]].V.(vector.Vector)}
				for _, pair := range vm.Mem[args[2]].V.([]values.Value) {
					key := pair.V.([]values.Value)[0]
					val := pair.V.([]values.Value)[1]
					var keys []values.Value
					if key.T == values.LIST {
						vec := key.V.(vector.Vector)
						ln := vec.Len()
						if ln == 0 {
							vm.Mem[args[0]] = vm.makeError("vm/with/list/b", args[3])
							break Switch
						}
						keys = make([]values.Value, ln)
						for i := 0; i < ln; i++ {
							el, _ := vec.Index(i)
							keys[i] = el.(values.Value)
						}
					} else {
						keys = []values.Value{key}
					}
					result = vm.with(result, keys, val, args[3])
					if result.T == values.ERROR {
						break
					}
				}
				vm.Mem[args[0]] = result
			case WthM:
				result := values.Value{vm.Mem[args[1]].T, vm.Mem[args[1]].V.(*values.Map)}
				for _, pair := range vm.Mem[args[2]].V.([]values.Value) {
					key := pair.V.([]values.Value)[0]
					val := pair.V.([]values.Value)[1]
					var keys []values.Value
					if key.T == values.LIST {
						vec := key.V.(vector.Vector)
						ln := vec.Len()
						if ln == 0 {
							vm.Mem[args[0]] = vm.makeError("vm/with/map/b", args[3])
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
					result = vm.with(result, keys, val, args[3])
					if result.T == values.ERROR {
						break
					}
				}
				vm.Mem[args[0]] = result
			case Wtht:
				typL := vm.Mem[args[1]].V.(values.AbstractType)
				if typL.Len() != 1 {
					vm.Mem[args[0]] = vm.makeError("vm/with/type/a", args[3], vm.DescribeAbstractType(typL, LITERAL))
					break Switch
				}
				typ := typL.Types[0]
				if !vm.ConcreteTypeInfo[typ].IsStruct() {
					vm.Mem[args[0]] = vm.makeError("vm/with/type/b", args[3], vm.DescribeType(typ, LITERAL))
					break Switch
				}
				typeInfo := vm.ConcreteTypeInfo[typ].(StructType)
				var pairs []values.Value
				if (vm.Mem[args[2]].T) == values.PAIR {
					pairs = []values.Value{vm.Mem[args[0]]}
				} else {
					pairs = vm.Mem[args[2]].V.([]values.Value)
				}
				outVals := make([]values.Value, len(vm.ConcreteTypeInfo[typ].(StructType).LabelNumbers))
				for _, pair := range pairs {
					if pair.T != values.PAIR {
						vm.Mem[args[0]] = vm.makeError("vm/with/type/c", args[3], vm.DescribeType(pair.T, LITERAL))
						break
					}
					key := pair.V.([]values.Value)[0]
					val := pair.V.([]values.Value)[1]
					if key.T != values.LABEL {
						vm.Mem[args[0]] = vm.makeError("vm/with/type/d", args[3], vm.DescribeType(pair.T, LITERAL))
						break Switch
					}
					keyNumber := typeInfo.Resolve(key.V.(int))
					if keyNumber == -1 {
						vm.Mem[args[0]] = vm.makeError("vm/with/type/e", args[3], vm.DefaultDescription(key), vm.DescribeType(typ, LITERAL))
						break Switch
					}
					if outVals[keyNumber].T != values.UNDEFINED_TYPE {
						vm.Mem[args[0]] = vm.makeError("vm/with/type/f", args[3], vm.DefaultDescription(key))
						break Switch
					}
					outVals[keyNumber] = val
				}
				for i, v := range outVals {
					if v.T == values.UNDEFINED_TYPE {
						if vm.ConcreteTypeInfo[typ].(StructType).AbstractStructFields[i].Contains(values.NULL) {
							outVals[i] = values.Value{values.NULL, nil}
							break Switch
						} else {
							labName := vm.Labels[vm.ConcreteTypeInfo[typ].(StructType).LabelNumbers[i]]
							vm.Mem[args[0]] = vm.makeError("vm/with/type/g", args[3], labName)
							break Switch
						}
					}
					if !vm.ConcreteTypeInfo[typ].(StructType).AbstractStructFields[i].Contains(v.T) {
						labName := vm.Labels[vm.ConcreteTypeInfo[typ].(StructType).LabelNumbers[i]]
						vm.Mem[args[0]] = vm.makeError("vm/with/type/h", args[3], vm.DescribeType(v.T, LITERAL), labName, vm.DescribeType(typ, LITERAL), vm.DescribeAbstractType(vm.ConcreteTypeInfo[typ].(StructType).AbstractStructFields[i], LITERAL))
						break Switch
					}
				}
				vm.Mem[args[0]] = values.Value{typ, outVals}
			case WthZ:
				typ := vm.Mem[args[1]].T
				outVals := make([]values.Value, len(vm.ConcreteTypeInfo[typ].(StructType).LabelNumbers))
				copy(outVals, vm.Mem[args[1]].V.([]values.Value))
				result := values.Value{typ, outVals}
				for _, pair := range vm.Mem[args[2]].V.([]values.Value) {
					key := pair.V.([]values.Value)[0]
					val := pair.V.([]values.Value)[1]
					var keys []values.Value
					if key.T == values.LIST {
						vec := key.V.(vector.Vector)
						ln := vec.Len()
						if ln == 0 {
							vm.Mem[args[0]] = vm.makeError("vm/with/struct/b", args[3])
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
					result = vm.with(result, keys, val, args[3])
					if result.T == values.ERROR {
						break
					}
				}
				vm.Mem[args[0]] = result
			case WtoM:
				mp := vm.Mem[args[1]].V.(*values.Map)
				for _, key := range vm.Mem[args[2]].V.([]values.Value) {
					if (key.T < values.NULL || key.T >= values.FUNC) { // Check that the key is orderable.
						vm.Mem[args[0]] = vm.makeError("vm/without", args[3], vm.DescribeType(key.T, LITERAL))
						break Switch
					}
					mp = (*mp).Delete(key)
				}
				vm.Mem[args[0]] = values.Value{vm.Mem[args[1]].T, mp}
			case Yeet:
				typeInfo := vm.ConcreteTypeInfo[vm.Mem[args[1]].T]
				var typeArgs []values.Value
				switch typeInfo := typeInfo.(type) {
				case StructType:
					typeArgs = typeInfo.TypeArguments
				case CloneType:
					typeArgs = typeInfo.TypeArguments
				default:
					panic("Unhandled case.")
				}
				for i, v := range typeArgs {
					vm.Mem[args[0]+uint32(i)] = v
				}
			default:
				panic("Unhandled opcode '" + OPERANDS[vm.Code[loc].Opcode].oc + "'")
			}
			loc++
		}
	}
	if settings.SHOW_RUNTIME {
		println()
	}
}

// Implements equality-by-value. Assumes that the two values have already been verified to have the same type.
func (mc Vm) equals(v, w values.Value) bool {
	switch v.T {
	case values.BOOL:
		return v.V.(bool) == w.V.(bool)
	case values.FLOAT:
		return v.V.(float64) == w.V.(float64)
	case values.FUNC:
		return false
	case values.INT:
		return v.V.(int) == w.V.(int)
	case values.LABEL:
		return v.V.(int) == w.V.(int)
	case values.LIST:
		return mc.listsAreEqual(v, w)
	case values.MAP:
		return mc.mapsAreEqual(v, w)
	case values.NULL:
		return true
	case values.PAIR:
		return mc.equals(v.V.([]values.Value)[0], w.V.([]values.Value)[0]) &&
			mc.equals(v.V.([]values.Value)[1], w.V.([]values.Value)[1])
	case values.RUNE:
		return v.V.(rune) == w.V.(rune)
	case values.SET:
		return mc.setsAreEqual(v, w)
	case values.STRING:
		return v.V.(string) == w.V.(string)
	case values.TUPLE:
		vVals := v.V.([]values.Value)
		wVals := w.V.([]values.Value)
		if len(vVals) != len(wVals) {
			return false
		}
		for i, val := range vVals {
			if !mc.equals(val, wVals[i]) {
				return false
			}
		}
		return true
	case values.TYPE:
		return v.V.(values.AbstractType).Equals(w.V.(values.AbstractType))
	}
	switch typeInfo := mc.ConcreteTypeInfo[v.T].(type) {
	case CloneType:
		switch typeInfo.Parent {
		case values.FLOAT:
			return v.V.(float64) == w.V.(float64)
		case values.INT:
			return v.V.(int) == w.V.(int)
		case values.LIST:
			return mc.listsAreEqual(v, w)
		case values.MAP:
			return mc.mapsAreEqual(v, w)
		case values.PAIR:
			return mc.equals(v.V.([]values.Value)[0], w.V.([]values.Value)[0]) &&
				mc.equals(v.V.([]values.Value)[1], w.V.([]values.Value)[1])
		case values.RUNE:
			return v.V.(rune) == w.V.(rune)
		case values.SET:
			return mc.setsAreEqual(v, w)
		case values.STRING:
			return v.V.(string) == w.V.(string)
		}
	case EnumType:
		return v.V.(int) == w.V.(int)
	case StructType:
		for i, v := range v.V.([]values.Value) {
			if !mc.equals(v, w.V.([]values.Value)[i]) {
				return false
			}
		}
		return true
	}
	panic("Wut?")
}

func (vm *Vm) listsAreEqual(v, w values.Value) bool {
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
		if !vm.equals(kEl.(values.Value), lEl.(values.Value)) {
			return false
		}
	}
	return true
}

func (vm *Vm) mapsAreEqual(v, w values.Value) bool {
	mapV := v.V.(*values.Map)
	mapW := w.V.(*values.Map)
	if mapV.Len() != mapW.Len() {
		return false
	}
	sl := mapV.AsSlice()
	for _, pair := range sl {
		if val, ok := mapW.Get(pair.Key); !ok || pair.Val != val {
			return false
		}
	}
	return true
}

func (vm *Vm) setsAreEqual(v, w values.Value) bool {
	setV := v.V.(values.Set)
	setW := w.V.(values.Set)
	if setV.Len() != setW.Len() {
		return false
	}
	sl := setV.AsSlice()
	for _, el := range sl {
		if !setW.Contains(el) {
			return false
		}
	}
	return true
}

// Implements `with`, which needs to be done separately because it may be recursive.
func (vm *Vm) with(container values.Value, keys []values.Value, val values.Value, errTok uint32) values.Value {
	key := keys[0]
	switch container.T {
	case values.LIST:
		vec := container.V.(vector.Vector)
		if key.T != values.INT {
			return vm.makeError("vm/with/a", errTok, vm.DescribeType(key.T, LITERAL))
		}
		keyNumber := key.V.(int)
		if keyNumber < 0 || keyNumber >= vec.Len() {
			return vm.makeError("vm/with/b", errTok, key.V.(int), vec.Len())
		}
		if len(keys) == 1 {
			container.V = vec.Assoc(keyNumber, val)
			return container
		}
		el, _ := vec.Index(keyNumber)
		container.V = vec.Assoc(keyNumber, vm.with(el.(values.Value), keys[1:], val, errTok))
		return container
	case values.MAP:
		mp := container.V.(*values.Map)
		if ((key.T < values.NULL) || (key.T >= values.FUNC && key.T < values.LABEL)) && !vm.ConcreteTypeInfo[key.T].IsEnum() { // Check that the key is orderable.
			return vm.makeError("vm/with/c", errTok, vm.DescribeType(key.T, LITERAL))
		}
		if len(keys) == 1 {
			mp = mp.Set(key, val)
			return values.Value{values.MAP, mp}
		}
		el, _ := mp.Get(key)
		mp = mp.Set(key, vm.with(el, keys[1:], val, errTok))
		return values.Value{values.MAP, mp}
	default: // It's a struct.
		fields := make([]values.Value, len(container.V.([]values.Value)))
		clone := values.Value{container.T, fields}
		copy(fields, container.V.([]values.Value))
		typeInfo := vm.ConcreteTypeInfo[container.T].(StructType)
		if key.T != values.LABEL {
			return vm.makeError("vm/with/d", errTok, vm.DescribeType(key.T, LITERAL))
		}
		fieldNumber := typeInfo.Resolve(key.V.(int))
		if fieldNumber == -1 {
			return vm.makeError("vm/with/e", errTok, vm.DefaultDescription(key), vm.DescribeType(container.T, LITERAL))
		}
		if len(keys) > 1 {
			val = vm.with(fields[fieldNumber], keys[1:], val, errTok)
		}
		if !vm.ConcreteTypeInfo[container.T].(StructType).AbstractStructFields[fieldNumber].Contains(val.T) {
			labName := vm.Labels[key.V.(int)]
			return vm.makeError("vm/with/f", errTok, vm.DescribeType(val.T, LITERAL), labName, vm.DescribeType(container.T, LITERAL), vm.DescribeAbstractType(vm.ConcreteTypeInfo[container.T].(StructType).AbstractStructFields[fieldNumber], LITERAL))
		}
		fields[fieldNumber] = val
		return clone
	}
}

// Struct and functions for handling the concrete type information.

type supertype int

const (
	NATIVE supertype = iota
	ENUM
	STRUCT
)

type TypeInformation interface {
	GetName(flavor descriptionFlavor) string
	getPath() string
	IsEnum() bool
	IsStruct() bool
	isSnippet() bool
	IsClone() bool
	IsPrivate() bool
	IsMandatoryImport() bool
	IsClonedBy() values.AbstractType
}

type BuiltinType struct {
	name   string
	path   string
	clones values.AbstractType
}

func (t BuiltinType) GetName(flavor descriptionFlavor) string {
	if flavor == LITERAL {
		return t.path + t.name
	}
	return string(t.name)
}

func (t BuiltinType) IsEnum() bool {
	return false
}

func (t BuiltinType) IsStruct() bool {
	return false
}

func (t BuiltinType) isSnippet() bool {
	return false
}

func (t BuiltinType) IsClone() bool {
	return false
}

func (t BuiltinType) IsPrivate() bool {
	return false
}

func (t BuiltinType) getPath() string {
	return t.path
}

func (t BuiltinType) IsMandatoryImport() bool {
	return true
}

func (t BuiltinType) IsClonedBy() values.AbstractType {
	return t.clones
}

func (t BuiltinType) AddClone(v values.ValueType) BuiltinType {
	t.clones = t.clones.Insert(v)
	return t
}

type EnumType struct {
	Name          string
	Path          string
	ElementNames  []string
	ElementValues values.Value // A list.
	Private       bool
	IsMI          bool
}

func (t EnumType) GetName(flavor descriptionFlavor) string {
	if flavor == LITERAL {
		return t.Path + t.Name
	}
	return t.Name
}

func (t EnumType) IsEnum() bool {
	return true
}

func (t EnumType) IsStruct() bool {
	return false
}

func (t EnumType) isSnippet() bool {
	return false
}

func (t EnumType) IsPrivate() bool {
	return t.Private
}

func (t EnumType) IsClone() bool {
	return false
}

func (t EnumType) getPath() string {
	return t.Path
}

func (t EnumType) IsMandatoryImport() bool {
	return t.IsMI
}

func (EnumType) IsClonedBy() values.AbstractType {
	return values.MakeAbstractType()
}

// Contains the information necessary to perform the runtime checks on type constructors
// on structs and clones.
type TypeCheck struct {
	CallAddress  uint32
	ParamsLoc    uint32
	InLoc        uint32
	ResultLoc    uint32
	Params       []values.Value
	TokNumberLoc uint32 // Contains a location which contains an integer which is an index of the tokens in the vm.
}

type CloneType struct {
	Name          string
	Path          string
	Parent        values.ValueType
	Private       bool
	IsSliceable   bool
	IsFilterable  bool
	IsMappable    bool
	IsMI          bool
	Using         []token.Token // TODO --- this is used during API serialization only and can be stored somewhere else once we move that to initialization time.
	TypeCheck     *TypeCheck
	TypeArguments []values.Value
}

func (t CloneType) GetName(flavor descriptionFlavor) string {
	if flavor == LITERAL {
		return t.Path + t.Name
	}
	return t.Name
}

func (t CloneType) IsEnum() bool {
	return false
}

func (t CloneType) IsStruct() bool {
	return false
}

func (t CloneType) isSnippet() bool {
	return false
}

func (t CloneType) IsPrivate() bool {
	return t.Private
}

func (t CloneType) IsClone() bool {
	return true
}

func (t CloneType) getPath() string {
	return t.Path
}

func (t CloneType) IsMandatoryImport() bool {
	return t.IsMI
}

func (CloneType) IsClonedBy() values.AbstractType {
	return values.MakeAbstractType()
}

func (t CloneType) AddTypeCheck(tc *TypeCheck) CloneType {
	t.TypeCheck = tc
	return t
}

type StructType struct {
	Name                 string
	Path                 string
	LabelNumbers         []int
	LabelValues          values.Value // A list.
	Snippet              bool
	Private              bool
	AbstractStructFields []values.AbstractType
	ResolvingMap         map[int]int // TODO --- it would probably be better to implment this as a linear search below a given threshhold and a binary search above it.
	IsMI                 bool
	TypeCheck            *TypeCheck
	TypeArguments        []values.Value
}

func (t StructType) GetName(flavor descriptionFlavor) string {
	if flavor == LITERAL {
		return t.Path + t.Name
	}
	return t.Name
}

func (t StructType) IsEnum() bool {
	return false
}

func (t StructType) IsStruct() bool {
	return true
}

func (t StructType) isSnippet() bool {
	return t.Snippet
}

func (t StructType) IsPrivate() bool {
	return t.Private
}

func (t StructType) IsClone() bool {
	return false
}

func (t StructType) Len() int {
	return len(t.LabelNumbers)
}

func (t StructType) getPath() string {
	return t.Path
}

func (t StructType) IsMandatoryImport() bool {
	return t.IsMI
}

func (StructType) IsClonedBy() values.AbstractType {
	return values.MakeAbstractType()
}

func (t StructType) AddLabels(labels []int) StructType {
	t.ResolvingMap = make(map[int]int)
	for k, v := range labels {
		t.ResolvingMap[v] = k
	}
	return t
}

func (t StructType) Resolve(labelNumber int) int {
	result, ok := t.ResolvingMap[labelNumber]
	if ok {
		return result
	}
	return -1
}

func (t StructType) AddTypeCheck(tc *TypeCheck) StructType {
	t.TypeCheck = tc
	return t
}

// Produces a Value of the internal type ITERATOR for use in implementing `for` loops.
// TODO --- since the only thing we're using the VM for is to look up the `concreteTypeInfo`,
// this clearly belongs in the compiler.
func (vm *Vm) NewIterator(container values.Value, keysOnly bool, tokLoc uint32) values.Value {
	ty := container.T
	if cloneInfo, ok := vm.ConcreteTypeInfo[ty].(CloneType); ok {
		ty = cloneInfo.Parent
	}
	switch ty {
	case values.INT:
		return values.Value{values.ITERATOR, &values.KeyIncIterator{Max: container.V.(int)}}
	case values.LIST:
		if keysOnly {
			return values.Value{values.ITERATOR, &values.KeyIncIterator{Max: container.V.(vector.Vector).Len()}}
		} else {
			return values.Value{values.ITERATOR, &values.ListIterator{VecIt: container.V.(vector.Vector).Iterator()}}
		}
	case values.MAP:
		mapAsSlice := container.V.(*values.Map).AsSlice()
		return values.Value{values.ITERATOR, &values.MapIterator{KVPairs: mapAsSlice, Len: len(mapAsSlice)}}
	case values.PAIR:
		pair := container.V.([]values.Value)
		left := pair[0]
		right := pair[1]
		if left.T != values.INT || right.T != values.INT {
			return values.Value{values.ERROR, vm.makeError("vm/for/pair", tokLoc)}
		}
		leftV := left.V.(int)
		rightV := right.V.(int)
		if leftV <= rightV {
			return values.Value{values.ITERATOR, &values.IncIterator{StartVal: leftV, MaxVal: rightV, Val: leftV}}
		} else {
			return values.Value{values.ITERATOR, &values.DecIterator{MinVal: rightV, StartVal: leftV - 1, Val: leftV - 1}}
		}
	case values.SET:
		setAsSlice := container.V.(values.Set).AsSlice()
		return values.Value{values.ITERATOR, &values.SetIterator{Elements: setAsSlice, Len: len(setAsSlice)}}
	case values.SNIPPET:
		if keysOnly {
			return values.Value{values.ITERATOR, &values.KeyIncIterator{Max: len(container.V.(values.Snippet).Data)}}
		} else {
			return values.Value{values.ITERATOR, &values.TupleIterator{Elements: container.V.(values.Snippet).Data, Len: len(container.V.(values.Snippet).Data)}}
		}
	case values.STRING:
		if keysOnly {
			return values.Value{values.ITERATOR, &values.KeyIncIterator{Max: len(container.V.(string))}}
		} else {
			return values.Value{values.ITERATOR, &values.StringIterator{Str: container.V.(string)}}
		}
	case values.TUPLE:
		tupleElements := container.V.([]values.Value)
		if keysOnly {
			return values.Value{values.ITERATOR, &values.KeyIncIterator{Max: len(tupleElements)}}
		} else {
			return values.Value{values.ITERATOR, &values.TupleIterator{Elements: tupleElements, Len: len(tupleElements)}}
		}
	case values.TYPE:
		abTyp := container.V.(values.AbstractType)
		if len(abTyp.Types) != 1 {
			return values.Value{values.ERROR, vm.makeError("vm/for/type/a", tokLoc)}
		}
		typ := abTyp.Types[0]
		if !vm.ConcreteTypeInfo[typ].IsEnum() {
			return values.Value{values.ERROR, vm.makeError("vm/for/type/b", tokLoc)}
		}
		if keysOnly {
			return values.Value{values.ITERATOR, &values.KeyIncIterator{Max: len(vm.ConcreteTypeInfo[typ].(EnumType).ElementNames)}}
		} else {
			return values.Value{values.ITERATOR, &values.EnumIterator{Type: typ, Max: len(vm.ConcreteTypeInfo[typ].(EnumType).ElementNames)}}
		}
	default:
		return values.Value{values.ERROR, vm.makeError("vm/for/type/c", tokLoc)}
	}
}

// Constants for describing the syntax of functions.
const (
	PREFIX uint32 = iota
	INFIX
	SUFFIX
	UNFIX
)
