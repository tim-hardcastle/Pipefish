package compiler

import (
	"bytes"
	"strconv"

	"github.com/tim-hardcastle/Pipefish/source/ast"
	"github.com/tim-hardcastle/Pipefish/source/settings"
	"github.com/tim-hardcastle/Pipefish/source/text"
	"github.com/tim-hardcastle/Pipefish/source/token"
	"github.com/tim-hardcastle/Pipefish/source/vm"
)

// When we have tracking turned on, and the compiler reaches a point where the generated code needs to track something, we need to (a)
// store sufficient data in the vm for it to describe the situation at runtime using the tracking information and the current state of
// the VM. At the same time, we need to emit an opcode trac with one operand giving the number of the tracking data item in the VM's list.

// This keeps track of what we should be logging, and is passed around the compiler in the context struct.
type LogFlavor int

const (
	LF_NONE  LogFlavor = iota // No logging is taking place.
	LF_INIT                   // We're still initializing the variables.
	LF_TRACK                  // We're logging everything.
)

// Although the arguments of this function are the same as the shape of the vm.TrackingData struct, we don't just naively shove one into the other,
// but may have to tamper with it for the greater convenience of the caller.
func (cp *Compiler) Track(tf vm.TrackingFlavor, trackingOn, autoOn bool, tok *token.Token, args ...any) {
	if settings.MandatoryImportSet().Contains(tok.Source) {
		return
	}
	logToVar, _ := cp.GlobalVars.GetVar("$logTo")
	logToLoc := logToVar.MLoc
	var newData vm.TrackingData
	switch tf {
	case vm.TR_FNCALL:
		newData = vm.TrackingData{vm.TR_FNCALL, tok, logToLoc, []any{args[0]}}
		sig := args[1].(ast.StringSig)
		loReg := args[2].(uint32)
		for i, pair := range sig {
			newData.Args = append(newData.Args, pair.VarName)
			newData.Args = append(newData.Args, loReg+uint32(i))
		}
	default:
		newData = vm.TrackingData{tf, tok, logToLoc, args}
	}
	cp.Cm(staticTrackingToString(len(cp.Vm.Tracking), newData), tok)
	if trackingOn {
		cp.Emit(vm.Trak, uint32(len(cp.Vm.Tracking)))
	}
	if autoOn {
		cp.Emit(vm.Auto, uint32(len(cp.Vm.Tracking)))
	}
	cp.Vm.Tracking = append(cp.Vm.Tracking, newData)
}

func staticTrackingToString(i int, td vm.TrackingData) string { // For the use of cp.cm.
	var out bytes.Buffer
	out.WriteString("Reserving tracking data ")
	out.WriteString(strconv.Itoa(i))
	out.WriteString(" for ")
	switch td.Flavor {
	case vm.TR_CONDITION:
		out.WriteString("the condition ")
		out.WriteString(td.Args[0].(string))
	case vm.TR_ELSE:
		out.WriteString("an 'else' statement")
	case vm.TR_FNCALL:
		out.WriteString("a function call to '")
		out.WriteString(td.Args[0].(string))
		out.WriteString("'")
	case vm.TR_LITERAL:
		out.WriteString("a user-defined logging expression")
	case vm.TR_RESULT:
		out.WriteString("the result of a conditional")
	case vm.TR_RETURN:
		out.WriteString("a return from function '")
		out.WriteString(text.Emph(td.Args[0].(string)))
		out.WriteString("'")
	}
	out.WriteString(" at line ")
	out.WriteString(strconv.Itoa(td.Tok.Line))
	out.WriteString(".")
	return out.String()
}

func (cp *Compiler) trackingOn(ctxt Context) bool {
	if !ctxt.IsReturn {
		return false
	}
	if ctxt.TrackingFlavor == LF_TRACK && cp.GetTrackingScope() == 2 {
		return true
	}
	return false
}

func (cp *Compiler) autoOn(ctxt Context) bool {
	if !ctxt.IsReturn {
		return false
	}
	return ctxt.LogFlavor == LF_TRACK
}

func (cp *Compiler) GetTrackingScope() int {
	return cp.getValueOfVariable("$logging").(int)
}

func (cp *Compiler) getValueOfVariable(s string) any {
	varIs, _ := cp.GlobalVars.GetVar(s)
	return cp.Vm.Mem[varIs.MLoc].V
}
