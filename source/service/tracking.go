package service

import (
	"bytes"
	"pipefish/source/ast"
	"pipefish/source/settings"
	"pipefish/source/text"
	"pipefish/source/token"
	"pipefish/source/values"
	"strconv"
)

// When we have tracking turned on, and the compiler reaches a point where the generated code needs to track something, we need to (a)
// store sufficient data in the vm for it to describe the situation at runtime using the tracking information and the current state of
// the VM. At the same time, we need to emit an opcode trac with one operand giving the number of the tracking data item in the VM's list.

type trackingFlavor int

const (
	trCONDITION trackingFlavor = iota
	trELSE
	trFNCALL
	trLITERAL
	trRESULT
	trRETURN
)

// This contains the information we need to generate tracking reports at runtime.
type TrackingData struct {
	flavor trackingFlavor
	tok    *token.Token
	args   []any
}

// Although the arguments of this function are the same as the shape of the trackingData struct, we don't just naively shove one into the other,
// but may have to tamper with it for the greater convenience of the caller.
func (cp *Compiler) track(tf trackingFlavor, tok *token.Token, args ...any) {
	if settings.MandatoryImportSet().Contains(tok.Source) {
		return
	}
	var newData TrackingData
	switch tf {
	case trFNCALL:
		newData = TrackingData{trFNCALL, tok, []any{args[0]}}
		sig := args[1].(ast.StringSig)
		loReg := args[2].(uint32)
		for i, pair := range sig {
			newData.args = append(newData.args, pair.VarName)
			newData.args = append(newData.args, loReg+uint32(i))
		}
	default:
		newData = TrackingData{tf, tok, args}
	}
	cp.cm(staticTrackingToString(len(cp.Vm.tracking), newData), tok)
	cp.Emit(Trak, uint32(len(cp.Vm.tracking)))
	cp.Vm.tracking = append(cp.Vm.tracking, newData)
}

func staticTrackingToString(i int, td TrackingData) string { // For the use of cp.cm.
	var out bytes.Buffer
	out.WriteString("Reserving tracking data ")
	out.WriteString(strconv.Itoa(i))
	out.WriteString(" for ")
	switch td.flavor {
	case trCONDITION:
		out.WriteString("the condition ")
		out.WriteString(td.args[0].(string))
	case trELSE:
		out.WriteString("an 'else' statement")
	case trFNCALL:
		out.WriteString("a function call to '")
		out.WriteString(td.args[0].(string))
		out.WriteString("'")
	case trLITERAL:
		out.WriteString("a user-defined logging expression")
	case trRESULT:
		out.WriteString("the result of a conditional")
	case trRETURN:
		out.WriteString("a return from function '")
		out.WriteString(text.Emph(td.args[0].(string)))
		out.WriteString("'")
	}
	out.WriteString(" at line ")
	out.WriteString(strconv.Itoa(td.tok.Line))
	out.WriteString(".")
	return out.String()
}

func (vm *Vm) TrackingToString() string {
	if len(vm.LiveTracking) == 0 {
		return ("\nNo tracking data exists.\n")
	}
	var out bytes.Buffer
	out.WriteString("\n")
	for i, td := range vm.LiveTracking {
		args := td.args
		switch td.flavor {
		case trCONDITION:
			out.WriteString("At@line ")
			out.WriteString(strconv.Itoa(td.tok.Line))
			out.WriteString("@we evaluated the condition ")
			out.WriteString(text.Emph(args[0].(string)))
			out.WriteString(". ")
		case trELSE:
			out.WriteString("At@line ")
			out.WriteString(strconv.Itoa(td.tok.Line))
			out.WriteString("@we took the ")
			out.WriteString(text.Emph("else"))
			out.WriteString(" branch")
			if !vm.trackingIs(i+1, trRETURN) {
				out.WriteString(".\n")
			}
		case trFNCALL:
			out.WriteString("We called function ")
			out.WriteString(text.Emph(args[0].(string)))
			out.WriteString(" — defined at@line ")
			out.WriteString(strconv.Itoa(td.tok.Line))
			out.WriteString("@")
			if len(args) > 1 {
				out.WriteString("— with ")
				sep := ""
				for i := 1; i < len(args); i = i + 2 {
					out.WriteString(sep)
					out.WriteString(text.Emph(args[i].(string) + " = " + vm.Literal(args[i+1].(values.Value))))
					sep = ", "
				}
			}
			out.WriteString(".\n")
		case trLITERAL:
			out.WriteString("Log at@line ")
			out.WriteString(strconv.Itoa(td.tok.Line))
			out.WriteString("@: ")
			out.WriteString(args[0].(values.Value).V.(string))
			out.WriteString("\n")
		case trRESULT:
			if args[0].(values.Value).V.(bool) {
				out.WriteString("The condition succeeded.\n")
			} else {
				out.WriteString("The condition failed.\n")
			}
		case trRETURN:
			if args[1].(values.Value).T != values.UNSATISFIED_CONDITIONAL {
				if vm.trackingIs(i-1, trELSE) {
					out.WriteString(", so at")
				} else {
					out.WriteString("At")
				}
				out.WriteString("@line ")
				out.WriteString(strconv.Itoa(td.tok.Line))
				out.WriteString("@")
				//out.WriteString("of ")
				//out.WriteString(text.Emph(td.tok.Source))
				//out.WriteString(" ")
				out.WriteString("function ")
				out.WriteString(text.Emph(args[0].(string)))
				out.WriteString(" returned ")
				out.WriteString(vm.Literal(args[1].(values.Value)))
				out.WriteString(".\n")
			}
		}

	}
	return out.String()
}

func (vm *Vm) trackingIs(i int, tf trackingFlavor) bool {
	if i < 0 || i >= len(vm.LiveTracking) {
		return false
	}
	return vm.LiveTracking[i].flavor == tf
}
