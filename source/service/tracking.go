package service

import (
	"bytes"
	"pipefish/source/ast"
	"pipefish/source/text"
	"pipefish/source/token"
	"pipefish/source/values"
	"strconv"
)

// When we have tracking turned on, and the compiler reaches a point where the generated code needs to track something, we need to (a)
// store sufficient data in the vm for it to describe the situation at runtime using the tracking information and the current state of
// the VM. At the same time, we need to emit an opcode trac with one operand giving the number of the tracking data item in the VM's list.

type trackingFlavor = int

const (
	trFNCALL trackingFlavor = iota
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
	var newData TrackingData
	switch tf {
	case trFNCALL:
		newData = TrackingData{trFNCALL, tok, []any{args[0]}}
		sig := args[1].(ast.AstSig)
		loReg := args[2].(uint32)
		for i, pair := range sig {
			newData.args = append(newData.args, pair.VarName)
			newData.args = append(newData.args, loReg+uint32(i))
		}
	default:
		newData = TrackingData{tf, tok, args}
	}
	cp.Emit(Trak, uint32(len(cp.vm.tracking)))
	cp.vm.tracking = append(cp.vm.tracking, newData)
}

func (vm *Vm) TrackingToString() string {
	if len(vm.LiveTracking) == 0 {
		return ("\nNo tracking data exists.\n")
	}
	var out bytes.Buffer
	out.WriteString("\n")
	for _, td := range vm.LiveTracking {
		args := td.args
		switch td.flavor {
		case trFNCALL:
			out.WriteString("At@line ")
			out.WriteString(strconv.Itoa(td.tok.Line))
			out.WriteString("@we called function ")
			out.WriteString(text.Emph(args[0].(string)))
			if len(args) > 1 {
				out.WriteString(" with arguments:")
				for i := 1; i < len(args); i = i + 2 {
					out.WriteString("\n")
					out.WriteString(text.BULLET)
					out.WriteString(args[i].(string))
					out.WriteString(" = ")
					out.WriteString(vm.Literal(args[i+1].(values.Value)))
				}
			}
			out.WriteString("\n")
		}
	}
	return out.String()
}
