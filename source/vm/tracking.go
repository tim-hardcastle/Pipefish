package vm

import (
	"strconv"
	"bytes"

	"github.com/tim-hardcastle/Pipefish/source/text"
	"github.com/tim-hardcastle/Pipefish/source/token"
	"github.com/tim-hardcastle/Pipefish/source/values"
)

// This file supplies resources for generating racking information at runtims.

type TrackingData struct {
	Flavor    TrackingFlavor
	Tok       *token.Token
	LogToLoc  uint32 // The memory location in the compiler 
	Args      []any
}

type TrackingFlavor int

const (
	TR_CONDITION TrackingFlavor = iota
	TR_ELSE
	TR_FNCALL
	TR_LITERAL
	TR_RESULT
	TR_RETURN
)

func (vm *Vm) trackingIs(i int, tf TrackingFlavor) bool {
	if i < 0 || i >= len(vm.LiveTracking) {
		return false
	}
	return vm.LiveTracking[i].Flavor == tf
}

func (vm *Vm) TrackingToString(tdL []TrackingData) string {
	if len(tdL) == 0 {
		return ("\nNo tracking data exists.\n")
	}
	var out bytes.Buffer
	for i, td := range tdL {
		args := td.Args
		switch td.Flavor {
		case TR_CONDITION:
			out.WriteString("At@line ")
			out.WriteString(strconv.Itoa(td.Tok.Line))
			out.WriteString("@we evaluated the condition ")
			out.WriteString(text.Emph(args[0].(string)))
			out.WriteString(". ")
		case TR_ELSE:
			out.WriteString("At@line ")
			out.WriteString(strconv.Itoa(td.Tok.Line))
			out.WriteString("@we took the ")
			out.WriteString(text.Emph("else"))
			out.WriteString(" branch")
			if !vm.trackingIs(i+1, TR_RETURN) {
				out.WriteString(".\n")
			}
		case TR_FNCALL:
			out.WriteString("We called function ")
			out.WriteString(text.Emph(args[0].(string)))
			out.WriteString(" - defined at@line ")
			out.WriteString(strconv.Itoa(td.Tok.Line))
			out.WriteString("@")
			if len(args) > 1 {
				out.WriteString("- with ")
				sep := ""
				for i := 1; i < len(args); i = i + 2 {
					out.WriteString(sep)
					out.WriteString(text.Emph(args[i].(string)) + " = " + text.Emph(vm.Literal(args[i+1].(values.Value))))
					sep = ", "
				}
			}
			out.WriteString(".\n")
		case TR_LITERAL:
			out.WriteString("Log at@line ")
			out.WriteString(strconv.Itoa(td.Tok.Line))
			out.WriteString("@: ")
			out.WriteString(args[0].(values.Value).V.(string))
			out.WriteString("\n")
		case TR_RESULT:
			if args[0].(values.Value).V.(bool) {
				out.WriteString("The condition succeeded.\n")
			} else {
				out.WriteString("The condition failed.\n")
			}
		case TR_RETURN:
			if args[1].(values.Value).T != values.UNSATISFIED_CONDITIONAL {
				if vm.trackingIs(i-1, TR_ELSE) {
					out.WriteString(", so at")
				} else {
					out.WriteString("At")
				}
				out.WriteString("@line ")
				out.WriteString(strconv.Itoa(td.Tok.Line))
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