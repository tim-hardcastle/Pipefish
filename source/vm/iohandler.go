package vm

import (
	"bytes"
	"io"

	"github.com/tim-hardcastle/Pipefish/source/values"

	"github.com/lmorg/readline"
)

type InHandler interface {
	Get() string
}

type OutHandler interface {
	Out(v values.Value)
	Write(s string)
}

type StandardInHandler struct {
	prompt string
}

func (iH *StandardInHandler) Get() string {
	rline := readline.NewInstance()
	rline.SetPrompt(iH.prompt)
	line, _ := rline.Readline()
	return line
}

type SimpleInHandler struct {
	input io.Reader
}

func (iH *SimpleInHandler) Get() string {
	var bytes []byte
	iH.input.Read(bytes)
	return string(bytes)
}

type SimpleOutHandler struct {
	output  io.Writer
	vm      *Vm
}

func MakeSimpleOutHandler(out io.Writer, vm *Vm) *SimpleOutHandler {
	return &SimpleOutHandler{out, vm}
}

func (oH *SimpleOutHandler) Out(v values.Value) {
	if oH.vm.Mem[oH.vm.UsefulValues.OutputAs].V.(int) == 0 {
		oH.output.Write([]byte(oH.vm.Literal(v)))
	} else {
		oH.output.Write([]byte(oH.vm.DefaultDescription(v)))
	}
	oH.output.Write([]byte{'\n'})
}

func (oH *SimpleOutHandler) Write(s string) {
	oH.output.Write([]byte(s))
}

func MakeCapturingOutHandler(vm *Vm) *CapturingOutHandler {
	buffer := bytes.NewBuffer(nil)
	simpleHandler := MakeSimpleOutHandler(buffer, vm)
	return &CapturingOutHandler{simpleHandler, buffer}
}

type CapturingOutHandler struct {
	handler *SimpleOutHandler
	capture *bytes.Buffer
}

func (oH *CapturingOutHandler) Out(v values.Value) {
	oH.capture.Write([]byte(oH.handler.vm.Literal(v)))
}

func (oH *CapturingOutHandler) Write(s string) {
	oH.capture.Write([]byte(s))
}

func (oH *CapturingOutHandler) Dump() string {
	s := oH.capture.String()
	oH.capture.Reset()
	return s
}

type ConsumingOutHandler struct{}

func (oH *ConsumingOutHandler) Out(vals []values.Value, vm *Vm) {
	// Sometimes we just want to eat the values. Yum, values.
}

func (oH *ConsumingOutHandler) Write(s string) {

}

func MakeSimpleInHandler(in io.Reader) *SimpleInHandler {
	return &SimpleInHandler{in}
}

func MakeStandardInHandler(prompt string) *StandardInHandler {
	return &StandardInHandler{prompt}
}
