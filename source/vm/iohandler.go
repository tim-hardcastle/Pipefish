package vm

import (
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
	literal bool
}

func MakeSimpleOutHandler(out io.Writer, vm *Vm, literal bool) *SimpleOutHandler {
	return &SimpleOutHandler{out, vm, literal}
}

func (oH *SimpleOutHandler) Out(v values.Value) {
	if oH.literal {
		oH.output.Write([]byte(oH.vm.Literal(v)))
	} else {
		oH.output.Write([]byte(oH.vm.Literal(v)))
	}
	oH.output.Write([]byte{'\n'})
}

func (oH *SimpleOutHandler) Write(s string) {
	oH.output.Write([]byte(s))
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
