package service

import (
	"io"
	"os"

	"pipefish/source/values"

	"github.com/lmorg/readline"
)


type InHandler interface {
	Get(query string) string
}

type OutHandler interface {
	Out(v values.Value, serializer func(values.Value) []byte)
	Write(s string)
}

type StandardInHandler struct{}

type StandardOutHandler struct{}

func (iH *StandardInHandler) Get(prompt string) string {
	rline := readline.NewInstance()
	rline.SetPrompt(prompt)
	line, _ := rline.Readline()
	return line
}

func (oH *StandardOutHandler) Out(v values.Value, fn func(values.Value) []byte) {
	os.Stdout.Write(fn(v))
}

func (oH *StandardOutHandler) Write(s string) {
	os.Stdout.Write([]byte(s))
}

type SimpleInHandler struct {
	input io.Reader
}

func (iH *SimpleInHandler) Get(prompt string) string {
	var bytes []byte
	iH.input.Read(bytes)
	return string(bytes)
}

type SimpleOutHandler struct {
	output io.Writer
}

func (oH *SimpleOutHandler) Out(v values.Value, fn func(values.Value) []byte) {
	oH.output.Write(fn(v))
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

func MakeSimpleInHandler(in io.Reader) SimpleInHandler {
	return SimpleInHandler{in}
}

func MakeSimpleOutHandler(out io.Writer) SimpleOutHandler {
	return SimpleOutHandler{out}
}
