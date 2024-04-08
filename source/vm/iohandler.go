package vm

import (
	"bytes"
	"io"
	"strings"

	"pipefish/source/values"

	"github.com/lmorg/readline"
)

type IoHandler struct {
	InHandle  InHandler
	OutHandle OutHandler
}

type InHandler interface {
	Get(query string) string
}

type OutHandler interface {
	Out(outVals []values.Value, vm *Vm)
}

func MakeStandardIoHandler(out io.Writer) IoHandler {
	iH := &standardInHandler{}
	oH := &standardOutHandler{out: out}
	return IoHandler{InHandle: iH, OutHandle: oH}
}

type standardInHandler struct{}

func (iH *standardInHandler) Get(prompt string) string {
	rline := readline.NewInstance()
	rline.SetPrompt(prompt)
	line, _ := rline.Readline()
	return line
}

type standardOutHandler struct {
	out io.Writer
}

func (oH *standardOutHandler) Out(vals []values.Value, vm *Vm) {
	var out bytes.Buffer

	elements := []string{}
	for _, e := range vals {
		elements = append(elements, vm.Describe(e))
	}
	out.WriteString(strings.Join(elements, ", "))
	out.WriteRune('\n')
	oH.out.Write(out.Bytes())
}
