package service

import (
	"bufio"
	"bytes"
	"io"
	"strings"

	"pipefish/source/text"
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

func MakeReadlnIoHandler(out io.Writer) IoHandler {
	iH := &ReadlnInHandler{}
	oH := &WriteOutHandler{Output: out}
	return IoHandler{InHandle: iH, OutHandle: oH}
}

type ReadlnInHandler struct{}

func (iH *ReadlnInHandler) Get(prompt string) string {
	rline := readline.NewInstance()
	rline.SetPrompt(prompt)
	line, _ := rline.Readline()
	return line
}

type StandardInHandler struct {
	in io.Reader
}

func (iH *StandardInHandler) Get(prompt string) string {
	var bytes []byte
	iH.in.Read(bytes)
	return string(bytes)
}

type WriteOutHandler struct {
	Output io.Writer
}

func (oH *WriteOutHandler) Out(vals []values.Value, vm *Vm) {
	var out bytes.Buffer

	elements := []string{}
	for _, e := range vals {
		elements = append(elements, vm.DefaultDescription(e))
	}
	out.WriteString(strings.Join(elements, ", "))
	out.WriteRune('\n')
	oH.Output.Write(out.Bytes())
}

type ConsumingOutHandler struct{}

func (oH *ConsumingOutHandler) Out(vals []values.Value, vm *Vm) {
	// Sometimes we just want to eat te values. Yum, values.
}

func MakeSnapIoHandler(out io.Writer, sn *Snap) IoHandler {
	iH := &snapInHandler{stdIn: ReadlnInHandler{}, snap: sn}
	oH := &snapOutHandler{stdOut: WriteOutHandler{Output: out}, snap: sn}
	return IoHandler{InHandle: iH, OutHandle: oH}
}

type snapInHandler struct {
	stdIn ReadlnInHandler
	snap  *Snap
}

type snapOutHandler struct {
	stdOut WriteOutHandler
	snap   *Snap
}

func (iH *snapInHandler) Get(prompt string) string {
	iH.snap.AddOutput("\"" + prompt + "\"")
	input := iH.stdIn.Get(prompt)
	iH.snap.AddInput(input)
	return input
}

func (oH *snapOutHandler) Out(vals []values.Value, vm *Vm) {

	var out bytes.Buffer

	elements := []string{}
	for _, e := range vals {
		elements = append(elements, vm.DefaultDescription(e))
	}
	out.WriteString(strings.Join(elements, ", "))
	out.WriteRune('\n')
	oH.snap.AppendOutput(out.String())
	oH.stdOut.Out(vals, vm)
}

func MakeTestIoHandler(out io.Writer, scanner *bufio.Scanner, testOutputType TestOutputType) IoHandler {
	iH := &TestInHandler{out: out, stdIn: ReadlnInHandler{}, scanner: scanner, testOutputType: testOutputType}
	oH := &TestOutHandler{stdOut: WriteOutHandler{Output: out}, scanner: scanner, testOutputType: testOutputType}
	return IoHandler{InHandle: iH, OutHandle: oH}
}

type TestInHandler struct {
	stdIn          ReadlnInHandler
	out            io.Writer
	scanner        *bufio.Scanner
	Fail           bool
	testOutputType TestOutputType
}

type TestOutHandler struct {
	stdOut         WriteOutHandler
	scanner        *bufio.Scanner
	Fail           bool
	testOutputType TestOutputType
}

func (iH *TestInHandler) Get(prompt string) string {
	iH.scanner.Scan()
	expectedPrompt := iH.scanner.Text()
	gotPrompt := "\"" + prompt + "\""

	switch iH.testOutputType {
	case ERROR_CHECK:
		iH.Fail = iH.Fail || expectedPrompt != gotPrompt
	case SHOW_ALL:
		if expectedPrompt != gotPrompt {
			iH.out.Write([]byte(text.WAS + expectedPrompt))
			iH.out.Write([]byte("\n" + text.GOT + gotPrompt + "\n"))
		} else {
			iH.out.Write([]byte(gotPrompt + "\n"))
		}
	case SHOW_DIFF:
		if expectedPrompt != gotPrompt {
			iH.out.Write([]byte(text.WAS + expectedPrompt))
			iH.out.Write([]byte("\n" + text.GOT + gotPrompt + "\n"))
		}
	}
	iH.scanner.Scan()
	input := iH.scanner.Text()
	if iH.testOutputType == SHOW_ALL {
		iH.out.Write([]byte(input + "\n"))
	}
	return input[3:]
}

func (oH *TestOutHandler) Out(vals []values.Value, vm *Vm) {

	var out bytes.Buffer

	elements := []string{}
	for _, e := range vals {
		elements = append(elements, vm.Literal(e))
	}
	out.WriteString(strings.Join(elements, ", "))
	oH.scanner.Scan()
	getExpected := oH.scanner.Text()
	getGot := out.String()
	switch oH.testOutputType {
	case ERROR_CHECK:
		oH.Fail = oH.Fail || getExpected != getGot
	case SHOW_ALL:
		if getExpected != getGot {
			oH.stdOut.Output.Write([]byte(text.WAS + getExpected))
			oH.stdOut.Output.Write([]byte("\n" + text.GOT + getGot + "\n"))
		} else {
			oH.stdOut.Output.Write([]byte(getGot))
		}
	case SHOW_DIFF:
		if getExpected != getGot {
			oH.stdOut.Output.Write([]byte(text.WAS + getExpected))
			oH.stdOut.Output.Write([]byte("\n" + text.GOT + getGot + "\n"))
		}
	}
}

type TestOutputType int

const (
	ERROR_CHECK TestOutputType = iota
	SHOW_ALL
	SHOW_DIFF
)
