package parser

// The evaluator needs a way to create side-effects, it should be injected rather than
// hard-wired. This is the struct that we pass to the evaluator, except that as we're passing
// it the parser anyway we're making it part of that.

import (
	"bufio"
	"bytes"
	"io"
	"strings"

	"charm/source/object"
	"charm/source/text"

	"github.com/lmorg/readline"
)

type EffectHandler struct {
	InHandle  InHandler
	OutHandle OutHandler
}

type InHandler interface {
	Get(query string) string
}

type OutHandler interface {
	Out(outObjects []object.Object, env *object.Environment)
}

func MakeStandardEffectHandler(out io.Writer) EffectHandler {
	iH := &standardInHandler{}
	oH := &standardOutHandler{out: out}
	return EffectHandler{InHandle: iH, OutHandle: oH}
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

func (oH *standardOutHandler) Out(vals []object.Object, env *object.Environment) {
	view, _ := env.Get("$view")
	viewStr := view.(*object.String).Value

	var out bytes.Buffer

	elements := []string{}
	for _, e := range vals {
		if viewStr == "charm" {
			elements = append(elements, e.Inspect(object.ViewCharmLiteral))
		}
		if viewStr == "plain" {
			elements = append(elements, e.Inspect(object.ViewStdOut))
		}

	}
	out.WriteString(strings.Join(elements, ", "))
	out.WriteRune('\n')
	oH.out.Write(out.Bytes())

}

type ConsumingOutHandler struct{}

func (oH *ConsumingOutHandler) Out(vals []object.Object, env *object.Environment) {

}

func MakeSnapEffectHandler(out io.Writer, env object.Environment, sn *Snap) EffectHandler {
	iH := &snapInHandler{stdIn: standardInHandler{}, snap: sn}
	oH := &snapOutHandler{stdOut: standardOutHandler{out: out}, snap: sn}
	return EffectHandler{InHandle: iH, OutHandle: oH}
}

type snapInHandler struct {
	stdIn standardInHandler
	snap  *Snap
}

type snapOutHandler struct {
	stdOut standardOutHandler
	snap   *Snap
}

func (iH *snapInHandler) Get(prompt string) string {
	iH.snap.AddOutput("\"" + prompt + "\"")
	input := iH.stdIn.Get(prompt)
	iH.snap.AddInput(input)
	return input
}

func (oH *snapOutHandler) Out(vals []object.Object, env *object.Environment) {

	var out bytes.Buffer

	elements := []string{}
	for _, e := range vals {
		elements = append(elements, e.Inspect(object.ViewCharmLiteral))
	}
	out.WriteString(strings.Join(elements, ", "))
	out.WriteRune('\n')
	oH.snap.AddOutput(out.String())
	oH.stdOut.Out(vals, env)
}

func MakeTestEffectHandler(out io.Writer, env object.Environment, scanner *bufio.Scanner, testOutputType TestOutputType) EffectHandler {
	iH := &TestInHandler{out: out, stdIn: standardInHandler{}, scanner: scanner, testOutputType: testOutputType}
	oH := &TestOutHandler{stdOut: standardOutHandler{out: out}, scanner: scanner, testOutputType: testOutputType}
	return EffectHandler{InHandle: iH, OutHandle: oH}
}

type TestInHandler struct {
	stdIn          standardInHandler
	out            io.Writer
	scanner        *bufio.Scanner
	Fail           bool
	testOutputType TestOutputType
}

type TestOutHandler struct {
	stdOut         standardOutHandler
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

func (oH *TestOutHandler) Out(vals []object.Object, env *object.Environment) {

	var out bytes.Buffer

	elements := []string{}
	for _, e := range vals {
		elements = append(elements, e.Inspect(object.ViewCharmLiteral))
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
			oH.stdOut.out.Write([]byte(text.WAS + getExpected))
			oH.stdOut.out.Write([]byte("\n" + text.GOT + getGot))
		} else {
			oH.stdOut.out.Write([]byte(getGot))
		}
	case SHOW_DIFF:
		if getExpected != getGot {
			oH.stdOut.out.Write([]byte(text.WAS + getExpected))
			oH.stdOut.out.Write([]byte("\n" + text.GOT + getGot))
		}
	}
}

type TestOutputType int

const (
	ERROR_CHECK TestOutputType = iota
	SHOW_ALL
	SHOW_DIFF
)
