package parser

// The evaluator needs a way to create side-effects, it should be injected rather than
// hard-wired. This is the struct that we pass to the evaluator, except that as we're passing
// it the parser anyway we're making it part of that.

import (
	"bytes"
	"io"
	"strings"

	"charm/source/object"

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
	iH := standardInHandler{}
	oH := standardOutHandler{out: out}
	return EffectHandler{InHandle: iH, OutHandle: oH}
}

type standardInHandler struct{}

func (iH standardInHandler) Get(prompt string) string {
	rline := readline.NewInstance()
	rline.SetPrompt(prompt)
	line, _ := rline.Readline()
	return line
}

type standardOutHandler struct {
	out io.Writer
}

func (oH standardOutHandler) Out(vals []object.Object, env *object.Environment) {
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

func (oH ConsumingOutHandler) Out(vals []object.Object, env *object.Environment) {

}

func MakeSnapEffectHandler(out io.Writer, env object.Environment, sn *Snap) EffectHandler {
	iH := snapInHandler{stdIn: standardInHandler{}, snap: sn}
	oH := snapOutHandler{stdOut: standardOutHandler{out: out}, snap: sn}
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

func (iH snapInHandler) Get(prompt string) string {
	iH.snap.AddOutput("\"" + prompt + "\"")
	input := iH.stdIn.Get(prompt)
	iH.snap.AddInput(input)
	return input
}

func (oH snapOutHandler) Out(vals []object.Object, env *object.Environment) {

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
