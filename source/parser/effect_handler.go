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

func MakeStandardEffectHandler(out io.Writer, env object.Environment) EffectHandler {
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
	env object.Environment
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
	oH.out.Write(out.Bytes())

}

type consumingOutHandler struct{}

func (oH consumingOutHandler) Out(vals []object.Object, env *object.Environment) {

}
