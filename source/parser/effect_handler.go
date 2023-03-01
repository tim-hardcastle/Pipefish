package parser

// The evaluator needs a way to create side-effects, it should be injected rather than
// hard-wired. This is the struct that we pass to the evaluator, except that as we're passing
// it the parser anyway we're making it part of that.

import (
	"charm/source/object"
)

type EffectHandler struct {
	InHandle InHandler
	OutHandle OutHandler
	LogHandle LogHandler
}

type InHandler interface {
	Get() string
}

type OutHandler interface  {
	Out(outObject *object.Object)
}

type LogHandler interface {
	Out(string)
}