package report

import (
	"pipefish/source/token"
	"pipefish/source/values"
)

// The 'error' type.
type Error struct {
	ErrorId string
	Message string
	Args    []any
	Values  []values.Value
	Trace   []*token.Token
	Token   *token.Token
}

func (e *Error) AddToTrace(tok *token.Token) {
	e.Trace = append(e.Trace, tok)
}
