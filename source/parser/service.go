package parser

import (
	"charm/source/object"
)

type Service struct {
	Parser         *Parser
	Env            *object.Environment
	ScriptFilepath string
	Timestamp      int64
	Broken         bool
	Visited        bool
}

func NewService() *Service {
	service := Service{}
	return &service
}
