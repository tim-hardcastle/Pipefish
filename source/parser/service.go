package parser

import (
	"os"

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

func (service *Service) NeedsUpdate() (bool, error) {
	file, err := os.Stat(service.ScriptFilepath)
	if err != nil {
		return false, err
	}
	modifiedTime := file.ModTime().UnixMilli()
	if modifiedTime != service.Timestamp {
		return true, nil
	}
	for _, s := range service.Parser.NamespaceBranch {
		needsUpdate, err := s.NeedsUpdate()
		if needsUpdate || (err != nil) {
			return needsUpdate, err
		}
	}
	return false, nil
}
