package parser

import (
	"os"
)

type ParserData struct {
	Parser         *Parser
	ScriptFilepath string
	Timestamp      int64
	Broken         bool
	Visited        bool
}

func NewService() *ParserData {
	service := ParserData{}
	return &service
}

func (service *ParserData) NeedsUpdate() (bool, error) {
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
	for _, contactName := range service.Parser.Contacts {
		needsUpdate, err := service.Parser.Services[contactName].NeedsUpdate()
		if needsUpdate || (err != nil) {
			return needsUpdate, err
		}
	}
	return false, nil
}
