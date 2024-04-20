package service

import (
	"os"
)

// This is what initialization constructs: a vm and a compiler that between them can evaluate a line of Pipefish.
type VmService struct {
	Mc             *Vm
	Cp             *Compiler
	ScriptFilepath string
	Timestamp      int64
	Broken         bool
	Visited        bool
}

func (service *VmService) NeedsUpdate() (bool, error) {
	file, err := os.Stat(service.ScriptFilepath)
	if err != nil {
		return false, err
	}
	modifiedTime := file.ModTime().UnixMilli()
	if modifiedTime != service.Timestamp {
		return true, nil
	}
	for _, s := range service.Cp.Services {
		needsUpdate, err := s.NeedsUpdate()
		if needsUpdate || (err != nil) {
			return needsUpdate, err
		}
	}
	for _, contactName := range service.Cp.Contacts {
		needsUpdate, err := service.Cp.Services[contactName].NeedsUpdate()
		if needsUpdate || (err != nil) {
			return needsUpdate, err
		}
	}
	return false, nil
}
