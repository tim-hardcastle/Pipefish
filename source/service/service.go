package service

// This is what initialization constructs: a vm and a compiler that between them can evaluate a line of Pipefish.
type VmService struct {
	Mc      *Vm
	Cp      *Compiler // This also contains all the metadata about the top-level source code.
	Broken  bool
	Visited bool
}

func (service *VmService) NeedsUpdate() (bool, error) {
	return service.Cp.NeedsUpdate()
}
