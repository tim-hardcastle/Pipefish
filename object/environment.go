package object

type AccessType int

const (
	ACCESS_PUBLIC = 0
	ACCESS_CONSTANT = 1
	ACCESS_PRIVATE = 2
)

type Environment struct {
	Store map[string]Storage
	Ext *Environment
}

type Storage struct{
	obj Object
	access AccessType
	VarType string
}

func NewEnvironment() *Environment {
	s := make(map[string]Storage)
	return &Environment{Store: s}
}


func (e *Environment) Get(name string) (Object, bool) {
	storage, ok := e.Store[name]
	if ok || e.Ext == nil { return storage.obj, ok }
	return e.Ext.Get(name)
}

func (e *Environment) StringDumpVariables() string {
	result := ""
	for k, v := range e.Store {
		if v.access != ACCESS_CONSTANT {
			result = result + k + " = " + (v.obj).Inspect(ViewCharmLiteral) + "\n"
		}
	}
	return result
}

func (e *Environment) Exists(name string) bool {
	_, ok := e.Store[name]
	if ok || e.Ext == nil { return ok }
	return e.Ext.Exists(name)
}

// Variable assumed to exist, and type check to have been done.
func (e *Environment) UpdateVar(name string, val Object) {
	_, ok := e.Store[name]
	if ok {
		e.Store[name] = Storage{val, e.Store[name].access, e.Store[name].VarType}
		return
	}
	e.Ext.UpdateVar(name, val)
}



func (e *Environment) getAccess(name string) AccessType {
	_, ok := e.Store[name]
	if ok || e.Ext == nil { return e.Store[name].access }
	return e.Ext.getAccess(name)
}

func (e *Environment) Set(name string, val Object) Object {
	e.Store[name] = Storage{val, e.Store[name].access, e.Store[name].VarType}
	return val
}

func (e *Environment) InitializeVariable(name string, val Object, ty string) Object {
	e.Store[name] = Storage{val, ACCESS_PUBLIC, ty}
	return val
}

func (e *Environment) InitializePrivate(name string, val Object, ty string) Object {
	e.Store[name] = Storage{val, ACCESS_PRIVATE, ty}
	return val
}

func (e *Environment) InitializeConstant(name string, val Object) Object {
	e.Store[name] = Storage{val, ACCESS_CONSTANT, TrueType(val)}
	return val
}

func (e *Environment) IsConstant(name string) bool {
	return e.Store[name].access == ACCESS_CONSTANT
}

func (e *Environment) IsPrivate(name string) bool {
	return e.Store[name].access == ACCESS_PRIVATE
}