package object

type AccessType int

const (
	ACCESS_LOCAL AccessType = iota
	ACCESS_PUBLIC
	ACCESS_CONSTANT
	ACCESS_PRIVATE
	ACCESS_GLOBAL // That is, somthing imported into a cmd by the 'global' keyword.
)

type Environment struct {
	Store   map[string]Storage
	Pending map[string]Object
	Ext     *Environment
}

func ToString(env *Environment) string {
	return "Environment : " + ToStringRecursive(env, "")
}

func ToStringRecursive(env *Environment, pad string) string {
	result := pad
	if env == nil {
		return pad + "nil"
	} else {

		for k, v := range env.Store {
			result = result + k + " = " + (v.obj).Inspect(ViewCharmLiteral) + ", "
		}
		if len(env.Store) == 0 {
			result = result + "Env has no vars"
		}
	}
	result = result + "\n" + pad + "Outer :"
	return result + ToStringRecursive(env.Ext, pad+"  ")
}

type Storage struct {
	obj     Object
	access  AccessType
	VarType string
}

func (storage *Storage) GetAccessType() AccessType {
	return storage.access
}

func (storage *Storage) GetValue() Object {
	return storage.obj
}

func NewEnvironment() *Environment {
	s := make(map[string]Storage)
	p := make(map[string]Object)
	return &Environment{Store: s, Pending: p}
}

func (e *Environment) ImportGlobal(name string, val Object, ty string) {
	e.Store[name] = Storage{val, ACCESS_GLOBAL, ty}
}

func (e *Environment) Get(name string) (Object, bool) {
	storage, ok := e.Store[name]
	if storage.access == ACCESS_PUBLIC || storage.access == ACCESS_PRIVATE {
		// Then it is a variable, and we check in the pending variables to see if there's anything to return.
		if pendingObject, exists := e.Pending[name]; exists {
			return pendingObject, exists
		}
	}
	if storage.access == ACCESS_GLOBAL {
		return e.Ext.Get(name)
	}
	if ok || e.Ext == nil {
		return storage.obj, ok
	}
	return e.Ext.Get(name)
}

func (e *Environment) Type(name string) (string, bool) {
	storage, ok := e.Store[name]
	if ok || e.Ext == nil {
		return storage.VarType, ok
	}
	return e.Ext.Type(name)
}

func (e *Environment) StringDumpVariables() string { // For outputting them as a file of assignments
	result := ""
	for k, v := range e.Store {
		if v.access != ACCESS_CONSTANT {
			result = result + k + " = " + (v.obj).Inspect(ViewCharmLiteral) + "\n"
		}
	}
	return result
}

func (e *Environment) String() string {
	result := ""
	for k, v := range e.Store {
		result = result + k + " = " + (v.obj).Inspect(ViewCharmLiteral) + ", "
	}
	if e.Ext != nil {
		result = result + "\n    + {" + e.Ext.String() + "}"
	}
	return result
}

func (e *Environment) VarsOnly() string {
	result := ""
	for k, v := range e.Store {
		if v.access != ACCESS_CONSTANT {
			result = result + k + " = " + (v.obj).Inspect(ViewCharmLiteral) + ", "
		}
	}
	if e.Ext != nil {
		result = result + "\n    + {" + e.Ext.VarsOnly() + "}"
	}
	return result
}

func (e *Environment) Exists(name string) bool {
	_, ok := e.Store[name]
	if ok || e.Ext == nil {
		return ok
	}
	return e.Ext.Exists(name)
}

// Variable assumed to exist, and type check to have been done.
func (e *Environment) UpdateVar(name string, val Object) {
	storage, ok := e.Store[name]
	if ok {
		if storage.access == ACCESS_PUBLIC || storage.access == ACCESS_PRIVATE {
			e.Pending[name] = val
			return
		}
		if storage.access == ACCESS_GLOBAL {
			e.Ext.UpdateVar(name, val)
			return
		}
		e.Store[name] = Storage{val, e.Store[name].access, e.Store[name].VarType}
		return
	}
	e.Ext.UpdateVar(name, val)
}

func (e *Environment) GetAccess(name string) AccessType {
	_, ok := e.Store[name]
	if ok || e.Ext == nil {
		return e.Store[name].access
	}
	return e.Ext.GetAccess(name)
}

func (e *Environment) Set(name string, val Object) Object {
	storage, ok := e.Store[name]
	if ok {
		if storage.access == ACCESS_PUBLIC || storage.access == ACCESS_PRIVATE {
			e.Pending[name] = val
			return val
		}
		if storage.access == ACCESS_GLOBAL {
			e.Ext.Set(name, val)
		}
	}
	e.Store[name] = Storage{val, e.Store[name].access, e.Store[name].VarType}
	return val
}

func (e *Environment) HardSet(name string, val Object) Object {
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
	e.Store[name] = Storage{val, ACCESS_CONSTANT, ConcreteType(val)}
	return val
}

func (e *Environment) InitializeLocal(name string, val Object, ty string) Object {
	e.Store[name] = Storage{val, ACCESS_LOCAL, ty}
	return val
}

func (e *Environment) IsConstant(name string) bool {
	return e.Store[name].access == ACCESS_CONSTANT
}

func (e *Environment) IsPrivate(name string) bool {
	return e.Store[name].access == ACCESS_PRIVATE
}
