package parser

import (

	"charm/ast"
	"charm/object"
	"charm/signature"
)


type FunctionTable map[string] []ast.Function

func (ft FunctionTable) Add(T TypeSystem, keyword string, f ast.Function) (ok bool) {
	if functions, ok := ft[keyword]; ok {
		functions, ok = AddInOrder(T, functions, f)
		ft[keyword] = functions
		return ok
	}
	ft[keyword] = []ast.Function{f}
	return true
} 

func (p *Parser) FindFunction(keyword string, parameters []object.Object) (returnFunction ast.Function, returnError string) {
	if _, ok := p.FunctionTable[keyword]; !ok {
		return returnFunction, "keyword"
	}
	for _, f := range p.FunctionTable[keyword] {
		if p.ParamsFitSig(f, parameters) {
			return f, ""
		}	
	}	
	return returnFunction, "sig"
}

func (p *Parser) ParamsFitSig(f ast.Function, parameters []object.Object) bool {
	
	if len(parameters) == 0 && len(f.Sig) == 0 { return true } 
	if len(parameters) > len(f.Sig) &&
	/**/ !(f.Sig[len(f.Sig) - 1].VarType == "tuple" || f.Sig[len(f.Sig) - 1].VarType == "any") {
		return false
	}
	if len(parameters) < len(f.Sig) && 
	/**/! (len(f.Sig) == len(parameters) + 1 && (f.Sig[len(parameters)].VarType == "tuple") || f.Sig[len(parameters)].VarType == "any")   {
		return false
	}
	for i, param := range parameters {
		if i == len(f.Sig) - 1 && (f.Sig[i].VarType == "tuple" || f.Sig[i].VarType == "any") {
			return true
		}
		if f.Sig[i].VarType != object.TrueType(param) &&
		!p.TypeSystem.PointsTo(object.TrueType(param), f.Sig[i].VarType){
			return false
		}
		if param.Type() == object.BLING_OBJ &&
			f.Sig[i].VarType == "bling" &&
			param.(*object.Bling).Value != f.Sig[i].VarName {
				return false
		}
		if i == len(parameters) - 1 {
			return true
		}	
	}

	if (len(f.Sig) == len(parameters) + 1 && 
	/**/(f.Sig[len(parameters)].VarType == "tuple") || f.Sig[len(parameters)].VarType == "any") { return true }

	return false
}


func UpdateEnvironment(sig signature.Signature, params []object.Object, env *object.Environment) *object.Environment { 
	for i := 0; i < len(sig); i++ {
		if sig[i].VarType == object.BLING_OBJ {
			continue
		}
		if i < len(sig) - 1 || sig[i].VarType != object.TUPLE_OBJ {
			env.Set(sig[i].VarName, params[i])
		} else {
			env.Set(sig[i].VarName, &object.Tuple{Elements: params[i:]})
		}
	}
	return env
}