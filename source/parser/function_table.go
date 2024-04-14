package parser

import (
	"pipefish/source/ast"
)

type FunctionTable map[string][]ast.Function

func (ft FunctionTable) Add(T TypeSystem, functionName string, f ast.Function) (ok bool) {
	f.Number = uint32(ft.Count())
	if functions, ok := ft[functionName]; ok {
		functions, ok = AddInOrder(T, functions, f)
		ft[functionName] = functions
		return ok
	}
	ft[functionName] = []ast.Function{f}
	return true
}

func (ft FunctionTable) Count() int {
	result := 0
	for _, v := range ft {
		result = result + len(v)
	}
	return result
}
