package parser

import (
	"pipefish/source/ast"
)

type FunctionTable map[string][]*ast.PrsrFunction

func (ft FunctionTable) Add(T TypeSys, functionName string, f *ast.PrsrFunction) (ok bool) {
	if functions, ok := ft[functionName]; ok {
		functions, ok = AddInOrder(T, functions, f)
		ft[functionName] = functions
		return ok
	}
	ft[functionName] = []*ast.PrsrFunction{f}
	return true
}
