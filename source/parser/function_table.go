package parser

import (
	"pipefish/source/ast"
)

type FunctionTable map[string][]*ast.PrsrFunction

func (ft FunctionTable) Add(T TypeSys, functionName string, f *ast.PrsrFunction) (*ast.PrsrFunction) {
	if functions, ok := ft[functionName]; ok {
		functions, conflictingFunction := AddInOrder(T, functions, f)
		ft[functionName] = functions
		return conflictingFunction
	}
	ft[functionName] = []*ast.PrsrFunction{f}
	return nil
}
