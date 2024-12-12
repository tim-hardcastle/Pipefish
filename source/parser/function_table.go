package parser

import (
	"github.com/tim-hardcastle/Pipefish/source/ast"
)

type FunctionTable map[string][]*ast.PrsrFunction

func (ft FunctionTable) Add(p *Parser, functionName string, f *ast.PrsrFunction) *ast.PrsrFunction {
	if functions, ok := ft[functionName]; ok {
		functions, conflictingFunction := p.AddInOrder(functions, f)
		ft[functionName] = functions
		return conflictingFunction
	}
	ft[functionName] = []*ast.PrsrFunction{f}
	return nil
}

func (ft FunctionTable) Describe(p *Parser, functionName string) string {
	result := "Function table for " + functionName + "\n\n"
	if functions, ok := ft[functionName]; ok {
		for _, f := range functions {
			result = result + f.NameSig.String() + "\n"
		}
		return result
	} else {
		return "Function table has no entry for " + functionName
	}
}
