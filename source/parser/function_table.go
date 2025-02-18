package parser

import (
	"github.com/tim-hardcastle/Pipefish/source/ast"
)

type FunctionTable map[string][]*ast.PrsrFunction

func (ft FunctionTable) Describe(p *Parser, functionName string) string {
	result := "Function table for " + functionName + "\n\n"
	if functions, ok := ft[functionName]; ok {
		for _, f := range functions {
			result = result + f.NameSig.String() + " : " + f.Body.String() + "\n\n"
		}
		return result
	} else {
		return "Function table has no entry for " + functionName
	}
}
