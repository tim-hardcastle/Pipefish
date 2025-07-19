package initializer

// The function table is an intermediate step towards producing the function tree in the compiler,
// which sorts overloaded functions in order of specificity as they're added to the table.

import (
	"github.com/tim-hardcastle/Pipefish/source/ast"
	"github.com/tim-hardcastle/Pipefish/source/compiler"
	"github.com/tim-hardcastle/Pipefish/source/parser"
)

type functionTable map[string][]*ast.PrsrFunction

// Methods for manipulating the function table.

func (iz *Initializer) Add(functionName string, f *ast.PrsrFunction) *ast.PrsrFunction {
	if functions, ok := iz.functionTable[functionName]; ok {
		functions, conflictingFunction := iz.AddInOrder(functions, f)
		iz.functionTable[functionName] = functions
		return conflictingFunction
	}
	iz.functionTable[functionName] = []*ast.PrsrFunction{f}
	return nil
}

func (iz *Initializer) AddInOrder(S []*ast.PrsrFunction, f *ast.PrsrFunction) ([]*ast.PrsrFunction, *ast.PrsrFunction) {
	fSig := f.CallInfo.(*compiler.CallInfo).Compiler.P.MakeAbstractSigFromStringSig(f.NameSig)
	for i := 0; i < len(S); i++ {
		gSig := S[i].CallInfo.(*compiler.CallInfo).Compiler.P.MakeAbstractSigFromStringSig(S[i].NameSig)
		yes, ok := parser.IsMoreSpecific(fSig, gSig)
		if !ok {
			return S, S[i]
		}
		if yes {
			S = insert(S, f, i)
			return S, nil
		}
	}
	S = append(S, f)
	return S, nil
}

func insert(a []*ast.PrsrFunction, value *ast.PrsrFunction, index int) []*ast.PrsrFunction {
	if len(a) == index { // nil or empty slice or after last element
		return append(a, value)
	}
	a = append(a[:index+1], a[index:]...) // index < len(a)
	a[index] = value
	return a
}

func (ft functionTable) Describe(functionName string) string {
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


