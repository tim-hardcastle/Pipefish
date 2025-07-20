package initializer

// The function table is an intermediate step towards producing the function tree in the compiler,
// which sorts overloaded functions in order of specificity as they're added to the table.

import (
	"github.com/tim-hardcastle/Pipefish/source/parser"
)

type functionTable map[string][]*parsedFunction

// Methods for manipulating the function table.

func (iz *Initializer) Add(functionName string, f *parsedFunction) *parsedFunction {
	if functions, ok := iz.functionTable[functionName]; ok {
		functions, conflictingFunction := iz.AddInOrder(functions, f)
		iz.functionTable[functionName] = functions
		return conflictingFunction
	}
	iz.functionTable[functionName] = []*parsedFunction{f}
	return nil
}

func (iz *Initializer) AddInOrder(S []*parsedFunction, f *parsedFunction) ([]*parsedFunction, *parsedFunction) {
	fSig := f.callInfo.Compiler.P.MakeAbstractSigFromStringSig(f.sig)
	for i := 0; i < len(S); i++ {
		gSig := S[i].callInfo.Compiler.P.MakeAbstractSigFromStringSig(S[i].sig)
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

func insert(a []*parsedFunction, value *parsedFunction, index int) []*parsedFunction {
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
			result = result + f.sig.String() + " : " + f.body.String() + "\n\n"
		}
		return result
	} else {
		return "Function table has no entry for " + functionName
	}
}


