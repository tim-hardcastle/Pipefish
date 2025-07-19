package compiler

import (
	"github.com/tim-hardcastle/Pipefish/source/ast"
	"github.com/tim-hardcastle/Pipefish/source/values"
)

type FunctionTree = struct { // Contains the start of a function tree plus the things all the functions with the same name have in common.
	Tree     *FnTreeNode
	RefCount int // For reasons, the reference variables in a function's sig must (a) come at the start (b) be of the same number for each of the overloaded variants of the function.
}

// A pointer to this struct is hung on the end of each branch of a function tree, to
// give it enough information for the compiler to look it up in the table of compiled
// functions.
//
// The original to which this is a pointer is part of the parsedFunction structures in
// the initializer, enabling us to assign the right information to the function trees by
// assigning it to the parsedFunctions.
type CallInfo struct {
	Compiler *Compiler
	Number   uint32
	// Having ReturnTypes here is a bit of kludge. It's required during intitialization to supply return types for interface backtracks.
	ReturnTypes ast.AstSig
}

type FnTreeNode struct {
	CallInfo *CallInfo
	Branch   []*TypeNodePair
}

type TypeNodePair struct { // This exists because we need an *ordered* collection of type-node pairs.
	Type     values.AbstractType
	IsVararg bool
	Node     *FnTreeNode
}

func (tree FnTreeNode) String() string {
	result := "["
	for i, v := range tree.Branch {
		result = result + v.Type.String()
		if v.Node.CallInfo != nil {
			result = result + "function call"
		} else {
			result = result + v.Node.String()
		}
		if i < len(tree.Branch)-1 {
			result = result + ", "
		}
	}
	return result + "]"
}

func (tree FnTreeNode) IndentString(indent string) string {
	result := ""
	for _, v := range tree.Branch {
		result = result + "\n" + indent + v.Type.String()
		if v.Node.CallInfo != nil {
			result = result + "function call"
		} else {
			result = result + v.Node.IndentString(indent+"    ")
		}
	}
	return result
}
