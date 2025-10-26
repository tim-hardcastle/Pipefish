package initializer

import (
	"github.com/tim-hardcastle/Pipefish/source/ast"
	"github.com/tim-hardcastle/Pipefish/source/compiler"
	"github.com/tim-hardcastle/Pipefish/source/token"
)

// We need to parse everything in order to sort out the dependencies between the declarations.

// "Everything" boils down to function/command declarations, constant/variable declarations,
// and type validation logic.

type parsedCode interface {
	getToken() *token.Token
}

type parsedFunction struct {
	decType   declarationType // Function or command.
	decNumber int             // The order of declaration (with functions and commands counted separately.)
	private   bool
	op        token.Token // The token where the function/operation name is declared.
	pos       opPosition  // PREFIX / INFIX / SUFFIX / UNFIX.
	sig       ast.AstSig  // The call signature.
	body      ast.Node    // The parsed body of the function, non-empty by construction.
	given     ast.Node    // The `given` block (nil if empty).
	// Information shared with the function tree, needed to make a function call.
	// This includes the return signatures because if recursion is involved we don't infer them
	// and this is the next best thing.
	callInfo *compiler.CallInfo
	isBoilerplate bool    // If the function has a body generated in Pipefish, i.e. presently only the `post` boilerplate around commands with refs.
}

func (pc *parsedFunction) getToken() *token.Token { return &pc.op }

type parsedAssignment struct {
	decType   declarationType // Constant or variable
	decNumber int
	indexTok  *token.Token
	sig       ast.AstSig
	body      ast.Node
}

func (pc *parsedAssignment) getToken() *token.Token { return pc.indexTok }

type parsedTypecheck struct {
	decType    declarationType // Clone or struct
	decNumber  int
	indexTok   *token.Token
	parameters ast.AstSig
	body       ast.Node
}

func (pc *parsedTypecheck) getToken() *token.Token { return pc.indexTok }

// When a parameterized type is instantiated, we monomorphize the typechecking because it
// would be a waste of time to e.g. keep fetching the '3' to check that things are in a
// type Vec{3}, etc.
type parsedTypeInstance struct {
	typeCheck      ast.Node              //
	instantiatedAt *token.Token          // The place in the code (or one of the places) where the type instance is named.
	env            *compiler.Environment // The values for the parameters, already put into an environment as named constants, ready for compilation.
}

func (pc *parsedTypeInstance) getToken() *token.Token { return pc.instantiatedAt }


