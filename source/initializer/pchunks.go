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

var PARSEABLE = []declarationType{cloneDeclaration, structDeclaration, constantDeclaration,
	variableDeclaration, functionDeclaration, commandDeclaration}

func (iz *Initializer) parseEverythingElse() {
	iz.parsedCode = make([][]parsedCode, len(iz.tokenizedCode))
	for _, decType := range PARSEABLE {
		iz.parsedCode[decType] = make([]parsedCode, len(iz.tokenizedCode[decType]))
		for i, _ := range iz.tokenizedCode[decType] {
			iz.parsedCode[decType][i] = iz.parse(decType, i)
			iz.P.ResetNesting()
		}
	}
}

func (iz *Initializer) parse(decType declarationType, decNumber int) parsedCode {
	tc := iz.tokenizedCode[decType][decNumber]
	switch tc := tc.(type) {
	case *tokenizedCloneDeclaration:
		var body ast.Node
		if tc.body.Length() != 0 {
			iz.P.TokenizedCode = tc.body
			body = iz.P.ParseTokenizedChunk()
		}
		return &parsedTypecheck{
			decType:    decType,
			decNumber:  decNumber,
			indexTok:   ixPtr(tc),
			parameters: iz.makeAstSigFromTokenizedSig(tc.params),
			body:       body,
		}
	case *tokenizedConstOrVarDeclaration:
		sig := iz.makeAstSigFromTokenizedSig(tc.sig)
		iz.P.TokenizedCode = tc.body
		return &parsedAssignment{
			decType:   decType,
			decNumber: decNumber,
			indexTok:  ixPtr(tc),
			sig:       sig,
			body:      iz.P.ParseTokenizedChunk(),
		}
	case *tokenizedFunctionDeclaration:
		iz.P.TokenizedCode = tc.body
		parsedBody := iz.P.ParseTokenizedChunk()
		var parsedGiven ast.Node
		if tc.given != nil {
			iz.P.TokenizedCode = tc.given
			parsedGiven = iz.P.ParseTokenizedChunk()
		}
		return &parsedFunction{
			decType:   decType,
			decNumber: decNumber,
			private:   tc.private,
			op:        tc.op,
			pos:       tc.pos,
			sig:       iz.makeAstSigFromTokenizedSig(tc.sig),

			body:  parsedBody,
			given: parsedGiven,
			callInfo: &compiler.CallInfo{
				Compiler:    iz.cp,
				Number:      DUMMY,
				ReturnTypes: iz.makeRetsFromTokenizedReturns(tc.rets),
			},
		}
	case *tokenizedStructDeclaration:
		var body ast.Node
		if tc.body.Length() != 0 {
			iz.P.TokenizedCode = tc.body
			body = iz.P.ParseTokenizedChunk()
		}
		return &parsedTypecheck{
			decType:    decType,
			decNumber:  decNumber,
			indexTok:   ixPtr(tc),
			parameters: iz.makeAstSigFromTokenizedSig(tc.params),
			body:       body,
		}
	default:
		panic("You're not meant to parse that!")
	}
}
