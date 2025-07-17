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
}

type parsedFunction struct {
	decType    declarationType    // Function or command.
	decNumber  int                // The order of declaration (with functions and commands counted separately.)
	op         token.Token        // The token where the function/operation name is declared.
	pos        opPosition         // PREFIX / INFIX / SUFFIX / UNFIX.
	sig        ast.AstSig         // The call signature.
	rets       ast.AstSig         // The return signature (in the same format, but with the variable names unused. TODO --- todon't.)
	body       ast.Node           // The parsed body of the function, non-empty by construction.
	given      ast.Node           // The `given` block (nil if empty).
	compiler   *compiler.Compiler // The compiler that compiled it, needed to cope with shared functions. Initialized as nil.
	compNumber uint32             // The index of the compiled function data in the compiler. Initialized as DUMMY.
}

type parsedAssignment struct {
	decType   declarationType    // Constant or variable
	decNumber int
	indexTok  *token.Token
	sig       ast.AstSig
	body      ast.Node
}

type parsedTypecheck struct {
	decType    declarationType    // Clone or struct
	decNumber  int
	indexTok   *token.Token
	parameters ast.AstSig
	body       ast.Node
}

var PARSEABLE = []declarationType{cloneDeclaration, structDeclaration, constantDeclaration,
	variableDeclaration, functionDeclaration, commandDeclaration}

func (iz *Initializer) newParseEverything() {
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
		return &parsedTypecheck {
			decType: decType,
			decNumber: decNumber,
			indexTok: ixPtr(tc),
			parameters: iz.makeAstSigFromTokenizedSig(tc.params),
			body: body,
		}
	case *tokenizedConstOrVarDeclaration:
		iz.P.TokenizedCode = tc.body
		return &parsedAssignment {
			decType: decType,
			decNumber: decNumber,
			indexTok: ixPtr(tc),
			body: iz.P.ParseTokenizedChunk(),
		}
	case *tokenizedFunctionDeclaration:
		iz.P.TokenizedCode = tc.body
		parsedBody := iz.P.ParseTokenizedChunk()
		var parsedGiven ast.Node
		if tc.given != nil {
			iz.P.TokenizedCode = tc.given
			parsedGiven = iz.P.ParseTokenizedChunk()
		}
		return &parsedFunction {
			decType: decType,
			decNumber: decNumber,
			op: tc.op,
			pos: tc.pos,
			sig: iz.makeAstSigFromTokenizedSig(tc.sig),
			rets: iz.makeRetsFromTokenizedReturns(tc.rets),
			body: parsedBody,
			given: parsedGiven,
			compiler: nil,
			compNumber: DUMMY,
		}
	case *tokenizedStructDeclaration:
		var body ast.Node
		if tc.body.Length() != 0 {
			iz.P.TokenizedCode = tc.body
			body = iz.P.ParseTokenizedChunk()
		}
		return &parsedTypecheck {
			decType: decType,
			decNumber: decNumber,
			indexTok: ixPtr(tc),
			parameters: iz.makeAstSigFromTokenizedSig(tc.params),
			body: body,
		}
	default:
		panic("You're not meant to parse that!")
	}
}
