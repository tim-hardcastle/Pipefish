package initializer

import (
	"strconv"

	"github.com/tim-hardcastle/pipefish/source/ast"
	"github.com/tim-hardcastle/pipefish/source/compiler"
	"github.com/tim-hardcastle/pipefish/source/parser"
	"github.com/tim-hardcastle/pipefish/source/settings"
	"github.com/tim-hardcastle/pipefish/source/token"
	"github.com/tim-hardcastle/pipefish/source/values"
)

// When we first scan the stream of tokens, it is convenient to break it down into chunks
// of tokens having their own format. E.g. even before any parsing, we can break a top-level
// function declaration down into its call signature, return signature, and body, and indeed
// analyse the function signatures into variables and types, using only lexical criteria.

// As we go, we validate some well-formedness properties:

// * Functions and commands have non-empty bodies.
// * If a validation block was present, it was non-empty.
// * A parameterized type can't have an empty list of parameters.
// * Every parameter of a parameterized type has (something that lexically could be) a type.
// * 'make' statements contain a non-empty list of types.
// * So do abstract type definitions.
// * The parent type of a clone is cloneable.
// * Every argument of a sig has been assigned a type.

// This then allows us to set up the parser with information about what things are functions,
// what things are types, etc.

// Since different declarations have different contents, we slap an interface over the top.

type tokenizedCode interface {
	getDeclarationType() declarationType
	indexToken() token.Token
	api() (string, string, bool)
}

func ixPtr(tc tokenizedCode) *token.Token {
	t := tc.indexToken()
	return &t
}

type tokenizedAbstractDeclaration struct {
	private   bool            // Whether it's declared private.
	op        token.Token     // The type operator.
	types     [][]token.Token // The names of the constituent types.
	docString string          // Documents what it does.
}

func (tc *tokenizedAbstractDeclaration) getDeclarationType() declarationType {
	return abstractDeclaration
}

func (tc *tokenizedAbstractDeclaration) indexToken() token.Token { return tc.op }

func (tc *tokenizedAbstractDeclaration) api() (string, string, bool) {
	if tc.private || settings.MandatoryImportSet().Contains(tc.op.Source) {
		return "", "", false
	}
	result := tc.op.Literal + " = "
	sep := ""
	for _, ty := range tc.types {
		result = result + sep + parser.StringifyTypeName(ty)
		sep = "/"
	}
	return result, tc.docString, true
}

type tokenizedCloneDeclaration struct {
	private   bool                      // Whether it's declared private.
	op        token.Token               // The type operator.
	params    parser.TokSig             // The type parameters, if any.
	parentTok token.Token               // The type being cloned.
	requests  []token.Token             // Types requested by the 'using' clause
	body      *token.TokenizedCodeChunk // Validation, if any.
	docString string                    // Documents what it does.
}

func (tc *tokenizedCloneDeclaration) getDeclarationType() declarationType { return cloneDeclaration }

func (tc *tokenizedCloneDeclaration) indexToken() token.Token { return tc.op }

func (tc *tokenizedCloneDeclaration) api() (string, string, bool) {
	if tc.private || settings.MandatoryImportSet().Contains(tc.op.Source) {
		return "", "", false
	}
	result := tc.op.Literal + " = clone"
	if len(tc.params) > 0 {
		result = result + "{"
	}
	result = result + " " + tc.parentTok.Literal
	if len(tc.requests) > 0 {
		result = result + " using "
		sep := ""
		for _, req := range tc.requests {
			result = result + sep + req.Literal
			sep = ", "
		}
	}
	return result, tc.docString, true
}

type tokenizedConstOrVarDeclaration struct {
	decType     declarationType           // Either constantDeclaration or variableDeclaration.
	private     bool                      // Whether it's declared private.
	sig         parser.TokSig             // The signature of the assignment
	assignToken token.Token               // The assignment operator '='.
	body        *token.TokenizedCodeChunk // The rhs of the assignment.
	docString   string                    // Documents what it does.
}

func (tc *tokenizedConstOrVarDeclaration) getDeclarationType() declarationType { return tc.decType }

func (tc *tokenizedConstOrVarDeclaration) indexToken() token.Token { return tc.assignToken }

func (tc *tokenizedConstOrVarDeclaration) api() (string, string, bool) {
	if tc.private || settings.MandatoryImportSet().Contains(tc.assignToken.Source) {
		return "", "", false
	}
	return tc.sig.SimpleString(), tc.docString, true
}

type tokenizedEnumDeclaration struct {
	private   bool          // Whether it's declared private.
	op        token.Token   // The type operator.
	elements  []token.Token // The elements of the enum.
	docString string        // Documents what it does.
}

func (tc *tokenizedEnumDeclaration) getDeclarationType() declarationType { return enumDeclaration }

func (tc *tokenizedEnumDeclaration) indexToken() token.Token { return tc.op }

func (tc *tokenizedEnumDeclaration) api() (string, string, bool) {
	if tc.private || settings.MandatoryImportSet().Contains(tc.op.Source) {
		return "", "", false
	}
	result := tc.op.Literal + " = enum "
	sep := ""
	for _, el := range tc.elements {
		result = result + sep + el.Literal
		sep = ", "
	}
	return result, tc.docString, true
}

type tokenizedExternalOrImportDeclaration struct {
	decType   declarationType // Either importDeclaration or externalDeclaration.
	private   bool            // Whether it's declared private.
	golang    bool            // If we're importing a Go library.
	name      token.Token     // The name of the service as an identifier.
	path      token.Token     // The path as a string literal.
	docString string          // Documents what it does.
}

func (tc *tokenizedExternalOrImportDeclaration) getDeclarationType() declarationType {
	return tc.decType
}

func (tc *tokenizedExternalOrImportDeclaration) indexToken() token.Token { return tc.name }

func (tc *tokenizedExternalOrImportDeclaration) api() (string, string, bool) {
	if tc.private || settings.MandatoryImportSet().Contains(tc.name.Source) {
		return "", "", false
	}
	return tc.name.Literal, tc.docString, true
}

type tokenizedFunctionDeclaration struct {
	decType       declarationType           // Can be commandDeclaration, functionDeclaration.
	private       bool                      // Whether it's private.
	op            token.Token               // The name of the fumction/operation.
	pos           opPosition                // Whether it's a prefix, infix, suffix, or unfix.
	sig           parser.TokSig             // The call signature, with the names of arguments as tokens and the types as lists of tokens.
	rets          parser.TokReturns         // The return types, as lists of tokens.
	body          *token.TokenizedCodeChunk // The body of the function.
	given         *token.TokenizedCodeChunk // The 'given' block, if any.
	docString     string                    // Documents what it does.
	isBoilerplate bool                      // If the function has a body generated in Pipefish, i.e. presently only the `post` boilerplate around commands with refs.
}

type opPosition int

const (
	prefix opPosition = iota
	infix
	suffix
	unfix
)

func (tc *tokenizedFunctionDeclaration) getDeclarationType() declarationType { return tc.decType }

func (tc *tokenizedFunctionDeclaration) indexToken() token.Token { return tc.op }

func (tc *tokenizedFunctionDeclaration) api() (string, string, bool) {
	if tc.private || settings.MandatoryImportSet().Contains(tc.op.Source) {
		return "", "", false
	}
	result := ""
	if tc.pos == prefix || tc.pos == unfix {
		result = tc.op.Literal
	}
	if len(tc.sig) > 0 && tc.sig[0].IsBling() {
		result = result + " "
	}
	result = result + tc.sig.String()
	if len(tc.rets) > 0 {
		result = result + " -> " + tc.rets.String()
	}
	if tc.pos == suffix {
		result = result + tc.op.Literal
	}
	return result, tc.docString, true
}

type tokenizedInterfaceDeclaration struct {
	private   bool                            // Whether it's declared private.
	op        token.Token                     // The type operator.
	sigs      []*tokenizedFunctionDeclaration // The signatures defining the interface.
	docString string                          // Documents what it does.
}

func (tc *tokenizedInterfaceDeclaration) getDeclarationType() declarationType {
	return interfaceDeclaration
}

func (tc *tokenizedInterfaceDeclaration) indexToken() token.Token { return tc.op }

func (tc *tokenizedInterfaceDeclaration) api() (string, string, bool) {
	if tc.private || settings.MandatoryImportSet().Contains(tc.op.Source) {
		return "", "", false
	}
	return tc.op.Literal, tc.docString, true
}

type tokenizedGolangDeclaration struct {
	private bool        // Whether it's declared private.
	goCode  token.Token // The code has already been squished into a single Pipefish token.
}

func (tc *tokenizedGolangDeclaration) getDeclarationType() declarationType { return golangDeclaration }

func (tc *tokenizedGolangDeclaration) indexToken() token.Token { return tc.goCode }

func (tc *tokenizedGolangDeclaration) api() (string, string, bool) {
	panic("This doesn't need an api description.")
}

type tokenizedMakeDeclaration struct {
	private  bool          // Whether it's declared private.
	typeToks []token.Token // The names of the types to declare.
}

func (tc *tokenizedMakeDeclaration) getDeclarationType() declarationType { return makeDeclaration }

func (tc *tokenizedMakeDeclaration) indexToken() token.Token { return tc.typeToks[0] }

func (tc *tokenizedMakeDeclaration) api() (string, string, bool) {
	panic("This doesn't need an api description.")
}

type tokenizedMakeDeclarations struct {
	private   bool            // Whether it's declared private.
	makeToken token.Token     // The token saying 'make'.
	types     [][]token.Token // The names of the types to declare.
}

func (tc *tokenizedMakeDeclarations) getDeclarationType() declarationType { return makeDeclarations }

func (tc *tokenizedMakeDeclarations) indexToken() token.Token { return tc.makeToken }

func (tc *tokenizedMakeDeclarations) api() (string, string, bool) {
	panic("This doesn't need an api description.")
}

type tokenizedStructDeclaration struct {
	private   bool                      // Whether it's declared private.
	op        token.Token               // The type operator.
	params    parser.TokSig             // The type parameters, if any.
	sig       parser.TokSig             // The signature of the struct.
	body      *token.TokenizedCodeChunk // Validation logic, if any.
	docString string                    // Documents what it does.
}

func (tc *tokenizedStructDeclaration) getDeclarationType() declarationType { return structDeclaration }

func (tc *tokenizedStructDeclaration) indexToken() token.Token { return tc.op }

func (tc *tokenizedStructDeclaration) api() (string, string, bool) {
	if tc.private || settings.MandatoryImportSet().Contains(tc.op.Source) {
		return "", "", false
	}
	result := tc.op.Literal + " = struct"
	if len(tc.params) > 0 {
		result = result + "{" + tc.params.SimpleString() + "}"
	}
	result = result + " " + tc.sig.String()
	return result, tc.docString, true
}

// As with all the chunkers, this assumes that the p.curToken is the first token of
// the thing we're trying to slurp.
// It will end with the p.curTok being the EOF/NEWLINE terminating the declaration.
func (iz *Initializer) ChunkConstOrVarDeclaration(isConst, private bool, docString string) (tokenizedCode, bool) {
	sig, ok := iz.P.ChunkNameTypePairs(parser.INFERRED)
	decType := variableDeclaration
	if isConst {
		decType = constantDeclaration
	}
	if !ok {
		iz.finishChunk()
		return &tokenizedConstOrVarDeclaration{}, false
	}
	iz.P.NextToken()
	if !iz.P.CurTokenIs(token.ASSIGN) {
		iz.throw("init/assign", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedConstOrVarDeclaration{}, false
	}
	assignTok := iz.P.CurToken
	toks := []token.Token{}
	for {
		iz.P.NextToken()
		if iz.P.CurTokenIs(token.NEWLINE) || iz.P.CurTokenIs(token.EOF) {
			break
		}
		toks = append(toks, iz.P.CurToken)
	}
	return &tokenizedConstOrVarDeclaration{decType, private, sig, assignTok, token.MakeCodeChunk(toks, private), docString}, true
}

func (iz *Initializer) ChunkGolangDeclaration(private bool) (tokenizedCode, bool) {
	result := tokenizedGolangDeclaration{private, iz.P.CurToken}
	iz.P.NextToken()
	if !(iz.P.CurTokenIs(token.NEWLINE) || iz.P.CurTokenIs(token.EOF)) {
		iz.throw("init/golang", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedGolangDeclaration{}, false
	}
	return &result, true
}

// As with all the chunkers, this assumes that the p.curToken is the first token of
// the thing we're trying to slurp.
// It will end with the p.curTok being the EOF/NEWLINE terminating the declaration.
func (iz *Initializer) ChunkImportOrExternalDeclaration(isExternal, private bool, docString string) (tokenizedCode, bool) {
	decType := importDeclaration
	if isExternal {
		decType = externalDeclaration
	}
	var name, path token.Token
	golang := false
	switch iz.P.CurToken.Type {
	case token.GOLANG: // TODO --- the lexer shoves everything into the gocode token and it should only do that if followed by '{'.
		path = iz.P.CurToken
		golang = true
	case token.IDENT:
		name = iz.P.CurToken
		iz.P.NextToken()
		if !(iz.P.CurTokenIs(token.IDENT) && iz.P.CurToken.Literal == "::") {
			iz.throw("init/impex/pair", &iz.P.CurToken)
			iz.finishChunk()
			return &tokenizedExternalOrImportDeclaration{}, false
		}
		iz.P.NextToken()
		if !iz.P.CurTokenIs(token.STRING) {
			iz.throw("init/impex/string", &iz.P.CurToken)
			iz.finishChunk()
			return &tokenizedExternalOrImportDeclaration{}, false
		}
		path = iz.P.CurToken
	case token.STRING:
		path = iz.P.CurToken
	default:
		iz.throw("init/impex/expect", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedExternalOrImportDeclaration{}, false
	}
	iz.P.NextToken()
	if !(iz.P.CurTokenIs(token.NEWLINE) || iz.P.CurTokenIs(token.EOF)) {
		iz.throw("init/impex/end", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedExternalOrImportDeclaration{}, false
	}
	return &tokenizedExternalOrImportDeclaration{decType, private, golang, name, path, docString}, true
}

// As with all the chunkers, this assumes that the p.curToken is the first token of
// the thing we're trying to slurp.
// It will end with the p.curTok being the EOF/NEWLINE terminating the declaration.
func (iz *Initializer) ChunkTypeDeclaration(private bool, docString string) (tokenizedCode, bool) {
	if !iz.P.CurTokenIs(token.IDENT) {
		iz.throw("init/type/ident", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedEnumDeclaration{}, false // Just as a dummy value.
	}
	opTok := iz.P.CurToken
	if opTok.Literal == "make" {
		iz.P.NextToken()
		return iz.chunkMake(opTok, private)
	}
	iz.P.NextToken()
	if !iz.P.CurTokenIs(token.ASSIGN) {
		iz.throw("init/type/assign", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedEnumDeclaration{}, false
	}
	iz.P.NextToken()
	if !iz.P.CurTokenIs(token.IDENT) {
		iz.throw("init/type/expect/a", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedEnumDeclaration{}, false
	}
	decliteral := iz.P.CurToken.Literal
	iz.P.NextToken()
	switch decliteral {
	case "abstract":
		return iz.chunkAbstract(opTok, private, docString)
	case "clone":
		return iz.chunkClone(opTok, private, docString)
	case "enum":
		return iz.chunkEnum(opTok, private, docString)
	case "interface":
		return iz.chunkInterface(opTok, private, docString)
	case "struct":
		return iz.chunkStruct(opTok, private, docString)
	default:
		iz.throw("init/type/expect/b", &iz.P.CurToken, decliteral)
		iz.finishChunk()
		return &tokenizedEnumDeclaration{}, false
	}
}

// Starts after the word 'abstract', ends on NEWLINE or EOF.
func (iz *Initializer) chunkAbstract(opTok token.Token, private bool, docString string) (tokenizedCode, bool) {
	types := [][]token.Token{}
	for {
		if !iz.P.CurTokenIs(token.IDENT) {
			iz.throw("init/abstract/ident", &iz.P.CurToken)
			iz.finishChunk()
			return &tokenizedAbstractDeclaration{}, false
		}
		toks := []token.Token{}
		braces := 0
		for {
			if iz.P.CurTokenIs(token.LBRACE) {
				braces++
			}
			if iz.P.CurTokenIs(token.RBRACE) {
				braces--
			}
			if iz.P.CurTokenIs(token.EOF) || iz.P.CurTokenIs(token.NEWLINE) ||
				(iz.P.CurTokenIs(token.IDENT) && iz.P.CurToken.Literal == "/" && braces == 0) {
				break
			}
			toks = append(toks, iz.P.CurToken)
			iz.P.NextToken()
		}
		types = append(types, toks)
		if iz.P.CurTokenIs(token.EOF) || iz.P.CurTokenIs(token.NEWLINE) {
			break
		}
		iz.P.NextToken()
	}
	return &tokenizedAbstractDeclaration{private, opTok, types, docString}, true
}

// Starts after the word 'clone', ends on NEWLINE or EOF.
func (iz *Initializer) chunkClone(opTok token.Token, private bool, docString string) (tokenizedCode, bool) {
	params := parser.TokSig{}
	if iz.P.CurTokenIs(token.LBRACE) {
		iz.P.NextToken()
		var ok bool
		params, ok = iz.P.ChunkNameTypePairs(parser.MISSING_TYPE_ERROR)
		if !ok {
			iz.finishChunk()
			return &tokenizedCloneDeclaration{}, false
		}
		if len(params) == 0 {
			iz.throw("init/clone/params", &iz.P.CurToken)
			iz.finishChunk()
			return &tokenizedCloneDeclaration{}, false
		}
		if !iz.P.PeekTokenIs(token.RBRACE) {
			iz.throw("init/clone/rbrace", &iz.P.CurToken)
			iz.finishChunk()
			return &tokenizedCloneDeclaration{}, false
		}
		for _, pair := range params {
			if pair.Typename[0].Literal == "*error*" {
				iz.throw("init/clone/typed", &pair.Name)
				iz.finishChunk()
				return &tokenizedCloneDeclaration{}, false
			}
		}
		iz.P.NextToken()
		iz.P.NextToken()
	}
	if !iz.P.CurTokenIs(token.IDENT) {
		iz.throw("init/clone/ident", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedCloneDeclaration{}, false
	}
	typeTok := iz.P.CurToken
	_, ok := compiler.ClonableTypes[typeTok.Literal]
	if !ok {
		iz.throw("init/clone/type/c", &iz.P.CurToken, typeTok.Literal)
		iz.finishChunk()
		return &tokenizedCloneDeclaration{}, false
	}
	iz.P.NextToken()
	requests := []token.Token{}
	if iz.P.CurTokenIs(token.IDENT) && iz.P.CurToken.Literal == "using" {
	loop:
		for {
			iz.P.NextToken()
			if !iz.P.CurTokenIs(token.IDENT) {
				iz.throw("init/clone/op", &iz.P.CurToken)
				iz.finishChunk()
				return &tokenizedCloneDeclaration{}, false
			}
			requests = append(requests, iz.P.CurToken)
			iz.P.NextToken()
			switch iz.P.CurToken.Type {
			case token.COMMA:
				continue
			case token.COLON, token.NEWLINE, token.EOF:
				break loop
			default:
				iz.throw("init/clone/expect/a", &iz.P.CurToken)
				iz.finishChunk()
				return &tokenizedCloneDeclaration{}, false
			}
		}
	}
	validation := token.NewCodeChunk()
	if iz.P.CurTokenIs(token.COLON) {
		validation, ok = iz.P.SlurpBlock(false)
	}
	if iz.P.CurTokenIs(token.GIVEN) {
		iz.throw("init/clone/given", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedCloneDeclaration{}, false
	}
	if !(iz.P.CurTokenIs(token.NEWLINE) || iz.P.CurTokenIs(token.EOF)) {
		iz.throw("init/clone/expect/b", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedCloneDeclaration{}, false
	}
	return &tokenizedCloneDeclaration{private, opTok, params, typeTok, requests, validation, docString}, true
}

// Starts after the word 'enum', ends on NEWLINE or EOF.
func (iz *Initializer) chunkEnum(opTok token.Token, private bool, docString string) (tokenizedCode, bool) {
	toks := []token.Token{}
	for {
		if iz.P.CurTokenIs(token.IDENT) {
			toks = append(toks, iz.P.CurToken)
		} else {
			iz.throw("init/enum/ident", &iz.P.CurToken)
			iz.finishChunk()
			return &tokenizedEnumDeclaration{}, false
		}
		iz.P.NextToken()
		if iz.P.CurTokenIs(token.NEWLINE) || iz.P.CurTokenIs(token.EOF) {
			if len(toks) == 0 {
				iz.throw("init/enum/empty", &iz.P.CurToken)
				iz.finishChunk()
				return &tokenizedEnumDeclaration{}, false
			} else {
				return &tokenizedEnumDeclaration{private: private, op: opTok, elements: toks, docString: docString}, true
			}
		}
		if iz.P.CurTokenIs(token.COMMA) {
			iz.P.NextToken()
			continue
		}
		iz.throw("init/enum/expect", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedEnumDeclaration{}, false
	}
}

// Starts after the word 'interface', ends on NEWLINE or EOF.
func (iz *Initializer) chunkInterface(opTok token.Token, private bool, docString string) (tokenizedCode, bool) {
	if !iz.P.CurTokenIs(token.COLON) {
		iz.throw("init/interface/colon", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedInterfaceDeclaration{}, false
	}
	iz.P.NextToken()
	hasIndent := false
	if iz.P.CurTokenMatches(token.LPAREN, "|->") {
		hasIndent = true
		iz.P.NextToken()
	}
	sigs := []*tokenizedFunctionDeclaration{}
	for {
		sig, ok := iz.ChunkFunctionSignature()
		if !ok {
			iz.finishChunk()
			return &tokenizedInterfaceDeclaration{}, false
		}
		sigs = append(sigs, sig)
		if iz.P.CurTokenIs(token.EOF) {
			break
		}
		if iz.P.CurTokenIs(token.NEWLINE) {
			if !hasIndent {
				break
			}
			iz.P.NextToken()
			continue
		}
		if iz.P.CurTokenMatches(token.RPAREN, "<-|") {
			iz.P.NextToken()
			break
		}
	}
	return &tokenizedInterfaceDeclaration{private, opTok, sigs, docString}, true
}

// Starts after the word 'make', ends on NEWLINE or EOF.
func (iz *Initializer) chunkMake(opTok token.Token, private bool) (tokenizedCode, bool) {
	types := [][]token.Token{}
	for {
		if !iz.P.CurTokenIs(token.IDENT) {
			iz.throw("init/make/ident", &iz.P.CurToken)
			iz.finishChunk()
			return &tokenizedMakeDeclarations{}, false
		}
		toks := []token.Token{}
		braces := 0
		for {
			if iz.P.CurTokenIs(token.LBRACE) {
				braces++
			}
			if iz.P.CurTokenIs(token.RBRACE) {
				braces--
			}
			if iz.P.CurTokenIs(token.EOF) || iz.P.CurTokenIs(token.NEWLINE) ||
				(iz.P.CurTokenIs(token.COMMA) && braces == 0) {
				break
			}
			toks = append(toks, iz.P.CurToken)
			iz.P.NextToken()
		}
		types = append(types, toks)
		if iz.P.CurTokenIs(token.EOF) || iz.P.CurTokenIs(token.NEWLINE) {
			break
		}
		iz.P.NextToken()
	}
	return &tokenizedMakeDeclarations{private, opTok, types}, true
}

// Starts after the word 'struct', ends on NEWLINE or EOF.
func (iz *Initializer) chunkStruct(opTok token.Token, private bool, docString string) (tokenizedCode, bool) {
	params := parser.TokSig{}
	if iz.P.CurTokenIs(token.LBRACE) {
		iz.P.NextToken()
		var ok bool
		params, ok = iz.P.ChunkNameTypePairs(parser.MISSING_TYPE_ERROR)
		if !ok {
			iz.finishChunk()
			return &tokenizedStructDeclaration{}, false
		}
		if len(params) == 0 {
			iz.throw("init/struct/params", &iz.P.CurToken)
			iz.finishChunk()
			return &tokenizedStructDeclaration{}, false
		}
		if !iz.P.PeekTokenIs(token.RBRACE) {
			iz.throw("init/struct/rbrace", &iz.P.CurToken)
			iz.finishChunk()
			return &tokenizedStructDeclaration{}, false
		}
		for _, pair := range params {
			if pair.Typename[0].Literal == "*error*" {
				iz.throw("init/struct/type", &pair.Name)
				iz.finishChunk()
				return &tokenizedStructDeclaration{}, false
			}
		}
		iz.P.NextToken()
		iz.P.NextToken()
	}
	if !iz.P.CurTokenIs(token.LPAREN) {
		iz.throw("init/struct/lparen", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedStructDeclaration{}, false
	}
	ok := true
	args := parser.TokSig{}
	if !iz.P.PeekTokenIs((token.RPAREN)) {
		iz.P.NextToken()
		args, ok = iz.P.ChunkNameTypePairs(parser.ANY_OR_NULL)
	}
	if !ok {
		iz.finishChunk()
		return &tokenizedStructDeclaration{}, false
	}
	iz.P.NextToken()
	if !iz.P.CurTokenIs(token.RPAREN) {
		iz.throw("init/struct/rparen", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedStructDeclaration{}, false
	}
	iz.P.NextToken()
	validation := token.NewCodeChunk()
	if iz.P.CurTokenIs(token.COLON) {
		validation, ok = iz.P.SlurpBlock(false)
	}
	if !ok {
		return &tokenizedStructDeclaration{}, false
	}
	if iz.P.CurTokenIs(token.GIVEN) {
		iz.throw("init/struct/given", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedCloneDeclaration{}, false
	}
	if !(iz.P.CurTokenIs(token.NEWLINE) || iz.P.CurTokenIs(token.EOF)) {
		iz.throw("init/struct/expect", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedStructDeclaration{}, false
	}
	return &tokenizedStructDeclaration{private, opTok, params, args, validation, docString}, true
}

// This is called in case of a malformed definiion to try to skip over the rest of it
// and get on to the next definition.
// As it emulates slurping up a definition, it ends when the p.curToken is EOF or NEWLINE.
func (iz *Initializer) finishChunk() {
	for {
		if iz.P.CurTokenIs(token.EOF) || iz.P.CurTokenIs(token.NEWLINE) {
			return
		}
		if iz.P.CurTokenIs(token.COLON) {
			iz.P.SlurpBlock(true)
			if iz.P.CurTokenIs(token.GIVEN) {
				iz.P.SlurpBlock(true)
			}
		}
		iz.P.SafeNextToken()
	}
}

// As with all the chunkers, this assumes that the p.curToken is the first token of
// the thing we're trying to slurp.
// It will end with the p.curTok being the EOF/NEWLINE terminating the declaration.
func (iz *Initializer) ChunkFunction(cmd, private bool, docString string) (*tokenizedFunctionDeclaration, bool) {
	fn, ok := iz.ChunkFunctionSignature()
	if !ok {
		return &tokenizedFunctionDeclaration{}, false
	}
	if fn.body, ok = iz.P.SlurpBlock(false); !ok {
		return &tokenizedFunctionDeclaration{}, false
	}
	if iz.P.CurTokenIs(token.GIVEN) {
		if fn.given, ok = iz.P.SlurpBlock(false); !ok {
			return &tokenizedFunctionDeclaration{}, false
		}
	}
	if cmd {
		fn.decType = commandDeclaration
	} else {
		fn.decType = functionDeclaration
	}
	fn.private = private
	fn.docString = docString
	iz.addWordsToParser(fn)
	return fn, true
}

// This wraps around chunkFunctionArguments and extracts the right name.
func (iz *Initializer) ChunkFunctionSignature() (*tokenizedFunctionDeclaration, bool) {
	position := prefix
	name := token.Token{Literal: "*dummy*"}
	if iz.P.CurTokenIs(token.IDENT) {
		name = iz.P.CurToken
		iz.P.NextToken()
	}
	if iz.P.CurTokenIs(token.COLON) || iz.P.CurTokenIs(token.PIPE) {
		position = unfix
	}
	sig, rets, ok := iz.P.ChunkFunctionArguments()
	if !ok {
		return &tokenizedFunctionDeclaration{}, false
	}
	if name.Literal == "*dummy*" { // Then it's an infix or isSuffix. It will have been processed as bling.
		for i, pair := range sig {
			if pair.Typename[0].Literal == "bling" {
				name = pair.Name
				if i == len(sig)-1 {
					position = suffix
				} else {
					position = infix
					break
				}
			}
		}
		if name.Literal == "*dummy*" { // Then we've found no bling. Disaster!
			iz.throw("sigs/name", &iz.P.CurToken)
			return &tokenizedFunctionDeclaration{}, false
		}
		if position == suffix { // Then the suffix will have been classified as bling, and we need to remove it from the sig.
			sig = sig[:len(sig)-1]
		}
	}
	return &tokenizedFunctionDeclaration{op: name, pos: position, sig: sig, rets: rets}, true
}

func (dec *tokenizedFunctionDeclaration) SigAsString() string {
	result := ""
	if dec.pos == prefix || dec.pos == unfix {
		result = dec.op.Literal
	}
	if dec.pos == prefix {
		result = result + " "
	}
	result = result + dec.sig.String()
	if dec.pos == prefix && len(dec.sig) == 0 {
		result = result + "()"
	}
	if len(dec.rets) > 0 {
		result = result + " -> " + dec.rets.String()
	}
	if dec.pos == suffix {
		result = result + " " + dec.op.Literal
	}
	return result
}

// Gives a summary of the contents of a tokenizedCode object in which the tokens in the
// body of a function/interface/constrained type are just counted.

// TODO --- we could fiddle with the api() method of the tokenized chunks to get most of this done
// byt that too.
func SummaryString(dec tokenizedCode) string {
	switch dec := dec.(type) {
	case *tokenizedAbstractDeclaration:
		result := dec.op.Literal + " = abstract "
		typeSeperator := ""
		for _, ty := range dec.types {
			result = result + typeSeperator
			tokenSeparator := ""
			for _, tok := range ty {
				result = result + tokenSeparator + tok.Literal
				tokenSeparator = " "
			}
			typeSeperator = "/"
		}
		return result
	case *tokenizedCloneDeclaration:
		result := dec.op.Literal + " = clone"
		if len(dec.params) > 0 {
			result = result + "{" + dec.params.SimpleString() + "}"
		}
		result = result + " " + dec.parentTok.Literal
		if dec.body.Length() > 0 {
			result = result + " : " + strconv.Itoa(dec.body.Length()) + " tokens."
		}
		return result
	case *tokenizedConstOrVarDeclaration:
		return dec.sig.SimpleString() + " = " + strconv.Itoa(dec.body.Length()) + " tokens."
	case *tokenizedEnumDeclaration:
		result := dec.op.Literal + " = enum "
		sep := ""
		for _, el := range dec.elements {
			result = result + sep + el.Literal
			sep = ", "
		}
		return result
	case *tokenizedExternalOrImportDeclaration:
		if (dec.name == token.Token{}) {
			return "\"" + dec.path.Literal + "\""
		}
		return dec.name.Literal + "::" + "\"" + dec.path.Literal + "\""
	case *tokenizedFunctionDeclaration:
		result := dec.SigAsString() + " : " + strconv.Itoa(dec.body.Length()) + " tokens"
		if dec.given != nil {
			result = result + "; given : " + strconv.Itoa(dec.given.Length()) + " tokens"
		}
		result = result + "."
		return result
	case *tokenizedInterfaceDeclaration:
		return dec.op.Literal + " = interface : " + strconv.Itoa(len(dec.sigs)) + " sigs."
	case *tokenizedMakeDeclarations:
		result := "make "
		typeSeperator := ""
		for _, ty := range dec.types {
			result = result + typeSeperator
			tokenSeparator := ""
			for _, tok := range ty {
				result = result + tokenSeparator + tok.Literal
				tokenSeparator = " "
			}
			typeSeperator = ", "
		}
		return result
	case *tokenizedStructDeclaration:
		result := dec.op.Literal + " = struct"
		if len(dec.params) > 0 {
			result = result + "{" + dec.params.SimpleString() + "}"
		}
		result = result + "(" + dec.sig.SimpleString() + ")"
		if dec.body.Length() > 0 {
			result = result + " : " + strconv.Itoa(dec.body.Length()) + " tokens."
		}
		return result
	default:
		panic("Unhandled case!")
	}
}

type parameterInfo struct {
	Names      []string
	Types      []values.ValueType
	Operations []token.Token
	Typecheck  *token.TokenizedCodeChunk
	ParentType string     // 'struct' if not a clone
	Sig        ast.AstSig // nil if not a struct (because the sig of a clone is implicit in the parent type).
	IsPrivate  bool
	Supertype  string
	Token      *token.Token
}
