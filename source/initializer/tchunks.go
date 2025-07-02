package initializer

import (
	"strconv"

	"github.com/tim-hardcastle/Pipefish/source/parser"
	"github.com/tim-hardcastle/Pipefish/source/token"
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
}

func ixPtr(tc tokenizedCode) *token.Token {
	t := tc.indexToken()
	return &t
}

type tokenizedAbstractDeclaration struct {
	private bool            // Whether it's declared private.
	op      token.Token     // The type operator.
	types   [][]token.Token // The names of the constituent types.
}

func (tc *tokenizedAbstractDeclaration) getDeclarationType() declarationType {
	return abstractDeclaration
}

func (tc *tokenizedAbstractDeclaration) indexToken() token.Token { return tc.op }

type tokenizedCloneDeclaration struct {
	private   bool                      // Whether it's declared private.
	op        token.Token               // The type operator.
	params    parser.TokSig             // The type parameters, if any.
	parentTok token.Token               // The type being cloned.
	requests  []token.Token             // Types requested by the 'using' clause
	body      *token.TokenizedCodeChunk // Validation, if any.
}

func (tc *tokenizedCloneDeclaration) getDeclarationType() declarationType { return cloneDeclaration }

func (tc *tokenizedCloneDeclaration) indexToken() token.Token { return tc.op }

type tokenizedConstOrVarDeclaration struct {
	decType     declarationType           // Either constantDeclaration or variableDeclaration.
	private     bool                      // Whether it's declared private.
	sig         parser.TokSig             // The signature of the assignment
	assignToken token.Token               // The assignment operator '='.
	body        *token.TokenizedCodeChunk // The rhs of the assignment.
}

func (tc *tokenizedConstOrVarDeclaration) getDeclarationType() declarationType { return tc.decType }

func (tc *tokenizedConstOrVarDeclaration) indexToken() token.Token { return tc.assignToken }

type tokenizedEnumDeclaration struct {
	private  bool          // Whether it's declared private.
	op       token.Token   // The type operator.
	elements []token.Token // The elements of the enum.
}

func (tc *tokenizedEnumDeclaration) getDeclarationType() declarationType { return enumDeclaration }

func (tc *tokenizedEnumDeclaration) indexToken() token.Token { return tc.op }

type tokenizedExternalOrImportDeclaration struct {
	decType declarationType // Either importDeclaration or externalDeclaration.
	private bool            // Whether it's declared private.
	golang  bool            // If we're importing a Go library.
	name    token.Token     // The name of the service as an identifier.
	path    token.Token     // The path as a string literal.
}

func (tc *tokenizedExternalOrImportDeclaration) getDeclarationType() declarationType {
	return tc.decType
}

func (tc *tokenizedExternalOrImportDeclaration) indexToken() token.Token { return tc.name }

type tokenizedFunctionDeclaration struct {
	decType declarationType           // Can be commandDeclaration, functionDeclaration.
	private bool                      // Whether it's private.
	op      token.Token               // The name of the fumction/operation.
	pos     opPosition                // Whether it's a prefix, infix, suffix, or unfix.
	sig     parser.TokSig             // The call signature, with the names of arguments as tokens and the types as lists of tokens.
	rets    parser.TokReturns         // The return types, as lists of tokens.
	body    *token.TokenizedCodeChunk // The body of the function, including the 'given' block, if any.
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

type tokenizedInterfaceDeclaration struct {
	private bool                            // Whether it's declared private.
	op      token.Token                     // The type operator.
	sigs    []*tokenizedFunctionDeclaration // The signatures defining the interface.
}

func (tc *tokenizedInterfaceDeclaration) getDeclarationType() declarationType {
	return interfaceDeclaration
}

func (tc *tokenizedInterfaceDeclaration) indexToken() token.Token { return tc.op }

type tokenizedGolangDeclaration struct {
	private bool        // Whether it's declared private.
	goCode  token.Token // The code has already been squished into a single Pipefish token.
}

func (tc *tokenizedGolangDeclaration) getDeclarationType() declarationType { return golangDeclaration }

func (tc *tokenizedGolangDeclaration) indexToken() token.Token { return tc.goCode }

type tokenizedMakeDeclaration struct {
	private   bool            // Whether it's declared private.
	makeToken token.Token     // The token saying 'make'.
	types     [][]token.Token // The names of the types to declare.
}

func (tc *tokenizedMakeDeclaration) getDeclarationType() declarationType { return makeDeclaration }

func (tc *tokenizedMakeDeclaration) indexToken() token.Token { return tc.makeToken }

type tokenizedStructDeclaration struct {
	private bool                      // Whether it's declared private.
	op      token.Token               // The type operator.
	params  parser.TokSig             // The type parameters, if any.
	sig     parser.TokSig             // The signature of the struct.
	body    *token.TokenizedCodeChunk // Validation logic, if any.
}

func (tc *tokenizedStructDeclaration) getDeclarationType() declarationType { return structDeclaration }

func (tc *tokenizedStructDeclaration) indexToken() token.Token { return tc.op }

// This is a temporary function to allow us to refactor from using TCCs to tokenizedCode.
func (iz *Initializer) TranslateEverything() {
	for decType := importDeclaration; decType <= makeDeclaration; decType++ {
		for _, dec := range iz.TokenizedDeclarations[decType] {
			iz.tokenizedCode[decType] = append(iz.tokenizedCode[decType], iz.tccToTokenizedCode(decType, dec.Private, dec))
			dec.ToStart()
		}
	}
}

func (iz *Initializer) tccToTokenizedCode(decType declarationType, private bool, tcc *token.TokenizedCodeChunk) tokenizedCode {
	iz.P.PrimeWithTokenSupplier(tcc)
	switch decType {
	case commandDeclaration, functionDeclaration:
		result, _ := iz.ChunkFunction(decType == commandDeclaration, private)
		return result
	case variableDeclaration, constantDeclaration:
		result, _ := iz.ChunkConstOrVarDeclaration(decType == constantDeclaration, private)
		return result
	case importDeclaration, externalDeclaration:
		result, _ := iz.ChunkImportOrExternalDeclaration(decType == externalDeclaration, private)
		return result
	case abstractDeclaration, cloneDeclaration, enumDeclaration, interfaceDeclaration,
		makeDeclaration, structDeclaration:
		result, _ := iz.ChunkTypeDeclaration(private)
		return result
	case golangDeclaration:
		result, _ := iz.ChunkGolangDeclaration(private)
		return result
	default:
		panic("Unhandled declaration type.")
	}
}

// As with all the chunkers, this assumes that the p.curToken is the first token of
// the thing we're trying to slurp.
// It will end with the p.curTok being the EOF/NEWLINE terminating the declaration.
func (iz *Initializer) ChunkConstOrVarDeclaration(isConst, private bool) (tokenizedCode, bool) {
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
		iz.Throw("init/assign", &iz.P.CurToken)
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
	return &tokenizedConstOrVarDeclaration{decType, private, sig, assignTok, token.MakeCodeChunk(toks, private)}, true
}

func (iz *Initializer) ChunkGolangDeclaration(private bool) (tokenizedCode, bool) {
	result := tokenizedGolangDeclaration{private, iz.P.CurToken}
	iz.P.NextToken()
	if !(iz.P.CurTokenIs(token.NEWLINE) || iz.P.CurTokenIs(token.EOF)) {
		iz.Throw("init/golang", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedGolangDeclaration{}, false
	}
	return &result, true
}

// As with all the chunkers, this assumes that the p.curToken is the first token of
// the thing we're trying to slurp.
// It will end with the p.curTok being the EOF/NEWLINE terminating the declaration.
func (iz *Initializer) ChunkImportOrExternalDeclaration(isExternal, private bool) (tokenizedCode, bool) {
	decType := importDeclaration
	if isExternal {
		decType = externalDeclaration
	}
	var name, path token.Token
	golang := false
	switch iz.P.CurToken.Type {
	case token.GOCODE: // TODO --- the lexer shoves everything into the gocode token and it should only do that if followed by '{'.
		path = iz.P.CurToken
		golang = true
	case token.IDENT:
		name = iz.P.CurToken
		iz.P.NextToken()
		if !(iz.P.CurTokenIs(token.IDENT) && iz.P.CurToken.Literal == "::") {
			iz.Throw("init/impex/pair", &iz.P.CurToken)
			iz.finishChunk()
			return &tokenizedExternalOrImportDeclaration{}, false
		}
		iz.P.NextToken()
		if !iz.P.CurTokenIs(token.STRING) {
			iz.Throw("init/impex/string", &iz.P.CurToken)
			iz.finishChunk()
			return &tokenizedExternalOrImportDeclaration{}, false
		}
		path = iz.P.CurToken
	case token.STRING:
		path = iz.P.CurToken
	default:
		iz.Throw("init/impex/expect", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedExternalOrImportDeclaration{}, false
	}
	iz.P.NextToken()
	if !(iz.P.CurTokenIs(token.NEWLINE) || iz.P.CurTokenIs(token.EOF)) {
		iz.Throw("init/impex/end", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedExternalOrImportDeclaration{}, false
	}
	return &tokenizedExternalOrImportDeclaration{decType, private, golang, name, path}, true
}

// As with all the chunkers, this assumes that the p.curToken is the first token of
// the thing we're trying to slurp.
// It will end with the p.curTok being the EOF/NEWLINE terminating the declaration.
func (iz *Initializer) ChunkTypeDeclaration(private bool) (tokenizedCode, bool) {
	if !iz.P.CurTokenIs(token.IDENT) {
		iz.Throw("init/type/ident", &iz.P.CurToken)
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
		iz.Throw("init/type/assign", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedEnumDeclaration{}, false
	}
	iz.P.NextToken()
	if !iz.P.CurTokenIs(token.IDENT) {
		iz.Throw("init/type/define", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedEnumDeclaration{}, false
	}
	decliteral := iz.P.CurToken.Literal
	iz.P.NextToken()
	switch decliteral {
	case "abstract":
		return iz.chunkAbstract(opTok, private)
	case "clone":
		return iz.chunkClone(opTok, private)
	case "enum":
		return iz.chunkEnum(opTok, private)
	case "interface":
		return iz.chunkInterface(opTok, private)
	case "struct":
		return iz.chunkStruct(opTok, private)
	default:
		iz.Throw("init/type/expect", &iz.P.CurToken, decliteral)
		iz.finishChunk()
		return &tokenizedEnumDeclaration{}, false
	}
}

// Starts after the word 'abstract', ends on NEWLINE or EOF.
func (iz *Initializer) chunkAbstract(opTok token.Token, private bool) (tokenizedCode, bool) {
	types := [][]token.Token{}
	for {
		if !iz.P.CurTokenIs(token.IDENT) {
			iz.Throw("init/abstract/ident", &iz.P.CurToken)
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
	return &tokenizedAbstractDeclaration{private, opTok, types}, true
}

// Starts after the word 'clone', ends on NEWLINE or EOF.
func (iz *Initializer) chunkClone(opTok token.Token, private bool) (tokenizedCode, bool) {
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
			iz.Throw("init/clone/params", &iz.P.CurToken)
			iz.finishChunk()
			return &tokenizedCloneDeclaration{}, false
		}
		if !iz.P.PeekTokenIs(token.RBRACE) {
			iz.Throw("init/clone/rbrace", &iz.P.CurToken)
			iz.finishChunk()
			return &tokenizedCloneDeclaration{}, false
		}
		for _, pair := range params {
			if pair.Typename[0].Literal == "*error*" {
				iz.Throw("init/struct/ptype", &pair.Name)
				iz.finishChunk()
				return &tokenizedCloneDeclaration{}, false
			}
		}
		iz.P.NextToken()
		iz.P.NextToken()
	}
	if !iz.P.CurTokenIs(token.IDENT) {
		iz.Throw("init/clone/ident", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedCloneDeclaration{}, false
	}
	typeTok := iz.P.CurToken
	_, ok := parser.ClonableTypes[typeTok.Literal]
	if !ok {
		iz.Throw("init/clone/type", &iz.P.CurToken)
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
				println(iz.P.CurToken.Type, iz.P.CurToken.Literal)
				iz.Throw("init/clone/ident", &iz.P.CurToken)
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
				iz.Throw("init/clone/expect/a", &iz.P.CurToken)
				iz.finishChunk()
				return &tokenizedCloneDeclaration{}, false
			}
		}
	}
	validation := token.NewCodeChunk()
	if iz.P.CurTokenIs(token.COLON) {
		validation, ok = iz.P.SlurpBlock(false)
	}
	if !ok {
		return &tokenizedCloneDeclaration{}, false
	}
	if !(iz.P.CurTokenIs(token.NEWLINE) || iz.P.CurTokenIs(token.EOF)) {
		iz.Throw("init/clone/expect/b", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedCloneDeclaration{}, false
	}
	return &tokenizedCloneDeclaration{private, opTok, params, typeTok, requests, validation}, true
}

// Starts after the word 'enum', ends on NEWLINE or EOF.
func (iz *Initializer) chunkEnum(opTok token.Token, private bool) (tokenizedCode, bool) {
	toks := []token.Token{}
	for {
		if iz.P.CurTokenIs(token.IDENT) {
			toks = append(toks, iz.P.CurToken)
		} else {
			iz.Throw("init/enum/ident", &iz.P.CurToken)
			iz.finishChunk()
			return &tokenizedEnumDeclaration{}, false
		}
		iz.P.NextToken()
		if iz.P.CurTokenIs(token.NEWLINE) || iz.P.CurTokenIs(token.EOF) {
			if len(toks) == 0 {
				iz.Throw("init/enum/empty", &iz.P.CurToken)
				iz.finishChunk()
				return &tokenizedEnumDeclaration{}, false
			} else {
				return &tokenizedEnumDeclaration{private: private, op: opTok, elements: toks}, true
			}
		}
		if iz.P.CurTokenIs(token.COMMA) {
			iz.P.NextToken()
			continue
		}
		iz.Throw("init/enum/expect", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedEnumDeclaration{}, false
	}
}

// Starts after the word 'interface', ends on NEWLINE or EOF.
func (iz *Initializer) chunkInterface(opTok token.Token, private bool) (tokenizedCode, bool) {
	if !iz.P.CurTokenIs(token.COLON) {
		iz.Throw("init/interface/colon", &iz.P.CurToken)
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
	return &tokenizedInterfaceDeclaration{private, opTok, sigs}, true
}

// Starts after the word 'make', ends on NEWLINE or EOF.
func (iz *Initializer) chunkMake(opTok token.Token, private bool) (tokenizedCode, bool) {
	types := [][]token.Token{}
	for {
		if !iz.P.CurTokenIs(token.IDENT) {
			iz.Throw("init/make/ident", &iz.P.CurToken)
			iz.finishChunk()
			return &tokenizedMakeDeclaration{}, false
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
	return &tokenizedMakeDeclaration{private, opTok, types}, true
}

// Starts after the word 'struct', ends on NEWLINE or EOF.
func (iz *Initializer) chunkStruct(opTok token.Token, private bool) (tokenizedCode, bool) {
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
			iz.Throw("init/struct/params", &iz.P.CurToken)
			iz.finishChunk()
			return &tokenizedStructDeclaration{}, false
		}
		if !iz.P.PeekTokenIs(token.RBRACE) {
			iz.Throw("init/struct/rbrace", &iz.P.CurToken)
			iz.finishChunk()
			return &tokenizedStructDeclaration{}, false
		}
		for _, pair := range params {
			if pair.Typename[0].Literal == "*error*" {
				iz.Throw("init/struct/ptype", &pair.Name)
				iz.finishChunk()
				return &tokenizedStructDeclaration{}, false
			}
		}
		iz.P.NextToken()
		iz.P.NextToken()
	}
	if !iz.P.CurTokenIs(token.LPAREN) {
		iz.Throw("init/struct/lparen", &iz.P.CurToken)
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
		iz.Throw("init/struct/rparen", &iz.P.CurToken)
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
	if !(iz.P.CurTokenIs(token.NEWLINE) || iz.P.CurTokenIs(token.EOF)) {
		iz.Throw("init/struct/expect", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedStructDeclaration{}, false
	}
	return &tokenizedStructDeclaration{private, opTok, params, args, validation}, true
}

// This is called in case of a malformed definiion to try to skip over the rest of it
// and get on to the next definition.
// As it emulates slurping up a definition, it ends when the p.curToken is EOF or NEWLINE.
func (iz *Initializer) finishChunk() {
	iz.P.ResetNesting()
	for {
		if iz.P.CurTokenIs(token.EOF) || iz.P.CurTokenIs(token.NEWLINE) {
			return
		}
		if iz.P.CurTokenIs(token.COLON) {
			iz.P.SlurpBlock(true)
			return
		}
		iz.P.SafeNextToken()
	}
}

// As with all the chunkers, this assumes that the p.curToken is the first token of
// the thing we're trying to slurp.
// It will end with the p.curTok being the EOF/NEWLINE terminating the declaration.
func (iz *Initializer) ChunkFunction(cmd, private bool) (*tokenizedFunctionDeclaration, bool) {
	fn, ok := iz.ChunkFunctionSignature()
	if !ok {
		return &tokenizedFunctionDeclaration{}, false
	}
	fn.body, ok = iz.P.SlurpBlock(false)
	if !ok {
		return &tokenizedFunctionDeclaration{}, false
	}
	if cmd {
		fn.decType = commandDeclaration
	} else {
		fn.decType = functionDeclaration
	}
	fn.private = private
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
				}
			}
		}
		if name.Literal == "*dummy*" { // Then we've found no bling. Disaster!
			iz.Throw("sigs/name", &iz.P.CurToken)
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
		return dec.SigAsString() + " : " + strconv.Itoa(dec.body.Length()) + " tokens."
	case *tokenizedInterfaceDeclaration:
		return dec.op.Literal + " = interface : " + strconv.Itoa(len(dec.sigs)) + " sigs."
	case *tokenizedMakeDeclaration:
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
