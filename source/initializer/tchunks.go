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

// This allows us to set up the parser with information about what things are functions, what
// things are types, etc.

// Since different declarations have different contents, we slap an interface over the top.

type tokenizedCode interface {
	getDeclarationType() declarationType
	indexToken() token.Token
}

type tokenizedAbstractDeclaration struct {
	private bool             // Whether it's declared private.
	op token.Token           // The type operator.
	types [][]token.Token // The names of the constituent types.
}

func (tc *tokenizedAbstractDeclaration) getDeclarationType() declarationType {return abstractDeclaration}

func (tc *tokenizedAbstractDeclaration) indexToken() token.Token {return tc.op}

type tokenizedCloneDeclaration struct {
	private bool            // Whether it's declared private.
	op token.Token          // The type operator.
	params parser.TokSig    // The type parameters, if any.
	parentTok token.Token // The type being cloned.
	body   *token.TokenizedCodeChunk // Validation, if any.
}

func (tc *tokenizedCloneDeclaration) getDeclarationType() declarationType {return cloneDeclaration}

func (tc *tokenizedCloneDeclaration) indexToken() token.Token {return tc.op}

type tokenizedConstOrVarDeclaration struct {
	decType     declarationType // Either constantDeclaration or variableDeclaration.
	private     bool            // Whether it's declared private.
	sig         parser.TokSig   // The signature of the assignment
	assignToken token.Token     // The assignment operator '='.
	body        *token.TokenizedCodeChunk // The rhs of the assignment.
}

func (tc *tokenizedConstOrVarDeclaration) getDeclarationType() declarationType {return tc.decType}

func (tc *tokenizedConstOrVarDeclaration) indexToken() token.Token {return tc.assignToken}

type tokenizedEnumDeclaration struct {
	private  bool           // Whether it's declared private.
	op token.Token          // The type operator.
	elements []token.Token  // The elements of the enum.
}

func (tc *tokenizedEnumDeclaration) getDeclarationType() declarationType {return enumDeclaration}

func (tc *tokenizedEnumDeclaration) indexToken() token.Token {return tc.op}

type tokenizedExternalOrImportDeclaration struct {
	decType declarationType // Either importDeclaration or externalDeclaration.
	private bool            // Whether it's declared private.
	name    token.Token     // The name of the service as an identifier.
	path    token.Token     // The path as a string literal.
}

func (tc *tokenizedExternalOrImportDeclaration) getDeclarationType() declarationType {return tc.decType}

func (tc *tokenizedExternalOrImportDeclaration) indexToken() token.Token {return tc.name}

type tokenizedFunctionDeclaration struct {
	decType declarationType  // Can be commandDeclaration, functionDeclaration, or golangDeclaration.
	private bool             // Whether it's private.
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

func (tc *tokenizedFunctionDeclaration) getDeclarationType() declarationType {return tc.decType}

func (tc *tokenizedFunctionDeclaration) indexToken() token.Token {return tc.op}

type tokenizedInterfaceDeclaration struct {
	private bool                        // Whether it's declared private.
	op      token.Token                 // The type operator.
	body    *token.TokenizedCodeChunk   // The definition of the interface.
}

func (tc *tokenizedInterfaceDeclaration) getDeclarationType() declarationType {return interfaceDeclaration}

func (tc *tokenizedInterfaceDeclaration) indexToken() token.Token {return tc.op}

type tokenizedMakeDeclaration struct {
	private   bool            // Whether it's declared private.
	makeToken token.Token     // The token saying 'make'.
	elements  [][]token.Token // The names of the types to declare.
}

func (tc *tokenizedMakeDeclaration) getDeclarationType() declarationType {return makeDeclaration}

func (tc *tokenizedMakeDeclaration) indexToken() token.Token {return tc.makeToken}

type tokenizedStructDeclaration struct {
	private bool             // Whether it's declared private.
	op      token.Token      // The type operator.
	params  parser.TokSig    // The type parameters, if any.
	sig     parser.TokSig    // The signature of the struct.
}

func (tc *tokenizedStructDeclaration) getDeclarationType() declarationType {return structDeclaration}

func (tc *tokenizedStructDeclaration) indexToken() token.Token {return tc.op}

// This is a temporary function to allow us to refactor from using TCCs to tokenizedCode.
func (iz *Initializer) tccToTokenizedCode(decType declarationType, private bool, tcc *token.TokenizedCodeChunk) tokenizedCode {
	iz.P.PrimeWithTokenSupplier(tcc)
	switch decType {
	case commandDeclaration, functionDeclaration:
		result, _ := iz.ChunkFunction(decType == commandDeclaration, private)
		return result
	case enumDeclaration, cloneDeclaration, structDeclaration:
		result, _ := iz.ChunkTypeDeclaration(private)
		return result
	default:
		panic("Unhandled declaration type.")
	}
}

// As with all the chunkers, this assumes that the p.curToken is the first token of 
// the thing we're trying to slurp.
// It will end with the p.curTok being the EOF/NEWLINE terminating the declaration.
func (iz *Initializer) ChunkTypeDeclaration(private bool) (tokenizedCode, bool) {
	if !iz.P.CurTokenIs(token.IDENT) {
		iz.Throw("init/type/ident", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedEnumDeclaration{}, false
	}
	opTok := iz.P.CurToken
	iz.P.NextToken()
	if !iz.P.CurTokenIs(token.ASSIGN) {
		iz.Throw("init/type/assign", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedEnumDeclaration{}, false
	}
	// assignTok := iz.P.CurToken
	iz.P.NextToken()
	if !iz.P.CurTokenIs(token.IDENT) {
		iz.Throw("init/type/assign", &iz.P.CurToken)
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
		panic("Not implemented!")
	case "struct":
		panic("Not implemented!")
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
				braces ++
			}
			if iz.P.CurTokenIs(token.RBRACE) {
				braces --
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
	validation := token.NewCodeChunk()
	if iz.P.CurTokenIs(token.COLON) {
		validation = iz.P.SlurpBlock(false)
	}
	if !(iz.P.CurTokenIs(token.NEWLINE) || iz.P.CurTokenIs(token.EOF)) {
		iz.Throw("init/clone/expect", &iz.P.CurToken)
		iz.finishChunk()
		return &tokenizedCloneDeclaration{}, false
	}
	return &tokenizedCloneDeclaration{private, opTok, params, typeTok, validation}, true
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
	fn.body = iz.P.SlurpBlock(false)
	if fn.body.Length() > 0 && fn.body.IndexToken().Type == token.GOCODE {
		fn.decType = golangDeclaration
	} else {
		if cmd {
			fn.decType = commandDeclaration
		} else {
			fn.decType = golangDeclaration
		}
	}
	fn.private = private
	return &fn, true
}

// This wraps around chunkFunctionArguments and extracts the right name.
func (iz *Initializer) ChunkFunctionSignature() (tokenizedFunctionDeclaration, bool) {
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
		return tokenizedFunctionDeclaration{}, false
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
			return tokenizedFunctionDeclaration{}, false
		}
		if position == suffix { // Then the suffix will have been classified as bling, and we need to remove it from the sig.
			sig = sig[:len(sig)-1]
		}
	}
	return tokenizedFunctionDeclaration{op: name, pos: position, sig: sig, rets: rets}, true
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
	case *tokenizedEnumDeclaration:
		result := dec.op.Literal + " = enum "
		sep := ""
		for _, el := range dec.elements {
			result = result + sep + el.Literal
			sep = ", "
		}
		return result
	case *tokenizedFunctionDeclaration:
		return dec.SigAsString() + " : " + strconv.Itoa(dec.body.Length()) + " tokens."
	default:
		panic("Unhandled case!")
	}
}

