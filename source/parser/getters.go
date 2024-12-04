package parser

import (
	"reflect"
	"strconv"

	"pipefish/source/ast"
	"pipefish/source/dtypes"
	"pipefish/source/err"
	"pipefish/source/token"
	"pipefish/source/values"
)

// Auxiliary functions that extract data from data.

// Slurps the parts of a function out of it. As the colon after a function definition has
// extremely low precedence, we should find it at the root of the tree.
// We extract the function name first and then hand its branch or branches off to a recursive tree-slurper.
func (prsr *Parser) ExtractPartsOfFunction(fn ast.Node) (string, uint32, ast.StringSig, ast.StringSig, ast.Node, ast.Node) {
	var (
		functionName          string
		sig                   ast.StringSig
		rTypes                ast.StringSig
		start, content, given ast.Node
	)
	if fn.GetToken().Type == token.GIVEN {
		given = fn.(*ast.InfixExpression).Args[2]
		fn = fn.(*ast.InfixExpression).Args[0]
	}

	switch fn := fn.(type) {
	case *ast.LazyInfixExpression:
		if !(fn.Token.Type == token.COLON) {
			prsr.Throw("parse/sig/malformed/a", fn.GetToken())
			return functionName, 0, sig, rTypes, content, given
		}
		start = fn.Left
		content = fn.Right
	case *ast.InfixExpression:
		if fn.Token.Type != token.MAGIC_COLON {
			prsr.Throw("parse/sig/malformed/b", fn.GetToken())
			return functionName, 0, sig, rTypes, content, given
		}
		start = fn.Args[0]
		content = fn.Args[2]
	default:
		prsr.Throw("parse/sig/malformed/c", fn.GetToken())
		return functionName, 0, sig, rTypes, content, given
	}

	if start.GetToken().Type == token.PIPE {
		rTypes = prsr.RecursivelySlurpReturnTypes(start.(*ast.PipingExpression).Right)
		start = start.(*ast.PipingExpression).Left
	}
	functionName, pos, sig := prsr.GetPartsOfSig(start)
	return functionName, pos, sig, rTypes, content, given
}

func (prsr *Parser) GetPartsOfSig(start ast.Node) (functionName string, pos uint32, sig ast.StringSig) {
	switch start := start.(type) {
	case *ast.PrefixExpression:
		functionName = start.Operator
		pos = 0
		sig = prsr.extractSig(start.Args)
	case *ast.InfixExpression:
		functionName = start.Operator
		pos = 1
		sig = prsr.extractSig(start.Args)
	case *ast.SuffixExpression:
		functionName = start.Operator
		pos = 2
		sig = prsr.extractSig(start.Args)
	case *ast.UnfixExpression:
		functionName = start.Operator
		pos = 3
		sig = ast.StringSig{}
	default:
		prsr.Throw("parse/sig/malformed/d", start.GetToken())
		return functionName, pos, sig
	}
	return functionName, pos, sig
}

func (p *Parser) extractSig(args []ast.Node) ast.StringSig {
	sig := ast.StringSig{}
	if len(args) == 0 || (len(args) == 1 && reflect.TypeOf(args[0]) == reflect.TypeOf(&ast.Nothing{})) {
		return sig
	}
	backTrackTo := 0
	for j, arg := range args {
		varName := ""
		varType := "*"
		switch arg := arg.(type) {
		case *ast.SuffixExpression:
			if arg.Operator == "raw" { // TODO --- same for 'ref'?
				switch inner := arg.Args[0].(type) {
				case *ast.SuffixExpression:
					if !p.TypeExists(inner.Operator) {
						p.Throw("parse/suffix/type", inner.GetToken())
						return nil
					}
					switch inmost := inner.Args[0].(type) {
					case *ast.Identifier:
						varName = inmost.Value
						varType = inner.Operator + " " + arg.Operator
					default:
						p.Throw("parse/suffix/ident", inmost.GetToken())
						return nil
					}
				case *ast.Identifier:
					varName = inner.Value
					varType = "any? " + arg.Operator
				default:
					p.Throw("parse/sig/suffix/a", arg.GetToken())
					return nil
				}
			} else { // The suffix is not 'raw'.
				if !p.TypeExists(arg.Operator) {
					p.Throw("parse/sig/type/a", &arg.Token)
					return nil
				}
				switch inner := arg.Args[0].(type) {
				case *ast.Identifier:
					varName = inner.Value
					varType = arg.Operator
				case *ast.SuffixExpression:
					if inner.Operator != "..." {
						p.Throw("parse/sig/suffix/b", inner.GetToken())
						return nil
					}
					switch innerer := inner.Args[0].(type) {
					case *ast.Identifier:
						varName = innerer.Value
						varType = "..." + arg.Operator
					default:
						p.Throw("parse/sig/ident/d", innerer.GetToken())
					}
				default:
					p.Throw("parse/sig/ident/a", inner.GetToken())
					return nil
				}
			}
		case *ast.Identifier:
			if p.Endfixes.Contains(arg.Value) {
				varName = arg.Value
				varType = "bling"
			} else {
				varName = arg.Value
				varType = "*"
			}
		case *ast.PrefixExpression:
			if p.Forefixes.Contains(arg.Operator) {
				varName = arg.Operator
				varType = "bling"
			} else {
				// We may be declaring a parameter which will have the same name as a function --- e.g. 'f'.
				// The parser will have parsed this as a prefix expression if it was followed by a type, e.g.
				// 'foo (f func) : <function body>'. We ought therefore to be interpreting it as a parameter
				// name under those circumstances. This tends to make the whole thing stupid, we should have
				// done all this before it got near the Pratt parser.
				switch inner := arg.Args[0].(type) {
				case *ast.Identifier:
					varName = arg.Operator
					varType = inner.Value
					if !(p.TypeExists(inner.Value) ||
						arg.Operator == "ast" || arg.Operator == "ident") {
						p.Throw("parse/sig/type/b", arg.GetToken())
						return nil
					}
				default:
					p.Throw("parse/sig/ident/b", inner.GetToken())
					return nil
				}
			}
		case *ast.InfixExpression:
			if arg.Operator == "varchar" {
				switch potentialVariable := arg.Args[0].(type) {
				case *ast.Identifier:
					varName = potentialVariable.Value
				default:
					p.Throw("parse/sig/ident/c", potentialVariable.GetToken())
					return nil
				}
				switch potentialInteger := arg.Args[2].(type) {
				case *ast.IntegerLiteral:
					varType = "varchar(" + strconv.Itoa(potentialInteger.Value) + ")"
				default:
					p.Throw("parse/sig/varchar/int/a", potentialInteger.GetToken())
					return nil
				}
			} else {
				if p.Midfixes.Contains(arg.Operator) {
					varName = arg.Operator
					varType = "bling"
				} else {
					p.Throw("parse/sig/infix", arg.GetToken())
					return nil
				}
			}
		case *ast.Bling:
			varName = arg.Value
			varType = "bling"
		}
		if j == len(args)-1 && varType == "*" {
			for i := backTrackTo; i < len(sig); i++ {
				sig[i].VarType = "any?"
			}
			varType = "any?"
		}
		if !(varType == "bling" || varType == "*") {
			for i := backTrackTo; i < len(sig); i++ {
				sig[i].VarType = varType
			}

		}
		if varType == "bling" {
			if len(sig) > 0 && sig[len(sig)-1].VarType == "*" {
				for i := backTrackTo; i < len(sig); i++ {
					sig[i].VarType = "any?"
				}
			}
		}
		sig = append(sig, ast.NameTypenamePair{VarName: varName, VarType: varType})
		if !(varType == "*") {
			backTrackTo = len(sig)
		}
		if varType == "bling" {
			varType = "*"
		}
	}
	return sig
}

// TODO --- this function is a refactoring patch over RecursivelySlurpSignature and they could probably be more sensibly combined in a any function.
func (p *Parser) getSigFromArgs(args []ast.Node, dflt string) (ast.StringSig, *err.Error) {
	sig := ast.StringSig{}
	for _, arg := range args {
		if arg.GetToken().Type == token.IDENT && p.Bling.Contains(arg.GetToken().Literal) {
			sig = append(sig, ast.NameTypenamePair{VarName: arg.GetToken().Literal, VarType: "bling"})
		} else {
			partialSig, err := p.RecursivelySlurpSignature(arg, dflt)
			if err != nil {
				return nil, err
			}
			sig = append(sig, partialSig...)
		}
	}
	return sig, nil
}

func (p *Parser) GetVariablesFromSig(node ast.Node) []string {
	result := []string{}
	sig, e := p.RecursivelySlurpSignature(node, "")
	if e != nil {
		return result
	}
	for _, pair := range sig {
		result = append(result, pair.VarName)
	}
	return result
}

// TODO --- is there any sensible alternative to this?
// This is all rather horrible and basically exists as a result of two reasons. First, since all the signatures whether of assignment
// or function definition or struct definition or whatever fit into the same mold, we would like to be able to keep our code DRY by
// extracting them all in the same way. However, as we don't have anything like a `let` command, the parser doesn't know that it's parsing an
// assignment until it reaches the equals sign, by which time it's already turned the relevant tokens into an AST. Rather than kludge
// my way out of that, I kludged my way around it by writing this thing which extracts the signature from an AST, and which has grown steadily
// more complex with the language.
func (p *Parser) RecursivelySlurpSignature(node ast.Node, dflt string) (ast.StringSig, *err.Error) {
	switch typednode := node.(type) {
	case *ast.InfixExpression:
		switch {
		case p.Midfixes.Contains(typednode.Operator):
			LHS, err := p.RecursivelySlurpSignature(typednode.Args[0], dflt)
			if err != nil {
				return nil, err
			}
			RHS, err := p.RecursivelySlurpSignature(typednode.Args[2], dflt)
			if err != nil {
				return nil, err
			}
			middle := ast.NameTypenamePair{VarName: typednode.Operator, VarType: "bling"}
			return append(append(LHS, middle), RHS...), nil
		case typednode.Token.Type == token.COMMA:
			RHS, err := p.RecursivelySlurpSignature(typednode.Args[2], dflt)
			if err != nil {
				return nil, err
			}
			LHS, err := p.RecursivelySlurpSignature(typednode.Args[0], RHS.GetVarType(0).(string))
			if err != nil {
				return nil, err
			}
			return append(LHS, RHS...), nil
		case typednode.Token.Type == token.WEAK_COMMA:
			RHS, err := p.RecursivelySlurpSignature(typednode.Args[2], dflt)
			if err != nil {
				return nil, err
			}
			LHS, err := p.RecursivelySlurpSignature(typednode.Args[0], dflt)
			if err != nil {
				return nil, err
			}
			return append(LHS, RHS...), nil
		case typednode.Operator == "varchar":
			switch potentialInteger := typednode.Args[2].(type) {
			case *ast.IntegerLiteral:
				varType := "varchar(" + strconv.Itoa(potentialInteger.Value) + ")"
				return p.RecursivelySlurpSignature(typednode.Args[0], varType)
			default:
				return nil, newError("parse/sig/varchar/int/b", potentialInteger.GetToken())
			}
		case typednode.Operator == ".":
			namespacedIdent, err := recursivelySlurpNamespace(typednode)
			if err != nil {
				return nil, err
			}
			return ast.StringSig{ast.NameTypenamePair{VarName: namespacedIdent, VarType: dflt}}, nil
		default:
			return nil, newError("parse/sig/b", typednode.GetToken())
		}
	case *ast.SuffixExpression:
		switch {
		case p.TypeExists(typednode.Operator):
			LHS, err := p.getSigFromArgs(typednode.Args, typednode.Operator)
			if err != nil {
				return nil, err
			}
			for k := range LHS {
				LHS[k].VarType = typednode.Operator
			}
			return LHS, nil
		case typednode.Operator == "raw":
			LHS, err := p.getSigFromArgs(typednode.Args, dflt)
			if err != nil {
				return nil, err
			}
			for k := range LHS {
				LHS[k].VarType = LHS[k].VarType + " raw"
			}
			return LHS, nil
		case p.Endfixes.Contains(typednode.Operator):
			LHS, err := p.getSigFromArgs(typednode.Args, dflt)
			if err != nil {
				return nil, err
			}
			end := ast.NameTypenamePair{VarName: typednode.Operator, VarType: "bling"}
			return append(LHS, end), nil
		default:
			return nil, newError("parse/sig/c", typednode.GetToken())
		}
	case *ast.Identifier:
		if p.Endfixes.Contains(typednode.Value) {
			return ast.StringSig{ast.NameTypenamePair{VarName: typednode.Value, VarType: "bling"}}, nil
		}
		return ast.StringSig{ast.NameTypenamePair{VarName: typednode.Value, VarType: dflt}}, nil
	case *ast.PrefixExpression:
		if p.Forefixes.Contains(typednode.Operator) {
			RHS, err := p.getSigFromArgs(typednode.Args, dflt)
			if err != nil {
				return nil, err
			}
			front := ast.StringSig{ast.NameTypenamePair{VarName: typednode.Operator, VarType: "bling"}}
			return append(front, RHS...), nil
		} else {
			// We may well be declaring a parameter which will have the same name as a function --- e.g. 'f'.
			// The parser will have parsed this as a prefix expression if it was followed by a type, e.g.
			// 'foo (f func) : <function body>'. We ought therefore to be interpreting it as a parameter
			// name under those circumstances.
			return ast.StringSig{ast.NameTypenamePair{VarName: typednode.Operator, VarType: dflt}}, nil
		}
	}
	return nil, newError("parse/sig/a", node.GetToken())
}

func recursivelySlurpNamespace(root *ast.InfixExpression) (string, *err.Error) {
	if len(root.Args) != 3 {
		return "", newError("parse/sig.namespace/a", root.Args[1].GetToken())
	}
	if root.Operator != "." {
		return "", newError("parse/sig.namespace/b", root.Args[1].GetToken())
	}
	LHS := ""
	RHS := ""
	var err *err.Error
	switch leftNode := root.Args[0].(type) {
	case *ast.Identifier:
		LHS = leftNode.Value
	case *ast.InfixExpression:
		LHS, err = recursivelySlurpNamespace(leftNode)
		if err != nil {
			return "", err
		}
	default:
		return "", newError("parse/sig.namespace/c", root.Args[1].GetToken())
	}
	switch rightNode := root.Args[2].(type) {
	case *ast.Identifier:
		RHS = rightNode.Value
	case *ast.InfixExpression:
		RHS, err = recursivelySlurpNamespace(rightNode)
		if err != nil {
			return "", err
		}
	default:
		return "", newError("parse/sig.namespace/d", root.Args[1].GetToken())
	}
	return LHS + "." + RHS, nil
}

func (p *Parser) RecursivelySlurpReturnTypes(node ast.Node) ast.StringSig {
	switch typednode := node.(type) {
	case *ast.InfixExpression:
		switch {
		case typednode.Token.Type == token.COMMA:
			LHS := p.RecursivelySlurpReturnTypes(typednode.Args[0])
			RHS := p.RecursivelySlurpReturnTypes(typednode.Args[2])
			return append(LHS, RHS...)
		default:
			p.Throw("parse/ret/a", typednode.GetToken())
		}
	case *ast.TypeLiteral:
		return ast.StringSig{ast.NameTypenamePair{VarName: "", VarType: typednode.Value}}
	default:
		p.Throw("parse/ret/b", typednode.GetToken())
	}
	return nil
}

// Gets the variable from the lhs and rhs of an assignment when it's still in the form of tokens.
func (p *Parser) ExtractVariables(T TokenSupplier) (dtypes.Set[string], dtypes.Set[string]) {
	LHS := make(dtypes.Set[string])
	RHS := make(dtypes.Set[string])
	assignHasHappened := false
	for tok := T.NextToken(); tok.Type != token.EOF; tok = T.NextToken() {
		if tok.Type == token.IDENT &&
			!p.AllFunctionIdents.Contains(tok.Literal) &&
			!p.TypeExists(tok.Literal) {
			if assignHasHappened {
				RHS.Add(tok.Literal)
			} else {
				LHS.Add(tok.Literal)
			}
		}
		if tok.Type == token.ASSIGN {
			assignHasHappened = true
		}

	}
	return LHS, RHS
}

func (p *Parser) IsPrivate(x, y int) bool {
	return p.TokenizedDeclarations[x][y].Private
}

func (p *Parser) MakeAbstractSigFromStringSig(sig ast.StringSig) ast.AbstractSig {
	result := make(ast.AbstractSig, sig.Len())
	for i, pair := range sig {
		typename := pair.VarType
		if len(typename) >= 3 && typename[:3] == "..." {
			typename = typename[3:]
		}
		if len(typename) >= 4 && typename[len(typename)-4:] == " raw" {
			typename = typename[:len(typename)-4]
		}
		result[i] = ast.NameAbstractTypePair{pair.VarName, p.GetAbstractType(typename)}
	}
	return result
}

func (p *Parser) TypeExists(name string) bool {
	_, ok := p.SafeGetAbstractType(name)
	return ok
}

func (p *Parser) GetAbstractType(name string) values.AbstractType {
	abType, _ := p.SafeGetAbstractType(name)
	return abType
}

func (p *Parser) SafeGetAbstractType(name string) (values.AbstractType, bool) {
	if varCharValue, ok := GetLengthFromType(name); ok { // We either special-case the varchars ...
		result := values.MakeAbstractType(values.STRING)
		if GetNullabilityFromType(name) {
			result = result.Insert(values.NULL)
		}
		result.Varchar = uint32(varCharValue)
		return result, true
	}
	// Check if it's a shared abstract type: 'int', 'struct', 'listlike', 'any?' etc.
	if result, ok := p.Common.Types[name]; ok {
		return result, true
	}
	// ... or the result should just be in the parser's own type map.
	result, ok := p.TypeMap[name]
	return result, ok
}

// Finds whether an identifier is in the right place to be a function, or whether it's being used
// as though it's a variable or constant.
func (p *Parser) isPositionallyFunctional() bool {
	if assignmentTokens.Contains(p.peekToken.Type) {
		return false
	}
	if p.peekToken.Type == token.RPAREN || p.peekToken.Type == token.PIPE ||
		p.peekToken.Type == token.MAPPING || p.peekToken.Type == token.FILTER ||
		p.peekToken.Type == token.COLON || p.peekToken.Type == token.MAGIC_COLON ||
		p.peekToken.Type == token.COMMA {
		return false
	}
	if p.curToken.Literal == "type" && p.TypeExists(p.peekToken.Literal) {
		return true
	}
	if p.Functions.Contains(p.curToken.Literal) && p.TypeExists(p.curToken.Literal) {
		if p.peekToken.Type == token.EMDASH {
			return false
		}
		if literalsAndLParen.Contains(p.peekToken.Type) {
			return true
		}
		if p.peekToken.Literal == "from" {
			return true
		}
		if p.Infixes.Contains(p.peekToken.Literal) {
			return false
		}
		if p.nativeInfixes.Contains(p.peekToken.Type) {
			return false
		}
		if p.Midfixes.Contains(p.peekToken.Literal) {
			return false
		}
		if p.Functions.Contains(p.peekToken.Literal) && p.peekToken.Type != token.EOF {
			return true
		}
		if p.Prefixes.Contains(p.peekToken.Literal) {
			return p.peekToken.Type != token.EOF
		}
	}

	if p.Functions.Contains(p.curToken.Literal) && p.peekToken.Type != token.EOF {
		return true
	}
	if p.Prefixes.Contains(p.curToken.Literal) {
		return p.peekToken.Type != token.EOF
	}
	if literalsAndLParen.Contains(p.peekToken.Type) {
		return true
	}
	if p.peekToken.Type != token.IDENT {
		return false
	}
	if p.Infixes.Contains(p.peekToken.Literal) {
		return false
	}
	if p.Midfixes.Contains(p.peekToken.Literal) {
		return false
	}
	if p.Endfixes.Contains(p.peekToken.Literal) {
		return false
	}
	if p.Suffixes.Contains(p.peekToken.Literal) {
		return false
	}
	return true
}