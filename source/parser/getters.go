package parser

import (
	"reflect"

	"github.com/tim-hardcastle/Pipefish/source/ast"
	"github.com/tim-hardcastle/Pipefish/source/err"
	"github.com/tim-hardcastle/Pipefish/source/token"
	"github.com/tim-hardcastle/Pipefish/source/values"
)

// Auxiliary functions that extract data from data.

// TODO --- at this point this is used only once to do something which isn't actually this hard.
func (p *Parser) extractSig(args []ast.Node) ast.AstSig {
	sig := ast.AstSig{}
	if len(args) == 0 || (len(args) == 1 && reflect.TypeOf(args[0]) == reflect.TypeOf(&ast.Nothing{})) {
		return sig
	}
	backTrackTo := 0
	for j, arg := range args {
		varName := ""
		var varType ast.TypeNode
		switch arg := arg.(type) {
		case *ast.TypeSuffixExpression:
			switch inner := arg.Args[0].(type) {
			case *ast.Identifier:
				varName = inner.Value
				varType = arg.Operator
			default:
				p.Throw("parse/sig/ident/a", inner.GetToken())
				return nil
			}
		case *ast.Identifier:
			varName = arg.Value
			varType = nil
		case *ast.PrefixExpression:
			switch inner := arg.Args[0].(type) {
			case *ast.TypeExpression:
				varName = arg.Operator
				astType := p.ToAstType(inner)
				if astType == nil {
					p.Throw("parse/sig/ident/c", inner.GetToken())
				}
				varType = astType
			default:
				p.Throw("parse/sig/ident/c", inner.GetToken())
				return nil
			}
			sig = append(sig, ast.NameTypeAstPair{VarName: varName, VarType: varType})
			if len(arg.Args) > 1 {
				kludge := p.extractSig(arg.Args[1:])
				sig = append(sig, kludge...)
			}
			if sig[len(sig)-1].VarType != nil {
				backTrackTo = len(sig)
			}
			continue
		}
		if j == len(args)-1 && varType == nil {
			for i := backTrackTo; i < len(sig); i++ {
				sig[i].VarType = ast.ANY_NULLABLE_TYPE_AST
			}
			varType = ast.ANY_NULLABLE_TYPE_AST
		}
		sig = append(sig, ast.NameTypeAstPair{VarName: varName, VarType: varType})
		if sig[len(sig)-1].VarType != nil {
			backTrackTo = len(sig)
		}
	}
	return sig
}

// TODO --- this function is a refactoring patch over RecursivelySlurpSignature and they could probably be more sensibly combined in a any function.
func (p *Parser) getSigFromArgs(args []ast.Node, dflt ast.TypeNode) (ast.AstSig, *err.Error) {
	sig := ast.AstSig{}
	for _, arg := range args {
		if arg.GetToken().Type == token.IDENT && p.Bling.Contains(arg.GetToken().Literal) {
			sig = append(sig, ast.NameTypeAstPair{VarName: arg.GetToken().Literal, VarType: &ast.TypeBling{*arg.GetToken(), arg.GetToken().Literal}})
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
	sig, e := p.RecursivelySlurpSignature(node, ast.DUMMY_TYPE_AST)
	if e != nil {
		return result
	}
	for _, pair := range sig {
		result = append(result, pair.VarName)
	}
	return result
}

func (p *Parser) GetVariablesFromAstSig(sig ast.AstSig) []string {
	result := []string{}
	for _, pair := range sig {
		result = append(result, pair.VarName)
	}
	return result
}

// TODO --- is there any sensible alternative to this?
func (p *Parser) RecursivelySlurpSignature(node ast.Node, dflt ast.TypeNode) (ast.AstSig, *err.Error) {
	switch typednode := node.(type) {
	case *ast.InfixExpression:
		switch {
		case typednode.Token.Type == token.COMMA:
			RHS, err := p.RecursivelySlurpSignature(typednode.Args[2], dflt)
			if err != nil {
				return nil, err
			}
			LHS, err := p.RecursivelySlurpSignature(typednode.Args[0], RHS.GetVarType(0).(ast.TypeNode))
			if err != nil {
				return nil, err
			}
			return append(LHS, RHS...), nil
		case typednode.Operator == ".":
			namespacedIdent, err := recursivelySlurpNamespace(typednode)
			if err != nil {
				return nil, err
			}
			return ast.AstSig{ast.NameTypeAstPair{VarName: namespacedIdent, VarType: dflt}}, nil
		default:
			return nil, newError("parse/sig/b", typednode.GetToken())
		}
	case *ast.TypeSuffixExpression:
		LHS, err := p.getSigFromArgs(typednode.Args, typednode.Operator)
		if err != nil {
			return nil, err
		}
		for k := range LHS {
			LHS[k].VarType = typednode.Operator
		}
		return LHS, nil
	case *ast.SuffixExpression:
		return nil, newError("parse/sig/c", typednode.GetToken())
	case *ast.Identifier:
		return ast.AstSig{ast.NameTypeAstPair{VarName: typednode.Value, VarType: dflt}}, nil
	case *ast.PrefixExpression:
		// We may be declaring a parameter which has the same name as a function --- e.g. 'f'.
		// The parser will have parsed this as a prefix expression if it was followed by a type, e.g.
		// 'foo (f func) : <function body>'.
		return ast.AstSig{ast.NameTypeAstPair{VarName: typednode.Operator, VarType: dflt}}, nil

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

func (p *Parser) RecursivelySlurpReturnTypes(node ast.Node) ast.AstSig {
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
	case *ast.TypeExpression:
		if typednode.TypeArgs == nil {
			return ast.AstSig{ast.NameTypeAstPair{VarName: "", VarType: &ast.TypeWithName{typednode.Token, typednode.Operator}}}
		}
		return ast.AstSig{ast.NameTypeAstPair{VarName: "", VarType: typednode}}
	case *ast.SuffixExpression:
		if typednode.Operator == "?" || typednode.Operator == "!" {
			return ast.AstSig{ast.NameTypeAstPair{VarName: "", VarType: &ast.TypeSuffix{typednode.Token, typednode.Operator, p.RecursivelySlurpReturnTypes(typednode.Args[0])}}}
		}
	default:
		println("node is", typednode.String(), reflect.TypeOf(typednode).String())
		p.Throw("parse/ret/b", typednode.GetToken())
	}
	return nil
}

// Converts type expressions to ast.TypeNodes, i.e. the sort of description of a type
// that we should be able to find in a function signature.
func (p *Parser) ToAstType(te *ast.TypeExpression) ast.TypeNode {
	if len(te.TypeArgs) == 0 {
		return &ast.TypeWithName{Token: te.Token, OperatorName: te.Operator}
	}
	// This is either a bool, float, int, rune, string, type or enum literal, in which
	// case the whole thing should be, OR it's a type with parameters, or it's not well-
	// formed and shouldn't be here at all.
	indexArg := te.TypeArgs[0]
	if p.findTypeArgument(indexArg).T != values.ERROR {
		return p.toTypeWithArguments(te)
	}
	return p.toTypeWithParameters(te)
}

func (p *Parser) toTypeWithArguments(te *ast.TypeExpression) *ast.TypeWithArguments {
	result := *&ast.TypeWithArguments{te.Token, te.Operator, []*ast.Argument{}}
	for _, arg := range te.TypeArgs {
		v := p.findTypeArgument(arg)
		if v.T == values.ERROR {
			p.Throw("parse/type/malformed/b", te.GetToken())
			return &result
		}
		result.Arguments = append(result.Arguments, &ast.Argument{*arg.GetToken(), v.T, v.V})
	}
	return &result
}

func (p *Parser) toTypeWithParameters(te *ast.TypeExpression) *ast.TypeWithParameters {
	sig := p.extractSig(te.TypeArgs)
	params := []*ast.Parameter{}
	for _, pair := range sig {
		newParameter := &ast.Parameter{pair.VarName, pair.VarType.String()}
		params = append(params, newParameter)
	}
	return &ast.TypeWithParameters{te.Token, te.Operator, params}
}

func (p *Parser) findTypeArgument(arg ast.Node) values.Value {
	switch arg := arg.(type) {
	case *ast.Identifier:
		if p.IsEnumElement(arg.Value) {
			return values.Value{0, arg.Value} // We don't know the enum types yet so we kludge them in later.
		}
	case *ast.BooleanLiteral:
		return values.Value{values.BOOL, arg.Value}
	case *ast.FloatLiteral:
		return values.Value{values.FLOAT, arg.Value}
	case *ast.IntegerLiteral:
		return values.Value{values.INT, arg.Value}
	case *ast.RuneLiteral:
		return values.Value{values.RUNE, arg.Value}
	case *ast.StringLiteral:
		return values.Value{values.STRING, arg.Value}
	case *ast.TypeExpression:
		return values.Value{values.TYPE, p.ToAstType(arg)}
	}
	return values.Value{values.ERROR, nil}
}

func (p *Parser) IsEnumElement(name string) bool {
	_, ok := p.EnumElementNames[name]
	return ok
}

// Finds whether an identifier is in the right place to be a function, or whether it's being used
// as though it's a variable or constant.
func (p *Parser) isPositionallyFunctional() bool {
	if assignmentTokens.Contains(p.PeekToken.Type) {
		return false
	}
	if p.BlingManager.canBling(p.PeekToken.Literal) {
		return false
	}
	if p.PeekToken.Type == token.RPAREN || p.PeekToken.Type == token.PIPE ||
		p.PeekToken.Type == token.MAPPING || p.PeekToken.Type == token.FILTER ||
		p.PeekToken.Type == token.COLON || p.PeekToken.Type == token.MAGIC_COLON ||
		p.PeekToken.Type == token.COMMA || p.PeekToken.Type == token.RBRACK ||
		p.PeekToken.Type == token.RBRACE {
		return false
	}
	if p.CurToken.Literal == "type" && p.IsTypePrefix(p.PeekToken.Literal) {
		return true
	}
	if p.Functions.Contains(p.CurToken.Literal) && p.Typenames.Contains(p.CurToken.Literal) {
		return p.typeIsFunctional()
	}

	if p.Functions.Contains(p.CurToken.Literal) && p.PeekToken.Type != token.EOF {
		return true
	}
	if p.Prefixes.Contains(p.CurToken.Literal) {
		return p.PeekToken.Type != token.EOF
	}
	if literalsAndLParen.Contains(p.PeekToken.Type) {
		return true
	}
	if p.PeekToken.Type != token.IDENT {
		return false
	}
	if p.Infixes.Contains(p.PeekToken.Literal) {
		return false
	}
	if p.Suffixes.Contains(p.PeekToken.Literal) {
		return false
	}
	return true
}

// TODO --- there may at this point not be any need to have this different from any other function.
func (p *Parser) typeIsFunctional() bool {
	if p.BlingManager.canBling(p.PeekToken.Literal) {
		return false
	}
	if p.PeekToken.Type == token.RPAREN || p.PeekToken.Type == token.PIPE ||
		p.PeekToken.Type == token.MAPPING || p.PeekToken.Type == token.FILTER ||
		p.PeekToken.Type == token.COLON || p.PeekToken.Type == token.MAGIC_COLON ||
		p.PeekToken.Type == token.COMMA || p.PeekToken.Type == token.RBRACK ||
		p.PeekToken.Type == token.RBRACE || p.PeekToken.Literal == "?" {
		return false
	}
	if p.PeekToken.Type == token.EMDASH || p.PeekToken.Type == token.LBRACK {
		return true
	}
	if literalsAndLParen.Contains(p.PeekToken.Type) {
		return true
	}
	if p.PeekToken.Literal == "from" {
		return true
	}
	if p.Infixes.Contains(p.PeekToken.Literal) {
		return false
	}
	if p.nativeInfixes.Contains(p.PeekToken.Type) {
		return false
	}
	if p.Midfixes.Contains(p.PeekToken.Literal) {
		return false
	}
	if p.Functions.Contains(p.PeekToken.Literal) && p.PeekToken.Type != token.EOF {
		return true
	}
	if p.Prefixes.Contains(p.PeekToken.Literal) {
		return p.PeekToken.Type != token.EOF
	}
	return p.PeekToken.Type != token.EOF
}
