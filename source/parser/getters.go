package parser

import (
	"bytes"
	"reflect"

	"github.com/tim-hardcastle/Pipefish/source/ast"
	"github.com/tim-hardcastle/Pipefish/source/err"
	"github.com/tim-hardcastle/Pipefish/source/token"
	"github.com/tim-hardcastle/Pipefish/source/values"
)

// Auxiliary functions that extract data from data.

// Slurps the parts of a function out of it. As the colon after a function definition has
// extremely low precedence, we should find it at the root of the tree.
// We extract the function name first and then hand its branch or branches off to a recursive tree-slurper.
func (prsr *Parser) ExtractPartsOfFunction(fn ast.Node) (string, uint32, ast.AstSig, ast.AstSig, ast.Node, ast.Node) {
	var (
		functionName          string
		sig                   ast.AstSig
		rTypes                ast.AstSig
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

func (prsr *Parser) GetPartsOfSig(start ast.Node) (functionName string, pos uint32, sig ast.AstSig) {
	switch start := start.(type) {
	case *ast.PrefixExpression:
		functionName = start.Operator
		pos = 0
		sig = prsr.extractSig(start.Args)
	case *ast.TypePrefixExpression:
		var out bytes.Buffer
		out.WriteString(start.Operator)
		if len(start.TypeArgs) != 0 {
			out.WriteString("{")
			sep := ""
			for _, v := range start.Args {
				out.WriteString(sep)
				out.WriteString(prsr.prettyPrint(v, inlineCtxt))
				sep = ", "
			}
			out.WriteString("}")
		}
		functionName = out.String()
		pos = 0
		sig = prsr.extractSig(start.Args)
	case *ast.SigTypePrefixExpression: // TODO --- can this ever happen?
		functionName = start.Operator.String()
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
	case *ast.TypeSuffixExpression:
		functionName = start.Operator.String()
		pos = 2
		sig = prsr.extractSig(start.Args)
	case *ast.UnfixExpression:
		functionName = start.Operator
		pos = 3
		sig = ast.AstSig{}
	default:
		prsr.Throw("parse/sig/malformed/d", start.GetToken())
		return functionName, pos, sig
	}
	return functionName, pos, sig
}

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
			if p.Endfixes.Contains(arg.Value) {
				varName = arg.Value
				varType = &ast.TypeBling{*arg.GetToken(), arg.Value}
			} else {
				varName = arg.Value
				varType = nil
			}
		// TODO --- we have to do something about this mess. Signatures and main bodies
		// have different syntax and this is a wake-up call.
		case *ast.PrefixExpression:
			if p.Forefixes.Contains(arg.Operator) {
				varName = arg.Operator
				varType = &ast.TypeBling{*arg.GetToken(), arg.Operator}
			} else {
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
		case *ast.InfixExpression:
			if p.Midfixes.Contains(arg.Operator) {
				varName = arg.Operator
				varType = &ast.TypeBling{*arg.GetToken(), arg.Operator}
			} else {
				p.Throw("parse/sig/infix", arg.GetToken())
				return nil
			}
		case *ast.Bling:
			varName = arg.Value
			varType = &ast.TypeBling{*arg.GetToken(), arg.Value}
		}
		if j == len(args)-1 && varType == nil {
			for i := backTrackTo; i < len(sig); i++ {
				sig[i].VarType = ast.ANY_NULLABLE_TYPE_AST
			}
			varType = ast.ANY_NULLABLE_TYPE_AST
		}
		if _, ok := varType.(*ast.TypeBling); !(ok || varType == nil) {
			for i := backTrackTo; i < len(sig); i++ {
				sig[i].VarType = varType
			}

		}
		if _, ok := varType.(*ast.TypeBling); ok {
			if len(sig) > 0 && sig[len(sig)-1].VarType == nil {
				for i := backTrackTo; i < len(sig); i++ {
					sig[i].VarType = ast.ANY_NULLABLE_TYPE_AST
				}
			}
		}
		sig = append(sig, ast.NameTypeAstPair{VarName: varName, VarType: varType})
		if sig[len(sig)-1].VarType != nil {
			backTrackTo = len(sig)
		}
		if _, ok := varType.(*ast.TypeBling); ok {
			varType = nil
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
// This is all rather horrible and basically exists as a result of two reasons. First, since all the signatures whether of assignment
// or function definition or struct definition or whatever fit into the same mold, we would like to be able to keep our code DRY by
// extracting them all in the same way. However, as we don't have anything like a `let` command, the parser doesn't know that it's parsing an
// assignment until it reaches the equals sign, by which time it's already turned the relevant tokens into an AST. Rather than kludge
// my way out of that, I kludged my way around it by writing this thing which extracts the signature from an AST, and which has grown steadily
// more complex with the language.
func (p *Parser) RecursivelySlurpSignature(node ast.Node, dflt ast.TypeNode) (ast.AstSig, *err.Error) {
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
			middle := ast.NameTypeAstPair{VarName: typednode.Operator, VarType: &ast.TypeBling{*typednode.GetToken(), typednode.Operator}}
			return append(append(LHS, middle), RHS...), nil
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
		if p.Endfixes.Contains(typednode.Operator) {
			LHS, err := p.getSigFromArgs(typednode.Args, dflt)
			if err != nil {
				return nil, err
			}
			end := ast.NameTypeAstPair{VarName: typednode.Operator, VarType: &ast.TypeBling{*typednode.GetToken(), typednode.Operator}}
			return append(LHS, end), nil
		} else {
			return nil, newError("parse/sig/c", typednode.GetToken())
		}
	case *ast.Identifier:
		if p.Endfixes.Contains(typednode.Value) {
			return ast.AstSig{ast.NameTypeAstPair{VarName: typednode.Value, VarType: &ast.TypeBling{*typednode.GetToken(), typednode.Value}}}, nil
		}
		return ast.AstSig{ast.NameTypeAstPair{VarName: typednode.Value, VarType: dflt}}, nil
	case *ast.PrefixExpression:
		if p.Forefixes.Contains(typednode.Operator) {
			RHS, err := p.getSigFromArgs(typednode.Args, dflt)
			if err != nil {
				return nil, err
			}
			front := ast.AstSig{ast.NameTypeAstPair{VarName: typednode.Operator, VarType: &ast.TypeBling{*typednode.GetToken(), typednode.Operator}}}
			return append(front, RHS...), nil
		} else {
			// We may well be declaring a parameter which will have the same name as a function --- e.g. 'f'.
			// The parser will have parsed this as a prefix expression if it was followed by a type, e.g.
			// 'foo (f func) : <function body>'. We ought therefore to be interpreting it as a parameter
			// name under those circumstances.
			return ast.AstSig{ast.NameTypeAstPair{VarName: typednode.Operator, VarType: dflt}}, nil
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
		return &ast.TypeWithName{Token: te.Token, Name: te.Operator}
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

func (p *Parser) MakeAbstractSigFromStringSig(sig ast.AstSig) ast.AbstractSig {
	result := make(ast.AbstractSig, sig.Len())
	for i, pair := range sig {
		typename := pair.VarType
		typeToUse := typename
		if typename, ok := typename.(*ast.TypeDotDotDot); ok {
			typeToUse = typename.Right
		}
		result[i] = ast.NameAbstractTypePair{pair.VarName, p.GetAbstractType(typeToUse)}
	}
	return result
}

func (p *Parser) TypeExists(name string) bool {
	// Check if it's a shared type: 'int', 'struct', 'like{list}', 'any?' etc.
	if _, ok := p.Common.Types[name]; ok {
		return true
	}
	// ... or the result should just be in the parser's own type map.
	_, ok := p.TypeMap[name]
	return ok
}

func (p *Parser) IsEnumElement(name string) bool {
	_, ok := p.EnumElementNames[name]
	return ok
}

func (p *Parser) GetAbstractType(typeNode ast.TypeNode) values.AbstractType {
	if typeNode == nil { // This can mark an absence of return types.
		return values.AbstractType{}
	}
	switch typeNode := typeNode.(type) {
	case *ast.TypeWithName:
		return p.GetAbstractTypeFromTypeSys(typeNode.Name)
	case *ast.TypeWithArguments:
		return p.GetAbstractTypeFromTypeSys(typeNode.String())
	case *ast.TypeInfix:
		LHS := p.GetAbstractType(typeNode.Left)
		RHS := p.GetAbstractType(typeNode.Right)
		if typeNode.Operator == "/" {
			return LHS.Union(RHS)
		}
		if typeNode.Operator == "&" {
			return LHS.Intersect(RHS)
		}
	case *ast.TypeSuffix:
		LHS := p.GetAbstractType(typeNode.Left)
		if typeNode.Operator == "?" {
			return LHS.Insert(values.NULL)
		}
		if typeNode.Operator == "!" {
			return LHS.Insert(values.ERROR)
		}
	case *ast.TypeBling:
		return values.AbstractType{[]values.ValueType{values.BLING}}
	case *ast.TypeDotDotDot:
		return p.GetAbstractType(typeNode.Right)
	case *ast.TypeWithParameters:
		return p.GetAbstractTypeFromTypeSys(typeNode.Blank().String())
	case *ast.TypeExpression:
		if typeNode.TypeArgs == nil {
			return p.GetAbstractTypeFromTypeSys(typeNode.Operator)
		} else {
			return p.ParTypes2[typeNode.Operator].PossibleReturnTypes
		}
	}
	panic("Can't compile type node " + typeNode.String() + " with type " + reflect.TypeOf(typeNode).String())
}

func (p *Parser) GetAbstractTypeFromTypeSys(name string) values.AbstractType {
	// Check if it's a shared type: 'int', 'struct', 'clones{list}', 'any?' etc.
	if result, ok := p.Common.Types[name]; ok {
		return result
	}
	// ... or the result should just be in the parser's own type map.
	result, _ := p.TypeMap[name]
	return result
}

// Finds whether an identifier is in the right place to be a function, or whether it's being used
// as though it's a variable or constant.
func (p *Parser) isPositionallyFunctional() bool {
	if assignmentTokens.Contains(p.PeekToken.Type) {
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
	if p.Functions.Contains(p.CurToken.Literal) && p.TypeExists(p.CurToken.Literal) {
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
	if p.Midfixes.Contains(p.PeekToken.Literal) {
		return false
	}
	if p.Endfixes.Contains(p.PeekToken.Literal) {
		return false
	}
	if p.Suffixes.Contains(p.PeekToken.Literal) {
		return false
	}
	return true
}

// TODO --- there may at this point not be any need to have this different from any other function.
func (p *Parser) typeIsFunctional() bool {
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
