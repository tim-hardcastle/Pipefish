package evaluator

// This is basically your standard tree-walking evaluator, with one or two minor peculiarities.

import (
	"strconv"
	"strings"

	"charm/ast"
	"charm/object"
	"charm/parser"
	"charm/signature"
	"charm/sysvars"
	"charm/token"
)

var (
	UNSATISFIED = &object.UnsatisfiedConditional{}
	SUCCESS = &object.SuccessfulAssignment{}
	
)

func Evaluate(node ast.Node, parser *parser.Parser, env *object.Environment) object.Object {
	result := Eval(node, parser, env)
	if result.Type() == object.UNSATISFIED_OBJ {
		return newError("eval/unsatisfied/a", node.GetToken())
	}
	return result
}

func Eval(node ast.Node, parser *parser.Parser, env *object.Environment) object.Object {

	switch node := node.(type) {

	// Statements

	case *ast.Expression:
		return Eval(node.Node, parser, env)

	case *ast.IntegerLiteral:
		return &object.Integer{Value: node.Value}

	case *ast.FloatLiteral:
		return &object.Float{Value: node.Value}

	case *ast.StringLiteral:
		return &object.String{Value: node.Value}

	case *ast.BooleanLiteral:
		return object.MakeBool(node.Value)

	case *ast.TypeLiteral:
		return &object.Type{Value: node.Value}

	case *ast.EmptyTuple:
		return &object.Tuple{Elements: []object.Object{}}

	case *ast.PrefixExpression:
		right := Eval(node.Right, parser, env)
		if isError(right) && node.Operator != "type" {
			right.(*object.Error).Trace = append(right.(*object.Error).Trace, node.GetToken())
			return right
		}
		return evalPrefixExpression(node.Token, node.Operator, right, parser, env)

	case *ast.UnfixExpression:
		return evalUnfixExpression(node.Token, node.Operator, parser, env)

	case *ast.SuffixExpression:
		left := Eval(node.Left, parser, env)
		if isError(left) {
			left.(*object.Error).Trace = append(left.(*object.Error).Trace, node.GetToken())
			return left
		}
		return evalSuffixExpression(node.Token, node.Operator, left, parser, env)

	case *ast.AssignmentExpression:
		variables := signature.Signature{}
		if node.Token.Type == token.TYP_ASSIGN {
			variables = append(variables, signature.NameTypePair{node.Left.(*ast.TypeLiteral).Token.Literal, "type"})
		} else {
			variables = parser.RecursivelySlurpSignature(node.Left, "*")
		}
		lLen := len(variables)
		right := Eval(node.Right, parser, env)
		if isError(right)  {
			if node.Token.Type == token.GVN_ASSIGN {
				for i := 0 ; i < lLen; i++ {
					err := Assign(variables[i], right, parser, env, node.Token)
					if err != nil { return err }
				}
				return SUCCESS
			} else {
				right.(*object.Error).Trace = append(right.(*object.Error).Trace, node.GetToken())
				return right
			}
		}
		if right.Type() == object.UNSATISFIED_OBJ && node.Token.Type != token.GVN_ASSIGN {
			return newError("eval/unsatisfied/b", node.Token)
		}

		if lLen == 1 { 
			err := Assign(variables[0], right, parser, env, node.Token)
			if err == nil {
				return SUCCESS
			} else {
				return err
			}
		}

		if right.Type() != object.TUPLE_OBJ ||
			right.(*object.Tuple).Len() < len(variables) {
				return newError("eval/values", node.Token)
		}

		rLen := right.(*object.Tuple).Len()

		for i := 0 ; i < lLen - 1 ; i++ {
			err := Assign(variables[i], right.(*object.Tuple).Elements[i], parser, env, node.Token)
			if err != nil { return err }
		}
		if lLen == rLen {
			err := Assign(variables[lLen - 1], right.(*object.Tuple).Elements[lLen - 1], parser, env, node.Token)
			if err != nil { return err }
			return SUCCESS
		}
		err := Assign(variables[lLen - 1], &object.Tuple{Elements : right.(*object.Tuple).Elements[lLen - 1 :]}, parser, env, node.Token)
		if err != nil { return err }
		return SUCCESS

	case *ast.InfixExpression:

		if node.GetToken().Type == token.GIVEN { // Then either something has gone very wrong or we
			leftBranch := node.Left	// are looking at the given block of an inner function
			switch leftBranch := leftBranch.(type) {
			case *ast.AssignmentExpression :
				rightBranchOfLeftBranch := leftBranch.Right
				switch rightBranchOfLeftBranch := rightBranchOfLeftBranch.(type) {
				case *ast.FuncExpression :
					rightBranchOfLeftBranch.Given = node.Right
					default: return newError("eval/given/a", node.GetToken())
				}
			default: return newError("eval/given/b", node.GetToken())
			}
			return Eval(leftBranch, parser, env)
		}

		left := Eval(node.Left, parser, env)
		if isError(left) {
			left.(*object.Error).Trace = append(left.(*object.Error).Trace, node.GetToken())
			return left
		}
		if isUnsatisfiedConditional(left) { return newError("unsatisfied/c", node.Token) }
		right := Eval(node.Right, parser, env)
		if isError(right) {
			right.(*object.Error).Trace = append(right.(*object.Error).Trace, node.GetToken())
			return right
		}
		if isUnsatisfiedConditional(right) { return newError("unsatisfied/d", node.Token) }
		return evalInfixExpression(node.Token, node.Operator, left, right, parser, env)

	case *ast.LazyInfixExpression:
		left := Eval(node.Left, parser, env)
		if isError(left) {
			left.(*object.Error).Trace = append(left.(*object.Error).Trace, node.GetToken())
			return left
		}
		leftEvaluation := evalLazyLeftExpression(node.Token, node.Operator, left, parser, env)
		if leftEvaluation != nil && leftEvaluation != SUCCESS && 
		     (leftEvaluation.Type() != object.RETURN_OBJ){
			return leftEvaluation
		}
		right := Eval(node.Right, parser, env)
		if isError(right) {              
			right.(*object.Error).Trace = append(right.(*object.Error).Trace, node.GetToken()) 
			return right
		}
		if leftEvaluation != nil && leftEvaluation.Type() == object.RETURN_OBJ {
			if node.Operator != ";" {
				return newError("eval/return", node.Token)
			}
			if right == SUCCESS { return left }
			if right.Type() == object.RETURN_OBJ {
				left.(*object.Return).Elements = append(left.(*object.Return).Elements, right.(*object.Return).Elements ...)
			return left
			}
			return newError("eval/malret", node.Token)
		}
		return evalLazyRightExpression(node.Token, node.Operator, right, parser, env)

	case *ast.ExecExpression :
		return Eval(node.Right, parser.Parsers[node.Left.(*ast.Identifier).Value], env)

	case *ast.Identifier:
		// We may have reached a bit of orphaned endbling.
		if parser.Endfixes.Contains(node.Value) {
			return &object.Bling{Value: node.Value}
		}
		// Otherwise this is an actual identifier and we should return its value
		return evalIdentifier(node, parser, env)

	case *ast.IndexExpression:
		left := Eval(node.Left, parser, env)
		if isError(left) {
			left.(*object.Error).Trace = append(left.(*object.Error).Trace, node.GetToken())
			return left
		}
		index := Eval(node.Index, parser, env)
		if isError(index) {
			index.(*object.Error).Trace = append(index.(*object.Error).Trace, node.GetToken())
			return index
		}
		return evalIndexExpression(node.Token, left, index, parser, env)
	case *ast.ListExpression:
		if node.List == nil {
			return &object.List{Elements: []object.Object{}} 
		}
		list := Eval(node.List, parser, env)
		if list.Type() == object.TUPLE_OBJ {
			return &object.List{Elements: list.(*object.Tuple).Elements}
		}
		if list.Type() == object.ERROR_OBJ {
			list.(*object.Error).Trace = append(list.(*object.Error).Trace, node.GetToken())
			return list
		}
		if list.Type() == object.UNSATISFIED_OBJ {
			return newError("eval/unsatisfied/e", node.Token)
		}
		return &object.List{Elements: []object.Object{list}}

	case *ast.SetExpression:
		if node.Set == nil {
			return object.SetFromSlice([]object.Object{})
		}
		list := Eval(node.Set, parser, env)
		if list.Type() == object.TUPLE_OBJ {
			return object.SetFromSlice(list.(*object.Tuple).Elements)
		}
		if list.Type() == object.ERROR_OBJ {
			list.(*object.Error).Trace = append(list.(*object.Error).Trace, node.GetToken())
			return list
		}
		if list.Type() == object.UNSATISFIED_OBJ {
			return newError("eval/unsatisfied/f", node.Token)
		}
		return object.SetFromSlice([]object.Object{list})

	case *ast.FuncExpression :
		return &object.Func{Function : ast.Function{Sig: node.Sig, Body: node.Body, Given: node.Given}, Env: env}

	case *ast.StructExpression :
		return &object.StructDef{Sig : node.Sig} 
	case *ast.ApplicationExpression :
		left := Eval(node.Left, parser, env)
		if isError(left) {
			left.(*object.Error).Trace = append(left.(*object.Error).Trace, node.GetToken())
			return left
		}
		right := Eval(node.Right, parser, env)
		if isError(right) {
			right.(*object.Error).Trace = append(right.(*object.Error).Trace, node.GetToken())
			return right
		}
		if !(left.Type() == object.FUNC_OBJ) {
			return newError("eval/apply", node.Token, object.TrueType(left))
		}
		params := []object.Object{}
		if right.Type() == object.TUPLE_OBJ {
			params = right.(*object.Tuple).Elements
		} else {
			params = []object.Object{right}
		}
		return applyFunction(left.(*object.Func).Function, params, parser, node.Token, left.(*object.Func).Env)
	}
	return newError("eval/oops", token.Token{Line: 0})
}

func evalLazyRightExpression(token token.Token, operator string, right object.Object, prsr *parser.Parser, env *object.Environment) object.Object {
	if isUnsatisfiedConditional(right) { return UNSATISFIED }
	if operator == ":" || operator == ";" { return right }
	return isTruthy(token, right, prsr, env)
}

func evalLazyLeftExpression(token token.Token, operator string, left object.Object, prsr *parser.Parser, env *object.Environment) object.Object {
	if operator == ";" {
		if isUnsatisfiedConditional(left) {
			return nil
		}
		return left
	}
	if isUnsatisfiedConditional(left) { return newError("eval/unsatisfied/g", token) }
	truthiness := isTruthy(token, left, prsr, env)
	if truthiness.Type() == object.ERROR_OBJ {
		truthiness.(*object.Error).Trace = append(truthiness.(*object.Error).Trace, token)
		return truthiness
	}
	switch operator {
	case ":":
		if truthiness == object.FALSE {
			return UNSATISFIED 
		}
		case "and":	
			if truthiness == object.FALSE { 
				return object.FALSE
			}
		case "or":
			if truthiness == object.TRUE { 
				return object.TRUE
		}
	}
	return nil
}

func evalPrefixExpression(token token.Token, operator string, right object.Object, prsr *parser.Parser, env *object.Environment) object.Object {
	if isUnsatisfiedConditional(right) { return newError("eval/unsatisfied/h", token) }
	variable, ok := env.Get(operator)
	params := make([]object.Object, 0) 
	switch {
	case operator == "not":
		return evalNotOperatorExpression(token, right, prsr, env)
	case operator == "eval":
		return evalEvalExpression(token, right, prsr, env)
	case operator == "return":
		return evalReturnExpression(token, right)
	case prsr.Prefixes.Contains(operator) || prsr.Functions.Contains(operator) || 
	     prsr.Forefixes.Contains(operator) || ok && variable.Type() == object.FUNC_OBJ:
		if right.Type() == object.TUPLE_OBJ {
			params = right.(*object.Tuple).Elements
		} else {
			params = []object.Object{right}
		}
		// A forefix is merely bling.
		if prsr.Forefixes.Contains(operator) {
			lparams := []object.Object{&object.Bling{Value: operator}}
			lparams = append(lparams, params...)
			return &object.Tuple{Elements: lparams}
		}
		// We may have ourselves a lambda
		if ok && variable.Type() == object.FUNC_OBJ {
			return applyFunction(variable.(*object.Func).Function, params, prsr, token, variable.(*object.Func).Env)
		}
		// Otherwise we have a function or prefix, which work the same at this point.
		return evaluateFunction(token, operator, params, prsr, env)
	}
	if token.Source == "REPL input" {
		return newError("eval/repl/a", token, params)
	}
	return newError("eval/unknown/prefix", token, right)
}

func evaluateFunction(token token.Token, operator string, params []object.Object, prsr *parser.Parser, env *object.Environment) object.Object {
	f, err := prsr.FindFunction(operator, params, token.Source == "REPL input")
	if err != "" && token.Source == "REPL input" {
		return newError("eval/repl/a", token, params) 
	}
	if err == "keyword" {
		return newError("eval/keyword/a", token)
	}
	if err == "sig" {
		return newError("eval/sig/a", token, params)
	}
	if f.Cmd {
		return applyFunction(f, params, prsr, token, env)
	}
	return applyFunction(f, params, prsr, token, prsr.Globals)
}


func evalSuffixExpression(token token.Token, operator string, left object.Object, prsr *parser.Parser, env *object.Environment) object.Object {
	params := make([]object.Object, 0) 
	if isUnsatisfiedConditional(left) { return newError("eval/unsatisfied/i", token) }
	if prsr.Suffixes.Contains(operator) || prsr.Endfixes.Contains(operator) {
		if left.Type() == object.TUPLE_OBJ {
			params = left.(*object.Tuple).Elements
		} else {
			params = []object.Object{left}
		}
		// If it's an endfix, it's just bling and should be added to the params ...
		if prsr.Endfixes.Contains(operator) {
			eparam := object.Bling{Value: operator}
			params = append(params, &eparam)
			return &object.Tuple{Elements: params}
		}
		// Otherwise we have a suffix function
		f, err := prsr.FindFunction(operator, params, token.Source == "REPL input")
		if err != "" && token.Source == "REPL input" {
			return newError("eval/repl/b", token, params) 
		}
		if err == "keyword" {
			return newError("eval/keyword/b", token)
		}
		if err == "sig" {
			return newError("eval/sig/b", token, params)
		}
		if f.Cmd {
			return applyFunction(f, params, prsr, token, env)
		}
		return applyFunction(f, params, prsr, token, prsr.Globals)
		
	}
	if token.Source == "REPL input" {
		return newError("eval/repl/b", token, params)
	}
	return newError("eval/unknown/suffix", token, left)
}


func evalUnfixExpression(token token.Token, operator string, prsr *parser.Parser, env *object.Environment) object.Object {
	params := make([]object.Object, 0) 
	f, err := prsr.FindFunction(operator, params, token.Source == "REPL input")
	if err != "" && token.Source == "REPL input" {
		return newError("eval/repl/c", token, params) 
	}
	if err == "keyword" {
		return newError("eval/keyword/c", token)
	}
	if err == "sig" {
		return newError("eval/sig/c", token, params)
	}
	if f.Cmd {
		return applyFunction(f, params, prsr, token, env)
	}
	return applyFunction(f, params, prsr, token, prsr.Globals)
}


func evalInfixExpression(
	tok token.Token, operator string, left, right object.Object, prsr *parser.Parser, env *object.Environment,
) object.Object {
	params := make([]object.Object, 0)
	switch {
	case left.Type() == object.UNSATISFIED_OBJ: 
		return newError("eval/unsatisfied/j", tok)
	case right.Type() == object.UNSATISFIED_OBJ:
		return newError("eval/unsatisfied/k" , tok)
	case operator == "," :
		return evalTupleExpression(left, right)
	case left.Type() == object.ERROR_OBJ:
		left.(*object.Error).Trace = append(left.(*object.Error).Trace, tok)
		return left
	case right.Type() == object.ERROR_OBJ:
		right.(*object.Error).Trace = append(right.(*object.Error).Trace, tok)
		 return right
	case prsr.Infixes.Contains(operator) || prsr.Midfixes.Contains(operator) :
		// Either way we start by assembling a parameters list ...
		rparams := make([]object.Object, 0) 
		if left.Type() == object.TUPLE_OBJ {
			params = left.(*object.Tuple).Elements
		} else {
			params = []object.Object{left}
		}
		mparam := object.Bling{Value: operator}
		if right.Type() == object.TUPLE_OBJ {
			rparams = right.(*object.Tuple).Elements
		} else {
			rparams = []object.Object{right}
		}
		params = append(params, &mparam)
		params = append(params, rparams...)
		// If it's a midfix then the operator was merely bling and we return a tuple ...
		if prsr.Midfixes.Contains(operator) {
			return &object.Tuple{Elements: params}
		}
		if operator == "with" && params[0].Type() == object.TYPE_OBJ {
			constructor, ok := prsr.BuiltinFunctions[params[0].(*object.Type).Value + "_with"]
			if !ok { return newError("eval/with/type", tok, params[0].(*object.Type).Value) }
			params = params[2:]
			return constructor(prsr, params...)
		}
		// Otherwise we have an infix function
		f, err := prsr.FindFunction(operator, params, tok.Source == "REPL input")
		if err != "" && tok.Source == "REPL input" {
			return newError("eval/repl/d", tok, params) 
		}
		if err == "keyword" {
			return newError("eval/keyword/d", tok)
		}
		if err == "sig" {
			return newError("eval/sig/d", tok, params)
		}
		if f.Cmd {
			return applyFunction(f, params, prsr, tok, env)
		}
		return applyFunction(f, params, prsr, tok, prsr.Globals)
	case operator == "==":
		return object.MakeBool(object.Equals(left, right))
	case operator == "!=":
		return object.MakeInverseBool(object.Equals(left, right))
	default:
		if tok.Source == "REPL input" {
			return newError("eval/repl/b", tok, params)
		}
		return newError("eval/unknown/operator", tok, left, right)
	}
}



func Assign(variable signature.NameTypePair, right object.Object, prsr *parser.Parser, env *object.Environment, tok token.Token) *object.Error {
	if right.Type() == object.UNSATISFIED_OBJ {
		return newError("eval/unsatisfied/l", tok)
	}
	if right.Type() == object.STRUCTDEF_OBJ {
		if tok.Type != token.TYP_ASSIGN {
			return newError("eval/struct/def", tok)
		}
		// So what we're going to do is add the constructors to the builtins,
		// and add the function name, sig, and body to the parser's
		// table of functions, and add the labels to the environment.

		// Registration of the type as a type and a suffix has already happened so
		// that the parser can parse the declarations properly.

		// The first constructor function ...
		constructor := func(p *parser.Parser, args ...object.Object) object.Object {
			result := &object.Struct{Value: make(map[string] object.Object)}
			for k, v := range(args) {
				result.Labels = append(result.Labels, right.(*object.StructDef).Sig[k].VarName)
				result.Value[right.(*object.StructDef).Sig[k].VarName] = v
			}
			result.Name = variable.VarName
			return result
		}

		prsr.BuiltinFunctions[variable.VarName] = constructor

		// And the first constructor function as it appears in the parser's function table.

		prsr.FunctionTable.Add(prsr.TypeSystem,
			variable.VarName, // The function name ...
		     ast.Function{Sig: right.(*object.StructDef).Sig, // ... signature ...
		     Body: &ast.BuiltInExpression{Name: variable.VarName} }) // ... and a reference to the built-in as the body

		// The second constructor function ...

		constructor_2 := func(p *parser.Parser, args ...object.Object) object.Object {
			result := &object.Struct{Value: make(map[string] object.Object)}
			for _, v := range(args) {
				if v.Type() != object.PAIR_OBJ {
					return newError("eval/pair", tok)
				}
				if v.(*object.Pair).Left.Type() != object.LABEL_OBJ {
					return newError("eval/label", tok)
				}
				positionOfLabelInFields := - 1
				for i, w := range(right.(*object.StructDef).Sig) {
					if string(v.(*object.Pair).Left.(*object.Label).Value) == w.VarName {
						positionOfLabelInFields = i
						break
					}
				}
				if positionOfLabelInFields == - 1 {
					return newError("eval/field/struct", tok, v.(*object.Pair).Left.(*object.Label).Value, variable.VarName)
				}
				if ! parser.IsSameTypeOrSubtype(p.TypeSystem, object.TrueType(v.(*object.Pair).Right), right.(*object.StructDef).Sig[positionOfLabelInFields].VarType) {
					return newError("eval/field/type", tok, v.(*object.Pair).Left.(*object.Label).Value ,
						variable.VarName, right.(*object.StructDef).Sig[positionOfLabelInFields].VarType,
						object.TrueType(v.(*object.Pair).Right))
				} 
				
				
				result.Value[v.(*object.Pair).Left.(*object.Label).Value] =
					v.(*object.Pair).Right
			
			}
			for _, v := range(right.(*object.StructDef).Sig) {
				result.Labels = append(result.Labels, v.VarName)
			}
			result.Name = variable.VarName
			return result
		}

		prsr.BuiltinFunctions[variable.VarName + "_with"] = constructor_2
		// And the second constructor function as it appears in the parser's function table.

		prsr.FunctionTable.Add(prsr.TypeSystem, variable.VarName, // The function name ...
		     ast.Function{Sig: signature.Signature{
		     signature.NameTypePair{VarName: "t",  VarType: "tuple"}}, // ... signature ...
		     Body: &ast.BuiltInExpression{Name: variable.VarName + "_with"} }) // ... and a reference to the built-in as the body

		// Now the labels ...

		for _, v := range(right.(*object.StructDef).Sig) {
			_, ok := prsr.Enums[v.VarName]
			if ok {
				prsr.Throw("eval/struct/enum", tok)
			}
			env.InitializeConstant(v.VarName, &object.Label{Value: v.VarName, Name: "field"})
		}

		return nil
	}
	inferredType := variable.VarType
	if inferredType == "*" { inferredType = object.TrueType(right)}
	switch tok.Type {
	case token.GVN_ASSIGN :
		if !parser.IsSameTypeOrSubtype(prsr.TypeSystem, object.TrueType(right), inferredType) {
			return newError("eval/var/type/a", tok, object.TrueType(right), inferredType)
		}
		env.Set(variable.VarName, right)
		return nil
	case token.PVR_ASSIGN :
		if env.IsConstant(variable.VarName) {
			return newError("eval/var/const/a", tok, variable.VarName)
		}
		if strings.HasPrefix(variable.VarName, "$") {
			return assignSysVar(tok, variable.VarName, right, env)
		}
		if env.Exists(variable.VarName) {
			return newError("eval/var/exists/a", tok, variable.VarName)
		}
		if !parser.IsSameTypeOrSubtype(prsr.TypeSystem, object.TrueType(right), inferredType) {
			return newError("eval/var/type/b", tok, object.TrueType(right), inferredType)
		}
		env.InitializePrivate(variable.VarName, right, inferredType)
		return nil
	case token.DEF_ASSIGN :
		if env.Exists(variable.VarName) {
			return newError("eval/const/assign", tok)
		}
		if !parser.IsSameTypeOrSubtype(prsr.TypeSystem, object.TrueType(right), inferredType) {
			return newError("eval/const/type", tok, object.TrueType(right), inferredType);
		}
		env.InitializeConstant(variable.VarName, right)
		return nil
	case token.VAR_ASSIGN :
		if env.IsConstant(variable.VarName) {
			return newError("eval/var/const/a", tok, variable.VarName)
		}
		if strings.HasPrefix(variable.VarName, "$") {
			return assignSysVar(tok, variable.VarName, right, env)
		}
		if env.Exists(variable.VarName) {
			return newError("eval/var/exists/b", tok, variable.VarName)
		}
		if !parser.IsSameTypeOrSubtype(prsr.TypeSystem, object.TrueType(right), inferredType) {
			return newError("eval/var/type/c", tok, object.TrueType(right), inferredType)
		}
		env.InitializeVariable(variable.VarName, right, inferredType)
		return nil
	case token.CMD_ASSIGN :
		if variable.VarType != "*" {
			return newError("eval/cmd/assign", tok)
		}
		if !env.Exists(variable.VarName) {
			return newError("eval/cmd/var", tok, variable.VarName)
		}
		if env.IsConstant(variable.VarName) {
			return newError("eval/cmd/const", tok, variable.VarName)
		}
		if strings.HasPrefix(variable.VarName, "$") {
			return assignSysVar(tok, variable.VarName, right, env)
		}
		if !parser.IsSameTypeOrSubtype(prsr.TypeSystem, object.TrueType(right), inferredType) {
			return newError("eval/cmd/type", tok, object.TrueType(right) , env.Store[variable.VarName].VarType)
		}
		env.UpdateVar(variable.VarName, right) // Unlike Set, this will work through linked environments.
											   // I can't remember why that seemed necessary.
		return nil
	default : // We must be assigning from the REPL.
		if (!env.Exists(variable.VarName)) || env.IsPrivate(variable.VarName) {
			return newError("eval/repl/assign", tok, variable.VarName) 
		}
		if env.IsConstant(variable.VarName) {
			return newError("eval/repl/const", tok, variable.VarName)
		}
		if strings.HasPrefix(variable.VarName, "$") {
			return assignSysVar(tok, variable.VarName, right, env)
		}
		if !parser.IsSameTypeOrSubtype(prsr.TypeSystem, object.TrueType(right), env.Store[variable.VarName].VarType) {
			return newError("eval/repl/type", tok, object.TrueType(right), env.Store[variable.VarName].VarType)
		}
		env.Set(variable.VarName, right)
		return nil
	}
	

}


func assignSysVar(tok token.Token, keyword string, right object.Object, env *object.Environment) *object.Error {
	if _, ok := sysvars.Sysvars[keyword]; ok {
		err := sysvars.Sysvars[keyword].Validator(right)
		if err == "" {
			env.Set(keyword, right)
			return nil
		}
		return newError("eval/sys/valid", tok, err)
	}
	return newError("eval/sys/exists", tok, keyword)
}


func evalNotOperatorExpression(token token.Token, right object.Object, prsr *parser.Parser, env *object.Environment) object.Object {
	truthiness := isTruthy(token, right, prsr, env)
	if truthiness.Type() == object.ERROR_OBJ { 
		truthiness.(*object.Error).Trace = append(truthiness.(*object.Error).Trace, token)
		return truthiness
	}
	if truthiness == object.TRUE {
		return object.FALSE
	}
	return object.TRUE
}


func evalEvalExpression(token token.Token, right object.Object, prsr *parser.Parser, env *object.Environment) object.Object {
	if right.Type() != object.STRING_OBJ {
		return newError("eval/eval", token)
	}
	source := "string evaluated at line " + strconv.Itoa(token.Line) + " of " + token.Source
	parsedCode := prsr.ParseLine(source, right.(*object.String).Value)
	return Evaluate(*parsedCode, prsr, env)
}

func evalReturnExpression(token token.Token, right object.Object) object.Object {
	if right.Type() == object.RETURN_OBJ {
		return newError("eval/return/return", token)
	}
	if right.Type() == object.TUPLE_OBJ {
		return &object.Return{Elements: right.(*object.Tuple).Elements}
	}
	return &object.Return{Elements: []object.Object{right}}
} 

func evalTupleExpression(
	left, right object.Object,
) object.Object {
	switch left.(type) {
	case *object.SuccessfulAssignment :
		return right
	case *object.Tuple:
		switch right.(type) {
		case *object.Tuple:
			return &object.Tuple{Elements: append(left.(*object.Tuple).Elements, right.(*object.Tuple).Elements ...)}
		default:
			return &object.Tuple{Elements: append(left.(*object.Tuple).Elements, right)}
		}	
	default:	
		switch t := right.(type) {
		case *object.SuccessfulAssignment :
			return left
		case *object.Tuple:
			return &object.Tuple{Elements: append([]object.Object{left}, t.Elements ...)}
		default:
			return &object.Tuple{Elements: []object.Object{left, right}}
		}		
	}
}

func evalIdentifier(node *ast.Identifier, prsr *parser.Parser, env *object.Environment) object.Object {

	val, ok := env.Get(node.Value);
	
	if ((!ok || env.IsPrivate(node.Value)) && node.GetToken().Source == "REPL input") {
		return newError("eval/repl/var", node.Token, node.Value)
	}
	
	if ok {
		return val
	}

	// However, we may be dealing with it as an identifier because it's positionally non-functional.
	// If so, just saying "identifier not found" would be weird.
	if prsr.AllFunctionIdents.Contains(node.Value) {
		return newError("eval/ident/context", node.Token, node.Value)
	}
	return newError("eval/ident/found", node.Token, node.Value)
}

func isTruthy(token token.Token, obj object.Object, prsr *parser.Parser, env *object.Environment) object.Object {
	if obj == object.TRUE || obj == object.FALSE {
		return obj
	}
	return evaluateFunction(token, "bool", []object.Object{obj}, prsr, env)
}


func newError(ident string, token token.Token, args ...any) *object.Error {
	return object.CreateErr(ident, token, args...)
}

func isError(obj object.Object) bool {
	if obj != nil {
		return obj.Type() == object.ERROR_OBJ
	}
	return false
}

func isUnsatisfiedConditional(obj object.Object) bool {
	if obj != nil {
		return obj.Type() == object.UNSATISFIED_OBJ
	}
	return false
}


func evalIndexExpression(token token.Token, left, index object.Object, prsr *parser.Parser, env *object.Environment) object.Object {
	return evaluateFunction(token, "index", []object.Object{index, left}, prsr, env)
}


func applyFunction(f ast.Function, params []object.Object, prsr *parser.Parser, token token.Token, ext *object.Environment) object.Object {
	
	switch f.Body.(type) {
	case *ast.BuiltInExpression :
		funcToApply := prsr.BuiltinFunctions[f.Body.(*ast.BuiltInExpression).Name]
		result := funcToApply(prsr, params...)
		if result.Type() == object.ERROR_OBJ {
			result.(*object.Error).Trace = append(result.(*object.Error).Trace, token)
			result.(*object.Error).Token = token 
			if result.(*object.Error).Message == "" {

				msgCreate, ok := object.ErrorCreatorMap[result.(*object.Error).ErrorId]

				if !ok {
					result.(*object.Error).Message = "Oopsie, can't find errorId " + result.(*object.Error).ErrorId
				} else {
					result.(*object.Error).Message = msgCreate.
					    Message(token, result.(*object.Error).Info...)
				}
			}
		}
		return result
	default :
		env := object.NewEnvironment()
		env.Ext = ext
		if !prsr.ParamsFitSig(f.Sig, params) {
			return newError("eval/sig/lambda", token, params)
		}
		newEnvironment := parser.UpdateEnvironment(f.Sig, params, env)
		if !f.Cmd {
			newEnvironment.Set("this", &object.Func{Function: f, Env: env})
		}
		if f.Given != nil {
			resultOfGiven := Eval(f.Given, prsr, newEnvironment)
			if resultOfGiven.Type() == object.ERROR_OBJ {
				resultOfGiven.(*object.Error).Trace = append(resultOfGiven.(*object.Error).Trace, token)
				return resultOfGiven
			}
			if resultOfGiven.Type() != object.SUCCESSFUL_OBJ {
				return newError("eval/given/return", token)
			}
		}
		result := Eval(f.Body, prsr, newEnvironment)
		if result.Type() == object.ERROR_OBJ {
			result.(*object.Error).Trace = append(result.(*object.Error).Trace, token)
		}
		if len(f.Rets) > 0 && !prsr.ParamsFitSig(f.Rets, toObjectList(result)) {
			return newError("eval/rets/match", token)
		}
		return result
	}
}

func toObjectList(obj object.Object) []object.Object {
	switch t := obj.(type) {
	case *object.Tuple :
		return t.Elements
	default:
		return []object.Object{obj}
	}
}