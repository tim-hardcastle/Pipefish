package evaluator

import (
	"fmt"
	"strconv"
	"strings"

	"charm/ast"
	"charm/object"
	"charm/text"
	"charm/token"
	"charm/parser"
	"charm/signature"
	"charm/sysvars"
)

var (
	UNSATISFIED = &object.UnsatisfiedConditional{}
	SUCCESS = &object.SuccessfulAssignment{}
)

func Evaluate(node ast.Node, parser *parser.Parser, env *object.Environment) object.Object {
	result := Eval(node, parser, env)
	if result.Type() == object.UNSATISFIED_OBJ {
		return newError(node.GetToken(), "unsatisfied conditional")
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

	case *ast.PrefixExpression:
		right := Eval(node.Right, parser, env)
		if isError(right) && node.Operator != "type" {
			return right
		}
		return evalPrefixExpression(node.Token, node.Operator, right, parser, env)

	case *ast.UnfixExpression:
		return evalUnfixExpression(node.Token, node.Operator, parser, env)

	case *ast.SuffixExpression:
		left := Eval(node.Left, parser, env)
		if isError(left) {
			return left
		}
		return evalSuffixExpression(node.Token, node.Operator, left, parser, env)

	case *ast.AssignmentExpression:
		variables := signature.NamedSignature{}
		if node.Token.Type == token.TYP_ASSIGN {
			variables = append(variables, signature.NameTypePair{node.Left.(*ast.TypeLiteral).Token.Literal, "type"})
		} else {
			variables = parser.RecursivelySlurpSignature(node.Left, "*")
		}
		right := Eval(node.Right, parser, env)
		if isError(right) && node.Token.Type != token.GVN_ASSIGN  {
			return right
		}
		if right.Type() == object.UNSATISFIED_OBJ && node.Token.Type != token.GVN_ASSIGN {
			return newError(node.Token, "unsatisfied conditional")
		}

		lLen := len(variables)

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
				return newError(node.Token, "not enough parameters on right-hand side of assignment")
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
		left := Eval(node.Left, parser, env)
		if isError(left) {
			return left
		}
		if isUnsatisfiedConditional(left) { return newError(node.Token, "unsatisfied conditional") }
		right := Eval(node.Right, parser, env)
		if isError(right) {
			return right
		}
		if isUnsatisfiedConditional(right) { return newError(node.Token, "unsatisfied conditional") }
		return evalInfixExpression(node.Token, node.Operator, left, right, parser, env)

	case *ast.LazyInfixExpression:
		left := Eval(node.Left, parser, env)
		if isError(left) {
			return left
		}
		leftEvaluation := evalLazyLeftExpression(node.Token, node.Operator, left, parser, env)
		if leftEvaluation != nil && leftEvaluation != SUCCESS && 
		/**/ (leftEvaluation.Type() != object.RETURN_OBJ){
			return leftEvaluation
		}
		right := Eval(node.Right, parser, env)
		if isError(right) {               
			return right
		}
		if leftEvaluation != nil && leftEvaluation.Type() == object.RETURN_OBJ {
			if node.Operator != ";" {
				return newError(node.Token, "unexpected return statement")
			}
			if right == SUCCESS { return left }
			if right.Type() == object.RETURN_OBJ {
				left.(*object.Return).Elements = append(left.(*object.Return).Elements, right.(*object.Return).Elements ...)
			return left
			}
			return newError(node.Token, "malformed return expression")
		}
		return evalLazyRightExpression(node.Token, node.Operator, right, parser, env)

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
			return left
		}
		index := Eval(node.Index, parser, env)
		if isError(index) {
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
			return list
		}
		if list.Type() == object.UNSATISFIED_OBJ {
			return newError(node.Token, "unsatisfied conditional")
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
			return list
		}
		if list.Type() == object.UNSATISFIED_OBJ {
			return newError(node.Token, "unsatisfied conditional")
		}
		return object.SetFromSlice([]object.Object{list})

	case *ast.FuncExpression :
		newEnv := object.NewEnvironment()
		for key, value := range env.Store {
			newEnv.Store[key] = value
		  }
		return &object.Func{Function : ast.Function{Sig: node.Sig, Body: node.Body, Given: node.Given}, Env: newEnv}

	case *ast.StructExpression :
		return &object.StructDef{Sig : node.Sig} 
	}
	return newError(token.Token{Line: 0}, "something too weird has happened to supply a meaningful error")
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
	if isUnsatisfiedConditional(left) { return newError(token, "unsatisfied conditional") }
	truthiness := isTruthy(token, left, prsr, env)
	if truthiness.Type() == object.ERROR_OBJ {return truthiness}
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
	if isUnsatisfiedConditional(right) { return newError(token, "unsatisfied conditional") }
	variable, ok := env.Get(operator)
	switch {
	case operator == "not":
		return evalNotOperatorExpression(token, right, prsr, env)
	case operator == "eval":
		return evalEvalExpression(token, right, prsr, env)
	case operator == "return":
		return evalReturnExpression(token, right)
	case prsr.Prefixes.Contains(operator) || prsr.Functions.Contains(operator) || 
	/**/ prsr.Forefixes.Contains(operator) || ok && variable.Type() == object.FUNC_OBJ:
		params := make([]object.Object, 0) 
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
	return newError(token, "unknown operator: " + text.Emph(operator + " <" + object.TrueType(right) + ">"))
}

func evaluateFunction(token token.Token, operator string, params []object.Object, prsr *parser.Parser, env *object.Environment) object.Object {
	f, err := prsr.FindFunction(operator, params)
	if err != "" {
		return newError(token, err)
	}
	if f.Cmd {
		return applyFunction(f, params, prsr, token, env)
	}
	return applyFunction(f, params, prsr, token, prsr.Globals)
}


func evalSuffixExpression(token token.Token, operator string, left object.Object, prsr *parser.Parser, env *object.Environment) object.Object {
	if isUnsatisfiedConditional(left) { return newError(token, "unsatisfied conditional") }
	if prsr.Suffixes.Contains(operator) || prsr.Endfixes.Contains(operator) {
		params := make([]object.Object, 0) 
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
		f, err := prsr.FindFunction(operator, params)
		if err != "" {
			return newError(token, err)
		}
		if f.Cmd {
			return applyFunction(f, params, prsr, token, env)
		}
		return applyFunction(f, params, prsr, token, prsr.Globals)
		
	}
	return newError(token, "unknown operator: <" + object.TrueType(left) + "> " + text.Emph(operator))
}


func evalUnfixExpression(token token.Token, operator string, prsr *parser.Parser, env *object.Environment) object.Object {
	params := make([]object.Object, 0) 
	f, err := prsr.FindFunction(operator, params)
	if err != "" {
		return newError(token, err)
	}
	if f.Cmd {
		return applyFunction(f, params, prsr, token, env)
	}
	return applyFunction(f, params, prsr, token, prsr.Globals)
}


func evalInfixExpression(
	token token.Token,
	operator string,
	left, right object.Object,
	prsr *parser.Parser,
	env *object.Environment,
) object.Object {
	switch {
	case left.Type() == object.UNSATISFIED_OBJ: 
		return newError(token, "unsatisfied conditional")
	case right.Type() == object.UNSATISFIED_OBJ:
		return newError(token, "unsatisfied conditional")
	case operator == "," :
		return evalTupleExpression(left, right)
	case left.Type() == object.ERROR_OBJ:
		return left
	case right.Type() == object.ERROR_OBJ:
		 return right
	case prsr.Infixes.Contains(operator) || prsr.Midfixes.Contains(operator):
		// Either way we start by assembling a parameters list ...
		params := make([]object.Object, 0)
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
		// Otherwise we have an infix function
		f, err := prsr.FindFunction(operator, params)
		if err != "" {
			return newError(token, err)
		}
		if f.Cmd {
			return applyFunction(f, params, prsr, token, env)
		}
		return applyFunction(f, params, prsr, token, prsr.Globals)
	case operator == "==":
		return object.MakeBool(object.Equals(left, right))
	case operator == "!=":
		return object.MakeInverseBool(object.Equals(left, right))
	default:
		return newError(token, "unknown operator: <" + text.Emph(object.TrueType(left) + "> " + operator + " <" + object.TrueType(right) + ">"))
	}
}



func Assign(variable signature.NameTypePair, right object.Object, prsr *parser.Parser, env *object.Environment, tok token.Token) *object.Error {
	if right.Type() == object.UNSATISFIED_OBJ {
		return newError(tok, "unsatisfied conditional")
	}
	if right.Type() == object.STRUCTDEF_OBJ {
		if tok.Type != token.TYP_ASSIGN {
			return newError(tok, "new struct types should be defined in the " + text.Emph("def") + " section")
		}
		// So what we're going to do is add the constructors to the builtins,
		// and add the function name, sig, and body to the parser's
		// table of functions, and add the labels to the environment.

		// Registration of the type as a type and a suffix has already happened so
		// that the parser can parse the declarations properly.

		// The first constructor function ...
		constructor := func(args ...object.Object) object.Object {
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
		/**/ ast.Function{Sig: right.(*object.StructDef).Sig, // ... signature ...
		/**/ Body: &ast.BuiltInExpression{Name: variable.VarName} }) // ... and a reference to the built-in as the body


		
		

		// The second constructor function ...

		constructor_2 := func(args ...object.Object) object.Object {
			result := &object.Struct{Value: make(map[string] object.Object)}
			for k, v := range(args) {
				if k != 0 { // Because the first parameter is bling: "having"
					if v.Type() != object.PAIR_OBJ {
						return newError(tok, "the parameters after " + text.Emph("having") + " should all be pairs")
					}
					if v.(*object.Pair).Left.Type() != object.LABEL_OBJ {
						return newError(tok, "the parameters after " + text.Emph("having") + "should all be pairs of which the first element is the label of a field")
					}
					positionOfLabelInFields := - 1
					for i, w := range(right.(*object.StructDef).Sig) {
						if string(v.(*object.Pair).Left.(*object.Label).Value) == w.VarName {
							positionOfLabelInFields = i
							break
						}
					}
					if positionOfLabelInFields == - 1 {
						return newError(tok, text.Emph(v.(*object.Pair).Left.(*object.Label).Value) + " does not name a field of " + text.Emph(variable.VarName))
					}
					if ! (object.TrueType(v.(*object.Pair).Right) == right.(*object.StructDef).Sig[positionOfLabelInFields].VarType) {
						return newError(tok, "field" + text.Emph(v.(*object.Pair).Left.(*object.Label).Value) + 
						/**/ " of " + variable.VarName + " should be of type " + 
						/**/ text.Emph(right.(*object.StructDef).Sig[positionOfLabelInFields].VarType) +", not " +
						/**/ object.TrueType(v.(*object.Pair).Right))
					} 
					
					
					result.Value[v.(*object.Pair).Left.(*object.Label).Value] =
						v.(*object.Pair).Right
				}
			}
			for _, v := range(right.(*object.StructDef).Sig) {
				result.Labels = append(result.Labels, v.VarName)
			}
			result.Name = variable.VarName
			return result
		}

		prsr.BuiltinFunctions[variable.VarName + " having"] = constructor_2
		// And the second constructor function as it appears in the parser's function table.

		prsr.FunctionTable.Add(prsr.TypeSystem, variable.VarName, // The function name ...
		/**/ ast.Function{Sig: signature.NamedSignature{
		/**/ signature.NameTypePair{VarName: "having",  VarType: "bling"}, 
		/**/ signature.NameTypePair{VarName: "t",  VarType: "tuple"}}, // ... signature ...
		/**/ Body: &ast.BuiltInExpression{Name: variable.VarName + " having"} }) // ... and a reference to the built-in as the body

		// The keyword of the function was added when we did the first constructor, and
		// the world "having" is added to the parser as a forefix when we first make the parser.

		// Now the labels ...

		for _, v := range(right.(*object.StructDef).Sig) {
			env.InitializeConstant(v.VarName, &object.Label{Value: v.VarName})
		}

		return nil
	}
	inferredType := variable.VarType
	if inferredType == "*" { inferredType = object.TrueType(right)}
	switch tok.Type {
	case token.GVN_ASSIGN :
		if !parser.IsSameTypeOrSubtype(prsr.TypeSystem, object.TrueType(right), inferredType) {
			return newError(tok, "attempt to assign object of type " +text.Emph(object.TrueType(right)) + " to variable of type " + text.Emph(inferredType))
		}
		env.Set(variable.VarName, right)
		return nil
	case token.PVR_ASSIGN :
		if env.IsConstant(variable.VarName) {
			return newError(tok, "attempt to reassign to a constant " + text.Emph(variable.VarName) + " in the " + text.Emph("var") + " section")
		}
		if strings.HasPrefix(variable.VarName, "$") {
			return assignSysVar(tok, variable.VarName, right, env)
		}
		if env.Exists(variable.VarName) {
			return newError(tok, "attempt to reassign to a variable " + text.Emph(variable.VarName) + " in the " + text.Emph("var") + " section")
		}
		if !parser.IsSameTypeOrSubtype(prsr.TypeSystem, object.TrueType(right), inferredType) {
			return newError(tok, "attempt to assign object of type " +text.Emph(object.TrueType(right)) + " to variable of type " + text.Emph(inferredType))
		}
		env.InitializePrivate(variable.VarName, right, inferredType)
		return nil
	case token.DEF_ASSIGN :
		if env.Exists(variable.VarName) {
			return newError(tok, "attempt to reassign to a constant in the " + text.Emph("def") + " section")
		}
		if !parser.IsSameTypeOrSubtype(prsr.TypeSystem, object.TrueType(right), inferredType) {
			return newError(tok, "attempt to assign object of type " +text.Emph(object.TrueType(right)) + " to variable of type " + text.Emph(inferredType))
		}
		env.InitializeConstant(variable.VarName, right)
		return nil
	case token.VAR_ASSIGN :
		if env.IsConstant(variable.VarName) {
			return newError(tok, "attempt to reassign to a constant " + text.Emph(variable.VarName) + " in the " + text.Emph("var") + " section")
		}
		if strings.HasPrefix(variable.VarName, "$") {
			return assignSysVar(tok, variable.VarName, right, env)
		}
		if env.Exists(variable.VarName) {
			return newError(tok, "attempt to reassign to a variable " + text.Emph(variable.VarName) + " in the " + text.Emph("var") + " section")
		}
		if !parser.IsSameTypeOrSubtype(prsr.TypeSystem, object.TrueType(right), inferredType) {
			return newError(tok, "attempt to assign object of type " +text.Emph(object.TrueType(right)) + " to variable of type " + text.Emph(inferredType))
		}
		env.InitializeVariable(variable.VarName, right, inferredType)
		return nil
	case token.CMD_ASSIGN :
		if variable.VarType != "*" {
			return newError(tok, "variables cannot be assigned types in the " + text.Emph("cmd") + " section")
		}
		if !env.Exists(variable.VarName) {
			return newError(tok, "attempt to update a non-existent variable " + text.Emph(variable.VarName) + " in the " + text.Emph("cmd") + " section")
		}
		if env.IsConstant(variable.VarName) {
			return newError(tok, "attempt to reassign to a constant " + text.Emph(variable.VarName) + " in the " + text.Emph("cmd") + " section")
		}
		if strings.HasPrefix(variable.VarName, "$") {
			return assignSysVar(tok, variable.VarName, right, env)
		}
		if !parser.IsSameTypeOrSubtype(prsr.TypeSystem, object.TrueType(right), inferredType) {
			return newError(tok, "attempt to assign object of type " +text.Emph(object.TrueType(right)) + " to variable of type " + text.Emph(env.Store[variable.VarName].VarType))
		}
		env.UpdateVar(variable.VarName, right) // Unlike Set, this will work through linked environments.
											   // I can't remember why that seemed necessary.
		return nil
	default : // We must be assigning from the REPL.
		if (!env.Exists(variable.VarName)) || env.IsPrivate(variable.VarName) {
			return newError(tok, "attempt to update a non-existent or private variable " + text.Emph(variable.VarName)) 
		}
		if env.IsConstant(variable.VarName) {
			return newError(tok, "attempt to reassign to a constant " + text.Emph(variable.VarName))
		}
		if strings.HasPrefix(variable.VarName, "$") {
			return assignSysVar(tok, variable.VarName, right, env)
		}
		if !parser.IsSameTypeOrSubtype(prsr.TypeSystem, object.TrueType(right), env.Store[variable.VarName].VarType) {
			return newError(tok, "attempt to assign object of type " +text.Emph(object.TrueType(right)) + " to variable of type " + text.Emph(env.Store[variable.VarName].VarType))
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
		return newError(tok, err)
	}
	return newError(tok, "attempt to assign to a nonexistent system variable " + text.Emph(keyword))
}


func evalNotOperatorExpression(token token.Token, right object.Object, prsr *parser.Parser, env *object.Environment) object.Object {
	truthiness := isTruthy(token, right, prsr, env)
	if truthiness.Type() == object.ERROR_OBJ {return truthiness}
	if truthiness == object.TRUE {
		return object.FALSE
	}
	return object.TRUE
}


func evalEvalExpression(token token.Token, right object.Object, prsr *parser.Parser, env *object.Environment) object.Object {
	if right.Type() != object.STRING_OBJ {
		return &object.Error{Message: text.Emph("eval") + " takes a " + text.Emph("string") + " as a parameter", Token: token}
	}
	source := "string evaluated at line " + strconv.Itoa(token.Line) + " of " + token.Source
	parsedCode := prsr.ParseLine(source, right.(*object.String).Value)
	return Evaluate(*parsedCode, prsr, env)
}

func evalReturnExpression(token token.Token, right object.Object) object.Object {
	if right.Type() == object.RETURN_OBJ {
		return newError(token, "trying to return a return statement")
	}
	if right.Type() == object.TUPLE_OBJ {
		return &object.Return{Elements: right.(*object.Tuple).Elements}
	}
	return &object.Return{Elements: []object.Object{right}}
} 

func evalStructDefExpression(token token.Token, sig signature.NamedSignature) object.Object {
	return &object.StructDef{Sig: sig}
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
		switch right.(type) {
		case *object.SuccessfulAssignment :
			return left
		case *object.Tuple:
			return &object.Tuple{Elements: append([]object.Object{left}, right.(*object.Tuple).Elements ...)}
		default:
			return &object.Tuple{Elements: []object.Object{left, right}}
		}		
	}
}

func evalIdentifier(
	node *ast.Identifier,
	prsr *parser.Parser,
	env *object.Environment,
) object.Object {

	if val, ok := env.Get(node.Value); ok {
		return val
	}
	// However, we may be dealing with it as an identifier because it's positionally non-functional.
	// If so, just saying "identifier not found" would be weird.
	if prsr.AllFunctionIdents.Contains(node.Value) {
		return newError(node.Token, "Charm can't make sense of the function/operator " + text.Emph(node.Value) +
	/**/" in that context")
	}
	return newError(node.Token, "identifier not found: " + text.Emph(node.Value))
}

func isTruthy(token token.Token, obj object.Object, prsr *parser.Parser, env *object.Environment) object.Object {
	if obj == object.TRUE || obj == object.FALSE {
		return obj
	}
	return evaluateFunction(token, "bool", []object.Object{obj}, prsr, env)
}


func newError(token token.Token, format string, a ...interface{}) *object.Error {
	return &object.Error{Token: token, Message: fmt.Sprintf(format, a...)}
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
		funcToApply, _ := prsr.BuiltinFunctions[f.Body.(*ast.BuiltInExpression).Name]
		result := funcToApply(params...)
		if result.Type() == object.ERROR_OBJ {
			result.(*object.Error).Token = token 
		}
		return result
	default :
		env := object.NewEnvironment()
		env.Ext = ext
		newEnvironment := parser.UpdateEnvironment(f.Sig, params, env)
		if !f.Cmd {
			newEnvironment.Set("this", &object.Func{Function: f, Env: env})
		}
		if f.Given != nil {
			resultOfGiven := Eval(f.Given, prsr, newEnvironment)
			if resultOfGiven.Type() == object.ERROR_OBJ {
				return resultOfGiven
			}
			if resultOfGiven.Type() != object.SUCCESSFUL_OBJ {
				return &object.Error{Message: "attempt to return value in " + text.Emph("given") + 
				/**/ " block", Token: token}
			}
		}
		return Eval(f.Body, prsr, newEnvironment)
	}
}
