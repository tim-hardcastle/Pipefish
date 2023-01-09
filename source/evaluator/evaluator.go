package evaluator

// This is basically your standard tree-walking evaluator, with one or two minor peculiarities.

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"

	"charm/source/ast"
	"charm/source/object"
	"charm/source/parser"
	"charm/source/signature"
	"charm/source/sysvars"
	"charm/source/text"
	"charm/source/token"
)

var (
	UNSATISFIED = &object.UnsatisfiedConditional{}
	SUCCESS     = &object.SuccessfulAssignment{}
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
		return evalPrefixExpression(node.Token, node.Operator, node, parser, env)

	case *ast.UnfixExpression:
		return evalUnfixExpression(node.Token, node.Operator, parser, env)

	case *ast.SuffixExpression:
		return functionCall(parser.FunctionTreeMap[node.Operator], node.Args, parser, node.Token, env)

	case *ast.AssignmentExpression:
		variables := signature.Signature{}
		if node.Token.Type == token.TYP_ASSIGN {
			variables = append(variables, signature.NameTypePair{VarName: node.Left.(*ast.TypeLiteral).Token.Literal, VarType: "type"})
		} else {
			variables = parser.RecursivelySlurpSignature(node.Left, "*")
		}
		lLen := len(variables)
		right := Eval(node.Right, parser, env)
		if isError(right) {
			if node.Token.Type == token.GVN_ASSIGN {
				for i := 0; i < lLen; i++ {
					err := Assign(variables[i], right, parser, env, node.Token)
					if err != nil {
						return err
					}
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

		for i := 0; i < lLen-1; i++ {
			err := Assign(variables[i], right.(*object.Tuple).Elements[i], parser, env, node.Token)
			if err != nil {
				return err
			}
		}
		if lLen == rLen {
			err := Assign(variables[lLen-1], right.(*object.Tuple).Elements[lLen-1], parser, env, node.Token)
			if err != nil {
				return err
			}
			return SUCCESS
		}
		err := Assign(variables[lLen-1], &object.Tuple{Elements: right.(*object.Tuple).Elements[lLen-1:]}, parser, env, node.Token)
		if err != nil {
			return err
		}
		return SUCCESS

	case *ast.InfixExpression:

		if node.GetToken().Type == token.GIVEN { // Then either something has gone very wrong or we
			leftBranch := node.Left // are looking at the given block of an inner function
			switch leftBranch := leftBranch.(type) {
			case *ast.AssignmentExpression:
				rightBranchOfLeftBranch := leftBranch.Right
				switch rightBranchOfLeftBranch := rightBranchOfLeftBranch.(type) {
				case *ast.FuncExpression:
					rightBranchOfLeftBranch.Given = node.Right
				default:
					return newError("eval/given/a", node.GetToken())
				}
			default:
				return newError("eval/given/b", node.GetToken())
			}
			return Eval(leftBranch, parser, env)
		}

		return evalInfixExpression(node, parser, env)

	case *ast.LazyInfixExpression:
		left := Eval(node.Left, parser, env)
		if isError(left) {
			left.(*object.Error).Trace = append(left.(*object.Error).Trace, node.GetToken())
			return left
		}
		leftEvaluation := evalLazyLeftExpression(node.Token, node.Operator, left, parser, env)
		if leftEvaluation != nil && leftEvaluation != SUCCESS &&
			(leftEvaluation.Type() != object.RETURN_OBJ) {
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
			if right == SUCCESS {
				return left
			}
			if right.Type() == object.RETURN_OBJ {
				left.(*object.Return).Elements = append(left.(*object.Return).Elements, right.(*object.Return).Elements...)
				return left
			}
			return newError("eval/malret", node.Token)
		}
		return evalLazyRightExpression(node.Token, node.Operator, right, parser, env)

	case *ast.ExecExpression:
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

	case *ast.LogExpression:
		if parser.Logging {
			logStr := "Log at line " + text.YELLOW + strconv.Itoa(node.Token.Line) + text.RESET
			logTime, _ := env.Get("$logTime")
			if logTime == object.TRUE {
				logStr = logStr + " @ " + text.BLUE + time.Now().Local().String() + text.RESET
			}
			logStr = logStr + ":\n    "
			for i, arg := range node.Args {
				if arg.GetToken().Type == token.AUTOLOG {
					parser.Logging = false
					logStr = logStr + autolog(node, parser, env) + "\n\n"
					parser.Logging = true
					emit(logStr, parser, env, node.GetToken())

					return Eval(node.Code, parser, env)
				}
				if isLiteral(arg) {
					logStr = logStr + (Eval(arg, parser, env).Inspect(object.ViewStdOut) + " ")
				} else {
					parser.Logging = false
					logStr = logStr + arg.String() + " = " + (Eval(arg, parser, env)).Inspect(object.ViewCharmLiteral)
					parser.Logging = true
					if i+1 < len(node.Args) && !isLiteral(node.Args[i+1]) {
						logStr = logStr + "; "
					}
				}
			}
			logStr = logStr + "\n\n"
			emit(logStr, parser, env, node.GetToken())
		}
		return Eval(node.Code, parser, env)

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

	case *ast.FuncExpression:
		return &object.Func{Function: ast.Function{Sig: node.Sig, Body: node.Body, Given: node.Given}, Env: env}

	case *ast.StructExpression:
		return &object.StructDef{Sig: node.Sig}
	case *ast.Bling:
		return &object.Bling{Value: node.Value}
	case *ast.ApplicationExpression:
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
			return newError("eval/apply", node.Token, left)
		}
		params := []object.Object{}
		if right.Type() == object.TUPLE_OBJ {
			params = right.(*object.Tuple).Elements
		} else {
			params = []object.Object{right}
		}
		return applyFunction(left.(*object.Func).Function, params, parser, node.Token, left.(*object.Func).Env)
	case *ast.CodeLiteral:
		return &object.Code{Value: node.Right}
	case *ast.StreamingExpression:
		left := Eval(node.Left, parser, env)
		if isError(left) {
			left.(*object.Error).Trace = append(left.(*object.Error).Trace, node.GetToken())
			return left
		}
		switch node.Token.Type {
		case token.PIPE:
			if node.Right.GetToken().Type == token.IDENT {
				val, ok := env.Get(node.Right.GetToken().Literal)
				if ok {
					if val.Type() == object.FUNC_OBJ {
						return applyFunction(val.(*object.Func).Function, []object.Object{left}, parser, node.Token, env)
					}
				}
			}
			envWithThat := object.NewEnvironment()
			envWithThat.HardSet("that", left)
			envWithThat.Ext = env
			return Eval(node.Right, parser, envWithThat)
		case token.MAP:
			if left.Type() == object.ERROR_OBJ {
				left.(*object.Error).Trace = append(left.(*object.Error).Trace, node.GetToken())
				return left
			}
			if left.Type() != object.LIST_OBJ {
				return newError("eval/map/list", node.Token, left)
			}
			resultList := &object.List{Elements: []object.Object{}}
			if node.Right.GetToken().Type == token.IDENT {
				val, ok := env.Get(node.Right.GetToken().Literal)
				if ok {
					if val.Type() == object.FUNC_OBJ {
						for _, v := range left.(*object.List).Elements {
							result := applyFunction(val.(*object.Func).Function, []object.Object{v}, parser, node.Token, env)
							if result.Type() == object.ERROR_OBJ {
								result.(*object.Error).Trace = append(result.(*object.Error).Trace, node.GetToken())
								return result
							}
							resultList.Elements = append(resultList.Elements, result)
						}
						return resultList
					}
				}
			}
			envWithThat := object.NewEnvironment()
			envWithThat.Ext = env
			for _, v := range left.(*object.List).Elements {
				envWithThat.HardSet("that", v)
				result := Eval(node.Right, parser, envWithThat)
				if result.Type() == object.ERROR_OBJ {
					result.(*object.Error).Trace = append(result.(*object.Error).Trace, node.GetToken())
					return result
				}
				resultList.Elements = append(resultList.Elements, result)
			}
			return resultList
		case token.FILTER:
			if left.Type() == object.ERROR_OBJ {
				left.(*object.Error).Trace = append(left.(*object.Error).Trace, node.GetToken())
				return left
			}
			if left.Type() != object.LIST_OBJ {
				return newError("eval/filter/list", node.Token, left)
			}
			resultList := &object.List{Elements: []object.Object{}}
			if node.Right.GetToken().Type == token.IDENT {
				val, ok := env.Get(node.Right.GetToken().Literal)
				if ok {
					if val.Type() == object.FUNC_OBJ {
						for _, v := range left.(*object.List).Elements {
							result := applyFunction(val.(*object.Func).Function, []object.Object{v}, parser, node.Token, env)
							if result.Type() == object.ERROR_OBJ {
								result.(*object.Error).Trace = append(result.(*object.Error).Trace, node.GetToken())
								return result
							}
							if result.Type() != object.BOOLEAN_OBJ {
								return newError("eval/filter/bool/a", node.Token, result)
							}
							if result.(*object.Boolean) == object.TRUE {
								resultList.Elements = append(resultList.Elements, v)
							}
						}
						return resultList
					}
				}
			}
			envWithThat := object.NewEnvironment()
			envWithThat.Ext = env
			for _, v := range left.(*object.List).Elements {
				envWithThat.HardSet("that", v)
				result := Eval(node.Right, parser, envWithThat)
				if result.Type() == object.ERROR_OBJ {
					result.(*object.Error).Trace = append(result.(*object.Error).Trace, node.GetToken())
					return result
				}
				if result.Type() != object.BOOLEAN_OBJ {
					return newError("eval/filter/bool/b", node.Token, result)
				}
				if result.(*object.Boolean) == object.TRUE {
					resultList.Elements = append(resultList.Elements, v)
				}
			}
			return resultList
		}
	}
	return newError("eval/oops", token.Token{Line: 0})
}

func evalLazyRightExpression(token token.Token, operator string, right object.Object, prsr *parser.Parser, env *object.Environment) object.Object {
	if isUnsatisfiedConditional(right) {
		return UNSATISFIED
	}
	if operator == ":" || operator == ";" {
		return right
	}
	if right.Type() != object.BOOLEAN_OBJ {
		return newError("eval/bool/right", token, right)
	}
	return right
}

func evalLazyLeftExpression(token token.Token, operator string, left object.Object, prsr *parser.Parser, env *object.Environment) object.Object {
	if operator == ";" {
		if isUnsatisfiedConditional(left) {
			return nil
		}
		return left
	}
	if isUnsatisfiedConditional(left) {
		return newError("eval/unsatisfied/g", token)
	}
	if left.Type() != object.BOOLEAN_OBJ {
		return newError("eval/bool/left", token, left)
	}
	switch operator {
	case ":":
		if left == object.FALSE {
			return UNSATISFIED
		}
	case "and":
		if left == object.FALSE {
			return object.FALSE
		}
	case "or":
		if left == object.TRUE {
			return object.TRUE
		}
	}
	return nil
}

func evalPrefixExpression(token token.Token, operator string, node *ast.PrefixExpression, prsr *parser.Parser, env *object.Environment) object.Object {
	variable, ok := env.Get(operator)
	params := []object.Object{}
	switch {
	case operator == "not":
		return evalNotOperatorExpression(token, Eval(node.Right, prsr, env), prsr, env)
	case operator == "eval":
		return evalEvalExpression(token, Eval(node.Right, prsr, env), prsr, env)
	case operator == "return":
		return evalReturnExpression(token, Eval(node.Right, prsr, env))
	case prsr.Prefixes.Contains(operator) || prsr.Functions.Contains(operator) ||
		prsr.Forefixes.Contains(operator) || ok && variable.Type() == object.FUNC_OBJ:

		// We may have ourselves a lambda
		if ok && variable.Type() == object.FUNC_OBJ {
			right := Eval(node.Right, prsr, env)
			if right.Type() == object.TUPLE_OBJ {
				params = right.(*object.Tuple).Elements
			} else {
				params = []object.Object{right}
			}
			if !prsr.ParamsFitSig(variable.(*object.Func).Sig, params) {
				return newError("eval/sig/lambda", token, params)
			}
			return applyFunction(variable.(*object.Func).Function, params, prsr, token, variable.(*object.Func).Env)
		}
		// Otherwise we have a function or prefix, which work the same at this point.
		result := functionCall(prsr.FunctionTreeMap[node.Operator], node.Args, prsr, node.Token, env)
		if result.Type() == object.ERROR_OBJ {
			if operator == "type" {
				return &object.Type{Value: "error"}
			}
		}
		return result
	}
	if token.Source == "REPL input" {
		return newError("eval/repl/a", token, params)
	}
	return newError("eval/unknown/prefix", token, Eval(node.Right, prsr, env))
}

func evaluateFunction(tok token.Token, operator string, params []object.Object, prsr *parser.Parser, env *object.Environment) object.Object {
	f, err := prsr.FindFunction(operator, params, tok.Source == "REPL input")
	if err == "keyword" {
		return newError("eval/keyword/a", tok)
	}
	if err == "sig" {
		if tok.Type == token.LBRACK {
			return newError("eval/sig/index", tok, params)
		} else {
			return newError("eval/sig/a", tok, params)
		}
	}
	if f.Cmd {
		return applyFunction(f, params, prsr, tok, env)
	}
	return applyFunction(f, params, prsr, tok, prsr.Globals)
}

func evalSuffixExpression(token token.Token, operator string, left object.Object, prsr *parser.Parser, env *object.Environment) object.Object {
	params := make([]object.Object, 0)
	if isUnsatisfiedConditional(left) {
		return newError("eval/unsatisfied/i", token)
	}
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

func evalInfixExpression(node *ast.InfixExpression, prsr *parser.Parser, env *object.Environment,
) object.Object {
	if prsr.Infixes.Contains(node.Operator) {
		if node.Operator == "with" {
			params := listArgs(node.Args, prsr, node.Token, env)
			if params[0].Type() == object.TYPE_OBJ {
				constructor, ok := prsr.BuiltinFunctions[params[0].(*object.Type).Value+"_with"]
				if !ok {
					return newError("eval/with/type", node.Token, params[0].(*object.Type).Value)
				}
				params = params[2:]
				return constructor(prsr, params...)
			}
		}
		// Otherwise we have an infix function
		return functionCall(prsr.FunctionTreeMap[node.Operator], node.Args, prsr, node.Token, env)
	}

	left, right := evalLeftRightArgs(node.Args, prsr, node.Token, env)

	switch {

	case left.Type() == object.ERROR_OBJ:
		left.(*object.Error).Trace = append(left.(*object.Error).Trace, node.Token)
		return left
	case right.Type() == object.ERROR_OBJ:
		right.(*object.Error).Trace = append(right.(*object.Error).Trace, node.Token)
		return right
	case left.Type() == object.UNSATISFIED_OBJ:
		return newError("eval/unsatisfied/j", node.Token)
	case right.Type() == object.UNSATISFIED_OBJ:
		return newError("eval/unsatisfied/k", node.Token)
	case node.Operator == ",":
		result := evalTupleExpression(left, right)
		return result
	case node.Operator == "==":
		return object.MakeBool(object.Equals(left, right))
	case node.Operator == "!=":
		return object.MakeInverseBool(object.Equals(left, right))
	default:
		if node.Token.Source == "REPL input" {
			return newError("eval/repl/b", node.Token, listArgs(node.Args, prsr, node.Token, env))
		}
		return newError("eval/unknown/operator", node.Token, left, right)
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
			result := &object.Struct{Value: make(map[string]object.Object)}
			for k, v := range args {
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
				Body: &ast.BuiltInExpression{Name: variable.VarName}}) // ... and a reference to the built-in as the body

		// The second constructor function ...

		constructor_2 := func(p *parser.Parser, args ...object.Object) object.Object {
			result := &object.Struct{Value: make(map[string]object.Object)}
			for _, v := range args {
				if v.Type() != object.PAIR_OBJ {
					return newError("eval/pair", tok)
				}
				if v.(*object.Pair).Left.Type() != object.LABEL_OBJ {
					return newError("eval/label", tok)
				}
				positionOfLabelInFields := -1
				for i, w := range right.(*object.StructDef).Sig {
					if string(v.(*object.Pair).Left.(*object.Label).Value) == w.VarName {
						positionOfLabelInFields = i
						break
					}
				}
				if positionOfLabelInFields == -1 {
					return newError("eval/field/struct", tok, v.(*object.Pair).Left.(*object.Label).Value, variable.VarName)
				}
				if !parser.IsSameTypeOrSubtype(p.TypeSystem, object.TrueType(v.(*object.Pair).Right), right.(*object.StructDef).Sig[positionOfLabelInFields].VarType) {
					return newError("eval/field/type", tok, v.(*object.Pair).Left.(*object.Label).Value,
						variable.VarName, right.(*object.StructDef).Sig[positionOfLabelInFields].VarType,
						v.(*object.Pair).Right)
				}

				result.Value[v.(*object.Pair).Left.(*object.Label).Value] =
					v.(*object.Pair).Right

			}
			for _, v := range right.(*object.StructDef).Sig {
				result.Labels = append(result.Labels, v.VarName)
			}
			result.Name = variable.VarName
			return result
		}

		prsr.BuiltinFunctions[variable.VarName+"_with"] = constructor_2
		// And the second constructor function as it appears in the parser's function table.

		prsr.FunctionTable.Add(prsr.TypeSystem, variable.VarName, // The function name ...
			ast.Function{Sig: signature.Signature{
				signature.NameTypePair{VarName: "t", VarType: "tuple"}}, // ... signature ...
				Body: &ast.BuiltInExpression{Name: variable.VarName + "_with"}}) // ... and a reference to the built-in as the body

		// Now the labels ...

		for _, v := range right.(*object.StructDef).Sig {
			_, ok := prsr.Enums[v.VarName]
			if ok {
				prsr.Throw("eval/struct/enum", tok)
			}
			env.InitializeConstant(v.VarName, &object.Label{Value: v.VarName, Name: "field"})
		}

		return nil
	}
	inferredType := variable.VarType
	if inferredType == "*" {
		inferredType = object.TrueType(right)
	}
	switch tok.Type {
	case token.GVN_ASSIGN:
		if !parser.IsSameTypeOrSubtype(prsr.TypeSystem, object.TrueType(right), inferredType) {
			return newError("eval/var/type/a", tok, right, inferredType)
		}
		env.Set(variable.VarName, right)
		return nil
	case token.PVR_ASSIGN:
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
			return newError("eval/var/type/b", tok, right, inferredType)
		}
		env.InitializePrivate(variable.VarName, right, inferredType)
		return nil
	case token.DEF_ASSIGN:
		if env.Exists(variable.VarName) {
			return newError("eval/const/assign", tok)
		}
		if !parser.IsSameTypeOrSubtype(prsr.TypeSystem, object.TrueType(right), inferredType) {
			return newError("eval/const/type", tok, right, inferredType)
		}
		env.InitializeConstant(variable.VarName, right)
		return nil
	case token.VAR_ASSIGN:
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
			return newError("eval/var/type/c", tok, right, inferredType)
		}
		env.InitializeVariable(variable.VarName, right, inferredType)
		return nil
	case token.CMD_ASSIGN:
		if variable.VarType != "*" && variable.VarType != "varname" {
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
		if variable.VarType == "*" {
			if !parser.IsSameTypeOrSubtype(prsr.TypeSystem, object.TrueType(right), inferredType) && variable.VarType != "varname" {
				return newError("eval/cmd/type", tok, right, env.Store[variable.VarName].VarType)
			}
			env.UpdateVar(variable.VarName, right) // Unlike Set, this will work through linked environments.
			// I can't remember why that seemed necessary.
		} else { // Otherwise we need to retrieve a variable name from it.
			contents, _ := env.Get(variable.VarName)
			if contents.Type() != object.CODE_OBJ {
				return newError("eval/cmd/varname/code", tok, variable.VarName)
			}
			if contents.(*object.Code).Value.GetToken().Type != token.IDENT {
				return newError("eval/cmd/varname/ident", tok, variable.VarName)
			}
			refName := contents.(*object.Code).Value.GetToken().Literal
			if !env.Exists(refName) {
				return newError("eval/cmd/varname/var", tok, refName)
			}
			if env.IsConstant(refName) {
				return newError("eval/cmd/varname/const", tok, refName)
			}
			if strings.HasPrefix(refName, "$") {
				return assignSysVar(tok, refName, right, env)
			}
			contType, _ := env.Type(refName)
			if !parser.IsSameTypeOrSubtype(prsr.TypeSystem, object.TrueType(right), contType) {
				return newError("eval/cmd/varname/type", tok, object.TrueType(right), contType)
			}
			env.UpdateVar(refName, right)
		}
		return nil
	default: // We must be assigning from the REPL.
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
			return newError("eval/repl/type", tok, right, env.Store[variable.VarName].VarType)
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
		return newError(err, tok)
	}
	return newError("eval/sys/exists", tok, keyword)
}

func evalNotOperatorExpression(token token.Token, right object.Object, prsr *parser.Parser, env *object.Environment) object.Object {
	if right.Type() == object.ERROR_OBJ {
		right.(*object.Error).Trace = append(right.(*object.Error).Trace, token)
		return right
	}
	if right.Type() != object.BOOLEAN_OBJ {
		return newError("eval/bool/not", token, right)
	}
	if right == object.TRUE {
		return object.FALSE
	}
	return object.TRUE
}

func evalEvalExpression(token token.Token, right object.Object, prsr *parser.Parser, env *object.Environment) object.Object {
	if right.Type() == object.ERROR_OBJ {
		return right
	}
	if right.Type() == object.CODE_OBJ {
		return Evaluate(right.(*object.Code).Value, prsr, env)
	}
	if right.Type() == object.STRING_OBJ {
		source := "string evaluated at line " + strconv.Itoa(token.Line) + " of " + token.Source
		parsedCode := prsr.ParseLine(source, right.(*object.String).Value)
		return Evaluate(*parsedCode, prsr, env)
	}
	return newError("eval/eval", token)
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
	case *object.SuccessfulAssignment:
		return right
	case *object.Tuple:
		switch right.(type) {
		case *object.Tuple:
			return &object.Tuple{Elements: append(left.(*object.Tuple).Elements, right.(*object.Tuple).Elements...)}
		default:
			return &object.Tuple{Elements: append(left.(*object.Tuple).Elements, right)}
		}
	default:
		switch t := right.(type) {
		case *object.SuccessfulAssignment:
			return left
		case *object.Tuple:
			return &object.Tuple{Elements: append([]object.Object{left}, t.Elements...)}
		default:
			return &object.Tuple{Elements: []object.Object{left, right}}
		}
	}
}

func evalIdentifier(node *ast.Identifier, prsr *parser.Parser, env *object.Environment) object.Object {

	val, ok := env.Get(node.Value)

	if (!ok || env.IsPrivate(node.Value)) && node.GetToken().Source == "REPL input" {
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

type functionTreeWalker struct {
	functionTree *ast.FnTreeNode
	position     *ast.FnTreeNode
	lastWasTuple bool
}

func (ft *functionTreeWalker) followBranch(prsr *parser.Parser, branch string) bool {

	if ft.lastWasTuple && parser.IsSameTypeOrSubtype(prsr.TypeSystem, branch, "tuple") {
		return true
	}

	for _, v := range ft.position.Branch {
		if parser.IsSameTypeOrSubtype(prsr.TypeSystem, branch, v.TypeName) ||
			branch == "code" && v.TypeName == "ast" {
			ft.position = &v.Node
			ft.lastWasTuple = (v.TypeName == "tuple")
			return true
		}
	}
	return false
}

func newFunctionTreeWalker(functionTree *ast.FnTreeNode) *functionTreeWalker {
	return &functionTreeWalker{functionTree: functionTree, position: functionTree, lastWasTuple: false}
}

func tuplify(args []object.Object) object.Object {
	if len(args) == 1 {
		return args[0]
	}
	result := []object.Object{}
	for _, v := range args {
		if v.Type() == object.TUPLE_OBJ {
			result = append(result, v.(*object.Tuple).Elements...)
		} else {
			result = append(result, v)
		}
	}
	if len(result) == 1 {
		return result[0]
	} else {
		return &object.Tuple{Elements: result}
	}
}

// For special cases like the 'not' operator, we need (for now) to evaluate the arguments
// without the treewalker.
func listArgs(args []ast.Node, prsr *parser.Parser, token token.Token, env *object.Environment) []object.Object {
	values := []object.Object{}
	for _, arg := range args {
		newObject := Eval(arg, prsr, env)
		if isUnsatisfiedConditional(newObject) {
			return []object.Object{newError("eval/unsatisfied/h", token)}
		}
		if isError(newObject) {
			return []object.Object{newObject}
		}
		if newObject.Type() == object.TUPLE_OBJ {
			values = append(values, newObject.(*object.Tuple).Elements...)
		} else {
			values = append(values, newObject)
		}
	}
	return values
}

// func evalArgs(args []ast.Node, prsr *parser.Parser, token token.Token, env *object.Environment) object.Object {
// 	fmt.Println(listArgs(args, prsr, token, env))
// 	return tuplify(listArgs(args, prsr, token, env))
// }

func evalLeftRightArgs(args []ast.Node, prsr *parser.Parser, token token.Token, env *object.Environment) (object.Object, object.Object) {
	leftValues := []object.Object{}
	rightValues := []object.Object{}
	blingHappened := false
	for _, arg := range args {
		newObject := Eval(arg, prsr, env)
		if newObject.Type() == object.BLING_OBJ {
			blingHappened = true
			continue
		}

		if isUnsatisfiedConditional(newObject) {
			return newError("eval/unsatisfied/h", token), object.FALSE
		}
		if isError(newObject) {
			return newObject, object.FALSE
		}
		if blingHappened {
			rightValues = append(rightValues, newObject)
		} else {
			leftValues = append(leftValues, newObject)
		}
	}
	return tuplify(leftValues), tuplify(rightValues)
}

func functionCall(functionTree *ast.FnTreeNode, args []ast.Node, prsr *parser.Parser, token token.Token, env *object.Environment) object.Object {

	// We need to evaluate the arguments one by one. If they are tuples, we need to look at
	// the elements of those one by one as we navigate the function tree.

	pos := 0
	values := []object.Object{}
	treeWalker := newFunctionTreeWalker(functionTree)

	var (
		currentObject       object.Object
		currentSingleObject object.Object
	)

	for arg := 0; arg < len(args); {

		astHappening := (len(treeWalker.position.Branch) > 0 && treeWalker.position.Branch[0].TypeName == "ast")

		if currentObject == nil {
			if astHappening {
				currentObject = &object.Code{Value: args[arg]}
			} else {
				currentObject = Eval(args[arg], prsr, env)
			}
		}

		//We may be at the end of a tuple, or at the start of an empty tuple.
		if currentObject.Type() == object.TUPLE_OBJ {
			for currentObject.Type() == object.TUPLE_OBJ &&
				len(currentObject.(*object.Tuple).Elements) == pos {
				arg++
				pos = 0
				if arg == len(args) {
					break
				}
				if astHappening {
					if pos > 0 {
						return newError("eval/ast", token)
					} else {
						currentObject = &object.Code{Value: args[arg]}
					}
				} else {
					currentObject = Eval(args[arg], prsr, env)
				}
			}
		} else {
			if arg > 0 {
				if astHappening {
					currentObject = &object.Code{Value: args[arg]}
				} else {
					currentObject = Eval(args[arg], prsr, env)
				}
			}
		}

		if arg == len(args) {
			break
		}

		// And now if we are looking at a tuple then it is definitely inhabited at position
		// pos.
		if currentObject.Type() == object.TUPLE_OBJ {
			currentSingleObject = currentObject.(*object.Tuple).Elements[pos]
		} else {
			currentSingleObject = currentObject
		}

		if isUnsatisfiedConditional(currentSingleObject) {
			return newError("eval/unsatisfied/h", token)
		}
		if isError(currentSingleObject) {
			currentSingleObject.(*object.Error).Trace = append(currentSingleObject.(*object.Error).Trace, token)
			return currentSingleObject
		}

		values = append(values, currentSingleObject)
		ok := treeWalker.followBranch(prsr, object.TypeOrBling(currentSingleObject))
		if !ok {
			return newError("eval/args/a", token)
		}

		if currentObject.Type() == object.TUPLE_OBJ {
			pos++
		} else {
			arg++
		}
	}

	if len(values) == 0 {
		ok := treeWalker.followBranch(prsr, "tuple")
		if !ok {
			return newError("eval/args/b", token)
		}
	}

	ok := treeWalker.followBranch(prsr, "")
	if !ok {
		return newError("eval/args/c", token)
	}

	return applyFunction(*treeWalker.position.Fn, values, prsr, token, env)
}

func applyFunction(f ast.Function, params []object.Object, prsr *parser.Parser, token token.Token, ext *object.Environment) object.Object {

	switch body := f.Body.(type) {
	case *ast.BuiltInExpression:
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
	case *ast.GolangExpression:
		gh := NewGoHandler(prsr)
		goParams := []any{}
		for i := 0; i < len(body.Sig); i++ {
			switch {
			case body.Raw[i]:
				goParams = append(goParams, params[i])
			case body.Sig[i].VarType == "tuple":
				lastEl := []any{}
				for j := i; j < len(params); j++ {
					lastEl = append(lastEl, gh.CharmToGo(params[j]))
				}
				goParams = append(goParams, lastEl)
				break
			default:
				goParams = append(goParams, gh.CharmToGo(params[i]))
			}
		}
		var result object.Object
		args := getArgs(body.ObjectCode(goParams...))
		if len(args) == 1 {
			result = gh.goToCharm(args[0])
		} else {
			result = &object.Tuple{Elements: []object.Object{}}
			for _, v := range args {
				el := gh.goToCharm(v)
				if el.Type() == object.ERROR_OBJ {
					result = el
					break
				}
				result.(*object.Tuple).Elements = append(result.(*object.Tuple).Elements, el)
			}
		}
		switch result := result.(type) {
		case *object.Error:
			prsr.Throw("eval/golang", f.Body.GetToken(), result.Message)
		}
		return result
	default:
		env := object.NewEnvironment()
		env.Ext = ext

		newEnvironment := parser.UpdateEnvironment(f.Sig, params, env)
		if !f.Cmd {
			newEnvironment.Set("this", &object.Func{Function: f, Env: env})
		}
		if f.Given != nil {
			prsr.Logging = false
			resultOfGiven := Eval(f.Given, prsr, newEnvironment)
			prsr.Logging = true
			if resultOfGiven.Type() == object.ERROR_OBJ {
				resultOfGiven.(*object.Error).Trace = append(resultOfGiven.(*object.Error).Trace, token)
				return resultOfGiven
			}
			if resultOfGiven.Type() != object.SUCCESSFUL_OBJ {
				return newError("eval/given/return", token, resultOfGiven)
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

func getArgs(args ...any) []any {
	return args
}

func toObjectList(obj object.Object) []object.Object {
	switch t := obj.(type) {
	case *object.Tuple:
		return t.Elements
	default:
		return []object.Object{obj}
	}
}

func isLiteral(node ast.Node) bool {
	switch node := node.(type) {
	case *ast.BooleanLiteral, *ast.FloatLiteral, *ast.IntegerLiteral, *ast.StringLiteral:
		return node == node // Yes, Go, you made me do this.
	default:
		return false
	}
}

func autolog(log *ast.LogExpression, prsr *parser.Parser, env *object.Environment) string {
	// If the log expression is an autolog, it will carry some information about
	// the circumstances under which it was generated.

	switch log.LogType {
	case ast.LogStart:
		return ("Function called.")
	case ast.LogIf:
		if log.Code.GetToken().Type == token.ELSE {
			return "The 'else' branch is taken."
		}
		result, story := narrate(log.Code, prsr, env)
		if result {
			return story + ", so the condition is met."
		} else {
			return story + ", so the condition fails."
		}
	case ast.LogReturn:
		if log.Code.GetToken().Type == token.COLON {
			if log.Code.(*ast.LazyInfixExpression).Left.GetToken().Type == token.ELSE {
				return "The 'else' branch is taken. Returning " + niceReturn(log.Code.(*ast.LazyInfixExpression).Right, prsr, env)

			}
			result, story := narrate(log.Code.(*ast.LazyInfixExpression).Left, prsr, env)
			if result {
				return story + ", so the condition is met. Returning " +
					niceReturn(log.Code.(*ast.LazyInfixExpression).Right, prsr, env)
			} else {
				return story + ", so the condition fails."
			}
		} else {
			return "Returning " + niceReturn(log.Code, prsr, env)
		}
	default:
		panic("Tim, you goofed. That was not supposed to happen.")
	}
}

func narrate(conditional ast.Node, prsr *parser.Parser, env *object.Environment) (bool, string) {
	switch conditional := conditional.(type) {
	case *ast.LazyInfixExpression:
		if conditional.Operator == "and" {
			leftResult, leftStory := narrate(conditional.Left, prsr, env)
			if !leftResult {
				return false, leftStory
			}
			rightResult, rightStory := narrate(conditional.Right, prsr, env)
			if rightResult {
				return true, leftStory + " and " + rightStory
			}
			return false, leftStory + ", but " + rightStory
		}
		if conditional.Operator == "or" {
			leftResult, leftStory := narrate(conditional.Left, prsr, env)
			if leftResult {
				return true, leftStory
			}
			rightResult, rightStory := narrate(conditional.Right, prsr, env)
			if rightResult {
				return true, leftStory + ", but " + rightStory
			}
			return false, leftStory + " and " + rightStory
		}
	case *ast.InfixExpression:
		if conditional.Operator == "==" || conditional.Operator == "!=" || conditional.Operator == "<" ||
			conditional.Operator == "<=" || conditional.Operator == ">" || conditional.Operator == ">=" {
			result := Eval(conditional, prsr, env)
			val := result == object.TRUE
			if isLiteral(conditional.Left) && isLiteral(conditional.Right) {
				return val, conditional.String()
			}
			if isLiteral(conditional.Left) {
				return val, conditional.Right.String() + " is " +
					Eval(conditional.Right, prsr, env).Inspect(object.ViewCharmLiteral)
			}
			if isLiteral(conditional.Right) {
				return val, conditional.Left.String() + " is " +
					Eval(conditional.Left, prsr, env).Inspect(object.ViewCharmLiteral)
			}
			leftVal := Eval(conditional.Left, prsr, env)
			rightVal := Eval(conditional.Right, prsr, env)
			if object.Equals(leftVal, rightVal) {
				return val, conditional.Left.String() + " and " + conditional.Right.String() + " are both " +
					leftVal.Inspect(object.ViewCharmLiteral)
			}
			if val {
				return val, conditional.Left.String() + " is " + leftVal.Inspect(object.ViewCharmLiteral) +
					" and " + conditional.Right.String() + " is " + rightVal.Inspect(object.ViewCharmLiteral)
			}
		}
	}
	result := Eval(conditional, prsr, env)
	description := ""
	val := result == object.TRUE
	if val {
		description = "true"
	} else {
		description = "false"
	}
	resultString := conditional.String() + " is " + description
	return val, resultString
}

func niceReturn(node ast.Node, prsr *parser.Parser, env *object.Environment) string {
	if isLiteral(node) {
		return Eval(node, prsr, env).Inspect(object.ViewCharmLiteral) + "."
	}
	return node.String() + " = " +
		Eval(node, prsr, env).Inspect(object.ViewCharmLiteral) + "."
}

func emit(logStr string, prsr *parser.Parser, env *object.Environment, tok token.Token) {
	logPath, _ := env.Get("$logPath")
	logPathStr := logPath.(*object.String).Value
	if logPathStr == "stdout" {
		fmt.Print(logStr)
		return
	}
	f, err := os.OpenFile(logPathStr, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
	if err != nil {
		prsr.Throw("eval/log/file", tok)
	}
	defer f.Close()
	if _, err := f.WriteString(logStr); err != nil {
		prsr.Throw("eval/log/append", tok)
	}
}
