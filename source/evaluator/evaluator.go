package evaluator

// This is basically your standard tree-walking evaluator, with one or two minor peculiarities.

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
	"unicode/utf8"

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
)

// A type for saying where the code comes from --- was it typed into the REPL, is it a cmd, etc.
type Access = int

const (
	REPL Access = iota
	CMD
	DEF
	INIT
	LAMBDA
)

// The evaluator is stateless, which as usual means we're going to be wrapping the state in a struct and passing
// it from function to function like a bong at a frat party. This is the struct.
// Its bill of goods:
// * The parser. Needed for evaluation because it contains the precedences, among other stuff.
// * The environment. Same as with any other evaluator.
// * Access. This exists mainly to enforce, at runtime, the rule that functions can't call commands and as such
// is a kludge that I'm living with until I have a syntax checker. Or given that I have no users, I could have asked
// all none of them politely not to do it.
// * The logging flag records whether we are in fact logging things.
type Context struct {
	prsr    *parser.Parser
	env     *object.Environment
	access  Access
	logging bool
}

func NewContext(p *parser.Parser, e *object.Environment, a Access, log bool) *Context {
	return &Context{prsr: p, env: e, access: a, logging: log}
}

// A tiny kludge. An Unsatisfied Conditional value gets turned into an error if it meets anything other than a ';'/newline operator.
// But what if it doesn't meet anything at all, because you wrote false : 42 in the REPL? Then it still has to be converted into an
// error. This does that.
func Evaluate(node ast.Node, c *Context) object.Object {
	result := Eval(node, c)
	if result.Type() == object.UNSATISFIED_OBJ {
		return newError("eval/unsatisfied/a", node.GetToken())
	}
	return result
}

// And then the main evaluator
func Eval(node ast.Node, c *Context) object.Object {

	switch node := node.(type) {

	case *ast.Expression:
		return Eval(node.Node, c)

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
		return evalPrefixExpression(node, c)

	case *ast.UnfixExpression:
		return evalUnfixExpression(node, c)

	case *ast.SuffixExpression:
		if node.Token.Type == token.EMDASH {
			return makeSnippet(node, c)
		}
		return functionCall(c.prsr.FunctionTreeMap[node.Operator], node.Args, node.Token, c)

	case *ast.LoopExpression:
		return (evalLoopExpression(node, c))

	case *ast.AssignmentExpression:
		variables := signature.Signature{}
		if node.Token.Type == token.TYP_ASSIGN {
			variables = append(variables, signature.NameTypePair{VarName: node.Left.(*ast.TypeLiteral).Token.Literal, VarType: "type"})
		} else {
			variables = c.prsr.RecursivelySlurpSignature(node.Left, "*")
		}
		var right object.Object
		lLen := len(variables)
		if node.Token.Type == token.GVN_ASSIGN {
			if lLen == 1 {
				right = &object.Lazy{Value: node.Right}
				err := Assign(variables[0], right, node.Token, c)
				if err != nil {
					return err
				}
			} else {
				node.Token.Type = token.LZY_ASSIGN
				right = &object.Lazy{Value: node}
				for i := 0; i < lLen; i++ {
					err := Assign(variables[i], right, node.Token, c)
					if err != nil {
						return err
					}
				}
			}
			return object.SUCCESS
		} else {
			right = Eval(node.Right, c)
		}
		if isError(right) {
			if node.Token.Type == token.LZY_ASSIGN {
				for i := 0; i < lLen; i++ {
					err := Assign(variables[i], right, node.Token, c)
					if err != nil {
						return err
					}
				}
				return object.SUCCESS
			} else {
				right.(*object.Error).Trace = append(right.(*object.Error).Trace, node.GetToken())
				return right
			}
		}
		if right.Type() == object.UNSATISFIED_OBJ && node.Token.Type != token.LZY_ASSIGN {
			return newError("eval/unsatisfied/b", node.Token)
		}

		if lLen == 1 {
			err := Assign(variables[0], right, node.Token, c)
			if err == nil {
				return object.SUCCESS
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
			err := Assign(variables[i], right.(*object.Tuple).Elements[i], node.Token, c)
			if err != nil {
				return err
			}
		}
		if lLen == rLen {
			err := Assign(variables[lLen-1], right.(*object.Tuple).Elements[lLen-1], node.Token, c)
			if err != nil {
				return err
			}
			return object.SUCCESS
		}
		err := Assign(variables[lLen-1], &object.Tuple{Elements: right.(*object.Tuple).Elements[lLen-1:]}, node.Token, c)
		if err != nil {
			return err
		}
		return object.SUCCESS

	case *ast.InfixExpression:
		if node.GetToken().Type == token.GIVEN { // Then we are looking at the 'given' block of an inner function.
			leftBranch := node.Args[0]
			switch leftBranch := leftBranch.(type) {
			case *ast.AssignmentExpression:
				rightBranchOfLeftBranch := leftBranch.Right
				switch rightBranchOfLeftBranch := rightBranchOfLeftBranch.(type) {
				case *ast.FuncExpression:
					rightBranchOfLeftBranch.Given = node.Args[2]
				default:
					return newError("eval/given/a", node.GetToken())
				}
			default:
				return newError("eval/given/b", node.GetToken())
			}
			return Eval(leftBranch, c)
		}

		return evalInfixExpression(node, c)

	case *ast.LazyInfixExpression:
		left := Eval(node.Left, c)
		if isError(left) {
			left.(*object.Error).Trace = append(left.(*object.Error).Trace, node.GetToken())
			return left
		}
		leftEvaluation := evalLazyLeftExpression(node.Token, left, c)
		if leftEvaluation != nil && (leftEvaluation.Type() != object.RESPONSE_OBJ) &&
			(leftEvaluation.Type() != object.SUCCESSFUL_OBJ) {
			return leftEvaluation
		}

		if leftEvaluation != nil && leftEvaluation.Type() == object.RESPONSE_OBJ {
			if node.Operator != ";" {
				return newError("eval/return", node.Token)
			}
			if leftEvaluation.(*object.Effects).ElseSeeking && node.Right.GetToken().Type == token.COLON {
				if node.Right.(*ast.LazyInfixExpression).Left.GetToken().Type == token.ELSE {
					leftEvaluation.(*object.Effects).ElseSeeking = false
				}
				return leftEvaluation
			}
		}

		right := Eval(node.Right, c)

		if isError(right) {
			right.(*object.Error).Trace = append(right.(*object.Error).Trace, node.GetToken())
			return right
		}

		if left.Type() == object.SUCCESSFUL_OBJ {
			return right
		}

		if node.Operator == ":" && right == object.SUCCESS {
			return &object.Effects{ElseSeeking: !(node.Left.GetToken().Type == token.ELSE)}
		}

		if node.Operator == ":" && right.Type() == object.RESPONSE_OBJ {
			right.(*object.Effects).ElseSeeking = !(node.Left.GetToken().Type == token.ELSE)
		}

		if leftEvaluation != nil && leftEvaluation.Type() == object.RESPONSE_OBJ {
			if right == object.SUCCESS {
				return left
			}
			if right.Type() == object.RESPONSE_OBJ {
				return combineEffects(left.(*object.Effects), right.(*object.Effects))
			}
			return newError("eval/malret", node.Token)
		}
		return evalLazyRightExpression(node.Token, right, c)

	case *ast.Identifier:
		// We may have reached a bit of orphaned endbling. TODO --- well we shouldn't. See also recursivelySlurpSignature for
		// an instance of the same kludge.
		if c.prsr.Endfixes.Contains(node.Value) {
			return &object.Bling{Value: node.Value}
		}
		// Otherwise this is an actual identifier and we should return its value
		return evalIdentifier(node, c)

	case *ast.IndexExpression:
		return evalIndexExpression(node.Token, node.Left, node.Index, c)
	case *ast.ListExpression:
		if node.List == nil {
			return &object.List{Elements: []object.Object{}}
		}
		list := Eval(node.List, c)
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
		if c.logging {
			logStr := "Log at line " + text.YELLOW + strconv.Itoa(node.Token.Line) + text.RESET
			logTime, _ := c.prsr.AllGlobals.Get("$logTime")
			if logTime == object.TRUE {
				logStr = logStr + " @ " + text.BLUE + time.Now().Local().String() + text.RESET
			}
			logStr = logStr + ":\n    "
			for i, arg := range node.Args {
				if arg.GetToken().Type == token.AUTOLOG {
					newContext := &Context{prsr: c.prsr, env: c.env, access: c.access, logging: false}
					logStr = logStr + autolog(node, newContext) + "\n\n"
					emit(logStr, node.GetToken(), c)

					return Eval(node.Code, c)
				}
				if isLiteral(arg) {
					logStr = logStr + (Eval(arg, c).Inspect(object.ViewStdOut) + " ")
				} else {
					newContext := &Context{prsr: c.prsr, env: c.env, access: c.access, logging: false}
					logStr = logStr + arg.String() + " = " + (Eval(arg, newContext)).Inspect(object.ViewCharmLiteral)
					if i+1 < len(node.Args) && !isLiteral(node.Args[i+1]) {
						logStr = logStr + "; "
					}
				}
			}
			logStr = logStr + "\n\n"
			emit(logStr, node.GetToken(), c)
		}
		return Eval(node.Code, c)

	case *ast.SetExpression:
		if node.Set == nil {
			return object.SetFromSlice([]object.Object{})
		}
		list := Eval(node.Set, c)
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
		return &object.Func{Function: ast.Function{Sig: node.Sig, Body: node.Body, Given: node.Given}, Env: c.env}
	case *ast.TryExpression:
		right := Eval(node.Right, c)
		if right.Type() == object.RESPONSE_OBJ {
			right.(*object.Effects).ElseSeeking = true
			return right
		}
		if node.VarName == "" {
			return &object.Effects{ElseSeeking: false}
		}
		if c.env.Exists(node.VarName) && c.env.GetAccess(node.VarName) == object.ACCESS_GLOBAL {
			return newError("eval/try/global", node.Token)
		}
		c.env.Set(node.VarName, right)
		return &object.Effects{ElseSeeking: false}
	case *ast.StructExpression:
		return &object.StructDef{Sig: node.Sig}
	case *ast.Bling:
		return &object.Bling{Value: node.Value}
	case *ast.ApplicationExpression:
		left := Eval(node.Left, c)
		if isError(left) {
			left.(*object.Error).Trace = append(left.(*object.Error).Trace, node.GetToken())
			return left
		}
		right := Eval(node.Right, c)
		if isError(right) {
			right.(*object.Error).Trace = append(right.(*object.Error).Trace, node.GetToken())
			return right
		}
		if !(left.Type() == object.FUNC_OBJ) {
			return newError("eval/apply", node.Token, left)
		}
		var params []object.Object
		if right.Type() == object.TUPLE_OBJ {
			params = right.(*object.Tuple).Elements
		} else {
			params = []object.Object{right}
		}
		left.(*object.Func).Env.Ext = c.env
		newContext := NewContext(c.prsr, left.(*object.Func).Env, c.access, c.logging)
		return applyFunction(left.(*object.Func).Function, params, node.Token, newContext)
	case *ast.CodeLiteral:
		return &object.Code{Value: node.Right, Env: c.env}
	case *ast.Nothing:
		return &object.Tuple{Elements: []object.Object{}}
	case *ast.StreamingExpression:
		left := Eval(node.Left, c)
		if isError(left) {
			left.(*object.Error).Trace = append(left.(*object.Error).Trace, node.GetToken())
			return left
		}
		switch node.Token.Type {
		case token.PIPE:
			if node.Right.GetToken().Type == token.IDENT {
				val, ok := c.env.Get(node.Right.GetToken().Literal)
				if ok {
					if val.Type() == object.LAZY_OBJ {
						val = Eval(val.(*object.Lazy).Value, c)
					}
					if val.Type() == object.FUNC_OBJ {
						newContext := NewContext(c.prsr, val.(*object.Func).Env, c.access, c.logging)
						return applyFunction(val.(*object.Func).Function, []object.Object{left}, node.Token, newContext)
					}
				}
			}
			envWithThat := object.NewEnvironment()
			envWithThat.HardSet("that", left)
			envWithThat.Ext = c.env
			newContext := &Context{prsr: c.prsr, env: envWithThat, access: c.access, logging: c.logging}
			return Eval(node.Right, newContext)
		case token.MAP:
			if left.Type() == object.ERROR_OBJ {
				left.(*object.Error).Trace = append(left.(*object.Error).Trace, node.GetToken())
				return left
			}
			if left.Type() != object.LIST_OBJ {
				return newError("eval/mapping/list", node.Token, left)
			}
			resultList := &object.List{Elements: []object.Object{}}
			if node.Right.GetToken().Type == token.IDENT {
				val, ok := c.env.Get(node.Right.GetToken().Literal)
				if ok {
					if val.Type() == object.LAZY_OBJ {
						val = Eval(val.(*object.Lazy).Value, c)
					}
					if val.Type() == object.FUNC_OBJ {
						for _, v := range left.(*object.List).Elements {
							newContext := NewContext(c.prsr, val.(*object.Func).Env, c.access, c.logging)
							result := applyFunction(val.(*object.Func).Function, []object.Object{v}, node.Token, newContext)
							if result.Type() == object.ERROR_OBJ {
								result.(*object.Error).Trace = append(result.(*object.Error).Trace, node.GetToken())
								return result
							}
							if result.Type() == object.TUPLE_OBJ {
								resultList.Elements = append(resultList.Elements, result.(*object.Tuple).Elements...)
							} else {
								resultList.Elements = append(resultList.Elements, result)
							}
						}
						return resultList
					}
				}
			}
			envWithThat := object.NewEnvironment()
			envWithThat.Ext = c.env
			for _, v := range left.(*object.List).Elements {
				envWithThat.HardSet("that", v)
				newContext := &Context{prsr: c.prsr, env: envWithThat, access: c.access, logging: c.logging}
				result := Eval(node.Right, newContext)
				if result.Type() == object.ERROR_OBJ {
					result.(*object.Error).Trace = append(result.(*object.Error).Trace, node.GetToken())
					return result
				}
				if result.Type() == object.TUPLE_OBJ {
					resultList.Elements = append(resultList.Elements, result.(*object.Tuple).Elements...)
				} else {
					resultList.Elements = append(resultList.Elements, result)
				}
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
				val, ok := c.env.Get(node.Right.GetToken().Literal)
				if ok {
					if val.Type() == object.LAZY_OBJ {
						val = Eval(val.(*object.Lazy).Value, c)
					}
					if val.Type() == object.FUNC_OBJ {
						for _, v := range left.(*object.List).Elements {
							newContext := NewContext(c.prsr, val.(*object.Func).Env, c.access, c.logging)
							result := applyFunction(val.(*object.Func).Function, []object.Object{v}, node.Token, newContext)
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
			envWithThat.Ext = c.env
			for _, v := range left.(*object.List).Elements {
				envWithThat.HardSet("that", v)
				newContext := &Context{prsr: c.prsr, env: envWithThat, access: c.access, logging: c.logging}
				result := Eval(node.Right, newContext)
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

func evalLazyRightExpression(tok token.Token, right object.Object, c *Context) object.Object {
	if isUnsatisfiedConditional(right) {
		return UNSATISFIED
	}
	if tok.Literal == ":" || tok.Literal == ";" {
		return right
	}
	if right.Type() != object.BOOLEAN_OBJ {
		return newError("eval/bool/right", tok, right)
	}
	return right
}

func evalLazyLeftExpression(tok token.Token, left object.Object, c *Context) object.Object {
	if tok.Literal == ";" {
		if isUnsatisfiedConditional(left) {
			return nil
		}
		return left
	}
	if isUnsatisfiedConditional(left) {
		return newError("eval/unsatisfied/g", tok)
	}
	if left.Type() != object.BOOLEAN_OBJ {
		return newError("eval/bool/left", tok, left)
	}
	switch tok.Literal {
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

func combineEffects(left, right *object.Effects) *object.Effects {
	left.Elements = append(left.Elements, right.Elements...)
	left.BreakHappened = left.BreakHappened || right.BreakHappened
	left.StopHappened = left.StopHappened || right.StopHappened
	left.ElseSeeking = left.ElseSeeking || right.ElseSeeking
	return left
}

func makeSnippet(node *ast.SuffixExpression, c *Context) object.Object {
	if len(node.Args) != 1 {
		return newError("eval/snippet/params", node.Token)
	}
	name := Eval(node.Args[0], c)
	switch name := name.(type) {
	case *object.Type:
		if !parser.IsSameTypeOrSubtype(c.prsr.TypeSystem, name.Value, "snippet") {
			return newError("eval/snippet/snippet", node.Args[0].GetToken())
		}
		flatEnv := object.Hash{Pairs: make(map[object.HashKey]object.HashPair)}
		flattenEnviroment(c.env, c.access, &flatEnv)
		labels := []string{"text", "env"}
		values := make(map[string]object.Object)
		values["text"] = &object.String{Value: node.Token.Literal}
		values["env"] = &flatEnv
		return &object.Struct{Name: name.Value, Value: values, Labels: labels}
	default:
		return newError("eval/snippet/type", node.Args[0].GetToken())
	}
}

func flattenEnviroment(e *object.Environment, access Access, h *object.Hash) {
	if e.Ext != nil { // We read from the outside in so that shadowing works right.
		flattenEnviroment(e.Ext, access, h)
	}
	for k, v := range e.Store {
		switch access {
		case DEF, LAMBDA:
			if v.GetAccessType() == object.ACCESS_LOCAL || v.GetAccessType() == object.ACCESS_CONSTANT {
				h.AddStringValuePair(k, v.GetValue())
			}
		case CMD:
			if v.GetAccessType() == object.ACCESS_LOCAL || v.GetAccessType() == object.ACCESS_CONSTANT {
				h.AddStringValuePair(k, v.GetValue())
			}
			if v.GetAccessType() == object.ACCESS_GLOBAL {
				h.AddStringValuePair(k, v.GetValue().DeepCopy())
			}
		case REPL:
			if v.GetAccessType() == object.ACCESS_LOCAL || v.GetAccessType() == object.ACCESS_CONSTANT {
				h.AddStringValuePair(k, v.GetValue())
			}
			if v.GetAccessType() == object.ACCESS_PUBLIC {
				h.AddStringValuePair(k, v.GetValue().DeepCopy())
			}
		case INIT:
			if v.GetAccessType() == object.ACCESS_CONSTANT {
				h.AddStringValuePair(k, v.GetValue())
			}
			if v.GetAccessType() == object.ACCESS_PRIVATE || v.GetAccessType() == object.ACCESS_PUBLIC {
				h.AddStringValuePair(k, v.GetValue().DeepCopy())
			}
		default:
			panic("Tim, you goofed.")
		}
		if e.Pending != nil && (access == CMD || access == REPL) {
			for k, v := range e.Pending {
				h.AddStringValuePair(k, v.DeepCopy())
			}
		}
	}
}

func evalPrefixExpression(node *ast.PrefixExpression, c *Context) object.Object {
	params := []object.Object{}
	tok := node.Token
	operator := node.Token.Literal
	switch {
	case tok.Type == token.NOT:
		return evalNotOperatorExpression(tok, Eval(node.Args[0], c)) // TODO: for consistency, we shouldn't be using Args[0] like this. Doesn't matter that much.
	case tok.Type == token.EVAL: // TODO, but for now if you are going to do this you should throw errors for too many args.
		return evalEvalExpression(tok, Eval(node.Args[0], c), c)
	case tok.Type == token.GLOBAL:
		return evalGlobalExpression(node, c)
	case c.prsr.Prefixes.Contains(operator) || c.prsr.Functions.Contains(operator):
		// We may have a function or prefix, which work the same at this point. TODO --- make one set for both?
		result := functionCall(c.prsr.FunctionTreeMap[node.Operator], node.Args, node.Token, c)
		if result.Type() == object.ERROR_OBJ {
			if operator == "type" {
				return &object.Type{Value: "error"}
			}
		}
		return result

	default: // We could be looking at a lazy object initialized in the `given` section.
		variable, ok := c.env.Get(node.Token.Literal)
		if ok && variable.Type() == object.LAZY_OBJ {
			variable = Eval(variable.(*object.Lazy).Value, c)
			c.env.Set(node.Token.Literal, variable)
		} // The prefix could be a constant/variable containing a lambda.
		if ok && variable.Type() == object.FUNC_OBJ {
			params := listArgs(node.Args, node.Token, c)
			if params[0].Type() == object.ERROR_OBJ {
				return params[0]
			}
			if !c.prsr.ParamsFitSig(variable.(*object.Func).Sig, params) {
				return newError("eval/sig/lambda", tok, params)
			}
			newContext := NewContext(c.prsr, variable.(*object.Func).Env, LAMBDA, c.logging)
			lamdbaResult := applyFunction(variable.(*object.Func).Function, params, tok, newContext)
			return lamdbaResult
		} // ... or it could contain an outer function.
		if ok && variable.Type() == object.OUTER_OBJ {
			return functionCall(c.prsr.FunctionTreeMap[variable.(*object.OuterFunc).Name], node.Args, node.Token, c)
		}
	}
	// TODO --- I think we have different error messages to hide from the end-user whether a private function or variable even
	// exists but this may not work any more.
	if tok.Source == "REPL input" {
		return newError("eval/repl/a", tok, params)
	}
	newContext := NewContext(c.prsr, c.env, c.access, false)
	return newErrorWithVals("eval/unknown/prefix", tok, listArgs(node.Args, tok, newContext))
}

func evalUnfixExpression(node *ast.UnfixExpression, c *Context) object.Object {
	if node.Operator == "break" { // TODO --- why did you implement these as unfixes? They are innately keywords.
		if c.access != CMD {
			return newError("eval/break", node.Token)
		}
		return &object.Effects{BreakHappened: true}
	}
	if node.Operator == "stop" {
		if c.access != CMD {
			return newError("eval/stop", node.Token)
		}
		return &object.Effects{StopHappened: true}
	}

	var result object.Object

	for _, branch := range c.prsr.FunctionTreeMap[node.Operator].Branch { // TODO --- this is a pretty vile hack to find which of them is the unfix.}
		if branch.Node.Fn != nil {
			result = applyFunction(*branch.Node.Fn, []object.Object{}, node.Token, c)
		}
	}
	return result
}

func evalInfixExpression(node *ast.InfixExpression, c *Context) object.Object {
	if c.prsr.Infixes.Contains(node.Operator) {
		return functionCall(c.prsr.FunctionTreeMap[node.Operator], node.Args, node.Token, c)
	}

	left, right := evalLeftRightArgs(node.Args, node.Token, c)

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
		if object.ConcreteType(left) != object.ConcreteType(right) {
			return newErrorWithVals("eval/equal/a", node.Token, []object.Object{left, right}, object.ConcreteType(left), object.ConcreteType(right))
		}
		return object.MakeBool(object.Equals(left, right))
	case node.Operator == "!=":
		if object.ConcreteType(left) != object.ConcreteType(right) {
			return newErrorWithVals("eval/equal/b", node.Token, []object.Object{left, right}, object.ConcreteType(left), object.ConcreteType(right))
		}
		return object.MakeInverseBool(object.Equals(left, right))
	default:
		if node.Token.Source == "REPL input" {
			return newError("eval/repl/b", node.Token, listArgs(node.Args, node.Token, c))
		}
		return newError("eval/unknown/operator", node.Token, left, right)
	}
}

// We implement the = operator. This goes on for a bit, because it has complicated semantics which
// have been refactored a bunch of times, so here we are.
func Assign(variable signature.NameTypePair, right object.Object, tok token.Token, c *Context) *object.Error {
	if right.Type() == object.UNSATISFIED_OBJ {
		return newError("eval/unsatisfied/l", tok)
	}
	if right.Type() == object.STRUCTDEF_OBJ {
		return AssignStructDef(variable.VarName, right.(*object.StructDef).Sig, tok, c)
	}
	inferredType := variable.VarType
	if inferredType == "*" {
		inferredType = object.ConcreteType(right)
	}
	switch tok.Type {
	case token.GVN_ASSIGN, token.LZY_ASSIGN:
		if !parser.IsObjectInType(c.prsr.TypeSystem, right, inferredType) {
			return newError("eval/var/type/a", tok, right, inferredType)
		}
		c.env.Set(variable.VarName, right)
		return nil
	case token.PVR_ASSIGN:
		if c.env.IsConstant(variable.VarName) {
			return newError("eval/var/const/a", tok, variable.VarName)
		}
		if strings.HasPrefix(variable.VarName, "$") {
			return assignSysVar(tok, variable.VarName, right, c.env)
		}
		if c.env.Exists(variable.VarName) {
			return newError("eval/var/exists/a", tok, variable.VarName)
		}
		if !parser.IsObjectInType(c.prsr.TypeSystem, right, inferredType) {
			return newError("eval/var/type/b", tok, right, inferredType)
		}
		c.env.InitializePrivate(variable.VarName, right, inferredType)
		return nil
	case token.DEF_ASSIGN:
		if c.env.Exists(variable.VarName) {
			return newError("eval/const/assign", tok)
		}
		if !parser.IsObjectInType(c.prsr.TypeSystem, right, inferredType) {
			return newError("eval/const/type", tok, right, inferredType)
		}
		c.env.InitializeConstant(variable.VarName, right)
		return nil
	case token.VAR_ASSIGN:
		if c.env.IsConstant(variable.VarName) {
			return newError("eval/var/const/a", tok, variable.VarName)
		}
		if strings.HasPrefix(variable.VarName, "$") {
			return assignSysVar(tok, variable.VarName, right, c.env)
		}
		if c.env.Exists(variable.VarName) {
			return newError("eval/var/exists/b", tok, variable.VarName)
		}
		if !parser.IsObjectInType(c.prsr.TypeSystem, right, inferredType) {
			return newError("eval/var/type/c", tok, right, inferredType)
		}
		c.env.InitializeVariable(variable.VarName, right, inferredType)
		return nil
	case token.CMD_ASSIGN:
		if variable.VarType != "*" && variable.VarType != "varname" && variable.VarType != "varref" {
			return newError("eval/cmd/assign", tok)
		}
		if c.env.IsConstant(variable.VarName) {
			return newError("eval/cmd/const", tok, variable.VarName)
		}
		if strings.HasPrefix(variable.VarName, "$") {
			return assignSysVar(tok, variable.VarName, right, c.env)
		}
		if !c.env.Exists(variable.VarName) {
			c.env.InitializeLocal(variable.VarName, right, inferredType)
			return nil
		}
		acc := c.env.GetAccess(variable.VarName)
		if acc == object.ACCESS_PRIVATE || acc == object.ACCESS_PUBLIC {
			return newError("eval/cmd/global/a", tok, variable.VarName)
		}
		if variable.VarType != "varname" && variable.VarType != "varref" {
			c.env.Set(variable.VarName, right)
			return nil
		}
		// Otherwise we are dereferencing a <code> object on the LHS of the assignment and we need to retrieve a variable name from it.
		contents, _ := c.env.Get(variable.VarName)
		if contents.Type() != object.CODE_OBJ {
			return newError("eval/cmd/varname/code", tok, variable.VarName)
		}
		if contents.(*object.Code).Value.GetToken().Type != token.IDENT {
			return newError("eval/cmd/varname/ident", tok, variable.VarName)
		}
		refName := contents.(*object.Code).Value.GetToken().Literal
		var envToUse *object.Environment
		if variable.VarType == "varname" {
			envToUse = c.env
		} else {
			envToUse = contents.(*object.Code).Env
		}
		if !envToUse.Exists(refName) {
			envToUse.InitializeLocal(refName, right, object.ConcreteType(right))
			return nil
		}
		if envToUse.IsConstant(refName) {
			return newError("eval/cmd/varname/const", tok, refName)
		}
		if strings.HasPrefix(refName, "$") {
			return assignSysVar(tok, refName, right, envToUse)
		}
		contType, _ := envToUse.Type(refName)
		if !parser.IsObjectInType(c.prsr.TypeSystem, right, contType) {
			return newError("eval/cmd/varname/type", tok, object.ConcreteType(right), contType)
		}
		envToUse.UpdateVar(refName, right)

		return nil
	default: // We must be assigning from the REPL.
		if (!c.env.Exists(variable.VarName)) || c.env.IsPrivate(variable.VarName) {
			return newError("eval/repl/assign", tok, variable.VarName)
		}
		if c.env.IsConstant(variable.VarName) {
			return newError("eval/repl/const", tok, variable.VarName)
		}
		if strings.HasPrefix(variable.VarName, "$") {
			return assignSysVar(tok, variable.VarName, right, c.env)
		}
		if !parser.IsObjectInType(c.prsr.TypeSystem, right, c.env.Store[variable.VarName].VarType) {
			return newError("eval/repl/type", tok, right, c.env.Store[variable.VarName].VarType)
		}
		c.env.Set(variable.VarName, right) // The global variables are the inner environment of the REPL.
		return nil
	}
}

// TODO --- there is no actual reason why this and the thing that calls it has to be in the evaluator, since it only
// happens on initialization.

// We turn the definition of a struct into its constructors and add them to the "builtin" functions, thereby turning
// their very name into a mockery.
func AssignStructDef(structName string, sig signature.Signature, tok token.Token, c *Context) *object.Error {

	// So what we're going to do is add the constructors to the builtins,
	// and add the function name, sig, and body to the parser's
	// table of functions, and add the labels to the environment.

	// Registration of the type as a type and a suffix has already happened so
	// that the parser can parse the declarations properly.

	// The first constructor function ...
	constructor := func(p *parser.Parser, tok token.Token, args ...object.Object) object.Object {
		result := &object.Struct{Value: make(map[string]object.Object)}
		for k, v := range args {
			result.Labels = append(result.Labels, sig[k].VarName)
			result.Value[sig[k].VarName] = v
		}
		result.Name = structName
		return result
	}

	c.prsr.BuiltinFunctions[structName] = constructor

	// And the first constructor function as it appears in the parser's function table.

	c.prsr.FunctionTable.Add(c.prsr.TypeSystem,
		structName, // The function name ...
		ast.Function{Sig: sig, // ... signature ...
			Body: &ast.BuiltInExpression{Name: structName}}) // ... and a reference to the built-in as the body

	// The second constructor function ...

	constructor_2 := func(p *parser.Parser, tok token.Token, args ...object.Object) object.Object {
		result := &object.Struct{Value: make(map[string]object.Object)}
		for _, v := range args {
			if v == object.EMPTY_TUPLE && len(sig) == 0 {
				result.Name = structName
				return result
			}
			if v.Type() != object.PAIR_OBJ {
				return newError("eval/pair", tok)
			}
			if v.(*object.Pair).Left.Type() != object.LABEL_OBJ {
				return newError("eval/label", tok)
			}
			positionOfLabelInFields := -1
			for i, w := range sig {
				if string(v.(*object.Pair).Left.(*object.Label).Value) == w.VarName {
					positionOfLabelInFields = i
					break
				}
			}
			if positionOfLabelInFields == -1 {
				return newError("eval/field/struct", tok, v.(*object.Pair).Left.(*object.Label).Value, structName)
			}
			if !parser.IsObjectInType(p.TypeSystem, v.(*object.Pair).Right, sig[positionOfLabelInFields].VarType) {
				return newError("eval/field/type", tok, v.(*object.Pair).Left.(*object.Label).Value,
					structName, sig[positionOfLabelInFields].VarType,
					v.(*object.Pair).Right)
			}

			result.Value[v.(*object.Pair).Left.(*object.Label).Value] =
				v.(*object.Pair).Right

		}
		for _, v := range sig {
			result.Labels = append(result.Labels, v.VarName)
		}
		result.Name = structName
		return result
	}

	c.prsr.BuiltinFunctions[structName+"_with"] = constructor_2
	// And the second constructor function as it appears in the parser's function table.

	c.prsr.FunctionTable.Add(c.prsr.TypeSystem, structName, // The function name ...
		ast.Function{Sig: signature.Signature{
			signature.NameTypePair{VarName: "t", VarType: "tuple"}}, // ... signature ...
			Body: &ast.BuiltInExpression{Name: structName + "_with"}}) // ... and a reference to the built-in as the body

	// Now the labels ...

	for _, v := range sig {
		_, ok := c.prsr.Enums[v.VarName]
		if ok {
			c.prsr.Throw("eval/struct/enum", tok)
		}
		label := &object.Label{Value: v.VarName, Name: "field"}
		c.env.InitializeConstant(v.VarName, label)
	}
	c.prsr.StructSig[structName] = sig

	return nil
}

func assignSysVar(tok token.Token, varName string, right object.Object, env *object.Environment) *object.Error {
	if _, ok := sysvars.Sysvars[varName]; ok {
		err := sysvars.Sysvars[varName].Validator(right)
		if err == "" {
			env.Set(varName, right)
			return nil
		}
		return newError(err, tok)
	}
	return newError("eval/sv/exists", tok, varName)
}

func evalNotOperatorExpression(token token.Token, right object.Object) object.Object {
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

func evalEvalExpression(token token.Token, right object.Object, c *Context) object.Object {
	if right.Type() == object.ERROR_OBJ {
		return right
	}
	if right.Type() == object.CODE_OBJ {
		return Evaluate(right.(*object.Code).Value, c)
	}
	if right.Type() == object.STRING_OBJ {
		source := "string evaluated at line " + strconv.Itoa(token.Line) + " of " + token.Source
		parsedCode := c.prsr.ParseLine(source, right.(*object.String).Value)
		return Evaluate(*parsedCode, c)
	}
	return newError("eval/eval", token)
}

func evalGlobalExpression(node *ast.PrefixExpression, c *Context) object.Object {
	for _, v := range node.Args {
		varName := v.GetToken().Literal
		if v.GetToken().Type != token.IDENT {
			return newError("eval/global/ident", node.GetToken())
		}
		val, ok := c.env.Get(varName)
		if ok {
			ty, _ := c.env.Type(varName)
			c.env.ImportGlobal(varName, val, ty)
		} else {
			return newError("eval/global/exists", node.GetToken(), varName)
		}
	}
	return object.SUCCESS
}

func evalLoopExpression(loopNode *ast.LoopExpression, c *Context) object.Object {
	result := Eval(loopNode.Code, c)
	switch result := result.(type) {
	case *object.Error:
		result.Trace = append(result.Trace, loopNode.Token)
		return result
	case *object.SuccessfulAssignment, *object.UnsatisfiedConditional:
		return evalLoopExpression(loopNode, c)
	case *object.Effects:
		if result.QuitHappened {
			return result
		}
		if result.BreakHappened {
			result.BreakHappened = false
			return result
		}
		return evalLoopExpression(loopNode, c)

	}
	return newError("eval/loop/value", loopNode.Token)
}

func evalReturnExpression(token token.Token, values []object.Object, c *Context) object.Object {
	for _, v := range values {
		if v.Type() == object.RESPONSE_OBJ {
			return newError("eval/return/return", token)
		}
		if v.Type() == object.ERROR_OBJ {
			v.(*object.Error).Trace = append(v.(*object.Error).Trace, token)
			return v
		}
	}
	c.prsr.EffHandle.OutHandle.Out(values, c.env)
	result := &object.Effects{Elements: values}
	return result
}

func evalTupleExpression(
	left, right object.Object,
) object.Object {
	switch left.(type) {
	case *object.SuccessfulAssignment: // TODO --- what? Why?!?
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

func evalIdentifier(node *ast.Identifier, c *Context) object.Object {
	val, ok := c.env.Get(node.Value)

	if ok && c.access == CMD {
		acc := c.env.GetAccess(node.Value)
		if acc == object.ACCESS_PUBLIC || acc == object.ACCESS_PRIVATE {
			return newError("eval/cmd/global/b", node.Token, node.Value)
		}
	}

	if ok && !(c.env.IsPrivate(node.Value) && node.GetToken().Source == "REPL input") {
		if val.Type() == object.LAZY_OBJ {
			if val.(*object.Lazy).Value.GetToken().Type == token.LZY_ASSIGN { // Then we have a multiple assignment in a 'given' statement.
				Eval(val.(*object.Lazy).Value, c)
				val, _ := c.env.Get(node.Value)
				return val
			}
			lazyResult := Eval(val.(*object.Lazy).Value, c)
			c.env.Set(node.Value, lazyResult)
			return lazyResult
		}
		return val
	}

	// We may be trying to reify an outer function.
	if c.prsr.Functions.Contains(node.Value) {
		return &object.OuterFunc{Name: node.Value}
	}

	// We don't want someone from the REPL to be able to find out whether (a) a variable with
	// a given name doesn't exist or (b) it does but it's private. So:
	if !ok || c.env.IsPrivate(node.Value) && node.GetToken().Source == "REPL input" {
		return newError("eval/repl/var", node.Token, node.Value)
	}

	// However, we may be dealing with it as an identifier because it's positionally non-functional.
	// If so, just saying "identifier not found" would be weird.
	if c.prsr.AllFunctionIdents.Contains(node.Value) {
		return newError("eval/ident/context", node.Token, node.Value)
	}

	// We can return a regular "not found".
	return newError("eval/ident/found", node.Token, node.Value)
}

func newError(ident string, token token.Token, args ...any) *object.Error {
	return object.CreateErr(ident, token, args...)
}

func newErrorWithVals(ident string, token token.Token, vals []object.Object, args ...any) *object.Error {
	return object.CreateErrWithVals(ident, token, vals, args...)
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

func evalIndexExpression(tok token.Token, left, indexNode ast.Node, c *Context) object.Object {
	index := Eval(indexNode, c)
	if index.Type() == object.ERROR_OBJ {
		return index
	}

	container := Eval(left, c)

	if container.Type() == object.HASH_OBJ {
		hashObject := container.(*object.Hash)
		key, ok := index.(object.Hashable)
		if !ok {
			return newError("eval/map/hashable", tok, object.ConcreteType(index))
		}
		pair, ok := hashObject.Pairs[key.HashKey()]
		if !ok {
			return newError("eval/map/key", tok, index)
		}
		return pair.Value
	}

	switch index := index.(type) {
	case *object.Integer:
		idx := index.Value
		switch container := container.(type) {
		case *object.List:
			if idx < 0 || idx > len(container.Elements)-1 {
				return newError("eval/range/index/list", tok, idx, len(container.Elements))
			}
			return container.Elements[idx]
		case *object.Tuple:
			if idx < 0 || idx > len(container.Elements)-1 {
				return newError("eval/range/index/tuple", tok, idx, len(container.Elements))
			}
			return container.Elements[idx]
		case *object.Pair:
			if idx < 0 || idx > 1 {
				return newError("eval/range/index/pair", tok, idx, 2)
			}
			if idx == 0 {
				return container.Left
			}
			return container.Right
		case *object.String:
			max := utf8.RuneCountInString(container.Value) - 1

			if idx < 0 || idx > max {
				return newError("eval/range/index/string", tok)
			}
			result := object.String{Value: string([]rune(container.Value)[idx])}
			return &result
		case *object.Type:
			if c.prsr.TypeSystem.PointsTo(container.Value, "enum") {
				if idx < 0 || idx >= len(c.prsr.Enums[container.Value]) {
					return newError("eval/range/index/enum", tok, idx, container.Value)
				}
				return c.prsr.Enums[container.Value][idx]
			} else {
				return newError("eval/index/type", tok, container.Value)
			}
		}
	case *object.Pair:
		if index.Left.Type() != object.INTEGER_OBJ {
			return newError("eval/pair/int/left", tok)
		}
		if index.Right.Type() != object.INTEGER_OBJ {
			return newError("eval/pair/int/right", tok)
		}
		idx := index.Left.(*object.Integer).Value
		idy := index.Right.(*object.Integer).Value
		switch container := container.(type) {
		case *object.List:
			max := len(container.Elements)
			idy2 := idy
			if idy < 0 {
				idy2 = max + idy
			}
			if (idx < 0 || idx > max) || (idy2 < 0 || idy2 > max) || (idy2 < idx) {
				return newError("eval/range/slice/list", tok, idx, idy, max)
			}
			return container.DeepSlice(idx, idy)
		case *object.Tuple:
			max := len(container.Elements)
			idy2 := idy
			if idy < 0 {
				idy2 = max + idy
			}
			if (idx < 0 || idx > max) || (idy2 < 0 || idy2 > max) || (idy2 < idx) {
				return newError("eval/range/slice/tuple", tok, idx, idy, max)
			}
			return container.DeepSlice(idx, idy)
		case *object.String:
			max := len(container.Value)
			idy2 := idy
			if idy < 0 {
				idy2 = max + idy
			}
			if (idx < 0 || idx > max) || (idy2 < 0 || idy2 > max) || (idy2 < idx) {
				return newError("eval/range/slice/string", tok, idx, idy, max)
			}
			return &object.String{Value: container.Value[idx:idy]}
		}
	case *object.Label:
		idx := index.Value
		switch container := container.(type) {
		case *object.Struct:
			result, ok := container.Value[idx]
			if !ok {
				return newError("eval/struct/field", tok, index, container.Name)
			}
			return result
		case *object.Error:
			if idx == "errorCode" {
				return &object.String{Value: container.ErrorId}
			}
			if idx == "errorMessage" {
				return &object.String{Value: container.Message}
			}
			return container
		}
	}

	if container.Type() == object.ERROR_OBJ {
		return container
	}

	return newError("eval/index/types", tok, container.Type(), index.Type())
}

// This and its methods supply us with a little stateful machine to crawl along the "function tree" of each function
// and see if the series of types/bling it's being fed leads to a function implementation, and if so to what,
// allowing us to do the multiple dispatch.
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
			ft.position = v.Node
			ft.lastWasTuple = (v.TypeName == "tuple")
			return true
		}
	}
	return false
}

func (ft *functionTreeWalker) hasAst() bool {
	return (len(ft.position.Branch) > 0 && ft.position.Branch[0].TypeName == "ast")
}

func (ft *functionTreeWalker) hasNewTuple() bool {
	return (len(ft.position.Branch) > 0 && ft.position.Branch[0].TypeName == "tuple" && !ft.lastWasTuple)
}

func newFunctionTreeWalker(functionTree *ast.FnTreeNode) *functionTreeWalker {
	return &functionTreeWalker{functionTree: functionTree, position: functionTree, lastWasTuple: false}
}

// Given a list of values, if it has length one, this returns that value as a singleton, otherwise it returns a tuple with the
// values as elements, flattening the tuple elements as it goes.
// TODO --- why doesn't this consider the possibility that one of the values is an error?
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
func listArgs(args []ast.Node, tok token.Token, c *Context) []object.Object {
	values := []object.Object{}
	for _, arg := range args {
		newObject := Eval(arg, c)
		if isUnsatisfiedConditional(newObject) {
			return []object.Object{newError("eval/unsatisfied/h", tok)}
		}
		if isError(newObject) {
			newError := newObject.(*object.Error)
			newError.Trace = append(newError.Trace, tok)
			return []object.Object{newError}
		}
		if newObject.Type() == object.TUPLE_OBJ {
			values = append(values, newObject.(*object.Tuple).Elements...)
		} else {
			values = append(values, newObject)
		}
	}
	return values
}

func evalLeftRightArgs(args []ast.Node, tok token.Token, c *Context) (object.Object, object.Object) {
	leftValues := []object.Object{}
	rightValues := []object.Object{}
	blingHappened := false
	for _, arg := range args {
		newObject := Eval(arg, c)
		if newObject.Type() == object.BLING_OBJ {
			blingHappened = true
			continue
		}

		if isUnsatisfiedConditional(newObject) {
			return newError("eval/unsatisfied/h", tok), object.FALSE
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

// We need to walk through the tree of functions according to the types of passed values to do the multiple dispatch,
// we can then pass the values and the body of the function on to applyFunction.
func functionCall(functionTree *ast.FnTreeNode, args []ast.Node, tok token.Token, c *Context) object.Object {

	treeWalker := newFunctionTreeWalker(functionTree)

	// We need to evaluate the arguments one by one. If the resulting objects are tuples, we need to look at
	// the elements of those one by one as we navigate the function tree.
	arg := 0
	var sourceObj object.Object
	var sourceTuple *object.Tuple
	posInSourceTuple := 0
	values := []object.Object{}
	for {
		//We try to get the next single object from the list of args, i.e. if an arg evaluates to a tuple we must take it a bit at a time.
		var singleObj object.Object
		for arg < len(args) { // Then we can try and populate it from the arguments. We use a for loop to skip over the args that evaluate to empty tuples.
			if sourceTuple == nil || posInSourceTuple >= len(sourceTuple.Elements) {
				if sourceTuple != nil { // Then we've gone off the edge of a tuple and must reset things.
					sourceTuple = nil
					posInSourceTuple = 0
					arg = arg + 1
					if arg >= len(args) {
						break
					}
				}
				if treeWalker.hasAst() {
					sourceObj = &object.Code{Value: args[arg], Env: c.env}
				} else {
					sourceObj = Eval(args[arg], c)
				}
				if sourceObj.Type() == object.TUPLE_OBJ { // If it's a tuple but it's empty ...
					if len(sourceObj.(*object.Tuple).Elements) == 0 {
						if treeWalker.hasNewTuple() &&
							(arg < len(args)-1 && args[arg+1].GetToken().Type == token.IDENT &&
								c.prsr.Bling.Contains(args[arg+1].GetToken().Literal)) ||
							arg == len(args)-1 {
							singleObj = object.EMPTY_TUPLE
							arg = arg + 1
							break
						} else { // ... then we go round again and look at the next arg.
							arg = arg + 1
							continue
						}
					} // Otherwise we've found a new populated tuple and can return its first element
					singleObj = sourceObj.(*object.Tuple).Elements[0]
					posInSourceTuple = 1
					sourceTuple = sourceObj.(*object.Tuple)
					break
				}
				singleObj = sourceObj // Or we found a plain old single object
				arg = arg + 1
				break //
			} else { // We're working out way through a populated tuple and haven't reached the end yet
				singleObj = sourceTuple.Elements[posInSourceTuple]
				posInSourceTuple = posInSourceTuple + 1
				break
			}
		}
		// Now we have singleObj, which will be nil if and only if we've come to the end of what
		// the arguments can yield us.

		// We handle exceptional cases.

		if isUnsatisfiedConditional(singleObj) {
			return newError("eval/unsatisfied/h", tok)
		}
		if isError(singleObj) {
			singleObj.(*object.Error).Trace = append(singleObj.(*object.Error).Trace, tok)
			return singleObj
		}

		// And now back to the happy path. We have a valid singleObject, and will use it to navigate the function tree.
		if singleObj != nil {
			// We (attempt to) scooch along the function tree.
			ok := treeWalker.followBranch(c.prsr, object.TypeOrBling(singleObj))
			values = append(values, singleObj)
			if !ok {
				return newErrorWithVals("eval/args/a", tok, listArgs(args, tok, c), values, (arg < len(args)-1) || sourceTuple != nil && posInSourceTuple < len(sourceTuple.Elements)) // The last, boolean value says whether we've seen all the values)
			}
		} else { // singleObj is nil. We have reached the end of the values and must find a function or return an error.
			if len(values) == 0 { // If we have both foo() and foo(t tuple) defined, then foo() takes precedence.
				for _, branch := range treeWalker.position.Branch { // TODO --- this is a pretty vile hack to find which of them takes no params. It would make sense for it to always be at the top.}
					if branch.Node.Fn != nil {
						return applyFunction(*branch.Node.Fn, []object.Object{}, tok, c)
					}
				}
			}

			ok := treeWalker.followBranch(c.prsr, "")
			if ok {
				return applyFunction(*treeWalker.position.Fn, values, tok, c)
			}

			if !treeWalker.lastWasTuple { // Then we might be able to reach a function via the empty tuple.
				ok := treeWalker.followBranch(c.prsr, "tuple")
				if !ok {
					return newErrorWithVals("eval/args/b", tok, listArgs(args, tok, c), values, false)
				}
				ok = treeWalker.followBranch(c.prsr, "")
				if !ok {
					return newErrorWithVals("eval/args/c", tok, listArgs(args, tok, c), values, false)
				}
				values = append(values, object.EMPTY_TUPLE)
				return applyFunction(*treeWalker.position.Fn, values, tok, c)
			}

		}

	}
}

// Having got a Function type out of a lambda or the function tree, we can apply it to the values to get a return value.
func applyFunction(f ast.Function, params []object.Object, tok token.Token, c *Context) object.Object {
	if f.Private && c.access == REPL {
		return newErrorWithVals("eval/repl/private", tok, params, params, false)
	}
	if f.Cmd && c.access == DEF {
		return newError("eval/cmd/function", tok)
	}
	env := object.NewEnvironment()
	var newAccess Access
	if (c.access != INIT) && (c.access != LAMBDA) && !f.Cmd {
		env.Ext = c.prsr.GlobalConstants // function and should drop all the environment except the global constants.
		newAccess = DEF
	} else {
		env.Ext = c.env
		newAccess = c.access
	}
	if f.Cmd {
		newAccess = CMD
	}

	// We punt off the cases where the function body is a builtin or written in Go.
	switch body := f.Body.(type) {
	case *ast.BuiltInExpression:
		newContext := &Context{prsr: c.prsr, logging: c.prsr.Logging, env: env, access: newAccess}
		// First we hijack a few things which can't actually be implemented as builtins but are convenient to
		// treat as such. (Or to put it another way this is a kludge, a shameful kludge. I could at least represent
		// it as a map somewhere. (TODO.))
		if body.Name == "for_loop" {
			return evalForLoop(params, tok, c)
		}
		if body.Name == "get_from_input" {
			return evalInput(params, tok, c)
		}
		if body.Name == "post_to_output" {
			return evalOutput(params, tok, c)
		}
		if body.Name == "post_to_SQL" {
			return evalPostSQL(params, tok, c)
		}
		if body.Name == "get_from_SQL" {
			return evalGetSQL(params, tok, c)
		}
		if body.Name == "post_to_contact" {
			return evalPostContact(params, tok, c)
		}
		if body.Name == "get_from_contact" {
			return evalGetContact(params, tok, c)
		}
		if body.Name == "long_form_constructor" { // Then we actually need a different constructor for each type.
			constructor, ok := c.prsr.BuiltinFunctions[params[0].(*object.Type).Value+"_with"]
			if !ok {
				return newError("eval/with/type", f.Body.GetToken(), params[0].(*object.Type).Value)
			}
			return constructor(c.prsr, tok, (params[2:])...) // TODO --- we're not passing a context to a constructor? But what if we're making something in an inner function?
		}
		return applyBuiltinFunction(f, params, tok, newContext) // Otherwise we can just call the builtin.
	case *ast.GolangExpression:
		newContext := &Context{prsr: c.prsr, logging: c.prsr.Logging, env: env, access: newAccess}
		return applyGolangFunction(body, params, tok, newContext)

	//So if we've got this far we have a regular old function/command/lambda with its body written in Charm.
	default:
		newEnvironment := parser.UpdateEnvironment(f.Sig, params, env)
		if !f.Cmd {
			newEnvironment.InitializeConstant("this", &object.Func{Function: f, Env: env}) // Commands aren't meant to be recursive.
		}
		newContext := &Context{prsr: c.prsr, logging: c.prsr.Logging, env: newEnvironment, access: newAccess}
		if f.Given != nil {
			resultOfGiven := Eval(f.Given, newContext)
			if resultOfGiven.Type() == object.ERROR_OBJ {
				resultOfGiven.(*object.Error).Trace = append(resultOfGiven.(*object.Error).Trace, tok)
				return resultOfGiven
			}
			if resultOfGiven.Type() != object.SUCCESSFUL_OBJ {
				return newError("eval/given/return", tok, resultOfGiven)
			}
		}
		result := Eval(body, newContext)
		if result.Type() == object.ERROR_OBJ {
			result.(*object.Error).Trace = append(result.(*object.Error).Trace, tok)
			return result
		}
		if res := toObjectList(result); len(f.Rets) > 0 && !c.prsr.ParamsFitSig(f.Rets, res) {
			return newErrorWithVals("eval/rets/match", tok, res, res)
		}
		return result
	}
}

func evalInput(params []object.Object, tok token.Token, c *Context) object.Object {
	str := c.prsr.EffHandle.InHandle.Get(params[0].(*object.String).Value)
	return &object.String{Value: str}
}

func evalOutput(params []object.Object, tok token.Token, c *Context) object.Object {
	return evalReturnExpression(tok, params[0:len(params)-2], c)
}

func evalForLoop(params []object.Object, tok token.Token, c *Context) object.Object {
	if params[0].(*object.Code).Value.GetToken().Type != token.IDENT {
		return newError("eval/for/ident", tok)
	}
	refName := params[0].(*object.Code).Value.GetToken().Literal
	functionToApply := params[4].(*object.Func).Function
	// There has to be a less reduplicative way to do this but I'm about to replace the evaluator anyway.
	var val object.Object
	if len(params) > 7 {
		val = &object.Tuple{Elements: params[6:]}
	} else {
		val = params[6]
	}
	var values []object.Object
	switch rng := params[2].(type) {
	case *object.Integer:
		for i := 0; i < rng.Value; i++ {
			c.env.HardSet(refName, &object.Integer{Value: i})
			if val.Type() == object.ERROR_OBJ {
				val.(*object.Error).Trace = append(val.(*object.Error).Trace, tok)
				return val
			}
			if val.Type() == object.TUPLE_OBJ {
				values = val.(*object.Tuple).Elements
			} else {
				values = []object.Object{val}
			}
			c.access = LAMBDA
			val = applyFunction(functionToApply, values, tok, c)
		}
		return val
	case *object.Pair:
		if rng.Left.Type() == object.INTEGER_OBJ && rng.Right.Type() == object.INTEGER_OBJ {
			for i := rng.Left.(*object.Integer).Value; i < rng.Right.(*object.Integer).Value; i++ {
				c.env.HardSet(refName, &object.Integer{Value: i})
				if val.Type() == object.ERROR_OBJ {
					val.(*object.Error).Trace = append(val.(*object.Error).Trace, tok)
					return val
				}
				if val.Type() == object.TUPLE_OBJ {
					values = val.(*object.Tuple).Elements
				} else {
					values = []object.Object{val}
				}
				c.access = LAMBDA
				val = applyFunction(functionToApply, values, tok, c)
			}
			return val
		}
		return newErrorWithVals("eval/for/pair", tok, []object.Object{rng.Left, rng.Right}, rng)
	case *object.Type:
		enum, ok := c.prsr.Enums[rng.Value]
		if !ok {
			return newError("eval/for/type", tok, rng)
		}
		for _, v := range enum {
			c.env.HardSet(refName, v)
			if val.Type() == object.ERROR_OBJ {
				val.(*object.Error).Trace = append(val.(*object.Error).Trace, tok)
				return val
			}
			if val.Type() == object.TUPLE_OBJ {
				values = val.(*object.Tuple).Elements
			} else {
				values = []object.Object{val}
			}
			c.access = LAMBDA
			val = applyFunction(functionToApply, values, tok, c)
		}
		return val
	case *object.List:
		for _, v := range rng.Elements {
			c.env.HardSet(refName, v)
			if val.Type() == object.ERROR_OBJ {
				val.(*object.Error).Trace = append(val.(*object.Error).Trace, tok)
				return val
			}
			if val.Type() == object.TUPLE_OBJ {
				values = val.(*object.Tuple).Elements
			} else {
				values = []object.Object{val}
			}
			c.access = LAMBDA
			val = applyFunction(functionToApply, values, tok, c)
		}
		return val
	default:
		return newErrorWithVals("eval/for/arg", tok, []object.Object{rng}, rng)
	}
}

func evalPostContact(params []object.Object, tok token.Token, c *Context) object.Object {
	result := evalContactExpression(params, tok, c)
	if result.Type() == object.ERROR_OBJ {
		return result
	}
	if result != object.SUCCESS {
		return newError("eval/contact/return", tok)
	}
	return object.SUCCESS
}

func evalGetContact(params []object.Object, tok token.Token, c *Context) object.Object {
	result := evalContactExpression(params, tok, c)
	if result.Type() == object.ERROR_OBJ {
		return result
	}
	if result.Type() != object.RESPONSE_OBJ {
		return newError("eval/contact/response", tok)
	}
	if len(result.(*object.Effects).Elements) == 1 {
		return result.(*object.Effects).Elements[0]
	}
	return &object.Tuple{Elements: result.(*object.Effects).Elements}
}

func evalContactExpression(params []object.Object, tok token.Token, c *Context) object.Object {
	serviceName := params[0].(*object.Struct).Name
	service, ok := c.prsr.Services[serviceName]
	if !ok {
		return newError("eval/contact/service", tok, serviceName)
	}
	otherParser := service.Parser
	oldHandle := service.Parser.EffHandle.OutHandle
	service.Parser.EffHandle.OutHandle = parser.ConsumingOutHandler{}
	contextToUse := NewContext(otherParser, service.Env, REPL, c.logging)
	preparsedExpression, err := preparseContactExpression(params[0].(*object.Struct).Value["text"].(*object.String).Value, tok, c)
	if err != nil {
		return err
	}
	ast := otherParser.ParseLine(tok.Source, preparsedExpression)
	result := Eval(*ast, contextToUse)
	service.Parser.EffHandle.OutHandle = oldHandle
	return result
}

// So, we're going to embed Charm in Charm, and because this is a working prototype we're going to do it ... lexically.
func preparseContactExpression(snippet string, tok token.Token, c *Context) (string, object.Object) {
	outputText := ""
	charmToEvaluate := ""
	readState := ' '
	for _, ch := range snippet {
		if readState == ' ' {
			if ch == '`' || ch == '"' || ch == '\'' || ch == '|' {
				readState = ch
			}
			if ch != '|' {
				outputText = outputText + string(ch)
			}
			continue
		}
		if readState == '|' && ch != '|' {
			charmToEvaluate = charmToEvaluate + string(ch)
			continue
		}
		// Or we have some Charm to parse.
		parsedCharm := c.prsr.ParseLine(tok.Source, charmToEvaluate)
		if c.prsr.ErrorsExist() {
			c.prsr.ClearErrors()
			return "", newError("eval/contact/charm", tok, charmToEvaluate)
		}
		charmValue := Eval(*parsedCharm, c)
		switch charmValue := charmValue.(type) {
		case *object.Error:
			return "", charmValue
		default:
			outputText = outputText + "(" + charmValue.Inspect(object.ViewCharmLiteral) + ")"
		}
	}
	return outputText, nil
}

func applyBuiltinFunction(f ast.Function, params []object.Object, tok token.Token, c *Context) object.Object {
	funcToApply := c.prsr.BuiltinFunctions[f.Body.(*ast.BuiltInExpression).Name]
	values := parser.GetValueList(f.Sig, params)
	result := funcToApply(c.prsr, tok, values...)
	return result
}

func applyGolangFunction(body *ast.GolangExpression, params []object.Object, tok token.Token, c *Context) object.Object {
	gh := NewGoHandler(c.prsr)
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
		c.prsr.Throw("eval/golang", body.GetToken(), result.Message)
	}
	return result
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

// If you use \\ to log without any parameters, this causes an "autolog" where it tries to guess what you would have
// said. TODO --- code doesn't know the names of the functions its in or what the parameters were and so it will take some
// effort to autolog these things when a function is called.
func autolog(log *ast.LogExpression, c *Context) string {
	// If the log expression is an autolog, it will carry some information about
	// the circumstances under which it was generated.

	switch log.LogType {
	case ast.LogStart:
		return ("Function called.")
	case ast.LogIf:
		if log.Code.GetToken().Type == token.ELSE {
			return "The 'else' branch is taken."
		}
		result, story := narrate(log.Code, c)
		if result {
			return story + ", so the condition is met."
		} else {
			return story + ", so the condition fails."
		}
	case ast.LogReturn:
		if log.Code.GetToken().Type == token.COLON {
			if log.Code.(*ast.LazyInfixExpression).Left.GetToken().Type == token.ELSE {
				return "The 'else' branch is taken. Returning " + niceReturn(log.Code.(*ast.LazyInfixExpression).Right, c)

			}
			result, story := narrate(log.Code.(*ast.LazyInfixExpression).Left, c)
			if result {
				return story + ", so the condition is met. Returning " +
					niceReturn(log.Code.(*ast.LazyInfixExpression).Right, c)
			} else {
				return story + ", so the condition fails."
			}
		} else {
			return "Returning " + niceReturn(log.Code, c)
		}
	default:
		panic("Tim, you goofed. That was not supposed to happen.")
	}
}

// Explains why a conditional passed or failed, as part of the autologger.
func narrate(conditional ast.Node, c *Context) (bool, string) {
	switch conditional := conditional.(type) {
	case *ast.LazyInfixExpression:
		if conditional.Operator == "and" {
			leftResult, leftStory := narrate(conditional.Left, c)
			if !leftResult {
				return false, leftStory
			}
			rightResult, rightStory := narrate(conditional.Right, c)
			if rightResult {
				return true, leftStory + " and " + rightStory
			}
			return false, leftStory + ", but " + rightStory
		}
		if conditional.Operator == "or" {
			leftResult, leftStory := narrate(conditional.Left, c)
			if leftResult {
				return true, leftStory
			}
			rightResult, rightStory := narrate(conditional.Right, c)
			if rightResult {
				return true, leftStory + ", but " + rightStory
			}
			return false, leftStory + " and " + rightStory
		}
	case *ast.InfixExpression:
		if conditional.Operator == "==" || conditional.Operator == "!=" || conditional.Operator == "<" ||
			conditional.Operator == "<=" || conditional.Operator == ">" || conditional.Operator == ">=" {
			result := Eval(conditional, c)
			val := result == object.TRUE
			if isLiteral(conditional.Args[0]) && isLiteral(conditional.Args[2]) {
				return val, conditional.String()
			}
			if isLiteral(conditional.Args[0]) {
				return val, conditional.Args[2].String() + " is " +
					Eval(conditional.Args[2], c).Inspect(object.ViewCharmLiteral)
			}
			if isLiteral(conditional.Args[2]) {
				return val, conditional.Args[0].String() + " is " +
					Eval(conditional.Args[0], c).Inspect(object.ViewCharmLiteral)
			}
			leftVal := Eval(conditional.Args[0], c)
			rightVal := Eval(conditional.Args[2], c)
			if object.Equals(leftVal, rightVal) {
				return val, conditional.Args[0].String() + " and " + conditional.Args[2].String() + " are both " +
					leftVal.Inspect(object.ViewCharmLiteral)
			}
			if val {
				return val, conditional.Args[0].String() + " is " + leftVal.Inspect(object.ViewCharmLiteral) +
					" and " + conditional.Args[2].String() + " is " + rightVal.Inspect(object.ViewCharmLiteral)
			}
		}
	}
	result := Eval(conditional, c)
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

// Just a bit of string-handling for the autologger.
func niceReturn(node ast.Node, c *Context) string {
	if isLiteral(node) {
		return Eval(node, c).Inspect(object.ViewCharmLiteral) + "."
	}
	return node.String() + " = " +
		Eval(node, c).Inspect(object.ViewCharmLiteral) + "."
}

// Logs things to the appropriate place
func emit(logStr string, tok token.Token, c *Context) {
	logPath, _ := c.prsr.AllGlobals.Get("$logPath")
	logPathStr := logPath.(*object.String).Value
	if logPathStr == "stdout" {
		fmt.Print(logStr)
		return
	}
	f, err := os.OpenFile(logPathStr, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
	if err != nil {
		c.prsr.Throw("eval/log/file", tok)
	}
	defer f.Close()
	if _, err := f.WriteString(logStr); err != nil {
		c.prsr.Throw("eval/log/append", tok)
	}
}
