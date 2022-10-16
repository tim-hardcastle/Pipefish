package evaluator

// Code for handling the embedded golang.

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	"plugin"
	"strconv"
	"strings"

	"charm/ast"
	"charm/object"
	"charm/parser"
	"charm/signature"
	"charm/token"
)

var counter int

type GoHandler struct {
	Prsr        *parser.Parser
	result      string
	plug        *plugin.Plugin
	rawHappened bool
}

func NewGoHandler(prsr *parser.Parser) *GoHandler {

	gh := GoHandler{
		Prsr: prsr,
	}
	return &gh
}

func (gh *GoHandler) CleanUp() {
	os.Remove("golang" + strconv.Itoa(counter) + ".go")
}

func (gh *GoHandler) BuildGo() {

	preface := "package main\n\n"

	appendix := `func tuplify(args ...any) any {
	if len(args) == 1 {
		return args[0]
	}
	result := &object.GoReturn{Elements: []any{}}
	for _, v := range(args) {
		result.Elements = append(result.Elements, v)
	}
	return result
}`

	objectHappened := false

	for _, v := range gh.Prsr.GolangImports {
		if v == "charm/object" {
			objectHappened = true
			break
		}
	}
	if !objectHappened {
		gh.Prsr.GolangImports = append(gh.Prsr.GolangImports, "charm/object")
	}

	if len(gh.Prsr.GolangImports) > 0 {
		preface = preface + "import (\n"
		for _, v := range gh.Prsr.GolangImports {
			preface = preface + "    \"" + v + "\"\n"
		}
		preface = preface + ")\n\n"
	}

	// You can't reuse the names of shared object files.
	counter++
	soFile := "golang" + strconv.Itoa(counter) + ".so"
	goFile := "golang" + strconv.Itoa(counter) + ".go"

	file, _ := os.Create(goFile)
	file.WriteString(preface + gh.result + appendix)
	file.Close()
	var err error
	exec.Command("go", "build", "-buildmode=plugin", "-o", soFile, goFile).Output()
	gh.plug, err = plugin.Open(soFile)
	//os.Remove(goFile)
	os.Remove(soFile)

	if err != nil {
		gh.Prsr.Throw("golang/build", token.Token{})
	}
}

func (gh *GoHandler) MakeFunction(keyword string, sig, rTypes signature.Signature, golang *ast.GolangExpression) {
	gh.result = gh.result + "func " + capitalize(keyword) + "(args ...any) any {\n\n"
	for i, v := range sig {
		ty := ""
		ok := false
		if golang.Raw[i] {
			gh.rawHappened = true
			ty, ok = rawConv[v.VarType]
			if !ok {
				gh.Prsr.Throw("golang/type/a", golang.Token, v.VarType)
				return
			}
		} else {
			ty, ok = typeConv[v.VarType]
			if !ok {
				gh.Prsr.Throw("golang/type/b", golang.Token, v.VarType)
				return
			}
		}
		kludge := ""
		if v.VarType == "int" {
			ty = ty + ")"
			kludge = "int("
		}
		gh.result = gh.result + "    " + v.VarName + " := " + kludge + "args[" + strconv.Itoa(i) + "]" + ty + "\n"
	}

	gh.result = gh.result + doctorReturns(golang.Token.Literal) + "\n\n"
}

func (gh *GoHandler) GetFn(fnName string, tok token.Token) func(args ...any) any {
	name := capitalize(fnName)
	fn, err := gh.plug.Lookup(name)
	if err != nil {
		fmt.Println(err.Error())
		gh.Prsr.Throw("golang/found", tok, name)
		return nil
	}
	fnToReturn := fn.(func(args ...any) any)
	return fnToReturn
}

func (gh *GoHandler) ShowResult() {
	fmt.Println(gh.result)
}

var rawConv = map[string]string{"bling": ".(*object.Bling)",
	"bool":    ".(*object.Boolean)",
	"error":   ".(*object.Error)",
	"float64": ".(*object.Float)",
	"func":    ".(*object.Function)",
	"int":     ".(*object.Integer)",
	"label":   ".(*object.Label)",
	"list":    ".(*object.List)",
	"pair":    ".(*object.Pair)",
	"set":     ".(*object.Set)",
	"single":  "",
	"string":  ".(*object.String)",
	"tuple":   ".(*object.Tuple)",
	"type":    ".(*object.Type)",
}

var typeConv = map[string]string{"bling": ".(string)",
	"bool":    ".(bool)",
	"error":   ".(error)",
	"float64": ".(float64)",
	"func":    ".(func(args ...any) any)",
	"int":     ".(int)",
	"label":   ".(string)",
	"list":    ".([]any)",
	"pair":    ".([]any)",
	"set":     ".([]any)",
	"single":  "",
	"string":  ".(string)",
	"tuple":   ".([]any)",
	"type":    ".(string)",
}

func (gh *GoHandler) CharmToGo(ch object.Object) any {
	switch ch := ch.(type) {
	case *object.Bling:
		return ch.Value
	case *object.Boolean:
		return ch.Value
	case *object.Error:
		return errors.New(ch.Message)
	case *object.Func:
		return func(args ...any) any {
			params := []object.Object{}
			for _, v := range args {
				params = append(params, gh.goToCharm(v))
			}
			charmResult := applyFunction(ch.Function, params, gh.Prsr, token.Token{}, &object.Environment{})
			return gh.goToCharm(charmResult)
		}
	case *object.Float:
		return ch.Value
	case *object.Hash:
		return errors.New("passing maps to golang functions is not yet supported")
	case *object.Integer:
		return ch.Value
	case *object.Label:
		return ch.Value
	case *object.List:
		slice := []any{}
		for _, v := range ch.Elements {
			slice = append(slice, gh.CharmToGo(v))
			return slice
		}
	case *object.Pair:
		slice := []any{}
		slice = append(slice, gh.CharmToGo(ch.Left), gh.CharmToGo(ch.Right))
		return slice

	case *object.Set:
		slice := []any{}
		for _, v := range ch.Elements {
			slice = append(slice, gh.CharmToGo(v))
			return slice
		}
	case *object.String:
		return ch.Value
	case *object.Struct:
		returnMap := make(map[string]any)
		for _, v := range ch.Labels {
			returnMap[v] = gh.CharmToGo(ch.Value[v])
		}
	case *object.Tuple:
		slice := []any{}
		for _, v := range ch.Elements {
			slice = append(slice, gh.CharmToGo(v))
			return slice
		}
	case *object.Type:
		return ch.Value
	}
	return errors.New("unable to convert parameter of type <" + object.TrueType(ch) + ">")
}

func (gh *GoHandler) goToCharm(goval any) object.Object {
	switch goval := goval.(type) {
	case object.Object:
		return goval
	case []string:
		returnList := &object.List{Elements: []object.Object{}}
		for _, v := range goval {
			chval := gh.goToCharm(v)
			if chval.Type() == object.ERROR_OBJ {
				return chval
			}
			returnList.Elements = append(returnList.Elements, chval)
		}
		return returnList
	case bool:
		if goval {
			return object.TRUE
		} else {
			return object.FALSE
		}
	case error:
		return &object.Error{Message: goval.Error(), ErrorId: "golang/error"}
	case float64:
		return &object.Float{Value: goval}
	case int:
		return &object.Integer{Value: goval}
	case string:
		return &object.String{Value: goval}
	case *object.GoReturn:
		result := &object.Tuple{Elements: []object.Object{}}
		for _, v := range goval.Elements {
			newObj := gh.goToCharm(v)
			if newObj.Type() == object.ERROR_OBJ {
				return newObj
			}
			result.Elements = append(result.Elements, newObj)
		}
		return result
	}
	return &object.Error{Message: "bad return type from golang", ErrorId: "golang/return"}
}

func capitalize(s string) string {
	return strings.ToUpper(s[0:1]) + s[1:]
}

// Note to self --- this would also pick out "return" in quotes, comments,
// you need to do a better one.
func doctorReturns(body string) string {
	output := ""
	for ix := strings.Index(body, "return "); ix != -1; ix = strings.Index(body, "return ") {

		output = output + body[:ix] + "return tuplify("

		body = body[ix+7:]

		returnBody := ""
		for {
			lineEnd := strings.IndexAny(body, "\n\r")
			ix = lineEnd
			if lineEnd == -1 {
				panic("Tim, you goofed. Lines are meant to have endings.")
			}
			newLine := strings.TrimRight(body[:lineEnd], "\n\r \t")
			if returnBody != "" {
				returnBody = returnBody + "\n"
			}
			returnBody = returnBody + newLine
			// This also is hacky but will work until I can do a simple Go lexer to do it all properly.
			// Or there's probably a library ... ?
			if lastChar := newLine[len(newLine)-1]; !(lastChar == '{' || lastChar == '(' ||
				lastChar == '|' || lastChar == '&' || lastChar == '+' || lastChar == '-' || lastChar == '*' ||
				lastChar == '/' || lastChar == ',' || lastChar == '=' || lastChar == '!' || lastChar == '<' ||
				lastChar == '>' || lastChar == '.') {
				break
			}

		}
		body = body[ix:]
		output = output + returnBody + ")"

	}

	return output + body

}
