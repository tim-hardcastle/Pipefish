package evaluator

// Code for handling the embedded golang.

import (
	"bufio"
	"errors"
	"os"
	"os/exec"
	"plugin"
	"strconv"
	"strings"

	"charm/source/ast"
	"charm/source/object"
	"charm/source/parser"
	"charm/source/signature"
	"charm/source/text"
	"charm/source/token"
)

var counter int

type GoHandler struct {
	Prsr        *parser.Parser
	timeMap     map[string]int
	modules     map[string]string
	plugins     map[string]*plugin.Plugin
	rawHappened bool
}

func NewGoHandler(prsr *parser.Parser) *GoHandler {

	gh := GoHandler{
		Prsr: prsr,
	}

	gh.timeMap = make(map[string]int)
	gh.modules = make(map[string]string)
	gh.plugins = make(map[string]*plugin.Plugin)

	file, err := os.Open("rsc/go/gotimes.dat")
	if err != nil {
		panic("Can't file 'rsc/go/gotimes.dat'.")
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	for i := 0; i < (len(lines) / 2); i++ {
		time, _ := strconv.Atoi(lines[(2*i)+1])
		gh.timeMap[lines[2*i]] = time
	}

	return &gh
}

func (gh *GoHandler) CleanUp() {

	// We add the newly compiled modules to the list of times.

	for k := range gh.modules {
		file, err := os.Stat(k)

		if err != nil {
			panic("Something weird has happened!")
		}

		modifiedTime := file.ModTime().UnixMilli()
		gh.timeMap[k] = int(modifiedTime)
	}

	// And then write out the list of times to the .dat file.
	f, err := os.Create("rsc/go/gotimes.dat")
	if err != nil {
		panic("Can't create file rsc/go/gotimes.dat")
	}
	defer f.Close()
	for k, v := range gh.timeMap {
		f.WriteString(k + "\n")
		f.WriteString(strconv.Itoa(v) + "\n")
	}
}

func (gh *GoHandler) BuildGoMods() {

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

	for source, functionBodies := range gh.modules {

		var modifiedTime int64
		f, err := os.Stat(source)
		if err == nil {
			modifiedTime = f.ModTime().UnixMilli()
		}

		lastChange, ok := gh.timeMap[source]
		if ok {
			if modifiedTime == int64(lastChange) {
				soFile := "rsc/go/" + text.Flatten(source) + "_" + strconv.Itoa(lastChange) + ".so"
				gh.plugins[source], err = plugin.Open(soFile)
				if err == nil { // If there is an error, it can usually be fixed by rebuilding the file, so we can fall through.
					continue
				}
				os.Remove(soFile)
			}
		}

		preface := "package main\n\n"

		objectHappened := false

		// We make sure it imports charm/object exactly once.

		for _, v := range gh.Prsr.GoImports[source] {
			if v == "charm/source/object" {
				objectHappened = true
				break
			}
		}
		if !objectHappened {
			gh.Prsr.GoImports[source] = append(gh.Prsr.GoImports[source], "charm/source/object")
		}

		if len(gh.Prsr.GoImports[source]) > 0 {
			preface = preface + "import (\n"
			for _, v := range gh.Prsr.GoImports[source] {
				preface = preface + "    \"" + v + "\"\n"
			}
			preface = preface + ")\n\n"
		}

		// You can't reuse the names of shared object files.
		counter++
		soFile := "rsc/go/" + text.Flatten(source) + "_" + strconv.Itoa(int(modifiedTime)) + ".so"
		if lastChange != 0 {
			os.Remove("rsc/go/" + text.Flatten(source) + "_" + strconv.Itoa(int(lastChange)) + ".so")
		}
		goFile := "gocode " + strconv.Itoa(counter) + ".go"
		file, _ := os.Create(goFile)
		file.WriteString(preface + functionBodies + appendix)
		file.Close()
		cmd := exec.Command("go", "build", "-buildmode=plugin", "-o", soFile, goFile) // Version to use running from terminal.
		// cmd := exec.Command("go", "build", "-gcflags=all=-N -l", "-buildmode=plugin", "-o", soFile, goFile) // Version to use with debugger.
		output, err := cmd.Output()
		if err != nil {
			gh.Prsr.Throw("golang/build", token.Token{}, err.Error()+": "+string(output))
		}
		gh.plugins[source], err = plugin.Open(soFile)
		if err != nil {
			gh.Prsr.Throw("golang/open", token.Token{}, err.Error())
		} else {
			os.Remove("gocode " + strconv.Itoa(counter) + ".go")
		}
	}
}

func (gh *GoHandler) MakeFunction(keyword string, sig, rTypes signature.Signature, golang *ast.GolangExpression) {

	source := golang.GetToken().Source

	// We check to see whether the source code has been modified.

	_, err := os.Stat(source)

	if err != nil {
		panic("Something weird has happened!")
	}

	// If the source has been modified, we proceed ...

	fnString := "func " + capitalize(keyword) + "(args ...any) any {\n\n"
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
		fnString = fnString + "    " + v.VarName + " := " + kludge + "args[" + strconv.Itoa(i) + "]" + ty + "\n"
	}

	fnString = fnString + doctorReturns(golang.Token.Literal) + "\n\n"

	gh.modules[golang.Token.Source] = gh.modules[golang.Token.Source] + fnString
}

func (gh *GoHandler) AddPureGoBlock(source, code string) {
	gh.modules[source] = gh.modules[source] + "\n" + code[:len(code)-2] + "\n\n"
}

func (gh *GoHandler) GetFn(fnName string, tok token.Token) func(args ...any) any {
	name := capitalize(fnName)
	fn, err := gh.plugins[tok.Source].Lookup(name)
	if err != nil {
		gh.Prsr.Throw("golang/found", tok, name)
		return nil
	}
	fnToReturn := fn.(func(args ...any) any)
	return fnToReturn
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
			charmResult := applyFunction(ch.Function, params, token.Token{}, &Context{env: &object.Environment{}, prsr: gh.Prsr, access: DEF, logging: false})
			return gh.goToCharm(charmResult)
		}
	case *object.Float:
		return ch.Value
	case *object.Hash:
		return errors.New("passing maps to gocodefunctions is not yet supported")
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
	return errors.New("unable to convert parameter of type <" + object.ConcreteType(ch) + ">")
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
		result := object.EMPTY_TUPLE
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
