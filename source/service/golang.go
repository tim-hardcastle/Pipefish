package service

// Code for compiling the embedded golang.

import (
	"bufio"
	"os"
	"os/exec"
	"plugin"
	"strconv"
	"strings"

	"pipefish/source/ast"
	"pipefish/source/dtypes"
	"pipefish/source/parser"
	"pipefish/source/text"
	"pipefish/source/token"
)

var counter int

type GoHandler struct {
	Prsr             *parser.Parser
	timeMap          map[string]int
	Modules          map[string]string
	Plugins          map[string]*plugin.Plugin
	rawHappened      bool
	StructNames      map[string]dtypes.Set[string] // Set of Pipefish structs appearing in the sigs of the functions.
	TypeDeclarations map[string]string             // A string to put the generated source code for declaring stucts in.
}

func NewGoHandler(prsr *parser.Parser) *GoHandler {

	gh := GoHandler{
		Prsr:        prsr,
		StructNames: map[string]dtypes.Set[string]{},
	}

	gh.timeMap = make(map[string]int)
	gh.Modules = make(map[string]string)
	gh.Plugins = make(map[string]*plugin.Plugin)
	gh.StructNames = make(map[string]dtypes.Set[string])
	gh.TypeDeclarations = make(map[string]string)

	file, err := os.Open(gh.Prsr.Directory + "rsc/go/gotimes.dat")
	if err != nil {
		panic("Can't open file 'rsc/go/gotimes.dat'.")
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

	for k := range gh.Modules {
		file, err := os.Stat(k)

		if err != nil {
			panic("Something weird has happened!")
		}

		modifiedTime := file.ModTime().UnixMilli()
		gh.timeMap[k] = int(modifiedTime)
	}

	// And then write out the list of times to the .dat file.
	f, err := os.Create(gh.Prsr.Directory + "rsc/go/gotimes.dat")
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

	// 'tuplify' ensures that just one thing of type any is returned, since this is all the definition of
	// golang functions can cope with.

	appendix := `func tuplify(args ...any) any {
	if len(args) == 1 {
		return args[0]
	}
	result := &values.GoReturn{Elements: []any{}}
	for _, v := range(args) {
		result.Elements = append(result.Elements, v)
	}
	return result
}`

	for source, functionBodies := range gh.Modules {

		var modifiedTime int64
		f, err := os.Stat(source)
		if err == nil {
			modifiedTime = f.ModTime().UnixMilli()
		}

		lastChange, ok := gh.timeMap[source]
		if ok {
			if modifiedTime == int64(lastChange) {
				soFile := gh.Prsr.Directory + "rsc/go/" + text.Flatten(source) + "_" + strconv.Itoa(lastChange) + ".so"
				gh.Plugins[source], err = plugin.Open(soFile)
				if err == nil { // If there is an error, it can usually be fixed by rebuilding the file, so we can fall through.
					continue
				}
				os.Remove(soFile)
			}
		}

		preface := "package main\n\n"

		gh.Prsr.GoImports[source] = append(gh.Prsr.GoImports[source], "pipefish/source/values")

		if len(gh.Prsr.GoImports[source]) > 0 {
			preface = preface + "import (\n"
			for _, v := range gh.Prsr.GoImports[source] {
				preface = preface + "    \"" + v + "\"\n"
			}
			preface = preface + ")\n\n"
		}

		// You can't reuse the names of shared object files.
		counter++
		soFile := gh.Prsr.Directory + "rsc/go/" + text.Flatten(source) + "_" + strconv.Itoa(int(modifiedTime)) + ".so"
		if lastChange != 0 {
			os.Remove(gh.Prsr.Directory + "rsc/go/" + text.Flatten(source) + "_" + strconv.Itoa(int(lastChange)) + ".so")
		}
		goFile := gh.Prsr.Directory + "gocode " + strconv.Itoa(counter) + ".go"
		file, _ := os.Create(goFile)
		file.WriteString(preface + functionBodies + appendix + gh.TypeDeclarations[source])
		file.Close()
		cmd := exec.Command("go", "build", "-buildmode=plugin", "-o", soFile, goFile) // Version to use running from terminal.
		// cmd := exec.Command("go", "build", "-gcflags=all=-N -l", "-buildmode=plugin", "-o", soFile, goFile) // Version to use with debugger.
		output, err := cmd.Output()
		if err != nil {
			gh.Prsr.Throw("golang/build", &token.Token{}, err.Error()+": "+string(output))
		}
		gh.Plugins[source], err = plugin.Open(soFile)
		if err != nil {
			gh.Prsr.Throw("golang/open", &token.Token{}, err.Error())
		} else {
			os.Remove("gocode " + strconv.Itoa(counter) + ".go")
		}
	}
}

func (gh *GoHandler) MakeFunction(keyword string, sig, rTypes ast.AstSig, golang *ast.GolangExpression) {

	source := golang.GetToken().Source

	if gh.StructNames[source] == nil {
		gh.StructNames[source] = make(dtypes.Set[string])
	}

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
			ty = ".(value.Value)"
			if !ok {
				gh.Prsr.Throw("golang/type/a", golang.GetToken(), v.VarType)
				return
			}
		} else {
			ty, ok = gh.doTypeConversion(source, v.VarType)
			if !ok {
				gh.Prsr.Throw("golang/type/b", golang.GetToken(), v.VarType)
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

	for _, v := range rTypes {
		_, ok := gh.doTypeConversion(source, v.VarType) // Note that this will add struct types to the GoHandler's list of them.
		if !ok {
			gh.Prsr.Throw("golang/type/c", golang.GetToken(), v.VarType)
			return
		}
	}

	fnString = fnString + doctorReturns(golang.Token.Literal) + "\n\n"

	gh.Modules[source] = gh.Modules[source] + fnString
}

func (gh *GoHandler) AddPureGoBlock(source, code string) {
	gh.Modules[source] = gh.Modules[source] + "\n" + code[:len(code)-2] + "\n\n"
}

func (gh *GoHandler) GetFn(fnName string, tok *token.Token) func(args ...any) any {
	name := capitalize(fnName)
	fn, err := gh.Plugins[tok.Source].Lookup(name)
	if err != nil {
		gh.Prsr.Throw("golang/found", tok, name)
		return nil
	}
	fnToReturn := fn.(func(args ...any) any)
	return fnToReturn
}

var typeConv = map[string]string{"bling": ".(string)",
	"bool":   ".(bool)",
	"error":  ".(error)",
	"float":  ".(float64)",
	"func":   ".(func(args ...any) any)",
	"int":    ".(int)",
	"label":  ".(string)",
	"list":   ".([]any)",
	"pair":   ".([]any)",
	"set":    ".([]any)",
	"single": "",
	"string": ".(string)",
	"tuple":  ".([]any)",
	"type":   ".(string)",
}

func (gh *GoHandler) doTypeConversion(source, pTy string) (string, bool) {
	goTy, ok := typeConv[pTy]
	if ok {
		return goTy, true
	}
	// If it's not a native type, then it may be a struct, so it may be namespaced.
	bits := strings.Split(pTy, ".")
	name := bits[len(bits)-1]
	namespacePath := bits[0 : len(bits)-1]
	resolvingParser := gh.Prsr
	for _, namespace := range namespacePath {
		s, ok := resolvingParser.NamespaceBranch[namespace]
		if !ok {
			gh.Prsr.Throw("golang/namespace", &token.Token{Source: "function doing type conversion for Golang"}, namespace) // ToDp
		}
		resolvingParser = s.Parser
	}

	if resolvingParser.Structs.Contains(name) {
		gh.StructNames[source].Add(pTy)
		return ".(" + text.Flatten(pTy) + ")", true
	}
	return "", false
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
