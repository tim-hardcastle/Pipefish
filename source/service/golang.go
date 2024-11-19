package service

// Code for compiling the embedded golang.

import (
	"bufio"
	"os"
	"os/exec"
	"path/filepath"
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
	Prsr             *parser.Parser            // The parser, to tell the GoHandler what the types mean. 
	Modules          map[string]string         // Where the source files are.
	timeMap          map[string]int            // When the source code was constructed.
	Plugins          map[string]*plugin.Plugin // Knows where the plugins live after they've been generated.
	rawHappened      bool                      // Very minor and temporry piece of state which should be somewhere else. TODO.
	CloneNames       dtypes.Set[string]        // Set of Pipefish clone types appearing in the sigs of the functions.
	EnumNames        dtypes.Set[string]        // Set of Pipefish struct types appearing in the sigs of the functions.
	StructNames      dtypes.Set[string]        // Set of Pipefish enum types appearing in the sigs of the functions.
	TypeDeclarations map[string]string         // A string to put the generated source code for declaring things in.
}

func NewGoHandler(prsr *parser.Parser) *GoHandler {

	gh := GoHandler{
		Prsr: prsr,
	}

	gh.timeMap = make(map[string]int)
	gh.Modules = make(map[string]string)
	gh.Plugins = make(map[string]*plugin.Plugin)
	gh.StructNames = make(dtypes.Set[string])
	gh.EnumNames = make(dtypes.Set[string])
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

func (gh *GoHandler) RecordGoTimes() {

	// We add the newly compiled modules to the list of times.

	for k := range gh.Modules {
		filepath := MakeFilepath(k, gh.Prsr.Directory)
		file, err := os.Stat(filepath)
		if err != nil {
			panic("Gohandler cleanup: " + err.Error())
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
		f, err := os.Stat(MakeFilepath(source, gh.Prsr.Directory))
		if err == nil {
			modifiedTime = f.ModTime().UnixMilli()
		}

		lastChange, ok := gh.timeMap[source]
		if ok {
			if modifiedTime == int64(lastChange) {
				soFile := gh.Prsr.Directory + "rsc/go/" + text.Flatten(source) + "_" + strconv.Itoa(lastChange) + ".so"
				gh.Plugins[source], err = plugin.Open(soFile)
				if err == nil {
					continue
				}
				println("Error building/using .so file")
				println("Error was", err.Error())
				panic("That's all folks.")
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
		soFile := filepath.Join(gh.Prsr.Directory, filepath.FromSlash("rsc/go/"+text.Flatten(source)+"_"+strconv.Itoa(int(modifiedTime))+".so"))
		if lastChange != 0 {
			os.Remove(filepath.Join(gh.Prsr.Directory, filepath.FromSlash("rsc/go/"+text.Flatten(source)+"_"+strconv.Itoa(int(lastChange))+".so")))
		}
		goFile := filepath.Join(gh.Prsr.Directory, "gocode_"+strconv.Itoa(counter)+".go")
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
		}
		if err == nil || strings.Contains(err.Error(), "plugin was built with a different version of package") {
			os.Remove("gocode_" + strconv.Itoa(counter) + ".go")
		}
	}
}

func (cp *Compiler) MakeFunction(gh *GoHandler, keyword string, sig, rTypes ast.AstSig, golang *ast.GolangExpression, pfDir string) {

	source := golang.GetToken().Source

	// We check to see whether the source code has been modified.

	doctoredFilename := MakeFilepath(source, pfDir)

	_, err := os.Stat(doctoredFilename)

	if err != nil {
		panic("GoHandler MakeFunction " + err.Error())
	}

	// If the source has been modified, we proceed ...

	fnString := "func " + capitalize(keyword) + "(args ...any) any {\n\n"
	for i, v := range sig {
		preconv := ""
		postconv := ""
		ok := false
		if golang.Raw[i] {
			gh.rawHappened = true
			postconv = ".(values.Value)"
		} else {
			preconv, postconv, ok = cp.doTypeConversion(gh, v.VarType)
			if !ok {
				cp.P.Throw("golang/type", golang.GetToken(), v.VarType)
				return
			}
		}
		fnString = fnString + "    " + v.VarName + " := " + preconv + "args[" + strconv.Itoa(i) + "]" + postconv + "\n"
	}

	for _, v := range rTypes {
		_, _, ok := cp.doTypeConversion(gh, v.VarType) // Note that this will add struct types to the GoHandler's list of them.
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
	"int":    ".(int))", // Extra parenthesis matches the kludge below.
	"label":  ".(string)",
	"list":   ".([]any)",
	"pair":   ".([]any)",
	"rune":   ".(rune)",
	"set":    ".([]any)",
	"any":    "",
	"string": ".(string)",
	"tuple":  ".([]any)",
	"type":   ".(string)",
}

func (cp *Compiler) doTypeConversion(gh *GoHandler, pTy string) (string, string, bool) {
	if len(pTy) >= 3 && pTy[:3] == "..." {
		return "", ".([]any)", true // Since whatever the type is, it turns into a tuple which is converted to a slice before being pssed to the Go function.
	} // TODO --- we should flag unconvertable types at compile time but for now it's their own silly fault.
	goTy, ok := typeConv[pTy]
	if ok {
		if pTy == "int" { // TODO --- I forget why I have to do this and should find out if I can stop.
			return "int(", goTy, true
		} else {
			return "", goTy, true
		}
	}
	
	if gh.Prsr.Structs.Contains(pTy) {
		gh.StructNames.Add(pTy)
		return "", ".(" + pTy + ")", true
	}
	abType := gh.Prsr.GetAbstractType(pTy)
	if abType.IsSubtypeOf(gh.Prsr.Common.Types["enum"]) {
		gh.EnumNames.Add(pTy)
		return pTy + "(", ".(int))", true
	}

	return "", "", false
}

func capitalize(s string) string {
	return strings.ToUpper(s[0:1]) + s[1:]
}

// TODO --- this would also pick out "return" in quotes, comments,
// you need to do a better one.
func doctorReturns(body string) string {
	output := ""
	for ix := strings.Index(body, "return "); ix != -1; ix = strings.Index(body, "return ") {

		output = output + body[:ix]

		body = body[ix+7:]

		returnBody := ""
		for {
			lineEnd := strings.IndexAny(body, "\n\r")
			ix = lineEnd
			if lineEnd == -1 {
				panic("Tim, you goofed. Lines are meant to have endings.")
			}
			newLine := strings.Trim(body[:lineEnd], "\n\r \t")
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
		if len(returnBody) >= 7 && returnBody[:7] == "golang " {
			output = output + "return " + returnBody[7:]
		} else {
			output = output + "return tuplify(" + returnBody + ")"
		}
	}

	return output + body

}
