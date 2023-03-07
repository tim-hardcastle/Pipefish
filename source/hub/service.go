package hub

import (
	"bufio"
	"os"
	"strings"

	"charm/source/ast"
	"charm/source/evaluator"
	"charm/source/object"
	"charm/source/parser"
	"charm/source/token"
)

type Service struct {
	Parser         *parser.Parser
	Env            *object.Environment
	scriptFilepath string
	dataFilepath   string
	timestamp	   int64
	broken		   bool
}

func NewService() *Service {
	service := Service{}
	return &service
}

// For opening data files.
func (service *Service) SuDo(line string) {
	tree := *service.Parser.ParseLine("data file", line)
	tree.(*ast.AssignmentExpression).Token.Type = token.CMD_ASSIGN
	c := evaluator.NewContext(service.Parser, service.Env, evaluator.CMD, false)
	evaluator.Evaluate(tree, c)
}

func (service *Service) ServiceDo(line string) object.Object {
	serviceWords := strings.Fields(line)
	if len(serviceWords) == 0 {
		panic("It shouldn't be possible to pass an empty string to a service. You goofed.")
	}
	possibleVerb := serviceWords[0]

	// Verbs in alphabetical order: open, save

	switch possibleVerb {
	case "open":
		if len(serviceWords) == 1 {
			return CreateErr("serve/open/filename")
		}
		if len(serviceWords) != 2 {
			return CreateErr("serve/open/only")
		}
		service.dataFilepath = serviceWords[1]
		return service.OpenDataFile(serviceWords[1])
	case "save":
		switch len(serviceWords) {
		case 1:
			if service.dataFilepath == "" {
				return CreateErr("serve/save/current")
			}
			return service.saveFile(service.dataFilepath)
		case 2:
			returnObj := service.saveFile(serviceWords[1])
			service.dataFilepath = serviceWords[1]
			return returnObj
		default:
			return CreateErr("serve/save/only")
		}
	}
	// Otherwise we just execute the line.
	return evaluator.Evaluate(*service.Parser.ParseLine("REPL input", line),
					 evaluator.NewContext(service.Parser, service.Env, evaluator.REPL, true))	
}

func (service *Service) saveFile(filepath string) object.Object {

	f, err := os.Create(filepath)
	if err != nil {
		return CreateErr("serve/save/file/a", strings.TrimSpace(err.Error()))
	}
	defer f.Close()
	_, err2 := f.WriteString(service.Env.StringDumpVariables())

	if err2 != nil {
		return CreateErr("serve/save/file/b", strings.TrimSpace(err.Error()))
	}
	service.dataFilepath = filepath
	return evaluator.SUCCESS
}

func (service *Service) OpenDataFile(filepath string) object.Object {
	f, err := os.Open(filepath)
	if err != nil {
		return CreateErr("serve/open/file", strings.TrimSpace(err.Error()))
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		service.SuDo(scanner.Text())
	}
	service.dataFilepath = filepath
	return evaluator.SUCCESS
}

func (service *Service) GetScriptFilepath() string {
	return service.scriptFilepath
}

func (service *Service) GetDataFilepath() string {
	return service.dataFilepath
}

func CreateErr(s string, args ...any) object.Object {
	return object.CreateErr(s, token.Token{Source: "REPL input", Line: 1}, args...)
}
