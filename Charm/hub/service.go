package hub

import(
	"bufio"
	"os"
	"strings"

	"charm/ast"
	"charm/evaluator"
	"charm/object"
	"charm/parser"
	"charm/text"
	"charm/token"
)

type Service struct{
	Parser *parser.Parser
	Env *object.Environment
	scriptFilepath string
	dataFilepath string
}

func NewService() *Service {
	service := Service{}
	return &service
}

// For opening data files.
func (service *Service) SuDo(line string) {
	tree := *service.Parser.ParseLine("REPL input", line)
	tree.(*ast.AssignmentExpression).Token.Type = token.CMD_ASSIGN
	evaluator.Evaluate(tree, &(*service.Parser), service.Env).Inspect(object.ViewCharmLiteral)
}

func (service *Service) Do(line string) string {
	serviceWords := strings.Fields(line)
	if len(serviceWords) == 0 {
		panic("It shouldn't be possible to pass an empty string to a service. You goofed.")
	}
	possibleVerb := serviceWords[0]

	// Verbs in alphabetical order: open, save

	switch possibleVerb {
	case "open" :
		if len(serviceWords) == 1 {
			return text.ERROR + "the " + text.Emph("open") + " keyword needs you to specify a file"
		}
		if len(serviceWords) != 2 {
			return text.ERROR + "the only thing that should follow " + text.Emph("open") + 
			/**/ " is a filename"
		}
		service.dataFilepath = serviceWords[1]
		returnStr := service.OpenDataFile(serviceWords[1])
		return returnStr
	case "save" :
		switch len(serviceWords){
		case 1 :
			if service.dataFilepath == "" {
				return text.ERROR + "there is no current file: Charm doesn't know where you want to save to"
			}
			returnStr := service.saveFile(service.dataFilepath)
			return returnStr
		case 2 :
			returnStr := service.saveFile(serviceWords[1])
			service.dataFilepath = serviceWords[1]
			return returnStr
		default : return text.ERROR + "the only thing that should follow " + text.Emph("save") + 
		/**/ " is a filename"
		}
	}
	value, _ := service.Env.Get("$view")
	switch value.(*object.String).Value {
	case "charm" :
		return evaluator.Evaluate(*service.Parser.ParseLine(
			"REPL input", line), &(*service.Parser), service.Env).Inspect(object.ViewCharmLiteral)
	case "plain" :
		return evaluator.Evaluate(*service.Parser.ParseLine(
			"REPL input", line), &(*service.Parser), service.Env).Inspect(object.ViewStdOut)
	default :
		panic ("You've assigned an impossible value to $view. Find out why and make it stop.")
	}
}

func (service *Service) saveFile(filepath string) string {

	f, err := os.Create(filepath)
    if err != nil {
        return text.ERROR + strings.TrimSpace(err.Error())
    }
    defer f.Close()
    _, err2 := f.WriteString(service.Env.StringDumpVariables())

    if err2 != nil {
        return text.ERROR + strings.TrimSpace(err.Error())
    }
	service.dataFilepath = filepath
	return text.OK
}

func (service *Service) OpenDataFile(filepath string) string {
	f, err := os.Open(filepath)
    if err != nil {
        return text.ERROR + strings.TrimSpace(err.Error())
    }
    defer f.Close()

    scanner := bufio.NewScanner(f)
    for scanner.Scan() {
        service.SuDo(scanner.Text())
	}
	service.dataFilepath = filepath
	return text.OK
}

func (service *Service) GetScriptFilepath() string {
	return service.scriptFilepath
}

func (service *Service) GetDataFilepath() string {
	return service.dataFilepath
}
