package hub

import (
	"bufio"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"charm/initializer"
	"charm/lexer"
	"charm/object"
	"charm/parser"
	"charm/relexer"
	"charm/text"
	"charm/token"
)

var (
	MARGIN = 84
)

type Hub struct{
	services map[string] *Service
	ers object.Errors
	currentServiceName string
	peek bool
	in  io.Reader
	out io.Writer
	anonymousServiceNumber int
	snap *Snap
	oldServiceName string // Somewhere to keep the old service name while taking a snap
	Sources map[string] []string
}

//
// This takes the input from the REPL, interprets it as a hub command if it begins with 'hub';
// as an instruction to switch services if it consists only of the name of a service; as 
// an expression to be passed to a service if it begins with the name of a service; and as an
// expression to be passed to the current service if none of the above hold.
//
func (hub *Hub) Do(line string) bool {

	// We may be talking to the hub.

	hubWords := strings.Fields(line)
	if hubWords[0] == "hub" {
		return hub.ParseHubCommand(hubWords[1:])
	}

	// We may be talking to the os

	if hubWords[0] == "os" {
		if len(hubWords) == 3 && hubWords[1] == "cd" {  // Because cd changes the directory for the current
			os.Chdir(hubWords[2]) 						// process, if we did it with exec it would do it for		
			hub.WriteString(text.OK)					// that process and not for Charm.
			return false                      
		}
		command := exec.Command(hubWords[1], hubWords[2:]...)
		out, err := command.Output()
		if err != nil {
			hub.WriteError(err.Error())
			return false
		}
		if len(out) == 0 {
			hub.WriteString(text.OK)
				return false
		}
		hub.WriteString(string(out))
		return false
	}

	// Otherwise, we need to find a service to talk to.

	service, ok := hub.services[hubWords[0]]
	if ok{
		if len(hubWords) == 1 {
			hub.currentServiceName = hubWords[0]
			hub.WriteString(text.OK + "\n")
			return false
		}
		line = line[len(hubWords[0]) + 1 : ]	
	} else {
		service, ok = hub.services[hub.currentServiceName]
	}
	if !ok {
		hub.WriteError("the hub can't find the service '" + hub.currentServiceName + "'.")
		return false
	}
	
	// If we do, we pass the line to the service and get back a string to output.

	// It hub peek is turned on, this will show us the wheels going round.
	if hub.peek {
		lexer.LexDump(line)
		relexer.RelexDump(line)
		service.Parser.ParseDump(hub.currentServiceName, line)
	}

	// Primitive built-in file-handling, done up here so that ExtractVariables doesn't take the
	// keywords for variables. 
	if line == "save" || line == "open" || len(line) > 4 && (line[0:5] == "save "|| line[0:5] == "open ") {
			obj := service.Do(line)
			if obj.Type() == object.ERROR_OBJ {
				hub.WritePretty("\n[0] " + objToString(service, obj))
				hub.WriteString("\n")
				hub.ers = []*object.Error{obj.(*object.Error)}
			} else {
				hub.WriteString(objToString(service, obj))
			}
			return false
		}

	// Errors in the parser are a signal for the parser/initializer to halt, so we need to clear them here.
	service.Parser.ClearErrors()

	hub.Sources["REPL input"] = []string{line}

	// We can check here if the input directly references a private variable or non-existent variable.
	LHS, RHS := service.Parser.ExtractVariables(relexer.New("REPL input", line))
	LHS.AddSet(RHS)
	for k, _ := range(LHS) {
		if (! service.Env.Exists(k)) || service.Env.IsPrivate(k) { 
			service.Parser.Throw("repl/var", token.Token{Source:"REPL input", Line: -1}, k)
		}
	}


	if service.Parser.ErrorsExist() {
		hub.GetAndReportErrors(service.Parser)
		return false
	}

	obj := service.Do(line)
	if service.Parser.ErrorsExist() {
		hub.GetAndReportErrors(service.Parser)
		return false
	}
	
	if obj.Type() == object.ERROR_OBJ {
		hub.WritePretty("\n[0] " + objToString(service, obj))
		hub.WriteString("\n")
		hub.ers = []*object.Error{obj.(*object.Error)}
	} else {
		hub.WriteString(objToString(service, obj))
	}

	if hub.currentServiceName == "#snap" {
		hub.snap.AddInput(line)
		hub.snap.AddOutput(objToString(service, obj))
	}
	return false
}

func (hub *Hub) ParseHubCommand(hubWords []string) bool { // Returns true if the command is 'quit', since it can't quit
	fieldCount := len(hubWords)                           // from the REPL itself.
	if fieldCount == 0 {
		hub.help()
		return false
	}
	verb := hubWords[0]
	switch verb {

	// Verbs are in alphabetical order :
	// edit, errors, help, peek, quit, replay, run, services, snap, test, trace

	case "edit" :
		switch {
			case fieldCount == 1 : 
				hub.WriteError("the 'hub edit' command requires a filename as a parameter.")
			case fieldCount > 2 :
				hub.WriteError("the 'hub edit' command takes at most one parameter.")
			default :
				command := exec.Command("vim", hubWords[1])
				command.Stdin = os.Stdin
				command.Stdout = os.Stdout
				err := command.Run()
				if err != nil {
					hub.WriteError(err.Error())
				}
		}
	case "errors" :
		if fieldCount > 1 {
			hub.WriteError("the 'errors' keyword takes no parameters.")
		}
		if len(hub.ers) == 0 {
			hub.WritePretty("There are no recent errors.")		
		}
		hub.WritePretty(object.GetList(hub.ers))
	case "halt" : 
		name := hub.currentServiceName
		if fieldCount > 2 {
			hub.WriteError("the 'hub reset' command takes at most one parameter, the name of a service.")
		}
		ok := true
		if fieldCount == 2  {
			_, ok = hub.services[hubWords[1]]
			if ok {
				name = hubWords[1]
			} else {
				hub.WriteError("the hub can't find the service '" + hub.currentServiceName + "'.")
				return false
			}
		}
		if name == hub.currentServiceName { hub.currentServiceName = "" }
		delete(hub.services, name) 
		hub.WriteString(text.OK + "\n")
		return false	
	case "help" :	
		switch {
			case fieldCount == 1 : hub.help()
			case fieldCount > 2 :
				hub.WriteError("the 'hub help' command takes at most one parameter.")
			default :
				if helpMessage, ok := helpStrings[hubWords[1]]; ok {
					hub.WritePretty(helpMessage + "\n")
				} else {
					hub.WriteError("the 'hub help' command doesn't accept " +
					/**/ "'" + hubWords[1] + "' as a parameter.")
				}
			}
	case "peek" : 	
		switch {
			case fieldCount == 1 : hub.peek = !hub.peek 
			case fieldCount == 2 :
				switch hubWords[1] {
				case "on" : hub.peek = true
				case "off" : hub.peek = false
				default : hub.WriteError("the 'hub peek' command only accepts the parameters "+ 
				/**/ "'on'  or 'off'.")
				}
			default : hub.WriteError("the 'hub peek' command only accepts a single parameter, "+ 
				/**/ "'on'  or 'off'.")

		}
	case "quit" : if fieldCount > 1 {
		hub.WriteError("the 'hub quit' command takes no parameters")
		} else {
			hub.quit()
			return true
		}
	case "replay" : 
	
		hub.oldServiceName = hub.currentServiceName

		switch {
			case fieldCount == 2 :
				hub.playTest(hubWords[1], false)
			case fieldCount == 3 :	
				if hubWords[2] != "diff" {
					hub.WriteError("the word '" + hubWords[2] + 
					/**/ "' makes no sense there")
				} else {
					hub.playTest(hubWords[1], true)
				}
			default :
				hub.WriteError("the 'hub replay'" + 
					/**/ " command takes the filepath of a test as a parameter, optionally" +
					/**/ " followed by 'diff'.")
		}

		hub.currentServiceName = hub.oldServiceName

		_, ok := hub.services["#test"];
		if ok {
			delete(hub.services, "#test");
		}
	case "reset" : 
		if len(hubWords) > 2 {
			hub.WriteError("the " + text.Emph("hub reset") + " command takes at most one parameter, the name of a service")
		}
		service, ok := hub.services[hub.currentServiceName]
		if len(hubWords) == 2  {
			service, ok = hub.services[hubWords[1]]
		}
		if !ok {
			hub.WriteError("the hub can't find the service '" + hub.currentServiceName + "'")
			return false
		}
		hub.WritePretty("Restarting script '" + service.GetScriptFilepath() + 
			/**/ "' as service '" + hub.currentServiceName + "'.\n")
		hub.Start(hub.currentServiceName, service.GetScriptFilepath())
		if service.GetDataFilepath() != "" {
			service.OpenDataFile(service.GetDataFilepath())
		}
		return false
	case "run" :
		switch fieldCount {
		case 1 :
			hub.currentServiceName = ""
			return false
	    case 2 :
			hub.WritePretty("Starting script '" + hubWords[1] + 
			/**/ "' as service '#" + strconv.Itoa(hub.anonymousServiceNumber) + "'.\n")
			hub.StartAnonymous(hubWords[1])
			return false
		case 3 :
			if hubWords[2] == "as" {
				hub.WriteError("missing service name after 'as'.")
				return false
			}
			hub.WriteError("the word '" + hubWords[2] + "' doesn't make any sense there.")
			return false
		case 4:
			if hubWords[2] != "as" {
				hub.WriteError("the word '" + hubWords[2] + "' doesn't make any sense there.")
				return false
			}
			hub.WritePretty("Starting script '" + hubWords[1] + "' as service '" + hubWords[3] + "'.\n")
			hub.Start(hubWords[3], hubWords[1])
		default :
			hub.WriteError("too many words after 'hub run'.")
			return false
		}
	case "services" :	
		switch {
			case fieldCount == 1 :
				hub.WriteString("\n") 
				hub.list()
			default :
				hub.WriteError("the 'hub services' command takes no parameters.")
		}
	case "snap" :
		switch fieldCount {
		case 1 :
			hub.WriteError("the 'hub snap' command needs some parameters.")
			return false
		case 2 :
			fieldOne := hubWords[1]
			if fieldOne == "good" || fieldOne == "bad" || fieldOne == "record" || fieldOne == "discard" {
				if hub.currentServiceName != "#snap" {
					hub.WriteError("you aren't taking a snap.")
				return false
				}
			}
			switch fieldOne {
			case "good" :
				result := hub.snap.Save(GOOD)
				hub.WriteString(result + "\n")
			case "bad"  :
				result := hub.snap.Save(BAD)
				hub.WriteString(result + "\n")
			case "record" :
				result := hub.snap.Save(RECORD)
				hub.WriteString(result + "\n")
			case "discard" :
				hub.WriteString(text.OK + "\n")
			default:
				scriptFilepath := fieldOne
				testFilename := getUnusedTestFilename(scriptFilepath) // If no filename is given, we just generate one.
				hub.snap = NewSnap(scriptFilepath, testFilename, "")
				hub.oldServiceName = hub.currentServiceName
				hub.Start("#snap", scriptFilepath)
				(*hub).services["#snap"].Do("$view = \"charm\"")
				hub.WriteString("Serialization is ON.\n")
			}
			if fieldOne == "good" || fieldOne == "bad" || fieldOne == "record" || fieldOne == "discard" {
				hub.currentServiceName = hub.oldServiceName
			}
			return false
		case 3 :
			if hubWords[2] == "as" {
				hub.WriteError("missing test filename after 'as'.")
				return false
			}
			hub.WriteError("the word '" + text.Emph(hubWords[2]) + 
			/**/ "' doesn't make any sense there")
			return false
		case 4:
			switch hubWords[2] {
			case "as":
				scriptFilepath := hubWords[1]
				testFilepath := hubWords[3]
				hub.snap = NewSnap(scriptFilepath, testFilepath, "")
				hub.oldServiceName = hub.currentServiceName
				hub.Start("#snap", scriptFilepath)
				(*hub).services["#snap"].Do("$view = \"charm\"")
				hub.WriteString("Serialization is ON.\n")
			case "with":
				scriptFilepath := hubWords[1]
				dataFilepath := hubWords[3]
				testFilename := getUnusedTestFilename(scriptFilepath) // If no filename is given, we just generate one.
				hub.snap = NewSnap(scriptFilepath, testFilename, dataFilepath)
				hub.oldServiceName = hub.currentServiceName
				hub.Start("#snap", scriptFilepath)
				hub.services["#snap"].OpenDataFile(dataFilepath)
				(*hub).services["#snap"].Do("$view = \"charm\"")
				hub.WriteString("Serialization is ON.\n")
			default:
				hub.WriteError("the word '" + hubWords[2] + 
				/**/ "' doesn't make any sense there.")
			return false
			}
		case 5 :
			if hubWords[4] == "with" {
				hub.WriteError("missing test filename after 'as'")
				return false
			}
			if hubWords[4] == "as" {
				hub.WriteError("missing data filename after 'with'.")
				return false
			}
			hub.WriteError("the word '" + hubWords[4] + 
			/**/ "' doesn't make any sense there.")
			return false
		case 6 :
			if hubWords[2] != "with" && hubWords[2] != "as" {
				hub.WriteError("the word '" + hubWords[2] + 
				/**/ "' doesn't make any sense there.")
				return false
			}
			if hubWords[4] != "with" && hubWords[4] != "as" {
				hub.WriteError("the word '" + hubWords[4] + 
				/**/ "' doesn't make any sense there.")
				return false
			}
			if hubWords[2] == "with" && hubWords[4] == "with" {
				hub.WriteError("it doesn't make sense to say 'with' twice")
				return false
			}
			if hubWords[2] == "as" && hubWords[4] == "as" {
				hub.WriteError("it doesn't make sense to say 'as' twice")
				return false
			}
			testFilepath := ""
			dataFilepath := ""
			scriptFilepath := hubWords[1]
			if hubWords[2] == "as" {
				testFilepath = hubWords[3]
				dataFilepath = hubWords[5]
			} else {
				testFilepath = hubWords[5]
				dataFilepath = hubWords[3]
			}
			hub.snap = NewSnap(scriptFilepath, testFilepath, dataFilepath)
			hub.Start("#snap", scriptFilepath)
			(*hub).services["#snap"].Do("$view = \"charm\"")
			hub.WriteString("Serialization is ON.\n")
			hub.oldServiceName = hub.currentServiceName
			return false
		default :
			hub.WriteError("too many words after 'hub snap'.")
			return false
		}
	case "test" :
		switch fieldCount {
		case 1 :
			hub.WriteError("the 'hub test' command needs some parameters.")
			return false
		case 2 :
			hub.TestScript(hubWords[1])
		default :
			hub.WriteError("too many words after 'hub test'.")
		}
	case "trace" :
		switch fieldCount {
		case 1 :
			if len(hub.ers) == 0 {
				hub.WriteError("there are no recent errors.")
				return false
			} 
			if len(hub.ers[0].Trace) == 0 {
				hub.WriteError("not a runtime error.")
				return false
			}
			hub.WritePretty("\n$Runtime error$" + hub.ers[0].Message + "\n\n")  // If it is a runtime error, then there is only one of them.
			for i := len(hub.ers[0].Trace) - 1; i >= 0; i-- {
				hub.WritePretty("  From: " + text.DescribeTok(hub.ers[0].Trace[i]) + text.DescribePos(hub.ers[0].Trace[i]) + ".")
			}
			hub.WriteString("\n")
			return false
		default :
			hub.WriteError("the 'hub trace' command takes no parameters.")
			return false
		}
	case "where" :
		if fieldCount == 1 {
			hub.WriteError("the 'where' keyword requires the number of an error as a parameter.")
			return false
		}
		if fieldCount > 2 {
			hub.WriteError("the 'where' keyword takes only one parameter, the number of an error.")
			return false
		}
		num, err := strconv.Atoi(hubWords[1])
		if err != nil {
			hub.WriteError("the 'where' keyword takes the number of an error as a parameter.")
			return false
		}
		if num < 0 { 
			hub.WriteError("the 'where' keyword can't take a negative number as a parameter.") 
			return false
		}
		if num >= len(hub.ers) { 
			hub.WriteError("there aren't that many errors.") 
			return false
		}


		hub.WritePretty("\nFound" + text.DescribePos(hub.ers[num].Token) + ":\n\n")
		line := hub.Sources[hub.ers[num].Token.Source][hub.ers[num].Token.Line - 1] + "\n"
		startUnderline := hub.ers[num].Token.ChStart
		lenUnderline := hub.ers[num].Token.ChEnd - startUnderline
		if lenUnderline == 0 { lenUnderline = 1 }
		endUnderline := startUnderline + lenUnderline
		hub.WriteString(line[0:startUnderline])
		hub.WriteString(text.Red(line[startUnderline:endUnderline]))
		hub.WriteString(line[endUnderline:len(line)])
		hub.WriteString(strings.Repeat(" ", startUnderline))
		hub.WriteString(text.Red(strings.Repeat("▔", lenUnderline)))
		return false
	case "why" :
		if fieldCount == 1 {
			hub.WriteError("the 'why' keyword requires the number of an error as a parameter.")
			return false
		}
		if fieldCount > 2 {
			hub.WriteError("the 'why' keyword takes only one parameter, the number of an error.")
			return false
		}
		num, err := strconv.Atoi(hubWords[1])
		if err != nil {
			hub.WriteError("the 'why' keyword takes the number of an error as a parameter.")
			return false
		}
		hub.WritePretty("\n$Error$" + hub.ers[num].Message + 
		".\n\n" + object.ErrorCreatorMap[hub.ers[num].ErrorId].Explanation(hub.ers, num, hub.ers[num].Token, hub.ers[num].Info...) + "\n")
		refLine := "Error has reference '" + hub.ers[num].ErrorId + "'."
		refLine = "\n" + strings.Repeat(" ", MARGIN - len(refLine) - 2) + refLine 
		hub.WritePretty(refLine)
		hub.WriteString("\n")
	default :
		hub.WriteError("the hub doesn't recognize the command '" + verb + "'.")
		return false	
	}
	return false
}

func getUnusedTestFilename(scriptFilepath string) string {
	directoryName := text.FlattenedFilename(scriptFilepath)
	name := text.FlattenedFilename(scriptFilepath) + "_"
	
	tryNumber := 1
	tryName := ""

	for ; ; tryNumber++ {
		tryName =  name + strconv.Itoa(tryNumber)
		_ , error := os.Stat("tst/" + directoryName + "/" + tryName)
		if os.IsNotExist(error) {
			break
		}
	}
	return tryName
}

func (hub *Hub)  quit() {
	hub.save()
	hub.WriteString(text.OK + "\n" + text.Logo() + "Thank you for using Charm. Have a nice day!\n\n")
}

func (hub *Hub) help() {
hub.WriteString("\n")
hub.WriteString("Help topics are:\n")
hub.WriteString("\n")
for _, v := range helpTopics {
	hub.WriteString("  " + text.BULLET + v + "\n")
}
hub.WriteString("\n")
}


func (hub *Hub) WritePretty(s string) {
	codeWidth := - 1
	highlighter := ' '
	for i :=0; i < len(s) ; {
		e := i + MARGIN
		j := 0
		if e > len(s) {
			j = len(s) - i
		} else if strings.Index(s[i:e], "\n") != -1 {
			j = strings.Index(s[i:e], "\n")
		} else {
			j = strings.LastIndex(s[i:e], " ")
		}
		if j == -1 {
			j = MARGIN
		}
		if strings.Index(s[i:i+j], "\n") != -1 {
			j = strings.Index(s[i:i+j], "\n")
		}

		plainLine := s[i:i + j]
		if len(plainLine) >= 2 && plainLine[0:2] == "|-" {
			if codeWidth > 0 {
				hub.WriteString(" └──" + strings.Repeat("─", codeWidth) + "┘\n")
				codeWidth = - 1
			} else {
				codeWidth = len(plainLine)
				hub.WriteString(" ┌──" + strings.Repeat("─", codeWidth) + "┐\n")
			}
		} else if codeWidth > 0 {
			hub.WriteString(" │  " + text.Cyan(plainLine) + strings.Repeat(" ", codeWidth - len(plainLine)) + "│\n")
		} else {
			hub.WriteString(text.HighlightLine(plainLine, highlighter) + "\n" )
		}
		i = i + j + 1
	}
}

func (hub *Hub) WriteError(s string) {
	hub.WritePretty("\n$Hub error$" + s)
	hub.WriteString("\n")
}

func (hub *Hub) WriteString(s string) {
	io.WriteString(hub.out, s)
}

var helpStrings = map[string] string{}

var helpTopics = []string{}

func init() {
	file, err := os.Open("rsc/manual.txt")
    if err != nil {
        return
    }
    defer file.Close()

    var lines []string
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        lines = append(lines, scanner.Text())
    }
   
	var helpMessage []string 
	for _, v := range(lines) {
		v = strings.TrimRight(v, " \n")
		if v == "***" {
			helpTopics = append(helpTopics, strings.TrimSpace(helpMessage[0]))
			helpStrings[strings.TrimSpace(helpMessage[0])] = strings.Join(helpMessage[1:len(helpMessage)], "\n")
			helpMessage = []string{}
		} else {
			helpMessage = append(helpMessage, v)
		}
	}
	sort.Strings(helpTopics)
}

func (hub *Hub) StartAnonymous(scriptFilepath string) {
	hub.Start("#" + fmt.Sprint(hub.anonymousServiceNumber), scriptFilepath)
	hub.anonymousServiceNumber = hub.anonymousServiceNumber + 1
}

func (hub *Hub) Start(name, scriptFilepath string) {
	code := ""
	if scriptFilepath != "" {
		dat, err := os.ReadFile(scriptFilepath)
		if err != nil {
			hub.WriteError("os returns \"" + err.Error() + "\".")
			return
		} else {
			code = strings.TrimRight(string(dat), "\n") + "\n"
		}
	}
	hub.currentServiceName = name
	hub.createService(name, scriptFilepath, code)
	return
}

func (hub *Hub) createService(name, scriptFilepath, code string) {
	init := initializer.New(scriptFilepath, code)
	init.GetSource(scriptFilepath)
	init.Parser = *parser.New()
	init.MakeParserAndTokenizedProgram()
	if init.ErrorsExist() {
		hub.GetAndReportErrors(&init.Parser)
		hub.Sources = init.Sources
		hub.currentServiceName = ""
		return
	}
	init.ParseImports()
	if init.ErrorsExist() {
		hub.GetAndReportErrors(&init.Parser)
		hub.Sources = init.Sources
		hub.currentServiceName = ""
		return
	}
	init.ImportEverything()
	if init.ErrorsExist() {
		hub.GetAndReportErrors(&init.Parser)
		hub.Sources = init.Sources
		hub.currentServiceName = ""
		return
	}

	env := object.NewEnvironment()
	init.ParseEnumDefs(env)
	if init.ErrorsExist() {
		hub.GetAndReportErrors(&init.Parser)
		hub.Sources = init.Sources
		hub.currentServiceName = ""
		return
	}

	init.ParseTypeDefs()
	if init.ErrorsExist() {
		hub.GetAndReportErrors(&init.Parser)
		hub.Sources = init.Sources
		hub.currentServiceName = ""
		return
	}

	init.EvaluateTypeDefs(env)
	if init.ErrorsExist() {
		hub.GetAndReportErrors(&init.Parser)
		hub.Sources = init.Sources
		hub.currentServiceName = ""
		return
	}

	init.ParseEverything()
	if init.ErrorsExist() {
		hub.GetAndReportErrors(&init.Parser)
		hub.Sources = init.Sources
		hub.currentServiceName = ""
		return
	}

	init.InitializeEverything(env)
	if init.ErrorsExist() {
		hub.GetAndReportErrors(&init.Parser)
		hub.Sources = init.Sources
		hub.currentServiceName = ""
		return
	}

	newService := NewService()
	(*newService).Parser = &init.Parser
	(*newService).Env = env
	(*newService).scriptFilepath = scriptFilepath
	hub.services[name] = newService
	hub.Sources = init.Sources

	return

}

func (hub *Hub) GetAndReportErrors(p *parser.Parser) {
	hub.ers = p.Errors
	hub.WritePretty(object.GetList(hub.ers))
}

func New(in io.Reader, out io.Writer) *Hub {
	hub := Hub{services : make(map[string]*Service), currentServiceName: "", in: in, out: out}
	return &hub
}

func (hub *Hub) GetCurrentServiceName() string {
	return hub.currentServiceName
}

func (hub *Hub) save() string {
	f, err := os.Create("rsc/hub.dat")
    if err != nil {
        return text.HUB_ERROR + "os reports \"" + strings.TrimSpace(err.Error()) + "\".\n"
    }
    defer f.Close()
	for k := range hub.services {
		if !isAnonymous(k) && k != "#snap" && k != "#test" {
    		_, err := f.WriteString(k + ", " + hub.services[k].GetScriptFilepath() + ", " +
			/**/ hub.services[k].GetDataFilepath() + "\n")
    		if err != nil {
        		return text.HUB_ERROR + "os reports \"" + strings.TrimSpace(err.Error()) + "\".\n"
			}
    	}
	}
	f, err = os.Create("rsc/current.dat")
    if err != nil {
        return text.HUB_ERROR + "os reports \"" + strings.TrimSpace(err.Error()) + "\".\n"
    }
    defer f.Close()
	if isAnonymous(hub.currentServiceName) {
		_, err = f.WriteString("")
	} else {
		_, err = f.WriteString(hub.currentServiceName)
	}
	if err != nil {
		return text.HUB_ERROR + "os reports \"" + strings.TrimSpace(err.Error()) + "\".\n"
	}
	return text.OK
}

func isAnonymous(serviceName string) bool {
	if serviceName == "" { return true }
	_, err := strconv.Atoi(serviceName[1:])
	return serviceName[0] == '#' && err == nil
}

func (hub *Hub) Open() {

	f, err := os.Open("rsc/hub.dat")
    if err != nil {
        hub.WriteError(strings.TrimSpace(err.Error()))
    }
    defer f.Close()

	scanner := bufio.NewScanner(f)
    for scanner.Scan() {
		params := strings.Split(scanner.Text(), ", ")
        hub.Start(params[0], params[1])
		if params[2] != "" {
			hub.services[params[0]].OpenDataFile(params[2])
		}
	}

	hub.createService("", "", "")

	hub.list()

	f, err = os.Open("rsc/current.dat")
    if err != nil {
        hub.WriteError(strings.TrimSpace(err.Error()))
    }
    defer f.Close()

	scanner = bufio.NewScanner(f)
    scanner.Scan()
	hub.currentServiceName = scanner.Text()
	return
}

func (hub *Hub) list() {
	if len(hub.services) == 1 { return } // The would be the empty service, the REPL
	hub.WriteString("The hub is running the following services:\n\n")
	for k := range(hub.services) {
		if k == "" { continue }
		hub.WritePretty("service '" + k + "' running script '" + filepath.Base(hub.services[k].GetScriptFilepath()) + "'")
		if hub.services[k].GetDataFilepath() != "" {
			hub.WriteString(" with data '" + filepath.Base(hub.services[k].GetDataFilepath()) + "'")
		}
		hub.WriteString("\n")
	}
	hub.WriteString("\n")
}



func (hub *Hub) TestScript(scriptFilepath string) {
	
	directoryName := "tst/" + text.FlattenedFilename(scriptFilepath)

	if _, err := os.Stat(directoryName); os.IsNotExist(err) {
		hub.WriteError(strings.TrimSpace(err.Error()) + "\n")
		return
	}
	hub.oldServiceName = hub.currentServiceName
	files, _ := ioutil.ReadDir(directoryName)
	for _, testFileInfo := range files {
		testFilepath := directoryName + "/" + testFileInfo.Name()
		f, err := os.Open(testFilepath)
			if err != nil {
				hub.WriteError(strings.TrimSpace(err.Error()) + "/n")
				continue
			}
		
		scanner := bufio.NewScanner(f)
        scanner.Scan()
		testType := strings.Split(scanner.Text(), ": ")[1]
		if testType == RECORD {
			f.Close()
			continue
		}
		scanner.Scan() 
		scanner.Scan() 
		dataFilepath := strings.Split(scanner.Text(), ": ")[1]
        hub.Start("#test", scriptFilepath)
		hub.WritePretty("Running test '" + testFilepath + "'.\n")
		if dataFilepath != "" {
			hub.services["#test"].OpenDataFile(dataFilepath)
		}
		(*hub).services["#test"].Do("$view = \"charm\"")
		service := (*hub).services["#test"]
		_ = scanner.Scan() // eats the newline
		executionMatchesTest := true
		for scanner.Scan() {
			lineIn := scanner.Text()[3:]
			scanner.Scan()
			lineOut := scanner.Text()
			result := service.Do(lineIn)
			if service.Parser.ErrorsExist() {
				hub.WritePretty(service.Parser.ReturnErrors())
				f.Close()
				continue
			}
		executionMatchesTest = executionMatchesTest && (objToString(service, result) == lineOut)
		}
		if executionMatchesTest && testType == BAD {
			hub.WriteError("bad behavior reproduced by test" + "\n")
			f.Close()
			hub.playTest(testFilepath, false) // not that it matters if it's true or false ...
			continue
		}
		if !executionMatchesTest && testType == GOOD {
			hub.WriteError("good behavior not reproduced by test" + "\n")
			f.Close()
			hub.playTest(testFilepath, true)
			continue
		}
		hub.WriteString("Test passed!" + "\n")
		f.Close()
	}
	_, ok := hub.services["#test"];
    if ok {
        delete(hub.services, "#test");
    }
	hub.currentServiceName = hub.oldServiceName
	
}

func (hub *Hub) playTest(testFilepath string, diffOn bool){
	f, err := os.Open(testFilepath)
			if err != nil {
				hub.WriteError(strings.TrimSpace(err.Error()) + "/n")
				return
			}
		
	scanner := bufio.NewScanner(f)
	scanner.Scan() // test type doesn't matter
	scanner.Scan() 
	scriptFilepath := strings.Split(scanner.Text(), ": ")[1]
	scanner.Scan() 
	dataFilepath := strings.Split(scanner.Text(), ": ")[1]
	hub.Start("#test", scriptFilepath)
	if dataFilepath != "" {
		hub.services["#test"].OpenDataFile(dataFilepath)
	}
	(*hub).services["#test"].Do("$view = \"charm\"")
	service := (*hub).services["#test"]
	_ = scanner.Scan() // eats the newline
	for scanner.Scan() {
		lineIn := scanner.Text()[3:]
		scanner.Scan()
		lineOut := scanner.Text()
		result := service.Do(lineIn)
		if service.Parser.ErrorsExist() {
			hub.WritePretty(service.Parser.ReturnErrors())
			f.Close()
			return
		}
		hub.WriteString("#test → " + lineIn + "\n")
		if objToString(service, result) == lineOut || !diffOn {
			hub.WriteString(objToString(service, result) + "\n")
		} else {
			hub.WriteString("was: " + lineOut + "\ngot: " + objToString(service, result) + "\n")
		}
	}
	return
}

func objToString(service *Service, obj object.Object) string {

	value, _ := service.Env.Get("$view")
		switch value.(*object.String).Value {
		case "charm" :
			return obj.Inspect(object.ViewCharmLiteral)
		case "plain" :
			return obj.Inspect(object.ViewStdOut)
		default : panic("I don't know what's going on any more.")
		}
}

