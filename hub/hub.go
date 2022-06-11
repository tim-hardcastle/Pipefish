package hub

import (
	"bufio"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"

	"charm/lexer"
	"charm/object"
	"charm/parser"
	"charm/relexer"
	"charm/text"
	"charm/token"
	"charm/initializer"
)

var MARGIN = 80

type Hub struct{
	services map[string] *Service
	currentServiceName string
	peek bool
	in  io.Reader
	out io.Writer
	anonymousServiceNumber int
	snap *Snap
	oldServiceName string // Somewhere to keep the old service name while taking a snap
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
			hub.WriteString(text.ERROR + err.Error())
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
		hub.WriteString(text.ERROR + "the hub can't find the service " + 
		/**/ text.Emph(hub.currentServiceName))
		return false
	}
	
	// If we do, we pass the line to the service and get back a string to output.

	if hub.peek {
		lexer.LexDump(line)
		relexer.RelexDump(line)
		service.Parser.ParseDump(hub.currentServiceName, line)
	}

	// We can check here if the input directly references a private variable.

	LHS, RHS := service.Parser.ExtractVariables(relexer.New("REPL input", line))
	LHS.AddSet(RHS)
	for k, _ := range(LHS) {
		if line == "save"|| len(line) > 4 && (line[0:5] == "save " || line[0:5] == "open ") { break }
		if service.Env.Exists(k) && service.Env.IsPrivate(k) { // We do in fact know it exists but why tell 'em?
			service.Parser.MakeError("reference to private or non-existent variable " + text.Emph(k) + " in REPL input", token.Token{})
		}
	}

	result := service.Do(line)
	if service.Parser.ErrorsExist() {
		hub.WriteString(service.Parser.ReturnErrors())
		service.Parser.ClearErrors()
		return false
	}
	hub.WriteString(result + "\n")
	if hub.currentServiceName == "#snap" {
		hub.snap.AddInput(line)
		hub.snap.AddOutput(result)
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
	// edit, help, list, peek, quit, replay, run, snap, test

	case "edit" :
		switch {
			case fieldCount == 1 : 
				hub.WriteString(text.ERROR + "the " + text.Emph("hub edit") + 
				/**/ " command requires a filename as a parameter")
			case fieldCount > 2 :
				hub.WriteString(text.ERROR + "the " + text.Emph("hub edit") + 
				/**/ " command takes at most one parameter")
			default :
				command := exec.Command("vim", hubWords[1])
				command.Stdin = os.Stdin
				command.Stdout = os.Stdout
				err := command.Run()
				if err != nil {
					hub.WriteString(fmt.Sprint(text.ERROR, err))
				}

		}
	
	case "halt" : 
		name := hub.currentServiceName
		if len(hubWords) > 2 {
			hub.WriteString(text.ERROR + "the " + text.Emph("hub reset") + " command takes at most one parameter, the name of a service")
		}
		ok := true
		if len(hubWords) == 2  {
			_, ok = hub.services[hubWords[1]]
			if ok {
				name = hubWords[1]
			} else {
				hub.WriteString(text.ERROR + "the hub can't find the service " + 
				/**/ text.Emph(hub.currentServiceName))
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
				hub.WriteString(text.ERROR + "the " + text.Emph("hub help") + 
				/**/ " command takes at most one parameter")
			default :
				if helpMessage, ok := helpStrings[hubWords[1]]; ok {
					hub.WritePretty(helpMessage)
				} else {
					hub.WriteString(text.ERROR + "the " + text.Emph("hub help") + " command doesn't accept " +
					/**/ text.Emph(hubWords[1]) + " as a parameter")
				}
			}
	case "list" :	
		switch {
			case fieldCount == 1 :
				hub.WriteString("\n") 
				hub.list()
			default :
				hub.WriteString(text.ERROR + "the " + text.Emph("hub list") + 
					/**/ " command takes no parameters")
		}
	case "peek" : 	
		switch {
			case fieldCount == 1 : hub.peek = !hub.peek 
			case fieldCount == 2 :
				switch hubWords[1] {
				case "on" : hub.peek = true
				case "off" : hub.peek = false
				default : hub.WriteString(text.ERROR + "the " + text.Emph("hub peek") + 
				/**/ " command only accepts the parameters "+ text.Emph("on") + " or " + text.Emph("off") + "")
				}
			default :
			hub.WriteString(text.ERROR + "the " + text.Emph("hub peek") + 
			/**/ " command takes at most one parameter, "+ text.Emph("on") + " or " + text.Emph("off") + "")

		}

	case "quit" : if fieldCount > 1 {
		hub.WriteString(text.ERROR + "the " + text.Emph("hub quit") + " command takes no parameters")
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
					hub.WriteString(text.ERROR + "the word " + text.Emph(hubWords[2]) + 
					/**/ " makes no sense there")
				} else {
					hub.playTest(hubWords[1], true)
				}
			default :
				hub.WriteString(text.ERROR + "the " + text.Emph("hub replay") + 
					/**/ " command takes the filepath of a test as a parameter, optionally" +
					/**/ " followed by " + text.Emph("diff"))
		}

		hub.currentServiceName = hub.oldServiceName

		_, ok := hub.services["#test"];
		if ok {
			delete(hub.services, "#test");
		}
	
	case "reset" : 
		if len(hubWords) > 2 {
			hub.WriteString(text.ERROR + "the " + text.Emph("hub reset") + " command takes at most one parameter, the name of a service")
		}
		service, ok := hub.services[hub.currentServiceName]
		if len(hubWords) == 2  {
			service, ok = hub.services[hubWords[1]]
		}
		if !ok {
			hub.WriteString(text.ERROR + "the hub can't find the service " + 
			/**/ text.Emph(hub.currentServiceName))
			return false
		}
		hub.WriteString("Restarting script " + text.Emph(service.GetScriptFilepath()) + 
			/**/ " as service " + text.Emph(hub.currentServiceName) + ".\n")
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
			hub.WriteString("Starting script " + text.Emph(hubWords[1]) + 
			/**/ " as service " + text.Emph("#" + strconv.Itoa(hub.anonymousServiceNumber)) + ".\n")
			hub.StartAnonymous(hubWords[1])
			return false
		case 3 :
			if hubWords[2] == "as" {
				hub.WriteString(text.ERROR + "missing service name after " + text.Emph("as"))
				return false
			}
			hub.WriteString(text.ERROR + "the word " + text.Emph(hubWords[2]) + 
			/**/ "doesn't make any sense there")
			return false
		case 4:
			if hubWords[2] != "as" {
				hub.WriteString(text.ERROR + "the word " + text.Emph(hubWords[2]) + 
				/**/ "doesn't make any sense there")
				return false
			}
			hub.WriteString("Starting script " + text.Emph(hubWords[1]) + " as service " + text.Emph(hubWords[3]) + ".\n")
			hub.Start(hubWords[3], hubWords[1])
		default :
			hub.WriteString(text.ERROR + "too many words after " + text.Emph("hub run") )
			return false
		}

	case "snap" :
		switch fieldCount {
		case 1 :
			hub.WriteString(text.ERROR + "the " + text.Emph("hub snap") + 
			/**/ " command needs some parameters")
			return false
		case 2 :
			fieldOne := hubWords[1]
			if fieldOne == "good" || fieldOne == "bad" || fieldOne == "record" || fieldOne == "discard" {
				if hub.currentServiceName != "#snap" {
					hub.WriteString(text.ERROR + "you aren't taking a snap")
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
				hub.WriteString(text.ERROR + "missing test filename after " + text.Emph("as"))
				return false
			}
			hub.WriteString(text.ERROR + "the word " + text.Emph(hubWords[2]) + 
			/**/ "doesn't make any sense there")
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
				hub.WriteString(text.ERROR + "the word " + text.Emph(hubWords[2]) + 
				/**/ "doesn't make any sense there")
			return false
			}
		case 5 :
			if hubWords[4] == "with" {
				hub.WriteString(text.ERROR + "missing test filename after " + text.Emph("as"))
				return false
			}
			if hubWords[4] == "as" {
				hub.WriteString(text.ERROR + "missing data filename after " + text.Emph("with"))
				return false
			}
			hub.WriteString(text.ERROR + "the word " + text.Emph(hubWords[4]) + 
			/**/ "doesn't make any sense there")
			return false
		case 6 :
			if hubWords[2] != "with" && hubWords[2] != "as" {
				hub.WriteString(text.ERROR + "the word " + text.Emph(hubWords[2]) + 
				/**/ "doesn't make any sense there")
				return false
			}
			if hubWords[4] != "with" && hubWords[4] != "as" {
				hub.WriteString(text.ERROR + "the word " + text.Emph(hubWords[4]) + 
				/**/ "doesn't make any sense there")
				return false
			}
			if hubWords[2] == "with" && hubWords[4] == "with" {
				hub.WriteString(text.ERROR + "it doesn't make sense to say " + text.Emph("with") + 
				/**/ "twice")
				return false
			}
			if hubWords[2] == "as" && hubWords[4] == "as" {
				hub.WriteString(text.ERROR + "it doesn't make sense to say " + text.Emph("as") + 
				/**/ "twice")
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
			hub.WriteString(text.ERROR + "too many words after " + text.Emph("hub snap") + "\n")
			return false
		}
	case "test" :
		switch fieldCount {
		case 1 :
			hub.WriteString(text.ERROR + "the " + text.Emph("hub test") + 
			/**/ " command needs some parameters")
			return false
		case 2 :
			hub.TestScript(hubWords[1])
		default :
			hub.WriteString(text.ERROR + "too many words after " + text.Emph("hub test") + "\n")
		}
	default :
		hub.WriteString(text.ERROR + "the hub doesn't recognize the command " + 
		/**/ text.Emph(verb) + "\n")
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
hub.WriteString("Hub commands are:\n")
hub.WriteString("\n")
hub.WriteString(text.BULLET + "edit <filename>\n")
hub.WriteString(text.BULLET + "halt <topic>\n")
hub.WriteString(text.BULLET + "help <topic>\n")
hub.WriteString(text.BULLET + "list\n")
hub.WriteString(text.BULLET + "peek <on/off>\n")
hub.WriteString(text.BULLET + "quit\n")
hub.WriteString(text.BULLET + "reset <service name>\n")
hub.WriteString(text.BULLET + "replay <filename>\n")
hub.WriteString(text.BULLET + "run <filename> as <service name> with <datafile>\n")
hub.WriteString(text.BULLET + "snap\n")
hub.WriteString(text.BULLET + "test <filename>\n")
hub.WriteString("\n")
}


func (hub *Hub) WritePretty(s string) {
	for i :=0; i < len(s) ; {
		e := i + MARGIN
		j := 0
		if e > len(s) {
			j = len(s) - i
		} else {
			j = strings.LastIndexAny(s[i:e], " \n")
		}
		if j == -1 {
			j = MARGIN
		}
		hub.WriteString(s[i:i + j] + "\n")
		i = i + j + 1
	}
}

func (hub *Hub) WriteString(s string) {
	io.WriteString(hub.out, s)
}

var helpStrings = map[string] string {
	"edit" :
	text.Emph("hub edit") + " followed by a filename will open the file in vim.",
	"halt" :
	text.Emph("hub halt") + " followed by the name of a service will halt the service. " + 
	/**/ " If no service name is given, the hub will halt the current service.",
	"help" :
	text.Emph("hub help") + " followed by the name of a topic will supply you with " +
	/**/ "information on that topic.",
	"list" :
	text.Emph("hub list") + " will list all services currently running on the hub.",
	"peek" :
	text.Emph("hub peek") + " followed by " + text.Emph("on") + " or " + text.Emph("off") + 
	/**/ " will allow you to see what the lexer, relexer, and parser are doing. " + text.Emph("peek") + 
	/**/ " without a parameter toggles between on and off.",
	"reset" :
	text.Emph("hub reset") + " followed by the name of a service will reintialize the script " + 
	/**/ "and reread the associated data file, if there is one. If no service name is given " +
	/**/ "the hub will reset the current service.",
	"run" :
	text.Emph("hub run") + " without parameters will start a REPL with no script. With one parameter (a " +
	"valid filename) it will run the script as an anonymous service. By adding " + text.Emph("as <name>") +
	"you can name the service; by adding " + text.Emph("with <filename>") + " you can specify a datafile.",
	"quit" :
	text.Emph("hub quit") + " closes everything down.",
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
			hub.WriteString("\n" + text.ERROR + err.Error() + "\n")
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
	init.Parser = *parser.New()
	init.MakeParserAndTokenizedProgram()
	if init.ErrorsExist() {
		init.ReportScriptFailure()
		hub.currentServiceName = ""
		return
	}
	init.ParseImports()
	if init.ErrorsExist() {
		init.ReportScriptFailure()
		hub.currentServiceName = ""
		return
	}
	init.ImportEverything()
	if init.ErrorsExist() {
		init.ReportScriptFailure()
		hub.currentServiceName = ""
		return
	}

	init.ParseTypeDefs()
	if init.ErrorsExist() {
		init.ReportScriptFailure()
		hub.currentServiceName = ""
		return
	}

	env := object.NewEnvironment()
	init.EvaluateTypeDefs(env)
	if init.ErrorsExist() {
		init.ReportScriptFailure()
		hub.currentServiceName = ""
		return
	}

	init.ParseEverything()
	if init.ErrorsExist() {
		init.ReportScriptFailure()
		hub.currentServiceName = ""
		return
	}

	init.InitializeEverything(env)
	if init.ErrorsExist() {
		init.ReportScriptFailure()
		hub.currentServiceName = ""
		return
	}

	newService := NewService()
	(*newService).Parser = &init.Parser
	(*newService).Env = env
	(*newService).scriptFilepath = scriptFilepath
	hub.services[name] = newService

	return

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
        return text.ERROR + strings.TrimSpace(err.Error())
    }
    defer f.Close()
	for k := range hub.services {
		if !isAnonymous(k) && k != "#snap" && k != "#test" {
    		_, err := f.WriteString(k + ", " + hub.services[k].GetScriptFilepath() + ", " +
			/**/ hub.services[k].GetDataFilepath() + "\n")
    		if err != nil {
        		return text.ERROR + strings.TrimSpace(err.Error())
			}
    	}
	}
	f, err = os.Create("rsc/current.dat")
    if err != nil {
        return text.ERROR + strings.TrimSpace(err.Error())
    }
    defer f.Close()
	if isAnonymous(hub.currentServiceName) {
		_, err = f.WriteString("")
	} else {
		_, err = f.WriteString(hub.currentServiceName)
	}
	if err != nil {
		return text.ERROR + strings.TrimSpace(err.Error())
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
        hub.WriteString(text.ERROR + strings.TrimSpace(err.Error()))
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
        hub.WriteString(text.ERROR + strings.TrimSpace(err.Error()))
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
		hub.WriteString("service " + text.Emph(k) + " running script " + text.Emph(filepath.Base(hub.services[k].GetScriptFilepath())))
		if hub.services[k].GetDataFilepath() != "" {
			hub.WriteString(" with data " + text.Emph(filepath.Base(hub.services[k].GetDataFilepath())))
		}
		hub.WriteString("\n")
	}
	hub.WriteString("\n")
}



func (hub *Hub) TestScript(scriptFilepath string) {
	
	directoryName := "tst/" + text.FlattenedFilename(scriptFilepath)

	if _, err := os.Stat(directoryName); os.IsNotExist(err) {
		hub.WriteString(text.ERROR + strings.TrimSpace(err.Error()) + "\n")
		return
	}
	hub.oldServiceName = hub.currentServiceName
	files, _ := ioutil.ReadDir(directoryName)
	for _, testFileInfo := range files {
		testFilepath := directoryName + "/" + testFileInfo.Name()
		f, err := os.Open(testFilepath)
			if err != nil {
				hub.WriteString(text.ERROR + strings.TrimSpace(err.Error()) + "/n")
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
		hub.WriteString("Running test " + text.Emph(testFilepath) + ".\n")
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
				hub.WriteString(service.Parser.ReturnErrors())
				service.Parser.ClearErrors()
				f.Close()
				continue
			}
		executionMatchesTest = executionMatchesTest && (result == lineOut)
		}
		if executionMatchesTest && testType == BAD {
			hub.WriteString(text.ERROR + "bad behavior reproduced by test" + "\n")
			f.Close()
			hub.playTest(testFilepath, false) // not that it matters if it's true or false ...
			continue
		}
		if !executionMatchesTest && testType == GOOD {
			hub.WriteString(text.ERROR + "good behavior not reproduced by test" + "\n")
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
				hub.WriteString(text.ERROR + strings.TrimSpace(err.Error()) + "/n")
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
			hub.WriteString(service.Parser.ReturnErrors())
			service.Parser.ClearErrors()
			f.Close()
			return
		}
		hub.WriteString("#test → " + lineIn + "\n")
		if result == lineOut || !diffOn {
			hub.WriteString(result + "\n")
		} else {
			hub.WriteString("was: " + lineOut + "\ngot: " + result + "\n")
		}
	}
	return
}