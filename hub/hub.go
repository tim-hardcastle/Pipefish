package hub

import (
	"bufio"
	"bytes"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"charm/database"
	"charm/initializer"
	"charm/lexer"
	"charm/object"
	"charm/parser"
	"charm/relexer"
	"charm/text"
)

var (
	MARGIN = 84
)

type Hub struct {
	services               map[string]*Service
	ers                    object.Errors
	currentServiceName     string
	peek                   bool
	in                     io.Reader
	out                    io.Writer
	anonymousServiceNumber int
	snap                   *Snap
	oldServiceName         string // Somewhere to keep the old service name while taking a snap
	Sources                map[string][]string
	lastRun                []string
	CurrentForm            *Form
	db                     *sql.DB
	administered		   bool
	listeningToHttp		   bool
	Username			   string
	Password			   string
	path, port			   string		   
}

// Most initialization is done in the Open method.
func New(in io.Reader, out io.Writer) *Hub {
	hub := Hub{
		services: make(map[string]*Service),
		currentServiceName: "",
		in: in,
		out: out,
		lastRun: []string{},}
	return &hub
}

// This takes the input from the REPL, interprets it as a hub command if it begins with 'hub';
// as an instruction to switch services if it consists only of the name of a service; as
// an expression to be passed to a service if it begins with the name of a service; and as an
// expression to be passed to the current service if none of the above hold.
func (hub *Hub) Do(line, username, password, passedServiceName string) (string, bool) {

	if line == "" {
		hub.WriteString("")
		return passedServiceName, false
	}

	if hub.administered && !hub.listeningToHttp && hub.Password == "" &&
			!(line == "hub register" || line == "hub log in") { 
		hub.WriteError("this is an administered hub and you aren't logged in. Please enter either " +
			"'hub register' to register as a user, or 'hub log in' to log in if you're already registered " +
			"with this hub.")	
		return passedServiceName, false
	} 
	
	// We may be talking to the hub itself.

	hubWords := strings.Fields(line)
	if hubWords[0] == "hub" {
		hubResult := hub.ParseHubCommand(username, password, hubWords[1:])
		if len(hubWords) > 1 && hubWords[1] == "run" && hub.administered {
			serviceName, _ := database.ValidateUser(hub.db, username, password)
			return serviceName, false
		}
		return passedServiceName, hubResult
	}

	// We may be talking to the os

	if hubWords[0] == "os" {
		if len(hubWords) == 3 && hubWords[1] == "cd" { // Because cd changes the directory for the current
			os.Chdir(hubWords[2])    // process, if we did it with exec it would do it for
			hub.WriteString(text.OK) // that process and not for Charm.
			return passedServiceName, false
		}
		command := exec.Command(hubWords[1], hubWords[2:]...)
		out, err := command.Output()
		if err != nil {
			hub.WriteError(err.Error())
			return passedServiceName, false
		}
		if len(out) == 0 {
			hub.WriteString(text.OK)
			return passedServiceName, false
		}
		hub.WriteString(string(out))
		return passedServiceName, false
	}

	// Otherwise, we need to find a service to talk to.

	// If the first word of the line is the name of a service, we use that as the name of the service.

	var serviceName string

	service, ok := hub.services[hubWords[0]]
	if ok {
		serviceName := hubWords[0]
		if len(hubWords) == 1 {
			hub.currentServiceName = hubWords[0]
			hub.WriteString(text.OK + "\n")
			if hub.administered {
				database.UpdateService(hub.db, username, serviceName)
			}
			return serviceName, false
		} else {
			line = line[len(hubWords[0])+1:]
		}
	} else {
		service, ok = hub.services[passedServiceName]
		serviceName = passedServiceName
	}
	if !ok {
		hub.WriteError("the hub can't find the service '" + passedServiceName + "'.")
		return passedServiceName, false
	}

	// If we find a service and the hub is administered, we check that the user has access rights.
	
	if hub.administered {
		access, err := database.DoesUserHaveAccess(hub.db, username, serviceName)
		if err != nil { 
			hub.WriteError(err.Error())
			return passedServiceName, false
		}
		if !access { 
			hub.WriteError("you don't have access to service '" + serviceName + "'.")
			return passedServiceName, false
		}
	}
	if len(hubWords) == 1 && serviceName == hubWords[0] { return passedServiceName, false } // In this case we've validated and switched services and that's
	                                       // all we're trying to do.

	// If they have access rights, we pass the line to the service and get back a string to output.

	// But first, if hub peek is turned on, this will show us the wheels going round.
	if hub.peek {
		lexer.LexDump(line)
		relexer.RelexDump(line)
		service.Parser.ParseDump(hub.currentServiceName, line)
	}

	// Primitive built-in file-handling.

	if line == "save" || line == "open" || len(line) > 4 && (line[0:5] == "save " || line[0:5] == "open ") {
		obj := service.Do(line)
		if obj.Type() == object.ERROR_OBJ {
			hub.WritePretty("\n[0] " + objToString(service, obj))
			hub.WriteString("\n")
			hub.ers = []*object.Error{obj.(*object.Error)}
		} else {
			hub.WriteString(objToString(service, obj))
		}
		return passedServiceName, false
	}

	// Errors in the parser are a signal for the parser/initializer to halt, so we need to clear them here.
	service.Parser.ClearErrors()

	hub.Sources["REPL input"] = []string{line}

	if service.Parser.ErrorsExist() {
		hub.GetAndReportErrors(service.Parser)
		return passedServiceName, false
	}

	obj := service.Do(line)

	if service.Parser.ErrorsExist() {
		hub.GetAndReportErrors(service.Parser)
		return passedServiceName, false
	}

	if obj.Type() == object.ERROR_OBJ {
		hub.WritePretty("\n[0] " + objToString(service, obj))
		hub.WriteString("\n")
		hub.ers = []*object.Error{obj.(*object.Error)}
	} else {
		hub.WriteString(objToString(service, obj))
		for k, v := range service.Env.Pending {
			service.Env.HardSet(k, v)
		}
	}
	service.Env.Pending = make(map[string]object.Object)

	if hub.currentServiceName == "#snap" {
		hub.snap.AddInput(line)
		hub.snap.AddOutput(objToString(service, obj))
	}
	return passedServiceName, false
}

func (hub *Hub) ParseHubCommand(username, password string, hubWords []string) bool { // Returns true if the command is 'quit', since it can't quit
	fieldCount := len(hubWords) // from the REPL itself.
	if fieldCount == 0 {
		hub.help()
		return false
	}
	verb := hubWords[0]
	switch verb {

	// Verbs are in alphabetical order :
	// add, config, create, edit, errors, help, let, log, listen, my, peek, quit, register, replay, run, services, snap, test, trace
	case "add":
		if fieldCount != 4 || hubWords[2] != "to" {
			hub.WriteError("the format of the 'hub add' command is 'hub add <username> to <GroupName>'.")
			return false
		}
		err := database.IsUserGroupOwner(hub.db, username, hubWords[3])
		if err != nil { 
			hub.WriteError(err.Error()) 
			return false
		}
		err = database.AddUserToGroup(hub.db, hubWords[1], hubWords[3], false)
		if err != nil { 
			hub.WriteError(err.Error()) 
			return false
		}
		hub.WriteString(text.OK + "\n")
	case "config":
		switch {
		case fieldCount == 1:
			hub.WriteError("the 'hub config' command needs you to say what you want to configure, " +
				"either 'admin' or 'db'.")
		case fieldCount > 2:
			hub.WriteError("the 'hub config' command takes at most one parameter, either 'admin' or 'db'.")
		default:
			switch hubWords[1] {
			case "admin":
				_, err := os.Stat("rsc/admin.dat");
				if errors.Is(err, os.ErrNotExist) {
					hub.configAdmin()
					return false
				}
				if err != nil { 
					hub.WriteError(err.Error()) 
					return false
				}
				hub.WriteError("you've already set up administration for this hub.")
			case "db":
				hub.configDb()
			}
		}
		return false
	case "create":
		switch {
		case fieldCount == 1:
			hub.WriteError("the 'hub create' command requires the name of a usergroup to create as a parameter.")
		case fieldCount > 2:
			hub.WriteError("the 'hub create' command takes at most one parameter.")
		default:
			isAdmin, err := database.IsUserAdmin(hub.db, username)
			if err != nil {
				hub.WriteError(err.Error())
				return false
			}
			if !isAdmin {
				hub.WriteError("you don't have the admin status necessary to do that.")
				return false
			}
			err = database.AddGroup(hub.db, hubWords[1])
			if err != nil {
				hub.WriteError(err.Error())
			}
		}
		err := database.AddUserToGroup(hub.db, username, hubWords[1], true)
		if err != nil {
			hub.WriteError(err.Error())
		}
		hub.WriteString(text.OK + "\n")
	case "edit":
		switch {
		case fieldCount == 1:
			hub.WriteError("the 'hub edit' command requires a filename as a parameter.")
		case fieldCount > 2:
			hub.WriteError("the 'hub edit' command takes at most one parameter.")
		default:
			command := exec.Command("vim", hubWords[1])
			command.Stdin = os.Stdin
			command.Stdout = os.Stdout
			err := command.Run()
			if err != nil {
				hub.WriteError(err.Error())
			}
		}
	case "errors":
		if fieldCount > 1 {
			hub.WriteError("the 'errors' keyword takes no parameters.")
		}
		if len(hub.ers) == 0 {
			hub.WritePretty("There are no recent errors.")
		}
		hub.WritePretty(object.GetList(hub.ers))
	case "halt":
		name := hub.currentServiceName
		if fieldCount > 2 {
			hub.WriteError("the 'hub halt' command takes at most one parameter, the name of a service.")
		}
		ok := true
		if fieldCount == 2 {
			_, ok = hub.services[hubWords[1]]
			if ok {
				name = hubWords[1]
			} else {
				hub.WriteError("the hub can't find the service '" + hubWords[1] + "'.")
				return false
			}
		}
		if name == "" {
			hub.WriteError("the hub doesn't know what you want to halt.")
			return false
		}
		delete(hub.services, name)
		hub.WriteString(text.OK + "\n")
		if name == hub.currentServiceName {
			hub.currentServiceName = ""
		}
		return false
	case "help":
		switch {
		case fieldCount == 1:
			hub.help()
		case fieldCount > 2:
			hub.WriteError("the 'hub help' command takes at most one parameter.")
		default:
			if helpMessage, ok := helpStrings[hubWords[1]]; ok {
				hub.WritePretty(helpMessage + "\n")
			} else {
				hub.WriteError("the 'hub help' command doesn't accept " +
					"'" + hubWords[1] + "' as a parameter.")
			}
		}
	case "join":
		if fieldCount > 1 {
			hub.WriteError("the 'hub quit' command takes no parameters.")
		} else {
			hub.addUserAsGuest()
			return false
		}
	case "let":
		if fieldCount != 4 || hubWords[2] != "use" {
			hub.WriteError("the format of the 'hub let' command is 'hub let <GroupName> use <SERVICE>'.")
			return false
		}
		isAdmin, err := database.IsUserAdmin(hub.db, username)
		if err != nil {
			hub.WriteError(err.Error())
			return false
		}
		if !isAdmin {
			hub.WriteError("you don't have the admin status necessary to do that.")
			return false
		}
		err = database.LetGroupUseService(hub.db, hubWords[1], hubWords[3])
		if err != nil { 
			hub.WriteError(err.Error()) 
			return false
		}
		hub.WriteString(text.OK + "\n")
	case "listen":
		switch {
		case fieldCount == 3:
			hub.WriteString(text.OK)
			hub.WriteString("\nHub is listening.\n\n")
			hub.StartHttp(hubWords[1], hubWords[2])
		default:
			hub.WriteError("the 'hub listen' command takes two parameters, a path and a port.")
		}
	case "log":
		switch {
		case fieldCount == 1:
			hub.WriteError("'hub log' takes a parameter, either 'in' or 'out'.")
		case fieldCount > 2:
			hub.WriteError("'hub log' takes only one parameter, either 'in' or 'out'.")
		case hubWords[1] == "in" :
			hub.getLogin()
		case hubWords[1] == "out" :
			hub.Username = ""
			hub.Password = ""
			hub.currentServiceName = ""
			hub.WriteString("\n" + text.OK + "\n")
			hub.WritePretty("\nThis is an administered hub and you aren't logged in. Please enter either " +
			"'hub register' to register as a user, or 'hub log in' to log in if you're already registered " +
			"with this hub.\n\n")
		default:
			hub.WriteError("'hub log' should be followed either by 'in' or 'out'.")
		}
	case "my":
		switch {
		case fieldCount == 1:
			hub.WriteError("something should follow 'my': either 'groups' or 'services'")
		case fieldCount > 2:
			hub.WriteError("only one word should follow 'my': either 'groups' or 'services'")
		default:
			switch hubWords[1] {
			case "groups" : 
				result, err := database.GetGroupsForUser(hub.db, username)
				if err!= nil { 
					hub.WriteError(err.Error())
				} else {
					hub.WriteString(result)
				}
			case "services" :
				result, err := database.GetAccessForUser(hub.db, username)
				if err!= nil { 
					hub.WriteError(err.Error())
				} else {
					hub.WriteString(result)
				}
			default:
				hub.WriteError("the only things that can follow 'my' are 'groups' or 'services'.")
			}
		}
	case "peek":
		switch {
		case fieldCount == 1:
			hub.peek = !hub.peek
		case fieldCount == 2:
			switch hubWords[1] {
			case "on":
				hub.peek = true
			case "off":
				hub.peek = false
			default:
				hub.WriteError("the 'hub peek' command only accepts the parameters " +
					"'on'  or 'off'.")
			}
		default:
			hub.WriteError("the 'hub peek' command only accepts a single parameter, " +
				"'on'  or 'off'.")

		}
	case "quit":
		if fieldCount > 1 {
			hub.WriteError("the 'hub quit' command takes no parameters.")
		} else {
			hub.quit()
			return true
		}

	case "register":
		if fieldCount > 1 {
			hub.WriteError("the 'hub register' command takes no parameters.")
		} else {
			hub.addUserAsGuest()
		}

	case "replay":

		hub.oldServiceName = hub.currentServiceName

		switch {
		case fieldCount == 2:
			hub.playTest(hubWords[1], false)
		case fieldCount == 3:
			if hubWords[2] != "diff" {
				hub.WriteError("the word '" + hubWords[2] +
					"' makes no sense there.")
			} else {
				hub.playTest(hubWords[1], true)
			}
		default:
			hub.WriteError("the 'hub replay'" +
				" command takes the filepath of a test as a parameter, optionally" +
				" followed by 'diff'.")
		}

		hub.currentServiceName = hub.oldServiceName

		_, ok := hub.services["#test"]
		if ok {
			delete(hub.services, "#test")
		}
	case "reset":
		if len(hubWords) > 2 {
			hub.WriteError("the " + text.Emph("hub reset") + " command takes at most one parameter, the name of a service.")
		}
		service, ok := hub.services[hub.currentServiceName]
		if len(hubWords) == 2 {
			service, ok = hub.services[hubWords[1]]
		}
		if !ok {
			hub.WriteError("the hub can't find the service '" + hub.currentServiceName + "'.")
			return false
		}
		if hub.currentServiceName == "" {
			hub.WriteError("service is empty, nothing to reset.")
			return false
		}
		hub.WritePretty("Restarting script '" + service.GetScriptFilepath() +
			"' as service '" + hub.currentServiceName + "'.\n")
		hub.Start(username, hub.currentServiceName, service.GetScriptFilepath())
		if service.GetDataFilepath() != "" {
			service.OpenDataFile(service.GetDataFilepath())
		}
		return false
	case "rerun":
		if len(hubWords) > 1 {
			hub.WriteError("the 'hub rerun' command takes no parameters.")
			return false
		}
		if len(hub.lastRun) == 0 {
			hub.WriteError("nothing to rerun.")
			return false
		}
		hub.ParseHubCommand(username, password, hub.lastRun)
		return false
	case "run":
		hub.lastRun = hubWords
		switch fieldCount {
		case 1:
			hub.currentServiceName = ""
			return false
		case 2:
			hub.WritePretty("Starting script '" + hubWords[1] +
				"' as service '#" + strconv.Itoa(hub.anonymousServiceNumber) + "'.\n")
			hub.StartAnonymous(hubWords[1])
			return false
		case 3:
			if hubWords[2] == "as" {
				hub.WriteError("missing service name after 'as'.")
				hub.lastRun = []string{}
				return false
			}
			hub.WriteError("the word '" + hubWords[2] + "' doesn't make any sense there.")
			hub.lastRun = []string{}
			return false
		case 4:
			if hubWords[2] != "as" {
				hub.WriteError("the word '" + hubWords[2] + "' doesn't make any sense there.")
				hub.lastRun = []string{}
				return false
			}
			hub.WritePretty("Starting script '" + hubWords[1] + "' as service '" + hubWords[3] + "'.\n")
			hub.Start(username, hubWords[3], hubWords[1])
		default:
			hub.lastRun = []string{}
			hub.WriteError("too many words after 'hub run'.")
			return false
		}
	case "services":
		switch {
		case fieldCount == 1:
			if len(hub.services) == 1 {
				hub.WriteString("\nThe hub is not presently running any services.\n")
			}
			hub.WriteString("\n")
			hub.list()
		default:
			hub.WriteError("the 'hub services' command takes no parameters.")
			return false
		}
	case "snap":
		switch fieldCount {
		case 1:
			hub.WriteError("the 'hub snap' command needs some parameters.")
			return false
		case 2:
			fieldOne := hubWords[1]
			if fieldOne == "good" || fieldOne == "bad" || fieldOne == "record" || fieldOne == "discard" {
				if hub.currentServiceName != "#snap" {
					hub.WriteError("you aren't taking a snap.")
					return false
				}
			}
			switch fieldOne {
			case "good":
				result := hub.snap.Save(GOOD)
				hub.WriteString(result + "\n")
			case "bad":
				result := hub.snap.Save(BAD)
				hub.WriteString(result + "\n")
			case "record":
				result := hub.snap.Save(RECORD)
				hub.WriteString(result + "\n")
			case "discard":
				hub.WriteString(text.OK + "\n")
			default:
				scriptFilepath := fieldOne
				testFilename := getUnusedTestFilename(scriptFilepath) // If no filename is given, we just generate one.
				hub.snap = NewSnap(scriptFilepath, testFilename, "")
				hub.oldServiceName = hub.currentServiceName
				hub.Start(username, "#snap", scriptFilepath)
				(*hub).services["#snap"].Do("$view = \"charm\"")
				hub.WriteString("Serialization is ON.\n")
			}
			if fieldOne == "good" || fieldOne == "bad" || fieldOne == "record" || fieldOne == "discard" {
				hub.currentServiceName = hub.oldServiceName
			}
			return false
		case 3:
			if hubWords[2] == "as" {
				hub.WriteError("missing test filename after 'as'.")
				return false
			}
			hub.WriteError("the word '" + text.Emph(hubWords[2]) +
				"' doesn't make any sense there")
			return false
		case 4:
			switch hubWords[2] {
			case "as":
				scriptFilepath := hubWords[1]
				testFilepath := hubWords[3]
				hub.snap = NewSnap(scriptFilepath, testFilepath, "")
				hub.oldServiceName = hub.currentServiceName
				hub.Start(username, "#snap", scriptFilepath)
				(*hub).services["#snap"].Do("$view = \"charm\"")
				hub.WriteString("Serialization is ON.\n")
			case "with":
				scriptFilepath := hubWords[1]
				dataFilepath := hubWords[3]
				testFilename := getUnusedTestFilename(scriptFilepath) // If no filename is given, we just generate one.
				hub.snap = NewSnap(scriptFilepath, testFilename, dataFilepath)
				hub.oldServiceName = hub.currentServiceName
				hub.Start(username, "#snap", scriptFilepath)
				hub.services["#snap"].OpenDataFile(dataFilepath)
				(*hub).services["#snap"].Do("$view = \"charm\"")
				hub.WriteString("Serialization is ON.\n")
			default:
				hub.WriteError("the word '" + hubWords[2] +
					"' doesn't make any sense there.")
				return false
			}
		case 5:
			if hubWords[4] == "with" {
				hub.WriteError("missing test filename after 'as'")
				return false
			}
			if hubWords[4] == "as" {
				hub.WriteError("missing data filename after 'with'.")
				return false
			}
			hub.WriteError("the word '" + hubWords[4] +
				"' doesn't make any sense there.")
			return false
		case 6:
			if hubWords[2] != "with" && hubWords[2] != "as" {
				hub.WriteError("the word '" + hubWords[2] +
					"' doesn't make any sense there.")
				return false
			}
			if hubWords[4] != "with" && hubWords[4] != "as" {
				hub.WriteError("the word '" + hubWords[4] +
					"' doesn't make any sense there.")
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
			hub.Start(username, "#snap", scriptFilepath)
			(*hub).services["#snap"].Do("$view = \"charm\"")
			hub.WriteString("Serialization is ON.\n")
			hub.oldServiceName = hub.currentServiceName
			return false
		default:
			hub.WriteError("too many words after 'hub snap'.")
			return false
		}
	case "test":
		switch fieldCount {
		case 1:
			hub.WriteError("the 'hub test' command needs some parameters.")
			return false
		case 2:
			hub.TestScript(hubWords[1])
		default:
			hub.WriteError("too many words after 'hub test'.")
		}
	case "trace":
		switch fieldCount {
		case 1:
			if len(hub.ers) == 0 {
				hub.WriteError("there are no recent errors.")
				return false
			}
			if len(hub.ers[0].Trace) == 0 {
				hub.WriteError("not a runtime error.")
				return false
			}
			hub.WritePretty("\n$Runtime error$" + hub.ers[0].Message + "\n\n") // If it is a runtime error, then there is only one of them.
			for i := len(hub.ers[0].Trace) - 1; i >= 0; i-- {
				hub.WritePretty("  From: " + text.DescribeTok(hub.ers[0].Trace[i]) + text.DescribePos(hub.ers[0].Trace[i]) + ".")
			}
			hub.WriteString("\n")
			return false
		default:
			hub.WriteError("the 'hub trace' command takes no parameters.")
			return false
		}
	case "where":
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
		line := hub.Sources[hub.ers[num].Token.Source][hub.ers[num].Token.Line-1] + "\n"
		startUnderline := hub.ers[num].Token.ChStart
		lenUnderline := hub.ers[num].Token.ChEnd - startUnderline
		if lenUnderline == 0 {
			lenUnderline = 1
		}
		endUnderline := startUnderline + lenUnderline
		hub.WriteString(line[0:startUnderline])
		hub.WriteString(text.Red(line[startUnderline:endUnderline]))
		hub.WriteString(line[endUnderline:])
		hub.WriteString(strings.Repeat(" ", startUnderline))
		hub.WriteString(text.Red(strings.Repeat("▔", lenUnderline)))
		return false
	case "why":
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
		refLine = "\n" + strings.Repeat(" ", MARGIN-len(refLine)-2) + refLine
		hub.WritePretty(refLine)
		hub.WriteString("\n")
	default:
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
		tryName = name + strconv.Itoa(tryNumber)
		_, error := os.Stat("tst/" + directoryName + "/" + tryName)
		if os.IsNotExist(error) {
			break
		}
	}
	return tryName
}

func (hub *Hub) quit() {
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
	codeWidth := -1
	highlighter := ' '
	for i := 0; i < len(s); {
		e := i + MARGIN
		j := 0
		if e > len(s) {
			j = len(s) - i
		} else if strings.Contains(s[i:e], "\n") {
			j = strings.Index(s[i:e], "\n")
		} else {
			j = strings.LastIndex(s[i:e], " ")
		}
		if j == -1 {
			j = MARGIN
		}
		if strings.Contains(s[i:i+j], "\n") {
			j = strings.Index(s[i:i+j], "\n")
		}

		plainLine := s[i : i+j]
		if len(plainLine) >= 2 && plainLine[0:2] == "|-" {
			if codeWidth > 0 {
				hub.WriteString(" └──" + strings.Repeat("─", codeWidth) + "┘\n")
				codeWidth = -1
			} else {
				codeWidth = len(plainLine)
				hub.WriteString(" ┌──" + strings.Repeat("─", codeWidth) + "┐\n")
			}
		} else if codeWidth > 0 {
			hub.WriteString(" │  " + text.Cyan(plainLine) + strings.Repeat(" ", codeWidth-len(plainLine)) + "│\n")
		} else {
			hub.WriteString(text.HighlightLine(plainLine, highlighter) + "\n")
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

var helpStrings = map[string]string{}

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
	for _, v := range lines {
		v = strings.TrimRight(v, " \n")
		if v == "***" {
			helpTopics = append(helpTopics, strings.TrimSpace(helpMessage[0]))
			helpStrings[strings.TrimSpace(helpMessage[0])] = strings.Join(helpMessage[1:], "\n")
			helpMessage = []string{}
		} else {
			helpMessage = append(helpMessage, v)
		}
	}
	sort.Strings(helpTopics)
}

func (hub *Hub) StartAnonymous(scriptFilepath string) {
	hub.Start("", "#"+fmt.Sprint(hub.anonymousServiceNumber), scriptFilepath)
	hub.anonymousServiceNumber = hub.anonymousServiceNumber + 1
}

func (hub *Hub) Start(username, serviceName, scriptFilepath string) {
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
	if hub.administered {
		err := database.UpdateService(hub.db, username, serviceName)
		if err != nil {
			hub.WriteError(err.Error())
			return
		}
	}
	hub.currentServiceName = serviceName
	hub.createService(serviceName, scriptFilepath, code)
}

func (hub *Hub) createService(name, scriptFilepath, code string) {
	init := initializer.New(scriptFilepath, code)
	init.GetSource(scriptFilepath)
	init.Parser = *parser.New()
	for k, v := range(hub.services) {
		init.Parser.Parsers[k] = v.Parser
	}
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

}

func (hub *Hub) GetAndReportErrors(p *parser.Parser) {
	hub.ers = p.Errors
	hub.WritePretty(object.GetList(hub.ers))
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
				hub.services[k].GetDataFilepath() + "\n")
			if err != nil {
				return text.HUB_ERROR + "os reports \"" + strings.TrimSpace(err.Error()) + "\".\n"
			}
		}
	}
	f, err = os.Create("rsc/current.dat")
	if err != nil {
		return text.HUB_ERROR + "os reports \"" + strings.TrimSpace(err.Error()) + "\".\n"
	}

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
	if serviceName == "" {
		return true
	}
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
		hub.Start("", params[0], params[1])
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

	scanner = bufio.NewScanner(f)
	scanner.Scan()
	hub.currentServiceName = scanner.Text()

	_, err = os.Stat("rsc/admin.dat");
	hub.administered = (err == nil)
	if hub.administered { hub.currentServiceName = "" }

	// If the database configuration doesn't exist this is because the user
	// hasn't created it yet, so we just skip the setup.
	if _, err := os.Stat("rsc/database.dat"); !errors.Is(err, os.ErrNotExist) {
		
		fileBytes, err := os.ReadFile("rsc/database.dat")

		if err != nil {
			hub.WriteError(err.Error())
			return
		}

		params := strings.Split(string(fileBytes), "\n")
		
		hub.db, err = database.GetdB(params[0], params[1], params[2], 
			params[3], params[4], params[5])

		if err != nil {
			hub.WriteError(err.Error())
			return
		}
		if hub.administered && !hub.listeningToHttp {
			hub.WritePretty("This is an administered hub and you aren't logged in. Please enter either " +
			"'hub register' to register as a user, or 'hub log in' to log in if you're already registered " +
			"with this hub.\n\n")
		}
	}
}

func (hub *Hub) list() {
	if len(hub.services) == 1 {
		return
	}
	hub.WriteString("The hub is running the following services:\n\n")
	for k := range hub.services {
		if k == "" {
			continue
		}
		hub.WritePretty(text.BULLET + "Service '" + k + "' running script '" + filepath.Base(hub.services[k].GetScriptFilepath()) + "'.")
		if hub.services[k].GetDataFilepath() != "" {
			hub.WriteString(" with data '" + filepath.Base(hub.services[k].GetDataFilepath()) + "'")
		}
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
	files, _ := os.ReadDir(directoryName)
	for _, testFileInfo := range files {
		testFilepath := directoryName + "/" + testFileInfo.Name()
		f, err := os.Open(testFilepath)
		if err != nil {
			hub.WriteError(strings.TrimSpace(err.Error()) + "/n")
			return
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
		hub.Start("", "#test", scriptFilepath)
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
	_, ok := hub.services["#test"]
	if ok {
		delete(hub.services, "#test")
	}
	hub.currentServiceName = hub.oldServiceName

}

func (hub *Hub) playTest(testFilepath string, diffOn bool) {
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
	hub.Start("", "#test", scriptFilepath)
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
}

func objToString(service *Service, obj object.Object) string {

	value, _ := service.Env.Get("$view")
	switch value.(*object.String).Value {
	case "charm":
		return obj.Inspect(object.ViewCharmLiteral)
	case "plain":
		return obj.Inspect(object.ViewStdOut)
	default:
		panic("I don't know what's going on any more.")
	}
}

func (h *Hub) StartHttp(path, port string) {
	h.path = path
	h.port = port
	h.listeningToHttp = true
	if h.administered {
		http.HandleFunc(path, h.handleJsonRequest)
	} else {
		http.HandleFunc(path, h.handleSimpleRequest)
	}
	err := http.ListenAndServe(":"+port, nil)
	if errors.Is(err, http.ErrServerClosed) {
		h.WriteError("server closed.")
	} else if err != nil {
		h.WriteError("error starting server: " + err.Error())
		return
	}
}

// This will simply feed text to the REPL of the hub, and will happen if you
// tell the interpreter to turn into a server but don't ask for administration.
func (h *Hub) handleSimpleRequest(w http.ResponseWriter, r *http.Request) {
	body, err := io.ReadAll(r.Body)
	if err != nil {
		fmt.Printf("could not read body: %s\n", err)
	}
	input := string(body[:])
	h.out = w
	h.Do(input, "", "", h.currentServiceName)
	io.WriteString(w, "\n")
}

// By contrast, once the hub is administered it expects an HTTP request to consist of JSON
// containing the line to be executed and the username and password of the user.
type jsonRequest = struct{
	Body string
	Username string
	Password string
}

type jsonResponse = struct{
	Body string
	Service string
}

func (h *Hub) handleJsonRequest(w http.ResponseWriter, r *http.Request) {
	
	var request jsonRequest

	err := json.NewDecoder(r.Body).Decode(&request)
    if err != nil {
        http.Error(w, err.Error(), http.StatusBadRequest)
        return
    }

	var serviceName string

	if h.administered && !((!h.listeningToHttp) && (request.Body == "hub register" || request.Body == "hub log in")) { 
		serviceName, err = database.ValidateUser(h.db, request.Username, request.Password)
		if err != nil {
			h.WriteError(err.Error())
			return
		}
	}

	var buf bytes.Buffer
	h.out = &buf
	serviceName, _ = h.Do(request.Body, request.Username, request.Password, serviceName)

	response := jsonResponse{Body: buf.String(), Service: serviceName}

	json.NewEncoder(w).Encode(response)

}

// So, the Form type. Yes, I basically am reinventing the object here because the fields of 
// a struct aren't first-class objects in Go, unlike other superior langages I could name.
type Form struct { // For when the hub wants to initiate structured input
	Fields []string
	Result map[string]string
	Call   func(f *Form)
}

func (h *Hub) addUserAsGuest() {
	h.CurrentForm = &Form{Fields: []string{"Username", "First name", "Last name", "Email", "*Password", "*Confirm password"},
		Call:   func(f *Form) { h.handleConfigUserForm(f) },
		Result: make(map[string]string)}
}

func (h *Hub) handleConfigUserForm(f *Form) {
	h.CurrentForm = nil
	_, err := os.Stat("rsc/admin.dat");
				if errors.Is(err, os.ErrNotExist) {
					h.WriteError("this Charm hub doesn't have administered " + 
						"access: there is nothing to join.")
					return
				}
				if err != nil { 
					h.WriteError(err.Error()) 
					return
				}
	if f.Result["*Password"] != f.Result["*Confirm password"] {
		h.WriteError("passwords don't match.")
		return
	}

	err = database.AddUser(h.db, f.Result["Username"], f.Result["First name"], 
		f.Result["Last name"], f.Result["Email"], f.Result["*Password"], "")
	if err != nil {
		h.WriteError(err.Error())
		return
	}
	err = database.AddUserToGroup(h.db, f.Result["Username"], "Guests", false)
	if err != nil {
		h.WriteError(err.Error())
		return
	}
	h.Username = f.Result["Username"]
	h.Password = f.Result["*Password"]
	h.WritePretty("You are logged in as '" + h.Username + "'.\n")
}

func (h *Hub) configAdmin() {
	h.CurrentForm = &Form{Fields: []string{"Username", "First name", "Last name", "Email", "*Password", "*Confirm password"},
		Call:   func(f *Form) { h.handleConfigAdminForm(f) },
		Result: make(map[string]string)}
}

func (h *Hub) handleConfigAdminForm(f *Form) {
	h.CurrentForm = nil
	if h.db == nil {
		h.WriteError("database has not been configured: do 'hub config db' first.")
		return
	}
	if f.Result["*Password"] != f.Result["*Confirm password"] {
		h.WriteError("passwords don't match.")
		return
	}
	err := database.AddAdmin(h.db, f.Result["Username"], f.Result["First name"], 
		f.Result["Last name"], f.Result["Email"], f.Result["*Password"], h.currentServiceName)
	if err != nil {
		h.WriteError(err.Error())
		return
	}
	h.WriteString(text.OK + "\n")
	h.Username = f.Result["Username"]
	h.Password = f.Result["*Password"]
	h.WritePretty("You are logged in as '" + h.Username + "'.\n")

	h.administered = true

	// If the hub's already an HTTP server we should restart it to tell it to expect Json.
	if h.listeningToHttp {
		h.StartHttp(h.path, h.port)
	}
}

func (h *Hub) getLogin() {
	h.CurrentForm = &Form{Fields: []string{"Username", "*Password"},
		Call:   func(f *Form) { h.handleLoginForm(f) },
		Result: make(map[string]string)}
}

func (h *Hub) handleLoginForm(f *Form) {
	h.CurrentForm = nil
	_, err := database.ValidateUser(h.db, f.Result["Username"], f.Result["*Password"])
		if err != nil {
			h.WriteError(err.Error())
			h.WriteString("Please try again.\n\n")
			return
		}
	h.Username = f.Result["Username"]
	h.Password = f.Result["*Password"]
	h.WriteString(text.OK + "\n")
}

func (h *Hub) configDb() {
	h.CurrentForm = &Form{Fields: []string{database.GetDriverOptions(), "Host", "Port", "Database", "Username for database access", "*Password for database access"},
		Call:   func(f *Form) { h.handleConfigDbForm(f) },
		Result: make(map[string]string)}
}

func (h *Hub) handleConfigDbForm(f *Form) {
	h.CurrentForm = nil
	number, err := strconv.Atoi(f.Result[database.GetDriverOptions()])
	if err != nil {
		h.WriteError(err.Error())
		return
	}

	h.db, err = database.GetdB(database.GetSortedDrivers()[number], f.Result["Host"], f.Result["Port"],
		f.Result["Database"], f.Result["Username for database access"], f.Result["*Password for database access"])

	if err != nil {
		h.WriteError(err.Error())
		return
	}

	fi, err := os.Create("rsc/database.dat")
	if err != nil {
		h.WriteError(err.Error())
	}

	fi.WriteString(database.GetSortedDrivers()[number] + "\n")
	fi.WriteString(f.Result["Host"] + "\n")
	fi.WriteString(f.Result["Port"] + "\n")
	fi.WriteString(f.Result["Database"] + "\n")
	fi.WriteString(f.Result["Username for database access"] + "\n")
	fi.WriteString(f.Result["*Password for database access"] + "\n")

	h.WriteString(text.OK + "\n")
}