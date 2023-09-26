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

	"charm/source/database"
	"charm/source/evaluator"
	"charm/source/initializer"
	"charm/source/lexer"
	"charm/source/object"
	"charm/source/parser"
	"charm/source/relexer"
	"charm/source/text"
)

var (
	MARGIN = 84
)

type Hub struct {
	services               map[string]*parser.Service
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
	Db                     *sql.DB
	administered           bool
	listeningToHttp        bool
	Username               string
	Password               string
	path, port             string
	hot                    bool
}

// Most initialization is done in the Open method.
func New(in io.Reader, out io.Writer) *Hub {
	hub := Hub{
		services:           make(map[string]*parser.Service),
		currentServiceName: "",
		in:                 in,
		out:                out,
		lastRun:            []string{},
		hot:                true}
	return &hub
}

// This takes the input from the REPL, interprets it as a hub command if it begins with 'hub';
// as an instruction to the os if it begins with 'os,
// and as an expression to be passed to the current service if none of the above hold.
func (hub *Hub) Do(line, username, password, passedServiceName string) (string, *object.Effects) {

	if line == "" {
		hub.WriteString("")
		return passedServiceName, object.OK_RESPONSE
	}

	if hub.administered && !hub.listeningToHttp && hub.Password == "" &&
		!(line == "hub register" || line == "hub log on" || line == "hub quit") {
		hub.WriteError("this is an administered hub and you aren't logged on. Please enter either " +
			"'hub register' to register as a user, or 'hub log on' to log on if you're already registered " +
			"with this hub.")
		return passedServiceName, object.OK_RESPONSE
	}

	// We may be talking to the hub itself.

	hubWords := strings.Fields(line)
	if hubWords[0] == "hub" {
		if len(line) == 3 {
			hub.WriteError("you need to say what you want the hub to do.")
			return passedServiceName, object.OK_RESPONSE
		}
		verb, args := hub.ParseHubCommand(line[4:])
		if verb == "error" {
			return passedServiceName, object.OK_RESPONSE
		}
		hubResult := hub.DoHubCommand(username, password, verb, args)
		if len(hubWords) > 1 && hubWords[1] == "run" && hub.administered { // TODO: find out what it does and where it should be now that we have ++ for hub commands.
			serviceName, _ := database.ValidateUser(hub.Db, username, password)
			return serviceName, object.OK_RESPONSE
		}
		if hubResult {
			return passedServiceName, object.QUIT_RESPONSE
		} else {
			return passedServiceName, object.OK_RESPONSE
		}
	}

	// We may be talking to the os

	if hubWords[0] == "os" {
		if hub.isAdministered() {
			hub.WriteError("for reasons of safety and sanity, the 'os' prefix doesn't work in administered hubs.")
			return passedServiceName, object.OK_RESPONSE
		}
		if len(hubWords) == 3 && hubWords[1] == "cd" { // Because cd changes the directory for the current
			os.Chdir(hubWords[2])    // process, if we did it with exec it would do it for
			hub.WriteString(text.OK) // that process and not for Charm.
			return passedServiceName, object.OK_RESPONSE
		}
		command := exec.Command(hubWords[1], hubWords[2:]...)
		out, err := command.Output()
		if err != nil {
			hub.WriteError(err.Error())
			return passedServiceName, object.OK_RESPONSE
		}
		if len(out) == 0 {
			hub.WriteString(text.OK)
			return passedServiceName, object.OK_RESPONSE
		}
		hub.WriteString(string(out))
		return passedServiceName, object.OK_RESPONSE
	}

	// Otherwise, we're talking to the current service.

	service, ok := hub.services[passedServiceName]
	if !ok {
		hub.WriteError("the hub can't find the service '" + passedServiceName + "'.")
		return passedServiceName, object.OK_RESPONSE
	}

	// The service may be broken, in which case we'll let the empty service handle the input.

	if service.Broken {
		service = hub.services[""]
	}

	// If hub peek is turned on, this will show us the wheels going round.
	if hub.peek {
		lexer.LexDump(line)
		relexer.RelexDump(line)
		service.Parser.ParseDump(hub.currentServiceName, line)
	}

	// Errors in the parser are a signal for the parser/initializer to halt, so we need to clear them here.
	// They may be sitting around so the end-user can do 'hub why', but we can get rid of them now.
	service.Parser.ClearErrors()

	hub.Sources["REPL input"] = []string{line}

	if service.Parser.ErrorsExist() {
		hub.GetAndReportErrors(service.Parser)
		return passedServiceName, object.OK_RESPONSE
	}

	// If hotcoding is on, we recreate the service. createService, which is called by Start, will test if the
	// service actually needs updating.
	if hub.hot {
		hub.Start(hub.Username, hub.currentServiceName, hub.services[hub.currentServiceName].ScriptFilepath)
		service = hub.services[hub.currentServiceName]
		if service.Broken {
			return passedServiceName, object.OK_RESPONSE
		}
	}

	// *** THIS IS THE BIT WHERE WE DO THE THING!
	obj := ServiceDo(service, line)
	// *** FROM ALL THAT LOGIC, WE EXTRACT ONE CHARM VALUE !!!

	if obj.Type() == object.RESPONSE_OBJ {
		if obj.(*object.Effects).StopHappened && hub.currentServiceName != "" {
			delete(hub.services, hub.currentServiceName)
			hub.currentServiceName = ""
			return passedServiceName, obj.(*object.Effects)
		}
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
	return passedServiceName, object.OK_RESPONSE
}

func (hub *Hub) ParseHubCommand(line string) (string, []string) {
	hubReturn := ServiceDo(hub.services["hub"], line)
	if hubReturn.Type() == object.ERROR_OBJ {
		hub.WriteError(hubReturn.(*object.Error).Message)
		return "error", []string{hubReturn.(*object.Error).Message}
	}
	if hubReturn.Type() == object.STRUCT_OBJ && hubReturn.(*object.Struct).Name == "HubResponse" {
		verb := (hubReturn.(*object.Struct).Value["responseName"]).(*object.String).Value
		args := []string{}
		for _, v := range (hubReturn.(*object.Struct).Value["vals"]).(*object.List).Elements {
			args = append(args, v.(*object.String).Value)
		}
		return verb, args
	}
	panic("You're only supposed to get an error or a HubResponse from the HUB service.")
}

func (hub *Hub) DoHubCommand(username, password, verb string, args []string) bool {
	if (!hub.isAdministered()) &&
		(verb == "add" || verb == "create" || verb == "log-on" || verb == "log-off" ||
			verb == "let" || verb == "register" || verb == "groups" ||
			verb == "groups-of-user" || verb == "groups-of-service" || verb == "services of group" ||
			verb == "services-of-user" || verb == "users-of-service" || verb == "users-of-group" ||
			verb == "let-use" || verb == "let-own") {
		hub.WriteError("this is not an administered hub. To initialize it as one, first do 'hub config db' " +
			"(if you haven't already) and then 'hub config admin'.")
		return false
	}
	if hub.isAdministered() {
		isAdmin, err := database.IsUserAdmin(hub.Db, username)
		if err != nil {
			hub.WriteError(err.Error())
			return false
		}
		if !isAdmin && (verb == "config-db" || verb == "create" || verb == "let" ||
			verb == "hot-on" || verb == "hot-off" || verb == "listen" || verb == "peek-on" ||
			verb == "peek-off" || verb == "run" || verb == "reset" || verb == "rerun" ||
			verb == "replay" || verb == "replay-diff" || verb == "snap" || verb == "test" ||
			verb == "groups-of-user" || verb == "groups-of-service" || verb == "services of group" ||
			verb == "services-of-user" || verb == "users-of-service" || verb == "users-of-group" ||
			verb == "let-use" || verb == "let-own") {
			hub.WriteError("you don't have the admin status necessary to do that.")
			return false
		}
		if username == "" && !(verb == "log-on" || verb == "register" || verb == "quit") {
			hub.WriteError("\nThis is an administered hub and you aren't logged on. Please enter either " +
				"'hub register' to register as a user, or 'hub log on' to log on if you're already registered " +
				"with this hub.\n\n")
			return false
		}
	}
	switch verb {
	case "add":
		err := database.IsUserGroupOwner(hub.Db, username, args[1])
		if err != nil {
			hub.WriteError(err.Error())
			return false
		}
		err = database.AddUserToGroup(hub.Db, args[0], args[1], false)
		if err != nil {
			hub.WriteError(err.Error())
			return false
		}
		hub.WriteString(text.OK + "\n")
		return false
	case "config-admin":
		if !hub.isAdministered() {
			hub.configAdmin()
			return false
		} else {
			hub.WriteError("this hub is already administered.")
			return false
		}
	case "config-db":
		hub.configDb()
		return false
	case "create":
		err := database.AddGroup(hub.Db, args[0])
		if err != nil {
			hub.WriteError(err.Error())
			return false
		}
		err = database.AddUserToGroup(hub.Db, username, args[0], true)
		if err != nil {
			hub.WriteError(err.Error())
			return false
		}
		hub.WriteString(text.OK + "\n")
		return false
	case "edit":
		command := exec.Command("vim", args[0])
		command.Stdin = os.Stdin
		command.Stdout = os.Stdout
		err := command.Run()
		if err != nil {
			hub.WriteError(err.Error())
		}
		return false
	case "errors":
		if len(hub.ers) == 0 {
			hub.WritePretty("There are no recent errors.")
			return false
		}
		hub.WritePretty(object.GetList(hub.ers))
		return false
	case "groups-of-user":
		result, err := database.GetGroupsOfUser(hub.Db, args[0], false)
		if err != nil {
			hub.WriteError(err.Error())
		} else {
			hub.WriteString(result)
			return false
		}
	case "groups-of-service":
		result, err := database.GetGroupsOfService(hub.Db, args[0])
		if err != nil {
			hub.WriteError(err.Error())
		} else {
			hub.WriteString(result)
			return false
		}
	case "halt":
		var name string
		_, ok := hub.services[args[0]]
		if ok {
			name = args[0]
		} else {
			hub.WriteError("the hub can't find the service '" + args[0] + "'.")
			return false
		}
		if name == "" || name == "hub" {
			hub.WriteError("the hub doesn't know what you want to stop.")
			return false
		}
		delete(hub.services, name)
		hub.WriteString(text.OK + "\n")
		if name == hub.currentServiceName {
			hub.currentServiceName = ""
		}
		return false
	case "help":
		if helpMessage, ok := helpStrings[args[0]]; ok {
			hub.WritePretty(helpMessage + "\n")
			return false
		} else {
			hub.WriteError("the 'hub help' command doesn't accept " +
				"'" + args[0] + "' as a parameter.")
			return false
		}
	case "hot-on":
		hub.hot = true
		return false
	case "hot-off":
		hub.hot = false
		return false
	case "let":
		isAdmin, err := database.IsUserAdmin(hub.Db, username)
		if err != nil {
			hub.WriteError(err.Error())
			return false
		}
		if !isAdmin {
			hub.WriteError("you don't have the admin status necessary to do that.")
			return false
		}
		err = database.LetGroupUseService(hub.Db, args[0], args[1])
		if err != nil {
			hub.WriteError(err.Error())
			return false
		}
		hub.WriteString(text.OK + "\n")
		return false
	case "listen":
		hub.WriteString(text.OK)
		hub.WriteString("\nHub is listening.\n\n")
		hub.StartHttp(args[0], args[1])
		return false
	case "log-on":
		hub.getLogin()
		return false
	case "log-off":
		hub.Username = ""
		hub.Password = ""
		hub.currentServiceName = ""
		hub.WriteString("\n" + text.OK + "\n")
		hub.WritePretty("\nThis is an administered hub and you aren't logged on. Please enter either " +
			"'hub register' to register as a user, or 'hub log on' to log on if you're already registered " +
			"with this hub.\n\n")
		return false
	case "groups":
		result, err := database.GetGroupsOfUser(hub.Db, username, true)
		if err != nil {
			hub.WriteError(err.Error())
		} else {
			hub.WriteString(result)
			return false
		}
	case "services":
		if hub.isAdministered() {
			result, err := database.GetServicesOfUser(hub.Db, username, true)
			if err != nil {
				hub.WriteError(err.Error())
			} else {
				hub.WriteString(result)
				return false
			}
		} else {
			if len(hub.services) == 2 {
				hub.WriteString("The hub isn't running any services.\n")
				return false
			}
			hub.WriteString("\n")
			hub.list()
			return false
		}
	case "peek-on":
		hub.peek = true
		return false
	case "peek-off":
		hub.peek = false
		return false
	case "quit":
		hub.quit()
		return true
	case "register":
		hub.addUserAsGuest()
		return false
	case "replay":
		hub.oldServiceName = hub.currentServiceName
		hub.playTest(args[0], false)
		hub.currentServiceName = hub.oldServiceName
		_, ok := hub.services["#test"]
		if ok {
			delete(hub.services, "#test")
		}
		return false
	case "replay-diff":
		hub.oldServiceName = hub.currentServiceName
		hub.playTest(args[0], true)
		hub.currentServiceName = hub.oldServiceName
		_, ok := hub.services["#test"]
		if ok {
			delete(hub.services, "#test")
		}
		return false
	case "reset":
		service, ok := hub.services[hub.currentServiceName]
		if !ok {
			hub.WriteError("the hub can't find the service '" + hub.currentServiceName + "'.")
			return false
		}
		if hub.currentServiceName == "" {
			hub.WriteError("service is empty, nothing to reset.")
			return false
		}
		hub.WritePretty("Restarting script '" + service.ScriptFilepath +
			"' as service '" + hub.currentServiceName + "'.\n")
		hub.Start(username, hub.currentServiceName, service.ScriptFilepath)
		hub.lastRun = []string{hub.currentServiceName}
		return false
	case "rerun":
		if len(hub.lastRun) == 0 {
			hub.WriteError("nothing to rerun.")
			return false
		}
		hub.Start(username, hub.lastRun[0], hub.services[hub.lastRun[0]].ScriptFilepath)
		hub.tryMain()
		return false
	case "run":
		hub.lastRun = args
		if args[1] == "" {
			hub.WritePretty("Starting script '" + args[0] +
				"' as service '#" + strconv.Itoa(hub.anonymousServiceNumber) + "'.\n")
			hub.StartAnonymous(args[0])
			hub.tryMain()
			return false
		}
		hub.WritePretty("Starting script '" + args[0] + "' as service '" + args[1] + "'.\n")
		hub.Start(username, args[1], args[0])
		hub.tryMain()
		return false
	case "services-of-user":
		result, err := database.GetServicesOfUser(hub.Db, args[0], false)
		if err != nil {
			hub.WriteError(err.Error())
		} else {
			hub.WriteString(result)
			return false
		}
	case "services-of-group":
		result, err := database.GetServicesOfGroup(hub.Db, args[0])
		if err != nil {
			hub.WriteError(err.Error())
		} else {
			hub.WriteString(result)
			return false
		}
	case "snap":
		scriptFilepath := args[0]
		testFilepath := args[1]
		if testFilepath == "" {
			testFilepath = getUnusedTestFilename(scriptFilepath) // If no filename is given, we just generate one.
		}
		hub.snap = NewSnap(scriptFilepath, testFilepath)
		hub.oldServiceName = hub.currentServiceName
		if hub.Start(username, "#snap", scriptFilepath) {
			ServiceDo((*hub).services["#snap"], "$view = \"charm\"")
			hub.WriteString("Serialization is ON.\n")
		}
		return false
	case "snap-good":
		if hub.currentServiceName != "#snap" {
			hub.WriteError("you aren't taking a snap.")
			return false
		}
		result := hub.snap.Save(GOOD)
		hub.WriteString(result + "\n")
		hub.currentServiceName = hub.oldServiceName
		return false
	case "snap-bad":
		if hub.currentServiceName != "#snap" {
			hub.WriteError("you aren't taking a snap.")
			return false
		}
		result := hub.snap.Save(BAD)
		hub.WriteString(result + "\n")
		hub.currentServiceName = hub.oldServiceName
		return false
	case "snap-record":
		if hub.currentServiceName != "#snap" {
			hub.WriteError("you aren't taking a snap.")
			return false
		}
		result := hub.snap.Save(RECORD)
		hub.WriteString(result + "\n")
		hub.currentServiceName = hub.oldServiceName
		return false
	case "snap-discard":
		hub.WriteString(text.OK + "\n")
		hub.currentServiceName = hub.oldServiceName
		return false
	case "switch":
		_, ok := hub.services[args[0]]
		if ok {
			hub.WriteString(text.OK + "\n")
			if hub.administered {
				access, err := database.DoesUserHaveAccess(hub.Db, username, args[0])
				if err != nil {
					hub.WriteError(err.Error())
					return false
				}
				if !access {
					hub.WriteError("you don't have access to service '" + args[0] + "'.")
					return false
				}
				database.UpdateService(hub.Db, username, args[0])
				return false
			} else {
				hub.currentServiceName = args[0]
				if !hub.services[hub.currentServiceName].Visited {
					hub.tryMain()
				}
				return false
			}
		} else {
			hub.WriteError("service '" + args[0] + "' doesn't exist")
		}
	case "test":
		hub.TestScript(args[0])
		return false
	case "trace":
		if len(hub.ers) == 0 {
			hub.WriteError("there are no recent errors.")
			return false
		}
		if len(hub.ers[0].Trace) == 0 {
			hub.WriteError("not a runtime error.")
			return false
		}
		hub.WritePretty(text.RT_ERROR + hub.ers[0].Message + "\n\n") // If it is a runtime error, then there is only one of them.
		for i := len(hub.ers[0].Trace) - 1; i >= 0; i-- {
			hub.WritePretty("  From: " + text.DescribeTok(hub.ers[0].Trace[i]) + text.DescribePos(hub.ers[0].Trace[i]) + ".")
		}
		hub.WriteString("\n")
		return false
	case "users-of-group":
		result, err := database.GetUsersOfGroup(hub.Db, args[0])
		if err != nil {
			hub.WriteError(err.Error())
		} else {
			hub.WriteString(result)
			return false
		}
	case "users-of-service":
		result, err := database.GetUsersOfService(hub.Db, args[0])
		if err != nil {
			hub.WriteError(err.Error())
		} else {
			hub.WriteString(result)
			return false
		}
	case "where":
		num, err := strconv.Atoi(args[0])
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
		num, err := strconv.Atoi(args[0])
		if err != nil {
			hub.WriteError("the 'why' keyword takes the number of an error as a parameter.")
			return false
		}
		hub.WritePretty("\n$Error$" + hub.ers[num].Message +
			".\n\n" + object.ErrorCreatorMap[hub.ers[num].ErrorId].Explanation(hub.ers, num, hub.ers[num].Token, hub.ers[num].Args...) + "\n")
		refLine := "Error has reference '" + hub.ers[num].ErrorId + "'."
		refLine = "\n" + strings.Repeat(" ", MARGIN-len(refLine)-2) + refLine
		hub.WritePretty(refLine)
		hub.WriteString("\n")
		return false
	case "values":
		if len(hub.ers) == 0 {
			hub.WriteError("there are no recent errors.")
			return false
		}
		if hub.ers[0].Values == nil {
			hub.WriteError("no values are available.")
			return false
		}
		if len(hub.ers[0].Values) == 0 {
			hub.WriteError("no values were passed.")
			return false
		}
		if len(hub.ers[0].Values) == 1 {
			hub.WriteString("\nThe value passed was:\n")
		} else {
			hub.WriteString("\nValues passed were:\n")
		}
		for _, v := range hub.ers[0].Values {
			hub.WritePretty(text.BULLET + v.Inspect(object.ViewCharmLiteral))
		}
		hub.WriteString("\n")
		return false
	default:
		panic("Didn't return from verb " + verb)
	}
	panic("Don't know what to do with verb " + verb)
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
			repeatNo := codeWidth - len(plainLine)
			if repeatNo < 0 {
				repeatNo = 0
			}
			hub.WriteString(" │  " + text.Cyan(plainLine) + strings.Repeat(" ", repeatNo) + "│\n")
		} else {
			hub.WriteString(text.HighlightLine(plainLine, highlighter) + "\n")
		}
		i = i + j + 1
	}
}

func (hub *Hub) isAdministered() bool {
	_, err := os.Stat("rsc/admin.dat")
	return !errors.Is(err, os.ErrNotExist)
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
		panic("Can't find manual in file 'rsc/manual.txt'.")
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
	helpTopics = append(helpTopics, "topics")
	sort.Strings(helpTopics)
	helpStringForHelp := "\nYou can get help on a subject by typing 'hub help \"<topic name>\"' into the REPL.\n\n" +
		"Help topics are: \n\n"
	for _, v := range helpTopics {
		helpStringForHelp = helpStringForHelp + text.BULLET + "\"" + v + "\"\n"
	}
	helpStrings["topics"] = helpStringForHelp
}

func (hub *Hub) StartAnonymous(scriptFilepath string) {
	hub.Start("", "#"+fmt.Sprint(hub.anonymousServiceNumber), scriptFilepath)
	hub.anonymousServiceNumber = hub.anonymousServiceNumber + 1
}

func (hub *Hub) Start(username, serviceName, scriptFilepath string) bool {
	code := ""
	if scriptFilepath != "" {
		dat, err := os.ReadFile(scriptFilepath)
		if err != nil {
			hub.WriteError("os returns \"" + err.Error() + "\".")
			return false
		} else {
			code = strings.TrimRight(string(dat), "\n") + "\n"
		}
	}
	if hub.administered {
		err := database.UpdateService(hub.Db, username, serviceName)
		if err != nil {
			hub.WriteError(err.Error())
			return false
		}
	}
	hub.currentServiceName = serviceName
	hub.createService(serviceName, scriptFilepath, code)
	return true
}

func (hub *Hub) tryMain() { // Guardedly tries to run the `main` command.

	if !hub.services[hub.currentServiceName].Broken && hub.services[hub.currentServiceName].Parser.Unfixes.Contains("main") {
		obj := ServiceDo(hub.services[hub.currentServiceName], "main")
		hub.lastRun = []string{hub.currentServiceName}
		hub.services[hub.currentServiceName].Visited = true
		if obj.Type() == object.RESPONSE_OBJ && obj.(*object.Effects).StopHappened && hub.currentServiceName != "" {
			delete(hub.services, hub.currentServiceName)
			hub.currentServiceName = ""
		} else {
			if obj.Type() == object.ERROR_OBJ {
				hub.WritePretty("\n[0] " + objToString(hub.services[hub.currentServiceName], obj))
				hub.WriteString("\n")
				hub.ers = []*object.Error{obj.(*object.Error)}
			} else {
				hub.WriteString(objToString(hub.services[hub.currentServiceName], obj))
			}
		}
	}
}

func (hub *Hub) createService(name, scriptFilepath, code string) bool {

	var modifiedTime int64

	// TODO --- if you changed an imported dependency this wouldn't notice.
	if name != "" {
		file, err := os.Stat(scriptFilepath)
		if err != nil {
			panic("Something weird has happened!")
		}
		modifiedTime = file.ModTime().UnixMilli()
		_, present := hub.services[name]
		if present && modifiedTime == hub.services[name].Timestamp {
			return false
		}
	}
	newService := parser.NewService()
	hub.services[name] = newService
	hub.services[name].Timestamp = modifiedTime
	hub.currentServiceName = name
	hub.services[name].ScriptFilepath = scriptFilepath

	init := initializer.New(scriptFilepath, code, hub.Db)
	init.GetSource(scriptFilepath)
	init.Parser = *parser.New()
	init.Parser.Database = hub.Db
	init.Parser.Services = hub.services
	init.Parser.Namespaces[scriptFilepath] = ""
	init.MakeParserAndTokenizedProgram()
	if init.ErrorsExist() {
		hub.GetAndReportErrors(&init.Parser)
		hub.Sources = init.Sources
		newService.Broken = true
		return false
	}

	// Import the builtins and world library.

	thingsToImport := []string{"rsc/builtins.ch", "rsc/world.ch"}

	for _, fname := range thingsToImport {
		libDat, _ := os.ReadFile(fname)
		stdImp := strings.TrimRight(string(libDat), "\n") + "\n"
		init.SetRelexer(*relexer.New(fname, stdImp))
		init.MakeParserAndTokenizedProgram()
		init.GetSource(fname)
	}

	for init.ImportsExist() {

		init.ParseImports()
		if init.ErrorsExist() {
			hub.GetAndReportErrors(&init.Parser)
			hub.Sources = init.Sources
			newService.Broken = true
			return false
		}

		init.ImportEverything()
		if init.ErrorsExist() {
			hub.GetAndReportErrors(&init.Parser)
			hub.Sources = init.Sources
			newService.Broken = true
			return false
		}
	}

	env := object.NewEnvironment()

	init.ParseEnumDefs(env)
	if init.ErrorsExist() {
		hub.GetAndReportErrors(&init.Parser)
		hub.Sources = init.Sources
		newService.Broken = true
		return false
	}

	init.MakeLanguagesAndContacts()
	if init.ErrorsExist() {
		hub.GetAndReportErrors(&init.Parser)
		hub.Sources = init.Sources
		newService.Broken = true
		return false
	}

	init.ParseTypeDefs()
	if init.ErrorsExist() {
		hub.GetAndReportErrors(&init.Parser)
		hub.Sources = init.Sources
		newService.Broken = true
		return false
	}

	init.EvaluateTypeDefs(env)
	if init.ErrorsExist() {
		hub.GetAndReportErrors(&init.Parser)
		hub.Sources = init.Sources
		newService.Broken = true
		return false
	}

	init.ParseEverything()
	if init.ErrorsExist() {
		hub.GetAndReportErrors(&init.Parser)
		hub.Sources = init.Sources
		newService.Broken = true
		return false
	}

	init.InitializeEverything(env, scriptFilepath)
	if init.ErrorsExist() {
		hub.GetAndReportErrors(&init.Parser)
		hub.Sources = init.Sources
		newService.Broken = true
		return false
	}

	init.Parser.EffHandle = parser.MakeStandardEffectHandler(hub.out, *env)
	(*newService).Parser = &init.Parser
	(*newService).Env = env
	hub.Sources = init.Sources
	return true
}

func (hub *Hub) GetAndReportErrors(p *parser.Parser) {
	hub.ers = p.Errors
	hub.WritePretty(object.GetList(hub.ers))
}

func (hub *Hub) GetCurrentServiceName() string {
	return hub.currentServiceName
}

func (hub *Hub) CurrentServiceIsBroken() bool {
	return hub.services[hub.currentServiceName].Broken
}

func (hub *Hub) save() string {
	f, err := os.Create("rsc/hub.dat")
	if err != nil {
		return text.HUB_ERROR + "os reports \"" + strings.TrimSpace(err.Error()) + "\".\n"
	}
	defer f.Close()
	for k := range hub.services {
		if !isAnonymous(k) && k != "#snap" && k != "#test" {
			_, err := f.WriteString(k + ", " + hub.services[k].ScriptFilepath + "\n")
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
	_, ok := hub.services[hub.currentServiceName]
	if !ok {
		hub.currentServiceName = ""
	}

	_, err = os.Stat("rsc/admin.dat")
	hub.administered = (err == nil)
	if hub.administered {
		hub.currentServiceName = ""
	}

	// If the database configuration doesn't exist this is because the user
	// hasn't created it yet, so we just skip the setup.
	if _, err := os.Stat("rsc/database.dat"); !errors.Is(err, os.ErrNotExist) {

		fileBytes, err := os.ReadFile("rsc/database.dat")

		if err != nil {
			hub.WriteError(err.Error())
			return
		}

		params := strings.Split(string(fileBytes), "\n")

		hub.Db, err = database.GetdB(params[0], params[1], params[2],
			params[3], params[4], params[5])

		for _, v := range hub.services {
			if !v.Broken {
				v.Parser.Database = hub.Db
			}
		}

		if err != nil {
			hub.WriteError(err.Error())
			return
		}
		if hub.administered && !hub.listeningToHttp {
			hub.WritePretty("This is an administered hub and you aren't logged on. Please enter either " +
				"'hub register' to register as a user, or 'hub log on' to log on if you're already registered " +
				"with this hub.\n\n")
			return
		}
		hub.tryMain()
	}
}

func (hub *Hub) list() {
	if len(hub.services) == 2 {
		return
	}
	hub.WriteString("The hub is running the following services:\n\n")
	for k := range hub.services {
		if k == "" || k == "hub" {
			continue
		}
		if hub.services[k].Broken {
			hub.WriteString(text.BROKEN)
			hub.WritePretty("Service '" + k + "' running script '" + filepath.Base(hub.services[k].ScriptFilepath) + "'.")
		} else {
			hub.WriteString(text.GOOD_BULLET)
			hub.WritePretty("Service '" + k + "' running script '" + filepath.Base(hub.services[k].ScriptFilepath) + "'.")
		}
	}
	hub.WriteString("\n")
}

func (hub *Hub) TestScript(scriptFilepath string) {

	fname := filepath.Base(scriptFilepath)
	fname = fname[:len(fname)-len(filepath.Ext(fname))]
	dname := filepath.Dir(scriptFilepath)
	directoryName := dname + "/charm-tests/" + fname

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
		//scanner.Scan()
		scanner.Scan()
		hub.Start("", "#test", scriptFilepath)
		hub.WritePretty("Running test '" + testFilepath + "'.\n")
		ServiceDo((*hub).services["#test"], "$view = \"charm\"")
		service := (*hub).services["#test"]
		_ = scanner.Scan() // eats the newline
		executionMatchesTest := true
		for scanner.Scan() {
			lineIn := scanner.Text()[3:]
			scanner.Scan()
			lineOut := scanner.Text()
			result := ServiceDo(service, lineIn)
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
	hub.Start("", "#test", scriptFilepath)
	ServiceDo((*hub).services["#test"], "$view = \"charm\"")
	service := (*hub).services["#test"]
	_ = scanner.Scan() // eats the newline
	for scanner.Scan() {
		lineIn := scanner.Text()[3:]
		scanner.Scan()
		lineOut := scanner.Text()
		result := ServiceDo(service, lineIn)
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

func objToString(service *parser.Service, obj object.Object) string {

	value, _ := service.Parser.AllGlobals.Get("$view")
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
		h.WriteError("could not read body: %s\n")
	}
	input := string(body[:])
	h.out = w
	h.Do(input, "", "", h.currentServiceName)
	io.WriteString(w, "\n")
}

// By contrast, once the hub is administered it expects an HTTP request to consist of JSON
// containing the line to be executed and the username and password of the user.
type jsonRequest = struct {
	Body     string
	Username string
	Password string
}

type jsonResponse = struct {
	Body    string
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
		serviceName, err = database.ValidateUser(h.Db, request.Username, request.Password)
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
// I can get rid of the whole thing when I do SQL integration and can just make the hub into
// a regular Charm service.
type Form struct { // For when the hub wants to initiate structured input.
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
	_, err := os.Stat("rsc/admin.dat")
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

	err = database.AddUser(h.Db, f.Result["Username"], f.Result["First name"],
		f.Result["Last name"], f.Result["Email"], f.Result["*Password"], "")
	if err != nil {
		h.WriteError(err.Error())
		return
	}
	err = database.AddUserToGroup(h.Db, f.Result["Username"], "Guests", false)
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
	if h.Db == nil {
		h.WriteError("database has not been configured: do 'hub config db' first.")
		return
	}
	if f.Result["*Password"] != f.Result["*Confirm password"] {
		h.WriteError("passwords don't match.")
		return
	}
	err := database.AddAdmin(h.Db, f.Result["Username"], f.Result["First name"],
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
	_, err := database.ValidateUser(h.Db, f.Result["Username"], f.Result["*Password"])
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

	h.Db, err = database.GetdB(database.GetSortedDrivers()[number], f.Result["Host"], f.Result["Port"],
		f.Result["Database"], f.Result["Username for database access"], f.Result["*Password for database access"])

	if err != nil {
		h.WriteError(err.Error())
		return
	}

	for _, v := range h.services {
		v.Parser.Database = h.Db
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

func ServiceDo(service *parser.Service, line string) object.Object {
	return evaluator.Evaluate(*service.Parser.ParseLine("REPL input", line),
		evaluator.NewContext(service.Parser, service.Env, evaluator.REPL, true))
}
