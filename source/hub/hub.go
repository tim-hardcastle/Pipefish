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
	"regexp"
	"sort"
	"strconv"
	"strings"

	"pipefish/source/database"
	"pipefish/source/lexer"
	"pipefish/source/parser"
	"pipefish/source/report"
	"pipefish/source/service"
	"pipefish/source/text"
	"pipefish/source/values"

	"src.elv.sh/pkg/persistent/vector"
)

var (
	MARGIN = 84
)

type Hub struct {
	hubFilepath            string
	services               map[string]*service.Service
	ers                    report.Errors
	peek                   bool
	in                     io.Reader
	out                    io.Writer
	anonymousServiceNumber int
	snap                   *service.Snap
	oldServiceName         string // Somewhere to keep the old service name while taking a snap. TODO --- you can now take snaps on their own dedicated hub, saving a good deal of faffing around.
	Sources                map[string][]string
	lastRun                []string
	CurrentForm            *Form
	Db                     *sql.DB
	administered           bool
	listeningToHttp        bool
	port, path             string
	Username               string
	Password               string

	directory string
}

func New(in io.Reader, out io.Writer) *Hub {
	appDir, _ := filepath.Abs(filepath.Dir(os.Args[0]))
	hub := Hub{
		services:  make(map[string]*service.Service),
		in:        in,
		out:       out,
		lastRun:   []string{},
		directory: appDir + "/"}
	return &hub
}

func (hub *Hub) currentServiceName() string {
	cs := hub.getSV("currentService")
	if cs.T == values.NULL {
		return ""
	} else {
		return cs.V.(string)
	}
}

func (hub *Hub) hasDatabase() bool {
	return hub.getSV("database").T == values.NULL
}

func (hub *Hub) getDB() (string, string, string, int, string, string) {
	dbStruct := hub.getSV("database").V.([]values.Value)
	driver := hub.services["hub"].Mc.Literal(dbStruct[0])
	return driver, dbStruct[1].V.(string), dbStruct[2].V.(string), dbStruct[3].V.(int), dbStruct[4].V.(string), dbStruct[5].V.(string)
}

func (hub *Hub) setDB(driver, name, path string, port int, username, password string) {
	hubService := hub.services["hub"]
	driverAsEnumValue := hubService.Mc.Mem[hubService.Cp.EnumElements[driver]]
	structType := hubService.Cp.StructNameToTypeNumber["Database"]
	hub.setSV("database", structType, []values.Value{driverAsEnumValue, {values.STRING, name}, {values.STRING, path}, {values.INT, port}, {values.STRING, username}, {values.STRING, password}})
}

func (hub *Hub) isLive() bool {
	return hub.getSV("isLive").V.(bool)
}

func (hub *Hub) setLive(b bool) {
	hub.setSV("isLive", values.BOOL, b)
}

func (hub *Hub) setServiceName(name string) {
	hub.setSV("currentService", values.STRING, name)
}

func (hub *Hub) makeEmptyServiceCurrent() {
	hub.setSV("currentService", values.NULL, nil)
}

func (hub *Hub) getSV(sv string) values.Value {
	return hub.services["hub"].GetVariable(sv)
}

func (hub *Hub) setSV(sv string, ty values.ValueType, v any) {
	hub.services["hub"].SetVariable(sv, ty, v)
}

// This takes the input from the REPL, interprets it as a hub command if it begins with 'hub';
// as an instruction to the os if it begins with 'os', and as an expression to be passed to
// the current service if none of the above hold.
func (hub *Hub) Do(line, username, password, passedServiceName string) (string, bool) {

	if match, _ := regexp.MatchString(`^\s*(|\/\/.*)$`, line); match {
		hub.WriteString("")
		return passedServiceName, false
	}

	if hub.administered && !hub.listeningToHttp && hub.Password == "" &&
		!(line == "hub register" || line == "hub log on" || line == "hub quit") {
		hub.WriteError("this is an administered hub and you aren't logged on. Please enter either " +
			"'hub register' to register as a user, or 'hub log on' to log on if you're already registered " +
			"with this hub.")
		return passedServiceName, false
	}

	// We may be talking to the hub itself.

	hubWords := strings.Fields(line)
	if hubWords[0] == "hub" {
		if len(line) == 3 {
			hub.WriteError("you need to say what you want the hub to do.")
			return passedServiceName, false
		}
		verb, args := hub.ParseHubCommand(line[4:])
		if verb == "error" || verb == "OK" {
			return passedServiceName, false
		}
		hubResult := hub.DoHubCommand(username, password, verb, args)
		if len(hubWords) > 1 && hubWords[1] == "run" && hub.administered { // TODO: find out what it does and where it should be now that we have ++ for hub commands.
			serviceName, _ := database.ValidateUser(hub.Db, username, password)
			return serviceName, false
		}
		if hubResult {
			return passedServiceName, true
		} else {
			return passedServiceName, false
		}
	}

	// We may be talking to the os

	if hubWords[0] == "os" {
		if hub.isAdministered() {
			hub.WriteError("for reasons of safety and sanity, the 'os' prefix doesn't work in administered hubs.")
			return passedServiceName, false
		}
		if len(hubWords) == 3 && hubWords[1] == "cd" { // Because cd changes the directory for the current
			os.Chdir(hubWords[2])    // process, if we did it with exec it would do it for
			hub.WriteString(text.OK) // that process and not for Pipefish.
			return passedServiceName, false
		}
		command := exec.Command(hubWords[1], hubWords[2:]...)
		out, err := command.Output()
		if err != nil {
			hub.WriteError("/a" + err.Error())
			return passedServiceName, false
		}
		if len(out) == 0 {
			hub.WriteString(text.OK)
			return passedServiceName, false
		}
		hub.WriteString(string(out))
		return passedServiceName, false
	}

	// Otherwise, we're talking to the current service.

	serviceToUse, ok := hub.services[passedServiceName]
	if !ok {
		hub.WriteError("the hub can't find the service '" + passedServiceName + "'.")
		return passedServiceName, false
	}

	// The service may be broken, in which case we'll let the empty service handle the input.

	if serviceToUse.Broken {
		serviceToUse = hub.services[""]
	}

	// If hub peek is turned on, this will show us the wheels going round.
	if hub.peek {
		lexer.LexDump(line)
		lexer.RelexDump(line)
		serviceToUse.Cp.GetParser().ParseDump(hub.currentServiceName(), line)
	}

	// Errors in the parser are a signal for the parser/initializer to halt, so we need to clear them here.
	// They may be sitting around so the end-user can do 'hub why', but we can get rid of them now.
	serviceToUse.Cp.GetParser().ResetAfterError()

	hub.Sources["REPL input"] = []string{line}

	if serviceToUse.Cp.GetParser().ErrorsExist() {
		hub.GetAndReportErrors(serviceToUse.Cp.GetParser())
		return passedServiceName, false
	}

	needsUpdate := hub.serviceNeedsUpdate(hub.currentServiceName())
	if hub.isLive() && needsUpdate {
		hub.StartAndMakeCurrent(hub.Username, hub.currentServiceName(), hub.services[hub.currentServiceName()].Cp.ScriptFilepath)
		serviceToUse = hub.services[hub.currentServiceName()]
		if serviceToUse.Broken {
			return passedServiceName, false
		}
	}

	if hub.currentServiceName() == "#snap" {
		hub.snap.AddInput(line)
	}

	// *** THIS IS THE BIT WHERE WE DO THE THING!
	val := ServiceDo(serviceToUse, line)
	// *** FROM ALL THAT LOGIC, WE EXTRACT ONE PIPEFISH VALUE !!!

	if serviceToUse.Cp.GetParser().ErrorsExist() { // Any lex-parse-compile errors should end up in the parser of the compiler of the service, returned in p.
		hub.GetAndReportErrors(serviceToUse.Cp.GetParser())
		return passedServiceName, false
	}

	if val.T == values.ERROR {
		hub.WriteString("\n[0] " + valToString(serviceToUse, val))
		hub.WriteString("\n")
		hub.ers = []*report.Error{val.V.(*report.Error)}
		if len(val.V.(*report.Error).Values) > 0 {
			hub.WritePretty("Values are available with 'hub values'.\n\n")
		}
	} else {
		var out string
		if hub.services["hub"].GetVariable("display").V.(int) == 0 || hub.currentServiceName() == "#snap" {
			out = serviceToUse.Mc.StringifyValue(val, service.LITERAL)
		} else {
			out = serviceToUse.Mc.StringifyValue(val, service.DEFAULT)
		}
		hub.WriteString(out)
		if hub.currentServiceName() == "#snap" {
			hub.snap.AddOutput(out)
		}
	}
	return passedServiceName, false
}

func (hub *Hub) ParseHubCommand(line string) (string, []string) {
	hubService := hub.services["hub"]
	hubReturn := ServiceDo(hubService, line)
	if hubService.Cp.GetParser().ErrorsExist() { // Any lex-parse-compile errors should end up in the parser of the compiler of the service, returned in p.
		hub.GetAndReportErrors(hubService.Cp.GetParser())
		return "error", []string{}
	}
	if hubReturn.T == values.ERROR {
		hub.WriteError(hubReturn.V.(*report.Error).Message)
		return "error", []string{hubReturn.V.(*report.Error).Message}
	}

	if hubReturn.T == hubService.Cp.StructNameToTypeNumber["HubResponse"] {
		hR := hubReturn.V.([]values.Value)
		verb := hR[0].V.(string)
		args := []string{}
		for i := 0; i < hR[1].V.(vector.Vector).Len(); i++ {
			el, _ := hR[1].V.(vector.Vector).Index(i)
			args = append(args, el.(values.Value).V.(string))
		}
		return verb, args
	}
	if hubReturn.T == values.SUCCESSFUL_VALUE {
		if hub.getSV("display").V.(int) == 0 {
			hub.WriteString("OK" + "\n")
		} else {
			hub.WriteString(text.OK + "\n")
		}
		return "OK", nil
	}

	hub.WriteError("couldn't parse hub instruction.")
	return "error", []string{"couldn't parse hub instruction"}
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
			hub.WriteError("b/" + err.Error())
			return false
		}
		if !isAdmin && (verb == "config-db" || verb == "create" || verb == "let" ||
			verb == "live-on" || verb == "live-off" || verb == "listen" || verb == "peek-on" ||
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
			hub.WriteError("c/ " + err.Error())
			return false
		}
		err = database.AddUserToGroup(hub.Db, args[0], args[1], false)
		if err != nil {
			hub.WriteError("d/ " + err.Error())
			return false
		}
		hub.WriteString(text.OK + "\n")
		return false
	case "api":
		srv, ok := hub.services[args[0]]
		if !ok {
			hub.WriteError("Hub can't find service " + text.Emph(args[0]) + ".")
			return false
		}
		serializationOfApi := srv.SerializeApi()
		stub := service.SerializedAPIToDeclarations(serializationOfApi, service.DUMMY)
		hub.WriteString(stub + "\n")
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
			hub.WriteError("e/ " + err.Error())
			return false
		}
		err = database.AddUserToGroup(hub.Db, username, args[0], true)
		if err != nil {
			hub.WriteError("f/" + err.Error())
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
			hub.WriteError("g/" + err.Error())
		}
		return false
	case "errors":
		if len(hub.ers) == 0 {
			hub.WritePretty("There are no recent errors.")
			return false
		}
		hub.WritePretty(report.GetList(hub.ers))
		return false
	case "groups-of-user":
		result, err := database.GetGroupsOfUser(hub.Db, args[0], false)
		if err != nil {
			hub.WriteError("h/ " + err.Error())
		} else {
			hub.WriteString(result)
			return false
		}
	case "groups-of-service":
		result, err := database.GetGroupsOfService(hub.Db, args[0])
		if err != nil {
			hub.WriteError("i/ " + err.Error())
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
		if name == hub.currentServiceName() {
			hub.makeEmptyServiceCurrent()
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
	case "let":
		isAdmin, err := database.IsUserAdmin(hub.Db, username)
		if err != nil {
			hub.WriteError("i/ " + err.Error())
			return false
		}
		if !isAdmin {
			hub.WriteError("you don't have the admin status necessary to do that.")
			return false
		}
		err = database.LetGroupUseService(hub.Db, args[0], args[1])
		if err != nil {
			hub.WriteError("j/ " + err.Error())
			return false
		}
		hub.WriteString(text.OK + "\n")
		return false
	case "listen":
		hub.WriteString(text.OK)
		hub.WriteString("\nHub is listening.\n\n")
		hub.StartHttp("/"+args[0], args[1])
		return false
	case "live-on":
		hub.setLive(true)
		return false
	case "live-off":
		hub.setLive(false)
		return false
	case "log-on":
		hub.getLogin()
		return false
	case "log-off":
		hub.Username = ""
		hub.Password = ""
		hub.makeEmptyServiceCurrent()
		hub.WriteString("\n" + text.OK + "\n")
		hub.WritePretty("\nThis is an administered hub and you aren't logged on. Please enter either " +
			"'hub register' to register as a user, or 'hub log on' to log on if you're already registered " +
			"with this hub.\n\n")
		return false
	case "groups":
		result, err := database.GetGroupsOfUser(hub.Db, username, true)
		if err != nil {
			hub.WriteError("l/ " + err.Error())
		} else {
			hub.WriteString(result)
			return false
		}
	case "serialize":
		srv, ok := hub.services[args[0]]
		if !ok {
			hub.WriteError("Hub can't find service " + text.Emph(args[0]) + ".")
			return false
		}
		serializationOfApi := srv.SerializeApi()
		hub.WriteString(serializationOfApi)
		return false
	case "services":
		if hub.isAdministered() {
			result, err := database.GetServicesOfUser(hub.Db, username, true)
			if err != nil {
				hub.WriteError("m/ " + err.Error())
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
		hub.oldServiceName = hub.currentServiceName()
		hub.playTest(args[0], false)
		hub.setServiceName(hub.oldServiceName)
		_, ok := hub.services["#test"]
		if ok {
			delete(hub.services, "#test")
		}
		return false
	case "replay-diff":
		hub.oldServiceName = hub.currentServiceName()
		hub.playTest(args[0], true)
		hub.setServiceName(hub.oldServiceName)
		_, ok := hub.services["#test"]
		if ok {
			delete(hub.services, "#test")
		}
		return false
	case "reset":
		serviceToReset, ok := hub.services[hub.currentServiceName()]
		if !ok {
			hub.WriteError("the hub can't find the service '" + hub.currentServiceName() + "'.")
			return false
		}
		if hub.currentServiceName() == "" {
			hub.WriteError("service is empty, nothing to reset.")
			return false
		}
		hub.WritePretty("Restarting script '" + serviceToReset.Cp.ScriptFilepath +
			"' as service '" + hub.currentServiceName() + "'.\n")
		hub.StartAndMakeCurrent(username, hub.currentServiceName(), serviceToReset.Cp.ScriptFilepath)
		hub.lastRun = []string{hub.currentServiceName()}
		return false
	case "rerun":
		if len(hub.lastRun) == 0 {
			hub.WriteError("nothing to rerun.")
			return false
		}
		hub.WritePretty("Rerunning script '" + hub.services[hub.lastRun[0]].Cp.ScriptFilepath +
			"' as service '" + hub.lastRun[0] + "'.\n")
		hub.StartAndMakeCurrent(username, hub.lastRun[0], hub.services[hub.lastRun[0]].Cp.ScriptFilepath)
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
		hub.StartAndMakeCurrent(username, args[1], args[0])
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
			hub.WriteError("n/ " + err.Error())
		} else {
			hub.WriteString(result)
			return false
		}
	case "snap":
		scriptFilepath := args[0]
		println("Script filepath is", scriptFilepath)
		testFilepath := args[1]
		if testFilepath == "" {
			testFilepath = getUnusedTestFilename(scriptFilepath) // If no filename is given, we just generate one.
		}
		hub.snap = service.NewSnap(scriptFilepath, testFilepath)
		hub.oldServiceName = hub.currentServiceName()
		if hub.StartAndMakeCurrent(username, "#snap", scriptFilepath) {
			ServiceDo((*hub).services["#snap"], "$view = \"\"")
			hub.WriteString("Serialization is ON.\n")
			hub.services[hub.currentServiceName()].Mc.IoHandle =
				service.MakeSnapIoHandler(hub.out, hub.snap)
		}

		return false
	case "snap-good":
		if hub.currentServiceName() != "#snap" {
			hub.WriteError("you aren't taking a snap.")
			return false
		}
		result := hub.snap.Save(service.GOOD)
		hub.WriteString(result + "\n")
		hub.setServiceName(hub.oldServiceName)
		return false
	case "snap-bad":
		if hub.currentServiceName() != "#snap" {
			hub.WriteError("you aren't taking a snap.")
			return false
		}
		result := hub.snap.Save(service.BAD)
		hub.WriteString(result + "\n")
		hub.setServiceName(hub.oldServiceName)
		return false
	case "snap-record":
		if hub.currentServiceName() != "#snap" {
			hub.WriteError("you aren't taking a snap.")
			return false
		}
		result := hub.snap.Save(service.RECORD)
		hub.WriteString(result + "\n")
		hub.setServiceName(hub.oldServiceName)
		return false
	case "snap-discard":
		if hub.currentServiceName() != "#snap" {
			hub.WriteError("you aren't taking a snap.")
			return false
		}
		hub.WriteString(text.OK + "\n")
		hub.setServiceName(hub.oldServiceName)
		return false
	case "switch":
		_, ok := hub.services[args[0]]
		if ok {
			hub.WriteString(text.OK + "\n")
			if hub.administered {
				access, err := database.DoesUserHaveAccess(hub.Db, username, args[0])
				if err != nil {
					hub.WriteError("o/ " + err.Error())
					return false
				}
				if !access {
					hub.WriteError("you don't have access to service '" + args[0] + "'.")
					return false
				}
				database.UpdateService(hub.Db, username, args[0])
				return false
			} else {
				hub.setServiceName(args[0])
				if !hub.services[hub.currentServiceName()].Visited {
					hub.tryMain()
				}
				return false
			}
		} else {
			hub.WriteError("service '" + args[0] + "' doesn't exist")
		}
	case "test":
		file, err := os.Open(args[0])
		if err != nil {
			hub.WriteError("p/ " + strings.TrimSpace(err.Error()) + "\n")
			return false
		}

		defer file.Close()
		fileInfo, err := file.Stat()
		if err != nil {
			hub.WriteError(strings.TrimSpace("q/ "+err.Error()) + "\n")
			return false
		}

		if fileInfo.IsDir() {
			files, err := file.Readdir(0)
			if err != nil {
				hub.WriteError("r/ " + strings.TrimSpace(err.Error()) + "\n")
				return false
			}

			for _, potentialCharmFile := range files {
				if filepath.Ext(potentialCharmFile.Name()) == ".pf" {
					hub.TestScript(args[0]+"/"+potentialCharmFile.Name(), service.ERROR_CHECK)
				}
			}
		} else {
			hub.TestScript(args[0], service.ERROR_CHECK)
		}

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
			hub.WriteError("s/ " + err.Error())
		} else {
			hub.WriteString(result)
			return false
		}
	case "users-of-service":
		result, err := database.GetUsersOfService(hub.Db, args[0])
		if err != nil {
			hub.WriteError("t/ " + err.Error())
		} else {
			hub.WriteString(result)
			return false
		}
	case "values":
		if len(hub.ers) == 0 {
			hub.WriteError("there are no recent errors.")
			return false
		}
		if hub.ers[0].Values == nil {
			hub.WriteError("no values were passed.")
			return false
		}
		if len(hub.ers[0].Values) == 0 {
			hub.WriteError("no values were passed.")
			return false
		}
		if len(hub.ers[0].Values) == 1 {
			hub.WriteString("\nThe value passed was:\n\n")
		} else {
			hub.WriteString("\nValues passed were:\n\n")
		}
		for _, v := range hub.ers[0].Values {
			if v.T == values.BLING {
				hub.WritePretty(text.BULLET_SPACING + hub.services[hub.currentServiceName()].Mc.Literal(v))
			} else {
				hub.WritePretty(text.BULLET + hub.services[hub.currentServiceName()].Mc.Literal(v))
			}
		}
		hub.WriteString("\n")
		return false
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
		hub.WritePretty("\nFound" + text.DescribePos(hub.ers[num].Token) + ":\n")
		println()
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
			".\n\n" + report.ErrorCreatorMap[hub.ers[num].ErrorId].Explanation(hub.ers, num, hub.ers[num].Token, hub.ers[num].Args...) + "\n")
		refLine := "Error has reference '" + hub.ers[num].ErrorId + "'."
		refLine = "\n" + strings.Repeat(" ", MARGIN-len(refLine)-2) + refLine
		hub.WritePretty(refLine)
		hub.WriteString("\n")
		return false
	default:
		panic("Didn't return from verb " + verb)
	}
	panic("Don't know what to do with verb " + verb)
}

func getUnusedTestFilename(scriptFilepath string) string {

	fname := filepath.Base(scriptFilepath)
	fname = fname[:len(fname)-len(filepath.Ext(fname))]
	dname := filepath.Dir(scriptFilepath)
	directoryName := dname + "/-tests/" + fname
	name := text.FlattenedFilename(scriptFilepath) + "_"

	tryNumber := 1
	tryName := ""

	for ; ; tryNumber++ {
		tryName = name + strconv.Itoa(tryNumber) + ".tst"
		_, error := os.Stat(directoryName + "/" + tryName)
		if os.IsNotExist(error) {
			break
		}
	}
	return tryName
}

func (hub *Hub) quit() {
	hub.saveHubFile()
	hub.WriteString(text.OK + "\n" + text.Logo() + "Thank you for using Pipefish. Have a nice day!\n\n")
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
	if hub.services["hub"].Broken {
		hub.WriteString(text.Pretty(s, 0, 92))
		return
	}
	hub.WriteString(text.Pretty(s, 0, hub.getSV("width").V.(int)))
}

func (hub *Hub) isAdministered() bool {
	_, err := os.Stat(hub.directory + "user/admin.dat")
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
	cwd, _ := filepath.Abs(filepath.Dir(os.Args[0]))
	file, err := os.Open(cwd + "/rsc/text/helpfile.txt")
	if err != nil {
		panic("Can't find helpfile 'rsc/text/helpfile.txt'.")
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
	serviceName := "#" + fmt.Sprint(hub.anonymousServiceNumber)
	hub.StartAndMakeCurrent("", serviceName, scriptFilepath)
	hub.lastRun = []string{serviceName}
	hub.anonymousServiceNumber = hub.anonymousServiceNumber + 1
}

func (hub *Hub) StartAndMakeCurrent(username, serviceName, scriptFilepath string) bool {
	if hub.administered {
		err := database.UpdateService(hub.Db, username, serviceName)
		if err != nil {
			hub.WriteError("u/ " + err.Error())
			return false
		}
	}
	hub.createService(serviceName, scriptFilepath)
	hub.setServiceName(serviceName)
	return true
}

func (hub *Hub) tryMain() { // Guardedly tries to run the `main` command.
	if !hub.services[hub.currentServiceName()].Broken && hub.services[hub.currentServiceName()].Cp.GetParser().Unfixes.Contains("main") {
		val := ServiceDo(hub.services[hub.currentServiceName()], "main")
		hub.lastRun = []string{hub.currentServiceName()}
		hub.services[hub.currentServiceName()].Visited = true
		if val.T == values.ERROR {
			hub.WritePretty("\n[0] " + valToString(hub.services[hub.currentServiceName()], val))
			hub.WriteString("\n")
			hub.ers = []*report.Error{val.V.(*report.Error)}
		} else {
			hub.WriteString(valToString(hub.services[hub.currentServiceName()], val))
		}
	}
}

func (hub *Hub) serviceNeedsUpdate(name string) bool {
	serviceToUpdate, present := hub.services[name]
	if !present {
		return true
	}
	if name == "" {
		return false
	}
	needsUpdate, err := serviceToUpdate.NeedsUpdate()
	if err != nil {
		hub.WriteError("v/ " + err.Error())
		return false
	}
	return needsUpdate
}

func (hub *Hub) createService(name, scriptFilepath string) bool {
	needsRebuild := hub.serviceNeedsUpdate(name)
	if !needsRebuild {
		return false
	}
	var (
		newService *service.Service
		init       *service.Initializer
	)
	newService, init = service.StartService(scriptFilepath, hub.directory, hub.Db, hub.services)
	hub.services[name] = newService
	hub.Sources = init.Sources

	if init.ErrorsExist() {
		newService.Broken = true
		if name == "hub" {
			fmt.Println("Pipefish: unable to compile hub.")
		}
		hub.GetAndReportErrors(init.Parser)
		if name == "hub" {
			os.Exit(2)
		}
		return false
	}

	return true
}

func (hub *Hub) GetAndReportErrors(p *parser.Parser) {
	hub.ers = p.Errors
	hub.WritePretty(report.GetList(hub.ers))
}

func (hub *Hub) CurrentServiceIsBroken() bool {
	return hub.services[hub.currentServiceName()].Broken
}

var prefix = `newtype

TextDisplayMode = enum LITERAL, STRING
DatabaseDrivers = enum COCKROACHDB, FIREBIRD_SQL, MARIADB, MICROSOFT_SQL_SERVER, MYSQL, ORACLE, 
                    .. POSTGRESQL, SNOWFLAKE, SQLITE, TIDB

Database = struct(driver DatabaseDrivers, name, host string, port int, username, password string)

var

`

func (hub *Hub) saveHubFile() string {
	hubService := hub.services["hub"]
	var buf strings.Builder
	buf.WriteString(prefix)
	buf.WriteString("allServices = map(")
	serviceList := []string{}
	for k := range hub.services {
		if k != "" && k[0] != '#' {
			serviceList = append(serviceList, k)
		}
	}
	for i, v := range serviceList {
		buf.WriteString("\"")
		buf.WriteString(v)
		buf.WriteString("\"::\"")
		buf.WriteString(hub.services[v].Cp.ScriptFilepath)
		buf.WriteString("\"")
		if i < len(serviceList)-1 {
			buf.WriteString(",\n               .. ")
		}
	}
	buf.WriteString(")\n\n")
	buf.WriteString("currentService string? = ")
	cs := hubService.Mc.Literal(hub.getSV("currentService"))
	if len(cs) > 0 && cs[0] == '#' {
		buf.WriteString("NULL")
	} else {
		buf.WriteString(cs)
	}
	buf.WriteString("\n\n")
	buf.WriteString("isLive = ")
	buf.WriteString(hubService.Mc.Literal(hub.getSV("isLive")))
	buf.WriteString("\n")
	buf.WriteString("display = ")
	buf.WriteString(hubService.Mc.Literal(hub.getSV("display")))
	buf.WriteString("\n")
	buf.WriteString("width = ")
	buf.WriteString(hubService.Mc.Literal(hub.getSV("width")))
	buf.WriteString("\n\n")
	buf.WriteString("database Database? = ")
	dbVal := hub.getSV("database")
	if dbVal.T == values.NULL {
		buf.WriteString("NULL\n")
	} else {
		args := dbVal.V.([]values.Value)
		buf.WriteString("Database with (driver::")
		buf.WriteString(hubService.Mc.Literal(args[0]))
		buf.WriteString(",\n")
		buf.WriteString("                                 .. name::")
		buf.WriteString(hubService.Mc.Literal(args[1]))
		buf.WriteString(",\n")
		buf.WriteString("                                 .. host::")
		buf.WriteString(hubService.Mc.Literal(args[2]))
		buf.WriteString(",\n")
		buf.WriteString("                                 .. port::")
		buf.WriteString(hubService.Mc.Literal(args[3]))
		buf.WriteString(",\n")
		buf.WriteString("                                 .. username::")
		buf.WriteString(hubService.Mc.Literal(args[4]))
		buf.WriteString(",\n")
		buf.WriteString("                                 .. password::")
		buf.WriteString(hubService.Mc.Literal(args[5]))
		buf.WriteString(")\n")
	}

	fname := service.MakeFilepath(hub.hubFilepath, hub.directory)

	f, err := os.Create(fname)
	if err != nil {
		return text.HUB_ERROR + "os reports \"" + strings.TrimSpace(err.Error()) + "\".\n"
	}
	defer f.Close()
	f.WriteString(buf.String())
	return text.OK

}

func (hub *Hub) OpenHubFile(hubFilepath string) {
	hub.createService("hub", hubFilepath)
	hubService := hub.services["hub"]
	hub.hubFilepath = service.MakeFilepath(hubFilepath, hub.directory)
	services := hubService.GetVariable("allServices").V.(*values.Map).AsSlice()

	var driver, name, host, username, password string
	var port int

	if hub.hasDatabase() {
		driver, name, host, port, username, password = hub.getDB()
	}
	hub.Db, _ = database.GetdB(driver, host, name, port, username, password)

	for _, pair := range services {
		serviceName := pair.Key.V.(string)
		serviceFilepath := pair.Val.V.(string)
		hub.createService(serviceName, serviceFilepath)
	}
	hub.createService("", "")

	hub.list()
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
			hub.WritePretty("Service '" + k + "' running script '" + filepath.Base(hub.services[k].Cp.ScriptFilepath) + "'.")
		} else {
			hub.WriteString(text.GOOD_BULLET)
			hub.WritePretty("Service '" + k + "' running script '" + filepath.Base(hub.services[k].Cp.ScriptFilepath) + "'.")
		}
	}
	hub.WriteString("\n")
}

func (hub *Hub) TestScript(scriptFilepath string, testOutputType service.TestOutputType) {

	fname := filepath.Base(scriptFilepath)
	fname = fname[:len(fname)-len(filepath.Ext(fname))]
	dname := filepath.Dir(scriptFilepath)
	directoryName := dname + "/-tests/" + fname

	hub.oldServiceName = hub.currentServiceName()
	files, _ := os.ReadDir(directoryName)
	for _, testFileInfo := range files {
		testFilepath := directoryName + "/" + testFileInfo.Name()
		hub.RunTest(scriptFilepath, testFilepath, testOutputType)
	}
	_, ok := hub.services["#test"]
	if ok {
		delete(hub.services, "#test")
	}
	hub.setServiceName(hub.oldServiceName)

}

func (hub *Hub) RunTest(scriptFilepath, testFilepath string, testOutputType service.TestOutputType) {

	f, err := os.Open(testFilepath)
	if err != nil {
		hub.WriteError("A/ " + strings.TrimSpace(err.Error()) + "/n")
		return
	}

	scanner := bufio.NewScanner(f)
	scanner.Scan()
	testType := strings.Split(scanner.Text(), ": ")[1]
	if testType == service.RECORD {
		f.Close() // TODO --- shouldn't this do something?
		return
	}
	scanner.Scan()
	if !hub.StartAndMakeCurrent("", "#test", scriptFilepath) {
		hub.WriteError("Can't initialize script " + text.Emph(scriptFilepath))
		return
	}
	hub.services["#test"].Mc.IoHandle =
		service.MakeTestIoHandler(hub.out, scanner, testOutputType)
	if testOutputType == service.ERROR_CHECK {
		hub.WritePretty("Running test '" + testFilepath + "'.\n")
	}
	ServiceDo((*hub).services["#test"], "$view = \"\"")
	testService := (*hub).services["#test"]
	_ = scanner.Scan() // eats the newline
	executionMatchesTest := true
	for scanner.Scan() {
		lineIn := scanner.Text()[3:]
		if testOutputType == service.SHOW_ALL {
			hub.WriteString("-> " + lineIn + "\n")
		}
		result := ServiceDo(testService, lineIn)
		if testService.Cp.GetParser().ErrorsExist() {
			hub.WritePretty(testService.Cp.GetParser().ReturnErrors())
			f.Close()
			continue
		}
		scanner.Scan()
		lineOut := scanner.Text()
		nonIoError := valToString(testService, result) != lineOut
		newError := nonIoError ||
			testService.Mc.IoHandle.InHandle.(*service.TestInHandler).Fail ||
			testService.Mc.IoHandle.OutHandle.(*service.TestOutHandler).Fail
		if newError {
			testService.Mc.IoHandle.InHandle.(*service.TestInHandler).Fail = false
			testService.Mc.IoHandle.OutHandle.(*service.TestOutHandler).Fail = false
			executionMatchesTest = false
			if testOutputType == service.SHOW_DIFF && nonIoError {
				hub.WriteString("-> " + lineIn + "\n" + text.WAS + lineOut + "\n" + text.GOT + valToString(testService, result) + "\n")
			}
			if testOutputType == service.SHOW_ALL && nonIoError {
				hub.WriteString(text.WAS + lineOut + "\n" + text.GOT + valToString(testService, result) + "\n")
			}
		}
		if !newError && testOutputType == service.SHOW_ALL {
			hub.WriteString(lineOut + "\n")
		}
	}
	if testOutputType == service.ERROR_CHECK {
		if executionMatchesTest && testType == service.BAD {
			hub.WriteError("bad behavior reproduced by test" + "\n")
			f.Close()
			hub.RunTest(scriptFilepath, testFilepath, service.SHOW_ALL)
			return
		}
		if !executionMatchesTest && testType == service.GOOD {
			hub.WriteError("good behavior not reproduced by test" + "\n")
			f.Close()
			hub.RunTest(scriptFilepath, testFilepath, service.SHOW_ALL)
			return
		}
		hub.WriteString(text.TEST_PASSED)
	}
	f.Close()
}

func (hub *Hub) playTest(testFilepath string, diffOn bool) {
	f, err := os.Open(testFilepath)
	if err != nil {
		hub.WriteError("C/ " + strings.TrimSpace(err.Error()) + "/n")
		return
	}
	scanner := bufio.NewScanner(f)
	scanner.Scan()
	_ = scanner.Text() // test type doesn't matter
	scanner.Scan()
	scriptFilepath := (scanner.Text())[8:]
	scanner.Scan()
	hub.StartAndMakeCurrent("", "#test", scriptFilepath)
	ServiceDo((*hub).services["#test"], "$view = \"\"")
	testService := (*hub).services["#test"]
	testService.Mc.IoHandle = service.MakeTestIoHandler(hub.out, scanner, service.SHOW_ALL)
	_ = scanner.Scan() // eats the newline
	for scanner.Scan() {
		lineIn := scanner.Text()[3:]
		scanner.Scan()
		lineOut := scanner.Text()
		result := ServiceDo(testService, lineIn)
		if testService.Cp.GetParser().ErrorsExist() {
			hub.WritePretty(testService.Cp.GetParser().ReturnErrors())
			f.Close()
			return
		}
		hub.WriteString("#test → " + lineIn + "\n")

		if valToString(testService, result) == lineOut || !diffOn {
			hub.WriteString(valToString(testService, result) + "\n")
		} else {
			hub.WriteString(text.WAS + lineOut + "\n" + text.GOT + valToString(testService, result) + "\n")
		}
	}
}

func valToString(srv *service.Service, val values.Value) string {
	// TODO --- the exact behavior of this function should depend on service variables but I haven't put them in the VM yet.
	// Alternately we can leave it as it is and have the vm's Describe method take care of it.
	return srv.Mc.StringifyValue(val, service.LITERAL)
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
	h.Do(input, "", "", h.currentServiceName())
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
			h.WriteError("D/ " + err.Error())
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
// a regular Charm service. TODO --- you can do this now!
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
	_, err := os.Stat(h.directory + "user/admin.dat")
	if errors.Is(err, os.ErrNotExist) {
		h.WriteError("this Charm hub doesn't have administered " +
			"access: there is nothing to join.")
		return
	}
	if err != nil {
		h.WriteError("E/ " + err.Error())
		return
	}
	if f.Result["*Password"] != f.Result["*Confirm password"] {
		h.WriteError("passwords don't match.")
		return
	}

	err = database.AddUser(h.Db, f.Result["Username"], f.Result["First name"],
		f.Result["Last name"], f.Result["Email"], f.Result["*Password"], "")
	if err != nil {
		h.WriteError("F/ " + err.Error())
		return
	}
	err = database.AddUserToGroup(h.Db, f.Result["Username"], "Guests", false)
	if err != nil {
		h.WriteError("G/ " + err.Error())
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
		f.Result["Last name"], f.Result["Email"], f.Result["*Password"], h.currentServiceName(), h.directory)
	if err != nil {
		h.WriteError("H/ " + err.Error())
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
		h.WriteError("I/ " + err.Error())
		h.WriteString("Please try again.\n\n")
		return
	}
	h.Username = f.Result["Username"]
	h.Password = f.Result["*Password"]
	h.WriteString(text.OK + "\n")
}

func (h *Hub) configDb() {
	h.CurrentForm = &Form{Fields: []string{database.GetDriverOptions(), "Host", "Port", "Database name", "Username for database access", "*Password for database access"},
		Call:   func(f *Form) { h.handleConfigDbForm(f) },
		Result: make(map[string]string)}
}

func (h *Hub) handleConfigDbForm(f *Form) {
	h.CurrentForm = nil
	number, err := strconv.Atoi(f.Result[database.GetDriverOptions()])
	if err != nil {
		h.WriteError("hub/db/config/a: " + err.Error())
		return
	}
	port, err := strconv.Atoi(f.Result["Port"])
	if err != nil {
		h.WriteError("hub/db/config/b: " + err.Error())
		return
	}
	DbDriverAsPfEnum := database.GetSortedDrivers()[number]
	h.Db, err = database.GetdB(DbDriverAsPfEnum, f.Result["Host"], f.Result["Database name"], port,
		f.Result["Username for database access"], f.Result["*Password for database access"])
	h.setDB(DbDriverAsPfEnum, f.Result["Host"], f.Result["Database name"], port,
		f.Result["Username for database access"], f.Result["*Password for database access"])
	if err != nil {
		h.WriteError("hub/db/config/c: " + err.Error())
		return
	}

	for _, v := range h.services {
		v.Mc.Database = h.Db
	}

	h.WriteString(text.OK + "\n")
}

// We return the parser because this is where any compile-time errors in lex-parse-compile will end up.
func ServiceDo(serviceToUse *service.Service, line string) values.Value {
	return serviceToUse.Cp.Do(line)
}
