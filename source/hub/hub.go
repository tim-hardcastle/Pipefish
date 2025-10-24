package hub

import (
	"bufio"
	"bytes"
	"crypto/aes"
	"crypto/cipher"
	"crypto/sha256"
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

	"github.com/caddyserver/certmagic"
	"github.com/lmorg/readline/v4"
	"golang.org/x/crypto/pbkdf2"

	"github.com/tim-hardcastle/Pipefish/source/database"
	"github.com/tim-hardcastle/Pipefish/source/pf"
	"github.com/tim-hardcastle/Pipefish/source/text"
	"github.com/tim-hardcastle/Pipefish/source/values"
)

type Hub struct {
	hubFilepath            string
	services               map[string]*pf.Service // The services the hub knows about.
	ers                    []*pf.Error            // The errors produced by the latest compilation/execution of one of the hub's services.
	in                     io.Reader
	out                    io.Writer
	anonymousServiceNumber int
	snap                   *Snap
	oldServiceName         string // Somewhere to keep the old service name while taking a snap. TODO --- you can now take snaps on their own dedicated hub, saving a good deal of faffing around.
	Sources                map[string][]string
	lastRun                []string
	CurrentForm            *Form // TODO!!! --- deprecate, you've had IO for a while.
	Db                     *sql.DB
	administered           bool
	listeningToHttpOrHttps        bool
	port                   string
	Username               string
	Password               string
	pipefishHomeDirectory  string
	store                  values.Map
	storekey               string
}

func New(in io.Reader, out io.Writer) *Hub {

	hub := Hub{
		services: make(map[string]*pf.Service),
		in:       in,
		out:      out,
		lastRun:  []string{},
	}
	appDir, _ := filepath.Abs(filepath.Dir(os.Args[0]))
	hub.pipefishHomeDirectory = appDir + "/"
	return &hub
}

func (hub *Hub) currentServiceName() string {
	cs := hub.getSV("currentService")
	if cs.T == pf.NULL {
		return ""
	} else {
		return cs.V.(string)
	}
}

func (hub *Hub) hasDatabase() bool {
	return hub.getSV("database").T != pf.NULL
}

func (hub *Hub) getDB() (string, string, string, int, string, string) {
	dbStruct := hub.getSV("database").V.([]pf.Value)
	driver := hub.services["hub"].ToLiteral(dbStruct[0])
	return driver, dbStruct[1].V.(string), dbStruct[2].V.(string), dbStruct[3].V.(int), dbStruct[4].V.(string), dbStruct[5].V.(string)
}

func (hub *Hub) setDB(driver, name, path string, port int, username, password string) {
	hubService := hub.services["hub"]
	driverAsEnumValue, _ := hubService.Do(driver)
	structType, _ := hubService.TypeNameToType("Database")
	hub.setSV("database", structType, []pf.Value{driverAsEnumValue, {pf.STRING, name}, {pf.STRING, path}, {pf.INT, port}, {pf.STRING, username}, {pf.STRING, password}})
}

func (hub *Hub) isLive() bool {
	return hub.getSV("isLive").V.(bool)
}

func (hub *Hub) setLive(b bool) {
	hub.setSV("isLive", pf.BOOL, b)
}

func (hub *Hub) setServiceName(name string) {
	hub.setSV("currentService", pf.STRING, name)
}

func (hub *Hub) makeEmptyServiceCurrent() {
	hub.setSV("currentService", pf.NULL, nil)
}

func (hub *Hub) getSV(sv string) pf.Value {
	v, _ := hub.services["hub"].GetVariable(sv)
	return v
}

func (hub *Hub) setSV(sv string, ty pf.Type, v any) {
	hub.services["hub"].SetVariable(sv, ty, v)
}

// This converts a string identifying the color of a token (e.g. `type`,
// `number`, to Linux control codes giving the correct coloring according
// to the color theme of the hub.)
func (hub *Hub) getFonts() *values.Map {
	theme := hub.getSV("theme")
	if theme.V == nil {
		return nil
	}
	mapOfThemes := hub.getSV("THEMES").V.(*values.Map)
	mapForTheme, themeExists := mapOfThemes.Get(theme)
	if !themeExists {
		return nil
	}
	fonts := mapForTheme.V.(*values.Map)
	return fonts
}

// This takes the input from the REPL, interprets it as a hub command if it begins with 'hub';
// as an instruction to the os if it begins with 'os', and as an expression to be passed to
// the current service if none of the above hold.
func (hub *Hub) Do(line, username, password, passedServiceName string, external bool) (string, bool) {

	if hub.administered && !hub.listeningToHttpOrHttps && hub.Password == "" &&
		!(line == "hub register" || line == "hub log on" || line == "hub quit") {
		hub.WriteError("this is an administered hub and you aren't logged on. Please enter either " +
			"`hub register` to register as a user, or `hub log on` to log on if you're already registered " +
			"with this hub.")
		return passedServiceName, false
	}

	// Otherwise, we're talking to the current service.

	serviceToUse, ok := hub.services[passedServiceName]
	if !ok {
		hub.WriteError("the hub can't find the service <C>\"" + passedServiceName + "\"</>.")
		return passedServiceName, false
	}

	// The service may be broken, in which case we'll let the empty service handle the input.

	if serviceToUse.IsBroken() {
		serviceToUse = hub.services[""]
	}
	hub.Sources["REPL input"] = []string{line}
	needsUpdate := hub.serviceNeedsUpdate(hub.currentServiceName())
	if hub.isLive() && needsUpdate {
		path, _ := hub.services[hub.currentServiceName()].GetFilepath()
		hub.StartAndMakeCurrent(hub.Username, hub.currentServiceName(), path)
		serviceToUse = hub.services[hub.currentServiceName()]
		if serviceToUse.IsBroken() {
			return passedServiceName, false
		}
	}

	// We may be talking to the hub itself.

	hubWords := strings.Fields(line)
	if len(hubWords) > 0 && hubWords[0] == "hub" {
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

	if len(hubWords) > 0 && hubWords[0] == "os" {
		if hub.isAdministered() {
			hub.WriteError("for reasons of safety and sanity, the `os` prefix doesn't work in administered hubs.")
			return passedServiceName, false
		}
		if len(hubWords) == 3 && hubWords[1] == "cd" { // Because cd changes the directory for the current
			os.Chdir(hubWords[2])     // process, if we did it with exec it would do it for
			hub.WriteString(GREEN_OK) // that process and not for Pipefish.
			return passedServiceName, false
		}
		command := exec.Command(hubWords[1], hubWords[2:]...)
		out, err := command.Output()
		if err != nil {
			hub.WriteError(err.Error())
			return passedServiceName, false
		}
		if len(out) == 0 {
			hub.WriteString(GREEN_OK)
			return passedServiceName, false
		}
		hub.WriteString(string(out))
		return passedServiceName, false
	}

	if match, _ := regexp.MatchString(`^\s*(|\/\/.*)$`, line); match {
		hub.WriteString("")
		return passedServiceName, false
	}

	if hub.currentServiceName() == "#snap" {
		hub.snap.AddInput(line)
	}

	// *** THIS IS THE BIT WHERE WE DO THE THING!
	val := ServiceDo(serviceToUse, line)
	// *** FROM ALL THAT LOGIC, WE EXTRACT ONE PIPEFISH VALUE !!!
	errorsExist, _ := serviceToUse.ErrorsExist()
	if errorsExist { // Any lex-parse-compile errors should end up in the parser of the compiler of the service, returned in p.
		hub.GetAndReportErrors(serviceToUse)
		return passedServiceName, false
	}

	if val.T == pf.ERROR && !external {
		hub.WriteString("\n[0] " + valToString(serviceToUse, val))
		hub.WriteString("\n")
		hub.ers = []*pf.Error{val.V.(*pf.Error)}
		if len(val.V.(*pf.Error).Values) > 0 {
			hub.WritePretty("Values are available with `hub values`.\n\n")
		}
	} else if !serviceToUse.PostHappened() {
		serviceToUse.Output(val)
		if hub.currentServiceName() == "#snap" {
			hub.snap.AddOutput(serviceToUse.ToLiteral(val))
		}
	}
	return passedServiceName, false
}

func (hub *Hub) ParseHubCommand(line string) (string, []values.Value) {
	hubService := hub.services["hub"]
	hubReturn := ServiceDo(hubService, line)
	if errorsExist, _ := hubService.ErrorsExist(); errorsExist { // Any lex-parse-compile errors should end up in the parser of the compiler of the service, returned in p.
		hub.GetAndReportErrors(hubService)
		return "error", []values.Value{}
	}
	if hubReturn.T == pf.ERROR {
		hub.WriteError(hubReturn.V.(*pf.Error).Message)
		return "error", []values.Value{hubReturn}
	}
	hrType, _ := hubService.TypeNameToType("HubResponse")
	if hubReturn.T == hrType {
		hR := hubReturn.V.([]pf.Value)
		verb := hR[0].V.(string)
		vargs := hR[1].V.(pf.List)
		args := []values.Value{}
		for i := 0; i < vargs.Len(); i++ {
			el, _ := vargs.Index(i)
			args = append(args, el.(values.Value))
		}
		return verb, args
	}
	if hubReturn.T == pf.OK {
		if hub.getSV("display").V.(int) == 0 {
			hub.WriteString("OK" + "\n")
		} else {
			hub.WriteString(GREEN_OK + "\n")
		}
		return "OK", nil
	}

	hub.WriteError("couldn't parse hub instruction.")
	return "error", []values.Value{values.Value{values.UNDEFINED_TYPE, nil}}
}

// Quick and dirty auxilliary function for when we know the value is in fact a string.
func toStr(v pf.Value) string {
	return v.V.(string)
}

func (hub *Hub) DoHubCommand(username, password, verb string, args []values.Value) bool {
	if (!hub.isAdministered()) &&
		(verb == "add" || verb == "create" || verb == "log-on" || verb == "log-off" ||
			verb == "let" || verb == "register" || verb == "groups" ||
			verb == "groups-of-user" || verb == "groups-of-service" || verb == "services of group" ||
			verb == "services-of-user" || verb == "users-of-service" || verb == "users-of-group" ||
			verb == "let-use" || verb == "let-own") {
		hub.WriteError("this is not an administered hub. To initialize it as one, first do `hub config db` " +
			"(if you haven't already) and then `hub config admin`.")
		return false
	}
	if hub.isAdministered() {
		isAdmin, err := database.IsUserAdmin(hub.Db, username)
		if err != nil {
			hub.WriteError(err.Error())
			return false
		}
		// TODO --- replace with set containing green list.
		if !isAdmin && (verb == "config-db" || verb == "create" || verb == "let" ||
			verb == "live-on" || verb == "live-off" || verb == "listen" ||
			verb == "run" || verb == "reset" || verb == "rerun" ||
			verb == "replay" || verb == "replay-diff" || verb == "snap" || verb == "test" ||
			verb == "groups-of-user" || verb == "groups-of-service" || verb == "services of group" ||
			verb == "services-of-user" || verb == "users-of-service" || verb == "users-of-group" ||
			verb == "let-use" || verb == "let-own" || verb == "store" || verb == "store-secret") {
			hub.WriteError("you don't have the admin status necessary to do that.")
			return false
		}
		if username == "" && !(verb == "log-on" || verb == "register" || verb == "quit") {
			hub.WriteError("\nThis is an administered hub and you aren't logged on. Please enter either " +
				"`hub register` to register as a user, or `hub log on` to log on if you're already registered " +
				"with this hub.\n\n")
			return false
		}
	}
	switch verb {
	case "add":
		err := database.IsUserGroupOwner(hub.Db, username, toStr(args[1]))
		if err != nil {
			hub.WriteError(err.Error())
			return false
		}
		err = database.AddUserToGroup(hub.Db, toStr(args[0]), toStr(args[1]), false)
		if err != nil {
			hub.WriteError(err.Error())
			return false
		}
		hub.WriteString(GREEN_OK + "\n")
		return false
	case "api":
		hub.WriteString(hub.services[hub.currentServiceName()].Api(hub.currentServiceName(), hub.getFonts(), hub.getSV("width").V.(int)))
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
		err := database.AddGroup(hub.Db, toStr(args[0]))
		if err != nil {
			hub.WriteError(err.Error())
			return false
		}
		err = database.AddUserToGroup(hub.Db, username, toStr(args[0]), true)
		if err != nil {
			hub.WriteError(err.Error())
			return false
		}
		hub.WriteString(GREEN_OK + "\n")
		return false
	case "edit":
		command := exec.Command("vim", toStr(args[0]))
		command.Stdin = os.Stdin
		command.Stdout = os.Stdout
		err := command.Run()
		if err != nil {
			hub.WriteError(err.Error())
		}
		return false
	case "errors":
		r, _ := hub.services[hub.currentServiceName()].GetErrorReport()
		hub.WritePretty(r)
		return false
	case "groups-of-user":
		result, err := database.GetGroupsOfUser(hub.Db, toStr(args[0]), false)
		if err != nil {
			hub.WriteError(err.Error())
		} else {
			hub.WriteString(result)
			return false
		}
	case "groups-of-service":
		result, err := database.GetGroupsOfService(hub.Db, toStr(args[0]))
		if err != nil {
			hub.WriteError(err.Error())
		} else {
			hub.WriteString(result)
			return false
		}
	case "halt":
		var name string
		_, ok := hub.services[toStr(args[0])]
		if ok {
			name = toStr(args[0])
		} else {
			hub.WriteError("the hub can't find the service <C>\"" + toStr(args[0]) + "\"</>.")
			return false
		}
		if name == "" || name == "hub" {
			hub.WriteError("the hub doesn't know what you want to stop.")
			return false
		}
		delete(hub.services, name)
		hub.WriteString(GREEN_OK + "\n")
		if name == hub.currentServiceName() {
			hub.makeEmptyServiceCurrent()
		}
		return false
	case "help":
		if helpMessage, ok := helpStrings[toStr(args[0])]; ok {
			hub.WritePretty(helpMessage + "\n")
			return false
		} else {
			hub.WriteError("the `hub help` command doesn't accept " +
				"`\"" + toStr(args[0]) + "\"` as a parameter.")
			return false
		}
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
		err = database.LetGroupUseService(hub.Db, toStr(args[0]), toStr(args[1]))
		if err != nil {
			hub.WriteError(err.Error())
			return false
		}
		hub.WriteString(GREEN_OK + "\n")
		return false
	case "http":
		hub.WriteString(GREEN_OK)
		go hub.StartHttp([]string{toStr(args[0])}, false)
		return false
	case "https":
		if len(args) == 0 {
			hub.WriteError("list of domain names cannot be empty")
			return false
		}
		hub.WriteString(GREEN_OK)
		domains := []string{}
		for _, arg := range args {
			domains = append(domains, toStr(arg))
		}
		go hub.StartHttp(domains, true)
		return false
	case "live-on":
		hub.setLive(true)
		return false
	case "live-off":
		hub.setLive(false)
		return false
	case "log":
		tracking, _ := hub.services[hub.currentServiceName()].GetTrackingReport()
		hub.WritePretty(tracking)
		hub.WriteString("\n")
		return false
	case "log-on":
		hub.getLogin()
		return false
	case "log-off":
		hub.Username = ""
		hub.Password = ""
		hub.makeEmptyServiceCurrent()
		hub.WriteString("\n" + GREEN_OK + "\n")
		hub.WritePretty("\nThis is an administered hub and you aren't logged on. Please enter either " +
			"`hub register` to register as a user, or `hub log on` to log on if you're already registered " +
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
	case "quit":
		hub.quit()
		return true
	case "register":
		hub.addUserAsGuest()
		return false
	case "replay":
		hub.oldServiceName = hub.currentServiceName()
		hub.playTest(toStr(args[0]), false)
		hub.setServiceName(hub.oldServiceName)
		_, ok := hub.services["#test"]
		if ok {
			delete(hub.services, "#test")
		}
		return false
	case "replay-diff":
		hub.oldServiceName = hub.currentServiceName()
		hub.playTest(toStr(args[0]), true)
		hub.setServiceName(hub.oldServiceName)
		_, ok := hub.services["#test"]
		if ok {
			delete(hub.services, "#test")
		}
		return false
	case "reset":
		serviceToReset, ok := hub.services[hub.currentServiceName()]
		if !ok {
			hub.WriteError("the hub can't find the service <C>\"" + hub.currentServiceName() + "\".")
			return false
		}
		if hub.currentServiceName() == "" {
			hub.WriteError("service is empty, nothing to reset.")
			return false
		}
		filepath, _ := serviceToReset.GetFilepath()
		hub.WritePretty("Restarting script <C>\"" + filepath +
			"\" as service <C>\"" + hub.currentServiceName() + "\"</>.\n")
		hub.StartAndMakeCurrent(username, hub.currentServiceName(), filepath)
		hub.lastRun = []string{hub.currentServiceName()}
		return false
	case "rerun":
		if len(hub.lastRun) == 0 {
			hub.WriteError("nothing to rerun.")
			return false
		}
		filepath, _ := hub.services[hub.lastRun[0]].GetFilepath()
		hub.WritePretty("Rerunning script <C>\"" + filepath +
			"</>\" as service <C>\"" + hub.lastRun[0] + "\"</>.\n")
		hub.StartAndMakeCurrent(username, hub.lastRun[0], filepath)
		hub.tryMain()
		return false
	case "run":
		fname := toStr(args[0])
		sname := toStr(args[1])
		if filepath.IsLocal(fname) {
			dir, _ := os.Getwd()
			fname = filepath.Join(dir, fname)
		}
		hub.lastRun = []string{fname, sname}
		if sname == "" {
			hub.WritePretty("Starting script <C>\"" + filepath.Base(fname) +
				"\" as service <C>\"#" + strconv.Itoa(hub.anonymousServiceNumber) + "\"</>.\n")
			hub.StartAnonymous(fname)
			hub.tryMain()
			return false
		}
		hub.WritePretty("Starting script <C>\"" + filepath.Base(fname) + "\"</> as service <C>\"" + sname + "\"</>.\n")
		hub.StartAndMakeCurrent(username, sname, fname)
		hub.tryMain()
		return false
	case "serialize":
		hub.WriteString(hub.services[toStr(args[0])].SerializeApi())
		return false
	case "services-of-user":
		result, err := database.GetServicesOfUser(hub.Db, toStr(args[0]), false)
		if err != nil {
			hub.WriteError(err.Error())
		} else {
			hub.WriteString(result)
			return false
		}
	case "services-of-group":
		result, err := database.GetServicesOfGroup(hub.Db, toStr(args[0]))
		if err != nil {
			hub.WriteError(err.Error())
		} else {
			hub.WriteString(result)
			return false
		}
	case "snap":
		scriptFilepath := toStr(args[0])
		if filepath.IsLocal(scriptFilepath) {
			dir, _ := os.Getwd()
			scriptFilepath = filepath.Join(dir, scriptFilepath)
		}
		testFilepath := toStr(args[1])
		if testFilepath == "" {
			testFilepath = getUnusedTestFilename(scriptFilepath) // If no filename is given, we just generate one.
		}
		hub.snap = NewSnap(scriptFilepath, testFilepath)
		hub.oldServiceName = hub.currentServiceName()
		if hub.StartAndMakeCurrent(username, "#snap", scriptFilepath) {
			snapService := hub.services["#snap"]
			ty, _ := snapService.TypeNameToType("$_OutputAs")
			snapService.SetVariable("$_outputAs", ty, 0)
			hub.WriteString("Serialization is ON.\n")
			in, out := MakeSnapIo(snapService, hub.out, hub.snap)
			currentService := snapService
			currentService.SetInHandler(in)
			currentService.SetOutHandler(out)
		} else {
			hub.WriteError("failed to start snap")
		}
		return false
	case "snap-good":
		if hub.currentServiceName() != "#snap" {
			hub.WriteError("you aren't taking a snap.")
			return false
		}
		result := hub.snap.Save(GOOD)
		hub.WriteString(result + "\n")
		hub.setServiceName(hub.oldServiceName)
		return false
	case "snap-bad":
		if hub.currentServiceName() != "#snap" {
			hub.WriteError("you aren't taking a snap.")
			return false
		}
		result := hub.snap.Save(BAD)
		hub.WriteString(result + "\n")
		hub.setServiceName(hub.oldServiceName)
		return false
	case "snap-record":
		if hub.currentServiceName() != "#snap" {
			hub.WriteError("you aren't taking a snap.")
			return false
		}
		result := hub.snap.Save(RECORD)
		hub.WriteString(result + "\n")
		hub.setServiceName(hub.oldServiceName)
		return false
	case "snap-discard":
		if hub.currentServiceName() != "#snap" {
			hub.WriteError("you aren't taking a snap.")
			return false
		}
		hub.WriteString(GREEN_OK + "\n")
		hub.setServiceName(hub.oldServiceName)
		return false
	case "store":
		hub.store = *hub.store.Set(args[0].V.([]values.Value)[0], args[0].V.([]values.Value)[1])
		hub.SaveHubStore()
		hub.WriteString(GREEN_OK + "\n")
		return false
	case "storekey":
		if hub.storekey != "" {
			rline := readline.NewInstance()
			rline.SetPrompt("Enter the current storekey for the hub: ")
			rline.PasswordMask = '▪'
			storekey, _ := rline.Readline()
			if storekey != hub.storekey {
				hub.WriteError("incorrect store key.")
				return false
			}
		}
		rline := readline.NewInstance()
		rline.SetPrompt("Enter the new storekey: ")
		rline.PasswordMask = '▪'
		storekey, _ := rline.Readline()
		hub.storekey = storekey
		hub.SaveHubStore()
		return false
	case "switch":
		sname := toStr(args[0])
		_, ok := hub.services[sname]
		if ok {
			hub.WriteString(GREEN_OK + "\n")
			if hub.administered {
				access, err := database.DoesUserHaveAccess(hub.Db, username, sname)
				if err != nil {
					hub.WriteError("o/ " + err.Error())
					return false
				}
				if !access {
					hub.WriteError("you don't have access to service <C>\"" + sname + "\"</>.")
					return false
				}
				database.UpdateService(hub.Db, username, sname)
				return false
			} else {
				hub.setServiceName(sname)
				return false
			}
		}
		hub.WriteError("service <C>\"" + sname + "\"</> doesn't exist")
		return false
	case "test":
		fname := toStr(args[0])
		if filepath.IsLocal(fname) {
			dir, _ := os.Getwd()
			fname = filepath.Join(dir, fname)
		}
		file, err := os.Open(fname)
		if err != nil {
			hub.WriteError(strings.TrimSpace(err.Error()) + "\n")
			return false
		}
		defer file.Close()
		fileInfo, err := file.Stat()
		if err != nil {
			hub.WriteError(strings.TrimSpace(err.Error()) + "\n")
			return false
		}

		if fileInfo.IsDir() {
			files, err := file.Readdir(0)
			if err != nil {
				hub.WriteError(strings.TrimSpace(err.Error()) + "\n")
				return false
			}

			for _, potentialPfFile := range files {
				if filepath.Ext(potentialPfFile.Name()) == ".pf" {
					hub.TestScript(fname+"/"+potentialPfFile.Name(), ERROR_CHECK)
				}
			}
		} else {
			hub.TestScript(fname, ERROR_CHECK)
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
		hub.WritePretty(pf.GetTraceReport(hub.ers[0]))
		return false

	case "users-of-group":
		result, err := database.GetUsersOfGroup(hub.Db, toStr(args[0]))
		if err != nil {
			hub.WriteError(err.Error())
		} else {
			hub.WriteString(result)
			return false
		}
	case "users-of-service":
		result, err := database.GetUsersOfService(hub.Db, toStr(args[0]))
		if err != nil {
			hub.WriteError(err.Error())
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
			if v.T == pf.BLING {
				hub.WritePretty(BULLET_SPACING + hub.services[hub.currentServiceName()].ToLiteral(v))
			} else {
				hub.WritePretty(BULLET + hub.services[hub.currentServiceName()].ToLiteral(v))
			}
		}
		hub.WriteString("\n")
		return false
	case "where":
		num := args[0].V.(int)
		if num < 0 {
			hub.WriteError("the `where` keyword can't take a negative number as a parameter.")
			return false
		}
		if num >= len(hub.ers) {
			hub.WriteError("there aren't that many errors.")
			return false
		}
		println()
		line := hub.Sources[hub.ers[num].Token.Source][hub.ers[num].Token.Line-1] + "\n"
		startUnderline := hub.ers[num].Token.ChStart
		lenUnderline := hub.ers[num].Token.ChEnd - startUnderline
		if lenUnderline == 0 {
			lenUnderline = 1
		}
		endUnderline := startUnderline + lenUnderline
		hub.WriteString(line[0:startUnderline])
		hub.WriteString(Red(line[startUnderline:endUnderline]))
		hub.WriteString(line[endUnderline:])
		hub.WriteString(strings.Repeat(" ", startUnderline))
		hub.WriteString(Red(strings.Repeat("▔", lenUnderline)))
		return false
	case "why":
		num := args[0].V.(int)
		if num >= len(hub.ers) {
			hub.WriteError("there aren't that many errors.")
			return false
		}
		exp, _ := pf.ExplainError(hub.ers, num)
		hub.WritePretty("\n<R>Error</>" + hub.ers[num].Message +
			".\n\n" + exp + "\n")
		refLine := "Error has reference `" + hub.ers[num].ErrorId + "`."
		refLine = "\n" + strings.Repeat(" ", MARGIN-len(refLine)-2) + refLine
		hub.WritePretty(refLine)
		hub.WriteString("\n")
		return false
	case "wipe-store":
		hub.storekey = ""
		hub.store = values.Map{}
		hub.SaveHubStore()
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
	name := FlattenedFilename(scriptFilepath) + "_"

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
	hub.WriteString(GREEN_OK + "\n" + Logo() + "Thank you for using Pipefish. Have a nice day!\n\n")
}

func (hub *Hub) help() {
	hub.WriteString("\n")
	hub.WriteString("Help topics are:\n")
	hub.WriteString("\n")
	for _, v := range helpTopics {
		hub.WriteString("  " + BULLET + v + "\n")
	}
	hub.WriteString("\n")
}

func (hub *Hub) WritePretty(s string) {
	// This shouldn't be happening here.
	hubService, ok := hub.services["hub"]
	if !ok {
		panic("Hub failed to initialize, error is `" + s + "`.")
	}
	mdFunc := hubService.GetMarkdowner("", hub.getSV("width").V.(int), hub.getFonts())
	hub.WriteString(mdFunc(s))
}

func (hub *Hub) isAdministered() bool {
	_, err := os.Stat(hub.pipefishHomeDirectory + "user/admin.dat")
	return !errors.Is(err, os.ErrNotExist)
}

func (hub *Hub) WriteError(s string) {
	hub.WriteString("\n")
	hub.WritePretty(HUB_ERROR + s)
	hub.WriteString("\n\n")
}

func (hub *Hub) WriteString(s string) {
	io.WriteString(hub.out, s)
}

var helpStrings = map[string]string{}

var helpTopics = []string{}

// This is replicated in the settings file and any changes made here must be reflected there.
var StandardLibraries = map[string]struct{}{} // TODO, start using the official Go sets.

func init() {
	for _, v := range []string{"filepath", "fmt", "math", "path", "reflect", "regexp", "sql", "strings", "time", "unicode"} {
		StandardLibraries[v] = struct{}{}
	}
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
	helpStringForHelp := "\nYou can get help on a subject by typing `hub help \"<topic name>\"` into the REPL.\n\n" +
		"Help topics are: \n\n"
	for _, v := range helpTopics {
		helpStringForHelp = helpStringForHelp + BULLET + "\"" + v + "\"\n"
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
	if !hub.services[hub.currentServiceName()].IsBroken() {
		val, _ := hub.services[hub.currentServiceName()].CallMain()
		hub.lastRun = []string{hub.currentServiceName()}
		switch val.T {
		case pf.ERROR:
			hub.WritePretty("\n[0] " + valToString(hub.services[hub.currentServiceName()], val))
			hub.WriteString("\n")
			hub.ers = []*pf.Error{val.V.(*pf.Error)}
		case pf.UNDEFINED_TYPE: // Which is what we get back if there is no `main` command.
		default:
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
		hub.WriteError(err.Error())
		return false
	}
	return needsUpdate
}

func (hub *Hub) createService(name, scriptFilepath string) bool {
	needsRebuild := hub.serviceNeedsUpdate(name)
	if !needsRebuild {
		return false
	}
	newService := pf.NewService()
	newService.SetLocalExternalServices(hub.services)
	e := newService.InitializeFromFilepathWithStore(scriptFilepath, &hub.store) // We get an error only if it completely fails to open the file, otherwise there'll be errors in the Common Parser Bindle as usual.
	hub.services[name] = newService
	hub.Sources, _ = newService.GetSources()
	if newService.IsBroken() {
		if name == "hub" {
			fmt.Println("Pipefish: unable to compile hub: " + text.Red(newService.GetErrors()[0].ErrorId) + ".")
		}
		if !newService.IsInitialized() {
			hub.WriteError("unable to open <C>\"" + scriptFilepath + "\"</> with error `" + e.Error() + "`")
		} else {
			hub.GetAndReportErrors(newService)
		}
		if name == "hub" {
			os.Exit(2)
		}
		return false
	}
	return true
}

func StartServiceFromCli() {
	filename := os.Args[2]
	newService := pf.NewService()
	// This ought to get the `$_hub` settings.
	// Then we could do proper markdown in the errors.
	newService.InitializeFromFilepathWithStore(filename, &values.Map{})
	if newService.IsBroken() {
		fmt.Println("\nThere were errors running the script <C>\"" +filename+ "\"</>.")
		s, _ := newService.GetErrorReport()
		fmt.Println(s)
		fmt.Print("Closing Pipefish.\n\n")
		os.Exit(3)
	}
	val, _ := newService.CallMain()
	if val.T == pf.UNDEFINED_TYPE {
		s := "\nScript <C>\""+ filename + "\"</> has no `main` command.\n\n"
		fmt.Println(s)
		fmt.Print("\n\nClosing Pipefish.\n\n")
		os.Exit(4)
	}
	fmt.Println(newService.ToString(val) + "\n")
	os.Exit(0)
}

func (hub *Hub) GetAndReportErrors(sv *pf.Service) {
	hub.ers = sv.GetErrors()
	r, _ := sv.GetErrorReport()
	hub.WritePretty(r)
}

func (hub *Hub) CurrentServiceIsBroken() bool {
	return hub.services[hub.currentServiceName()].IsBroken()
}

var prefix = `import

NULL::"sql"

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
		buf.WriteString("`")
		buf.WriteString(v)
		buf.WriteString("`::`")
		name, _ := hub.services[v].GetFilepath()
		buf.WriteString(name)
		buf.WriteString("`")
		if i < len(serviceList)-1 {
			buf.WriteString(",\n               .. ")
		}
	}
	buf.WriteString(")\n\n")
	buf.WriteString("currentService string? = ")
	csV := hub.getSV("currentService")
	if csV.T == values.NULL {
		buf.WriteString("NULL")
	} else {
		cs := csV.V.(string)
		if len(cs) == 0 || cs[0] == '#' {
			buf.WriteString("NULL")
		} else {
			buf.WriteString("`")
			buf.WriteString(cs)
			buf.WriteString("`")
		}
	}
	buf.WriteString("\n\n")
	buf.WriteString("isLive = ")
	buf.WriteString(hubService.ToLiteral(hub.getSV("isLive")))
	buf.WriteString("\n\n")
	buf.WriteString("theme Theme? = ")
	buf.WriteString(hubService.ToLiteral(hub.getSV("theme")))
	buf.WriteString("\n\n")
	buf.WriteString("width = ")
	buf.WriteString(hubService.ToLiteral(hub.getSV("width")))
	buf.WriteString("\n\n")
	buf.WriteString("database Database? = ")
	dbVal := hub.getSV("database")
	if dbVal.T == pf.NULL {
		buf.WriteString("NULL\n")
	} else {
		args := dbVal.V.([]pf.Value)
		buf.WriteString("Database with (driver::")
		buf.WriteString(hubService.ToLiteral(args[0]))
		buf.WriteString(",\n")
		buf.WriteString("                                 .. name::")
		buf.WriteString(hubService.ToLiteral(args[1]))
		buf.WriteString(",\n")
		buf.WriteString("                                 .. host::")
		buf.WriteString(hubService.ToLiteral(args[2]))
		buf.WriteString(",\n")
		buf.WriteString("                                 .. port::")
		buf.WriteString(hubService.ToLiteral(args[3]))
		buf.WriteString(",\n")
		buf.WriteString("                                 .. username::")
		buf.WriteString(hubService.ToLiteral(args[4]))
		buf.WriteString(",\n")
		buf.WriteString("                                 .. password::")
		buf.WriteString(hubService.ToLiteral(args[5]))
		buf.WriteString(")\n")
	}

	fname := hub.MakeFilepath(hub.hubFilepath)

	f, err := os.Create(fname)
	if err != nil {
		return HUB_ERROR + "os reports \"" + strings.TrimSpace(err.Error()) + "\".\n"
	}
	defer f.Close()
	f.WriteString(buf.String())
	return GREEN_OK

}

func (hub *Hub) OpenHubFile(hubFilepath string) {
	hub.createService("", "")
	hub.createService("hub", hubFilepath)
	storePath := hubFilepath[0:len(hubFilepath)-len(filepath.Ext(hubFilepath))] + ".str"
	_, err := os.Stat(storePath)
	if err == nil {
		file, err := os.Open(storePath)
		if err != nil {
			panic("Can't open hub data store")
		}
		b, err := io.ReadAll(file)
		if err != nil {
			panic("Can't open hub data store")
		}
		s := string(b)
		for ; s[0:9] != "PLAINTEXT"; println("Invalid storekey. Enter a valid one or press return to continue without loading the store.") {
			salt := s[0:32]
			ciphertext := s[32:]
			rline := readline.NewInstance()
			rline.SetPrompt("Enter the storekey for the hub: ")
			rline.PasswordMask = '▪'
			storekey, _ := rline.Readline()
			if storekey == "" {
				println("Starting hub without opening store.")
				s = "PLAINTEXT"
				break
			}
			key := pbkdf2.Key([]byte(storekey), []byte(salt), 65536, 32, sha256.New) // sha256 has nothing to do with it but the API is stupid.
			block, err := aes.NewCipher(key)
			if err != nil {
				panic(err)
			}
			iv := ciphertext[:aes.BlockSize]
			ciphertext = ciphertext[aes.BlockSize:]
			mode := cipher.NewCBCDecrypter(block, []byte(iv))
			decrypt := make([]byte, len(ciphertext))
			mode.CryptBlocks(decrypt, []byte(ciphertext))
			if string(decrypt[0:9]) == "PLAINTEXT" {
				s = string(decrypt)
				hub.storekey = storekey
				break
			}
		}
		bits := strings.Split(strings.TrimSpace(s), "\n")[1:]
		for _, bit := range bits {
			pair, _ := hub.services["hub"].Do(bit)
			hub.store = *hub.store.Set(pair.V.([]pf.Value)[0], pair.V.([]pf.Value)[1])
		}
	}
	hubService := hub.services["hub"]
	hub.hubFilepath = hub.MakeFilepath(hubFilepath)
	v, _ := hubService.GetVariable("allServices")
	services := v.V.(pf.Map).AsSlice()

	var driver, name, host, username, password string
	var port int

	if hub.hasDatabase() {
		driver, name, host, port, username, password = hub.getDB()
		hub.Db, _ = database.GetdB(driver, host, name, port, username, password)
	}

	for _, pair := range services {
		serviceName := pair.Key.V.(string)
		serviceFilepath := pair.Val.V.(string)
		hub.createService(serviceName, serviceFilepath)
	}
	hub.list()
}

func (hub *Hub) SaveHubStore() {
	storePath := hub.hubFilepath[0:len(hub.hubFilepath)-len(filepath.Ext(hub.hubFilepath))] + ".str"
	storeDump := hub.services["hub"].WriteSecret(hub.store, hub.storekey)
	file, _ := os.Create(storePath)
	file.WriteString(storeDump)
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
		fpath, _ := hub.services[k].GetFilepath()
		if hub.services[k].IsBroken() {
			hub.WriteString(BROKEN)
			hub.WritePretty("Service <C>\"" + k + "\"</> running script <C>\"" + filepath.Base(fpath) + "\"</>.")
		} else {
			hub.WriteString(GOOD_BULLET)
			hub.WritePretty("Service <C>\"" + k + "\"</> running script <C>\"" + filepath.Base(fpath) + "\"</>.")
		}
		hub.WriteString("\n")
	}
	hub.WriteString("\n")
}

func (hub *Hub) TestScript(scriptFilepath string, testOutputType TestOutputType) {

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

func (hub *Hub) RunTest(scriptFilepath, testFilepath string, testOutputType TestOutputType) {

	f, err := os.Open(testFilepath)
	if err != nil {
		hub.WriteError(strings.TrimSpace(err.Error()) + "\n")
		return
	}

	scanner := bufio.NewScanner(f)
	scanner.Scan()
	testType := strings.Split(scanner.Text(), ": ")[1]
	if testType == RECORD {
		f.Close() // TODO --- shouldn't this do something?
		return
	}
	scanner.Scan()
	if !hub.StartAndMakeCurrent("", "#test", scriptFilepath) {
		hub.WriteError("Can't initialize script <C>\"" + scriptFilepath + "\"</>")
		return
	}
	testService := hub.services["#test"]
	in, out := MakeTestIoHandler(testService, hub.out, scanner, testOutputType)
	testService.SetInHandler(in)
	testService.SetOutHandler(out)
	if testOutputType == ERROR_CHECK {
		hub.WritePretty("Running test <C>\"" + testFilepath + "\"</>.\n")
	}
	ty, _ := testService.TypeNameToType("$_OutputAs")
	testService.SetVariable("$_outputAs", ty, 0)
	_ = scanner.Scan() // eats the newline
	executionMatchesTest := true
	for scanner.Scan() {
		lineIn := scanner.Text()[3:]
		if testOutputType == SHOW_ALL {
			hub.WriteString("-> " + lineIn + "\n")
		}
		result := ServiceDo(testService, lineIn)
		if errorsExist, _ := testService.ErrorsExist(); errorsExist {
			report, _ := testService.GetErrorReport()
			hub.WritePretty(report)
			f.Close()
			continue
		}
		scanner.Scan()
		lineOut := scanner.Text()
		if valToString(testService, result) != lineOut {
			executionMatchesTest = false
			if testOutputType == SHOW_DIFF {
				hub.WriteString("-> " + lineIn + "\n" + WAS + lineOut + "\n" + GOT + valToString(testService, result) + "\n")
			}
			if testOutputType == SHOW_ALL {
				hub.WriteString(WAS + lineOut + "\n" + GOT + valToString(testService, result) + "\n")
			}
		} else {
			if testOutputType == SHOW_ALL {
				hub.WriteString(lineOut + "\n")
			}
		}
	}
	if testOutputType == ERROR_CHECK {
		if executionMatchesTest && testType == BAD {
			hub.WriteError("bad behavior reproduced by test" + "\n")
			f.Close()
			hub.RunTest(scriptFilepath, testFilepath, SHOW_ALL)
			return
		}
		if !executionMatchesTest && testType == GOOD {
			hub.WriteError("good behavior not reproduced by test" + "\n")
			f.Close()
			hub.RunTest(scriptFilepath, testFilepath, SHOW_ALL)
			return
		}
		hub.WriteString(TEST_PASSED)
	}
	f.Close()
}

func (hub *Hub) playTest(testFilepath string, diffOn bool) {
	f, err := os.Open(testFilepath)
	if err != nil {
		hub.WriteError(strings.TrimSpace(err.Error()) + "\n")
		return
	}
	scanner := bufio.NewScanner(f)
	scanner.Scan()
	_ = scanner.Text() // test type doesn't matter
	scanner.Scan()
	scriptFilepath := (scanner.Text())[8:]
	scanner.Scan()
	hub.StartAndMakeCurrent("", "#test", scriptFilepath)
	testService := (*hub).services["#test"]
	ty, _ := testService.TypeNameToType("$_OutputAs")
	testService.SetVariable("$_outputAs", ty, 0)
	in, out := MakeTestIoHandler(testService, hub.out, scanner, SHOW_ALL)
	testService.SetInHandler(in)
	testService.SetOutHandler(out)
	_ = scanner.Scan() // eats the newline
	for scanner.Scan() {
		lineIn := scanner.Text()[3:]
		scanner.Scan()
		lineOut := scanner.Text()
		result := ServiceDo(testService, lineIn)
		if errorsExist, _ := testService.ErrorsExist(); errorsExist {
			report, _ := testService.GetErrorReport()
			hub.WritePretty(report)
			f.Close()
			return
		}
		hub.WriteString("#test → " + lineIn + "\n")

		if valToString(testService, result) == lineOut || !diffOn {
			hub.WriteString(valToString(testService, result) + "\n")
		} else {
			hub.WriteString(WAS + lineOut + "\n" + GOT + valToString(testService, result) + "\n")
		}
	}
}

func valToString(srv *pf.Service, val pf.Value) string {
	// TODO --- the exact behavior of this function should depend on service variables but I haven't put them in the VM yet.
	// Alternately we can leave it as it is and have the vm's Describe method take care of it.
	return srv.ToLiteral(val)
}

func (h *Hub) StartHttp(args []string, isHttps bool) {
	// TODO --- everything that depends on this should depend on something else.
	h.listeningToHttpOrHttps = true
	var err error
	http.HandleFunc("/", h.handleJsonRequest)
	if isHttps {
		handler := http.NewServeMux()
		err = certmagic.HTTPS(args, handler)
	} else {
		err = http.ListenAndServe(":"+args[0], nil)
	}
	if errors.Is(err, http.ErrServerClosed) {
		h.WriteError("server closed.")
	} else { // err is always non-nil.
		h.WriteError("error starting server: " + err.Error())
		return
	}
}

// The hub expects an HTTP request to consist of JSON containing the line to be executed,
// the service to execute it, and the username and password of the user.
type jsonRequest = struct {
	Body     string
	Service  string
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
	if h.administered && !((!h.listeningToHttpOrHttps) && (request.Body == "hub register" || request.Body == "hub log in")) {
		_, err = database.ValidateUser(h.Db, request.Username, request.Password)
		if err != nil {
			h.WriteError(err.Error())
			return
		}
	}
	var buf bytes.Buffer
	h.out = &buf
	sv := h.services[request.Service]
	sv.SetOutHandler(sv.MakeLiteralOutHandler(&buf))
	serviceName, _ = h.Do(request.Body, request.Username, request.Password, request.Service, true)
    h.out = os.Stdout
	response := jsonResponse{Body: buf.String(), Service: serviceName}
	json.NewEncoder(w).Encode(response)
}

// So, the Form type. Yes, I basically am reinventing the object here because the fields of
// a struct aren't first-class objects in Go, unlike other superior langages I could name.
// I can get rid of the whole thing when I do SQL integration and can just make the hub into
// a regular Pipefish service. TODO --- you can do this now!
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
	_, err := os.Stat(h.pipefishHomeDirectory + "user/admin.dat")
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
	h.WriteString("You are logged in as " + h.Username + ".\n")
}

func (h *Hub) configAdmin() {
	h.CurrentForm = &Form{Fields: []string{"Username", "First name", "Last name", "Email", "*Password", "*Confirm password"},
		Call:   func(f *Form) { h.handleConfigAdminForm(f) },
		Result: make(map[string]string)}
}

func (h *Hub) handleConfigAdminForm(f *Form) {
	h.CurrentForm = nil
	if h.Db == nil {
		h.WriteError("database has not been configured: edit the hub file or do `hub config db` first.")
		return
	}
	if f.Result["*Password"] != f.Result["*Confirm password"] {
		h.WriteError("passwords don't match.")
		return
	}
	err := database.AddAdmin(h.Db, f.Result["Username"], f.Result["First name"],
		f.Result["Last name"], f.Result["Email"], f.Result["*Password"], h.currentServiceName(), h.pipefishHomeDirectory)
	if err != nil {
		h.WriteError("H/ " + err.Error())
		return
	}
	h.WriteString(GREEN_OK + "\n")
	h.Username = f.Result["Username"]
	h.Password = f.Result["*Password"]
	h.WritePretty("You are logged in as " + h.Username + ".\n")

	h.administered = true
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
	h.WriteString(GREEN_OK + "\n")
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
	h.WriteString(GREEN_OK + "\n")
}

func ServiceDo(serviceToUse *pf.Service, line string) pf.Value {
	v, _ := serviceToUse.Do(line)
	return v
}

var (
	MARGIN         = 92
	GREEN_OK       = ("\033[32mOK\033[0m")
	WAS            = Green("was") + ": "
	GOT            = Red("got") + ": "
	TEST_PASSED    = Green("Test passed!") + "\n"
	VERSION        = "0.6.8"
	BULLET         = "  ▪ "
	BULLET_SPACING = "    " // I.e. whitespace the same width as BULLET.
	GOOD_BULLET    = Green("  ▪ ")
	BROKEN         = Red("  ✖ ")
	PROMPT         = "→ "
	INDENT_PROMPT  = "  "
	ERROR          = text.ERROR
	RT_ERROR       = text.ERROR
	HUB_ERROR      = "<R>Hub error</>: "
)

const HELP = "\nUsage: pipefish [-v | --version] [-h | --help]\n" +
	"                <command> [args]\n\n" +
	"Commands are:\n\n" +
	"  tui           Starts the Pipfish TUI (text user interface).\n" +
	"  run <file>    Runs a Pipefish script if it has a `main` command.\n\n"

func Red(s string) string {
	return "\033[31m" + s + "\033[0m"
}

func Green(s string) string {
	return "\033[32m" + s + "\033[0m"
}

func Cyan(s string) string {
	return "\033[36m" + s + "\033[0m"
}

func Logo() string {
	titleText := " 🧿 Pipefish version " + VERSION + " "
	leftMargin := "  "
	bar := strings.Repeat("═", len(titleText)-2)
	logoString := "\n" +
		leftMargin + "╔" + bar + "╗\n" +
		leftMargin + "║" + titleText + "║\n" +
		leftMargin + "╚" + bar + "╝\n\n"
	return logoString
}

func FlattenedFilename(s string) string {
	base := filepath.Base(s)
	withoutSuffix := strings.TrimSuffix(base, filepath.Ext(base))
	flattened := strings.Replace(withoutSuffix, ".", "_", -1)
	return flattened
}

func (h *Hub) MakeFilepath(scriptFilepath string) string {
	doctoredFilepath := strings.Clone(scriptFilepath)
	if len(scriptFilepath) >= 4 && scriptFilepath[0:4] == "hub/" {
		doctoredFilepath = filepath.Join(h.pipefishHomeDirectory, filepath.FromSlash(scriptFilepath))
	}
	if len(scriptFilepath) >= 7 && scriptFilepath[0:7] == "rsc-pf/" {
		doctoredFilepath = filepath.Join(h.pipefishHomeDirectory, "source", "initializer", filepath.FromSlash(scriptFilepath))
	}
	if _, ok := StandardLibraries[scriptFilepath]; ok {
		doctoredFilepath = h.pipefishHomeDirectory + "lib/" + scriptFilepath
	}
	if len(scriptFilepath) >= 3 && scriptFilepath[len(scriptFilepath)-3:] != ".pf" && len(scriptFilepath) >= 4 && scriptFilepath[len(scriptFilepath)-4:] != ".hub" {
		doctoredFilepath = doctoredFilepath + ".pf"
	}
	return doctoredFilepath
}
