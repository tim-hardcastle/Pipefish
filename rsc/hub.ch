def

HubResponse = struct(responseName string, vals list)

// Vebrs are in alphabetical order:
// add, config, create, do, edit, errors, halt, help, hot let, listen, log, my, peek, quit, register, replay, run, services, snap,
// test, trace, where, why, values

configOptions = enum ADMIN, DB

onOffOptions = enum ON, OFF

myOptions = enum GROUPS, SERVICES

snapOptions = enum GOOD, BAD, RECORD, DISCARD

add(username string) to (group string) :
    HubResponse("add", [username, group])

config(option configOptions) :
    HubResponse("config", [string option])

create(groupname string) :
    HubResponse("create" [groupname])

do(line string) :
    HubResponse("do" [line])

edit(filename string) :
    HubResponse("edit", [filename])

errors :
    HubResponse("errors", [])

groupsOfService(service string) :
    HubResponse("groups-of-service", service)

groupsOfUser(user string) :
    HubResponse("groups-of-user", user)

halt(service string) :
    HubResponse("halt", [service])

help(topic string) :
    HubResponse("help", [topic])

hot(option onOffOptions) :
    HubResponse("hot", [string option])

let(username string) use (service string) :
    HubResponse("let", [username, service])

listen(path string, port int) :
    HubResponse("listen", [path, string port])

log(option onOffOptions) :
    HubResponse("log", [string option])

my(option myOptions) :
    HubResponse("my", [string option])

peek(option onOffOptions) :
    HubResponse("peek", [string option])

quit :
    HubResponse("quit", [])

register :
    HubResponse("register", [])

replay (filename string) :
    HubResponse("replay", [filename])

replay (filename string) diff :
    HubResponse("replay-with-diff", [filename])

reset :
    HubResponse("reset", [])

rerun :
    HubResponse("rerun", [])

run(filename string) :
    HubResponse("run", [filename, ""])

run(filename string) as (service string) :
    HubResponse("run", [filename, service])

services :
    HubResponse("services", [])

servicesOfUser(user string) :
    HubResponse("services-of-user", user)

servicesOfGroup(group string) :
    HubResponse("services-of-group", group)

snap(option snapOptions) :
    HubResponse("snap-option", [string option])

snap(filename string) :
    HubResponse("snap", [filename, ""])

snap(filename string) as (testName string) :
    HubResponse("snap", [filename, testName])

switch(service label) :
    HubResponse("switch", [string service])

switch(service string) :
    HubResponse("switch", [service])

test(filename string) :
    HubResponse("test", [filename])

trace :
    HubResponse("trace", [])

unadd(username string) to (group string) :
    HubResponse("unadd", [username, group])

uncreate(groupname string) :
    HubResponse("uncreate" [groupname])

unlet(username string) use (service string) :
    HubResponse("unlet", [username, service])

unregister :
    HubResponse("unregister", [])

usersOfService(service string) :
    HubResponse("users-of-service", service)

usersOfGroup(group string) :
    HubResponse("users-of-group", group)

where(errorNo int) :
    HubResponse("where", [string errorNo])
    
why(errorNo int) :
    HubResponse("why", [string errorNo])
    
values :
    HubResponse("values", [])
    
    

