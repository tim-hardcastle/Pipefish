def

HubResponse = struct(responseName string, vals list)

// Vebrs are in alphabetical order:
// add, config, create, do, edit, errors, halt, help, hot let, listen, log, my, peek, quit, register, replay, run, services, snap,
// test, trace, where, why, values

add(usr string) to (group string) :
    HubResponse("add", [usr, group])

config admin :
    HubResponse("config-admin")

config db :
    HubResponse("config-db")

create(groupname string) :
    HubResponse("create" [groupname])

do(line string) :
    HubResponse("do" [line])

edit(filename string) :
    HubResponse("edit", [filename])

errors :
    HubResponse("errors", [])

groups :
    HubResponse("my-groups", [])

groups of service(srv string) :
    HubResponse("groups-of-service", [srv])

groups of user(usr string) :
    HubResponse("groups-of-user", [usr])

halt(srv string) :
    HubResponse("halt", [srv])

help(topic string) :
    HubResponse("help", [topic])

hot on :
    HubResponse("hot-on", [])

hot off :
    HubResponse("hot-off", [])

let(usr string) use (srv string) :
    HubResponse("let", [usr, srv])

listen(path string, port int) :
    HubResponse("listen", [path, string port])

log on :
    HubResponse("log-on", [])

log off :
    HubResponse("log-off", [])

peek on :
    HubResponse("peek-on", [string option])

peek off :
    HubResponse("peek-off", [string option])

quit :
    HubResponse("quit", [])

register :
    HubResponse("register", [])

replay (filename string) :
    HubResponse("replay", [filename])

replay diff (filename string) :
    HubResponse("replay-with-diff", [filename])

reset :
    HubResponse("reset", [])

rerun :
    HubResponse("rerun", [])

run(filename string) :
    HubResponse("run", [filename, ""])

run(filename string) as (srv string) :
    HubResponse("run", [filename, srv])

services of group(grp string) :
    HubResponse("services-of-group", [grp])

services :
    HubResponse("my-services", [])

services of user(usr string) :
    HubResponse("services-of-user", [usr])

snap good :
    HubResponse("snap-good")

snap bad :
    HubResponse("snap-bad")
    
snap record :
    HubResponse("snap-record")

snap discard :
    HubResponse("snap-discard")    

snap(filename string) :
    HubResponse("snap", [filename, ""])

snap(filename string) as (testName string) :
    HubResponse("snap", [filename, testName])

switch(srv label) :
    HubResponse("switch", [string srv])

switch(srv string) :
    HubResponse("switch", [srv])

test(filename string) :
    HubResponse("test", [filename])

trace :
    HubResponse("trace", [])

unadd(usr string) to (grp string) :
    HubResponse("unadd", [usr, grp])

uncreate(grp string) :
    HubResponse("uncreate" [grp])

unlet(usr string) use (srv string) :
    HubResponse("unlet", [usr, srv])

unregister :
    HubResponse("unregister", [])

users of service(srv string) :
    HubResponse("users-of-service", [srv])

users of group(grp string) :
    HubResponse("users-of-group", [grp])

where(errorNo int) :
    HubResponse("where", [string errorNo])
    
why(errorNo int) :
    HubResponse("why", [string errorNo])
    
values :
    HubResponse("values", [])
    


