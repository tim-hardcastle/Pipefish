def

HubResponse = struct(responseName string, vals list)

// Verb are in alphabetical order:
// add, config, create, do, edit, errors, halt, help, hot let, listen, log, my, peek, quit, register, replay, run, services, snap,
// test, trace, where, why, values

add(usr string) to (grp string) :
    HubResponse("add", [usr, grp])

config admin :
    HubResponse("config-admin", [])

config db :
    HubResponse("config-db", [])

create(grp string) :
    HubResponse("create" [grp])

edit(filename string) :
    HubResponse("edit", [filename])

errors :
    HubResponse("errors", [])

groups :
    HubResponse("groups", [])

groups of service(srv string) :
    HubResponse("groups-of-service", [srv])

groups of user(usr string) :
    HubResponse("groups-of-user", [usr])

halt(srv string) :
    HubResponse("halt", [srv])

help :
    HubResponse("help", ["topics"])

help(topic string) :
    HubResponse("help", [topic])

hot on :
    HubResponse("hot-on", [])

hot off :
    HubResponse("hot-off", [])

let(usr string) own (grp string) :
    HubResponse("let-own", [usr, grp])

let(grp string) use (srv string) :
    HubResponse("let", [grp, srv])

listen(path string, port int) :
    HubResponse("listen", [path, string port])

log on :
    HubResponse("log-on", [])

log off :
    HubResponse("log-off", [])

peek on :
    HubResponse("peek-on", [])

peek off :
    HubResponse("peek-off", [])

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

services :
    HubResponse("services", [])

services of group(grp string) :
    HubResponse("services-of-group", [grp])

services of user(usr string) :
    HubResponse("services-of-user", [usr])

snap(filename string) :
    HubResponse("snap", [filename, ""])

snap(filename string) as (testName string) :
    HubResponse("snap", [filename, testName])

snap bad :
    HubResponse("snap-bad", [])

snap discard :
    HubResponse("snap-discard", []) 

snap good :
    HubResponse("snap-good", [])
    
snap record :
    HubResponse("snap-record", [])  

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

unlet(grp string) use (srv string) :
    HubResponse("unlet-use", [grp, srv])

unlet(usr string) own (grp string) :
    HubResponse("unlet-own", [usr, grp])

unregister :
    HubResponse("unregister", [])

users of group(grp string) :
    HubResponse("users-of-group", [grp])

users of service(srv string) :
    HubResponse("users-of-service", [srv])

values :
    HubResponse("values", [])

vm(sourcecode string) :
    HubResponse("vm", [sourcecode])

where(errorNo int) :
    HubResponse("where", [string errorNo])
    
why(errorNo int) :
    HubResponse("why", [string errorNo])
    
    


