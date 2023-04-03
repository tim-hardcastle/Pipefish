# Using the Charm hub

## 0. Introduction

The Charm hub allows you to start scripts running and stop them, debug them, write tests for them, and to turn the whole hub into a server for the services you create.

The hub is itself a Charm service, which apart from other benefits means that you don't have to learn two sets of syntax.

This file documents the uses of the Charm hub. It explains first of all the basic commands that allow you to start a script running, to stop it, to ask for help, and to quit Charm.

We then mention the use of the `os` prefix in the command line.

We then move on to the commands you can use to help develop your code: commands for tracing errors, asking for help with them, making tests, etc.

If you're just using Charm for your own purposes, that's all you need to know, but if you want to make your hub public and administer access to it, then you'll need to know the commands for that, plus the set of commands any end-user can then use to register with the hub and log on and log off.

**NOTE** that besides letting you talk to Charm services and to the hub, Charm's CLI also lets you talk directly to the OS (for the purposes of for example navigating through the file system) by prefixing whatever you want to feed it with `os`: so you can write `os cd ..` etc. This does *not* share any part of its syntax and semantics with Charm.

## 1. Basic commands

#### `hub run(filename string) as (srv service)`

This starts up the given file as a service with the given name. E.g:

```
→ hub run "examples/foo.ch" as "foo"
foo → 
```

The service name `foo` now appears in the prompt to remind you that it's now the "current service", i.e. the service that your input will be passed to by default.

When you start using Charm, hotcoding is turned on. That is, when you change the script and then interact with the REPL again, Charm will reinitialize the script without you telling it to. So you don't need to keep rerunning your code.

#### `hub run(filename string)`

If you omit the name of the service, and just do `hub run(filename string)`, then the hub will supply a name for the from the series `#0`, `#1`, `#2` ...

```
→ hub run "examples/bar.ch"
#0 → 
```

#### `hub services`

Starting up one service doesn't stop the other services from running. `hub services` provides you with a list of all the ones that are.

#### `hub switch(srv string)`

This changes the current service to the service given.

#### `hub halt(srv string)`

This halts the given service. (If this is the service you're using, you will find yourself booted off it and instead using the "empty service", i.e. the service you'd get if you compiled a completely empty script.)

#### `hub quit`

This shuts down Charm, but remembers every service you had running at shutdown which you gave a specific name to (by using `hub run ... as`) and starts them up again when you restart Charm.

#### `hub help(topic string)`

This supplies help on the given topic.

#### `hub help`

This gives a list of the help topics.

## 2. Development

This category includes ways to investigate errors, the system for making tests, and a number of miscellaneous features.

### 2.1 Development: Investigating errors

#### `hub why(errNo int)`

The initial report of an error is concise. `hub why` provides a more detailed explanation of what has gone wrong, and may suggest a help topic, or how it relates to other errors that are reported at the same time.

#### `hub where(errNo int)`

This shows the line where the error occurred with the exact place where the error occurred underlined in red, occasionally useful when the line is a long one.

#### `hub trace`

This gives the course a runtime error took as it passed up the stack. Unlike `why` and `where` it doesn't require an error number, because there can only ever be one runtime error thrown at a time, unlike syntax errors.

#### `hub values`

When a runtime error is the result of a type mismatch, the initial error report will give the expected and actual types, but won't give the actual value or values passed because this might be a firehose of informations.

When such values are available, the initial error report will contain a note to that effect, and `hub values` will show the values involved. As with `hub trace`, there is no need to specify an error number because only one runtime error can happen at a time.

#### `hub errors`

Repeats the list of the last set of errors thrown, to save you from having to scroll up looking for it.

### 2.2 Development: Making tests

#### `hub snap (scriptFilename string) as (testFilename string)`

This creates a test to let you record your interactions with the REPL: what you type and the service’s responses will be recorded. (There will be a `#snap →` prompt to remind you that this is what you’re doing.) 

While the test is underway, Charm will turn serialization on, so that you can tell the difference between "true" and true; and between four spaces and a tab, etc.

#### `hub snap (scriptFilename string)`

This supplies you with an unused default name for the test, if you don't supply one.

To finish recording, you tell Charm what to do with the snap, using one of the four following commands:

#### `hub snap good`

This means: Stop the test. This is the desired behavior and I want to make it into a test that will ensure it keeps happening.

#### `hub snap bad`

This means: Stop the test. his is undesirable behavior and I want to make a test that checks that it doesn’t happen again.

#### `hub snap record`

This means: Stop the test. I want to be able to replay this and see how the output changes as I change the script.

#### `hub snap discard`

This means: Stop the test. I don’t need this test after all, throw it away.

#### `hub test(scriptFilename string)`

This runs all the tests ended with `snap good` or `snap bad` and checks that they get the right results.

#### `hub replay(testFilename string)`

This replays the inputs from a test ended with `snap record`, and shows the new outputs.

#### `hub replay diff(testFilename string)`

This does the same as `hub replay` but shows the difference between the results obtained and those produced when the test was made.

### 2.3: Development: Miscelaneous

#### `hub hot on`

Turns hotcoding on.

#### `hub hot off`

Turns hotcoding off.

#### `hub peek on`

Shows the intermediate representations of code typed into the REPL. More use to me than you

#### `hub peek off`

Turns off the thing that `hub peek on` turns on.

#### `hub edit (filename string)`

Opens the file in vim.

#### `hub do (line string)`

This tells the current service to execute the current line. It used to be useful for something, but is no longer, and will be deprecated unless I can think why I should keep it.

