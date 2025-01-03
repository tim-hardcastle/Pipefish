# Overall architecture of Pipefish

## `main` and `hub`

The top layer of the application consists of the `main.go` file, which just starts up the REPL, and the `hub` package consisting of `hub.go`, `repl.go`, and `snap.go`. Of these `repl.go` just wraps lightly round the hub for reasons now moot, and will shortly be removed. `snap.go` provides facilities for making automated tests.

`hub.go` contains the code for interacting with the Pipefish TUI, "the hub" and with the services that it controls.

It does this by interacting with the `Service` class defined in the `pf` package.

## `pf.Service`

The `Service` class exists to encapsulate services and provide nice clean public methods for dealing with them. This gives a nice way for other people to embed Pipefish services in Go code, since they can then just import it `pf` as a library.

## `initializer`

We will give a longer explanation of initialization in the README of the initializer. For now, the important point to grasp is that while there is only one VM per project, there is a separate initializer, compiler, and parser for every module of the project. There is no one uberclass in charge of this, rather the first initializer, if there are imports, will spawn further initializers (each with their own compiler and parser), and so on recursively.

What is finally returned is a `Compiler`, which is then wrapped in a `Service` type by the `pf` package. We will discuss the `Compiler` below. The `initializer` can then be disposed of together with the initialization-time data.

The `initializer` package consists of the `initializer.go` file itself, `getters.go`, which supplies some utility functions for getting and transforming data, `gohandler.go` which does housekeeping for the Go interop, and `gogen.go` which generates Golang source files. Finally `deserializer.go` is used to deserialize the APIs of external services.

Fields of note in the `initializer` struct are its compiler (naturally); its parser (a shortcut to the parser of the compiler); `Common`, a bindle of data that all the initializers of all the modules need to share; and `GoBucket`, which is used to accumulate the miscellaneous data swept up during parsing that we need to generate Go source files.
 
