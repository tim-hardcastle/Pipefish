## README for the initializer

The `initializer` package supplies an `Initializer` struct which contains the data and supplies the methods necessary to initialize a Pipefish script, by creating and directing a parser and a compiler to operate on a VM. The `Initializer` returns a `Compiler` capable of dealing with runtime compilation of requests from the REPL: the `Initializer` can then be discarded together with its data.

There is one `Initializer` per module, as with compilers and parsers: an `Initializer` can spawn further `Intitializer`s recursively to initialize modules and external services.

The `initializer` package consists of:

* `deserializer.go`, which is used to deserialize the APIs of external services.
* `initializer.go`, the main file directing initialization.
* `getters.go`, which supplies some miscellaneous utility functions for getting and transforming data.
* `gohandler.go` which does housekeeping for the Go interop.
* `gogen.go` which generates Golang source files.

Fields of note in the `Initializer` struct are its compiler (naturally); its parser (a shortcut to the parser of the compiler); `Common`, a bindle of data that all the initializers of all the modules need to share; and `GoBucket`, which is used to accumulate the miscellaneous data swept up during parsing that we need to generate Go source files.