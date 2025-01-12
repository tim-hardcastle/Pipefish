# Overall architecture of Pipefish

Almost every important package has its own README by now, explaining the purpose of the files in it: in this section we will take a less detailed overview of the whole system.

## `main` and `hub`

The top layer of the application consists of the `main.go` file, which just starts up the REPL, and the `hub` package consisting of `hub.go`, `repl.go`, and `snap.go`. Of these `repl.go` just wraps lightly round the hub for reasons now moot, and will shortly be removed. `snap.go` provides facilities for making automated tests.

`hub.go` contains the code for interacting with the Pipefish TUI, "the hub" and with the services that it controls.

It does this by interacting with the `Service` class defined in the `pf` package.

## The `pf` package

The `pf.Service` class exists to encapsulate services and provide nice clean public methods for dealing with them. This gives a nice way for other people to embed Pipefish services in Go code, since they can then just import it `pf` as a library.

## The `initializer` package

We will give a longer explanation of initialization in the README of the initializer. For now, the important point to grasp is that while there is only one VM per project, there is a separate initializer, compiler, and parser for every module of the project. There is no one uberclass in charge of this, rather the first initializer, if there are imports or external services, will spawn further initializers (each with their own compiler and parser), and so on recursively.

What is finally returned is a `Compiler`, which is then wrapped in a `Service` type by the `pf` package. We will discuss the `Compiler` below. The `initializer` can then be disposed of together with the initialization-time data.

The initializer doesn't operate on the VM directly: rather it directs the compiler's interactions with the VM.
 
## The `compiler` package

The `compiler` package supplies a `Compiler` object having the methods and data necessary to compile an expression supplied at runtime on to the VM and run it.

The `Compiler` object therefore has a `Vm` and a `Parser`.

Since there is one `Compiler` per module, the `Compiler` also maintains a map (by namespace) of dependent `Compilers`.

## The `parser` and `ast` packages

This supplies a `Parser` that parses things for the `Compiler`, having been set up by the `Initializer` to know the names of the types, functions, etc.

The AST that it parses things into is defined in the `ast` package.

Just as the `Compiler` needs to know about the `Compiler`s of its modules and external services, so too does the `Parser` need to know about the corresponding `Parser`s.

## The `lexer` and `token` package

The `Lexer` turns a string into a series of `Token`s. The `Relexer`, in the same package, then tweaks the output to make it more suitable for the parser.

## The `vm` package

This needs to know how to do two things:

* Go round in a big loop performing operations.
* Describe things (values, types, its own workings).

It performs this second function for all the other packages, acting as the SSoT.
