# README for the "compiler" package.

* `api_serialization` serializes the API for the benefit of potential client services. (TODO: this could be done once at compile time and stored in the compiler.)

* `builtin` contains the code for generating builtin functions.

* `compiler` knows everything needed to compile a line of code from the REPL at runtime. It does *not* know how to compile a script by itself, and is directed in this by the `initializer` package.

* `compfcall` breaks out the logic for compiling a function call, which is complicated because of the multiple dispatch.

* `environment` supplies data structures and their getters and setters for keeping track of where in (virtual) memory variables are stored.

* `getters` is a miscellaneous collection of helper functions for extracting data conveniently from wherever its stored and converting it from one form to another. This can be quite convoluted as a result of my attempts to maintain a single source of truth.

* `typeschemes` contains types, and functions for manipulating them, that represents the compiler's view of the type system. This is somewhat richer than that enjoyed by the user or the compiled code, in that it can keep track of the types of the elements of tuples.
