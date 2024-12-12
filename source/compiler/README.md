# README for the "service" package.

## Overview

This package consists of two systems which need to be able to see one another. In order to make use of external services the VM needs to be able to see the compiler class, but of course also the compiler needs to be able to see the VM so as to compile onto it. Hence the tangled web. It may be possible to remedy this in a future version.

So the package can be divided into two parts, the compiler and the VM.

## The compiler

This is performed by the `api_serialization`, `builtin`, `compiler`, `compfcall`, `environment`, `getters` and `typeschemes` files.

* `api_serialization` serializes the API for the benefit of potential client services. (TODO: this could be done once at compile time and stored in the compiler.)

* `builtin` contains the code for generating builtin functions.

* `compiler` knows everything needed to compile a line of code from the REPL at runtime. It does *not* know how to compile a script by itself, and is directed in this by the `initializer` package.

* `compfcall` breaks out the logic for compiling a function call, which is complicated because of the multiple dispatch.

* `environment` supplies data structures and their getters and setters for keeping track of where in (virtual) memory variables are stored.

* `getters` is a miscellaneous collection of helper functions for extracting data conveniently from wherever its stored and converting it from one form to another. This can be quite convoluted as a result of my attempts to maintain a single source of truth.

* `typeschemes` contains types, and functions for manipulating them, that represents the compiler's view of the type system. This is somewhat richer than that enjoyed by the user or the compiled code, in that it can keep track of the types of the elements of tuples.

## The vm

This is implemented by the `descriptors`, `iohandler`, `operations`, `SQL`, `vm`, and `vmgo` files.

Mainly, the vm needs to do two things

(1) Go round in a big loop running the bytecode.
(2) Know how to describe things as text --- values, types, and its own workings.

* `descriptors` contains functions for describing Pipefish values and types. In order to maintain DRYness and a single source of truth it does this for the rest of the application, by one route or another.

* `iohandler` lets you create iohandlers to modify where the vm gets its input and sends its output. Much of this has been moved to the `pf` package, which aliases the relevant types.

* `operations` defines the opcodes of the machine are defined in the file, which also contains information about the number and semantic significance of the operands, and so constitutes an informal specification. It also contains the code for peeking at the workings of the VM for debugging purposes.

* `SQL` contains utilities for the vm to talk to SQL, as you would guess.

* `vm` contains the main loop: one big `switch` statement on the operands.

* `vmgo` contains functions for converting Pipefish values to Go values and vice versa.

### Note on the operations of the vm.

The opcodes of the vm, as found in the `operations` file, are based on English. They often have one or more suffix characters indicating the type to which they relate. Container types are capitalized.

Conditionals begin with `Q`, their last operand being a code location. The semantics of the conditional is "if the condition is met, continue to the next operation, otherwise jump to the given location". `Qn` means "if not".

The following suffixes may be used to indicate the types of the operands:

b - bool
f - float
i - int
l - label
L - list
M - map
n - a uint32 literal
q - null
s - string
S - set
t - type
T - tuple
x - anything
Z - a struct
