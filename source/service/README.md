# README for the "service" package.

## Overview

This package consists of two systems which need to be able to see one another. It is therefore something of a tangled web.

It can be divided into:

(A) Compilation, as performed by the `compiler`, `builtin`, `environment`, and `typeschemes` files.

(C) The vm, as implemented by the `vm`, `descriptors`, `iohandler`, `operations`, `snap`, `SQL`, and `vmgo` files.

In order to make use of external services the VM needs to be able to see the compiler, but of course also the compiler needs to be able to see the VM so as to compile onto it.

## Notes on the vm.

Mainly, the vm needs to do two things

(1) Go round in a big loop running the bytecode.
(2) Know how to describe things as text --- values, types, and its own workings.

The opcodes of the machine are defined in the `operations` file, which also contains information about the number and semantic significance of the operands, and so constitutes an informal specification.

The main loop is in the `vm` file.

The `descriptors` file contains functions for describing Pipefish values and types.

The `SQL` file contains utilities for the vm to talk to SQL, as you would guess.

The `iohandler` file lets you create iohandlers to be passed to the vm to modify where it gets its input and sends its output.

The `snap` file contains functions to help the hub make and run tests.

The opcodes of the vm, as found in the `operations` file, are based on English. They often have one or more suffix characters indicating the type to which they relate. Container types are capitalized.

Conditionals begin with `Q`, their last operand being a code location. The semantics of the conditional is "if the condition is met, continue to the next operation, otherwise jump to the given location". `Qn` means "if not".

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
