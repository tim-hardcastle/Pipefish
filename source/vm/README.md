## README for the VM

Mainly, the vm needs to do two things

(1) Go round in a big loop running the bytecode.
(2) Know how to describe things as text --- values, types, and its own workings.

* `descriptors` contains functions for describing Pipefish values and types. In order to maintain DRYness and a single source of truth it does this for the rest of the application, by one route or another.

* `gohandler` contains functions for converting Pipefish values to Go values and vice versa.

* `iohandler` lets you create iohandlers to modify where the vm gets its input and sends its output. Much of this has been moved to the `pf` package, which aliases the relevant types.

* `operations` defines the opcodes of the machine are defined in the file, which also contains information about the number and semantic significance of the operands, and so constitutes an informal specification. It also contains the code for peeking at the workings of the VM for debugging purposes.

* `SQL` contains utilities for the vm to talk to SQL, as you would guess.

* `vm` contains the main loop: one big `switch` statement on the operands.

## Note on the operations of the vm.

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
