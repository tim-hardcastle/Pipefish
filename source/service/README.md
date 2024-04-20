Some notes on the VM.

Mainly, the VM needs to do two things

(1) Go round in a big loop running the bytecode.
(2) Know how to describe things as text --- values, types, and its own workings.

The opcodes of the machine are defined in the `operations.go`, which also contains information about the number and semantic significance of the operands, and so constitutes an informal specification.

The opcodes are based on English, They often have one or more suffix characters indicating the type to which they relate. Container types are capitalized.

b - bool
f - float
i - int
l - label
L - list
M - map
n - a number, a uint32 literal
q - null
s - string
S - set
t - type
T - tuple
x - anything
Z - a struct

Conditionals begin with `Q`, their last operand being a code location. The semantics of the conditional is "if the condition is met, continue to the next operation, otherwise jump to the given location". `Qn` means "if not".