# Functional core, imperative shell

| Functional core  | Imperative shell |
| ------------- | ------------- |
| The `def` section of a Charm script | The `cmd` section of a Charm script |
| Has functions ... | Has commands ... |
| ... which return values | ... which return nothing |
| Can only call functions | Can call both functions and commands |
| Pure | All variables are global for all commands |
| Effectless | Exists only to perform side-effects |
| Allows recursion | Forbids recursion |
| Turing-complete without the `cmd` section | Not Turing-complete without the `def` section |
| Contains all the business logic | Dumb as a sack of rocks |
| 99% of your code | 0.5% of your code (the remaining 0.5% is type definitions) |
| Easy to test because it's pure | Easy to test because it's simple |
| Isolates the demon of flow-of-control ... | ... from the demon of mutable state |
