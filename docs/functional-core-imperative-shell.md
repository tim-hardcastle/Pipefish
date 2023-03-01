# Functional core, imperative shell

| Functional core  | Imperative shell |
| ------------- | ------------- |
| The `def` section | The `cmd` section |
| Has functions ... | Has commands ... |
| ... which return values | ... which return nothing |
| Can only call functions | Can call both functions and commands |
| Pure | All variables are global for all commands |
| Effectless | Exists only to perform side-effects |
| Contains all the business logic | Dumb as a sack of rocks |
| Easy to test because it's pure | Easy to test because it's simple |
| Isolates the demon of flow-of-control ... | ... from the demon of mutable state |
