# Functional core, imperative shell

| Functional core  | Imperative shell |
| ------------- | ------------- |
| The `def` section  | The `cmd` section |
| Has functions | Has commands ... |
| ... which return values | ... which return nothing |
| Pure | All variables are global for all commands |
| Effectless | Exists only to perform side-effects |
| Isolates the demon of flow-of-control ... | ... from the demon of mutable state |
