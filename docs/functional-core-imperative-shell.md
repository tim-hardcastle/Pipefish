# Functional core, imperative shell

Functional Core / Imperative Shell (FC/IS) is based on the observation that impurity is a [function color](https://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/): if a function can produce side-effects or is susceptible to side-causes, then this is true of the calling functions all the way up the stack.

Consequently, one way to manage impurity is simply to push it as far up the calling stack as it will go, so that effects are bound very tightly to incoming requests. (As they should be. Clocks and RNGs aside, usually we don't want data to change by itself, but as a result of someone/something saying: "change this data".)

This leaves us with an absolutely tiny impure region of our code consisting of variable reassignments along the lines of `data = pureFunctionOf(data, userInput)` The remainder of our code can then be composed of pure functions expressing our business logic.

Such code is easier to write, easier to read, easier to debug, and easier to test. (On the one hand, the small impure part of the program barely needs testing at all, since it has hardly any flow-of-control: whereas the big pure part of the program is easy to test because it has no state to mock.)

From the above description, you can see that it would be possible for people to write FC/IS programs in many languages, and [in](https://www.youtube.com/watch?v=eOYal8elnZk) [fact](https://www.destroyallsoftware.com/screencasts/catalog/functional-core-imperative-shell) [they](https://doordash.engineering/2022/07/26/functional-core-imperative-shell-using-structured-concurrency-to-write-maintainable-grpc-endpoints-in-kotlin/) [do](https://medium.com/@magnusjt/functional-core-imperative-shell-in-javascript-29bef2353ac2). However, a purpose-built language will (if I've got it right!) be better; besides which, so long as the language permits other paradigms, FC/IS is a matter of programmer discipline, which [does not scale](https://www.sicpers.info/2020/10/discipline-doesnt-scale/).

Charm, however, enforces FC/IS, by making a strict distinction between commands, which have effects and don't return values, and functions, which return values but have no effects. Since functions can't call commands, the commands have no choice but to occupy the top of the call stack.

The result of all this is expressed in the following table:

| Functional core  | Imperative shell |
| ------------- | ------------- |
| The `def` section of a Charm script | The `cmd` section of a Charm script |
| Has functions ... | Has commands ... |
| ... which return values | ... which return nothing |
| ... and can only call functions | ... and can call both functions and commands |
| Pure | All variables are global for all commands |
| Effectless | Exists only to perform side-effects |
| Allows recursion | Forbids recursion |
| Contains all the business logic | Dumb as a sack of rocks |
| 99% of your code | 0.5% of your code (the remaining 0.5% is type definitions) |
| Easy to test because it's pure | Easy to test because it's simple |
| Isolates the demon of flow-of-control ... | ... from the demon of mutable state |
