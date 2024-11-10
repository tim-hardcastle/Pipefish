# Style guide for Pipefish

Despite Pipefish’s affinity with Go, the style is a little more like Java, for [reasons](https://github.com/tim-hardcastle/Pipefish/blob/main/docs/the-whys-of-pipefish.md#ok-then-why-isnt-it-more-like-go).

Names of functions, variables, *local* constants, and the fields of structs should be in camelCase. Exceptions are any-letter variables as parameters of inner functions, which can usefully be capitalized to suggest a container type, e.g. `L` for a list, `M` for a map.

Except when naming the parameters of inner functions (where `i` for an integer, `L` for a list, `f`	 for a function, etc, is acceptable) names should be verbose.

Names of user-defined types (i.e. of structs and enums) should be in PascalCase.

*Global* constants and the *members* of enums should be in SCREAMING_SNAKE_CASE.

The continuation sign `..` exists for a reason. Without my prescribing a hard upper limit on line length, a good coder will keep lines short, and use the whitespace-immunity of continuations to align like with like.

Infix operators should be separated by spaces from their operands on either side, with the exceptions:
* `,` and `;` should be followed but not preceeded a space.
* The `::` operator should be neither preceded nor followed by a space.

The perhaps excessive flexibility of Pipefish’s syntax is there to clarify and not to befuddle. The fancier forms of functions can certainly be used (a) to provide a DSL for the end-user of a service (b) to emulate mathematical notation where this is suitable. Otherwise, this feature should be used with caution. Further guidance will be provided when I have decided what it should be.
