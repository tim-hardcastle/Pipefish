# Version 0.4: a "working prototype"

I'm calling version 0.4 of Charm a "working prototype". What do I mean by that? In what sense is it a prototype, and in what sense is it working?

It is a *working* prototype in that:

* The core language is pretty much finished and feature-stable.
* Some batteries are included. There are `string`, `math`, `fmt` and `regexp` libraries which do what you would expect, and a `world` library for performing IO.
* There is some nice tooling for a pleasant DX.
* There is a large test suite which I will continue to add to to keep Charm functionally stable.
* I will continue to maintain version 0.4.x and fix bugs while working in a separate branch on the compiled version.

It is a *prototype* because:

* It is slower than it should be. All my efforts so far have been on syntax and semantics, i.e. on *design*, while the actual implementation, the bit that runs the app, has been delegated to a tree-walking interpreter with increasingly complex and cumbersome logic. Replacing this with compilation to a VM (the goal of version 0.5) will increase performance by orders of magnitude.
* Some of the features are still in a somewhat sketchy state. For example, both the Go interop and the SQL interop should work with a wider range of types.
* It probably still has bugs in it. In particular, it probably has a number of bugs where, if you stray off the "happy path", you get a crash or undefined behavior rather than a syntax or runtime error. This is an unfortunate consequence of the fact that so far Charm has been tested mainly by its author, who knows exactly how it's meant to work.
* There aren't enough standard libraries.
