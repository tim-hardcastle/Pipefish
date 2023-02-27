# The whys of Charm

## Introduction

This document is supplementary material provided to explain some of the less common choices made in the design of Charm. Nothing contained in it is neceassary to knowing how to use the language.

## Why does the `pair` type exist?

As a form of syntactic and (so to speak) semantic sugar. It was originally my intention to use a single colon as syntax in slices and in giving key-value-pairs in map and struct literals. Because of the other things I’m doing with colons this would be slightly fiddly and would in very rare cases require the user to use parentheses to disambiguate the colon, but I was still going to do it. What turned me around was the realization that if I used a colon it could never be *more than* syntax: `foo : bar` can never refer to a first-class object. But this is very un-Charm-like. Hence the pair type: `foo :: bar` *does* refer to a first-class object. This is Charm-like and convenient and gives us some nice idioms.

## Why are the tuples flat?

(a) Lists exist for those that want nesting.

(b) I have been guided in my design choices by the idea that mathematicians are probably right in how they communicate, because they have no limits on how they do it and are driven only by questions of readability by humans. They don’t “pass” flat tuples to their functions because unfortunately ‘cos of a design flaw in the whiteboard that’s the only way their equations will compile — they do it because that’s how they make things clear to other people. (Note that MatLab has flat tuples for just this reason, it would be infuriating to mathematicians if it didn’t.)

I’m with them. If I have a function `swap(x, y) : y, x` then I want `swap swap x, y` to be the identity function and not a syntax error.

(c) If not, what happens to referential transparency? In Python we have a situation where `swap(1, 2)` is `2, 1` but if we set `x = 1, 2` then `swap(x)` is an error. Well, Python has no referential transparency anyway but Charm does.

(d) If you had to splat tuples you’d have to splat them a lot. It would be noisy.

## Why are variables typed by default?

Most dynamic languages don’t do that. Why did I? There are three things we can do if we want variables to be dynamic: have them all dynamic, have them dynamic by default, or have them dynamic by request. I’ve gone with the third option.

My thinking is that we rarely use the dynamism of dynamic variables *as such*. To truly exploit this feature we would be changing the type of a variable *at runtime*. And we do this sometimes, but what we mostly enjoy about dynamic languages is how easy it is to change the types of things *between versions of our code*. And that is a result of type inference on the variables and of having untyped function parameters, not of the runtime dynamism.

For most variables, during any given runtime, we know what type they’re intended to be, we intend them to be the same type throughout the runtime, and would like to be alerted if this goes wrong.

## Why the fancy function syntax?

Because Charm can function as its own frontend, and this gives the user a way to provide the end-user with a DSL as that frontend. And for the other reasons one might put it in a language: but that is the particular aspect of Charm that tipped the balance.

## Why overloading and multiple dispatch?

Again, it seems to suit the language and its aims, in that it’s an easy, graspable way of supplying people with all the abstraction and more that most users would want.

## Why the syntactic whitespace?

Now, I can see that there are arguments on both sides. On the one hand proponents of braces point out that it’s easy to lose your way in whitespaced code and braces avoid this problem. On the other hand proponents of whitespace point out that it’s easy to lose your way in code with braces and whitespace avoids this problem.

However, in this case there *is* a reason to decide on one side. Charm is essentially a functional language. And they do tend to have syntactic whitespace, and the reason is that if you can by some perverse feat of ingenuity write a function in an FPL which is both long enough and deeply-nested enough to get lost in, that would be a code smell. A function in Charm is not supposed to have enough nested structure that the people who like braces would actually need braces to be explicit about it. At this point we may simply go with the option that takes less typing.

## Why do errors propagate through commas?

It would be too easy to hide an error value otherwise.

Suppose, for example, that we allowed a function which normally returned (e.g.) a string and an integer to return a tuple containing an integer and an error. As it has type `tuple`, it has no way to force itself up through the stack. It is true that *eventually* if we do anything with the supposed integer the error will pop out at us and demand to be handled. But it isn't necessarily true that the first thing we do with a tuple will be to start talking about its individual elements. We might first wrap it up in a list, and then add that list to another list, and not until for example we enquire `5 in myList` several functions up the call stack and find that we're comparing `5` with an error value would we be obliged to handle it. This would be terrible.

## Why the `given` section?

Because top-down-coding is good. If the constants and inner functions in the `given` section are aptly named, there will hardly be any need to read them: for the human reader as for the interpreter they will serve as explanatory footnotes to a function body which will often be quite small.

The interpreter is quite capable of deciding for itself whether it is necessary to perform the evaluation, without the user putting `let` statements in the apprpriate conditional branches of their functions. (N.B: for “is” read “will be”: I haven't implemented that yet and in some cases this deficit can turn a recursively-implemented `O(n)` computation into a non-terminating one if you're not careful.)

## Why Go?

At this point, why would anyone design a new language *without* seeking a high degree of compatibility with an existing one? By aligning the vocabulary, syntax and semantics of Charm with Go so far as possible for two such different languages, and providing interoperation, I make another language and its libraries available to them.

But why Go in particular? It has a backend orientation suitable for my use-case; the support for concurrency should make it easy to exploit the purity of my functions; it’s a designed language rather than an accreted one, I can assume that the choices I’m stealing have been bikeshedded; it shares with Charm the intention of being small and easy to learn and use. Also the Gophers seem pleased with my project, a fact that may eventually bear on its success.

## OK then, why isn’t it more like Go?

Some of the reasons for specific decisions, e.g. the use of syntactic whitespace, are given above. Others have a similar rationale — for example, there is no point in a `return` keyword in a language in which the body of every function is implicitly a return statement.

The use of `not`, `and`, and `or` was suggested by the utility of English-like DSLs for Charm’s end-users.

The reasoning behind the style guide and its differences from Go will be found in the next section.

## Why is the style guide like that?

As far as the style guide goes, the development of Charm apps makes it convenient to be able to declare and undeclare things as private *en masse*. This makes Go’s use of semantic capitalization unsuitable, and so it has been possible to rethink the style guide in a radical, groundbreaking way and conclude that actually the Java people probably got it about right for once. It was a choice between that and copying Python and at least Go and Java share a preference for camelCase.

## Why the SCREAMING_SNAKE_CASE, though?

Given an identifier in a function, it could be the name of a function, a parameter, a local constant, a global constant, or an enum. Functions can be recognized by their syntactic role. Parameters can be found at the top of the function and local constants at the bottom. Whereas the constants and enums can be found ... anywhere at all The SCREAMING is apt because it's a warning: don't bother looking in the function for the definition of this, 'cos it ain't here.
