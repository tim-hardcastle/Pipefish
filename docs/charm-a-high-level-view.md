# Charm: a high level view

## 0. Introduction
While Charm is a general-purpose language, it was particularly created to implement ideas about how people could more ergonomically create, manage, and use CRUD apps.

Charm was, roughly speaking, inspired by the thought: “People like databases and they like spreadsheets. Let's make a language which at a *very very* high level captures the things people like about them.” In part 1 of this document I will first explain which features of these apps I wish to emulate, and then briefly point out how Charm seeks to embody them. In part 2 I will point out some principles that I think apply generally to language design, and add a few notes on how these relate to the more distinctive decisions of the language design and to the aims of Charm in particular as set out in part 1.

## 1. A CRUD language

So, what *do* people like about databases and spreadsheets?

* Start with the obvious: they're CRUD apps. Or we might say CRUDE, with the E standing for "Evaluate", since you can write formulas, run queries, etc. But the point is, they both support a model of data processing where you have a repository of data which you add to and change incrementally and query from time to time, rather than: batch of data in, final answer out.

* Furthermore, they're CRUD *apps*. They don't just give you ways to C, R, U, and D, they can provide you with an environment with options to, e.g. turn autosave on, or export as CSV, or look at a help file, or whatever. Users don't have to write that themselves, the users write the business logic and supply the data.

* These apps have nice front-ends that include point (2). In the case of spreadsheets, the front-end can in the long run be a problem, it's both too permissive and too restrictive. In SQL, on the other hand, the language serves as its own front-end.

* The languages are nice and simple. There are, for example, no pointers and dereferencing. No deep and shallow copy.

* The barriers to use are low: there is progressive disclosure. People who would run a mile if asked to "write a computer program" are comfortable writing Excel formulas or SQL queries, and they are able to learn the simpler aspects of doing so while leaving the more complicated concepts until later.

* The relationship with the app can be made exploratory and interactive.

* I/O is initiated by the end-user. They run queries, write formulas, etc.

* In particular, changes in the variables are initiated by the user. Now, this is not *strictly* true of the apps I'm taking as models: if you want to you can simulate Conway's Game of Life in Excel. But except to show off, people don't use it like that, they put (e.g.) their sales projections in one place, and their consequent projections of profit and loss in another, and the sales projections don't change or vanish unless the user makes that happen: such behavior would usually be considered extremely undesirable.

* This put together means that the rest of what people want to do, writing queries and formulas to evaluate the data, can be done in a functional style, since there's nothing else left to do.

* They don't crash (or at least, by design they don't crash). This last point follows from the previous ones. If we try to *perform a computation* where the *values* become nonsense, then it makes sense to return an error value saying "This is nonsense"; whereas in a more procedural language, if we try to *execute a process* where the *state* becomes nonsense, then we have to abort the process.

These considerations lead us to Charm, a REPL-oriented, data-oriented, [functional core / imperative shell](https://github.com/tim-hardcastle/Charm/blob/main/docs/functional-core-imperative-shell.md) language which thinks that everything is a CRUD app, and which comes with extensive facilities to let you build DSLs to form the front-ends of Charm services. For your convenience the language comes wrapped in an invisible framework that lets you reconfigure a desktop app you interact with via its REPL as a backend service which you interact with via a client app. (While there are ways to put other front-ends on Charm, it is supposed that it will often, like SQL, be used as its own front-end.)

# 2. Principles of design

The following properties seem generally desirable in a language. It should be:

**Small**. All things being equal, you want less of a language, because the more features the language has, the harder it is to reason about code.

**Orthogonal**. The way to get the most power out of your relatively small language is to have your features do different things, without duplicating one another's functionality.

**Composable**. It should be easy to use these different features together.

**Local**. It should be easy to understand the meaning and purpose of a piece of code by reading as little as possible of the rest of the code. 

**Consistent**. Knowing how some of how the language works, it should be easy to guess how a feature you haven't learned yet will work. 

**Intolerant**. A language should not default to covering for your false assumptions. Failure should be immediate and noisy. 

**Capable of enough abstraction** (i.e. the ability to treat different things as the same to the extent that they actually are). What is “enough” and how it is to be achieved varies from case to case.

**Friendly to users**. "Great software is an act of empathy." A programming language in particular is a nice front-end for machine code, that’s pretty much the whole deal.

**Friendly to ecosystems**. It is 2023, a language should be able to play ball with other languages.

**Purposeful**. Again, it is 2023, some idea of what niche a language should fill is necessary.

A functional language must particularly be composable, of course, and Charm is very much so. A functional language can (other things being equal) also be smaller than an imperative one, and more simply structured on a small scale.

Charm is exceptionally hardcore about types considering that it’s technically a dynamic language. (I have learned from the mistakes of others.) This supplies a useful degree of intolerance.

It achieves abstraction by multiple dispatch (with reflection and first-class types as backup). This allows a small consistent syntax that progresses from functions with untyped arguments to functions with typed arguments to functions with more than one version depending on the arguments.

The functional core/imperative shell style that Charm enforces for reasons given in section 1 gives us great locality of code. It also allows a more familiar style of language than many FPLs.

In particular, where issues of syntax, semantics, style or nomenclature seemed minor, I have copied from Go. (The reasons for the choice of Go in particular are laid out in [another document](https://github.com/tim-hardcastle/Charm/blob/main/docs/the-whys-of-charm.md#why-go).) This makes it easier to use Charm as part of the Go ecosystem and vice-versa. It also allows me to reuse the work Google has put in on consistency, e.g. in the naming of library functions, the order of their parameters, etc. 

The fact that Charm is a REPL language gives us many ways to make things easy for the user. For example, it enables progressive disclosure of error messages, so that the user can be presented with a concise, readable summary of what went wrong and can then ask for further information at need.
