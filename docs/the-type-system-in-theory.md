# The type system in theory

## Introduction

This document will explain the type system. By contrast to the explanation in the manual, which treats each particular type in detail, this document gives a broad overview but explain why the type system is like this, which the manual does not. This document can therefore serve as an introduction for people who wish to offer constructive criticism of the type system and suggestions for augmentations.

At the end I will review some ideas for extension of the type system.

## Concrete types

A concrete type is a type that a value can actually have. They can be divided into base types, container types, and definable tpes.

* Base types are the familiar `string`, `int`, `bool`, etc, plus some that are more specific to Pipefish.

* Container types: `list`, `set`, `tuple`, `pair`. These are not generic types: you cannot specify their contents by saying `list<int>` or `set<string>`.

* Definable types: structs and enums. Example definitions are given below.

```
newtype

Color = enum RED, ORANGE, YELLOW, GREEN, BLUE, PURPLE

Dragon = struct(name string, color Color, valueOfHoard int)
```

# Abstract types

Perhaps these had better be called "filter types": I'll change this next time I overhaul the documents. In the meantime I'll go on calling them abstract types for consistency with the rest of the documentation.

These are not types that a value can actually have. Rather they are a filter on what can be stored in a variable, or the field of a struct, or passed to a function.

Pipefish suppplies you with a few built-in abstract types: `single`, which includes evrything but the `null` type ad the `tuple` type; `struct`, which includes all structs; and a "nullable twin" of each type, containing that type and null: `int?`, `string?`, etc. User-defined types and other abstrct types also have nullable twins: `Dragon?`, `struct?`, etc.

Abstract types can be user-defined. In this example we define an abstract type `Monster` which contains the concrete types `Orc`, `Troll`, and `Dragon`, and which does not contain the type `Person`; and we write two functions which dispatch on whether something is a `Monster` or a `Person`.

```
newtype

Troll = struct(name string, turnedToStone bool, IQ bool)
Orc = struct(name string, urukHai bool)
Dragon = struct(name string, color Color, flameTemperature int)
Person = struct(name string, age int)

Monster = Dragon / Troll / Orc

def

kill(m Monster) :
	"You kill" + m[name] + " the " + (string type m) + "."

kill(p Person) : 
    "Killing people is wrong! Even " + p[name] + "."
```

(Sidenote: if you want to, and 99.9% of the time you don't, this works for making type aliases if there's just one concrete type in the abstract type. E.g: `newtype Human = Person` would let you use `Human` as a synonym for `Person`.)

Now let's discuss why it's like this. First, a digression on the nature of static and dynamic languages.

## Static v. dynamic

A dynamic language can be defined as one in which the runtime often needs to inspect the types of values, which must therefore be each be decorated by a tag representing its type.

(For the pedants, note the word "needs": the *need* for a lot of runtime type inspection to get the language semantics to work is what makes a language per se a dynamic language and not merely a dynamically implemented language.)

I have given (what I hope is) a reasonable definition of a dynamic language because this often gets confused with other concepts. For example with weakly-typed languages, whereas Pipefish is very strongly typed. Or with the absence of type-checking whereas the version of Pipefish now under construction catches many type errors. What it cannot do is type-check so thoroughly that it can erase all the type information from the values and still preserve the same semantics, thereby becoming a static language.

## Problems with generic types

We can now talk about why Pipefish doesn't have generic types like `list<list<int>>` or whatever.

### The semantic problems

First, implementing this raises some question about semantics the answers to which would inevitably cause surprise under some circumstances. For example, suppose we try to add `x` of type `list<string>` and `y` of type `list<single>`. Should the result be:

(a) A type error? (In which case it's hardly behaving like a dynamic language any more — even if we delay the type error until runtime.)

(b) A `list<single>` even if `y` happens to contain only strings.

(c) The runtime checks the types of everything in `y` (at O(n) cost, natch) and returns a `list<string>` if everything in it is a string but otherwise returns a `list<single>`.

(d) The runtime checks the types of everything in `y`, and returns a `list<string>` if everything in it is a string but otherwise returns a `runtime error.

(e) Other?

(Now consider what whould happen if we try to add together two things of type `list<int>` and `list<string>` if this is always a type error requiring conversions to get it to work, then the language is again becoming undynamic in spirit. If on the other hand it blithely returns something of type `list<int/string>` without any type error then what is the point of being able to represent generic types?)

This is exactly the sort of thing that trips programmers up because in the heat of the moment they will tend to assume that the semantics will do what would make most sense in the context of the particular thing they're doing.

It also means that if they wanted to do one of the options I *didn't* implement, then not only would they have to use reflection quite a lot but also I'd have to supply them with some tools to do downcasting, and we'd have to ask ourselves if a language which requires so much reflection and explicit downcasting still has the virtues of a dynamic language.

### The implementation problems

Besides deciding what to do, there's the problem of doing it, and of doing it efficently. The problem here is that, again, a dynamic language is one where the values have to be tagged with their types so that the type tag can be inspected at runtime.

Now in the system implemented, each concrete type can be represented as an integer, and so comparison of two concrete types is just integer comparison, the question of whether a concrete type belongs to a given abstract type can be given by indexing an array of booleas by the type number; the question of whether a given type can be put in a container type doesn't arise, since the answer is always "yes"; and two values can be compared only if they are of the same concrete type.

If we add generic types, however, then the types themselves have to have structure. If we have a list literal like `[1, 2, [true, "foo"]` then its type is `list<int/list<bool/string>>`. We will have to construct that type in parallel with constructing the list, and we have to have a little recursive algorithm to inspect the type any time we want to see if it's the same type or a subtype of another type. We even have a litle bit of overhead even if we have things of a basic type like `int`, since they still have to be expressed in terms of this more elaborate structure.

These considerations together suggest that it's hard to make a dynamic type system that can incorporate generic types and yet have sensible syntax and be performant.

## Other extensions

But there are other interesting things we might do to it. Here's an overview of some possibilities.

### Validation

We could add validation to types on construction. E.g. to make sure that every dragon has a flame hot enough to boil water, and that none of them is purple (because I am not a teenage girl), we could write something like this:

```
Dragon = struct(name string, color Color, flameTemperature int)
given :
	self[flameTemperature] >= 100
        self[color] != PURPLE
```

### Formulaic definition of abstract types

As things are, abstract types must be defined "by hand", e.g. by writing `Monster = Troll/Orc/Dragon`. But we could define an abstact class by the properties of its members, e.g. that it contains all structs having a given field label. Or that it can have function `foo` called on it.

Given that simplicity is an important goal of the language, I'd have to think about what could be commonly useful and clearly expressible. I don't want to havre obscure idioms. People can just write more code.

### Inheritance

I'm against this. We have duck typing and multiple dispatch for when we want to do abstraction. Adding something widely perceived as an antipattern doesn't seem like a good idea.

It would also have the strange effect that a parent type would be concrete, since it could be instantiated, and abstract in that it subsumes more than one concrete type, thus messing up the semantics of the type system itself.

### Cloning

We can make types clonable, e.g:

```
newtype

ShoeSize = clone int
NumberOfHeads = clone int
```

Now we have two types that behave like integers but which the type system can discriminate between, so that we will get most likely a compile-time error and in the worst case a run-time error if we try to pass one off as the other.

This leaves a lot of points to be clarified about the semantics.

First of all, considering our examples so far, both clones of `int`, we might ask, what is their relationship to the `int` type? We might make them subtype it, giving us again the unwelcome situation that `int` would be both a concrete and an abstract type depending on context. Moreover, it would allow us to call strange and inappropriate functions on the clones types: we would be able to square a shoe size. So for now let's not do that, and instead have `int` and `ShoeSize` and `numberOfHeads` be separate concrete types.

But should they have no relation with `int`? It seems like it would be cumbersome in a dynamic language to have to increment a variable `n` of type `NumberOfHeads` by doing type coercion and writing `t = t + NumberOfHeads(1)`.

And when it comes to multiplication, multiplying a NumberOfHeads by a plain int makes more sense than multiplying it by a NumberOfHeads, because of dimensional correctness.

So looked at that way, it might make sense that the builtin functions/operations for a type should work for its cloned types together with a little bit of type coercion. It's what the programmer would want to happen.

OK, but now let's apply that same rule to a list. We make a type called `ListOfMonsters` by cloning `list`. We can now add the list `["foo", "bar"]` to a ListOfMonsters without any type error. This is not what the programmer would want to happen. Or again this rule would alow us to add any string to a CapitalizedString, etc.

(Treating the different builtin functions differently would be madness all round.)

Now think about what happens if someone wants to add validation to cloned types. For example:

```
newtype

EvenNumber = clone int
given :
    self % 2 = 0

OddNumber = clone int
given :
    self % 2 = 1
```

Should addition even be defined on the types themselves by default? We have a choice of evils:

(1) By default we get an operation `+` on OddNumber meaning that adding two of them together will *always* pass the compile time checks while also *always* causing a runtime error

(2) The user has to write code by hand to say that adding two even numbers together is a valid operation returning an even number.

Option 2 is easy to explain, but it places some silly burdens on the user. For example no-one should have to write `len` for every clone of `list`. So maybe we need to distinguish between binary operations and functions? Or just make a list of builtins that get cloned and those that don't ... the ultimate in unprincipled, inconsistent language design! A breakthrough!

Now think about what happens when someone wants to clone validated types in the expectation of inheriting their validation. Now ask whether it is meaningful to clone an abstract type ...

## Picking and choosing

Pipefish, again, aims at simplicity. I want to find the features that will genuinely get used, and that are easy enough to explain. I don't want to open up a semantic tarpit which I have to spend pages explaining and hardly anyone wants to use. And whatever I pick should be freely composable and the results of doing so should be obvious.

So you can see why for now I've stopped at abstract types. All suggestions will be gratefully received.
