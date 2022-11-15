def

cat = struct(name string, age int)

dog = struct(name string, age int)

foo(x string) : "foo of string"

foo(x any) : "foo of any"

foo(x int) : "foo of int"

foo(x tuple) : "foo of tuple"

foo(s struct) : "foo of struct"

foo(c cat) : "foo of cat"

foo(n nil) : "foo of nil"

var

//myDog = dog with name :: "Bonzo", age :: 4

//myCat = cat with name :: "Tibbles", age :: 10