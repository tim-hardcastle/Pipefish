def

Person = struct(name string, age int)

Cat = struct(name string, nobelPrizes int, pink bool)

catDefaults = nobelPrizes :: 0, pink :: false

CatOwner = struct(name string, pet Cat)

foo(t tuple) : "foo"

var

doug = Person "Douglas", 42

joe = Person with name :: "Joseph", age :: 22

tom = Person with age :: 49, name :: "Thomas"

myCat = Cat with name :: "Felix", catDefaults

me = CatOwner("Tim", myCat)

myField = name



