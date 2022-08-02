def

person = struct(name string, age int)

cat = struct(name string, nobelPrizes int, pink bool)

catDefaults = nobelPrizes :: 0, pink :: false

catOwner = struct(name string, pet cat)

var

doug = person "Douglas", 42

joe = person with name :: "Joseph", age :: 22

tom = person with age :: 49, name :: "Thomas"

myCat = cat with name :: "Felix", catDefaults

me = catOwner("Tim", myCat)

myField = name
