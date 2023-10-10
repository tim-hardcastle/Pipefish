// In which we specifically demonstrate the claims about the type 
// system made in the wiki.

// Page 3.0 is the intro to the type system,
// https://github.com/tim-hardcastle/Charm/wiki/The-type-system
// It notes the restrictions on equality and inequality. These can 
// be demonstrated in the REPL.

// Pages 3.1 and 3.2:
// https://github.com/tim-hardcastle/Charm/wiki/Simple-types
// https://github.com/tim-hardcastle/Charm/wiki/Container-types
// are demonstrated using literals in the manual, so we need supply
// no code for the tests.

// 3.3 is on structs
// https://github.com/tim-hardcastle/Charm/wiki/Structs

def

Person = struct(name string, age int)
Cat = struct(name string, nobelPrizes int, pink bool)
CatOwner = struct(name string, pet Cat)

CAT_DEFAULTS = nobelPrizes :: 0, pink :: false

var

doug = Person "Douglas", 42
joe = Person with name :: "Joseph", age :: 22
tom = Person with age :: 49, name :: "Thomas"
myCat = Cat with name :: "Felix", CAT_DEFAULTS
me = CatOwner("Tim", myCat)
myField = name

// 3.4, enums:
// https://github.com/tim-hardcastle/Charm/wiki/Enums

def

Suit = enum CLUBS, HEARTS, SPADES, DIAMONDS

Value = enum ACE, TWO, THREE, FOUR, FIVE, SIX, SEVEN,
          .. EIGHT, NINE, TEN, JACK, QUEEN, KING

Card = struct(value Value, suit Suit)

isBlack(suit Suit) : suit in {CLUBS, SPADES}

isBlack(card Card) : isBlack(card[suit])

// 3.5, abstract types:
// https://github.com/tim-hardcastle/Charm/wiki/Abstract-types

def

Penguin = struct(name varchar(20), numberOfChicks int?, favoriteFood string?)

nullToZero(i int?) : 
    i in null : 0
    else : i

var

shortString varchar(10) = "walrus"
couldBeInt int? = 42
couldBeBool bool? = NULL


// Page 3.6 is the type system diagram and has no associated examples.

// Page 3.7 is on type conversion. It will be tested in the REPL.
// https://github.com/tim-hardcastle/Charm/wiki/Type-conversion-and-reflection
