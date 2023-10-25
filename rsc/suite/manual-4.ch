def

// Basic functions, from the page
// https://github.com/tim-hardcastle/Charm/wiki/Introducing-functions

square(x) : x * x

parity(x) : 
    x % 2 == 0 : "even"
    else : "odd"

swap(x, y) : y, x

classify(i int) :
    i > 0 :
        i < 10 : "small number"
        else : "big number"
    i == 0 :
        "zero"
    else :
        i > -10 : "small negative number"
        else : "big negative number"

// Infixes, postfixes, etc
// https://github.com/tim-hardcastle/Charm/wiki/Infixes,-postfixes,-etc

(x) squared : x * x

(x) times (y) : x * y

(x) divides (y) :
    y % x == 0

(x) is even:
    2 divides x

say (x) nicely :
    "*~*" + x + "*~*"

divide (x) by (y) : 
    x / y

// Function signatures and overloading
// https://github.com/tim-hardcastle/Charm/wiki/Function-signatures-and-overloading

twice(x string) : x + ", " + x

twice(x int) : 2 * x

twice(b bool) :
    b : "That's as true as things get!"
    else : "That's as false as things get!"

twice(t single) :
    error "I don't know how to double that!"

twice(t tuple) :
	t, t

Money = struct(dollars, cents int)

ONE_DOLLAR = Money(1, 0)
TREE_FIDDY = Money(3, 50)

(m Money) + (n Money) :
    m[cents] + n[cents] >= 100 :
        Money(m[dollars] + n[dollars] + 1, m[cents] + n[cents] - 100)
    else :
        Money(m[dollars] + n[dollars], m[cents] + n[cents])

string(m Money) :
    "$" + string(m[dollars]) + "." .. 
     .. + (m[cents] < 10 : "0"; else : "") ..
     .. + string(m[cents])

// Local constants and inner functions
// https://github.com/tim-hardcastle/Charm/wiki/Local-constants-and-inner-functions

g(x) : a * b * x
given : 
    a = x + 1
    b = x + 2

happyAddition(a, b) : 
    makeHappyString a + b
given : 
    makeHappyString(i) : "*!* " + (string i) + " *!*"

foo(x) : a * b(x)
given : 
    a = x + 1
    b(x) : x + c(x)
    given :
        c(x): m * x
        given m = 5

// Iterators and piping
// https://github.com/tim-hardcastle/Charm/wiki/Iterators-and-piping

// Note that piping is tested mainly in the REPL.

triangularNumber(n int) : 
    (while condition do action to 1, 0)[1]
given : 
    condition(counter, total) : counter <= n
    action(counter, total) : counter + 1, total + counter

betterTriangularNumber(n int) : 
    for i over 1::(n + 1) do addI to 0
given : 
    addI(x) : x + i

shorterTriangularNumber(n int) : 
    for i over 1::(n + 1) do (func(x) : x + i) to 0

// Functions and tuples
// https://github.com/tim-hardcastle/Charm/wiki/Functions-and-tuples

rotateLeft(t tuple) : 
    t == () : 
        ()
    else : 
        t[1::arity t] , t[0]

Direction = enum LEFT, RIGHT

rotate(d Direction, t tuple) : 
    t == () : 
        ()
    d == RIGHT : 
        t[arity(t) - 1] , t[0::arity(t) - 1]
    else : 
        rotateLeft t

rotate(t tuple) to (d Direction): 
    t == () : 
        ()
    d == RIGHT : 
        t[arity(t) - 1] , t[0::arity(t) - 1]
    else : 
        rotateLeft t


zort :
    "No parameters"

zort(s single) :
    "Single"

zort(x single, y tuple) :
    "Single, then tuple"

zort(t tuple) :
    "Tuple"

safeNull(t tuple) :
    ([t] ?> that in null -> len) > 0 :
        NULL

safeAdd(a int?, b int?) :
    safeNull(a, b)
    else :
        a + b

// Error handling
// https://github.com/tim-hardcastle/Charm/wiki/Error-handling

(x int) modC (y int) :
    type remainder == error : error "taking the modulus of a number by zero"
    remainder >= 0 : remainder
    else : remainder + y
given :
    remainder = x % y

(x int) modD (y int) :
    type remainder == error : "this isn't an error, just a friendly warning"
    remainder >= 0 : remainder
    else : remainder + y
given :
    remainder = x % y

// Recursion
// https://github.com/tim-hardcastle/Charm/wiki/Recursion

FACTORIAL = func(n int) :
    n == 0 : 
        1
    else :
        n * this n - 1 