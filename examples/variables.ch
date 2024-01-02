var

h = "Hello world!"
x = MONTHS_IN_A_YEAR

b = false or TRUTH

def

foo(x int) :
    "Called foo on an integer."

foo(x string) :
    "Called foo on a string."

foo(x single) :
    "Called foo on something else."

MONTHS_IN_A_YEAR = 12

LIE = false

TRUTH = not LIE

FOO = "foo"

zort(x int) :
    x == 8

troz(x, y int) :
    x == y

implies(x, y bool) :
    not (x and not y)

oof(x int) :
    x == y 
given :
    y = 5