// Constants and variables
// https://github.com/tim-hardcastle/Charm/wiki/Constants-and-variables

var

h = "Hello world!"
x = 2 + 2

def

MONTHS_IN_A_YEAR = 12

cmd

addToXAndShow(n) : 
    global x
    x = x + n
    post x to Output()

// Comments and continuations.
// https://github.com/tim-hardcastle/Charm/wiki/Comments-and-continuations

var

X = "hello " + ..
 .. "world"

Y = 1, 2, 3,
 .. 4, 5, 6

 