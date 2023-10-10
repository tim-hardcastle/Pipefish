// Code for testing, to ensure that the manual does what it says it does.
// Parts 1 and 2: "Getting started" and "Language basics". 

// Code from "A first script", also quoted in "Commnds and functions".
// https://github.com/tim-hardcastle/Charm/wiki/A-first-script
// https://github.com/tim-hardcastle/Charm/wiki/Commands-and-functions

cmd

greet : 
    get name from Input("What's your name? ")
    post "Hello " + name + "!"

def

factorial (n) :
    n == 0 : 1
    n > 0 : n * factorial n - 1
    else : error "can't take the factorial of a negative number"


// Code from "Constants and variables"
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

// Code from "Comments and continuations".
// https://github.com/tim-hardcastle/Charm/wiki/Comments-and-continuations

var

X = "hello " + ..
 .. "world"

Y = 1, 2, 3,
 .. 4, 5, 6

 