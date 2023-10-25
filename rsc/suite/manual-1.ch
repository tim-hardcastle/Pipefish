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


