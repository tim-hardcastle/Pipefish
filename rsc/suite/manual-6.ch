var

// Globals and encapsulation
// https://github.com/tim-hardcastle/Charm/wiki/Global-variables-and-encapsulation

private

animal = "aardvark"

cmd

setAnimal(s string) :
    global animal
    animal = s 

getAnimal :
    global animal
    post animal

// Basic IO
// // Globals and encapsulation
// https://github.com/tim-hardcastle/Charm/wiki/Global-variables-and-encapsulation

// (Also with tests via REPL.)

greet : 
    get name from Input("What's your name? ")
    post "Hello " + name + "!"

poem :
    get lines from File("rsc/suite/sample.txt", list)
    post lines[2]

