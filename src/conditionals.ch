def

// Conditionals nest more comprehesibly with whitespace.

classify (i int) :
    i > 0 :
        i < 10 : "small number"
        else : "big number"
    i == 0 : "zero"
    else :
        i > -10 : "small negative number"
        else: "big negative number"

// An incomplete conditional.

incomplete(i) :
    i > 0 :
        i < 10 : "small number"

// And a use for one.

objectToZero(x) :
    x == 0 : "Zeros are bad! Bad!"

(x) divides (y) :
    objectToZero(x)
    else : y % x == 0