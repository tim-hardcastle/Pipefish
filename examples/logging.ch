// Example of logging/intrumentation. Anything preceeded by \\ is
// logged.

def

foo(x, y) :             \\ "Called with parameters", x, y
    x % 2 == 0:         \\ "Testing if x is even."
        x               \\ "x is even. Returning", x
    else :              \\ "Else branch taken"
        2 * y           \\ "Returning", 2 * y

zort(x, y) :                      \\ 
    x % 2 == 0 and y > 7:         \\ 
        x                         \\ 
    else :                        \\
        x < y : 42                \\
        else : x + y              \\ 