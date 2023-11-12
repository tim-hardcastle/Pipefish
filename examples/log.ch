def

foo(x) :            \\ 'foo' called.
    x % 2 == 0  :   \\ Checks if x is even. 
        x           \\ Returns |x|     
    else :          \\ Else branch taken.
        twice x     \\ Returns ||twice x||.
given :             \\
    twice(n) :      \\ 'twice' called.
        TWO * n     \\ Calculates ||2 * n||.
    TWO = 2         \\ Lazy assignment to TWO.