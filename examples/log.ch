def

foo(x) :            \\ 'foo' called.
    x % 2 == 0  :   \\ Checks if x is even. 
        x           \\ Returns |x|     
    else :          \\ Else branch taken.
        twice x     \\ Returns ||twice x||.
given :             \\
    twice(n) :      \\ 'twice' called.
        two * n     \\ Calculates ||2 * n||.
    two = 2         \\ Lazy assignment to TWO.

bar(x) :            \\ 
    x % 2 == 0  :   \\ 
        x           \\ 
    else :          \\ 
        twice x     \\ 
given :             \\
    twice(n) :      \\ 
        two * n     \\ 
    two = 2         \\ 

