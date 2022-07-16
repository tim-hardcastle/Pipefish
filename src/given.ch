def

g (x) : a * b * x
given : 
    a = x + 1
    b = x + 2

// Now let's use that 'while' function properly.

while (p) do (f) to (x tuple) :
    p x : while p do f to f x
    else : x

power (x) :
    while condition do action to x, 1
given : 
    condition = func (x, y) : x > 0
    action = func (x, y) : x - 1, y * 2

// Lambdas can have 'given' blocks too.
// And lambdas in 'given' blocks can have 'given' blocks, etc.

var 

f = func (x) : a * b(x)
given : 
    a = x + 1
    b = func(x) : x + c
    given :
        c = 2



