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
    (while condition do action to x, 1)[1]
given : 
    condition(x, y) : x > 0
    action(x, y) : x - 1, y * 2

// Inner functions can have their own 'given' blocks,
// as can lambdas.

var 

f = func (x) : a * b(x)
given : 
    a = x + 1
    b(x) : x + c(x)
    given :
        c = func(x): m * x
        given m = 5



