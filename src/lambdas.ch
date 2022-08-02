var

add = func(x, y) :
    x + y

g = addN(5)

def

addN (n) :             //  Look, a closure!
    func(x) : x + n

apply (f) to (x tuple) :
    f x

apply (f) twice to (x tuple) :
    f f x

while (p) do (f) to (x tuple) :
    p x : while p do f to f x
    else : x

condition = func (x, y) : x > 0
action = func (x, y) : x - 1, y * 2

