var

w = a, b
x = 1, true, ["banana", false], "elephant"
y = ["marmalade", ["owl", 2]]
z = "♥Charm♥"

f = func (x) : a * b(x)
given : 
    a = x + 1
    b = func(x) : x + c
    given :
        c = 2

g = addN(5)

def

addN (n) : 
    func(x) : x + n

a = "umbrella"
b = 3

max (x, y int) :
    x > y : x
    else : y

max (L list):
    len(L) == 0 : error "taking the max of an empty list"
    else : maxer(1 , L[0], L)
given :
    maxer = func(i int, m int, L list) :
            i < len(L) : this i + 1, max(m, L[i]), L
            else : m

