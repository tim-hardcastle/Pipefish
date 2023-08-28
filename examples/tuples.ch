def

swap (x, y) : y, x

rotateLeft(t tuple) : 
    t == () : 
        ()
    else : 
        t[1::arity t] , t[0]

Direction = enum LEFT, RIGHT

rotate(d Direction, t tuple) : 
    t == () : 
        ()
    d == RIGHT : 
        t[arity(t) - 1] , t[0::arity(t) - 1]
    else : 
       rotateLeft t

