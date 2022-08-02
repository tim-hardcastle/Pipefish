def

// We could propagate the error thrown by %.

(x int) modA (y int) : 
    remainder >= 0 : remainder
    else : remainder + y
given :
    remainder = x % y

// We could anticipate the error and throw our own.

(x int) modB (y int) : 
    y == 0 : error "taking the modulus of a number by zero"
    remainder >= 0 : remainder
    else : remainder + y
given :
    remainder = x % y

// We could catch the error and throw our own.

(x int) modC (y int) : 
    type remainder == error : error "taking the modulus of a number by zero"
    remainder >= 0 : remainder
    else : remainder + y
given :
    remainder = x % y

// We could catch the error and return something other than an error.

(x int) modD (y int) : 
    type remainder == error : "this isn't an error, just a friendly warning"
    remainder >= 0 : remainder
    else : remainder + y
given :
    remainder = x % y

// ... etc, etc.