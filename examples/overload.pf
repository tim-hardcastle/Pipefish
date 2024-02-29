def 

foo :
    "Just plain foo"

foo (s single) : // single means 'anything but a tuple'
    "signature (s single) with s of type " + string (type s)

foo (b bool) :
    "signature (b bool)"

foo (i int) :
    "signature (i int)"

foo (b bool, t tuple) : // 'tuple' is how Charm does varargs
    "signature (b bool, t tuple) with t = " + string t 

foo (t tuple) :
    "signature (t tuple) with t = " + string t 

