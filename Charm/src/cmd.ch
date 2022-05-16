var

x = 42

cmd

times (n) :
    x = n * x

add (n) :
    x = x + n
    return x

step2A :
    x = x + 1
    return x
    x = x + 1
    return x

step2B :
    add (n)
    add (n)
given n = 1







