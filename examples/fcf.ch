def

makeFancy(s string) : "*!* " + s + " *!*"

apply(f func, arg) : 
    f arg

doApply(s string) :
    apply((func(x) : "*!* " + x + " *!*"), s)

