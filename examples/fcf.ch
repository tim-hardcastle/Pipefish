def

applyTwice(f func, arg string) : 
    f f arg

makeFancy(s string) : "*!* " + s + " *!*"

decorator(decoration string) :
    func(x) : decoration + " " + x + " " + decoration
