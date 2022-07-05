def

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

// For fancier syntax, you can just substitute 'this' 
// for the first word of the function.

say (s string) nicely :
    len s < 12 : this "*~" + s + "~*" nicely
    else : s
