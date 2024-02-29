def

isEven(x int) :
    x % 2 == 0 :
        "even"

safeNull(t tuple) :
    ([t] ?> that in null -> len) > 0 :
        NULL

safeAdd(a int?, b int?) :
    safeNull(a, b)
    else :
        a + b
