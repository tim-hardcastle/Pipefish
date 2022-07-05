def

// You only really need this one:       

while (p) do (f) to (x tuple) :
    p x : while p do f to f x
    else : x

// But we can make other general-purpose recursive functions ...

for (n) do (f) to (x tuple) :
    (while unfinished do loop to 0, x)[1::count(x) + 1]
given :
    unfinished = func(i int, x tuple) : i < n
    loop = func(i int, x tuple) : i + 1, f x

// And all the nasty recursion disappears ...

power2 (n int) : for n do action to 1
given : action = func(x) : 2 * x

stars (n int) : for n do action to ""
given : action = func(x) : x + "*"

// Let’s make that max function shorter

max(L list) : (while condition do action to 1, L[0], L)[1]
given :
    condition = func(i, m, L) : i < len(L)
    action = func(i, m, L) : i + 1, max(m, L[i]), L

max (x, y int) :
    x > y : x
    else : y
