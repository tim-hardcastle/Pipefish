def

while (p) do (f) to (z tuple) :
    p z : while p do f to f z
    else : z

(L) apply (f func) :
    (for len(L) do action to 0, [])[1]
given:
    action(i, R) : i + 1, R + [f L[i]]

for (n) do (f) to (x tuple) :
    (while unfinished do loop to 0, x)[1::arity(x) + 1]
given :
    unfinished = func(i int, x tuple) : i < n
    loop = func(i int, x tuple) : i + 1, f x

mergesort (L list) :
    len L <= 1 : L
    else :
        mergeSorted(mergesort(L[0 :: len(L)/2]), mergesort(L[len(L)/2 :: len(L)]))

mergeSorted(A, B) :
    (while condition do action to [], A, B) [0]
given :
    condition = func(output, A, B) : A or B
    action = func (output, A, B) :
        not A : output + B, [], []
        not B : output + A, [], []
        A[0] < B[0] : output + [A[0]], tail(A), B
        else : output + [B[0]], A, tail(B)

tail (L) : 
    len(L) <= 1 : []
    else : L[1::len L]

(L list) behead (i int) :
    len(L) < i : error "behead removing more elements than the object contains"
    len(L) == i : []
    else : L[i::len L]

(L list) curtail (i int) :
    len(L) < i : error "curtail removing more elements than the object contains"
    len(L) == i : []
    else : L[0::len(L) - i]

(s string) behead (i int) :
    len(s) < i : error "behead removing more elements than the object contains"
    len(s) == i : ""
    else : s[i::len s]

(s string) curtail (i int) :
    len(s) < i : error "curtail removing more elements than the object contains"
    len(s) == i : ""
    else : s[0::len(s) - i]

max (x, y) :
    x > y : x
    else : y

min (x, y) :
    x < y : x
    else : y

zip (L, R list):
    (for (len L) do action to 0, map ())[1]
given :
    action(i, M) : i + 1, (M with L[i] :: R[i])
