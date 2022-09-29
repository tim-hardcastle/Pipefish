import "lib/prelude.ch" :: ""

def

DVal = struct(val float, KG, M, S int)

unit(x float) : DVal(x, 0, 0, 0)

(x float) * (v DVal) : unit(x) * v

(v DVal) * (x float) : v * unit(x)

(x float) / (v DVal) : unit(x) / v

(v DVal) / (x float): v / unit(x)

(x float) + (v DVal) : unit(x) + v

(v DVal) + (x float) : v + unit(x)

(x float) - (v DVal) : unit(x) - v

(v DVal) - (x float) : v - unit(x)

(u DVal) * (v DVal) :
    DVal(u[val] * v[val], u[KG] + v[KG], u[M] + v[M], u[S] + v[S])

(u DVal) / (v DVal) :
    DVal(u[val] / v[val], u[KG] - v[KG], u[M] - v[M], u[S] - v[S])

(u DVal) + (v DVal) :
    u[KG] != v[KG] or u[M] != v[M] or u[S] != v[S] :
        error "sum is dimensionally incorrect"
    else :
        DVal(u[val] + v[val], u[KG], u[M], u[S])

(u DVal) - (v DVal) :
    u[KG] != v[KG] or u[M] != v[M] or u[S] != v[S] :
        error "difference is dimensionally incorrect"
    else :
        DVal(u[val] - v[val], u[KG], u[M], u[S])

(f float) kg : (unit f) kg

(v DVal) kg : v with KG :: v[KG] + 1

(f float) m : (unit f) m

(v DVal) m : v with M :: v[M] + 1

(f float) s : (unit f) s

(v DVal) s : v with S :: v[S] + 1

print (v DVal) :
       string(v[val]) + " " + (repeat "kg ", v[KG] times) ..
       .. + (repeat "m ", v[M] times) + (repeat "s", v[S] times) ..
       .. + (negativesExist(v) : negatives(v); else : "")
given :
    negativesExist(v) : v[KG] < 0 or v[M] < 0 or v[S] < 0
    negatives(v) :
       "/ " + (repeat "kg ", -v[KG] times) ..
       .. + (repeat "m ", -v[M] times) + (repeat "s", -v[S] times) 

repeat (s string, n int) times :
    n <= 0 : ""
    else : (for n do action to "")[0]
given:
    action(t string) : t + s