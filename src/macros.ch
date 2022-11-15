import

"lib/prelude.ch" :: ""

var

x = 42
c = code NIL

def

foo(x ast) : x

sum (exp ast) over (ix ast) range (p pair):
    (while condition do action to p[0], 0)[1]
given:
    condition(j, s): j < p[1]
    action(ix varname, s) : (eval ix) + 1, s + (eval exp)

cmd

(v ast) ++ :
	v varname = (eval v) + 1


