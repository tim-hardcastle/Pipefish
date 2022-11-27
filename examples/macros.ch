import

"lib/prelude.ch" :: ""

var

x = 42
c = code NIL

def

capture (exp ast) : exp

cmd

zero (v ast) :
    v varname = 0

(v ast) ++ :
	v varname = (eval v) + 1

def

sum (exp ast) over (ix ast) range (p pair):
    (while condition do action to p[0], 0)[1]
given:
    condition(j, s): j < p[1]
    action(ix varname, s) : (eval ix) + 1, s + (eval exp)




