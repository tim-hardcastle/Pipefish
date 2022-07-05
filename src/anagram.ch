import

"lib/prelude.ch"

def

anagram(s, t):
    S == T
given :
    S = letterCount s
    T = letterCount t

letterCount(s) :
    while condition do action to 0, listOf 26 times 0
given :
    condition = func(i, S) : i < len s
    action = func(i, S) : i + 1, S ++ codepoint(s[i]) - 97 :: S[codepoint(s[i]) - 97] + 1

listOf (n) times (t tuple) :
    for n do f to []
given : f = func(x) : x + [t]