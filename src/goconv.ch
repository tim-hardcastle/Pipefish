import

"lib/prelude.ch" :: ""
"lib/strings.ch"
"lib/math.ch"
"lib/fmt.ch"

def

process(filename string) :
    prettyPrint(rewrite(file filename)[contents])

rewrite(L list) : 
    L apply deReturn apply de64 apply makeFn
given :
    deReturn(s) : s[0::len(s) - 8]
    de64(s) : strings.replaceAll(s, "64", "")
    makeFn(s) : strings.toLower(s[0]) + s[1::len(s)] + " : golang {\n    " + ..
        .. "return math." + strings.replaceAll(s, " float64", "") + "\n}\n"

prettyPrint(L):
    (for len(L) do f to 0, "")[1]
given:
    f(i, s) : i + 1, s + L[i] + "\n"


bar (x int) : golang {
    if (x % 2) == 0 {
        return x, true
    }
    return 42, false
}



