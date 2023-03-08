import

gocode "errors"
gocode "strings"
gocode "charm/source/object"

def

fancy (s string) : gocode {
    return "**" + s + "**"
}

head (L list raw) : gocode {
    if len(L.Elements) == 0 {
        return errors.New("list has no members")
    } else {
        return L.Elements[0]
    }
}

contains(haystack, needle string) : gocode {
    return strings.Contains(haystack, needle)
}
