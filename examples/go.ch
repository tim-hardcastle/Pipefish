import

gofunc "errors"
gofunc "strings"
gofunc "charm/source/object"

def

fancy (s string) : gofunc {
    return "**" + s + "**"
}

head (L list raw) : gofunc {
    if len(L.Elements) == 0 {
        return errors.New("list has no members")
    } else {
        return L.Elements[0]
    }
}

contains(haystack, needle string) : gofunc {
    return strings.Contains(haystack, needle)
}
