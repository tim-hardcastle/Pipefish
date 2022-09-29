import

golang "errors"
golang "strings"
golang "charm/object"

def

fancy (s string) : golang {
    return "**" + s + "**"
}

head (L list raw) : golang {
    if len(L.Elements) == 0 {
        return errors.New("list has no members")
    } else {
        return L.Elements[0]
    }
}

contains(haystack, needle string) : golang {
    return strings.Contains(haystack, needle)
}