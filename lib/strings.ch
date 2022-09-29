import

golang "strings"

def

compare(a, b string) : golang {
   return strings.Compare(a, b)
}

contains(s, substr string) : golang {
   return strings.Contains(s, substr)
}

containsAny(s, chars string) : golang {
    return strings.ContainsAny(s, chars)
}

count(s, substr string) : golang {
    return strings.Count(s, substr)
}

cut(s, sep string) : golang {
    return strings.Cut(s, sep)
}

equalFold(s, t string) : golang {
    return strings.EqualFold(s, t)
}

fields(s string) : golang {
    return strings.Fields(s)
}

hasPrefix(s, prefix string) : golang {
    return strings.HasPrefix(s, prefix)
}

hasSuffix(s, suffix string) : golang {
    return strings.HasSuffix(s, suffix)
}

index(s, substr string) : golang {
    return strings.Index(s, substr)
}

indexAny(s, chars string) : golang {
    return strings.IndexAny(s, chars)
}

lastIndex(s, substr string) : golang {
    return strings.LastIndex(s, substr)
}

lastIndexAny(s, chars string) : golang {
    return strings.LastIndexAny(s, chars)
}

repeat(s string, c int) : golang {
    return strings.Repeat(s, c)
}

replace(s string, old string, new string, n int) : golang {
    return strings.Replace(s, old, new, n)
}

replaceAll(s string, old string, new string) : golang {
    return strings.ReplaceAll(s, old, new)
}

split(s, sep string) : golang {
    return strings.Split(s, sep)
}

splitAfter(s, sep string) : golang {
    return strings.SplitAfter(s, sep)
}

splitAfterN(s string, sep string, n int) : golang {
    return strings.SplitAfterN(s, sep, n)
}

splitN(s string, sep string, n int) : golang {
    return strings.SplitN(s, sep, n)
}

toLower(s string) : golang {
    return strings.ToLower(s)
}

toTitle(s string) : golang {
    return strings.ToTitle(s)
}

toUpper(s string) : golang {
    return strings.ToUpper(s)
}

ToValidUTF8(s, replacementString string) : golang {
    return strings.ToValidUTF8(s, replacementString)
}

trim(s, cutset string) : golang {
    return strings.Trim(s, cutset)
}

trimLeft(s, cutset string) : golang {
    return strings.TrimLeft(s, cutset)
}

trimPrefix(s, prefix string) : golang {
    return strings.TrimPrefix(s, prefix)
}

trimRight(s, cutset string) : golang {
    return strings.TrimRight(s, cutset)
}

TrimSuffix(s, prefix string) : golang {
    return strings.TrimPrefix(s, prefix)
}
