import

gofunc "strings"

def

compare(a, b string) : gofunc {
   return strings.Compare(a, b)
}

contains(s, substr string) : gofunc {
   return strings.Contains(s, substr)
}

containsAny(s, chars string) : gofunc {
    return strings.ContainsAny(s, chars)
}

count(s, substr string) : gofunc {
    return strings.Count(s, substr)
}

cut(s, sep string) : gofunc {
    return strings.Cut(s, sep)
}

equalFold(s, t string) : gofunc {
    return strings.EqualFold(s, t)
}

fields(s string) : gofunc {
    return strings.Fields(s)
}

hasPrefix(s, prefix string) : gofunc {
    return strings.HasPrefix(s, prefix)
}

hasSuffix(s, suffix string) : gofunc {
    return strings.HasSuffix(s, suffix)
}

index(s, substr string) : gofunc {
    return strings.Index(s, substr)
}

indexAny(s, chars string) : gofunc {
    return strings.IndexAny(s, chars)
}

lastIndex(s, substr string) : gofunc {
    return strings.LastIndex(s, substr)
}

lastIndexAny(s, chars string) : gofunc {
    return strings.LastIndexAny(s, chars)
}

repeat(s string, c int) : gofunc {
    return strings.Repeat(s, c)
}

replace(s string, old string, new string, n int) : gofunc {
    return strings.Replace(s, old, new, n)
}

replaceAll(s string, old string, new string) : gofunc {
    return strings.ReplaceAll(s, old, new)
}

split(s string, sep string) : gofunc {
    return strings.Split(s, sep)
}

splitAfter(s string, sep string) : gofunc {
    return strings.SplitAfter(s, sep)
}

splitAfterN(s string, sep string, n int) : gofunc {
    return strings.SplitAfterN(s, sep, n)
}

splitN(s string, sep string, n int) : gofunc {
    return strings.SplitN(s, sep, n)
}

toLower(s string) : gofunc {
    return strings.ToLower(s)
}

toTitle(s string) : gofunc {
    return strings.ToTitle(s)
}

toUpper(s string) : gofunc {
    return strings.ToUpper(s)
}

ToValidUTF8(s, replacementString string) : gofunc {
    return strings.ToValidUTF8(s, replacementString)
}

trim(s, cutset string) : gofunc {
    return strings.Trim(s, cutset)
}

trimLeft(s, cutset string) : gofunc {
    return strings.TrimLeft(s, cutset)
}

trimPrefix(s, prefix string) : gofunc {
    return strings.TrimPrefix(s, prefix)
}

trimRight(s, cutset string) : gofunc {
    return strings.TrimRight(s, cutset)
}

TrimSuffix(s, prefix string) : gofunc {
    return strings.TrimPrefix(s, prefix)
}
