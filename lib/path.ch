import

gocode "path"
gocode "errors"

def

base(p string) : gocode {
    return path.Base(p)
}

clean(p string) : gocode {
    return path.Clean(p)
}

dir(p string)  : gocode {
    return path.Dir(p)
}

ext(p string)  : gocode {
    return path.Ext(p)
}

isAbs(p string)  : gocode {
    return path.IsAbs(p)
}

join(t tuple)  : gocode {
    strings := []string{}
    for _, v := range(t) {
        switch v := v.(type) {
        case string :
            strings = append(strings, v)
        default :
            return errors.New("'join' function in 'path' library passed non-string type")
        }
    }
    return path.Join(strings...)
}

match(pattern, name string)  : gocode {
    m, err := path.Match(pattern, name)
    if err != nil {
        return err
    }
    return m
}

split(p string)  : gocode {
    return path.Split(p)
}