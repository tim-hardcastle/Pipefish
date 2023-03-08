import

gofunc "fmt"

def

errorf(format string, a tuple) : gofunc {
    return fmt.Errorf(format, a...)
}

sprint(a tuple) : gofunc {
    return fmt.Sprint(a...)
}

sprintf(format string, a tuple) : gofunc {
    return fmt.Sprintf(format, a...)
}

sprintln(a tuple) : gofunc {
    return fmt.Sprintln(a...)
}
