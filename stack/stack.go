package stack

type Stack[T comparable] struct {
    vals []T
}

func NewStack[T comparable]() *Stack[T] { return &Stack[T]{vals: []T{}} }

func (s *Stack[T]) Push(val T) {
    s.vals = append(s.vals, val)
}

func (s *Stack[T]) Pop() (T, bool) {
    if len(s.vals) == 0 {
        var zero T
        return zero, false
    }
    top := s.vals[len(s.vals)-1]
    s.vals = s.vals[:len(s.vals)-1]
    return top, true
}

func (s *Stack[T]) HeadValue() (T, bool) {
    if len(s.vals) == 0 {
        var zero T
        return zero, false
    }
    top := s.vals[len(s.vals)-1]
    return top, true
}

func (S Stack[T]) Find(e T) int {
    level := -1
    for i := len(S.vals)-1 ; i >= 0; i-- {
        level++
        if S.vals[i] == e {
            return level
        }
    }
    return -1
}

func ExplainWhitespaceStack(S *Stack[string]) string {
    output := "["
    for i := len((*S).vals)-1 ; i >= 0; i-- {
        if output != "[" {
            output = output + ", "
        }
        output = output + "'"
		output = output + ExplainWhitespace((*S).vals[i])
        output = output + "'"
    }
    output = output + "]"
    return output
}

func ExplainWhitespace(s string) string {
    explanation := ""
    for i := 0 ; i < len(s) ; i++ {
        switch s[i] {
        case '\n' : explanation = explanation + "\\n"
        case '\r' : explanation = explanation + "\\r"
        case '\t' : explanation = explanation + "\\t"
        case ' ' : explanation = explanation + "\\s"
        default : explanation = explanation + "*I can't explain that*"
        } 
    }
    return explanation
}