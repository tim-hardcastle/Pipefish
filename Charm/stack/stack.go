package stack

import (
	"fmt"
)


type Stack []string;

func (S *Stack) Push(value string) {
	(*S) = append((*S), value)
}

func (S *Stack) Pop() error {
	if len(*S) > 0 {
		(*S) = (*S)[:len(*S)-1]
		return fmt.Errorf("")
	}
	return fmt.Errorf("Pop Error: Stack is empty")
}

func (S Stack) HeadValue() string {
	return (S)[len(S) - 1]
}

func (S Stack) Find(e string) int {
	level := -1
	for i := len(S)-1 ; i >= 0; i-- {
		level++
		if (S)[i] == e {
			return level
		}
	}
    return -1
}


func (S *Stack) ExplainWhitespaceStack() string {
	output := "["
	for i := len(*S)-1 ; i >= 0; i-- {
		if output != "[" {
			output = output + ", "
		}
		output = output + "'"+ ExplainWhitespace((*S)[i])+"'"
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