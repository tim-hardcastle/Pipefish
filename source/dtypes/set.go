package dtypes

import (
	"fmt"
)

type Set[E comparable] map[E]struct{}

func MakeFromSlice[E comparable](slice []E) Set[E] {
	S := Set[E]{}
	for _, v := range slice {
		S.Add(v)
	}
	return S
}

func From[E comparable](slice ... E) Set[E] {
	S := Set[E]{}
	for _, v := range slice {
		S.Add(v)
	}
	return S
}

func (S Set[E]) String() string {
	result := "{ "
	for e := range S {
		result += fmt.Sprintf("%v, ", e)
	}
	result = result[0:len(result)-1] + "}\n"
	return result
}

func (S Set[E]) ToSlice() []E {
	result := []E{}
	for e := range S {
		result = append(result, e)
	}
	return result
}

func (S Set[E]) IsEmpty() bool {
	return len(S) == 0
}

func (S Set[E]) Add(e E) Set[E] {
	S[e] = struct{}{}
	return S
}

func (S Set[E]) AddSet(T Set[E]) {
	for e := range T {
		S.Add(e)
	}
}

func (S Set[E]) SubtractSet(T Set[E]) Set[E] {
	U := make(Set[E], 0)
	for e := range S {
		if !T.Contains(e) {
			U.Add(e)
		}
	}
	return U
}

func (S Set[E]) OverlapsWith(T Set[E]) bool {
	for e := range T {
		if S.Contains(e) {
			return true
		}
	}
	return false
}

func (S Set[E]) Contains(e E) bool {
	_, found := S[e]
	return found
}

func (S Set[E]) GetArbitraryElement() (E, bool) {
	var result E
	var ok bool
	for e := range S { // There should be a less clumsy way to do this but ...
		result = e
		ok = true
		break
	}
	return result, ok
}
