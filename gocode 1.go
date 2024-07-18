package main

import (
    "pipefish/source/values"
)

func Boo(args ...any) any {

    x := args[0].(bool)

    return tuplify(!x)
}

func Foo(args ...any) any {

    x := args[0].(float64)

    return tuplify(x)
}

func Ioo(args ...any) any {

    x := int(args[0].(int))

    return tuplify(double(x))
}

func Noo(args ...any) any {


    return tuplify(nil)
}

func Roo(args ...any) any {

    x := args[0].(values.Value)

    return tuplify(x)
}

func Soo(args ...any) any {

    x := args[0].(string)

    return tuplify(x)
}

func ConstructPerson(args ...any) any {

    aName := args[0].(string)
    anAge := int(args[1].(int))

    return tuplify(Person{name: aName, age: anAge})
}

func DeconstructPerson(args ...any) any {

    aPerson := args[0].(Person)

    return tuplify(aPerson.name, aPerson.age)
}



    func double(x int) int {
        return 2 * x
    }

func tuplify(args ...any) any {
	if len(args) == 1 {
		return args[0]
	}
	result := &values.GoReturn{Elements: []any{}}
	for _, v := range(args) {
		result.Elements = append(result.Elements, v)
	}
	return result
}

type Person struct {
	name string
	age int
}

func ConvertGoStructHalfwayToPipefish(v any) (uint32, []any, bool) {
	switch v.(type) {
	case Person : 
		return uint32(36), []any{v.(Person).name, v.(Person).age}, true
	default:
		return uint32(0), []any{}, false
	}
}


func ConvertPipefishStructToGoStruct(T uint32, args []any) any {
	switch T {
	case 36 : 
		return Person{args[0].(string), args[1].(int)}
	default:
		panic("I'm not sure if this error can arise.")
	}
}

