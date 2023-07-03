def

// A type definition.
Person = struct(name varchar(10), age int)    

// A list constant.
PERSON_LIST = [Person("Joe", 22), Person("Doug", 42)]

// Constants are computed on initialization.
// Order of declaration is free ...
NAME_LIST = slice PERSON_LIST on name

// ... so let's define the function we just called.
slice (L list) on (key) :      // Dynamism FTW.
    L >> that[key]             // A `map` operator.

// On initialization, NAME_LIST will be set to ["Joe", "Doug"]

foo(x string) :
    x

bar(y) :
    y
