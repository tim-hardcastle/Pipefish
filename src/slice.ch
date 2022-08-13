import "lib/prelude.ch"

def // We'll make some data types and data to demonstrate:

Cat = struct(name string, age int)
Person = struct(name string, age, income int)

pete = Person "Pete", 42, 50000 // Two flavors of constructor as needed.
tibbles = Cat with name :: "Tibbles", age :: 6
arbitraryMap = map(name :: "Foo", age :: 19, "bar" :: "troz")

// Now the clever bit :

(L list) slice (key) :
    L apply (func(x) : x[key])

// And now let's overload 'slice' to take a list of keys :

(L list) slice (keys list) :
    L apply (func(x) : mappify (x, keys))

mappify(S, keys) :
    (for len(keys) do action to 0, map())[1]
given:
    action(i, M) : i + 1, (M with keys[i] :: S[keys[i]])
