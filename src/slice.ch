import

"lib/prelude.ch" // This is where the 'for' loop comes from.
// There's not many standard iterative functions yet.

def

// We'll make some data types and data to demonstrate:

Cat = struct(name string, age int)

Person = struct(name string, age, income int)

pete = Person "Pete", 42, 50000

tibbles = Cat with name :: "Tibbles", age :: 6

arbitraryMap = map(name :: "Foo", age :: 19, "bar" :: "troz")

// Now the clever bit :

(L list) slice (key):
    (for len(L) do action to 0, [])[1]
given:
    action(i, R): i + 1, R + [L[i][key]]

// Let's overload 'slice' to take a list of keys :

(L list) slice (keys list):
    (for len(L) do applyMappify to 0, [])[1]
given:
    applyMappify(i, R list): i + 1, R + [mappify(keys, map())]
    given :
        mappify(k, M) :
            k : this k behead 1, (M with k[0] :: L[i][k[0]])
            else : M

