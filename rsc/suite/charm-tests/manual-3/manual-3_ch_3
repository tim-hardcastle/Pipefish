snap: good
script: rsc/suite/manual-3.ch

-> Penguin "Gunther", NULL, NULL
Penguin with (name :: "Gunther", numberOfChicks :: NULL, favoriteFood :: NULL)
-> couldBeBool = true
ok
-> couldBeInt = NULL
ok
-> shortString = "Marmaduke"
ok
-> shortString = "Marmaduke Emmanuel Polkinghorne III"
error "attempting to assign object of type 'string' to a variable of type 'varchar(10)' (value supplied was "Marmaduke Emmanuel Polkinghorne III")"
-> nullToZero 42
42
-> nullToZero NULL
0
-> eval "2 + 2"
4
-> float64 99
99.000000
-> float64 "10.0"
10.000000
-> int 99.9
99
-> int "42"
42
-> set 1, 2, 3
set (1, 2, 3)
-> set (["foo", "bar"])
set ("foo", "bar")
-> literal 5
"5"
-> literal "Hello"
""Hello""
-> string 4
"4"
-> string true
"true"
-> string (1, 2, 3)
"1, 2, 3"
-> tuple 1, 2, 3
1, 2, 3
-> tuple 1
tuple (1)
-> tuple ([1, 2, 3])
tuple ([1, 2, 3])
-> tuplify ([1, 2, 3])
1, 2, 3
-> type 42
int
-> type "Hello"
string
-> type string
type
-> type type
type
-> type NULL
null
-> type null
type
-> "foo" in string
true
-> string in type
true