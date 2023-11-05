snap: good
script: rsc/suite/manual-3.ch

-> 2 + 2 == 4
true
-> "walrus" == "carpenter"
false
-> 2 + 2 == "walrus"
error "comparing values of type 'int' and 'string'"
-> 3 == NULL
error "comparing values of type 'int' and 'null'"
-> 1 < 2
true
-> 2 < 1
false
-> 2 <= 1
false
-> 1 <= 2
true
-> 6 * 7
42
-> 9 % 6
3
-> 10 / 3
3
-> -1 * (-1 + 1 + 1)
-1
-> -3 * -2
6
-> 3 == 4
false
-> 3 != 4
true
-> 5 - 8
-3
-> 2.0 * 2.0
4.000000
-> 8.0 / 5.0
1.600000
-> 2.0 + 5.0
7.000000
-> 2.0 - 5.0
-3.000000
-> 1.0 < 2.0
true
-> 2.0 < 2.0
false
-> 2.0 <= 2.0
true
-> 2.0 >= 2.0
true
-> 2.0 > 2.0
false
-> 2.0 == 2.0
true
-> 2.0 != 2.0
false
-> 6.0 * 0.5
3.000000
-> true and false
false
-> true and true
true
-> false and false
false
-> false and true
false
-> true or false
true
-> false or true
true
-> true or treu
true
-> true or 1/0
true
-> true or dksdfgkjd
true
-> false and troooo
false
-> false and 1/0
false
-> 2 + 2 == 3 : "Oops" ; 2 + 2 == 4 : "Yay" ; else : "Errr?"
"Yay"
-> "mac" + "hine"
"machine"
-> len "walrus"
6
-> "Charm"[0]
"C"
-> "Charm"[4]
"m"
-> "Charm"[1::4]
"har"
-> NULL == NULL
true
-> NULL != NULL
false
-> 0b10011
19
-> 0o23
19
-> 0x13
19
-> 42::"foo"
42::"foo"
-> ("walrus"::true)[0]
"walrus"
-> ["a", "b", "c", "d"][1]
"b"
-> ["a", "b", "c", "d"][1::3]
["b", "c"]
-> len ["a", "b", "c", "d"]
4
-> "a" in ["a", "b", "c", "d"]
true
-> "z" in ["a", "b", "c", "d"]
false
-> ["a", "b"] + ["c", "d"]
["a", "b", "c", "d"]
-> len {1, 2, 3}
3
-> 1 in {1, 2, 3}
true
-> 99 in {1, 2, 3}
false
-> (map "foo"::"bar", 42::"walrus")[42]
"walrus"
-> len (map "foo"::"bar", 42::"walrus")
2
-> len keys (map "foo"::"bar", 42::"walrus")
2
-> ((1, (2, 3)), ((4)))
1, 2, 3, 4
-> arity 1, 2, 3
3
-> len "banana"
6
-> arity "banana"
1
-> ["a", "b", "c", ["d", "e"]] with 1::"x"
["a", "x", "c", ["d", "e"]]
-> ["a", "b", "c", ["d", "e"]] with 1::"x", 2::"y"
["a", "x", "y", ["d", "e"]]
-> ["a", "b", "c", ["d", "e"]] with [3, 1]::"z"
["a", "b", "c", ["d", "z"]]
-> len (map(1::"a", 2::"b", 3::"c") without 2)
2
-> len (map(1::"a", 2::"b", 3::"c") without 1, 3)
1