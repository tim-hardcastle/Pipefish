snap: good
script: rsc/suite/manual-4.ch

-> square 5
25
-> parity 8
"even"
-> swap "foo", 2
2, "foo"
-> classify -9
"small negative number"
-> 7 squared
49
-> 6 times 7
42
-> 6 divides 7
false
-> 8 is even
true
-> say "hello" nicely
"*~*hello*~*"
-> divide 8 by 3
2
-> twice "foo"
"foo, foo"
-> twice 5
10
-> twice true
"That's as true as things get!"
-> twice NULL
error "I don't know how to double that!"
-> twice 1, 2, 3
1, 2, 3, 1, 2, 3
-> string (ONE_DOLLAR + TREE_FIDDY)
"$4.50"
-> g 2
24
-> happyAddition 4, 5
"*!* 9 *!*"
-> foo 5
180
-> triangularNumber 10
55
-> betterTriangularNumber 10
55
-> shorterTriangularNumber 10
55
-> [1, 2, 3, 4, 5] >> triangularNumber
[1, 3, 6, 10, 15]
-> 3 -> that + 5 -> that * that
64
-> [1, 2, 3, 4, 5] >> that + 5 >> that * that
[36, 49, 64, 81, 100]
-> [1, 2, 3, 4, 5] ?> that % 2 == 1
[1, 3, 5]