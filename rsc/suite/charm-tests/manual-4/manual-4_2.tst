snap: good
script: rsc/suite/manual-4.ch

-> rotateLeft 1, 2, 3
2, 3, 1
-> rotateLeft ()
()
-> rotate RIGHT, 1, 2, 3
3, 1, 2
-> rotate 1, 2, 3 to RIGHT
3, 1, 2
-> rotateLeft ((swap 1, 2) , (swap 3, 4))
1, 4, 3, 2
-> zort
"No parameters"
-> zort 1
"Single"
-> zort 1, 2, 3
"Single, then tuple"
-> zort ()
"Tuple"
-> safeNull 1, 2, 3, NULL
NULL
-> safeNull 1, 2, 3
error "unsatisfied conditional"
-> safeAdd(7, NULL)
NULL
-> safeAdd(NULL, 8)
NULL
-> safeAdd(7, 8)
15
-> 7 modC 0
error "taking the modulus of a number by zero"
-> 7 modD 0
"this isn't an error, just a friendly warning"
-> type (1/0)
error
-> (1 / 0)[errorMessage]
"division by zero"
-> FACTORIAL 5
120