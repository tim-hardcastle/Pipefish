def

Money = struct(dollars, cents int)

ONE_DOLLAR = Money(1, 0)
TREE_FIDDY = Money(3, 50)

(m Money) + (n Money) :
    m[cents] + n[cents] >= 100 :
        Money(m[dollars] + n[dollars] + 1, m[cents] + n[cents] - 100)
    else :
        Money(m[dollars] + n[dollars], m[cents] + n[cents])

string(m Money) :
    "$" + string(m[dollars]) + "." .. 
     .. + (m[cents] < 10 : "0"; else : "") ..
     .. + string(m[cents])


