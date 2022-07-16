def

twice(x string) : x + x

twice(x int) : 2 * x

twice(b bool) :
    b : "That's as true as things get!"
    else : "That's as false as things get!"

swap(x, y) : y, x

sign(n) :
    n > 0 : "positive"
    n == 0 : "zero"
    else : "negative"

(x) squared : x * x

(x) times (y) : x * y

(x) divides (y) :
    y % x == 0

(x) is even:
    2 divides x

say (x) nicely :
    "*~*" + x + "*~*"

gcd (x, y) :
    x < y : gcd(y, x)
    y divides x : y
    else : gcd(y, x % y)
