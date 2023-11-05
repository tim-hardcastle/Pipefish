snap: good
script: rsc/suite/manual-3.ch

-> doug
Person with (name::"Douglas", age::42)
-> me
CatOwner with (name::"Tim", pet::Cat with (name::"Felix", nobelPrizes::0, pink::false))
-> doug[age]
42
-> me[pet][pink]
false
-> doug with age::43
Person with (name::"Douglas", age::43)
-> me with [pet, pink]::true
CatOwner with (name::"Tim", pet::Cat with (name::"Felix", nobelPrizes::0, pink::true))
-> joe with name::"Josephine", age::23
Person with (name::"Josephine", age::23)
-> Card ACE, CLUBS
Card with (value::ACE, suit::CLUBS)
-> isBlack CLUBS
true
-> isBlack Card QUEEN, HEARTS
false
-> type DIAMONDS
Suit
-> len Suit
4