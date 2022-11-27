def

Suit = enum CLUBS, HEARTS, SPADES, DIAMONDS

Value = enum ACE, TWO, THREE, FOUR, FIVE, SIX, SEVEN, 
          .. EIGHT, NINE, TEN, JACK, QUEEN, KING

Card = struct(value Value, suit Suit)

isBlack(suit Suit) : suit in {CLUBS, SPADES}

isBlack(card Card) : isBlack(card[suit])


