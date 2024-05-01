# Tokenization rules

Here we give the exact rules for how to form a Pipefish identifier — the sequence of characters which names a variable, a function, a type, a struct's field, etc.

The purpose of these rules is to make things work as people would expect; to minimize suprisal.

We can divide Unicode characters into the following groups:

Whitespace. You can't use this in an identifier.

The "protected punctuation", the characters `(`, `)`, `[`, `]`, `{`, `}`, `,`, `;`, `:`, `.`, `"`, `` ` ``, `'` and `|`. You can't use any of these in an identifier.

Alphabetic characters together with `$` and `?`. You can use any of these in an identifier.

The numerals `0` ... `9`. You can use these in an identifier, but you can't use them to start an identifier.

So far this should look familiar from other languages, perhaps with a slightly different choice of protected punctuation. But we now have :

The underscore, `_`. You can use this in an identifier, but you can't use it at the start or the end of an identifier.

Symbols, consisting of everything else. You can use these in an identifier, but you can't use them as an identifier next to a number or an alphabetic character; but you can use them next to an underscore, or next to other symbols.

The upshot of this is that if people want to write a/b or x+1 they can do so and this is unambiguous, since the letters and numbers must belong to different identifiers than the symbols. *However*, it may sometimes be useful to qualify a symbol by a letter or word or vice-versa. At this point one can write for example `~_R`, using the `_` as a neutral bridge between symbol and letter.

## Note

A little experimentation may make you think that you can break these rules. For example if you define :

```
def

(x int) ~R (y int) :
    x == -y 
```

then this will compile and work almost exactly how you think it will. However, you have *not* in this case defined an infix operator `~R`: rather, the definition above is equivalent to:

```
def

(x int) ~ R (y int) :
    x == -y 
```

In the first version the actual underlying syntax and semantics are being disguised by writing the `~` and the `R` identifier without whitespace between them, but with or without whitespace a token boundary is there, and they are two different identifiers.

Doing this sort of thing is not encouraged.
