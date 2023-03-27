# Writing a Forth in Charm

Implementing a small language is a good test of a larger language. Forth has a pleasantly small specification, in that of the three usual steps in an intepreted language --- lexer, parser, evaluator --- the lexer may be reduced to a couple of lines and the parser is unnecessary.

The lexer of a language breaks it down into its semantic parts, for example in Charm it identifies  `("foo"::42)` as being made up of the parts `(`, `"foo"` `::`, `42`, and `)`.

In Forth the rules are much simpler: the tokens of the language are separated by whitespace, and anything can be a valid token. This means that a lexer for Forth just looks like this:

```
import

"lib/strings.ch"

def

lex(s) :
    s -> strings.replaceAll(that, "\n", " ") -> strings.replaceAll(that, "\t", " ") ..
  ..  -> strings.split(s, " ")
```

