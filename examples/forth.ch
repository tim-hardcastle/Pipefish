import

"lib/strings.ch"

def

lex(s) :
    s -> strings.replaceAll(that, "\n", " ") -> strings.replaceAll(that, "\t", " ") ..
  ..  -> strings.split(s, " ")

