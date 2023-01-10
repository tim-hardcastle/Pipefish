import

"lib/strings.ch"

def

toWords(s string):
    padBrackets(s) >> strings.split(that, " ")
given :
    padBrackets(s string) :
        strings.replaceAll(s, "(", " ( ") //>> strings.replaceAll(that, ")", " ) ")

