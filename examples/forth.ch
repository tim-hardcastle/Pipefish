import

"lib/strings.ch"

def

TokenType = enum NUMBER, STRING, IDENT

Token = struct(lit string, tokenType TokenType, lineNumber, start, end int)

tokenizeProgram(text) :
    strings.split(text, "\n") -> tokenizeLines

tokenizeLines(lines list) :
    for lineNo over 0::len(lines) do addLine to []
given :
    addLine(L) : L + tokenizeLine(lineNo, lines[lineNo])

tokenizeLine(lineNo, text) :
    (while notFinished do action to 0, [])[1]
given :
    notFinished(i, L) : i < len text 
    action(i, L) :
        text[i] == " " :
            i + 1, L 
        else : 
            newIndex, L + [newWord]
    given :
            newIndex, newWord = slurpToken(lineNo, i, text)

slurpToken(lineNo, startIndex, text) :
    endIndex, Token(word, classifyWord(word), lineNo, startIndex, endIndex)
given :
    endIndex, word = slurpWord(startIndex, text)

slurpWord(startIndex, text) :
    while notFinishedOrSpace do action to(startIndex, "")
given :
    notFinishedOrSpace(i, L) : not (i >= len text or text[i] == " ")
    action(i, s) : i + 1, s + text[i]

classifyWord(s string) :
    type(int s) != error :
        NUMBER
    else :
        IDENT

foo(s string, i int) :
    s, i
