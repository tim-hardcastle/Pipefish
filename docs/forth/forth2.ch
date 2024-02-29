import

"lib/strings.pf"

def

ex(text string) :
    text -> tokenizeProgram -> evaluate(that, CLEAN_STATE) 

// Evaluator

ForthMachine = struct(stack list)

CLEAN_STATE = ForthMachine([])

evaluate(L list, S ForthMachine) : 
    L == [] :
        S
    currentType == NUMBER :
            evaluate(codeTail, S with stack::S[stack] + [int currentLiteral])
    else :
        error "can't evaluate '" + currentLiteral + "'"
given :
    currentToken = L[0]
    currentLiteral = currentToken[lit]
    currentType = currentToken[tokenType]
    codeTail = L[1::len L]

// Tokenizer

TokenType = enum NUMBER, STRING, IDENT, KEYWORD

Token = struct(lit string, tokenType TokenType, lineNumber, start, end int)

tokenizeProgram(program string) :
    strings.split(program, "\n") -> tokenizeLines

tokenizeLines(lines list) :
    range(0::len lines) >> tokenizeLine(that + 1, lines[that]) -> sum

tokenizeLine(lineNo, line) :
    (while notFinished do addToken to [], 0)[0]
given :
    notFinished(L, i) : i < len line 
    addToken(L, i) :
        line[i] == " " or line == "\t" :
            L, i + 1 
        else : 
            L + [newToken], newToken[end]
    given :
            newToken = slurpToken(lineNo, line, i)

slurpToken(lineNo, line, startIndex) :
    Token(word, classifyWord(word), lineNo, startIndex, endIndex)
given :
    word, endIndex = slurpWord(line, startIndex)

slurpWord(line, startIndex) :
    while notFinishedOrSpace do addOneLetter to ("", startIndex)
given :
    notFinishedOrSpace(L, i) : not (i >= len line or line[i] == " ")
    addOneLetter(s, i) : s + line[i], i + 1

classifyWord(s string) :
    type(int s) != error :
        NUMBER
    else :
        IDENT
