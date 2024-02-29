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
    currentLiteral in keys BUILTINS :
        evaluate(codeTail, evalBuiltin(currentLiteral, S))
    else :
        error "can't evaluate '" + currentLiteral + "'"
given :
    currentToken = L[0]
    currentLiteral = currentToken[lit]
    currentType = currentToken[tokenType]
    codeTail = L[1::len L]

// Builtins

Builtin = struct(paramCount int, function func)

BUILTINS = map("+"::Builtin(2, func(x): x[0] + x[1]), 
            .. "-"::Builtin(2, func(x): x[0] - x[1]),
            .. "*"::Builtin(2, func(x): x[0] * x[1]),
            .. "/"::Builtin(2, func(x): x[0] / x[1]),
            .. "mod"::Builtin(2, func(x): x[0] % x[1]),
            .. "negate"::Builtin(1, func(x): - x[0]),
            .. "cells"::Builtin(1, func(x): x[0]),
            .. "="::Builtin(2, func(x): boolToInt(x[0] == x[1])),
            .. "<"::Builtin(2, func(x): boolToInt(x[0] < x[1])),
            .. ">"::Builtin(2, func(x): boolToInt(x[0] > x[1])),
            .. "<="::Builtin(2, func(x): boolToInt(x[0] <= x[1])),
            .. ">="::Builtin(2, func(x): boolToInt(x[0] >= x[1])),
            .. "and"::Builtin(2, func(x): boolToInt(intToBool(x[0]) and intToBool(x[1]))),
            .. "or"::Builtin(2, func(x): boolToInt(intToBool(x[0]) or intToBool(x[1]))),
            .. "invert"::Builtin(1, func(x): boolToInt(not intToBool(x[0]))),
            .. "dup"::Builtin(1, func(x): x[0], x[0]),
            .. "swap"::Builtin(2, func(x): x[1], x[0]),
            .. "over"::Builtin(2, func(x): x[0], x[1], x[0]),
            .. "rot"::Builtin(3, func(x): x[1], x[2], x[0]),
            .. "drop"::Builtin(1, func(x): ()))

evalBuiltin(builtinName, S) :
    S with stack::S[stack][0::len(S[stack]) - parameters] + [resultOfFunction]
given :
    parameters = BUILTINS[builtinName][paramCount]
    functionToApply = BUILTINS[builtinName][function]
    resultOfFunction = functionToApply(S[stack][len(S[stack]) - parameters::len(S[stack])])

// Bool-int translation.

boolToInt(b bool) :
    b : -1
    else : 0

intToBool(i int) :
    i != 0

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
