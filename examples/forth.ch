import

"lib/strings.ch"

def

ex(text string) :
    text -> tokenizeProgram -> evaluate(that, CLEAN_STATE) 

// Evaluator

Error = struct(errorMessage string, token Token)

ForthMachine = struct(stack list, err Error?, vars map, mem list)

CLEAN_STATE = ForthMachine with stack::[], err::NULL, vars::map(), mem::[]

KEYWORDS = "variable"::0, "!"::2, "?"::1, "allot"::1, "if"::1, "do"::2

evaluate(L list, S ForthMachine) : 
    L == [] or S[err] in Error:
        S
    currentType == NUMBER :
        evaluate(codeTail, S with stack::S[stack] + [int currentLiteral])
    currentType == KEYWORD and len S[stack] < KEYWORDS[currentLiteral] :
        S with err::Error("stack underflow", currentToken)
    currentLiteral == "variable" :
        makeVariable(codeTail, S, token) -> evaluate
    currentLiteral == "!" :
        setVariable(S, currentToken) -> evaluate(codeTail, that)
    currentLiteral == "@" :
        getVariable(S, currentToken) -> evaluate(codeTail, that)
    currentLiteral == "allot" :
        allotMemory(S) -> evaluate(codeTail, that)
    currentLiteral == "if" :
        doIf(codeTail, S) -> evaluate
    currentLiteral == "do" :
        doLoop(codeTail, S) -> evaluate
    currentLiteral in keys BUILTINS :
        evalBuiltin(S, currentToken) -> evaluate(codeTail, that)   
    currentLiteral in keys S[vars] :
        evaluate(codeTail, S with stack::S[stack] + [S[vars][currentLiteral]])
    else :
        S with err::Error("unknown identifier error", currentToken)
given :
    currentToken = L[0]
    currentLiteral = currentToken[lit]
    currentType = currentToken[tokenType]
    codeTail = L[1::len L]

// Variables

makeVariable(L, S, token) :
    len L == 0 :
        [], S with err::Error("missing variable name", token)
    ((L[0])[tokenType]) != IDENT :
        [], S with err::Error("variable name should be identifier", token)
    else :
        L[1::len L], S with [vars, L[0][lit]]::len(S[mem]), mem::S[mem] + [0]

setVariable(S, token) :
    len S[mem] <= stackTop :
        S with err::Error("memory location " + (string stackTop) + " unreserved", token)
    else :
        S with stack::S[stack][0::len(S[stack]) - 2],
            .. [mem, stackTop]::S[stack][len(S[stack]) - 2] 
given :
    stackTop = S[stack][len(S[stack]) - 1]

getVariable(S, token) :
    len S[mem] <= stackTop :
        S with err::Error("memory location " + (string stackTop) + " unreserved", token)
    else :
        S with stack::stackTail + [S[mem][stackTop]]
given :
    stackTop = S[stack][len(S[stack]) - 1]
    stackTail = S[stack][0::len(S[stack]) - 1]

allotMemory(S) : 
    S with mem::S[mem] + (range(0::stackTop) >> [0] -> sum),
        .. stack::stackTail
given :
    stackTop = S[stack][len(S[stack]) - 1]
    stackTail = S[stack][0::len(S[stack]) - 1]

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

evalBuiltin(S, builtinToken) :
    len S[stack] < parameters :
        S with err::Error("stack underflow", builtinToken)
    builtinName == "/" and S[stack][len(S[stack]) - 1] == 0 :
        S with err::Error("division by zero", builtinToken)
    else :
        S with stack::S[stack][0::len(S[stack]) - parameters] + [resultOfFunction]
given :
    builtinName = builtinToken[lit]
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
    s in keys KEYWORDS :
        KEYWORD
    else :
        IDENT
