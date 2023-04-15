import

"lib/strings.ch"

def

MachineState = struct(stack list, defs map, vars map, mem list, output string, err string) 

Builtin = struct(paramCount int, function func)

STANDARD_DEFS = map("i"::["IX", "@"],
                 .. "?"::["@", "."],
                 .. "+!"::["swap", "over", "@", "+", "swap", "!"])

CLEAN_STATE = MachineState([], STANDARD_DEFS, map("IX"::0), [0], "", "")

CLEAN_OUTPUT = output::"", err::""

var

S = CLEAN_STATE

cmd

main :
    lineToExecute == "quit" :
        break
    else :
        S = lex(lineToExecute) -> interpret(that, S with CLEAN_OUTPUT)
        S[output] == "" and S[err] == "":
            respond "OK."
        S[output] == "" or S[err] == "":
            respond S[output] + S[err]
        else :
            respond S[output] + "\nForth error: " + S[err]
given :
    lineToExecute = request "Forth > "

def

// The lexer. Isn't Forth great?
lex(s) :
    s -> strings.replaceAll(that, "\n", " ") -> strings.replaceAll(that, "\t", " ") ..
  ..  -> strings.split(s, " ")

// The main body of the interpreter.
interpret(L, S) : 
    L == [] :
        S
    currentToken == "reset" :
        CLEAN_STATE with output::"OK"
    type int currentToken == int :
        interpret(tail, S with stack::S[stack] + [int currentToken])
    currentToken in keys BUILTINS :
        interpret(tail, evalBuiltin(currentToken, S))
    currentToken in keys S[vars] :
        interpret(tail, S with stack::S[stack] + [S[vars][currentToken]])
    currentToken in keys S[defs] :
        interpret(tail, interpret(S[defs][currentToken], S))  
    currentToken == "." :
        gatekeepStack(".", 1, S)
        interpret(tail, S with (output::S[output] + string(topOfStack) + " ", 
                             .. stack::stackTail))
    currentToken == "allot" :
        gatekeepStack("allot", 1, S)
        interpret(tail, S with (mem::S[mem] + zeroedList(topOfStack),
                             .. stack::stackTail))
    currentToken == ".\"" :
        interpret outputString(L, S)
    currentToken == "cr" :
        interpret(tail, S with output::S[output] + "\n")
    currentToken == "if" :
        interpret doIf(tail, S)
    currentToken == "do" :
        gatekeepStack("do ... loop", 2, S)
        interpret doLoop(tail, S)
    currentToken == "variable" :
        interpret makeVariable(tail, S)
    currentToken == "@" :
        gatekeepStack("@", 1, S)
        gatekeepMemory(topOfStack, S)
        interpret(tail, S with stack::(stackTail + [S[mem][topOfStack]]))
    currentToken == "!" :
        gatekeepStack("!", 2, S)
        gatekeepMemory(topOfStack, S)
        interpret(tail, S with stack::stackTail[0::-1],
                            .. [mem, topOfStack]::secondInStack)   
    currentToken == ":" :
        interpret getDefinition L[1::len(L)], S
    else :
        S with err::"FORTH doesn't understand '" + currentToken + "'."
given :
    currentToken = L[0]
    tail = L[1::len L]
    topOfStack = S[stack][len(S[stack]) - 1]
    secondInStack = S[stack][len(S[stack]) - 2]
    stackTail = S[stack][0::- 1]

//The various bits and pieces the interpreter calls.

getDefinition(L, S) :
    len(L) <= 1 or semicolonLocation == len L :
        [], S with err::"malformed definition."
    else :
        L[semicolonLocation + 1::len L], S with [defs, L[0]]::L[1::semicolonLocation]
given :
    semicolonLocation = findFirstOrEnd(L, func(x) : x == ";")

outputString(L, S) :
    len(L) == 0 or endQuoteLocation == len L :
        [], S with err::"malformed string."
    else :
        L[endQuoteLocation + 1::len L], S with output::S[output] + joinedStrings[0::-2]
given :
    endQuoteLocation = 1 + findFirstOrEnd(L[1::len(L)], func(x) : x[len(x) - 1] == "\"")
    joinedStrings = join(L[1::endQuoteLocation + 1])

doLoop(L, S) :
    len(L) < 1 or loopLocation == len L :
        [], S with err::"malformed 'do' clause."
    else :
        L[loopLocation + 1::len L], iterate(topOfStack, secondInStack, L[0::loopLocation], S with newStack)
given :
    loopLocation = findFirstOrEnd(L, func(x) : x == "loop")
    newStack = stack :: S[stack][0::-2]
    topOfStack = S[stack][len(S[stack]) - 1]
    secondInStack = S[stack][len(S[stack]) - 2]

iterate(startValue, endValue int, L list, S MachineState) :
    for i over startValue::endValue do iterator to S
given :
    iterator(S) : 
        interpret(L, S with [mem, 0]::i)

doIf(L, S) :
    len(S[stack]) == 0 :
        [], S with err::"can't apply 'if' when the stack is empty"
    len(L) < 1 or thenOrElseLocation == len L or thenLocation == len L:
        [], S with err::"malformed 'if' clause."
    conditionIsTrue :
        remainderOfTokens, (interpret(L[0::thenOrElseLocation], S with newStack))
    clauseHasElse : // Then the condition has failed so we must do the 'else' clause.
        remainderOfTokens, (interpret(L[thenOrElseLocation + 1::thenLocation], S with newStack))
    else :  // ... unless there is no 'else' clause.
        remainderOfTokens, S with newStack
given :
    thenOrElseLocation = findFirstOrEnd(L, func(x) : x == "then" or x == "else")
    thenLocation = findFirstOrEnd(L, func(x) : x == "then")
    clauseHasElse = L[thenOrElseLocation] == "else"
    conditionIsTrue = intToBool(S[stack][len(S[stack]) - 1])
    remainderOfTokens = L[thenLocation + 1::len L]
    newStack = stack::S[stack][0::-1]

makeVariable(L, S) :
    len L == 0 :
        [], S with err::"missing variable name."
    else :
        L[1::len L], S with ([vars, L[0]]::len(S[mem]), mem::S[mem] + [0])

evalBuiltin(builtinName, S) :
    type resultOfFunction == error : // Should only happen on division by zero.
        S with err::"FORTH doesn't know how to do that."
    else :
        S with stack::S[stack][0::len(S[stack]) - parameters] + [resultOfFunction]
given :
    parameters = BUILTINS[builtinName][paramCount]
    functionToApply = BUILTINS[builtinName][function]
    resultOfFunction = functionToApply(S[stack][len(S[stack]) - parameters::len(S[stack])])

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

// Bool to in translation.

boolToInt(b bool) :
    b : -1
    else : 0

intToBool(i int) :
    i != 0

// Partial conditionals for gatekeeping.

gatekeepStack(name string, parameters int, S) :
    len(S[stack]) < parameters :
        S with err::"can't apply '" + name + "' because there are " ..
                    .. + "too few numbers on the stack."

gatekeepMemory(address int, S) :
    address >= len(S[mem]) :
        S with err::"memory location " + string(address) + " is unallocated."

// And a few little functions to help with lists.

zeroedList(n int):
    for i over n do (func(L) : L + [0]) to []

findFirstOrEnd(L list, f func) :
    (while condition do action to 0)
given :
    condition(i) : i < len L and not (i < len L and f(L[i]))
    action(i) : i + 1

join(L list) :
    for i over len(L) do addWord to ""
given :
    addWord(s) : s + L[i] + " "