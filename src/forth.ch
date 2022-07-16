import

"lib/prelude.ch"

var private

S state = initialState

cmd

ex (s string):
    S = evaluate S with code :: tokenize(s) with err :: "" with output :: ""
    return forthOutput(S)

show (l label):
    return S[l]

showall:
    return S

clear (l label):
    S = S with l :: (initialState[l])

clearall:
    S = initialState

def

state = struct(code, stack, mem list, defs, vars, consts map, output, err string, loopVar any)

forthOutput(S state) :
    S[err] != "" : S[err]
    S[output] == "" : "ok"
    else : S[output]

standardDefs = map ( "?" :: ["@", "."], "+!" :: ["dup", "rot", "swap", "@", "+", "swap", "!"] )

initialState = state([], [], [], standardDefs, map(), map(), "", "", NIL)

evaluate(S state) :
    S[err] != "" : S
    len(S[code]) == 0 : S
    type(int firstInstruction) != error :   evaluate evalNumber S
    type(builtins[firstInstruction]) != error :   evaluate evalBuiltin S
    firstInstruction == ":" :   evaluate evalDefinition S
    type(S[defs][firstInstruction]) != error :   evaluate evalDefCall S
    firstInstruction == "." :   evaluate evalOutput S
    firstInstruction == "emit" : evaluate evalEmit S
    firstInstruction == "cr" :   evaluate evalCR S
    firstInstruction == ".\"" : evaluate evalStringLiteral S
    firstInstruction == "if" : evaluate evalConditional S
    firstInstruction == "do" : evaluate evalDoLoop S
    firstInstruction == "i" : evaluate evalPushIndex S
    firstInstruction == "variable" :   evaluate evalVarDeclaration S
    firstInstruction == "constant" :   evaluate evalConstDeclaration S
    firstInstruction == "@" :   evaluate evalGetVar S
    firstInstruction == "!" :   evaluate evalPutVar S
    type(S[vars][firstInstruction]) != error :   evaluate evalVarReference S
    type(S[consts][firstInstruction]) != error :   evaluate evalConstReference S
    else : S with err :: "FORTH can't interpret " + firstInstruction
given :
    firstInstruction = S[code][0]

evalNumber(S) :
    S  with code :: tail(S[code]) ..
    .. with stack :: (S[stack] + [int S[code][0]])

evalBuiltin(S) :
    stackSize <  operandCount:
        error "FORTH stack underflow"
    else : 
        S with (code :: tail S[code]) ..
        .. with (stack :: operationToApply(S[stack]))
given :
    operandCount = builtins[S[code][0]][plicity]
    operationToApply = builtins[S[code][0]][operation]
    stackSize = len(S[stack])

btin = struct(plicity int, operation)

cellWidth = 1

builtins = map "+" :: btin(2, func(L) : (L curtail 2) + [(L[len(L) - 2] + L[len(L) - 1])]) ,
            .. "*" :: btin(2, func(L) : (L curtail 2) + [(L[len(L) - 2] * L[len(L) - 1])]) ,
            .. "-" :: btin(2, func(L) : (L curtail 2) + [(L[len(L) - 2] - L[len(L) - 1])]) ,
            .. "/" :: btin(2, func(L) : (L curtail 2) + [(L[len(L) - 2] / L[len(L) - 1])]) ,
            .. "mod" :: btin(2, func(L) : (L curtail 2) + [(L[len(L) - 2] % L[len(L) - 1])]) ,
            .. "dup" :: btin(1, func(L) : L + [L[len(L) - 1]]) ,
            .. "drop" :: btin(1, func(L) : L curtail 1) ,
            .. "swap" :: btin(2, func(L) : (L curtail 2) + [L[len(L) - 1], L[len(L) - 2]]) ,
            .. "over" :: btin(2, func(L) : L + [L[len(L) - 2]]) ,
            .. "rot" :: btin(3, func(L) : (L curtail 3) + [L[len(L) - 2], L[len(L) - 1], L[len(L) - 3]]) ,
            .. "cells" :: btin(1, func(L) : (L curtail 1) + [cellWidth * L[len(L) - 1]] ) ,
            .. "=" :: btin(2, func(L) : (L curtail 2) + [forthTruth(L[len(L) - 2] == L[len(L) - 1])] ) ,
            .. "<" :: btin(2, func(L) : (L curtail 2) + [forthTruth(L[len(L) - 2] < L[len(L) - 1])] ) ,
            .. ">" :: btin(2, func(L) : (L curtail 2) + [forthTruth(L[len(L) - 2] > L[len(L) - 1])] ) ,
            .. "and" :: btin(2, func(L) : (L curtail 2) + ..
                                        .. [forthTruth(L[len(L) - 2] != 0 and L[len(L) - 1] != 0)] ) ,
            .. "or" :: btin(2, func(L) : (L curtail 2) + ..
                                        .. [forthTruth(L[len(L) - 2] != 0 or L[len(L) - 1] != 0)] ) ,
            .. "invert" :: btin(1, func(L) : (L curtail 1) + ..
                                        .. [forthTruth(L[len(L) - 1] == 0)] )

evalDefinition(S) :
    (len S[code]) < 3 : S with err :: "FORTH error: incomplete definition at end of program."
    else : S with (code :: (S[code] behead (len defBody) + 3)) ..
          .. with (defs :: (S[defs] with newDef))
given : 
    defBody = extractDefinition(S[code][2::len(S[code])])
    newDef = S[code][1] :: defBody

extractDefinition(C list) :
    extracter(C, [])
given :
    extracter = func(C, R) :
        not C : S with err :: "FORTH error: definition unterminated by ;"
        C[0] == ";" : R
        else : this(tail(C), R + [C[0]])

evalDefCall(S) :
    evaluate(S with code :: S[defs][S[code][0]]) with code :: tail S[code]

evalOutput(S) :
    S with code :: tail(S[code]) ..
    .. with stack :: (S[stack] curtail 1) ..
    .. with output :: S[output] + string(S[stack][len(S[stack]) - 1]) + " "

evalEmit(S) :
    S with code :: tail(S[code]) ..
    .. with stack :: (S[stack] curtail 1) ..
    .. with output :: S[output] + rune(S[stack][len(S[stack]) - 1])

evalCR(S) :
    S with code :: tail(S[code]) ..
    .. with output :: S[output] + "\n"

evalStringLiteral(S) :
    length == -1 :
        S with err :: "FORTH error: unterminated string literal"
    else : 
        S with code :: (S[code] behead length + 1) ..
        .. with output :: S[output] + lit
given:
    counter = func(C, n) :
        len(C) == 0 : -1
        C[0][len(C[0]) - 1] == `"` : n + 1
        else : this(tail(C), n + 1)
    length = counter(tail(S[code]), 0)
    lit = join(S[code][1::length + 1])

join(L) :
    joiner(L, "")
given :
    joiner = func(L, s) :
        not L : s[0::len(s) - 2]  // Removes final space and "
        else : this(tail(L), s + L[0] + " ")

evalConditional(S) :
    gatekeepStack(S, 1)
    type leftBranch == error:
        S with err :: "FORTH error: malformed conditional, left branch"
    type rightBranch == error :
        S with err :: "FORTH error: malformed conditional, right branch"
    S[stack][len(S[stack]) - 1] != 0 :
        (evaluate(S with code :: leftBranch with stack :: (S[stack] curtail 1))) ..
            .. with code :: (S[code] behead length)
    else :
        (evaluate(S with code :: rightBranch with stack :: (S[stack] curtail 1))) ..
            .. with code :: (S[code] behead length)
given :
    leftBranch = getLeftBranch tail S[code]
    rightBranch = getRightBranch tail S[code]
    conditionalLength = func(left, right):
        right : len(left) + len(right) + 3
        else : len(left) + 2
    length = conditionalLength(leftBranch, rightBranch)

getLeftBranch(C) :
    leftBrancher(C, [])
given :
    leftBrancher = func(C, D) :
        not C : error "malformed conditional"
        C[0] == "then" or C[0] == "else" : D
        else : this(tail(C), D + [C[0]])

getRightBranch(C) :
    rightBrancher(C, [], false)
given :
    rightBrancher = func(C, D, flag) :
        not C : error "malformed conditional"
        C[0] == "then" : D
        C[0] == "else" : this(tail(C), D, true)
        flag : this(tail(C), D + [C[0]], true)
        else : this(tail(C), [], false)

evalDoLoop(S) :
    gatekeepStack(S, 2)
    doLength == -1 : S with err :: "FORTH error: malformed do loop"
    else : evalLoopBody( ..
            .. (S with stack :: (S[stack] curtail 2)) , doCode, 
            .. S[stack][len(S[stack]) - 1], S[stack][len(S[stack]) - 2]) ..
        .. with code :: (S[code] behead doLength + 1) with loopVar :: NIL
given :
    doLength = findNext("loop", S[code])
    doCode = S[code][1::doLength]

evalLoopBody(S, doCode, i, upTo) :
    i >= upTo : S
    else : evalLoopBody(evaluate(S with loopVar :: i with code :: doCode), doCode, i + 1, upTo)

findNext(needle, haystack) :
    finder(needle, haystack, 0)
given finder = func(needle, haystack, n) :
    len(haystack) <= n : -1
    haystack[n] == needle : n
    this(needle, haystack, n + 1)
    
evalPushIndex(S):
    S[loopVar] == NIL : S with err :: "FORTH error: use of i outside of loop"
    else : S with code :: tail(S[code]) with stack :: S[stack] + [S[loopVar]]

evalVarDeclaration(S) :
    len(S[code]) < 2 : S with err :: "FORTH error: incomplete variable declaration"
    len(S[code]) >= 5 and type int S[code][2] == int ..
    .. and S[code][3] == "cells" and S[code][4] == "allot" :
        S with code :: (S[code] behead 5) ..
        .. with vars :: (S[vars] with S[code][1] :: 1000 + len(S[mem])) ..
        .. with mem :: S[mem] + listOfZeros(cellWidth * (1 + int S[code][2]))
    else :
        S with code :: (S[code] behead 2) ..
        .. with vars :: (S[vars] with S[code][1] :: 1000 + len(S[mem])) ..
        .. with mem :: S[mem] + [0]
given :
    listOfZeros = func(n) : for n do addZero to []
    given : addZero = func(L) : L + [0]

evalVarReference(S) :
    S  with code :: tail(S[code]) ..
    .. with stack :: (S[stack] + [S[vars][S[code][0]]])

evalGetVar(S) :
    gatekeepStack(S, 1)
    gatekeepMemory(S)
    else :
        S with code :: tail(S[code]) ..
        .. with stack :: (S[stack] curtail 1) + [S[mem][location - 1000]]
given :
    location = S[stack][len(S[stack]) - 1]   

evalPutVar(S) :
    gatekeepStack(S, 2)
    gatekeepMemory(S)
    else :
        S with code :: tail(S[code]) ..
        .. with stack :: (S[stack] curtail 2) ..
        .. with mem :: (S[mem] with location - 1000 :: S[stack][len(S[stack]) - 2])
given :
    location = S[stack][len(S[stack]) - 1]   

evalConstDeclaration(S) :
   len(S[code]) < 2 : S with err :: "FORTH error: incomplete const declaration" 
   gatekeepStack(S, 1)
   else :  S with code :: (S[code] behead 2) ..
          .. with stack :: tail (S[stack]) ..
          .. with consts :: (S[consts] with S[code][1] :: S[stack][len(S[stack]) - 1])

evalConstReference(S) : 
    S with code :: tail(S[code]) ..
    .. with stack :: S[stack] + [S[consts][S[code][0]]]

gatekeepMemory(S) :
    location < 1000 :
        S with err :: "FORTH error: memory allocation for variables starts at 1000"
    location - 999 > len(S[mem]) :
        S with err :: "FORTH error: location " + string(location) + " has not been allocated"
given :
    location = S[stack][len(S[stack]) - 1]

gatekeepStack(S, n) :
    len(S[stack]) < n : S with err :: "FORTH error: stack underflow"

forthTruth(b) :
    b : -1
    else : 0

tokenize(input string) :
    tokenizer(input, "", [])
given :
    tokenizer = func(input, word, words) :
        len(input) == 0 :
            word : words + [word]
            else : words
        input[0] == " " or input[0] == "\n" or input[0] == "\t":
            word :
                this(tail(input), "", words + [word])
            else :
                this(tail(input), "", words)
        else :
            this(tail(input), word + input[0], words)
