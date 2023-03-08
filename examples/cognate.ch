import // All the imports we need ...

"lib/prelude.ch" :: ""
"lib/strings.ch"

def // All the types we need ...

TokenType = enum STRING, INT, BOOL, BLOCK, LIST, BUILTIN, IDENT, SEMICOLON, LBRACK, RBRACK, LET, DEF, ANY
Token = struct(tokenType TokenType, value single)
Env = struct(inner map, ext Env)
MachineState = struct(stack list, outStr string, vars Env, funcs Env)
Builtin = struct(signature list, function func)
Closure = struct(codeBlock list, vars, funcs Env)

var // All the state we need ...

state = MachineState([], "", Env(map(), empty), Env(map(), empty))

cmd // All the imperative shell we need ...

ex (lineToExecute string) :
    state = execute(lineToExecute, (state with outStr :: ""))
    return (state[outStr])

print(filename string) :
    return ((file filename)[contents] ]> that + "\n" >> sum(that, "\n") + "\n")

show(i label) :
    return state[i]

run(filename string) :
    ex((file filename)[contents] >> sum(that, "\n"))

def // And the rest is the functional core. Nothing but pure functions with local constants.

execute(lineToExecute, state) :
    tokenize(lineToExecute) >> parseBindings >> parseBlocks >> desugarToRpn >> interpret(that, state)

interpret(parsedCode list, state MachineState) : \\ currentToken, "\n", state[stack][len(state[stack]) - 1]
    parsedCode == [] : state
    else : 
        currentToken[tokenType] in [INT, STRING, BOOL] :
            interpret (codeTail, (state with stack :: state[stack] + [currentToken]))
        currentToken[tokenType] == BLOCK :
            interpret (codeTail, pushClosedBlock(currentToken, state))
        currentToken[tokenType] == BUILTIN :
            interpret (codeTail, applyBuiltin(currentToken[value], state)) 
        currentToken[tokenType] == LET :
            interpret (codeTail, setVar(currentToken[value], state))
        currentToken[tokenType] == DEF :
            interpret (codeTail, setDef(currentToken[value], state))
        currentToken[tokenType] == IDENT :
            type(getFromEnv(state[vars], currentToken[value])) != error :
                interpret (codeTail, (state with stack::state[stack] + [getFromEnv(state[vars], currentToken[value])]))
            type(getFromEnv(state[funcs], currentToken[value])) != error : 
                (interpret (codeTail, (applyFunction(getFromEnv(state[funcs], currentToken[value]), state))))
            else :
                error "Cognate error: unknown identifier " + currentToken[value]
        else :
            error "Can't interpret token " + currentToken[value]
given :
    currentToken = parsedCode[0]
    codeTail = parsedCode behead 1

pushClosedBlock(b, S) :
    S with stack:: (S[stack] + [b with ([value, funcs]::S[funcs], [value, vars]::S[vars])])

applyBuiltin(nameOfBuiltin, S) :   
    gatekeepStack(signatureOfBuiltin, S) 
    functionToApply(S)
given :
    functionToApply = builtins[nameOfBuiltin][function]
    signatureOfBuiltin = builtins[nameOfBuiltin][signature]

applyFunction(fn, S) :
    (interpret(fn[value][codeBlock], (S with funcs::concatenateEnvironments(fn[value][funcs], S[funcs]),
                                         .. vars::concatenateEnvironments(fn[value][vars], S[vars])))) ..
    .. with (vars::S[vars], funcs::S[funcs])

concatenateEnvironments(innerEnv, outerEnv) :
    innerEnv[ext] == empty :
        innerEnv with ext::outerEnv
    else :
        innerEnv with ext::concatenateEnvironments(innerEnv[ext], outerEnv)

setVar(name, S) :
    gatekeepName(name, S)   
    gatekeepStack([ANY], S)    
    S with [vars, inner, name] :: S[stack][len(S[stack]) - 1] with stack::(S[stack] curtail 1)

setDef(name, S) :
    gatekeepName(name, S)
    gatekeepStack([BLOCK], S)
    S with [funcs, inner, name] :: (S[stack][len(S[stack]) - 1]) with stack::(S[stack] curtail 1)

gatekeepStack(sig , S) :     
    len(S[stack]) < len sig : 
        error "Cognate error: Stack underflow"
    not stackMatchesSignature :
        error "Cognate error: Type mismatch"
given :
    stackMatchesSignature = (range(0::len(sig)) ..
    .. ?> (sig[that] == ANY or sig[that] == S[stack][len(S[stack]) - len(sig) + that][tokenType]) ..
    .. >> len(that) == len(sig))

gatekeepName(name, S) :
    type S[vars][inner][name] != error :            // TODO for Charm. I need a `keys` function. And a 'valid' function for where this *is* the right pattern.
        error "Cognate error: name '" + name + "' is already in use for a variable"
    type S[funcs][inner][name] != error : 
        error "Cognate error: name '" + name + "' is already in use for a function"

getFromEnv(env Env, name string) :
    type resultFromInner == error :
        env[ext] == empty :
            error "Cognate error: can't find identifier '" + name + "'"
        else :
            getFromEnv(env[ext], name)
    else :
        resultFromInner
given :
    resultFromInner = env[inner][name]

parseBindings(L) :
    (while condition do action to (0, []))[1] 
given :
    condition(i, outList) : i < len L
    action(i, outList) :              
        L[i][tokenType] in [DEF, LET] :  
            i == len(L) - 1 or L[i + 1][tokenType] != IDENT : 
                error "Cognate error: '" + L[i][value] + "' should be followed by an identifier"
            else : 
                i + 2, outList + [Token(L[i][tokenType], L[i + 1][value])] 
        else : 
            i + 1, outList + [L[i]] 
 
prettyPrint(t Token) :
    t[tokenType] == INT : 
        string(t[value]) + " "
    t[tokenType] == BOOL :
        t[value] : "True "
        else : "False "
    t[tokenType] == LIST :
        "List(" + (t[value] ]> prettyPrint >> sum(that, "") >> strings.trimRight(that, " ")) + ")"
    t[tokenType] == BLOCK :
        "Block-RPN(" + (t[value][codeBlock] ]> prettyPrint >> sum(that, "")  >> strings.trimRight(that, " ")) + ")"
    else :
        t[value] + " "

parseBlocks(L) :
    (blockParser 0, L, [])[1]

blockParser(i int, L list, outList list):                               
    i >= len L or L[i][tokenType] == RBRACK :     
        i, outList                                      
    L[i][tokenType] == LBRACK :
        blockParser(slurpBlock(i))
    else :
        blockParser(i + 1, L, outList + [L[i]])
given :
    slurpBlock(i) :                                                
        endIndex + 1, L, outList + [Token(BLOCK, Closure(tokenList, empty, empty))]
    given :
        endIndex, tokenList = blockParser(i + 1, L, []) 
        

desugarToRpn(L) :
    (while condition do action to 0, [], [])[2]
given :
    condition(i, runningTotal, outList) :
        i <= len L
    action(i, runningTotal, outList) :
        i == len(L) or L[i][tokenType] == SEMICOLON : i + 1, [], outList + reverse runningTotal
        else :
            L[i][tokenType] == BLOCK :
                i + 1, runningTotal + [L[i] with [value, codeBlock] :: desugarToRpn(L[i][value][codeBlock])], outList
            else :
                i + 1, runningTotal + [L[i]], outList 

tokenize(s) : 
    wordify(s) ]> tokenizer(that)
given :
    tokenizer(s) :
        s == "Let" : Token(LET, "<no identifier assigned yet>")
        s == "Def" : Token(DEF, "<no identifier assigned yet>")
        s == "(" : Token(LBRACK, "(")
        s == ")" : Token(RBRACK, ")")
        s == ";" : Token(SEMICOLON, ";")
        s in ["True", "False"] : Token(BOOL, (s == "True"))
        type int s == int : Token(INT, int s)
        s[0] == "\"" : Token(STRING, s[1::len(s) - 1])
        type builtins[s] != error : Token(BUILTIN, s)
        else : Token(IDENT, s) 

wordify(s) :
    wordifier(0, "", [], false) >> removeEmptyStrings >> removeThingsThatStartWithLowerCase
given :
    wordifier(i, runningTotal, outList, stringLiteral) :
        i == len(s) : outList + [runningTotal]
        stringLiteral :
            s[i] == "\"" : wordifier(i + 1, "", outList + [runningTotal + "\""], false)
            else : wordifier(i + 1, runningTotal + s[i], outList, true)
        s[i] == "\"" : wordifier(i + 1, "\"", outList + [runningTotal], true)
        s[i] in ["(", ")", ";"] : wordifier (i + 1, "", outList + [runningTotal, s[i]], false)
        s[i] in [" ", "\t", "\n"] : wordifier(i + 1, "", outList + [runningTotal], false)
        else : wordifier(i + 1, runningTotal + s[i], outList, false)
    removeEmptyStrings(L) :
        L ?> (that != "")
    removeThingsThatStartWithLowerCase(L) :
        L ?> not that[0] in ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", 
                      .. "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]
        // Yes, I need a 'rune' type and a way to convert it to a number.

// It remains only to define the builtins. Yeah I could make this a lot DRY-er with a little work but meh.

builtins = map( "+" :: Builtin([INT, INT], func(S) : S with stack :: (S[stack] curtail 2) + ..
            .. [Token(INT, S[stack][len(S[stack]) - 2][value] + S[stack][len(S[stack]) - 1][value])]),
    ..  "-" :: Builtin([INT, INT], func(S) : S with stack :: (S[stack] curtail 2) + ..
            .. [Token(INT, S[stack][len(S[stack]) - 2][value] - S[stack][len(S[stack]) - 1][value])]),
    ..  "*" :: Builtin([INT, INT], func(S) : S with stack :: (S[stack] curtail 2) + ..
            .. [Token(INT, S[stack][len(S[stack]) - 2][value] * S[stack][len(S[stack]) - 1][value])]),
    ..  "/" :: Builtin([INT, INT], func(S) : S with stack :: (S[stack] curtail 2) + ..
            .. [Token(INT, S[stack][len(S[stack]) - 2][value] / S[stack][len(S[stack]) - 1][value])]),
    .. "Modulo" :: Builtin([INT, INT], func(S) : S with stack :: (S[stack] curtail 2) + ..
        .. [Token(INT, S[stack][len(S[stack]) - 2][value] % S[stack][len(S[stack]) - 1][value])]),
    ..  "<" :: Builtin([INT, INT], func(S) : S with stack :: (S[stack] curtail 2) + ..
            .. [Token(BOOL, (S[stack][len(S[stack]) - 2][value] < S[stack][len(S[stack]) - 1][value]))]),
    ..  "<=" :: Builtin([INT, INT], func(S) : S with stack :: (S[stack] curtail 2) + ..
            .. [Token(BOOL, (S[stack][len(S[stack]) - 2][value] <= S[stack][len(S[stack]) - 1][value]))]),
    ..  ">" :: Builtin([INT, INT], func(S) : S with stack :: (S[stack] curtail 2) + ..
            .. [Token(BOOL, (S[stack][len(S[stack]) - 2][value] > S[stack][len(S[stack]) - 1][value]))]),
    ..  ">=" :: Builtin([INT, INT], func(S) : S with stack :: (S[stack] curtail 2) + ..
            .. [Token(BOOL, (S[stack][len(S[stack]) - 2][value] >= S[stack][len(S[stack]) - 1][value]))]),
    ..  "==" :: Builtin([ANY, ANY], func(S) : S with stack :: (S[stack] curtail 2) + ..
            .. [Token(BOOL, (S[stack][len(S[stack]) - 2] == S[stack][len(S[stack]) - 1]))]),
    ..  "!=" :: Builtin([ANY, ANY], func(S) : S with stack :: (S[stack] curtail 2) + ..
            .. [Token(BOOL, (S[stack][len(S[stack]) - 2] != S[stack][len(S[stack]) - 1]))]),
    ..  "Integer?" :: Builtin([ANY], func(S) : S with stack :: (S[stack] curtail 1) + ..
            .. [Token(BOOL, (S[stack][len(S[stack]) - 1][tokenType] == INT))]),
    ..  "String?" :: Builtin([ANY], func(S) : S with stack :: (S[stack] curtail 1) + ..
            .. [Token(BOOL, (S[stack][len(S[stack]) - 1][tokenType] == STRING))]),
    ..  "Boolean?" :: Builtin([ANY], func(S) : S with stack :: (S[stack] curtail 1) + ..
            .. [Token(BOOL, (S[stack][len(S[stack]) - 1][tokenType] == BOOL))]),
    ..  "Zero?" :: Builtin([INT], func(S) : S with stack :: (S[stack] curtail 1) + ..
            .. [Token(BOOL, (S[stack][len(S[stack]) - 1][value] == Token(INT, 0)))]), 
    ..  "Integer!" :: Builtin([ANY], func(S) : (S[stack][len(S[stack]) - 1][tokenType] == INT : S; ..
            .. else : error "Cognate error: expected integer")),
    ..  "String!" :: Builtin([ANY], func(S) : (S[stack][len(S[stack]) - 1][tokenType] == STRING : S; ..
            .. else : error "Cognate error: expected string")),
    ..  "Boolean!" :: Builtin([ANY], func(S) : (S[stack][len(S[stack]) - 1][tokenType] == BOOL : S; ..
            .. else : error "Cognate error: expected boolean")),
    ..  "Not" :: Builtin([BOOL], func(S) : S with stack :: (S[stack] curtail 1) + ..
            .. [Token(BOOL, not (S[stack][len(S[stack]) - 1][value]))]),
    ..  "Either" :: Builtin([BOOL, BOOL], func(S) : S with stack :: (S[stack] curtail 2) + ..
            .. [Token(BOOL, (S[stack][len(S[stack]) - 2][value] or S[stack][len(S[stack]) - 1][value]))]),
    ..  "Both" :: Builtin([BOOL, BOOL], func(S) : S with stack :: (S[stack] curtail 2) + ..
            .. [Token(BOOL, (S[stack][len(S[stack]) - 2][value] and S[stack][len(S[stack]) - 1][value]))]),
    ..  "One-of" :: Builtin([BOOL, BOOL], func(S) : S with stack :: (S[stack] curtail 2) + ..
                .. [Token(BOOL, (S[stack][len(S[stack]) - 2][value] != S[stack][len(S[stack]) - 1][value]))]),          
    ..  "If" :: Builtin([ANY, ANY, BOOL], func(S) : (S[stack][len(S[stack]) - 1][value] : S with stack :: (S[stack] curtail 3) ..
            ..+ [S[stack][len(S[stack]) - 2]] ; else : S with stack :: (S[stack] curtail 3) + [S[stack][len(S[stack]) - 3]])),
    ..  "Do" :: Builtin([BLOCK], func(S) : interpret(S[stack][len(S[stack]) - 1][value][codeBlock], (S with stack :: (S[stack] curtail 1) ..
            .. with vars::S[stack][len(S[stack]) - 1][value][vars] with funcs::S[stack][len(S[stack]) - 1][value][funcs])) ..
                        .. with (vars::S[stack][vars], funcs::S[stack][funcs])) ,
    ..  "When" :: Builtin([BLOCK, BOOL], func(S) : (S[stack][len(S[stack]) - 1][value] : (interpret(S[stack][len(S[stack]) - 2][value][codeBlock],
    .. (S with (stack :: (S[stack] curtail 2), vars::S[stack][len(S[stack]) - 2][value][vars], funcs::S[stack][len(S[stack]) - 2][value][funcs]))) ..
                .. with (vars::S[stack][vars], funcs::S[stack][funcs]) ; .. 
    .. else : S  with stack :: (S[stack] curtail 2)))) ,
    ..  "Unless" :: Builtin([BLOCK, BOOL], func(S) : (not S[stack][len(S[stack]) - 1][value] : (interpret(S[stack][len(S[stack]) - 2][value][codeBlock],
            .. (S with (stack :: (S[stack] curtail 2), vars::S[stack][len(S[stack]) - 2][value][vars], funcs::S[stack][len(S[stack]) - 2][value][funcs])))) ..
                        .. with (vars::S[stack][vars], funcs::S[stack][funcs]) ; .. 
            .. else : S  with stack :: (S[stack] curtail 2))) ,
    ..  "Drop" :: Builtin([ANY], func(S) : S with stack :: (S[stack] curtail 1)),
    ..  "Twin" :: Builtin([ANY], func(S) : S with stack :: S[stack] + [S[stack][len(S[stack]) - 1]]),
    ..  "Triplet" :: Builtin([ANY], func(S) : S with stack :: S[stack] ..
                .. + [S[stack][len(S[stack]) - 1], S[stack][len(S[stack]) - 1]]),
    ..  "Swap" :: Builtin([ANY], func(S) : S with stack :: (S[stack] curtail 2) ..
            .. + [S[stack][len(S[stack]) - 1], S[stack][len(S[stack]) - 2]]),
    ..  "List" :: Builtin([BLOCK], func(S) : S with stack :: (S[stack] curtail 1) ..
            .. + [Token(LIST, reverse((interpret(S[stack][len(S[stack]) - 1][value][codeBlock], (S with stack::[] ..
            .. with vars::S[stack][len(S[stack]) - 1][value][vars] with funcs::S[stack][len(S[stack]) - 1][value][funcs])))[stack]))]),
    ..  "Push" :: Builtin([LIST, ANY], func(S) : S with stack :: (S[stack] curtail 2) + [S[stack][len(S[stack]) - 2] with value:: ..
            .. [S[stack][len(S[stack]) - 1]] + S[stack][len(S[stack]) - 2][value]]),
    ..  "First" :: Builtin([LIST], func(S) : S with stack :: (S[stack] curtail 1) + [S[stack][len(S[stack]) - 1][value][0]]),
    ..  "Rest" :: Builtin([LIST], func(S) : S with stack :: (S[stack] curtail 1) + [S[stack][len(S[stack]) - 1] with value:: ..
            .. (S[stack][len(S[stack]) - 1][value] behead 1)]),
    ..  "Range" :: Builtin([INT, INT], func(S) : S with stack :: (S[stack] curtail 2) + ..
        .. [Token(LIST, (range(S[stack][len(S[stack]) - 1][value]::S[stack][len(S[stack]) - 2][value]) ]> Token(INT, that))) ]),
    ..  "For" :: Builtin([BLOCK, LIST], func(S) : doFor((S with stack::(S[stack] curtail 2)), S[stack][len(S[stack]) - 1][value], 
        .. S[stack][len(S[stack]) - 2][value][codeBlock])),
    ..  "While" :: Builtin([BLOCK, BLOCK], func(S) : doWhile((S with stack::(S[stack] curtail 2)),
        .. S[stack][len(S[stack]) - 1][value][codeBlock], S[stack][len(S[stack]) - 2][value][codeBlock])),
    ..  "Until" :: Builtin([BLOCK, BLOCK], func(S) : doUntil((S with stack::(S[stack] curtail 2)),
        .. S[stack][len(S[stack]) - 1][value][codeBlock], S[stack][len(S[stack]) - 2][value][codeBlock])),
    ..  "Print" :: Builtin([ANY], func(S) : S with stack :: (S[stack] curtail 1) ..
            .. with outStr :: S[outStr] + prettyPrint(S[stack][len(S[stack]) - 1]) + "\n") ..
.. )

doFor = func (S, conditionBlock, actionBlock) :
    (for (len L) do action to 0, S)[1]
given :
    action(i, S) : i + 1, interpret(block, (S with stack:: S[stack] + [L[i]]))

doWhile = func(S, conditionBlock, actionBlock): 
    resultOfWhiler with stack::(resultOfWhiler[stack] curtail 1) // gets rid of last False from stack.
given :
    resultOfWhiler = whiler(S, conditionBlock, actionBlock, true)

doUntil = func(S, conditionBlock, actionBlock):
    resultOfWhiler with stack::(resultOfWhiler[stack] curtail 1) // gets rid of last False from stack.
given :
    resultOfWhiler = whiler(S, conditionBlock, actionBlock, false)

whiler(S, conditionBlock, actionBlock, flag) :
    (while condition do action to (S))[0] 
given :                                                                    
    condition(S) :
        (interpret(conditionBlock, S))[stack][len((interpret(conditionBlock, S))[stack]) - 1][tokenType] != BOOL:
            error "Cognate error: 'While' requires a boolean value"
        else :
            (interpret(conditionBlock, S))[stack][len((interpret(conditionBlock, S))[stack]) - 1][value] == flag
    action(S) :
        interpret(actionBlock, ((interpret(conditionBlock, S)) with (stack::((interpret(conditionBlock, S))[stack] curtail 1)))),
            .. conditionBlock, actionBlock, flag

prettyStack(L) :
    L == [] : "\nStack is empty.\n"
    (range(0::len L) ]> (string(that) + " : " + prettyPrint(L[that])) >> join) + "\n"

join(L) :
    (for (len L) do action to 0, "")[1]
given :
    action(i, acc): i + 1, acc + "\n" + L[i]
