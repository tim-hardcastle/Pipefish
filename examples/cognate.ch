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

state = MachineState([], "", Env(map(), NIL), Env(map(), NIL))

cmd // All the imperative shell we need ...

ex(lineToExecute string) :
    state = execute(lineToExecute, (state with outStr :: ""))
    return state[outStr]

show(i label) :
    return state[i]

def // And the rest is the functional core. Nothing but pure functions with local constants.

execute(lineToExecute, state) :
    tokenize(lineToExecute) >> parseBindings >> parseBlocks >> desugarToRpn >> interpret(that, state)

interpret(parsedCode list, state MachineState) : 
    parsedCode == [] : state
    else :
        currentToken[tokenType] in [INT, STRING, BOOL] :
            interpret (codeTail, (state with stack :: state[stack] + [currentToken]))
        currentToken[tokenType] == BLOCK :
            interpret (codeTail, (state with stack :: state[stack] + [currentToken ..
               .. with [value, vars]::Env(map(), state[vars]) with [value, funcs]::Env(map(), state[funcs])])) 
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
                (interpret (codeTail, interpret((getFromEnv(state[funcs], currentToken[value]))[value][codeBlock] , 
                                  .. (state with vars::(getFromEnv(state[funcs], currentToken[value]))[value][vars] ..
                                        .. with funcs::(getFromEnv(state[funcs], currentToken[value]))[value][funcs]))) ..
                                  .. with vars::state[vars] with funcs::state[funcs])
            else :
                error "Cognate error: unknown identifier " + currentToken[value]
        else :
            error "Can't interpret token " + currentToken[value]
given :
    currentToken = parsedCode[0]
    codeTail = parsedCode behead 1

applyBuiltin(nameOfBuiltin, S) :   
    gatekeepStack(signatureOfBuiltin, S) 
    functionToApply(S)
given :
    functionToApply = builtins[nameOfBuiltin][function]
    signatureOfBuiltin = builtins[nameOfBuiltin][signature]

setVar(name, S) :
    gatekeepName(name, S)   
    gatekeepStack([ANY], S)    
    S with [vars, inner, name] :: S[stack][len(S[stack]) - 1] with stack::(S[stack] behead 1)

setDef(name, S) :
    gatekeepName(name, S)
    gatekeepStack([BLOCK], S)
    S with [funcs, inner, name] :: (S[stack][len(S[stack]) - 1]) with stack::(S[stack] behead 1)

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
        env[ext] == NIL :
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
                error "Cognate error: " + describeToken(L[i]) + " should be followed by an identifier"
            else : 
                i + 2, outList + [Token(L[i][tokenType], L[i + 1][value])] 
        else : 
            i + 1, outList + [L[i]] 
    
describeToken(t) :
    t[tokenType] == DEF : "'Def'"
    t[tokenType] == LET : "'Let'"
    else : error "asked to describe a token I don't know how to describe"

parseBlocks(L) :
    (blockParser 0, L, [])[1]

blockParser(i int, L list, outList list) -> int, list:                                
    i >= len L or L[i][tokenType] == RBRACK :      
        i, outList                                      
    L[i][tokenType] == LBRACK :
        blockParser(slurpBlock(i))
    else :
        blockParser(i + 1, L, outList + [L[i]])
given :
    slurpBlock(i) :                                                
        endIndex + 1, L, outList + [Token(BLOCK, Closure(tokenList, NIL, NIL))]
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
        s[i] == " ": wordifier(i + 1, "", outList + [runningTotal], false)
        else : wordifier(i + 1, runningTotal + s[i], outList, false)
    removeEmptyStrings(L) :
        L ?> (that != "")
    removeThingsThatStartWithLowerCase(L) :
        L ?> not that[0] in ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", 
                      .. "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]
        // Yes, I need a 'rune' type and a way to convert it to a number.

// It remains only to define the builtins.

add = func(S) : 
    S with stack :: (S[stack] curtail 2) + [Token(INT, ((S[stack][len(S[stack]) - 2][value]) + (S[stack][len(S[stack]) - 1][value])))] 
given :
    op1 = S[stack][len(S[stack]) - 2][value]
    op2 = S[stack][len(S[stack]) - 1][value]

builtins = map( "+" :: Builtin([INT, INT], add),
            ..  "-" :: Builtin([INT, INT], func(S) : S with stack :: (S[stack] curtail 2) + ..
                    .. [Token(INT, S[stack][len(S[stack]) - 2][value] - S[stack][len(S[stack]) - 1][value])]),
            ..  "*" :: Builtin([INT, INT], func(S) : S with stack :: (S[stack] curtail 2) + ..
                    .. [Token(INT, S[stack][len(S[stack]) - 2][value] * S[stack][len(S[stack]) - 1][value])]),
            ..  "/" :: Builtin([INT, INT], func(S) : S with stack :: (S[stack] curtail 2) + ..
                    .. [Token(INT, S[stack][len(S[stack]) - 2][value] / S[stack][len(S[stack]) - 1][value])]),
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
            ..  "Not" :: Builtin([BOOL], func(S) : S with stack :: (S[stack] curtail 1) + ..
                    .. [Token(BOOL, not (S[stack][len(S[stack]) - 1][value]))]),
            ..  "If" :: Builtin([ANY, ANY, BOOL], func(S) : (S[stack][len(S[stack]) - 1][value] : S with stack :: (S[stack] curtail 3) + [S[stack][len(S[stack]) - 2]] ; ..
                                .. else : S with stack :: (S[stack] curtail 3) + [S[stack][len(S[stack]) - 3]])),
            ..  "Do" :: Builtin([BLOCK], func(S) : interpret(S[stack][len(S[stack]) - 1][value][codeBlock], (S with stack :: (S[stack] curtail 1) ..
                            .. with vars::S[stack][len(S[stack]) - 1][value][vars] with funcs::S[stack][len(S[stack]) - 1][value][funcs]))) ,
            ..  "Drop" :: Builtin([ANY], func(S) : S with stack :: (S[stack] curtail 1)),
            ..  "Twin" :: Builtin([ANY], func(S) : S with stack :: S[stack] + [S[stack][len(S[stack]) - 1]]),
            ..  "Triplet" :: Builtin([ANY], func(S) : S with stack :: S[stack] ..
                                .. + [S[stack][len(S[stack]) - 1], S[stack][len(S[stack]) - 1]]),
            ..  "Swap" :: Builtin([ANY], func(S) : S with stack :: (S[stack] curtail 2) ..
                                .. + [S[stack][len(S[stack]) - 1], S[stack][len(S[stack]) - 2]]),
            ..  "List" :: Builtin([BLOCK], func(S) : S with stack :: (S[stack] curtail 1) ..
                                .. + [Token(LIST, reverse((interpret(S[stack][len(S[stack]) - 1][value][codeBlock], (S with stack::[] ..
                                .. with vars::S[stack][len(S[stack]) - 1][value][vars] with funcs::S[stack][len(S[stack]) - 1][value][funcs])))[stack]))]),
            ..  "Print" :: Builtin([ANY], func(S) : S with stack :: (S[stack] curtail 1) ..
                                                   .. with outStr :: S[outStr] + string (S[stack][len(S[stack]) - 1][value]) + " ") ..
.. )
