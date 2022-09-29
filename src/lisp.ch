import

"lib/prelude.ch" :: ""
"lib/strings.ch"

def

TokenType = enum DEFINE, LAMBDA, LPAREN, IF, INT, RPAREN, SET, SYMBOL, QUOTE

Token = struct(tokenType TokenType, literal single)

Env = struct(vars map, outer)

INIT_VARS  =  map  "+"          :: (func(x) : x[0] + x[1]),
                .. "-"          :: (func(x) : x[0] - x[1]),
                .. "*"          :: (func(x) : x[0] * x[1]),
                .. "<"          :: (func(x) : x[0] < x[1]),
                .. ">"          :: (func(x) : x[0] > x[1]),
                .. "<="         :: (func(x) : x[0] <= x[1]),
                .. ">="         :: (func(x) : x[0] <= x[1]),
                .. "="          :: (func(x) : x[0] == x[1]),
                .. "begin"      :: (func(x) : x[len(x) - 1]),
                .. "car"        :: (func(x) : x[0]),
                .. "cdr"        :: (func(x) : x behead 1),
                .. "cons"       :: (func(x) : [x[0]] + x[1]),
                .. "length"     :: (func(x) : len x[0]),
                .. "list"       :: (func(x) : x),
                .. "list?"      :: (func(x) : type x[0] == list),
                .. "map"        :: (func(x) : (x behead 1) apply x[0]),
                .. "max"        :: (func(x) : max x[0], x[1]),
                .. "min"        :: (func(x) : min x[0], x[1]),
                .. "not"        :: (func(x) : not x[0]),
                .. "null?"      :: (func(x) : x[0] == []),
                .. "number?"    :: (func(x) : type x[0] == int),
                .. "symbol?"    :: (func(x) : type x[0] == string),
                .. "quotient"   :: (func(x) : x[0] / x[1])      

var

result single = ""
currentEnv = Env(INIT_VARS, NIL)

cmd 

ex(input string) :
    result, currentEnv = evaluate (parse(input), currentEnv)
    return sanitize result

def

keywordMap = map "define" :: DEFINE, "lambda" :: LAMBDA, "(" :: LPAREN, "if" :: IF,
            .. ")" :: RPAREN, "set!" :: SET, "quote" :: QUOTE

evaluate(lisp single, env Env):
    type lisp == list :
        lisp[0] is DEFINE :
            lisp[1][literal] isVariableIn env :
                error "Lisp can't redefine variable '" + lisp[1][literal] + "'"
            else :
                (evaluate lisp[2], env)[0], (env with [vars, lisp[1][literal]] :: (evaluate lisp[2], env)[0])
        lisp[0] is LAMBDA :
            makeFn(lisp[1], lisp[2], env), env
        lisp[0] is IF : 
            (evaluate lisp[1], env)[0] : (evaluate lisp[2], env)
            else : (evaluate lisp[3], env)
        lisp[0] is SET : 
            lisp[1][literal] isVariableIn env :
                (evaluate lisp[2], env)[0], env updatedWith (lisp[1][literal], (evaluate lisp[2], env)[0])
            else:
                error "Lisp can't find variable '" + lisp[1][literal] + "'"
        lisp[0] is QUOTE : (literalize lisp behead 1)[0], env
        else :
            ((evaluate lisp[0], env)[0]) (lisp[1::len(lisp)] apply func(x) : (evaluate x, env)[0]) , env
    else :
        lisp is INT :
            lisp[literal], env
        lisp is SYMBOL :
            lisp[literal] isVariableIn env :
                (getVariable lisp[literal] from env), env
            else : error "Lisp doesn't understand '" + lisp[literal] + "'"

makeFn(params, body, env) :
    func(x) : (evaluate body, newEnv)[0]
    given :
        paramLiterals = params apply (func(x) : x[literal])
        newEnv = Env with vars :: (zip paramLiterals, x), outer :: env

(v string) isVariableIn (env Env):
    v isKeyOf env[vars] : true
    env[outer] == NIL : false
    else : v isVariableIn env[outer]

getVariable (v string) from (env Env):
    v isKeyOf env[vars] : env[vars][v]
    env[outer] == NIL : error "Lisp can't find variable '" + v + "'"
    else : getVariable v from env[outer]

(env Env) updatedWith (name string, value) :
    name isKeyOf env[vars] : env with [vars, name] :: value
    else : env with outer updatedWith (name, value)

literalize (thing):
    type thing == list : thing apply (func(x) : literalize x)
    else : thing[literal]

parse(input string) :
    tokens[0] is LPAREN:
        getList tokens, 1
    else:
        (getList tokens, 0)[0]
given :
    tokens = tokenize(input)

getList (tokens, pos) :
    (while condition do action to pos, [])[1]
given :
    condition(i, L) : i < len(tokens) and not (tokens[i] is RPAREN)
    action(i, L) :
        tokens[i] is LPAREN :
            i + 2 + len(getList tokens, i + 1), L + [getList tokens, i + 1]
        else :
            i + 1, L + [tokens[i]]
            
tokenize(input string):
    (for len(words) do action to 0, [])[1]
given :
    words = toWords(input)
    action(i, tokens):
        type keywordMap[words[i]] == TokenType :
            i + 1, tokens + [Token(keywordMap[words[i]], words[i])]
        type int words[i] == int :
            i + 1, tokens + [Token(INT, int words[i])]
        else :
            i + 1, tokens + [Token(SYMBOL, words[i])]

toWords(input string):
    strings.split(strings.trim(pad(input), " "), " ")
given :
    pad (s) :
        strings.replaceAll(strings.replaceAll(s, "(", " ( ") , ")", " ) ")

(t Token) is (tT TokenType) : t[tokenType] == tT

(k single) isKeyOf (M map): type M[k] != error

sanitize (x) :
    type x in {int, string} : x
    else : "OK"