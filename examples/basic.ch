import 

"lib/prelude.ch" :: ""
"lib/strings.ch"

def

// The imperative shell.

MachineState = struct(outStr string, vars map, program Program, pointer int)
Program = struct(lines map, lineNumbers list)

var 

state = MachineState("", map(), Program(map(), [-1]), 0)

cmd

main :
    lineToExecute == "STOP" :
        stop
    else :
        state = newState
        respond newState[outStr]
given :
    lineToExecute = request "BASIC > "
    _, newState = execute(lineToExecute,
        ..  (state with pointer::len(state[program][lineNumbers]) - 1, outStr::""))

def // And the rest is the functional core.

// This bit executes a line from the BASIC 'REPL', either treating it as something to evaluate or
// as a numbered line to add to the BASIC program.

execute(lineToExecute, state) :
    type newLineNumber != error : // It may be an addition to the BASIC program.
        OK, updateProgram(state, lineToExecute)
    else : // Otherwise we can evaluate it as normal.
        lineToExecute >> tokenize >> (parse(that, 0, NULL_NODE))[2] >> evaluate(that, state)
given :
    newLineNumber = (lineToExecute >> strings.split(that, " ") >> int that[0])
    remainderOfLine = (lineToExecute >> strings.index(that, " ") ..
                    .. >> lineToExecute[that + 1::len lineToExecute])
    updateProgram(state, lineToExecute) :
        state with [program, lines, newLineNumber]::remainderOfLine ..
           .. with [program, lineNumbers]::insert(state[program][lineNumbers], newLineNumber)
  
insert(L, newItem) : // Puts a line number in the right place in the list. -1 goes at the end.
    inserter 0
given :
    inserter(i) :
        newItem == L[i] :
            L
        newItem < L[i] or L[i] == -1 :
            L[0::i] + [newItem] + L[i::len L]
        else :
            inserter i + 1

// The evaluator.

ValueType = enum STR_VAL, INT_VAL, BOOL_VAL, OK_VAL, ARGS_VAL

Value = struct(valueType ValueType, value single)

BUILTINS = map("SQR"::(func(x) : x * x) ,
            .. "LEN"::(func(x) : len x) ,
            .. "LEFT"::(func(x, y) : x[0::y]) ,
            .. "RIGHT"::(func(x, y) : x[len(x) - y::len x]))

OPERATIONS = map(PLUS::(func(x, y): x + y),
              .. MINUS::(func(x, y): x - y),
              .. MULTIPLY::(func(x, y): x * y),
              .. DIVIDE::(func(x, y): x / y),
              .. MOD::(func(x, y): x % y),
              .. AND::(func(x, y): x and y),
              .. OR::(func(x, y): x or y),
              .. GT::(func(x, y): x > y),
              .. GEQ::(func(x, y): x >= y),
              .. LT::(func(x, y): x < y),
              .. LEQ::(func(x, y): x <= y),
              .. NEQ::(func(x, y): x != y),
              .. EQUALS::(func(x, y): x == y))

OK = Value(OK_VAL, "ok")

evaluateProgram(state) :
    lineNumber == -1 :
        state
    else :
        lexAndParseLine(lineNumber, state) >> evaluate(that, state) ..
            .. >> evaluateProgram(that[1] with pointer::state[pointer] + 1)
given :
    lineNumber = state[program][lineNumbers][state[pointer]]

evaluate(node Node, state MachineState):
    nodeType == INTEGER :
        makeVal(int nodeLiteral), state
    nodeType == BOOLEAN :
        makeVal(nodeLiteral == "TRUE"), state
    nodeType == STRING :
        makeVal(nodeLiteral), state
    nodeType == IDENTIFIER :
        state[vars][nodeLiteral], state
    nodeType == THEN :
        evalConditional(node, state)
    nodeType == COMMA :
        evalArgList(node, state), state
    nodeType in INFIXES :
        evalInfixExpression(node, state), state
    nodeType == LET :
        evalAssignment(node, state)
    nodeType in PREFIXES :
        evalPrefixExpression(node, state)
    nodeType == BUILTIN :
        evalBuiltinExpression(node, state), state
    else :
        error "node can't be evaluated"
given :
    nodeLiteral = node[token][tokenLiteral]
    nodeType = node[token][tokenType]

evalInfixExpression(node, state) :
    op(left, right) >> makeVal(that), state
given :
    op = OPERATIONS[node[token][tokenType]]
    left = (evaluate(node[branches][0], state))[0][value]
    right = (evaluate(node[branches][1], state))[0][value]

evalBuiltinExpression(node, state) : 
    op(node[branches][0] >> (evaluate(that, state))[0] >> makeArgsList >> tuplify) >> makeVal
given :
    op = BUILTINS[node[token][tokenLiteral]]
    makeArgsList(val Value) :
        val[valueType] == ARGS_VAL : val[value]
        else : [val[value]]

evalConditional(node, state) :
    conditionIsTrue :
        evaluate(node[branches][1], state)
    else :
        OK, state
given :
    conditionIsTrue = (evaluate(node[branches][0], state))[0][value]

evalArgList(node, state) :
    left[valueType] == ARGS_VAL :
        right[valueType] == ARGS_VAL :
            Value(ARGS_VAL, left[value] + right[value])
        else :
            Value(ARGS_VAL, left[value] + [right[value]])
    else :
        right[valueType] == ARGS_VAL :
            Value(ARGS_VAL, [left[value]] + right[value])
        else :
            Value(ARGS_VAL, [left[value], right[value]])
given :
    left, _ = evaluate(node[branches][0], state)
    right, _ = evaluate(node[branches][1], state)

evalPrefixExpression(node, state) :
    nodeType == IF : // All the logic of conditionals is done when we parse THEN.
        OK, resultingState 
    nodeType == PRINT :
        OK, (state with outStr::state[outStr] + valToBASIC val)  
    nodeType == NEWLINE :
        OK, (state with outStr::state[outStr] + "\n") 
    nodeType == NOT :
        makeVal(not val), state
    nodeType == LIST :
        OK, (state with outStr:: (state[program][lineNumbers] curtail 1 ..
        .. ]> string(that) + " " + state[program][lines][that] + "\n" >> sum(that, "")))  
    nodeType == GOTO :
        OK, evaluateProgram(state with pointer::findIn(state[program][lineNumbers], val)) 
    nodeType == RUN :
        OK, evaluateProgram(state with pointer::0) 
given :
    nodeType = node[token][tokenType]
    argVal, resultingState = (evaluate(node[branches][0], state))
    val = argVal[value]

evalAssignment(node, state) : 
    OK, (state with [vars, varName]::rhs)
given :
    equalsNode = node[branches][0]
    varName = equalsNode[branches][0][token][tokenLiteral]
    rhs, _ = evaluate(equalsNode[branches][1], state)

makeVal(charmVal single) :
    type charmVal == int : Value(INT_VAL, charmVal)
    type charmVal == string : Value(STR_VAL, charmVal)
    type charmVal == bool : Value(BOOL_VAL, charmVal)
    else : error "who even knows?"

lexAndParseLine(i int, state) :
    i >> state[program][lines][i] >> tokenize >> (parse(that, 0, NULL_NODE))[2]

valToBASIC(val) :
    type val != bool : string val
    val : "TRUE"
    else :false

// The parser.

Node = struct(token Token, branches list)

PREFIXES = {IF, NOT, PRINT, LET, GOTO, LIST, RUN, NEWLINE}
INFIXES = {PLUS, MINUS, MULTIPLY, DIVIDE, EQUALS, THEN, COMMA, AND, OR, 
        .. LT, LEQ, GT, GEQ, NEQ, MOD}
LITERALS = {INTEGER, STRING, BOOLEAN}

NULL_NODE = Node(Token(NULL, "null"), [])

PRECEDENCE = map(MULTIPLY::5, DIVIDE::5, MOD::5, PLUS::4, MINUS::4, 
              .. GOTO::4, LET::4, EQUALS::3, LT::3, LEQ::3, GT::3, GEQ::3, 
              .. NEQ::3, NOT::2, AND::2, OR::1, THEN::0, COMMA::0, EOL::-1)

parse(tokens list, precedence int, node Node) :
    tokens == [] or currentType in {EOL, R_PAREN} or ..
    .. (currentType in keys PRECEDENCE and PRECEDENCE[currentType] < precedence):
        tokens, 0, node
    currentType in INFIXES :
        parse(infixExpression(tokens, precedence, node))
    node != NULL_NODE :
        error "BASIC error: unexpected " + tokens[0][tokenLiteral]
    currentType in LITERALS + {IDENTIFIER} : 
        parse(valueExpression(tokens, precedence))
    currentType == L_PAREN :
        parse(groupedExpression(tokens, precedence))
    currentType in PREFIXES + {BUILTIN} :
        parse(prefixExpression(tokens))
    else :
        error("BASIC doesn't know how to parse that!")
given :
    currentType = tokens[0][tokenType]

infixExpression(tokens list, precedence int, leftNode Node) : 
    remainingTokens, newPrecedence, Node(tokens[0], [leftNode, rightNode]) 
given :
    remainingTokens, newPrecedence, rightNode ..
        .. = parse(tail(tokens), precedence, NULL_NODE)

valueExpression(tokens list, precedence int) : 
    nextTokenType in INFIXES and nextPrecedence > precedence : 
        infixExpression(tail(tokens), nextPrecedence, Node(curTok, [])) 
    else : 
        tail(tokens), precedence, Node(curTok, []) 
given:
    curTok = tokens[0]
    nextTokenType = tokens[1][tokenType] 
    nextPrecedence = PRECEDENCE[nextTokenType] 

groupedExpression(tokens list, precedence int) : 
    tail(remainingTokens), precedence, groupNode
given :
    remainingTokens, _, groupNode = parse(tail(tokens), 0, NULL_NODE)

prefixExpression(tokens list) : 
    remainingToks, newPrec, Node(tokens[0], [arg]) 
given :
    precToUse = (tokens[0][tokenType] in [BUILTIN, NOT]: 6; else: 0)
    remainingToks, newPrec, arg = parse(tail(tokens), precToUse, NULL_NODE)

// The tokenizer.

TokenType = enum STRING, INTEGER, BOOLEAN, PLUS, MINUS, MULTIPLY, DIVIDE, IF, THEN, COMMA, AND, OR, 
                .. NOT, GOTO, PRINT, BUILTIN, IDENTIFIER, EQUALS, LET, L_PAREN, R_PAREN, EOL, NULL, LIST,
                .. LT, GT, LEQ, GEQ, NEQ, RUN, MOD, NEWLINE

Token = struct(tokenType TokenType, tokenLiteral single)

KEYWORD_MAP = map("+"::PLUS, "-"::MINUS, "*"::MULTIPLY, "/"::DIVIDE, "="::EQUALS, "IF"::IF,
                .. "THEN"::THEN, ","::COMMA, "AND"::AND, "OR"::OR, "NOT"::NOT, "GOTO"::GOTO, 
                .. "PRINT"::PRINT, "LET"::LET, "("::L_PAREN, ")"::R_PAREN, "LIST"::LIST,
                .. "<"::LT, "<="::LEQ, ">"::GT, ">="::GEQ, "<>"::NEQ, "NEWLINE"::NEWLINE,
                .. "RUN"::RUN, "MOD"::MOD)

tokenize(s) : 
    wordify(s) ]> tokenizer(that) >> that + [Token(EOL, "")]
given :
    tokenizer(s) :
        s in keys(KEYWORD_MAP) : 
            Token(KEYWORD_MAP[s], s)
        s in ["TRUE", "FALSE"] : 
            Token(BOOLEAN, s)
        type int s == int : 
            Token(INTEGER, s)
        s[0] == "\"" : 
            Token(STRING, s[1::len(s) - 1])
        s in keys BUILTINS : Token(BUILTIN, s)
        else : 
            Token(IDENTIFIER, s) 

wordify(s) :
    wordifier(0, "", [], false) ?> nonEmptyString
given :
    nonEmptyString(s) :
        s != ""
    wordifier(i, runningTotal, outList, stringLiteral) :
        i == len(s) : outList + [runningTotal]
        stringLiteral :
            s[i] == "\"" : 
                wordifier(i + 1, "", outList + [runningTotal + "\""], false)
            else : 
                wordifier(i + 1, runningTotal + s[i], outList, true)
        s[i] == "\"" : 
            wordifier(i + 1, "\"", outList + [runningTotal], true)
        s[i] in ["(", ")", ","] : 
            wordifier (i + 1, "", outList + [runningTotal, s[i]], false)
        s[i] in [" ", "\t"] : 
            wordifier(i + 1, "", outList + [runningTotal], false)
        else :
            wordifier(i + 1, runningTotal + s[i], outList, false)
