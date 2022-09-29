import

"lib/prelude.ch" :: ""


def

// A struct to keep the machine state in.
State = struct(regs map, mem list, code list, pos int, stack list, lbls map)

// DEPTH * 16 gives the memory capacity of the machine, in 8-bit bytes.
DEPTH = 8

// A 16-bit number made out of two integers.
Num16 = struct(hi, lo int)

// 'value' will either be an int or a Num16. "Parsing" an indirection operand, e.g.
// (#8fe6), will yield the memory location and not its contents, which are extracted
// by a further step. 
parsedOperand = struct(literal string, value single, opType string)

// Names of registers
reg16s = {"af", "de", "bc", "hl"}
reg8s = {"a", "f", "b", "c", "d", "e", "h", "l"}

regMap = map ("a" :: "af", "f" :: "af", "h" :: "hl", "l" :: "hl", "b" :: "bc", ..
           .. "c" :: "bc", "d" :: "de", "e" :: "de" )


var

// Our one and only variable. Initialized with lots of zeros.

S = State(zeroedRegs, (listOf DEPTH * 16 times 0), [], 0, [], map())


// The imperative shell.
cmd

load(filename) :
    S = State(zeroedRegs, (listOf DEPTH * 16 times 0), [], 0, [], map())
    S = S with code :: (file filename)[contents]
    S = S with lbls :: getLbls(S[code])

ex(s) :
    S = execute(S, s)
    show

run :
    S = runCode(S with pos :: 0)
    show

step :
    S = stepCode(S)
    show

reset :
    S = State(zeroedRegs, (listOf DEPTH * 16 times 0), S[code], 0, [], S[lbls])
    show

show :
    return prettyPrint S

// And the main functions.

def

// A tiny function for running a program

runCode(S) :
    (S[pos]) >= (len S[code]) : S
    else :
        getOperator(currentLine) == "jp" :
            runCode(result)
        else :
            runCode(result with pos :: S[pos] + 1)
given :
    currentLine = S[code][S[pos]]
    result = (execute(S, currentLine))

// And one for stepping through it.

stepCode(S) :
    S[pos] >= len S[code] : error "program terminated, no more code"
    else :
        getOperator(currentLine) == "jp" :
            result
        else :
            result with pos :: S[pos] + 1 
given :
    currentLine = S[code][S[pos]]
    result = (execute(S, currentLine))

// A big conditional on all the Z80 operators. Or the ones I could be
// bothered with.

execute(S State, line string) :
    line in {"nop", ""} : S
    line[0] in {"/", "@"} : S
    type operator == error : operator
    type parsedOperands == error : parsedOperands
    operator == "ld" :
        exLd S, parsedOperands
    operator == "push" :
        exPush S, parsedOperands
    operator == "pop" :
        exPop S, parsedOperands
    operator == "add" :
        exAdd S, parsedOperands
    operator == "adc" :
        exAdc S, parsedOperands
    operator == "sub" :
        exSub S, parsedOperands
    operator == "sbc" :
        exSbc S, parsedOperands
    operator == "cp" :
        exCp S, parsedOperands
    operator == "inc" :
        exInc S, parsedOperands
    operator == "dec" :
        exDec S, parsedOperands
    operator == "neg" :
        exNeg S, parsedOperands
    operator == "jp" :
        exJp S, parsedOperands
    else :
        error "operator '" + operator + "' undefined"
given :
    operator = getOperator(line)
    operands = (line == operator : []; else : csvToList(line behead len(operator) + 1))
    parsedOperands = parse (S, operands)

// And now another big conditional to deal with all the whacky things
// ld can get up to. (Again, modulo me not caring about IX very much.)

exLd(S State, operands) :
    gatekeepOperands("ld", operands, 2)
    operands[0][opType] == "Reg8" and operands[1][opType] in {"Num8", "Reg8"} :
        putNum8InReg8(S, operands[0][literal], operands[1][value])
    operands[0][opType] == "Addr16" and operands[1][opType] == "Reg8" ..
        .. and (operands[0][literal] == "(hl)" or operands[1][literal] == "a") :
            putNum8InAddr16(S, operands[0][value], operands[1][value]) 
    operands[0][opType] == "Reg8" and operands[1][opType] == "Addr16" ..
        .. and (operands[0][literal] == "a" or operands[1][literal] == "(hl)"):
            putNum8InReg8(S, operands[0][literal], getFromMemory8(S, operands[1][value])) 
    operands[0][opType] == "Reg16" and operands[1][opType] in {"Num16", "Reg16"} :
        putNum16InReg16(S, operands[0][literal], operands[1][value]) 
    operands[0][opType] == "Reg16" and operands[1][opType] == "Addr16" :
        putNum16InReg16(S, operands[0][literal], getFromMemory16(S, operands[1][value]))       
    operands[0][opType] == "Addr16" and operands[1][opType] == "Reg16" :
        putNum16InAddr16(S, operands[0][value], operands[1][value])   
    else : error "'ld' can't be applied to operands of that type"

// Stack operations

exPush(S State, operands) :
    gatekeepOperands("push", operands, 1)
    operands[0][opType] != "Reg16" :
        error "can't push that"
    else :
        S with stack :: (S[stack] + [operands[0][value]])

exPop(S State, operands) :
    gatekeepOperands("pop", operands, 1)
    operands[0][opType] != "Reg16" :
        error "can't pop to that"
    else :
        putNum16InReg16(S, operands[0][literal], S[stack][len(S[stack]) - 1]) ..
            .. with stack :: (S[stack] curtail 1)

// Addition

exAdd(S State, operands) :
    gatekeepOperands("add", operands, 2)
    operands[0][literal] == "a" and operands[1][opType] in {"Num8", "Reg8", "Addr16"} :
        exAdd8 (S, operands, 0)   
    operands[0][literal] == "hl" and operands[1][opType] == "Reg16" :
        exAdd16 (S, operands, 0)   
    else :
        error "can't apply 'add' to that"

exAdc(S State, operands) :
    gatekeepOperands("adc", operands, 2)
    operands[0][literal] == "a" and operands[1][opType] in {"Num8", "Reg8", "Addr16"} :
        exAdd8 (S, operands, oneIfCarry(S))   
    operands[0][literal] == "hl" and operands[1][opType] == "Reg16" :
        exAdd16 (S, operands, oneIfCarry(S))  
    else :
        error "can't apply 'add' to that"

exAdd8(S, operands, flag) :
    putNum8InReg8(F, "a", (flag + operands[0][value] + (val8(S, operands[1]))[value]) % 256)
given :
    F = putNum8InReg8(S, "f", add8Flags(operands[0][value], (val8(S, operands[1]))[value], flag))

add8Flags(m, n, flag) : 
    (sum >= 256 : 1; else : 0) + (sum % 256 == 0 : 64; else : 0) + .. 
     .. (sum % 256 >= 128 : 128; else : 0)
given : sum = m + n + flag

exAdd16(S, operands, flag) :
    putNum16InReg16(F, "hl", sum)
given :
    sum = operands[0][value] + operands[1][value] + Num16(0, flag)
    F = putNum8InReg8(S, "f", add16Flags(sum, operands[0][value], operands[1][value], flag))

add16Flags(sum, m, n, flag) :
    (m[hi] + n[hi] + (m[lo] + n[lo] + flag >= 256 : 1; else: 0) >= 256 : 1; else : 0) + ..
 .. (sum == Num16(0, 0) : 64; else : 0) + .. 
 .. (sum[hi] >= 128 : 128; else : 0)

// Subtraction

exSub(S State, operands) :
    gatekeepOperands("sub", operands, 2)
    operands[0][literal] == "a" and operands[0][opType] in {"Num8", "Reg8", "Addr16"} :
        exSub8 (S, operands, 0)   
    else :
        error "can't apply 'sub' to that"

exSbc(S State, operands) :
    gatekeepOperands("sbc", operands, 2)
    operands[0][literal] == "a" and operands[0][opType] in {"Num8", "Reg8", "Addr16"} :
        exSub8 (S, operands, oneIfCarry(S)) 
    operands[0][literal] == "hl" and operands[1][opType] == "Reg16" :
        exSbc16 (S, operands)
    else :
        error "can't apply 'sbc' to that"

exSbc16(S, operands) :
    putNum16InReg16(F, "hl", diff)
given :
    diff = operands[0][value] - operands[1][value] - Num16(0, oneIfCarry(S))
    F = putNum8InReg8(S, "f", sbc16Flags(diff, operands[0][value], operands[1][value], oneIfCarry(S)))

sbc16Flags(diff, m, n, flag) :
    (m[hi] - n[hi] - (m[lo] - n[lo] - flag < 0 : 1; else: 0) < 0 : 1; else : 0) + ..
 .. (diff == Num16(0, 0) : 64; else : 0) + .. 
 .. (diff[hi] >= 128 : 128; else : 0)

oneIfCarry(S) :
    S[regs]["af"][lo] % 2

exSub8(S, operands, flag) :
    operands[0][opType] in {"Num8", "Reg8", "Addr16"} :
        putNum8InReg8(F, "a", (256 - flag + operands[0][value] - (val8(S, operands[1]))[value]) % 256)
given :
    F = putNum8InReg8(S, "f", subFlags(operands[0][value], (val8(S, operands[1]))[value], flag))

subFlags(m, n, flag) : 
    (diff < 0 : 1; else : 0) + (diff % 256 == 0 : 64; else : 0) + .. 
     .. ((256 + diff) % 256 >= 128 : 128; else : 0)
given : diff = m - n - flag

// Comparison

exCp(S, operands) :
    gatekeepOperands("cp", operands, 1)
    operands[0][opType] in {"Num8", "Reg8", "Addr16"} :
        putNum8InReg8(S, "f", subFlags(reg8toNum(S, "a"), (val8(S, operands[0]))[value], 0))
    else : error "can't apply cp to that"

// Inc and dec

exInc(S, operands) :
    gatekeepOperands("inc", operands, 1)
    operands[0][opType] == "Reg8" :
        putNum8InReg8(F, operands[0][literal], (thingToIncrement + 1) % 256)
    operands[0][opType] == "Addr16" :
        putNum8InAddr16(F, operands[0][value], (thingToIncrement + 1) % 256)
    operands[0][opType] == "Reg16" and not (operands[0][literal] == "af") :
        putNum16InReg16(F, operands[0][literal], (thingToIncrement + Num16(0,1)))
    else : error "can't apply inc to that"
given :
    thingToIncrement = (operands[0][opType] == "Reg8" : reg8toNum(S, operands[0][literal]) ; ..
                        .. operands[0][opType] == "Reg16" : S[regs][operands[0][literal]] ; ..
                        .. else : getFromMemory8 S, operands[0][value])
    F = putNum8InReg8(S, "f", incFlags(S, thingToIncrement))

incFlags(S, n) :
    type n == int :
        (S[regs]["af"][lo] % 2) + (n == 255 : 64; else : 0) + .. 
        .. (n >= 127 : 128; else : 0)
    else :
        S[regs]["af"][lo]

exDec(S, operands) :
    gatekeepOperands("inc", operands, 1)
    operands[0][opType] == "Reg8" :
        putNum8InReg8(F, operands[0][literal], (thingToDecrement + 255) % 256)
    operands[0][opType] == "Addr16" :
        putNum8InAddr16(F, operands[0][value], (thingToDecrement + 255) % 256)
    operands[0][opType] == "Reg16" and not (operands[0][literal] == "af") :
        putNum16InReg16(F, operands[0][literal], (thingToDecrement - Num16(0,1)))
    else : error "can't apply dec to that"
given :
    thingToDecrement = (operands[0][opType] == "Reg8" : reg8toNum(S, operands[0][literal]) ; ..
                        .. operands[0][opType] == "Reg16" : S[regs][operands[0][literal]] ; ..
                        .. else : getFromMemory8 S, operands[0][value])
    F = putNum8InReg8(S, "f", decFlags(S, thingToDecrement))

decFlags(S, n) :
    type n == int :
        (S[regs]["af"][lo] % 2) + (n == 1 : 64; else : 0) + .. 
         .. (n >= 129 or n == 0: 128; else : 0)
    else :
        S[regs]["af"][lo]

exNeg(S, operands) :
    gatekeepOperands("neg", operands, 0)
    putNum8InReg8(F, "a", negA)
given :
    regA = reg8toNum(S, "a")
    negA = (256 - regA) % 256
    F = putNum8InReg8(S, "f", (regA != 0 : 1; else : 0) + ..
                      .. (negA == 0 : 64; else : 0) + (negA >= 80 : 128; else : 0))

exJp(S, operands):
    len operands == 1 :
        operands[0][opType] == "Lbl" :
        else : error "can't jump to that"
    len operands == 2 :
        operands[0][opType] != "Cnd" :
            error "not a condition"
        operands[1][opType] != "Lbl" :
            error "can't jump to that"
        operands[0][value] :
            S with pos :: S[lbls][operands[1][literal]]
        else :
            S with pos :: S[pos] + 1
    else : "wrong number of operands for jump."

// We need to extract the labels from the code when we load it.
getLbls(L) :
    (while p do f to 0, map ())[1]
given :
    p = func(i, M) : i < len L
    f = func(i, M) :
        len L[i] > 1 and L[i][0] == "@" :
            i + 1, (M with L[i] :: i)
        else :
            i + 1, M 

// A thing to find the operator in a line.

getOperator(line) :
    getfirstWord(0)
given :
    getfirstWord = func(i) :
        i == len(line) : line
        line[i] == " " : line[0::i]
        this i + 1

// We "parse" the operands, getting out their type, and the value or memory location
// they hold, and a literal.

parse(S State, operands list) :
    operands apply f
given :
    f = func(s string) :
        len s == 5 and s[0] == "#" and type hexToNum16(s[1::5]) != error :
            parsedOperand(s, hexToNum16(s[1::5]), "Num16")
        len s == 3 and s[0] == "#" and type hexToNum8(s[1::3]) != error :
            parsedOperand(s, hexToNum8(s[1::3]), "Num8")
        len s == 7 and s[0] == "(" and s[1] == "#" and s[6] == ")" and type hexToNum16(s[2::6]) != error :
            parsedOperand(s, hexToNum16(s[2::6]), "Addr16")
        s in reg16s :
            parsedOperand(s, S[regs][s], "Reg16")
        s in reg8s :
            parsedOperand(s, S[regs][regMap[s]][hilo s], "Reg8")
        len s == 4 and s[0] == "(" and s[1::3] in reg16s and s[3] == ")" :
            parsedOperand(s, S[regs][s[1::3]], "Addr16")
        s[0] == "@" :
            parsedOperand(s, S[lbls][s], "Lbl")
        s == "z" :
            parsedOperand(s, (reg8toNum(S, "f") % 128 / 64 == 1) , "Cnd")
        s == "nz" :
            parsedOperand(s, (reg8toNum(S, "f") % 128 / 64 == 0) , "Cnd")
        s == "m" :
            parsedOperand(s, (reg8toNum(S, "f") / 128 == 1) , "Cnd")
        s == "p" :
            parsedOperand(s, (reg8toNum(S, "f") / 128 == 0) , "Cnd")
        s == "c" :
            parsedOperand(s, (reg8toNum(S, "f") % 2 == 1) , "Cnd")
        s == "nc" :
            parsedOperand(s, (reg8toNum(S, "f") % 2 == 0) , "Cnd")
        else :
            error "can't parse operand '" + s + "'"

// And a bunch of helper functions.

gatekeepOperands(verb, ops, n) :
    len ops != n : error "'" + verb + "' should have " + string(n) + " operands"


putNum8InReg8(S, s, n) :
    S with [regs, regMap[s], hilo(s)] :: n  

putNum16InReg16(S, s, n) :
    S with [regs, s] :: n

getFromMemory8(S State, n) :
    S[mem][num16ToDec n]

getFromMemory16(S State, n) :
    Num16(S[mem][1 + num16ToDec n], S[mem][num16ToDec n])

putNum8InAddr16(S State, dest, n) :
    S with [mem, num16ToDec(dest)] :: n

putNum16InAddr16(S State, dest, n) :
    (S with [mem,num16ToDec(dest)] :: n[lo]) with [mem][1 + num16ToDec(dest)] :: n[hi]

// Indirects an Addr16 to a Num8, otherwise returns what it was given.
val8(S, p) :
    p[opType] == "Addr16" :
        parsedOperand(p[literal], getFromMemory8(S, p[value]), "Num8")
    else : p

val16(S, p) :
    p[opType] == "Addr16" :
        getFromMemory16(S, p[value])
    else : p

reg8toNum (S, s) :
    S[regs][regMap[s]][hilo s]

hilo(s) :
    s in {"a", "h", "b", "d"} :
        hi
    s in {"f", "l", "c", "e"} :
        lo
    else : error "Can't parse as 8-bit register."

csvToList(remainder) :
    trimWhitespace(remainder) == "" : []
    else : csver([], 0, 0)
given :
    csver = func(L, p, i) :
        i == len(remainder) : L + [trimWhitespace(remainder[p::i])]
        remainder[i] == "," : this L + [trimWhitespace(remainder[p::i])], i + 1, i + 1
        else : this (L, p, i + 1)

trimLeft(s) :
    s behead (while condition do action to 0)
given :
    condition = func(i) : i < len(s) and s[i] == " "
    action = func(i) : i + 1

trimRight(s) :
    s curtail (while condition do action to 0)
given :
    condition = func(i) : i < len(s) and s[len(s) - i - 1] == " "
    action = func(i) : i + 1

trimWhitespace(s) : trimLeft trimRight s

zeroedRegs = map ("hl" :: Num16(0,0), "bc" :: Num16(0,0), "de" :: Num16(0,0), "af" :: Num16(0,0))

hexToDecMap = map "0" :: 0, "1" :: 1, "2" :: 2, "3" :: 3, "4" :: 4, "5" :: 5, "6" :: 6, "7" :: 7, 
          .. "8" :: 8, "9" :: 9, "a" :: 10, "b" :: 11, "c" :: 12, "d" :: 13, "e" :: 14, "f" :: 15

decToHexList = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f"]

decToHex8(i) : decToHexList[i / 16] + decToHexList[i % 16]

hexToNum8(s) : 16 * hexToDecMap[s[0]] + hexToDecMap[s[1]]

decToNum16(i) : Num16(i / 256, i % 256)

hexToNum16(s) : Num16(hexToNum8(s[0::2]), hexToNum8(s[2::4]))

num16ToHex16(n Num16) : decToHex8(n[hi]) + decToHex8(n[lo])

num16ToDec(n Num16) : n[hi] * 256 + n[lo]

(m Num16) + (n Num16) :
    Num16( (m[hi] + n[hi] + (m[lo] + n[lo] >= 256 : 1; else: 0)) % 256, 
        .. (m[lo] + n[lo]) % 256 )

(m Num16) - (n Num16) :
    Num16( (256 + (m[hi] - n[hi]) + (m[lo] - n[lo] < 0 : 255; else: 0)) % 256, 
        .. (256 + m[lo] - n[lo]) % 256 )

prettyPrint(S State) :

    "\n   |  0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f\n" + ..
 .. "---|------------------------------------------------" + ..
 .. (while p do f to S[mem], "", 0)[1] + "\n\n" + ..
       .. "af: " + num16ToHex16(S[regs]["af"]) + "   " + ..
       .. "bc: " + num16ToHex16(S[regs]["bc"]) + "   " + ..
       .. "de: " + num16ToHex16(S[regs]["de"]) + "   " + ..
       .. "hl: " + num16ToHex16(S[regs]["hl"]) + "\n\n" + ..
       .. "stack :" + prettyStack(S[stack]) + "\n\n"
given :
    p = func(L, s, i) : i < len(S[mem])
    f = func(L, s, i) : S[mem] ,
                     .. s + (i % 16 : " "; else : "\n" + decToHex8(i / 16)  + " | ") + decToHex8(S[mem][i]) ,
                     .. i + 1

prettyStack(L) :
    (while condition do action to 0, "")[1]
given :
    condition = func(i, s) : i < len(L)
    action = func(i, s) : i + 1, s + " " + num16ToHex16 L[i]

listOf (n) times (t tuple) :
    for n do f to []
given : f = func(x) : x + [t]