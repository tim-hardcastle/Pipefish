import "lib/prelude.ch" :: ""

cmd

add (n string, s int) :
    data = data + [Student(n, s)]

show :
    return prettyPrint(data, formula)

use(f) :
    formula = f

sort :
    data = mergesort data

var private

data = []
formula = func(x) : "N/A"

def private

Student = struct(name string, score int)

(s Student) < (t Student) : s[score] > t[score]

prettyPrint(data, formula) :
    "\n    # | Name                 | Score   | Adjusted\n" ..
    .. + "-------------------------------------------------\n" ..
    .. + (while condition do action to 0, "")[1] + "\n"
given:
    condition(i, s) : i < len(data)
    action(i, s) : i + 1, s + prettyString(data[i], formula, i)

prettyString(item, formula, number) : 
    spaces(5 - len string number) + string(number) + " | "  + item[name] ..
    .. + spaces(20 - len item[name]) + " | " + spaces(7 - len string item[score]) ..
    .. + string(item[score]) + " | " + spaces(7 - len string formula item[score]) ..
    .. + string (formula item[score]) + "\n"

spaces (n) : for n do action to ""
given :
    action(x) : x + " "
