var

x = 42

cmd

zort :
    global x
    post (string x) to Output()

troz :
    x = "foo"
    post (string x) to Output()

spoit :
    post (string x) to Output()

duh :
    x = x + 1