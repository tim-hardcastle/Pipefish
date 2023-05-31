var

x = 42

cmd

zort :
    global x
    x = x + 1
    post (string x) to Output()

troz :
    x = "foo"
    post (string x) to Output()
