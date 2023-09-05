cmd

tryWithoutCapture :
    try :
        get x from MadeUpThing()
    else :
        post "Well, that didn't work." to Output()


tryWithCapture :
    try e :
        get x from MadeUpThing()
    else :
        post "Well, that didn't work." to Output()
        post e to Output()


dividePotentialNulls(a, b int?) :
    a in null or b in null :
        post NULL to Output()
    try :
        post a / b to Output()
    else :
        error "You tried to divide by zero"
