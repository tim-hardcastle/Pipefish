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
        post e to Output()


