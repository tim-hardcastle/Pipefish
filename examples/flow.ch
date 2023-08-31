var 

cmd

seq :
    post "It's just one thing ..." to Output()
    post "... after another." to Output()

check :
    5 % 2 == 0 :
        post "5 is even" to Output()
    else :
        post "5 is odd" to Output()
    6 % 2 == 0 :
        post "6 is even" to Output()
    else :
        post "6 is odd" to Output()

demoLoop :
    loop :
        get userInput from Input("Guess my number! > ")
        int userInput == 7 :
            post "Correct!" to Output()
            break
        else :
            post "Wrong! Guess again!" to Output()

    