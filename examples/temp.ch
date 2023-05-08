cmd 

repl :
    loop :
        get name from Input("What's your name? ")
        name == "quit" :
            break
        else :
            post "Hello " + name + "!" to Output()            

foo(x) :
    x == 0 :
        z = 0
    x == 1 :
        z = 1
    else :
        error "oopsie"

        
