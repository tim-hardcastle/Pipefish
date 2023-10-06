cmd

greet :
	get name from Input("What's your name? ")
	post "Hello " + name + "!"

def

factorial (n) :
	n == 0 : 1
	n > 0 : n * factorial n - 1
	else : error "can't take the factorial of a negative number"

