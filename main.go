//
// Charm version 0.2.1
//
// Acknowledgments
//
// I began with Thorsten Ball’s Writing An Interpreter In Go (https://interpreterbook.com/) and the 
// accompanying code, and although his language and mine differ very much in their syntax, semantics, 
// implementation, and ambitions, I still owe him a considerable debt.
//
// The readline library is by Lawrence Morgan. https://github.com/lmorg
//


package main

import (
	"fmt"
	"os"
	"charm/hub"
	"charm/repl"
	"charm/text"

)

func main() {

	fmt.Print(text.Logo())
	
	hub := hub.New(os.Stdin, os.Stdout)
	hub.Open()
	if len(os.Args) == 1 || !hub.ParseHubCommand(os.Args[1:]) { // Thus taking care of the case where some cheeky 
		repl.Start(hub, os.Stdin, os.Stdout)                    // goose starts it up with ./charm quit
	}
} 


