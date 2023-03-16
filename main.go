//
// Charm version 0.3.7
//
// Acknowledgments
//
// I began with Thorsten Ball’s Writing An Interpreter In Go (https://interpreterbook.com/) and the
// accompanying code, and although his language and mine differ very much in their syntax, semantics,
// implementation, and ambitions, I still owe him a considerable debt.
//

package main

import (
	"fmt"
	"os"

	"charm/source/hub"
	"charm/source/repl"
	"charm/source/text"
)

func main() {

	fmt.Print(text.Logo())

	hub := hub.New(os.Stdin, os.Stdout)
	hub.Open()
	argString := ""
	if len(os.Args) > 1 {
		for _,v := range(os.Args[1:]) {
			argString = argString + v + " "
		}
	}
	quit := false
	if argString != "" {
		verb, args := hub.ParseHubCommand(argString)
		if verb != "error"  {
			quit = hub.DoHubCommand("", "", verb, args)
		}
	}
	if len(os.Args) == 1 || !quit { // Thus taking care of the case where some cheeky
		repl.Start(hub, os.Stdin, os.Stdout)  // goose starts it up with ./charm quit
	}
}
