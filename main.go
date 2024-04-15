//
// Charm version 0.4.1
//
// Acknowledgments
//
// I began with Thorsten Ball’s Writing An Interpreter In Go (https://interpreterbook.com/) and the
// accompanying code, and although his language and mine differ very much in their syntax, semantics,
// implementation, and ambitions, I still owe him a considerable debt.
//
// Much gratitude is due to r/programminglanguages collectively for advice and encouragement.
//

package main

import (
	"fmt"
	"os"

	"pipefish/source/hub"
	"pipefish/source/text"
)

func main() {

	fmt.Print(text.Logo())

	hb := hub.New(os.Stdin, os.Stdout)
	hb.Open()
	argString := ""
	if len(os.Args) > 1 {
		for _, v := range os.Args[1:] {
			argString = argString + v + " "
		}
	}
	quit := false
	if argString != "" {
		verb, args := hb.ParseHubCommand(argString)
		if verb != "error" {
			quit = hb.DoHubCommand("", "", verb, args)
		}
	}
	if len(os.Args) == 1 || !quit {
		hub.StartHub(hb, os.Stdin, os.Stdout)
	}
}
