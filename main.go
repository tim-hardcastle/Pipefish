//
// Pipefish version 0.4.9
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
	"bufio"
	"fmt"
	"os"
	"path/filepath"

	"pipefish/source/hub"
	"pipefish/source/text"
)

func main() {

	if len(os.Args) == 1 {
		showhelp()
		return
	}
	if len(os.Args) > 1 {
		switch os.Args[1] {
		case "-h", "--help":
			showhelp()
			return
		case "-v", "--version":
			os.Stdout.WriteString("\nPipefish version " + text.VERSION + ".\n\n")
			return
		case "tui": // Left blank to avoid the default.
		default:
			os.Stdout.WriteString("\nPipefish doesn't recognize the command " + text.Emph(os.Args[1]) + ".\n")
			println()
			showhelp()
			os.Exit(1)
		}
	}

	fmt.Print(text.Logo())

	h := hub.New(os.Stdin, os.Stdout)
	appDir, _ := filepath.Abs(filepath.Dir(os.Args[0]))
	f, err := os.Open(appDir + "/user/hubloc.dat")
	if err != nil {
		h.WriteError(err.Error())
		panic("That's all folks!")
	}
	scanner := bufio.NewScanner(f)
	scanner.Scan()
	scanner.Scan()
	scanner.Scan()
	line := scanner.Text()
	fname := line[8 : len(line)-1]
	h.OpenHubFile(fname)
	hub.StartHub(h, os.Stdin, os.Stdout)
}

func showhelp() {
	os.Stdout.WriteString(text.HELP)
}
