//
// Pipefish version 0.5.9
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
	"runtime"

	"pipefish/source/hub"
	"pipefish/source/settings"
	"pipefish/source/text"
)

func main() {

	if runtime.GOOS == "windows" { // This allows a cut-down version that doesn't require the plugins package.
		settings.MandatoryImports = []string{"service/rsc/pipefish/builtins.pf", "service/rsc/pipefish/interfaces.pf"}
	}

	if len(os.Args) == 1 {
		showhelp()
		return
	}
	if len(os.Args) > 1 {
		switch os.Args[1] {
		case "-h", "--help", "help":
			showhelp()
			return
		case "-v", "--version", "version":
			os.Stdout.WriteString("\nPipefish version " + text.VERSION + ".\n\n")
			return
		case "-t", "--tui", "tui": // Left blank to avoid the default.
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
	f, err := os.Open(filepath.Join(appDir, filepath.FromSlash("/user/hub.dat")))
	if err != nil {
		println(err.Error())
		panic("That's all folks!")
	}
	scanner := bufio.NewScanner(f)
	scanner.Scan()
	line := scanner.Text()
	h.OpenHubFile(line)
	hub.StartHub(h, os.Stdin, os.Stdout)
}

func showhelp() {
	os.Stdout.WriteString(text.HELP)
}
