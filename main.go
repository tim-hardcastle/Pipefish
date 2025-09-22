//
// Pipefish version 0.6.8
//
// Acknowledgments
//
// I began with Thorsten Ball’s Writing An Interpreter In Go (https://interpreterbook.com/) and the
// accompanying code, and although his language and mine differ very much in their syntax, semantics,
// implementation, and ambitions, I still owe him a considerable debt.
//
// I owe thanks to Laurence Morgan (lmorg on Github) for adding features to his readline library for
// my sake.
//
// Much gratitude is due to r/programminglanguages collectively for advice and encouragement.
//

package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"

	"github.com/tim-hardcastle/Pipefish/source/hub"
	"github.com/tim-hardcastle/Pipefish/source/text"
)

func main() {

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
			os.Stdout.WriteString("\nPipefish version " + hub.VERSION + ".\n\n")
			return
		case "-r", "--run", "run":
			hub.StartServiceFromCli()
		case "-t", "--tui", "tui": // Left blank to avoid the default.
		default:
			os.Stdout.WriteString("\nPipefish doesn't recognize the command '" + os.Args[1] + "'.\n")
			println()
			showhelp()
			os.Exit(1)
		}
	}
	
	fmt.Print(hub.Logo())
	// Test
	headString1 := "# Heading 1"
	headString2 := "## Heading 2"
	headString3 := "### Heading 3"
	headString4 := "#### Heading 4"
	normieString := "Here's a block quote, with some color, bold, italics, etc, to show what we can do."
	testString := "> Lorem ipsum <green>dolor<plain> sit amet, `consectetur adipiscing` elit, sed do eiusmod <purple>tempor incididunt ut labore<plain> et *dolore magna* aliqua." 
	testString2 := "> Ut enim ad **minim** veniam, quis <yellow>nostrud exercitation<plain> ullamco laboris nisi ut aliquip ex ea commodo consequat."
	normieString2 := "And now a block of code."
	
	dummyFunc := func(r []rune) string {return string(r)}
	md := text.NewMarkdown("  ", 70, dummyFunc)
	println(md.Render([]string{headString1, "", headString2, "", headString3, "", 
		headString4, "", normieString, "", testString, testString2, "", normieString2,
		}))
	println()
	// Test
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
	os.Stdout.WriteString(hub.HELP)
}
