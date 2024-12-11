package hub

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"pipefish/source/pf"
	"pipefish/source/text"
	"pipefish/source/values"
)

type ioPair struct {
	input  string
	output string
}

type Snap struct {
	testFilename   string
	scriptFilepath string
	ioList         []ioPair
}

const (
	BAD    = "bad"
	GOOD   = "good"
	RECORD = "record"
)

func NewSnap(scriptFilepath, testFilename string) *Snap {
	sn := Snap{scriptFilepath: scriptFilepath, testFilename: testFilename, ioList: []ioPair{}}
	return &sn
}

func (sn *Snap) AddInput(s string) {
	ioPair := ioPair{input: s, output: ""}
	sn.ioList = append(sn.ioList, ioPair)
}

func (sn *Snap) AddOutput(s string) {
	sn.ioList[len(sn.ioList)-1].output = s
}

func (sn *Snap) AppendOutput(s string) {
	sn.ioList[len(sn.ioList)-1].output = sn.ioList[len(sn.ioList)-1].output + s
}

func (sn *Snap) Save(st string) string {
	snapOutput := fmt.Sprintf("snap: %v\nscript: %v\n", st, sn.scriptFilepath)
	for _, v := range (*sn).ioList {
		snapOutput = snapOutput + "\n" + "-> " + v.input + "\n" + v.output
	}
	fname := filepath.Base((*sn).scriptFilepath)
	fname = fname[:len(fname)-len(filepath.Ext(fname))]
	dname := filepath.Dir((*sn).scriptFilepath)
	directoryName := dname + "/-tests/" + fname
	err := os.MkdirAll(directoryName, 0777)
	if err != nil {
		return text.HUB_ERROR + "os reports \"" + strings.TrimSpace(err.Error()) + "\".\n"
	}
	testFilepath := directoryName + "/" + sn.testFilename
	f, err := os.Create(testFilepath)
	if err != nil {
		return text.HUB_ERROR + "os reports \"" + strings.TrimSpace(err.Error()) + "\".\n"
	}
	defer f.Close()

	f.WriteString(snapOutput)

	return "Created test as file " + text.Emph(testFilepath) + "."
}


func MakeSnapIo(out io.Writer, sn *Snap) (pf.InHandler, pf.OutHandler) {
	iH := snapInHandler{stdIn: pf.STANDARD_INPUT, snap: sn}
	oH := snapOutHandler{stdOut: pf.MakeSimpleOutHandler(out), snap: sn}
	return &iH, &oH
}

type snapInHandler struct {
	stdIn pf.StandardInHandler
	snap  *Snap
}

type snapOutHandler struct {
	stdOut pf.SimpleOutHandler
	snap   *Snap
}

func (iH *snapInHandler) Get(prompt string) string {
	iH.snap.AddOutput("\"" + prompt + "\"")
	input := iH.stdIn.Get(prompt)
	iH.snap.AddInput(input)
	return input
}

func (oH *snapOutHandler) Out(v values.Value, fn func(values.Value)[]byte) {
	oH.snap.AppendOutput(string(fn(v)))
	oH.stdOut.Out(v, fn)
}

func (oH *snapOutHandler) Write(s string) {
	oH.stdOut.Write(s)
}

func snapFunctionMaker(sv *pf.Service) func(values.Value)[]byte {
	return func (v values.Value) []byte {
		var out bytes.Buffer
		vals := v.V.([]values.Value) // A snap always returns a tuple.
		elements := []string{}
		for _, e := range vals {
			elements = append(elements, sv.Literal(e))
		}
		out.WriteString(strings.Join(elements, ", "))
		out.WriteRune('\n')
		return out.Bytes()
	}
}

func MakeTestIoHandler(out io.Writer, scanner *bufio.Scanner, testOutputType TestOutputType) (pf.InHandler, pf.OutHandler) {
	iH := &TestInHandler{out: out, stdIn: pf.STANDARD_INPUT, scanner: scanner, testOutputType: testOutputType}
	oH := &TestOutHandler{stdOut: pf.MakeSimpleOutHandler(out), scanner: scanner, testOutputType: testOutputType}
	return iH, oH
}

type TestInHandler struct {
	stdIn          pf.StandardInHandler
	out            io.Writer
	scanner        *bufio.Scanner
	Fail           bool
	testOutputType TestOutputType
}

type TestOutHandler struct {
	stdOut         pf.SimpleOutHandler
	scanner        *bufio.Scanner
	Fail           bool
	testOutputType TestOutputType
}

func (iH *TestInHandler) Get(prompt string) string {
	iH.scanner.Scan()
	expectedPrompt := iH.scanner.Text()
	gotPrompt := "\"" + prompt + "\""

	switch iH.testOutputType {
	case ERROR_CHECK:
		iH.Fail = iH.Fail || expectedPrompt != gotPrompt
	case SHOW_ALL:
		if expectedPrompt != gotPrompt {
			iH.out.Write([]byte(text.WAS + expectedPrompt))
			iH.out.Write([]byte("\n" + text.GOT + gotPrompt + "\n"))
		} else {
			iH.out.Write([]byte(gotPrompt + "\n"))
		}
	case SHOW_DIFF:
		if expectedPrompt != gotPrompt {
			iH.out.Write([]byte(text.WAS + expectedPrompt))
			iH.out.Write([]byte("\n" + text.GOT + gotPrompt + "\n"))
		}
	}
	iH.scanner.Scan()
	input := iH.scanner.Text()
	if iH.testOutputType == SHOW_ALL {
		iH.out.Write([]byte(input + "\n"))
	}
	return input[3:]
}

func (oH *TestOutHandler) Out(v values.Value, fn func(values.Value)[]byte) {

	var out bytes.Buffer
	vals := v.V.([]values.Value)
	elements := []string{}
	for _, e := range vals {
		elements = append(elements, string(fn(e)))
	}
	out.WriteString(strings.Join(elements, ", "))
	oH.scanner.Scan()
	getExpected := oH.scanner.Text()
	getGot := out.String()
	switch oH.testOutputType {
	case ERROR_CHECK:
		oH.Fail = oH.Fail || getExpected != getGot
	case SHOW_ALL:
		if getExpected != getGot {
			oH.Write(text.WAS + getExpected)
			oH.Write("\n" + text.GOT + getGot + "\n")
		} else {
			oH.Write(getGot)
		}
	case SHOW_DIFF:
		if getExpected != getGot {
			oH.Write(text.WAS + getExpected)
			oH.Write("\n" + text.GOT + getGot + "\n")
		}
	}
}

func (oH *TestOutHandler) Write(s string) {
	oH.stdOut.Write(s)
}

type TestOutputType int

const (
	ERROR_CHECK TestOutputType = iota
	SHOW_ALL
	SHOW_DIFF
)
