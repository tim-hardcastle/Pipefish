package hub

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"github.com/tim-hardcastle/Pipefish/source/pf"
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
		return HUB_ERROR + "os reports \"" + strings.TrimSpace(err.Error()) + "\".\n"
	}
	testFilepath := directoryName + "/" + sn.testFilename
	f, err := os.Create(testFilepath)
	if err != nil {
		return HUB_ERROR + "os reports \"" + strings.TrimSpace(err.Error()) + "\".\n"
	}
	defer f.Close()

	f.WriteString(snapOutput)

	return "Created test as file " + Cyan("'"+testFilepath+"'") + "."
}

func MakeSnapIo(sv *pf.Service, out io.Writer, sn *Snap) (*snapInHandler, *snapOutHandler) {
	iH := snapInHandler{stdIn: pf.MakeTerminalInHandler(""), snap: sn}
	oH := snapOutHandler{stdOut: sv.MakeWritingOutHandler(out), snap: sn}
	return &iH, &oH
}

type snapInHandler struct {
	stdIn  *pf.TerminalInHandler
	snap   *Snap
	prompt string
}

type snapOutHandler struct {
	stdOut *pf.SimpleOutHandler
	snap   *Snap
	sv     *pf.Service
}

func (iH *snapInHandler) Get() string {
	iH.snap.AddOutput("\"" + iH.prompt + "\"")
	input := iH.stdIn.Get()
	iH.snap.AddInput(input)
	return input
}

func (oH *snapOutHandler) Out(v pf.Value) {
	oH.snap.AppendOutput(oH.sv.ToLiteral(v))
	oH.stdOut.Out(v)
}

func (oH *snapOutHandler) Write(s string) {
	oH.stdOut.Write(s)
}

func snapFunctionMaker(sv *pf.Service) func(pf.Value) []byte {
	return func(v pf.Value) []byte {
		var out bytes.Buffer
		vals := v.V.([]pf.Value) // A snap always returns a tuple.
		elements := []string{}
		for _, e := range vals {
			elements = append(elements, sv.ToLiteral(e))
		}
		out.WriteString(strings.Join(elements, ", "))
		out.WriteRune('\n')
		return out.Bytes()
	}
}

func MakeTestIoHandler(sv *pf.Service, out io.Writer, scanner *bufio.Scanner, testOutputType TestOutputType) (*TestInHandler, *TestOutHandler) {
	iH := &TestInHandler{out: out, stdIn: pf.MakeTerminalInHandler(""), scanner: scanner, testOutputType: testOutputType}
	oH := &TestOutHandler{sv: sv, stdOut: sv.MakeWritingOutHandler(out), scanner: scanner, testOutputType: testOutputType}
	return iH, oH
}

type TestInHandler struct {
	stdIn          *pf.TerminalInHandler
	out            io.Writer
	scanner        *bufio.Scanner
	Fail           bool
	testOutputType TestOutputType
}

type TestOutHandler struct {
	sv             *pf.Service
	stdOut         *pf.SimpleOutHandler
	scanner        *bufio.Scanner
	Fail           bool
	testOutputType TestOutputType
}

func (iH *TestInHandler) Get() string {
	iH.scanner.Scan()
	prompt := iH.scanner.Text()
	if iH.testOutputType == SHOW_ALL {
		iH.out.Write([]byte(prompt + "\n"))
	}
	iH.scanner.Scan()
	input := iH.scanner.Text()
	if iH.testOutputType == SHOW_ALL {
		iH.out.Write([]byte(input + "\n"))
	}
	return input[3:]
}

func (oH *TestOutHandler) Out(v pf.Value) {
	var out bytes.Buffer
	vals := v.V.([]pf.Value) // We make sure it is always passed a tuple.
	elements := []string{}
	for _, e := range vals {
		elements = append(elements, string(oH.sv.ToLiteral(e)))
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
			oH.Write(WAS + getExpected)
			oH.Write("\n" + GOT + getGot + "\n")
		} else {
			oH.Write(getGot)
		}
	case SHOW_DIFF:
		if getExpected != getGot {
			oH.Write(WAS + getExpected)
			oH.Write("\n" + GOT + getGot + "\n")
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
