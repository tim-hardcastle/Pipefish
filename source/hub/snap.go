package hub

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"pipefish/source/svc"
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

func MakeSnapIo(sv *svc.Service, out io.Writer, sn *Snap) (*snapInHandler, *snapOutHandler) {
	iH := snapInHandler{stdIn: svc.MakeStandardInHandler(""), snap: sn}
	oH := snapOutHandler{stdOut: sv.MakeLiteralWritingOutHandler(out), snap: sn}
	return &iH, &oH
}

type snapInHandler struct {
	stdIn  *svc.StandardInHandler
	snap   *Snap
	prompt string
}

type snapOutHandler struct {
	stdOut *svc.SimpleOutHandler
	snap   *Snap
	sv     *svc.Service
}

func (iH *snapInHandler) Get() string {
	iH.snap.AddOutput("\"" + iH.prompt + "\"")
	input := iH.stdIn.Get()
	iH.snap.AddInput(input)
	return input
}

func (oH *snapOutHandler) Out(v svc.Value) {
	oH.snap.AppendOutput(oH.sv.Literal(v))
	oH.stdOut.Out(v)
}

func (oH *snapOutHandler) Write(s string) {
	oH.stdOut.Write(s)
}

func snapFunctionMaker(sv *svc.Service) func(svc.Value) []byte {
	return func(v svc.Value) []byte {
		var out bytes.Buffer
		vals := v.V.([]svc.Value) // A snap always returns a tuple.
		elements := []string{}
		for _, e := range vals {
			elements = append(elements, sv.Literal(e))
		}
		out.WriteString(strings.Join(elements, ", "))
		out.WriteRune('\n')
		return out.Bytes()
	}
}

func MakeTestIoHandler(sv *svc.Service, out io.Writer, scanner *bufio.Scanner, testOutputType TestOutputType) (*TestInHandler, *TestOutHandler) {
	iH := &TestInHandler{out: out, stdIn: svc.MakeStandardInHandler(""), scanner: scanner, testOutputType: testOutputType}
	oH := &TestOutHandler{sv: sv, stdOut: sv.MakeLiteralWritingOutHandler(out), scanner: scanner, testOutputType: testOutputType}
	return iH, oH
}

type TestInHandler struct {
	stdIn          *svc.StandardInHandler
	out            io.Writer
	scanner        *bufio.Scanner
	Fail           bool
	testOutputType TestOutputType
}

type TestOutHandler struct {
	sv             *svc.Service
	stdOut         *svc.SimpleOutHandler
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

func (oH *TestOutHandler) Out(v svc.Value) {
	var out bytes.Buffer
	vals := v.V.([]svc.Value) // We make sure it is always passed a tuple.
	elements := []string{}
	for _, e := range vals {
		elements = append(elements, string(oH.sv.Literal(e)))
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
