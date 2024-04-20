package service

import (
	"fmt"
	"os"
	"path/filepath"
	"pipefish/source/text"
	"strings"
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
