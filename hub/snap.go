package hub

import (
	"fmt"
	"os"
	"strings"
	"charm/text"
)

type ioPair struct{
	input string
	output string
}

type Snap struct{
	testFilename string
	scriptFilepath string
	dataFilepath string
	ioList []ioPair
}

const (
	BAD    = "bad"
	GOOD   = "good"
	RECORD = "record"
)

func NewSnap(scriptFilepath, testFilename, dataFilepath string) *Snap {
	sn := Snap{scriptFilepath: scriptFilepath, testFilename: testFilename, dataFilepath: dataFilepath,
	/**/	ioList: []ioPair{}}
	return &sn
}

func (sn *Snap) AddInput(s string) {
	ioPair := ioPair{input: s, output: ""}
	sn.ioList = append(sn.ioList, ioPair)
}

func (sn *Snap) AddOutput(s string) {
	sn.ioList[len(sn.ioList) - 1].output = s
}

func (sn *Snap) Save(st string) string {
	snapOutput := fmt.Sprintf("snap: %v\nscript: %v\ndata: %v\n", st, sn.scriptFilepath, sn.dataFilepath)
	for _, v := range((*sn).ioList) {
		snapOutput = snapOutput + "\n" + "-> " + v.input + "\n" + v.output
	}
	
	directoryName := "tst/" + text.FlattenedFilename((*sn).scriptFilepath)
	err := os.MkdirAll(directoryName, 0777)
	if err != nil {
		return text.ERROR + strings.TrimSpace(err.Error())
	}
	testFilepath := directoryName + "/" + sn.testFilename
	f, err := os.Create(testFilepath)
		if err != nil {
			return text.ERROR + strings.TrimSpace(err.Error())
		}
	defer f.Close()
	
	f.WriteString(snapOutput)

	return "Created test as file " + text.Emph(testFilepath) + "."
}

