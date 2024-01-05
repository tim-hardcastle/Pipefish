package vm

import "strconv"

func makeOp(oc opcode, args ...uint32) *operation {
	return &operation{opcode: oc, args: args}
}

type operation struct {
	opcode opcode
	args   []uint32
}

type opcode uint8

func (op *operation) ppLoc(i int) string {
	return " @" + strconv.Itoa(int(op.args[i]))
}

func (op *operation) ppMem(i int) string {
	return " m" + strconv.Itoa(int(op.args[i]))
}

func (op *operation) ppType(i int) string {
	return " t" + strconv.Itoa(int(op.args[i]))
}

func (op *operation) ppInt(i int) string {
	return " %" + strconv.Itoa(int(op.args[i]))
}

const LA = " <-"
const FN = " <- func"
const TP = " is"
const CM = ","
const LS = " else"

func describe(op *operation) string {
	switch op.opcode {
	case addf:
		return "addf" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case addi:
		return "addi" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case adds:
		return "adds" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case andb:
		return "andb" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case asgm:
		return "asgm" + op.ppMem(0) + LA + op.ppMem(1)
	case cc11:
		return "cc11" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case cc1T:
		return "cc1T" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case ccT1:
		return "ccT1" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case ccTT:
		return "ccTT" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case ccxx:
		return "ccxx" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case divf:
		return "divf" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case divi:
		return "divi" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case equb:
		return "equb" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case equf:
		return "equf" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case equi:
		return "equi" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case equs:
		return "equs" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case call:
		result := "call" + op.ppLoc(0) + op.ppMem(1) + " ::" + op.ppMem(2) + " ("
		for i := 3; i < len(op.args); i++ {
			result = result + op.ppMem(i)
		}
		result = result + " )"
		return result
	case flti:
		return "flti" + op.ppMem(0) + LA + op.ppMem(1)
	case flts:
		return "flts" + op.ppMem(0) + LA + op.ppMem(1)
	case gtef:
		return "gtef" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case gtei:
		return "gtei" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case gthf:
		return "gthf" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case gthi:
		return "gthi" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case halt:
		return "halt"
	case idxT:
		return "idxT" + op.ppMem(0) + LA + op.ppMem(1) + op.ppInt(2)
	case intf:
		return "intf" + op.ppMem(0) + LA + op.ppMem(1)
	case ints:
		return "ints" + op.ppMem(0) + LA + op.ppMem(1)
	case jmp:
		return "jmp " + op.ppLoc(0)
	case jsr:
		return "jsr " + op.ppLoc(0)
	case lens:
		return "lens" + op.ppMem(0) + LA + op.ppMem(1)
	case litx:
		return "litx" + op.ppMem(0) + LA + op.ppMem(1)
	case modi:
		return "modi" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case mulf:
		return "mulf" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case muli:
		return "muli" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case negf:
		return "negf" + op.ppMem(0) + LA + op.ppMem(1)
	case negi:
		return "negi" + op.ppMem(0) + LA + op.ppMem(1)
	case notb:
		return "notb" + op.ppMem(0) + LA + op.ppMem(1)
	case orb:
		return "orb " + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case qtru:
		return "qtru" + op.ppMem(0) + LS + op.ppLoc(1)
	case qlnT:
		return "qlnT" + op.ppMem(0) + TP + op.ppInt(1) + LS + op.ppLoc(2)
	case qtyp:
		return "qtyp" + op.ppMem(0) + TP + op.ppType(1) + LS + op.ppLoc(2)
	case qsng:
		return "qsng" + op.ppMem(0) + LS + op.ppLoc(1)
	case qsnQ:
		return "qsnQ" + op.ppMem(0) + LS + op.ppLoc(1)
	case ret:
		return "ret "
	case strx:
		return "strx" + op.ppMem(0) + LA + op.ppMem(1)
	case subf:
		return "subf" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case subi:
		return "subi" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case thnk:
		return "thnk" + op.ppMem(0) + LA + op.ppLoc(1)
	case typx:
		return "typx" + op.ppMem(0) + LA + op.ppMem(1)
	case untk:
		return "untk" + op.ppMem(0)
	}
	return "indescribable thing"
}

func (op *operation) makeLastArg(loc uint32) {
	op.args[len(op.args)-1] = loc
}

const (
	addf opcode = iota
	addi
	addl
	adds
	andb
	cc11
	cc1T
	ccT1
	ccTT
	ccxx
	asgm
	call
	cmp
	divf
	divi
	equb
	equf
	equi
	equs
	flti
	flts
	getS // S, key number
	gtef
	gtei
	gthf
	gthi
	halt // do we use this?
	idxl
	idxm
	idxs
	idxT
	ints
	intf
	jmp
	jsr
	keym
	keyS
	lenl
	lens
	lenT
	leqf
	leqi
	litx
	makS
	modi
	mulf
	muli
	negf
	negi
	notb
	orb
	resS // S, field
	qtru
	qtyp
	qsng
	qsnQ
	qlnT
	ret
	slil
	slis
	sliT
	strx
	subf
	subi
	thnk
	typx
	untk
)
