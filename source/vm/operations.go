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

func (op *operation) ppFunc(i int) string {
	return " f" + strconv.Itoa(int(op.args[i]))
}

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
	case andb:
		return "andb" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case apnT:
		return "apnT" + op.ppMem(0) + LA + op.ppMem(1)
	case asgm:
		return "asgm" + op.ppMem(0) + LA + op.ppMem(1)
	case equb:
		return "equb" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case equf:
		return "equf" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case equi:
		return "equi" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case equs:
		return "equs" + op.ppMem(0) + LA + op.ppMem(1) + CM + op.ppMem(2)
	case call:
		result := "call " + op.ppLoc(1) + op.ppMem(2) + " ::" + op.ppMem(3) + " ("
		for i := 4; i < len(op.args); i++ {
			result = result + op.ppMem(i)
		}
		result = result + " )"
		return result
	case halt:
		return "halt"
	case idxT:
		return "idxT" + op.ppMem(0) + LA + op.ppMem(1) + op.ppInt(2)
	case jmp:
		return "jmp " + op.ppLoc(0)
	case jsr:
		return "jsr " + op.ppLoc(0)
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
	case thnk:
		return "thnk" + op.ppMem(0) + LA + op.ppLoc(1)
	case untk:
		return "untk" + op.ppMem(0)
	}
	return "indescribable thing"
}

func (op *operation) makeLastArg(loc uint32) {
	op.args[len(op.args)-1] = loc
}

const (
	jmp opcode = iota
	ret
	qtru
	qtyp
	qsng
	qsnQ
	qlnT
	asgm
	thnk
	untk
	call
	apnT
	idxT
	jsr

	halt // do we use this?

	andb
	orb
	notb
	equb

	cmp

	addi
	subi
	muli
	divi
	modi
	equi
	leqi

	addf
	subf
	mulf
	divf
	equf
	leqf

	equs
	lens
	adds
	idxs
	slis

	lenl
	addl
	ixl
	slil

	lent
	addt
	ixt
	slit

	keysm
	ixm

	makeS
	keysS
	resolveS // S, field
	getS     // S, key number
)
