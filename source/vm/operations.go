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

func (op *operation) ppConst(i int) string {
	return " c" + strconv.Itoa(int(op.args[i]))
}

func (op *operation) ppType(i int) string {
	name, ok := VALUE_MAP[op.args[i]]
	if ok {
		return " t #" + name
	}
	return " t" + strconv.Itoa(int(op.args[i]))
}

const LA = " <-"
const EQ = " =="
const CM = ","

func describe(op *operation) string {
	switch op.opcode {
	case asgc:
		return "asgc" + op.ppMem(0) + LA + op.ppConst(1)
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
	case halt:
		return "halt"
	case jmp:
		return "jmp" + op.ppLoc(0)
	case jsr:
		return "jsr" + op.ppLoc(0)
	case notb:
		return "notb" + op.ppMem(0) + LA + op.ppMem(1)
	case qtype:
		return "type" + op.ppMem(0) + EQ + op.ppType(1)
	case ret:
		return "ret"
	}
	return "indescribable thing"
}

const (
	jmp opcode = iota
	jsr
	ret
	qtype
	qenum
	qstruct // etc

	asgc // mem, const
	asgm // mem, mem

	cmp // mem, mem

	andb
	orb
	notb
	equb

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

	halt
)
