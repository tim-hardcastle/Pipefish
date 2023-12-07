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

func describe(op *operation) string {
	switch op.opcode {
	case jmp:
		return "jmp" + op.ppLoc(0)
	case jsr:
		return "jsr" + op.ppLoc(0)
	case ret:
		return "ret"
	case asgnc:
		return "asgn" + op.ppMem(0) + LA + op.ppConst(1)
	case asgnm:
		return "asgn" + op.ppMem(0) + LA + op.ppMem(1)
	case halt:
		return "halt"
	case qtype:
		return "type" + op.ppMem(0) + EQ + op.ppType(1)
	}

	return "indescibable thing"
}

const (
	jmp opcode = iota
	jsr
	ret
	qtype
	qenum
	qstruct // etc

	asgnc // mem, const
	asgnm // mem, mem

	andb
	orb
	notb

	addi
	subi
	muli
	divi
	modi
	cmpi
	leqi

	addf
	subf
	mulf
	divf
	cmpf
	leqf

	lens
	adds
	ixs
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
