package vm

import "strconv"

func makeOp(oc opcode, args ...uint32) *operation {
	return &operation{opcode: oc, args: args}
}

type operation struct {
	opcode opcode
	args   []uint32
}

type operandType uint8

const (
	dst operandType = iota
	mem
	lfc
	loc
	typ
	num
	tok
	tup
)

type operands []operandType

type opcode uint8

func (op *operation) ppOperand(i int) string {
	opType := OPERANDS[op.opcode].or[i]
	opVal := strconv.Itoa(int(op.args[i]))
	switch opType {
	case dst:
		return " m" + opVal + " <-"
	case lfc:
		return " Λ" + opVal
	case loc:
		return " @" + opVal
	case mem:
		return " m" + opVal
	case num:
		return " %" + opVal
	case tok:
		return " TK" + opVal
	case tup:
		args := op.args[i:]
		result := " ("
		for i, v := range args {
			result = result + "m" + strconv.Itoa(int(v))
			if i < len(args)-1 {
				result = result + " "
			}
		}
		return result + ")"
	case typ:
		return " t" + opVal
	}
	panic("Unknown operand type")
}

type opDescriptor struct {
	oc string
	or []operandType
}

var OPERANDS = map[opcode]opDescriptor{
	addf: {"addf", operands{dst, mem, mem}},
	addi: {"addi", operands{dst, mem, mem}},
	adds: {"adds", operands{dst, mem, mem}},
	adtk: {"adtk", operands{dst, tok}},
	andb: {"andb", operands{dst, mem, mem}},
	asgm: {"asgm", operands{dst, mem}},
	call: {"call", operands{loc, mem, mem, tup}}, // The location to call, the bottom and (exclusive) top of where to put the parameters; and a tuple saying where to get them from.
	cc11: {"cc11", operands{dst, mem, mem}},
	cc1T: {"cc1T", operands{dst, mem, mem}},
	ccT1: {"ccT1", operands{dst, mem, mem}},
	ccTT: {"ccTT", operands{dst, mem, mem}},
	ccxx: {"ccxx", operands{dst, mem, mem}},
	cv1T: {"cv1T", operands{dst, mem, mem}},
	divf: {"divf", operands{dst, mem, mem}},
	divi: {"divi", operands{dst, mem, mem}},
	dofn: {"dofn", operands{dst, mem, tup}},
	dref: {"dref", operands{dst, mem}},
	equb: {"equb", operands{dst, mem, mem}},
	equf: {"equf", operands{dst, mem, mem}},
	equi: {"equi", operands{dst, mem, mem}},
	equs: {"equs", operands{dst, mem, mem}},
	flti: {"flti", operands{dst, mem}},
	flts: {"flts", operands{dst, mem}},
	gtef: {"gtef", operands{dst, mem, mem}},
	gtei: {"gtei", operands{dst, mem, mem}},
	gthf: {"gthf", operands{dst, mem, mem}},
	gthi: {"gthi", operands{dst, mem, mem}},
	halt: {"halt", operands{}},
	idxT: {"idxT", operands{dst, mem, mem}},
	intf: {"intf", operands{dst, mem}},
	ints: {"ints", operands{dst, mem}},
	jmp:  {"jmp", operands{loc}},
	jsr:  {"jsr", operands{loc}},
	lens: {"lens", operands{dst, mem}},
	litx: {"lits", operands{dst, mem}},
	mker: {"mker", operands{dst, mem, tok}},
	mkfn: {"mkfn", operands{dst, lfc}},
	modi: {"modi", operands{dst, mem, mem}},
	mulf: {"mulf", operands{dst, mem, mem}},
	muli: {"muli", operands{dst, mem, mem}},
	negf: {"negf", operands{dst, mem}},
	negi: {"negi", operands{dst, mem}},
	notb: {"notb", operands{dst, mem}},
	orb:  {"orb", operands{dst, mem, mem}},
	qlnT: {"qlnT", operands{mem, num, loc}},
	qsnQ: {"qsnQ", operands{mem, loc}},
	qsng: {"qsng", operands{mem, loc}},
	qtru: {"qtru", operands{mem, loc}},
	qtyp: {"qtyp", operands{mem, typ, loc}},
	ret:  {"ret", operands{}},
	strx: {"strx", operands{dst, mem}},
	subf: {"subf", operands{dst, mem, mem}},
	subi: {"subi", operands{dst, mem, mem}},
	thnk: {"thnk", operands{dst, loc}},
	typx: {"typx", operands{dst, mem}},
	untk: {"untk", operands{dst}},
}

func describe(op *operation) string {
	operands := OPERANDS[op.opcode].or
	result := OPERANDS[op.opcode].oc
	for i := range operands {
		result = result + op.ppOperand(i)
	}
	return result
}

func (op *operation) makeLastArg(loc uint32) {
	op.args[len(op.args)-1] = loc
}

const (
	addf opcode = iota
	addi
	addl
	adds
	adtk
	andb
	cc11
	cc1T
	ccT1
	ccTT
	ccxx
	cv1T
	asgm
	call
	cmp
	divf
	divi
	dofn // For lambdas, as opposed to call for outer functions.
	dref
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
	mker
	mkfn
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
