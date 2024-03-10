package vm

import "strconv"

func MakeOp(oc Opcode, args ...uint32) *Operation {
	return &Operation{Opcode: oc, Args: args}
}

type Operation struct {
	Opcode Opcode
	Args   []uint32
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

type Opcode uint8

func (op *Operation) ppOperand(i int) string {
	opType := OPERANDS[op.Opcode].or[i]
	opVal := strconv.Itoa(int(op.Args[i]))
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
		args := op.Args[i:]
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

var OPERANDS = map[Opcode]opDescriptor{
	Addf: {"addf", operands{dst, mem, mem}},
	Addi: {"addi", operands{dst, mem, mem}},
	Adds: {"adds", operands{dst, mem, mem}},
	Adtk: {"adtk", operands{dst, mem, tok}},
	Andb: {"andb", operands{dst, mem, mem}},
	Asgm: {"asgm", operands{dst, mem}},
	Call: {"call", operands{loc, mem, mem, tup}}, // The location to call, the bottom and (exclusive) top of where to put the parameters; and a tuple saying where to get them from.
	Cc11: {"cc11", operands{dst, mem, mem}},
	Cc1T: {"cc1T", operands{dst, mem, mem}},
	CcT1: {"ccT1", operands{dst, mem, mem}},
	CcTT: {"ccTT", operands{dst, mem, mem}},
	Ccxx: {"ccxx", operands{dst, mem, mem}},
	Cv1T: {"cv1T", operands{dst, mem, mem}},
	Divf: {"divf", operands{dst, mem, mem}},
	Divi: {"divi", operands{dst, mem, mem}},
	Dofn: {"dofn", operands{dst, mem, tup}},
	Dref: {"dref", operands{dst, mem}},
	Equb: {"equb", operands{dst, mem, mem}},
	Equf: {"equf", operands{dst, mem, mem}},
	Equi: {"equi", operands{dst, mem, mem}},
	Equs: {"equs", operands{dst, mem, mem}},
	Flti: {"flti", operands{dst, mem}},
	Flts: {"flts", operands{dst, mem}},
	Gtef: {"gtef", operands{dst, mem, mem}},
	Gtei: {"gtei", operands{dst, mem, mem}},
	Gthf: {"gthf", operands{dst, mem, mem}},
	Gthi: {"gthi", operands{dst, mem, mem}},
	Halt: {"halt", operands{}},
	IdxT: {"idxT", operands{dst, mem, mem}},
	Intf: {"intf", operands{dst, mem}},
	Ints: {"ints", operands{dst, mem}},
	Jmp:  {"jmp", operands{loc}},
	Jsr:  {"jsr", operands{loc}},
	Lens: {"lens", operands{dst, mem}},
	Litx: {"litx", operands{dst, mem}},
	List: {"list", operands{dst, mem}},
	Mker: {"mker", operands{dst, mem, tok}},
	Mkfn: {"mkfn", operands{dst, lfc}},
	Mkmp: {"mkmp", operands{dst, mem}},
	Mkpr: {"mkpr", operands{dst, mem, mem}},
	Mkst: {"mkst", operands{dst, mem}},
	Modi: {"modi", operands{dst, mem, mem}},
	Mulf: {"mulf", operands{dst, mem, mem}},
	Muli: {"muli", operands{dst, mem, mem}},
	Negf: {"negf", operands{dst, mem}},
	Negi: {"negi", operands{dst, mem}},
	Notb: {"notb", operands{dst, mem}},
	Orb:  {"orb", operands{dst, mem, mem}},
	QlnT: {"qlnT", operands{mem, num, loc}},
	QsnQ: {"qsnQ", operands{mem, loc}},
	Qsng: {"qsng", operands{mem, loc}},
	Qtru: {"qtru", operands{mem, loc}},
	Qtyp: {"qtyp", operands{mem, typ, loc}},
	Ret:  {"ret", operands{}},
	Strx: {"strx", operands{dst, mem}},
	Subf: {"subf", operands{dst, mem, mem}},
	Subi: {"subi", operands{dst, mem, mem}},
	Thnk: {"thnk", operands{dst, loc}},
	Typx: {"typx", operands{dst, mem}},
	Untk: {"untk", operands{dst}},
}

func describe(op *Operation) string {
	operands := OPERANDS[op.Opcode].or
	result := OPERANDS[op.Opcode].oc
	for i := range operands {
		result = result + op.ppOperand(i)
	}
	return result
}

func (op *Operation) MakeLastArg(loc uint32) {
	op.Args[len(op.Args)-1] = loc
}

const (
	Addf Opcode = iota
	Addi
	addl
	Adds
	Adtk
	Andb
	Cc11
	Cc1T
	CcT1
	CcTT
	Ccxx
	Cv1T
	Asgm
	Call
	cmp
	Divf
	Divi
	Dofn // For lambdas, as opposed to call for outer functions.
	Dref
	Equb
	Equf
	Equi
	Equs
	Flti
	Flts
	getS // S, key number
	Gtef
	Gtei
	Gthf
	Gthi
	Halt // do we use this?
	idxl
	idxm
	idxs
	IdxT
	Ints
	Intf
	Jmp
	Jsr
	keym
	keyS
	lenl
	Lens
	lenT
	leqf
	leqi
	List
	Litx
	Mker
	Mkfn
	Mkmp
	Mkpr
	Mkst
	makS
	Modi
	Mulf
	Muli
	Negf
	Negi
	Notb
	Orb
	resS // S, field
	Qtru
	Qtyp
	Qsng
	QsnQ
	QlnT
	Ret
	slil
	slis
	sliT
	Strx
	Subf
	Subi
	Thnk
	Typx
	Untk
)
