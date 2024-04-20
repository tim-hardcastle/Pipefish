package service

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
	gfn
	lfc
	loc
	mem
	num
	sfc
	tok
	tup
	typ
)

type operands []operandType

type Opcode uint8

func (op *Operation) ppOperand(i int) string {
	opType := OPERANDS[op.Opcode].or[i]
	if i >= len(op.Args) {
		panic("Not enough operands for " + OPERANDS[op.Opcode].oc)
	}
	opVal := strconv.Itoa(int(op.Args[i]))
	switch opType {
	case dst:
		return " m" + opVal + " <-"
	case gfn:
		return " Γ" + opVal
	case lfc:
		return " Λ" + opVal
	case loc:
		return " @" + opVal
	case mem:
		return " m" + opVal
	case num:
		return " %" + opVal
	case sfc:
		return " Σ" + opVal
	case tok:
		return " TK" + opVal
	case tup:
		args := op.Args[i : len(op.Args)+1-len(OPERANDS[op.Opcode].or)+i]
		result := " ("
		for j, v := range args[:] {
			result = result + "m" + strconv.Itoa(int(v))
			if j < len(args)-1 {
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
	AddL: {"addL", operands{dst, mem, mem}},
	AddS: {"addS", operands{dst, mem, mem}},
	Adds: {"adds", operands{dst, mem, mem}},
	Adtk: {"adtk", operands{dst, mem, tok}},
	Andb: {"andb", operands{dst, mem, mem}},
	Aref: {"aref", operands{dst, mem, mem}},
	Asgm: {"asgm", operands{dst, mem}},
	Call: {"call", operands{loc, mem, mem, tup}}, // The location to call, the bottom and (exclusive) top of where to put the parameters; and a tuple saying where to get them from.
	CalT: {"calt", operands{loc, mem, mem, tup}}, // The location to call, the bottom and (exclusive) top of where to put the parameters; the memory location of a list of places where we capture tuples, and a tuple saying where to get them from.
	Cc11: {"cc11", operands{dst, mem, mem}},
	Cc1T: {"cc1T", operands{dst, mem, mem}},
	CcT1: {"ccT1", operands{dst, mem, mem}},
	CcTT: {"ccTT", operands{dst, mem, mem}},
	Ccxx: {"ccxx", operands{dst, mem, mem}},
	Cv1T: {"Cv1T", operands{dst, mem}},
	CvTT: {"CvTT", operands{dst, tup}},
	Divf: {"divf", operands{dst, mem, mem}},
	Divi: {"divi", operands{dst, mem, mem}},
	Dofn: {"dofn", operands{dst, mem, tup}},
	Dref: {"dref", operands{dst, mem}},
	Equb: {"equb", operands{dst, mem, mem}},
	Equf: {"equf", operands{dst, mem, mem}},
	Equi: {"equi", operands{dst, mem, mem}},
	Equs: {"equs", operands{dst, mem, mem}},
	Equt: {"equt", operands{dst, mem, mem}},
	Eqxx: {"eqxx", operands{dst, mem, mem, mem}},
	Flti: {"flti", operands{dst, mem}},
	Flts: {"flts", operands{dst, mem}},
	Gofn: {"gofn", operands{dst, gfn, tup}},
	Gtef: {"gtef", operands{dst, mem, mem}},
	Gtei: {"gtei", operands{dst, mem, mem}},
	Gthf: {"gthf", operands{dst, mem, mem}},
	Gthi: {"gthi", operands{dst, mem, mem}},
	Halt: {"halt", operands{}},
	Idfn: {"idfn", operands{dst, mem}},
	IdxL: {"idxL", operands{dst, mem, mem, mem}},
	Idxp: {"idxs", operands{dst, mem, mem, mem}},
	Idxs: {"idxs", operands{dst, mem, mem, mem}},
	Idxt: {"idxs", operands{dst, mem, mem, mem, mem}},
	IdxT: {"idxs", operands{dst, mem, mem, mem}},
	IxTn: {"ixTn", operands{dst, mem, num}},
	IxZl: {"ixZl", operands{dst, mem, mem, mem}},
	IxZn: {"ixZn", operands{dst, mem, num}},
	Inpt: {"inpt", operands{dst, mem}},
	InxL: {"inxL", operands{dst, mem, mem}},
	InxS: {"inxS", operands{dst, mem, mem}},
	Inxt: {"inxt", operands{dst, mem, mem}},
	InxT: {"inxT", operands{dst, mem, mem}},
	Intf: {"intf", operands{dst, mem}},
	Ints: {"ints", operands{dst, mem}},
	IxXx: {"ixXx", operands{dst, mem, mem, mem}},
	Jmp:  {"jmp", operands{loc}},
	Jsr:  {"jsr", operands{loc}},
	KeyM: {"keyM", operands{dst, mem}},
	KeyZ: {"keyZ", operands{dst, mem}},
	LenL: {"lenL", operands{dst, mem}},
	LenM: {"lenM", operands{dst, mem}},
	Lens: {"lens", operands{dst, mem}},
	LenS: {"lenS", operands{dst, mem}},
	LenT: {"lenT", operands{dst, mem}},
	Litx: {"litx", operands{dst, mem}},
	List: {"list", operands{dst, mem}},
	Log:  {"log", operands{mem}},
	Logn: {"logn", operands{}},
	Logy: {"logy", operands{}},
	Mker: {"mker", operands{dst, mem, tok}},
	Mkfn: {"mkfn", operands{dst, lfc}},
	Mkmp: {"mkmp", operands{dst, mem}},
	Mkpr: {"mkpr", operands{dst, mem, mem}},
	MkSn: {"mkSn", operands{dst, sfc}},
	Mkst: {"mkst", operands{dst, mem}},
	Modi: {"modi", operands{dst, mem, mem}},
	Mulf: {"mulf", operands{dst, mem, mem}},
	Muli: {"muli", operands{dst, mem, mem}},
	Negf: {"negf", operands{dst, mem}},
	Negi: {"negi", operands{dst, mem}},
	Notb: {"notb", operands{dst, mem}},
	Orb:  {"orb", operands{dst, mem, mem}},
	Outp: {"outp", operands{mem}},
	Outt: {"outt", operands{mem}},
	Qabt: {"qabt", operands{mem, tup, loc}},
	QlnT: {"qlnT", operands{mem, num, loc}},
	Qlog: {"qlog", operands{loc}},
	Qntp: {"qntp", operands{mem, typ, loc}},
	Qsat: {"qsat", operands{mem, loc}},
	Qsng: {"qsng", operands{mem, loc}},
	Qsnq: {"qsnq", operands{mem, loc}},
	Qstr: {"qstr", operands{mem, loc}},
	Qstq: {"qstq", operands{mem, loc}},
	Qtru: {"qtru", operands{mem, loc}},
	Qtyp: {"qtyp", operands{mem, typ, loc}},
	Ret:  {"ret", operands{}},
	SliL: {"SliL", operands{dst, mem, mem, mem}}, // Third operand is error.
	Slis: {"Slis", operands{dst, mem, mem, mem}}, //
	SliT: {"SliT", operands{dst, mem, mem, mem}}, //
	Strc: {"strc", operands{dst, typ, tup}},
	Strx: {"strx", operands{dst, mem}},
	Subf: {"subf", operands{dst, mem, mem}},
	Subi: {"subi", operands{dst, mem, mem}},
	Thnk: {"thnk", operands{dst, loc}},
	TupL: {"tupL", operands{dst, mem}},
	Typu: {"typu", operands{dst, mem, mem}},
	Typx: {"typx", operands{dst, mem}},
	Untk: {"untk", operands{dst}},
	WthL: {"wthL", operands{dst, mem, mem, mem}}, // Third operand is error.
	WthM: {"wthM", operands{dst, mem, mem, mem}}, //
	Wtht: {"wtht", operands{dst, mem, mem, mem}}, //
	WthZ: {"wthZ", operands{dst, mem, mem, mem}}, //
	WtoM: {"wthM", operands{dst, mem, mem, mem}}, //
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
	AddL
	AddS
	Adds
	Adtk
	Andb
	Aref
	Asgm
	Cc11
	Cc1T
	CcT1
	CcTT
	Ccxx
	Cv1T
	CvTT
	Call
	CalT // Specialized for tuple capture.
	Divf
	Divi
	Dofn // For lambdas, as opposed to call for outer functions.
	Dref
	Equb
	Equf
	Equi
	Equs
	Equt
	Eqxx
	Flti
	Flts
	Gofn
	Gtef
	Gtei
	Gthf
	Gthi
	Halt // do we use this?
	Idfn // Identity function, for testing.
	IdxL
	IdxM
	Idxp
	Idxs
	Idxt
	IdxT
	Inpt
	InxL
	InxS
	Inxt
	InxT
	Ints
	Intf
	IxTn
	IxXx
	IxZl
	IxZn
	Jmp
	Jsr
	KeyM
	KeyZ
	LenL
	Lens
	LenM
	LenS
	LenT
	List
	Litx
	Log
	Logn
	Logy
	Mker
	Mkfn
	Mkmp
	Mkpr
	Mkst
	MkSn
	Modi
	Mulf
	Muli
	Negf
	Negi
	Notb
	Orb
	Outp
	Outt
	Qabt
	QlnT
	Qlog
	Qntp
	Qsat
	Qsng
	Qsnq
	Qstr
	Qstq
	Qtru
	Qtyp
	Ret
	SliL
	Slis
	SliT
	Strc
	Strx
	Subf
	Subi
	Thnk
	TupL
	Typu
	Typx
	Untk
	WthL
	WthM
	Wtht
	WthZ
	WtoM
)
