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
	gfn
	lfc
	loc
	mem
	num
	sfc
	tok
	trk
	tup
	typ
)

type operands []operandType

type Opcode uint8

func (op *Operation) ppOperand(i int) string {
	// If we're calling this, the OPERANDS table shows that the operation ought to have an i-dx operand.
	//
	opType := OPERANDS[op.Opcode].or[i]
	if i >= len(op.Args) {
		if opType == tup {
			return " ()"
		}
		println("Not enough operands supplied to " + OPERANDS[op.Opcode].oc + "; was expecting " + strconv.Itoa(len(OPERANDS[op.Opcode].or)) + " but got " + strconv.Itoa(len(op.Args)) + ".")
		argStr := "Args were:"
		for j := range op.Args {
			argStr = argStr + " " + op.ppOperand(j)
		}
		argStr = argStr + "."
		println(argStr)
		panic("That's all folks!")
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
	case trk:
		return " ~" + opVal
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
	Adrr: {"adds", operands{dst, mem, mem}},
	Adrs: {"adds", operands{dst, mem, mem}},
	Adsr: {"adds", operands{dst, mem, mem}},
	Adtk: {"adtk", operands{dst, mem, tok}},
	Andb: {"andb", operands{dst, mem, mem}},
	Aref: {"aref", operands{dst, mem}},
	Asgm: {"asgm", operands{dst, mem}},
	Auto: {"auto", operands{trk}},
	Bcon: {"bcon", operands{dst, mem}},
	Bsql: {"bsql", operands{dst, mem}},
	Call: {"call", operands{loc, mem, mem, tup}}, // The location to call, the bottom and (exclusive) top of where to put the parameters; and a tuple saying where to get them from.
	CalT: {"calt", operands{loc, mem, mem, tup}}, // The location to call, the bottom and (exclusive) top of where to put the parameters; the memory location of a list of places where we capture tuples, and a tuple saying where to get them from.
	Cast: {"cast", operands{dst, mem, typ}},
	Casx: {"cast", operands{dst, mem, typ, tok}},
	Cc11: {"cc11", operands{dst, mem, mem}},
	Cc1T: {"cc1T", operands{dst, mem, mem}},
	CcT1: {"ccT1", operands{dst, mem, mem}},
	CcTT: {"ccTT", operands{dst, mem, mem}},
	Ccxx: {"ccxx", operands{dst, mem, mem}},
	Cpnt: {"cpnt", operands{dst, mem}},
	Cv1T: {"cv1T", operands{dst, mem}},
	CvTT: {"cvTT", operands{dst, tup}},
	Diif: {"diif", operands{dst, mem, mem, tok}},
	Divf: {"divf", operands{dst, mem, mem, tok}},
	Divi: {"divi", operands{dst, mem, mem, tok}},
	Dvfi: {"dvfi", operands{dst, mem, mem, tok}},
	Dvif: {"dvif", operands{dst, mem, mem, tok}},
	Dofn: {"dofn", operands{dst, mem, tup}},
	Dref: {"dref", operands{dst, mem}},
	Equb: {"equb", operands{dst, mem, mem}},
	Equf: {"equf", operands{dst, mem, mem}},
	Equi: {"equi", operands{dst, mem, mem}},
	Equs: {"equs", operands{dst, mem, mem}},
	Equt: {"equt", operands{dst, mem, mem}},
	Eqxx: {"eqxx", operands{dst, mem, mem, tok}},
	Extn: {"extn", operands{dst, mem, mem, mem, mem, tup}}, // Operands are: the external service to call; whether the function is PREFIX, INFIX, or POSTFIX; the remainder of the namespace of the function as a string; the name of the function as a string; the locations of the arguments.
	Flti: {"flti", operands{dst, mem}},
	Flts: {"flts", operands{dst, mem}},
	Gofn: {"gofn", operands{dst, mem, gfn, tup}}, // Mem contains the location of a *mutable* error, i.e. we will copy its token but change its contents when returning it. 
	Gsql: {"gsql", operands{dst, mem, mem, mem, mem, tok}},
	Gtef: {"gtef", operands{dst, mem, mem}},
	Gtei: {"gtei", operands{dst, mem, mem}},
	Gthf: {"gthf", operands{dst, mem, mem}},
	Gthi: {"gthi", operands{dst, mem, mem}},
	IdxL: {"idxL", operands{dst, mem, mem, tok}},
	Idxp: {"idxs", operands{dst, mem, mem, tok}},
	Idxs: {"idxs", operands{dst, mem, mem, tok}},
	Idxt: {"idxt", operands{dst, mem, mem, tok}},
	IdxT: {"idxT", operands{dst, mem, mem, tok}},
	IxTn: {"ixTn", operands{dst, mem, num}},
	IxZl: {"ixZl", operands{dst, mem, mem, tok}},
	IxZn: {"ixZn", operands{dst, mem, num}},
	Inpt: {"inpt", operands{dst, mem}},
	InxL: {"inxL", operands{dst, mem, mem}},
	InxS: {"inxS", operands{dst, mem, mem}},
	Inxt: {"inxt", operands{dst, mem, mem}},
	InxT: {"inxT", operands{dst, mem, mem}},
	Inte: {"inte", operands{dst, mem}},
	Intf: {"intf", operands{dst, mem}},
	Ints: {"ints", operands{dst, mem}},
	Itgk: {"itgk", operands{dst, mem}},
	Itkv: {"itgv", operands{dst, dst, mem}},
	Itgv: {"itgv", operands{dst, mem}},
	Itor: {"itor", operands{dst, mem}},
	IxSn: {"inxS", operands{dst, mem, mem}},
	IxXx: {"ixXx", operands{dst, mem, mem, tok}},
	Jmp:  {"jmp", operands{loc}},
	Jsr:  {"jsr", operands{loc}},
	KeyM: {"keyM", operands{dst, mem}},
	KeyZ: {"keyZ", operands{dst, mem}},
	Lbls: {"lbls", operands{dst, mem, tok}},
	LenL: {"lenL", operands{dst, mem}},
	LenM: {"lenM", operands{dst, mem}},
	Lens: {"lens", operands{dst, mem}},
	LenS: {"lenS", operands{dst, mem}},
	LenT: {"lenT", operands{dst, mem}},
	Litx: {"litx", operands{dst, mem}},
	List: {"list", operands{dst, mem}},
	LnSn: {"lnSn", operands{dst, mem}},
	Logn: {"logn", operands{}},
	Logy: {"logy", operands{}},
	Mker: {"mker", operands{dst, mem, tok}},
	Mkfn: {"mkfn", operands{dst, lfc}},
	Mkit: {"mkit", operands{dst, mem, num, tok}}, // the num is 0 or 1 according to whether the iterator doesn't or does only return keys.
	Mkmp: {"mkmp", operands{dst, mem, tok}},
	Mkpr: {"mkpr", operands{dst, mem, mem}},
	MkSc: {"mkSn", operands{dst, sfc}},
	MkSn: {"mkSn", operands{dst, sfc}},
	Mkst: {"mkst", operands{dst, mem, tok}},
	Modi: {"modi", operands{dst, mem, mem, tok}},
	Mulf: {"mulf", operands{dst, mem, mem}},
	Muli: {"muli", operands{dst, mem, mem}},
	Negf: {"negf", operands{dst, mem}},
	Negi: {"negi", operands{dst, mem}},
	Notb: {"notb", operands{dst, mem}},
	Orb:  {"orb", operands{dst, mem, mem}},
	Outp: {"outp", operands{mem}},
	Outt: {"outt", operands{mem}},
	Qabt: {"qabt", operands{mem, tup, loc}},
	Qfls: {"qfls", operands{mem, loc}},
	Qitr: {"qitr", operands{mem, loc}},
	QleT: {"qleT", operands{mem, num, loc}},
	QlnT: {"qlnT", operands{mem, num, loc}},
	Qlog: {"qlog", operands{loc}},
	Qntp: {"qntp", operands{mem, typ, loc}},
	Qnvh: {"qnvh", operands{mem, num, loc}},
	Qnvq: {"qnvq", operands{mem, num, loc}},
	Qsat: {"qsat", operands{mem, loc}},
	Qsng: {"qsng", operands{mem, loc}},
	Qsnq: {"qsnq", operands{mem, loc}},
	Qstr: {"qstr", operands{mem, loc}},
	Qstq: {"qstq", operands{mem, loc}},
	Qtpt: {"qtpt", operands{mem, num, tup, loc}},
	Qtru: {"qtru", operands{mem, loc}},
	Qtyp: {"qtyp", operands{mem, typ, loc}},
	Qvch: {"qvch", operands{mem, num, loc}},
	Qvcq: {"qvcq", operands{mem, num, loc}},
	Psql: {"gsql", operands{dst, mem, mem, tok}},
	Ret:  {"ret", operands{}},
	Rpop: {"rpop", operands{}},
	Rpsh: {"rpsh", operands{num, num}},
	Rsit: {"rsit", operands{dst}},
	SliL: {"sliL", operands{dst, mem, mem, tok}},
	Slis: {"slis", operands{dst, mem, mem, tok}},
	SliT: {"sliT", operands{dst, mem, mem, tok}},
	SlTn: {"slTn", operands{dst, mem, num}},
	Strc: {"strc", operands{dst, typ, tup}},
	Strx: {"strx", operands{dst, mem}},
	Subf: {"subf", operands{dst, mem, mem}},
	Subi: {"subi", operands{dst, mem, mem}},
	SubS: {"subS", operands{dst, mem, mem}},
	Thnk: {"thnk", operands{dst, mem, loc}},
	Tplf: {"tupf", operands{dst, mem, tok}},
	Tpll: {"tupl", operands{dst, mem, tok}},
	Trak: {"trak", operands{trk}},
	TupL: {"tupL", operands{dst, mem}},
	TuLx: {"tuLx", operands{dst, mem, tok}},
	Typu: {"typu", operands{dst, mem, mem}},
	Typx: {"typx", operands{dst, mem}},
	UntE: {"untE", operands{dst}},
	Untk: {"untk", operands{dst}},
	Uwrp: {"uwrp", operands{dst, mem, tok}},
	Varc: {"wthL", operands{dst, mem, tok}},
	Vlid: {"vlid", operands{dst, mem}},
	WthL: {"wthL", operands{dst, mem, mem, tok}},
	WthM: {"wthM", operands{dst, mem, mem, tok}},
	Wtht: {"wtht", operands{dst, mem, mem, tok}},
	WthZ: {"wthZ", operands{dst, mem, mem, tok}},
	WtoM: {"wthM", operands{dst, mem, mem, tok}},
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
	Adrr
	Adrs
	Adsr
	Adtk
	Andb
	Aref
	Asgm
	Auto
	Bcon
	Bsql
	Cc11
	Cc1T
	CcT1
	CcTT
	Ccxx
	Cv1T
	CvTT
	Cpnt
	Call
	CalT // Specialized for tuple capture.
	Cast
	Casx
	CoSn
	Diif
	Divf
	Divi
	Dofn // For lambdas, as opposed to call for outer functions.
	Dref
	Dvfi
	Dvif
	Equb
	Equf
	Equi
	Equs
	Equt
	Eqxx
	Extn
	Flti
	Flts
	Gofn
	Gsql
	Gtef
	Gtei
	Gthf
	Gthi
	IctS
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
	Inte
	Intf
	Ints
	Itgk
	Itkv
	Itgv
	Itor
	IxSn
	IxTn
	IxXx
	IxZl
	IxZn
	Jmp
	Jsr
	KeyM
	KeyZ
	Lbls
	LenL
	Lens
	LenM
	LenS
	LenT
	List
	Litx
	LnSn
	Logn
	Logy
	Mker
	Mkfn
	Mkit
	Mkmp
	Mkpr
	MkSc
	MkSn
	Mkst
	Mlfi
	Modi
	Mulf
	Muli
	Negf
	Negi
	Notb
	Orb
	Outp
	Outt
	Psql
	Qabt
	Qfls
	Qitr
	QleT
	QlnT
	Qlog
	Qntp
	Qsat
	Qsng
	Qsnq
	Qspt
	Qspq
	Qstr
	Qstq
	Qtpt
	Qtru
	Qtyp
	Qnvq
	Qnvh
	Qvch
	Qvcq
	Ret
	Rpop
	Rpsh
	Rsit
	SliL
	Slis
	SliT
	SlTn
	Strc
	Strx
	Subf
	Subi
	SubS
	Thnk
	Tplf
	Tpll
	Trak
	TupL
	TuLx
	Typs
	Typu
	Typx
	UntE
	Untk
	Uwrp
	Varc
	Vlid
	WthL
	WthM
	Wtht
	WthZ
	WtoM
)
