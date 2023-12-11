package vm

import (
	"charm/source/ast"
	"charm/source/parser"
	"charm/source/token"
)

type enumOrdinates struct {
	enum    uint32
	element int
}

type Compiler struct {
	p       *parser.Parser
	vm      *Vm
	enums   map[string]enumOrdinates
	gconsts *environment
	gvars   *environment
}

func (cp *Compiler) memTop() uint32 {
	return uint32(len(cp.vm.mem))
}

func (cp *Compiler) codeTop() uint32 {
	return uint32(len(cp.vm.code))
}

const MAX_32 = 4294967295

func NewCompiler(p *parser.Parser) *Compiler {
	return &Compiler{
		p:       p,
		vm:      blankVm(),
		enums:   make(map[string]enumOrdinates),
		gconsts: newEnvironment(),
		gvars:   newEnvironment(),
	}
}

func (cp *Compiler) Run() {
	cp.vm.Run(0)
}

func (cp *Compiler) GetParser() *parser.Parser {
	return cp.p
}

const SHOW_BYTECODE = true

func (cp *Compiler) Do(line string) string {
	mT := cp.memTop()
	cT := cp.codeTop()
	node := cp.p.ParseLine("REPL input", line)
	if cp.p.ErrorsExist() {
		return ""
	}
	cp.compileNode(node, cp.gvars)
	if cp.p.ErrorsExist() {
		return ""
	}
	cp.emit(ret)
	if SHOW_BYTECODE {
		print("\nBytecode:\n\n")
		for i := cT; i < cp.codeTop(); i++ {
			println(cp.vm.describe(i))
		}
	}
	cp.vm.Run(cT)
	result := cp.vm.mem[cp.memTop()-1]
	cp.vm.mem = cp.vm.mem[:mT]
	cp.vm.code = cp.vm.code[:cT]
	return result.describe()
}

func (cp *Compiler) Compile(source, sourcecode string) {
	if SHOW_COMPILE {
		print("\nCompiling\n\n")
	}
	cp.vm = blankVm()
	node := cp.p.ParseLine(source, sourcecode)
	cp.compileNode(node, cp.gvars)
	cp.emit(ret)
}

// We have two different ways of emiting an opcode: 'emit' does it the regular way, 'put' ensures that
// the destination is the next free memory address.

func (cp *Compiler) compileNode(node ast.Node, env *environment) typeList {
	switch node := node.(type) {
	case *ast.IntegerLiteral:
		cp.addConstant(INT, node.Value)
		return []valType{&simpleType{t: INT}}
	case *ast.StringLiteral:
		cp.addConstant(STRING, node.Value)
		return []valType{&simpleType{t: STRING}}
	case *ast.BooleanLiteral:
		cp.addConstant(BOOL, node.Value)
		return []valType{&simpleType{t: BOOL}}
	case *ast.FloatLiteral:
		cp.addConstant(FLOAT, node.Value)
		return []valType{&simpleType{t: FLOAT}}
	case *ast.InfixExpression:
		if node.Operator == "==" {
			return cp.emitEquals(node, env)
		}
		if node.Operator == "!=" {
			types := cp.emitEquals(node, env)
			cp.put(notb, cp.memTop()-1)
			return types
		}
		cp.p.Throw("comp/infix", node.Token)
		return []valType{&simpleType{t: TYPE_ERROR}}
	case *ast.LazyInfixExpression:
		if node.Operator == "or" {
			lTypes := cp.compileNode(node.Left, env)
			if !lTypes.contains(BOOL) {
				cp.p.Throw("comp/or/bool/left", node.Token)
				return []valType{&simpleType{t: TYPE_ERROR}}
			}
			leftRg := cp.memTop() - 1
			cp.emit(qtru, leftRg, cp.codeTop()+2)
			backtrack := cp.codeTop()
			cp.emit(jmp, MAX_32)
			rTypes := cp.compileNode(node.Right, env)
			if !rTypes.contains(BOOL) {
				cp.p.Throw("comp/or/bool/right", node.Token)
				return []valType{&simpleType{t: TYPE_ERROR}}
			}
			rightRg := cp.memTop() - 1
			cp.vm.code[backtrack].args[0] = cp.codeTop()
			cp.put(orb, leftRg, rightRg)
			return []valType{&simpleType{t: BOOL}}
		}
		if node.Operator == "and" {
			lTypes := cp.compileNode(node.Left, env)
			if !lTypes.contains(BOOL) {
				cp.p.Throw("comp/and/bool/left", node.Token)
				return []valType{&simpleType{t: TYPE_ERROR}}
			}
			leftRg := cp.memTop() - 1
			backtrack := cp.codeTop()
			cp.emit(qtru, leftRg, MAX_32)
			rTypes := cp.compileNode(node.Right, env)
			if !rTypes.contains(BOOL) {
				cp.p.Throw("comp/and/bool/right", node.Token)
				return []valType{&simpleType{t: TYPE_ERROR}}
			}
			rightRg := cp.memTop() - 1
			cp.vm.code[backtrack].args[1] = cp.codeTop()
			cp.put(andb, leftRg, rightRg)
			return []valType{&simpleType{t: BOOL}}
		}
		if node.Operator == ":" {
			if node.Left.GetToken().Type == token.ELSE {
				return cp.compileNode(node.Right, env)
			}
			lTypes := cp.compileNode(node.Left, env)
			if !lTypes.contains(BOOL) {
				cp.p.Throw("comp/cond/bool", node.Token)
				return []valType{&simpleType{t: TYPE_ERROR}}
			}
			leftRg := cp.memTop() - 1
			backtrack := cp.codeTop()
			cp.emit(qtru, leftRg, MAX_32)
			rTypes := cp.compileNode(node.Right, env)
			cp.put(asgm, cp.memTop()-1)
			cp.emit(jmp, cp.codeTop()+2)
			cp.vm.code[backtrack].args[1] = cp.codeTop()
			cp.reput(asgm, C_U_OBJ)
			return rTypes.union([]valType{&simpleType{t: UNSAT}})
		}
		if node.Operator == ";" {
			lTypes := cp.compileNode(node.Left, env)
			leftRg := cp.memTop() - 1
			backtrack := cp.codeTop()
			cp.emit(qtyp, leftRg, UNSAT, MAX_32)
			rTypes := cp.compileNode(node.Right, env)
			rightRg := cp.memTop() - 1
			cp.put(asgm, rightRg)
			cp.emit(jmp, cp.codeTop()+2)
			cp.vm.code[backtrack].args[2] = cp.codeTop()
			cp.reput(asgm, leftRg)
			if !(lTypes.contains(UNSAT) && rTypes.contains(UNSAT)) {
				return lTypes.union(rTypes).without(&simpleType{UNSAT})
			}
			return lTypes.union(rTypes)
		}
		panic("Unimplemented lazy infix.")
	case *ast.PrefixExpression:
		if node.Operator == "not" {
			allTypes := cp.compileNode(node.Args[0], env)
			if allTypes.only(BOOL) {
				cp.put(notb, cp.memTop()-1)
				return []valType{&simpleType{t: BOOL}}
			}
			if !allTypes.contains(BOOL) {
				cp.p.Throw("comp/not/bool", node.Token)
				return []valType{&simpleType{t: TYPE_ERROR}}
			}
			panic("Haven't implemented this bit because of having no way to test it at this point.")
		} else {
			panic("Unimplemented prefix.")
		}
	case *ast.Identifier:
		v, ok := env.getVar(node.Value)
		if ok {
			cp.put(asgm, v.mLoc)
			return v.types
		}
		cp.p.Throw("comp/ident/known", node.Token)
		return []valType{&simpleType{t: TYPE_ERROR}}
	default:
		panic("Unimplemented node type.")
	}
}

// Despite their names, addConstant and addVariable do very different things.
func (cp *Compiler) addConstant(t uint32, v any) {
	cp.vm.mem = append(cp.vm.mem, Value{T: t, V: v})
}

func (cp *Compiler) addVariable(env *environment, name string, val Value, acc varAccess, types typeList) {
	env.data[name] = variable{mLoc: cp.memTop() - 1, access: acc, types: types}
}

func (cp *Compiler) put(opcode opcode, args ...uint32) {
	args = append([]uint32{cp.memTop()}, args...)
	cp.emit(opcode, args...)
	cp.vm.mem = append(cp.vm.mem, Value{})
}

func (cp *Compiler) reput(opcode opcode, args ...uint32) {
	args = append([]uint32{cp.memTop() - 1}, args...)
	cp.emit(opcode, args...)
}

const SHOW_COMPILE = false

func (cp *Compiler) emit(opcode opcode, args ...uint32) {
	cp.vm.code = append(cp.vm.code, makeOp(opcode, args...))
	if SHOW_COMPILE {
		println(describe(cp.vm.code[len(cp.vm.code)-1]))
	}
}

func (cp *Compiler) emitEquals(node *ast.InfixExpression, env *environment) typeList {
	lTypes := cp.compileNode(node.Args[0], env)
	if lTypes.only(ERROR) {
		cp.p.Throw("comp/eq/err/a", node.Token)
		return []valType{&simpleType{t: TYPE_ERROR}}
	}
	leftRg := cp.memTop() - 1
	rTypes := cp.compileNode(node.Args[2], env)
	if rTypes.only(ERROR) {
		cp.p.Throw("comp/eq/err/b", node.Token)
		return []valType{&simpleType{t: TYPE_ERROR}}
	}
	rightRg := cp.memTop() - 1
	oL := lTypes.intersect(rTypes)
	if oL.only(ERROR) {
		cp.p.Throw("comp/eq/err/c", node.Token)
		return []valType{&simpleType{t: TYPE_ERROR}}
	}
	if len(oL) == 0 {
		cp.p.Throw("comp/eq/types", node.Token)
		return []valType{&simpleType{t: TYPE_ERROR}}
	}
	if len(oL) == 1 && len(lTypes) == 1 && len(rTypes) == 1 {
		switch oL[0].concreteType() {
		case INT:
			cp.put(equi, leftRg, rightRg)
		case STRING:
			cp.put(equs, leftRg, rightRg)
		case BOOL:
			cp.put(equb, leftRg, rightRg)
		case FLOAT:
			cp.put(equf, leftRg, rightRg)
		default:
			panic("Unimplemented comparison type.")
		}
		return []valType{&simpleType{t: BOOL}}
	} else {
		panic("Haven't implemented this bit because of having no way to test it at this point.")
	}
}
