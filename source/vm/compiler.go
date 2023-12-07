package vm

import (
	"charm/source/ast"
	"charm/source/parser"
)

type Compiler struct {
	p           *parser.Parser
	vm          *Vm
	memTop      uint32
	constantTop uint32
}

func NewCompiler(p *parser.Parser) *Compiler {
	return &Compiler{
		p:      p,
		vm:     blankVm(),
		memTop: 1,
	}
}

func (cp *Compiler) Run(sourcecode string) { // Just for testing.
	print("\nCompiling :\n\n")
	cp.compile("compiler test", sourcecode)
	cp.vm.Run(0)
}

func (cp *Compiler) compile(source, sourcecode string) {
	cp.vm = blankVm()
	node := cp.p.ParseLine(source, sourcecode)
	cp.compileNode(node, node)
}

func (cp *Compiler) compileNode(node ast.Node, head ast.Node) {
	switch node := node.(type) {
	case *ast.IntegerLiteral:
		if node == head {
			cp.emit(asgnc, 0, cp.constantTop)
			cp.emit(ret)
		} else {
			cp.emit(asgnc, cp.memTop, cp.constantTop)
			cp.memTop++
		}
		cp.addConstant(INT, node.Value)
	case *ast.StringLiteral:
		if node == head {
			cp.emit(asgnc, 0, cp.constantTop)
			cp.emit(ret)
		} else {
			cp.emit(asgnc, cp.memTop, cp.constantTop)
			cp.memTop++
		}
		cp.addConstant(STRING, node.Value)
	case *ast.BooleanLiteral:
		if node == head {
			cp.emit(asgnc, 0, cp.constantTop)
			cp.emit(ret)
		} else {
			cp.emit(asgnc, cp.memTop, cp.constantTop)
			cp.memTop++
		}
		cp.addConstant(BOOL, node.Value)
	case *ast.FloatLiteral:
		if node == head {
			cp.emit(asgnc, 0, cp.constantTop)
			cp.emit(ret)
		} else {
			cp.emit(asgnc, cp.memTop, cp.constantTop)
			cp.memTop++
		}
		cp.addConstant(FLOAT, node.Value)
	case *ast.InfixExpression:
		if node.Operator == "==" {
			println("Found the comparison!")
		}
	}
}

func (cp *Compiler) addConstant(t uint32, v any) {
	cp.vm.con = append(cp.vm.con, Value{T: t, V: v})
	cp.constantTop++
}

func (cp *Compiler) emit(opcode opcode, args ...uint32) {
	cp.vm.code = append(cp.vm.code, makeOp(opcode, args...))
	println(describe(cp.vm.code[len(cp.vm.code)-1]))
}
