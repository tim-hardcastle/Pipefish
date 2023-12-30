package vm

import (
	"charm/source/ast"
	"charm/source/parser"
	"charm/source/token"
)

type enumOrdinates struct {
	enum    simpleType
	element int
}

type thunk struct {
	mLoc uint32
	cLoc uint32
}

type fnTreeNode struct {
	fn     *cpFunc
	branch []*typeNodePair
}

type typeNodePair struct {
	typeName string
	fnNode   *fnTreeNode
}

type Compiler struct {
	p              *parser.Parser
	vm             *Vm
	enums          map[string]enumOrdinates
	gconsts        *environment
	gvars          *environment
	fns            []*cpFunc
	thunkList      []thunk
	functionForest map[string]*fnTreeNode
}

type cpFunc struct {
	callTo uint32
	loReg  uint32
	hiReg  uint32
	outReg uint32
	types  alternateType
}

func (cp *Compiler) memTop() uint32 {
	return uint32(len(cp.vm.mem))
}

func (cp *Compiler) that() uint32 {
	return uint32(len(cp.vm.mem) - 1)
}

func (cp *Compiler) codeTop() uint32 {
	return uint32(len(cp.vm.code))
}

func (cp *Compiler) next() uint32 {
	return uint32(len(cp.vm.code))
}

const DUMMY = 4294967295

func NewCompiler(p *parser.Parser) *Compiler {
	return &Compiler{
		p:         p,
		vm:        blankVm(),
		enums:     make(map[string]enumOrdinates),
		gconsts:   newEnvironment(),
		gvars:     newEnvironment(),
		thunkList: []thunk{},
		fns:       []*cpFunc{},
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
			println(cp.vm.describeCode(i))
		}
	}
	cp.vm.Run(cT)
	result := cp.vm.mem[cp.that()]
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

func (cp *Compiler) reserve(t simpleType, v any) uint32 {
	cp.vm.mem = append(cp.vm.mem, Value{T: t, V: v})
	return uint32(len(cp.vm.mem) - 1)
}

func (cp *Compiler) addVariable(env *environment, name string, acc varAccess, types alternateType) {
	env.data[name] = variable{mLoc: cp.that(), access: acc, types: types}
}

func (cp *Compiler) compileNode(node ast.Node, env *environment) alternateType {
	switch node := node.(type) {
	case *ast.IntegerLiteral:
		cp.reserve(INT, node.Value)
		return simpleList(INT)
	case *ast.StringLiteral:
		cp.reserve(STRING, node.Value)
		return simpleList(STRING)
	case *ast.BooleanLiteral:
		cp.reserve(BOOL, node.Value)
		return simpleList(BOOL)
	case *ast.FloatLiteral:
		cp.reserve(FLOAT, node.Value)
		return simpleList(FLOAT)
	case *ast.InfixExpression:
		if node.Operator == "==" {
			return cp.emitEquals(node, env)
		}
		if node.Operator == "!=" {
			types := cp.emitEquals(node, env)
			cp.put(notb, cp.that())
			return types
		}
		cp.p.Throw("comp/infix", node.Token)
		return simpleList(TYPE_ERROR)
	case *ast.LazyInfixExpression:
		if node.Operator == "or" {
			lTypes := cp.compileNode(node.Left, env)
			if !lTypes.contains(BOOL) {
				cp.p.Throw("comp/or/bool/left", node.Token)
				return simpleList(TYPE_ERROR)
			}
			leftRg := cp.that()
			cp.emit(qtru, leftRg, cp.next()+2)
			backtrack := cp.next()
			cp.emit(jmp, DUMMY)
			rTypes := cp.compileNode(node.Right, env)
			if !rTypes.contains(BOOL) {
				cp.p.Throw("comp/or/bool/right", node.Token)
				return simpleList(TYPE_ERROR)
			}
			rightRg := cp.that()
			cp.vm.code[backtrack].args[0] = cp.next()
			cp.put(orb, leftRg, rightRg)
			return simpleList(BOOL)
		}
		if node.Operator == "and" {
			lTypes := cp.compileNode(node.Left, env)
			if !lTypes.contains(BOOL) {
				cp.p.Throw("comp/and/bool/left", node.Token)
				return simpleList(TYPE_ERROR)
			}
			leftRg := cp.that()
			backtrack := cp.next()
			cp.emit(qtru, leftRg, DUMMY)
			rTypes := cp.compileNode(node.Right, env)
			if !rTypes.contains(BOOL) {
				cp.p.Throw("comp/and/bool/right", node.Token)
				return simpleList(TYPE_ERROR)
			}
			rightRg := cp.that()
			cp.vm.code[backtrack].args[1] = cp.next()
			cp.put(andb, leftRg, rightRg)
			return simpleList(BOOL)
		}
		if node.Operator == ":" {
			if node.Left.GetToken().Type == token.ELSE {
				return cp.compileNode(node.Right, env)
			}
			lTypes := cp.compileNode(node.Left, env)
			if !lTypes.contains(BOOL) {
				cp.p.Throw("comp/cond/bool", node.Token)
				return simpleList(TYPE_ERROR)
			}
			leftRg := cp.that()
			backtrack := cp.next()
			cp.emit(qtru, leftRg, DUMMY)
			rTypes := cp.compileNode(node.Right, env)
			cp.put(asgm, cp.that())
			cp.emit(jmp, cp.next()+2)
			cp.vm.code[backtrack].args[1] = cp.next()
			cp.reput(asgm, C_U_OBJ)
			return rTypes.union(simpleList(UNSAT))
		}
		if node.Operator == ";" {
			lTypes := cp.compileNode(node.Left, env)
			// We deal with the case where the newline is separating local constant definitions
			// in the 'given' block.
			if lTypes.only(CREATED_LOCAL_CONSTANT) {
				cp.compileNode(node.Right, env)
				return simpleList(CREATED_LOCAL_CONSTANT)
			}
			leftRg := cp.that()
			backtrack := cp.next()
			cp.emit(qtyp, leftRg, uint32(UNSAT), DUMMY)
			rTypes := cp.compileNode(node.Right, env)
			rightRg := cp.that()
			cp.put(asgm, rightRg)
			cp.emit(jmp, cp.next()+2)
			cp.vm.code[backtrack].args[2] = cp.next()
			cp.reput(asgm, leftRg)
			if !(lTypes.contains(UNSAT) && rTypes.contains(UNSAT)) {
				return lTypes.union(rTypes).without(UNSAT)
			}
			return lTypes.union(rTypes)
		}
		panic("Unimplemented lazy infix.")
	case *ast.Identifier:
		v, ok := env.getVar(node.Value)
		if ok {
			if v.access == LOCAL_CONSTANT_THUNK {
				cp.emit(untk, v.mLoc)
			}
			cp.put(asgm, v.mLoc)
			return v.types
		}
		cp.p.Throw("comp/ident/known", node.Token)
		return simpleList(COMPILATION_ERROR)
	case *ast.AssignmentExpression:
		if node.Token.Type == token.GVN_ASSIGN {
			// TODO --- need to do this better after we implement tuples
			if node.Left.GetToken().Type != token.IDENT {
				cp.p.Throw("comp/assign/ident", node.Left.GetToken())
				return simpleList(COMPILATION_ERROR)
			}
			thunkStart := cp.next()
			types := cp.compileNode(node.Right, env)
			cp.emit(ret)
			cp.addVariable(env, node.Left.(*ast.Identifier).Value, LOCAL_CONSTANT_THUNK, types)
			cp.thunkList = append(cp.thunkList, thunk{cp.that(), thunkStart})
		}
		cp.p.Throw("comp/assign", node.Token)
		return simpleList(COMPILATION_ERROR)
	case *ast.PrefixExpression:
		if node.Operator == "not" {
			allTypes := cp.compileNode(node.Args[0], env)
			if allTypes.only(BOOL) {
				cp.put(notb, cp.that())
				return simpleList(BOOL)
			}
			if !allTypes.contains(BOOL) {
				cp.p.Throw("comp/not/bool", node.Token)
				return simpleList(TYPE_ERROR)
			}
			panic("Haven't implemented this bit because of having no way to test it at this point.")
		}
		if cp.p.Prefixes.Contains(node.Operator) || cp.p.Functions.Contains(node.Operator) {
			return cp.createFunctionCall(node, env)
		}
		cp.p.Throw("comp/prefix/known", node.Token)
		return simpleList(COMPILATION_ERROR)
	default:
		panic("Unimplemented node type.")
	}
}

func (cp *Compiler) createFunctionCall(node *ast.PrefixExpression, env *environment) alternateType {
	b := &bindle{tok: node.Token,
		treePosition: cp.p.FunctionTreeMap[node.Operator],
		outLoc:       cp.reserve(ERROR, DUMMY),
		env:          env,
		valLocs:      make([]uint32, len(node.Args)),
		types:        make(finiteTupleType, len(node.Args)),
	}
	for i, arg := range node.Args {
		b.types[i] = cp.compileNode(arg, env)
		b.valLocs[i] = cp.that()
	}
	returnTypes := cp.handleNewArgument(b)
	return returnTypes
}

type bindle struct {
	treePosition *ast.FnTreeNode // Our position on the function tree.
	branchNo     int             // The number of the branch in the function tree.
	argNo        int             // The number of the argument we're looking at.
	index        int             // The index we're looking at in the argument we're looking at.
	targetList   alternateType   // The possible types associated with this tree position.
	doneList     alternateType   // The types we've looked at up to and including those of the current branchNo.
	valLocs      []uint32        // The locations of the values evaluated from the arguments.
	types        finiteTupleType // The types of the values.
	outLoc       uint32          // Where we're going to put the output.
	env          *environment    // Associates variable names with memory locations
	tupleTime    bool            // Once we've taken a tuple path, we can discard values 'til we reach bling or run out.
	tok          token.Token     // For generating errors.
}

func (cp *Compiler) handleNewArgument(b *bindle) alternateType {
	// Case (1) : we've used up all our arguments. In this case we should look in the function tree for a function call.
	if b.argNo >= len(b.types) {
		cp.seekFunctionCall(b)
	}
	// Case (2) : We aren't yet at the end of the list of arguments.
	newBindle := *b
	newBindle.index = 0
	return cp.generateFromTopBranchDown(b)
}

func (cp *Compiler) generateFromTopBranchDown(b *bindle) alternateType {
	newBindle := *b
	newBindle.branchNo = 0
	newBindle.targetList = typesAtIndex(b.types[b.argNo], b.index)
	newBindle.doneList = make(alternateType, 0, len(b.targetList))
	return cp.generateConditionalOnBranchNumber(&newBindle)
}

// We look at the current branch and see if its type can account for some, all, or none of the possibilities in the targetList.
// If the answer is "all", we can recurse on the next argument.
// If "none", then we can recurse on the next branch down.
// If "some", then we must generate a conditional where it recurses on the next argument for the types accepted by the branch
// and on the next branch for the unaccepted types.
// It may also be the run-off-the-end branch number, in which case we can generate an error.
func (cp *Compiler) generateConditionalOnBranchNumber(b *bindle) alternateType {

}

func (cp *Compiler) seekFunctionCall(b *bindle) alternateType {
	for _, branch := range b.treePosition.Branch { // TODO --- this is a pretty vile hack; it would make sense for it to always be at the top.}
		if branch.Node.Fn != nil {
			fNo := branch.Node.Fn.Number
			cp.putFunctionCall(fNo, b.valLocs)
			cp.emit(asgm, b.outLoc, cp.fns[fNo].outReg) // Because the different implementations of the function will have their own out register.
			return cp.fns[fNo].types
		}
	}
	cp.reserve(ERROR, DUMMY)
	cp.emit(asgm, b.outLoc, cp.that())
	return simpleList(TYPE_ERROR)
}

const SHOW_COMPILE = false

// We have two different ways of emiting an opcode: 'emit' does it the regular way, 'put' ensures that
// the destination is the next free memory address.
func (cp *Compiler) emit(opcode opcode, args ...uint32) {
	cp.vm.code = append(cp.vm.code, makeOp(opcode, args...))
	if SHOW_COMPILE {
		println(describe(cp.vm.code[len(cp.vm.code)-1]))
	}
}

func (cp *Compiler) put(opcode opcode, args ...uint32) {
	args = append([]uint32{cp.memTop()}, args...)
	cp.emit(opcode, args...)
	cp.vm.mem = append(cp.vm.mem, Value{})
}

// Reput puts the value in the last memory address to be used.
func (cp *Compiler) reput(opcode opcode, args ...uint32) {
	args = append([]uint32{cp.that()}, args...)
	cp.emit(opcode, args...)
}

func (cp *Compiler) putFunctionCall(funcNumber uint32, valLocs []uint32) {
	args := append([]uint32{cp.fns[funcNumber].callTo, cp.fns[funcNumber].loReg, cp.fns[funcNumber].hiReg}, valLocs...)
	cp.emit(call, args...)
	cp.put(asgm, cp.fns[funcNumber].outReg)
}

func (cp *Compiler) emitFunctionCall(dest, funcNumber uint32, valLocs []uint32) {
	args := append([]uint32{cp.fns[funcNumber].callTo, cp.fns[funcNumber].loReg, cp.fns[funcNumber].hiReg}, valLocs...)
	cp.emit(call, args...)
	cp.emit(asgm, dest, cp.fns[funcNumber].outReg)
}

func (cp *Compiler) emitEquals(node *ast.InfixExpression, env *environment) alternateType {
	lTypes := cp.compileNode(node.Args[0], env)
	if lTypes.only(ERROR) {
		cp.p.Throw("comp/eq/err/a", node.Token)
		return simpleList(TYPE_ERROR)
	}
	leftRg := cp.that()
	rTypes := cp.compileNode(node.Args[2], env)
	if rTypes.only(ERROR) {
		cp.p.Throw("comp/eq/err/b", node.Token)
		return simpleList(TYPE_ERROR)
	}
	rightRg := cp.that()
	oL := lTypes.intersect(rTypes)
	if oL.only(ERROR) {
		cp.p.Throw("comp/eq/err/c", node.Token)
		return simpleList(TYPE_ERROR)
	}
	if len(oL) == 0 {
		cp.p.Throw("comp/eq/types", node.Token)
		return simpleList(TYPE_ERROR)
	}
	if len(oL) == 1 && len(lTypes) == 1 && len(rTypes) == 1 {
		switch el := oL[0].(type) {
		case simpleType:
			switch el {
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
			return simpleList(BOOL)
		default:
			panic("Unimplemented comparison type.")
		}
	} else {
		panic("Haven't implemented this bit because of having no way to test it at this point.")
	}
}
