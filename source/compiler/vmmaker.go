package compiler

import (
	"pipefish/source/ast"
	"pipefish/source/initializer"
	"pipefish/source/parser"
	"pipefish/source/token"
	"pipefish/source/values"
	"pipefish/source/vm"

	"database/sql"
)

// Just as the initializer directs the tokenizer and the parser in the construction of the parsed code
// chunks from the tokens, so the vmMaker directs the initializer and compiler in the construction of the mc.

// Hence the vmMaker contains all the state which is not needed by the compiler (e.g. the function table),
// and the compiler contains the state needed at compile time but not at runtime.

type VmMaker struct {
	cp             *Compiler
	uP             *initializer.Initializer
	scriptFilepath string
}

func NewVmMaker(scriptFilepath, sourcecode string, db *sql.DB) *VmMaker {
	uP := initializer.New(scriptFilepath, sourcecode, db)
	vmm := &VmMaker{
		cp: NewCompiler(uP.Parser),
		uP: uP,
	}
	vmm.scriptFilepath = scriptFilepath
	vmm.uP.GetSource(scriptFilepath)
	return vmm
}

func (vmm *VmMaker) GetCompiler() *Compiler {
	return vmm.cp
}

func (vmm *VmMaker) Make() {

	vmm.uP.MakeParserAndTokenizedProgram()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.uP.AddToNameSpace([]string{"rsc/pipefish/test.pf"}) // , "rsc/pipefish/world.pf"
	vmm.uP.ParseImports()
	if vmm.uP.ErrorsExist() {
		return
	}
	// unnamespacedImports := vmm.uP.InitializeNamespacedImportsAndReturnUnnamespacedImports(root, namePath)

	// if vmm.uP.ErrorsExist() {
	// 	return newService, init
	// }
	// vmm.uP.AddToNameSpace(unnamespacedImports)

	vmm.createEnums()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.uP.MakeLanguagesAndContacts()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.uP.ParseTypeDefs()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.uP.ParseEverything()
	if vmm.uP.ErrorsExist() {
		return
	}

	// An intermediate step that groups the functions by name and orders them by specificity.
	vmm.uP.MakeFunctions(vmm.scriptFilepath)
	if vmm.uP.ErrorsExist() {
		return
	}
	// Now we turn this into a different data structure, a decision tree with its branches labeled
	// with types. Following it tells us which version of an overloaded function to use.
	vmm.uP.MakeFunctionTrees()
	if vmm.uP.ErrorsExist() {
		return
	}
	// And we compile them in what is mainly a couple of loops wrapping around the aptly-named
	// .compileFunction method.
	vmm.compileFunctions()
	if vmm.uP.ErrorsExist() {
		return
	}
	// NOTE: There's some unDRYness here --- e.g. we use .ExtractPartsOfFunction twice --- but that can
	// be disposed of when we strip out the evaluator.

	// Finally we can evaluate the constants and variables, which needs the full resources of the language
	// first because the RHS of the assignment can be any expression.
	// NOTE: is this even going to work any more? You also need to use the types of the variables/consts.
	// So it all needs to be thrown into a dependency digraph and sorted.
	vmm.evaluateConstantsAndVariables()
	if vmm.uP.ErrorsExist() {
		return
	}
}

func (vmm *VmMaker) compileFunctions() {
	total := 0
	for j := functionDeclaration; j <= privateCommandDeclaration; j++ {
		total = total + len(vmm.cp.p.ParsedDeclarations[j])
	}
	vmm.cp.fns = make([]*cpFunc, total)

	c := 0
	for j := functionDeclaration; j <= privateCommandDeclaration; j++ {
		for i := 0; i < len(vmm.cp.p.ParsedDeclarations[j]); i++ {
			if vmm.cp.fns[c] == nil { // This is so that if some functions are built recursively we won't waste our time.
				vmm.compileFunction(vmm.cp.mc, vmm.cp.p.ParsedDeclarations[j][i], vmm.cp.gconsts, c)
			}
			c++
		}
	}
}

// TODO This duplicates the type in the initializer and is therefore terrible.
type declarationType int

const (
	importDeclaration          declarationType = iota
	enumDeclaration                            //
	typeDeclaration                            //
	languageDeclaration                        //
	contactDeclaration                         // The fact that these things come
	constantDeclaration                        // in this order is used in the code
	variableDeclaration                        // and should not be changed without
	functionDeclaration                        // a great deal of forethought.
	privateFunctionDeclaration                 //
	commandDeclaration                         //
	privateCommandDeclaration                  //
	golangDeclaration                          // Pure golang in a block; the Charm functions with golang bodies don't go here.

)

// On the one hand, the mc must know the names of the enums and their elements so it can describe them.
// Otoh, the compiler needs to know how to turn enum literals into values.
func (vmm *VmMaker) createEnums() {
	for chunk := 0; chunk < len(vmm.uP.Parser.TokenizedDeclarations[enumDeclaration]); chunk++ {
		vmm.uP.Parser.TokenizedDeclarations[enumDeclaration][chunk].ToStart()
		tok1 := vmm.uP.Parser.TokenizedDeclarations[enumDeclaration][chunk].NextToken()
		tok2 := vmm.uP.Parser.TokenizedDeclarations[enumDeclaration][chunk].NextToken()
		if !(tok1.Type == token.IDENT && tok2.Type == token.DEF_ASSIGN) {
			vmm.uP.Throw("init/enum/lhs", tok1)
		}

		vmm.cp.mc.TypeNames = append(vmm.cp.mc.TypeNames, tok1.Literal)
		vmm.cp.typeNameToTypeList["single"] = vmm.cp.typeNameToTypeList["single"].union(altType(values.LB_ENUMS + values.ValueType(len(vmm.cp.mc.Enums))))
		vmm.cp.typeNameToTypeList["single?"] = vmm.cp.typeNameToTypeList["single?"].union(altType(values.LB_ENUMS + values.ValueType(len(vmm.cp.mc.Enums))))
		vmm.cp.typeNameToTypeList[tok1.Literal] = vmm.cp.typeNameToTypeList[tok1.Literal].union(altType(values.LB_ENUMS + values.ValueType(len(vmm.cp.mc.Enums))))
		vmm.cp.mc.Enums = append(vmm.cp.mc.Enums, []string{})
		vmm.cp.mc.Ub_enums++

		vmm.uP.Parser.TokenizedDeclarations[enumDeclaration][chunk].NextToken() // This says "enum" or we wouldn't be here.
		for tok := vmm.uP.Parser.TokenizedDeclarations[enumDeclaration][chunk].NextToken(); tok.Type != token.EOF; {
			if tok.Type != token.IDENT {
				vmm.uP.Throw("init/enum/ident", tok)
			}
			vmm.cp.enums[tok.Literal] = vmm.cp.reserve(vmm.cp.mc, values.ValueType(chunk)+values.LB_ENUMS, len(vmm.cp.mc.Enums[chunk]))
			vmm.cp.mc.Enums[chunk] = append(vmm.cp.mc.Enums[chunk], tok.Literal)

			tok = vmm.uP.Parser.TokenizedDeclarations[enumDeclaration][chunk].NextToken()
			if tok.Type != token.COMMA && tok.Type != token.WEAK_COMMA && tok.Type != token.EOF {
				vmm.uP.Throw("init/enum/comma", tok)
			}
			tok = vmm.uP.Parser.TokenizedDeclarations[enumDeclaration][chunk].NextToken()
			vmm.uP.Parser.Suffixes.Add(tok1.Literal)
		}
	}
}

func (vmm *VmMaker) compileFunction(mc *vm.Vm, node ast.Node, outerEnv *environment, ix int) *cpFunc {
	cpF := cpFunc{}
	functionName, sig, _, body, given := vmm.uP.Parser.ExtractPartsOfFunction(node)
	if body.GetToken().Type == token.PRELOG && body.GetToken().Literal == "" {
		body.(*ast.LogExpression).Value = parser.DescribeFunctionCall(functionName, &sig)
	}
	if vmm.uP.Parser.ErrorsExist() {
		return nil
	}
	if body.GetToken().Type == token.BUILTIN {
		cpF.builtin = body.(*ast.BuiltInExpression).Name
	}
	fnenv := newEnvironment()
	fnenv.ext = outerEnv
	cpF.loReg = mc.MemTop()
	for _, pair := range sig {
		if pair.VarType == "bling" {
			continue
		}
		vmm.cp.reserve(mc, values.INT, DUMMY)
		if pair.VarType == "ref" {
			vmm.cp.addVariable(mc, fnenv, pair.VarName, REFERENCE_VARIABLE, vmm.cp.typeNameToTypeList[pair.VarType])
			continue
		}
		vmm.cp.addVariable(mc, fnenv, pair.VarName, FUNCTION_ARGUMENT, vmm.cp.typeNameToTypeList[pair.VarType])
	}
	cpF.hiReg = mc.MemTop()
	cpF.callTo = mc.CodeTop()
	if body.GetToken().Type == token.BUILTIN {
		types, ok := BUILTINS[body.(*ast.BuiltInExpression).Name]
		if ok {
			cpF.types = types.t
		}
	} else {
		if given != nil {
			vmm.cp.thunkList = []thunk{}
			vmm.cp.compileNode(mc, given, fnenv)
			cpF.callTo = mc.CodeTop()
			for _, pair := range vmm.cp.thunkList {
				vmm.cp.emit(mc, vm.Thnk, pair.mLoc, pair.cLoc)
			}
		}
		cpF.types, _ = vmm.cp.compileNode(mc, body, fnenv) // TODO --- could we in fact do anything useful if we knew it was a constant?
		vmm.cp.emit(mc, vm.Ret)
		cpF.outReg = mc.That()
	}
	vmm.cp.fns[ix] = &cpF
	return &cpF
}

func (vmm *VmMaker) evaluateConstantsAndVariables() {
	vmm.cp.gvars.ext = vmm.cp.gconsts
	vmm.cp.reserve(vmm.cp.mc, values.NULL, nil)
	vmm.cp.addVariable(vmm.cp.mc, vmm.cp.gconsts, "NULL", GLOBAL_CONSTANT_PUBLIC, altType(values.NULL))
	vmm.cp.tupleType = vmm.cp.reserve(vmm.cp.mc, values.TYPE, values.TUPLE)
	for declarations := int(constantDeclaration); declarations <= int(variableDeclaration); declarations++ {
		assignmentOrder := vmm.uP.ReturnOrderOfAssignments(declarations)
		for _, v := range assignmentOrder {
			dec := vmm.uP.Parser.ParsedDeclarations[declarations][v]
			lhs := dec.(*ast.AssignmentExpression).Left
			rhs := dec.(*ast.AssignmentExpression).Right
			if lhs.GetToken().Type != token.IDENT { // TODO --- use assignment signature once tuples are working.
				vmm.uP.Throw("vmm/assign/ident", *dec.GetToken())
			}
			vname := lhs.(*ast.Identifier).Value
			runFrom := vmm.cp.mc.CodeTop()
			inferedType, _ := vmm.cp.compileNode(vmm.cp.mc, rhs, vmm.cp.gvars)
			if vmm.uP.ErrorsExist() {
				return
			}
			vmm.cp.emit(vmm.cp.mc, vm.Ret)
			vmm.cp.mc.Run(runFrom)
			if declarations == int(constantDeclaration) {
				vmm.cp.addVariable(vmm.cp.mc, vmm.cp.gconsts, vname, GLOBAL_CONSTANT_PUBLIC, inferedType)
			} else {
				vmm.cp.addVariable(vmm.cp.mc, vmm.cp.gvars, vname, GLOBAL_VARIABLE_PUBLIC, inferedType)
			}
			vmm.cp.mc.Code = vmm.cp.mc.Code[:runFrom]
		}
	}
}
