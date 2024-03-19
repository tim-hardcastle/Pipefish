package compiler

import (
	"pipefish/source/ast"
	"pipefish/source/initializer"
	"pipefish/source/parser"
	"pipefish/source/signature"
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

	// We make the struct names and labels, but not the constructors, which come later.
	vmm.createStructs()
	if vmm.uP.ErrorsExist() {
		return
	}

	vmm.uP.ParseEverything()
	if vmm.uP.ErrorsExist() {
		return
	}

	// An intermediate step that groups the functions by name and orders them by specificity in a "function table"
	vmm.uP.MakeFunctions(vmm.scriptFilepath)
	if vmm.uP.ErrorsExist() {
		return
	}

	// Now we turn this into a different data structure, a "function tree" with its branches labeled
	// with types. Following it tells us which version of an overloaded function to use.
	vmm.uP.MakeFunctionTrees()
	if vmm.uP.ErrorsExist() {
		return
	}
	// And we compile the functions in what is mainly a couple of loops wrapping around the aptly-named
	// compileFunction method.
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
	total = total + len(vmm.cp.p.ParsedDeclarations[typeDeclaration])
	vmm.cp.fns = make([]*cpFunc, total)

	vmm.makeConstructors()

	c := len(vmm.cp.p.ParsedDeclarations[typeDeclaration])

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

// On the one hand, the VM must know the names of the enums and their elements so it can describe them.
// Otoh, the compiler needs to know how to turn enum literals into values.
func (vmm *VmMaker) createEnums() {
	for chunk, tokens := range vmm.uP.Parser.TokenizedDeclarations[enumDeclaration] {
		tokens.ToStart()
		tok1 := tokens.NextToken()
		tok2 := tokens.NextToken()
		if !(tok1.Type == token.IDENT && tok2.Type == token.DEF_ASSIGN) {
			vmm.uP.Throw("init/enum/lhs", tok1)
		}

		if parser.TypeExists(tok1.Literal, vmm.uP.Parser.TypeSystem) {
			vmm.uP.Throw("init/enum/type", tok1)
		}

		vmm.cp.mc.TypeNames = append(vmm.cp.mc.TypeNames, tok1.Literal)
		vmm.uP.Parser.Suffixes.Add(tok1.Literal)
		vmm.uP.Parser.TypeSystem.AddTransitiveArrow(tok1.Literal, "enum")
		typeNo := values.LB_ENUMS + values.ValueType(chunk)
		vmm.cp.typeNameToTypeList["single"] = vmm.cp.typeNameToTypeList["single"].union(altType(typeNo))
		vmm.cp.typeNameToTypeList["single?"] = vmm.cp.typeNameToTypeList["single?"].union(altType(typeNo))
		vmm.cp.typeNameToTypeList[tok1.Literal] = altType(typeNo)
		vmm.cp.typeNameToTypeList[tok1.Literal+"?"] = altType(values.NULL, typeNo)
		vmm.cp.mc.Enums = append(vmm.cp.mc.Enums, []string{})
		vmm.cp.mc.Ub_enums++

		tokens.NextToken() // This says "enum" or we wouldn't be here.
		for tok := tokens.NextToken(); tok.Type != token.EOF; {
			if tok.Type != token.IDENT {
				vmm.uP.Throw("init/enum/ident", tok)
			}
			_, alreadyExists := vmm.cp.enumElements[tok.Literal]
			if alreadyExists { // Enums in the same namespace can't have overlapping elements or we wouldn't know their type.
				vmm.uP.Throw("init/enum/element", tok)
			}

			vmm.cp.enumElements[tok.Literal] = vmm.cp.reserve(vmm.cp.mc, values.ValueType(chunk)+values.LB_ENUMS, len(vmm.cp.mc.Enums[chunk]))
			vmm.cp.mc.Enums[chunk] = append(vmm.cp.mc.Enums[chunk], tok.Literal)

			tok = tokens.NextToken()
			if tok.Type != token.COMMA && tok.Type != token.WEAK_COMMA && tok.Type != token.EOF {
				vmm.uP.Throw("init/enum/comma", tok)
			}
			tok = tokens.NextToken()
		}
	}
}

func (vmm *VmMaker) createStructs() {
	for chunk, node := range vmm.uP.Parser.ParsedDeclarations[typeDeclaration] {
		lhs := node.(*ast.AssignmentExpression).Left
		if lhs.GetToken().Type != token.IDENT {
			vmm.uP.Throw("init/enum/lhs", *lhs.GetToken())
		}
		name := lhs.GetToken().Literal

		_, alreadyExists := vmm.cp.structNumbers[name]
		if alreadyExists {
			vmm.uP.Throw("init/struct/type", *lhs.GetToken())
		}

		// We make the type itself exist.

		vmm.cp.mc.TypeNames = append(vmm.cp.mc.TypeNames, name)
		typeNo := vmm.cp.mc.Ub_enums + values.ValueType(chunk)
		vmm.cp.typeNameToTypeList["single"] = vmm.cp.typeNameToTypeList["single"].union(altType(typeNo))
		vmm.cp.typeNameToTypeList["single?"] = vmm.cp.typeNameToTypeList["single?"].union(altType(typeNo))
		vmm.cp.typeNameToTypeList["struct"] = vmm.cp.typeNameToTypeList["struct"].union(altType(typeNo))
		vmm.cp.typeNameToTypeList["struct?"] = vmm.cp.typeNameToTypeList["struct?"].union(altType(typeNo))
		vmm.cp.typeNameToTypeList[name] = altType(typeNo)
		vmm.cp.typeNameToTypeList[name+"?"] = altType(values.NULL, typeNo)
		vmm.cp.structNumbers[name] = typeNo

		// The parser needs to know about it too.

		vmm.uP.Parser.Functions.Add(name)
		sig := node.(*ast.AssignmentExpression).Right.(*ast.StructExpression).Sig
		vmm.cp.p.FunctionTable.Add(vmm.cp.p.TypeSystem, name, ast.Function{Sig: sig, Body: &ast.BuiltInExpression{Name: name}, Number: uint32(chunk)}) // TODO --- give them their own ast type?

		// We make the labels exist.

		labelsForStruct := make([]int, 0, len(sig))
		for _, labelNameAndType := range sig {
			labelName := labelNameAndType.VarName
			labelLocation, alreadyExists := vmm.cp.fieldLabels[labelName]
			if alreadyExists { // Structs can of course have overlapping fields but we don't want to declare them twice..
				labelsForStruct = append(labelsForStruct, vmm.cp.mc.Mem[labelLocation].V.(int))
			} else {
				vmm.cp.fieldLabels[labelName] = vmm.cp.reserve(vmm.cp.mc, values.LABEL, len(vmm.cp.mc.Labels))
				labelsForStruct = append(labelsForStruct, len(vmm.cp.mc.Labels))
				vmm.cp.mc.Labels = append(vmm.cp.mc.Labels, labelName)
			}
		}
		vmm.cp.mc.StructLabels = append(vmm.cp.mc.StructLabels, labelsForStruct)
		vmm.cp.mc.StructResolve = vmm.cp.mc.StructResolve.Add(chunk, labelsForStruct)
	}
}

func (vmm *VmMaker) makeConstructors() {
	for i, node := range vmm.uP.Parser.ParsedDeclarations[typeDeclaration] {
		name := node.(*ast.AssignmentExpression).Left.GetToken().Literal // We know this and the next line are safe because we already checked in createStructs
		sig := node.(*ast.AssignmentExpression).Right.(*ast.StructExpression).Sig
		vmm.cp.fns[i] = vmm.compileConstructor(vmm.cp.mc, name, sig)
	}
}

func (vmm *VmMaker) compileConstructor(mc *vm.Vm, name string, sig signature.Signature) *cpFunc {
	typeNo := vmm.cp.structNumbers[name]
	cpF := &cpFunc{types: altType(typeNo), builtin: name}
	fnenv := newEnvironment() // Note that we don't use this for anything, we just need some environment to pass to addVariables.
	cpF.loReg = mc.MemTop()
	for _, pair := range sig {
		vmm.cp.addVariable(mc, fnenv, pair.VarName, FUNCTION_ARGUMENT, vmm.cp.typeNameToTypeList[pair.VarType])
	}
	cpF.hiReg = mc.MemTop()
	return cpF
}

func (vmm *VmMaker) compileFunction(mc *vm.Vm, node ast.Node, outerEnv *environment, ix int) *cpFunc {
	cpF := cpFunc{}
	functionName, sig, _, body, given, tupleList := vmm.uP.Parser.ExtractPartsOfFunction(node)
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
		vmm.cp.reserve(mc, values.UNDEFINED_VALUE, DUMMY)
		if pair.VarType == "ref" {
			vmm.cp.addVariable(mc, fnenv, pair.VarName, REFERENCE_VARIABLE, vmm.cp.typeNameToTypeList[pair.VarType])
			continue
		}
		vmm.cp.addVariable(mc, fnenv, pair.VarName, FUNCTION_ARGUMENT, vmm.cp.typeNameToTypeList[pair.VarType])
	}
	cpF.hiReg = mc.MemTop()
	cpF.callTo = mc.CodeTop()
	if len(tupleList) > 0 {
		cpF.tupleReg = vmm.cp.reserve(mc, values.INT_ARRAY, tupleList)
	} else {
		cpF.tupleReg = DUMMY
	}
	if body.GetToken().Type == token.BUILTIN {
		name := body.(*ast.BuiltInExpression).Name
		types, ok := BUILTINS[name]
		if ok {
			cpF.types = types.t
		} else {
			structNo, ok := vmm.cp.structNumbers[name]
			if ok {
				cpF.types = altType(structNo)
			}
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
