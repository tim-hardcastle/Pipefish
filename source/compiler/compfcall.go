package compiler

import (
	"strconv"

	"github.com/tim-hardcastle/Pipefish/source/ast"
	"github.com/tim-hardcastle/Pipefish/source/dtypes"
	"github.com/tim-hardcastle/Pipefish/source/err"
	"github.com/tim-hardcastle/Pipefish/source/parser"
	"github.com/tim-hardcastle/Pipefish/source/text"
	"github.com/tim-hardcastle/Pipefish/source/token"
	"github.com/tim-hardcastle/Pipefish/source/values"
	"github.com/tim-hardcastle/Pipefish/source/vm"
)

// Generating a function call is an elaborate business because of the multiple dispatch. This needs
// to be performed as much as we can at compile time with as much type inference as we can do, and
// then anything we can't figure out at compile time needs to be lowered into the bytecode to be
// dispatched at runtime.

// We generate the necessary code by working our way along the "function tree" associated with the
// name of the function.

// To achieve this, we create a PODO called a "bindle", a miscellaneous collection of the data we need
// to do the dispatch, some of which (e.g.) the token associated with the caller, are stable throughout
// the construction of the function call, while others cahnge to keep track of where we are.
type bindle struct {
	treePosition *ast.FnTreeNode // Our position on the function tree.
	branchNo     int             // The number of the branch in the function tree.
	argNo        int             // The number of the argument we're looking at.
	index        int             // The index we're looking at in the argument we're looking at.
	lengths      dtypes.Set[int] // The possible arities of the values of the argument we're looking at.
	maxLength    int             // The maximum of the 'lengths' set, or -1 if the set contains this.
	targetList   AlternateType   // The possible types associated with this tree position.
	doneList     AlternateType   // The types we've looked at up to and including those of the current branchNo.
	valLocs      []uint32        // The locations of the values evaluated from the arguments.
	types        FiniteTupleType // The types of the values.
	outLoc       uint32          // Where we're going to put the output.
	env          *Environment    // Associates variable names with memory locations
	varargsTime  bool            // Once we've taken a varArgs path, we can discard values 'til we reach bling or run out.
	tok          *token.Token    // For generating errors.
	access       CpAccess        // Whether the function call is coming from the REPL, the cmd section, etc.
	override     bool            // A kludgy flag to pass back whether we have a forward declaration that would prevent constant folding.
	libcall      bool            // Are we in a namespace?
	lowMem       uint32          // The lowest point in memory that the function uses. Hence the lowest point from which we could need to copy when putting memory on the recursionStack.
}

// This is where we come in. The `createFunctionCall` method initializes the bindle and then makes a call
// `returnTypes := cp.generateNewArgument(b)` where `b` is the bindle. This starts off the whole recursive chain
// of creating a function call by getting the (potential) type information for the first argument.
//
// The compiler in the method receiver is where we look up the function name (the "resolving compiler").
// The arguments need to be compiled in their own namespace by the argCompiler, unless they're bling in which case we
// use them to look up the function.
func (cp *Compiler) createFunctionCall(argCompiler *Compiler, node ast.Callable, ctxt Context, libcall bool) (AlternateType, bool) {
	args := node.GetArgs()
	env := ctxt.Env
	ac := ctxt.Access
	b := &bindle{tok: node.GetToken(),
		treePosition: cp.P.FunctionForest[node.GetToken().Literal].Tree,
		outLoc:       cp.reserveError("vm/oopsie", node.GetToken()),
		env:          env,
		valLocs:      make([]uint32, len(args)),
		types:        make(FiniteTupleType, len(args)),
		access:       ac,
		libcall:      libcall,
		lowMem:       ctxt.LowMem, // Where the memory of the function we're compiling (if indeed we are) starts, and so the lowest point from which we may need to copy memory in case of recursion.
	}
	backtrackList := make([]uint32, len(args))
	var cstI bool
	cst := true
	for i, arg := range args {
		backtrackList[i] = DUMMY
		if i < cp.P.FunctionForest[node.GetToken().Literal].RefCount { // It might be a reference variable
			if arg.GetToken().Type != token.IDENT {
				cp.P.Throw("comp/ref/ident", arg.GetToken())
				return AltType(values.COMPILE_TIME_ERROR), false
			}
			var v *variable
			v, ok := env.GetVar(arg.GetToken().Literal)
			if !ok {
				if ac == REPL {
					cp.P.Throw("comp/ref/var", arg.GetToken())
					return AltType(values.COMPILE_TIME_ERROR), false
				} else { // We must be in a command. We can create a local variable.
					cp.Reserve(values.UNDEFINED_TYPE, nil, node.GetToken())
					newVar := variable{cp.That(), LOCAL_VARIABLE, cp.GetAlternateTypeFromTypeName("any?")}
					env.Data[arg.GetToken().Literal] = newVar
					v = &newVar
				}
			}
			b.types[i] = cp.GetAlternateTypeFromTypeName("any?")
			cst = false
			if v.access == REFERENCE_VARIABLE { // If the variable we're passing is already a reference variable, then we don't re-wrap it.
				cp.put(vm.Asgm, v.MLoc)
				b.valLocs[i] = cp.That()
			} else {
				cp.Reserve(values.REF, v.MLoc, node.GetToken())
				b.valLocs[i] = cp.That()
			}
			continue
		}
		switch arg := arg.(type) { // It might be bling.
		case *ast.Bling:
			b.types[i] = AlternateType{blingType{arg.Value}}
			cp.Reserve(values.BLING, arg.Value, node.GetToken())
			b.valLocs[i] = cp.That()
		default: // Otherwise we emit code to evaluate it.
			b.types[i], cstI = argCompiler.CompileNode(arg, ctxt.x())
			if b.types[i].(AlternateType).Contains(values.COMPILE_TIME_ERROR) {
				return AltType(values.COMPILE_TIME_ERROR), false
			}
			if len(b.types[i].(AlternateType)) == 1 && (b.types[i].(AlternateType))[0] == SimpleType(values.TUPLE) {
				b.types[i] = AlternateType{cp.Common.AnyTuple}
			}
			cst = cst && cstI
			b.valLocs[i] = cp.That()
			if b.types[i].(AlternateType).isOnly(values.ERROR) {
				cp.P.Throw("comp/error/arg", arg.GetToken())
				return AltType(values.COMPILE_TIME_ERROR), false
			}
			if b.types[i].(AlternateType).Contains(values.ERROR) { // IMPORTANT --- find out if the ret statement is going to cause a problem with thunks as it did below before I fixed it.
				cp.Emit(vm.Qtyp, cp.That(), uint32(tp(values.ERROR)), cp.CodeTop()+4)
				backtrackList[i] = cp.CodeTop()
				cp.Emit(vm.Asgm, DUMMY, cp.That())
				cp.Emit(vm.Adtk, cp.That(), cp.That(), cp.reserveToken(arg.GetToken()))
				cp.Emit(vm.Ret)
			}
		}
	}
	// Having gotten the arguments, we create the function call itself.
	cp.cmP("Prepared bindle, making initial call into generateNewArgument.", b.tok)
	returnTypes := cp.generateNewArgument(b) // This is our path into the recursion that will in fact generate the whole function call.
	cp.cmP("Returned from initial call into generateNewArgument", b.tok)
	cp.put(vm.Asgm, b.outLoc)
	if returnTypes.isOnly(values.ERROR) && node.GetToken().Literal != "error" {
		cp.P.Throw("comp/types", b.tok, b.tok.Literal, b.types.describeWithPotentialInfix(cp.Vm, b.tok.Literal))
	}
	for _, v := range backtrackList {
		if v != DUMMY {
			cp.Vm.Code[v].Args[0] = cp.That()
		}
	}
	if returnTypes.Contains(values.ERROR) {
		cp.Emit(vm.Qtyp, cp.That(), uint32(values.ERROR), cp.CodeTop()+2)
		cp.Emit(vm.Adtk, cp.That(), cp.That(), cp.reserveToken(b.tok))
	}
	cp.cmP("Returning from createFunctionCall.", b.tok)
	return returnTypes, cst && !b.override
}

func (cp *Compiler) generateNewArgument(b *bindle) AlternateType {
	cp.cmP("Called generateNewArgument.", b.tok)
	// Case (1) : we've used up all our arguments. In this case we should look in the function tree for a function call.
	if b.argNo >= len(b.types) {
		cp.cmP("Run out of arguments, calling seekFunctionCall", b.tok)
		return cp.seekFunctionCall(b)
	}
	// Case (2) : the argument is bling.
	if len(b.types[b.argNo].(AlternateType)) == 1 {
		switch bl := (b.types[b.argNo].(AlternateType)[0]).(type) {
		case blingType:
			cp.cmP("Found bling, calling seekBling", b.tok)
			return cp.seekBling(b, bl.tag)
		}
	}
	// Case (3) : we're in varargs time.
	if b.varargsTime {
		newBindle := *b
		newBindle.argNo++
		cp.cmP("In varargs time, calling generateNewArgument.", b.tok)
		return cp.generateNewArgument(&newBindle)
	}
	// Case (4) : we have a reference.
	if b.treePosition.Branch[b.branchNo].Type.Contains(values.REF) {
		newBindle := *b
		newBindle.treePosition = b.treePosition.Branch[b.branchNo].Node
		newBindle.argNo++
		cp.cmP("Found reference variable, calling generateNewArgument.", b.tok)
		return cp.generateNewArgument(&newBindle)
	}
	// Case (5) : We aren't yet at the end of the list of arguments.
	newBindle := *b
	newBindle.index = 0
	cp.cmP("Found a new argument. Calling generateFromTopBranchDown.", b.tok)
	return cp.generateFromTopBranchDown(&newBindle)
}

func (cp *Compiler) seekBling(b *bindle, bling string) AlternateType {
	cp.cmP("Called seekBling.", b.tok)
	for i, branch := range b.treePosition.Branch {
		if branch.Type.Contains(values.BLING) {
			newBindle := *b
			newBindle.branchNo = i
			newBindle.varargsTime = false
			cp.cmP("Function seekBling calling moveAlongBranchViaSingleValue.", b.tok)
			return cp.generateMoveAlongBranchViaSingleOrTupleValue(&newBindle)
		}
	}
	return AltType(values.ERROR)
}

func (cp *Compiler) generateFromTopBranchDown(b *bindle) AlternateType {
	cp.cmP("Called generateFromTopBranchDown.", b.tok)
	newBindle := *b
	newBindle.branchNo = 0
	newBindle.targetList = typesAtIndex(b.types[b.argNo], b.index)
	cp.cmP("Function generateFromTopBranchDown @ arg "+strconv.Itoa(b.argNo)+" , index "+strconv.Itoa(b.index)+" made new target list "+newBindle.targetList.describe(cp.Vm), newBindle.tok)
	newBindle.doneList = make(AlternateType, 0, len(b.targetList))
	if newBindle.index == 0 {
		newBindle.lengths = lengths(b.types[b.argNo])
		newBindle.maxLength = maxLengthsOrMinusOne(newBindle.lengths)
	}
	cp.cmP("Calling generateBranch.", b.tok)
	return cp.generateBranch(&newBindle)
}

// We look at the current branch and see if its type can account for some, all, or none of the possibilities in the targetList.
// If the answer is "all", we can recurse on the next argument.
// If "none", then we can recurse on the next branch down.
// If "some", then we must generate a conditional where it recurses on the next argument for the types accepted by the branch
// and on the next branch for the unaccepted types.
// It may also be the run-off-the-end branch number, in which case we can generate an error.
func (cp *Compiler) generateBranch(b *bindle) AlternateType {
	cp.cmP("Called generateBranch.", b.tok)
	if b.branchNo >= len(b.treePosition.Branch) { // We've tried all the alternatives and have some left over.
		cp.reserveError("vm/types/a", b.tok)
		for _, loc := range b.valLocs {
			cp.Vm.Mem[cp.That()].V.(*err.Error).Args = append(cp.Vm.Mem[cp.That()].V.(*err.Error).Args, loc)
		}
		cp.cmP("Unthunking error 'vm/types/a'.", b.tok)
		cp.Emit(vm.UntE, cp.That())
		cp.Emit(vm.Asgm, b.outLoc, cp.That())
		return AltType(values.ERROR)
	}
	branch := b.treePosition.Branch[b.branchNo]
	acceptedTypes := branch.Type
	acceptingTuple := acceptedTypes.Contains(values.TUPLE) && b.index == 0
	isVarargs := b.varargsTime || branch.IsVararg
	isVarchar := acceptedTypes.Contains(values.STRING) && acceptedTypes.Varchar < DUMMY

	cp.cmP("Accepted types are "+acceptedTypes.String(), b.tok)
	cp.cmP("Target list is "+b.targetList.describe(cp.Vm), b.tok)
	var overlap AlternateType
	if acceptingTuple {
		cp.cmP("Accepting tuple.", b.tok)
		_, overlap = b.types[b.argNo].(AlternateType).splitSinglesAndTuples() // TODO --- it should in fact only be those types in the target list that we got from tuples.
	} else {
		overlap = AbstractTypeToAlternateType(acceptedTypes).intersect(b.targetList)
	}
	if len(overlap) == 0 { // We drew a blank.
		cp.cmP("No overlap. Calling generateNextBranchDown", b.tok)
		return cp.generateNextBranchDown(b)
	}
	// If we've got this far, the current branch accepts at least some of our types.
	newBindle := *b
	newBindle.doneList = newBindle.doneList.Union(overlap)
	// Now we need to do conditionals based on whether this is some or all, and on whether we're looking at a mix of any values and of 0th elements of tuples, or one, or the other.

	acceptedSingleTypes := make(AlternateType, 0)
	if newBindle.index == 0 { // Otherwise we *must* be looking at the index-dx position of a tuple, and there are no any values to inspect.
		acceptedSingleTypes, _ = b.types[b.argNo].(AlternateType).splitSinglesAndTuples()
	}

	// So now the length of acceptedSingleTypes tells us whether some, none, or all of the ways to follow the branch involve any values,
	// whereas the length of doneList tells us whether we need to recurse on the next branch or not.

	// We may have found a match because any string is a match for a varchar at this point. In that case we do need to do a type check on the length and
	// conditionally continue to the next branch. We can kludge this by taking STRING out of the doneList of the bindle.
	if isVarchar {
		newBindle.doneList = newBindle.doneList.without(SimpleType(values.STRING))
	}

	needsLowerBranch := len(newBindle.doneList) != len(newBindle.targetList)
	singleTypeCheck := bkGoto(DUMMY)
	elementOfTupleTypeCheck := bkGoto(DUMMY)
	varargsSlurpingTupleTypeCheck := bkGoto(DUMMY)
	if needsLowerBranch && !acceptingTuple {
		cp.cmP("Overlap is partial: "+overlap.describe(cp.Vm), b.tok)
		cp.cmP("Accepted any types are "+acceptedSingleTypes.describe(cp.Vm), b.tok)
		cp.cmP("Emitting type comparisons for any types.", b.tok)
		// Then we need to generate a conditional. Which one exactly depends on whether we're looking at a any, a tuple, or both.
		switch len(acceptedSingleTypes) {
		case 0:
			if isVarargs { // I think this has to be true at this point but it can do no harm to check.
				varargsSlurpingTupleTypeCheck = cp.emitVarargsTypeComparisonOfTupleFromAbstractType(acceptedTypes, b.valLocs[b.argNo], b.index)
			}
		case len(overlap):
			singleTypeCheck = cp.emitTypeComparisonFromAbstractType(acceptedTypes, b.valLocs[b.argNo], b.tok)
		default:
			cp.Emit(vm.Qsnq, b.valLocs[b.argNo], cp.CodeTop()+3)
			singleTypeCheck = cp.emitTypeComparisonFromAbstractType(acceptedTypes, b.valLocs[b.argNo], b.tok)
			cp.Emit(vm.Jmp, cp.CodeTop()+3)
			cp.put(vm.IxTn, b.valLocs[b.argNo], uint32(b.index))
			elementOfTupleTypeCheck = cp.emitTypeComparisonFromAbstractType(acceptedTypes, cp.That(), b.tok)
		}
	}
	// Now we're in the 'if' part of the condition we just generated, if we did. So either we definitely had
	// a type match, or we're inside a conditional that has checked for one.

	// Now we can recurse along the branch.
	// If we know whether we're looking at a any or a tuple, we can erase this and act accordingly, otherwise we generate a conditional.
	var typesFromGoingAcross, typesFromGoingDown AlternateType

	if isVarargs { // Then we don't want to move along the branch of the function tree, just get a new argument and continue.
		newBindle.treePosition = b.treePosition.Branch[b.branchNo].Node
		newBindle.varargsTime = true
		newBindle.argNo++
		cp.cmP("Varargs time, calling generateNewArgument.", b.tok)
		typesFromGoingAcross = cp.generateNewArgument(&newBindle)
	} else {
		var typesFromTuples AlternateType
		var typesFromSingles AlternateType
		switch len(acceptedSingleTypes) {
		case 0:
			cp.cmP("Nothing but tuples.", b.tok)
			if acceptedTypes.Contains(values.TUPLE) {
				cp.cmP("Type is tuple. Consuming tuple value.", b.tok)
				typesFromGoingAcross = cp.generateMoveAlongBranchViaSingleOrTupleValue(&newBindle)
			} else {
				cp.cmP("Consuming one element of the tuple.", b.tok)
				typesFromGoingAcross = cp.generateMoveAlongBranchViaTupleElement(&newBindle)
			}
		case len(overlap):
			cp.cmP("Nothing but any types", b.tok)
			if acceptedTypes.Contains(values.TUPLE) {
				cp.reserveError("vm/types/b", b.tok)
				for _, loc := range b.valLocs {
					cp.Vm.Mem[cp.That()].V.(*err.Error).Args = append(cp.Vm.Mem[cp.That()].V.(*err.Error).Args, loc)
				}
				cp.cmP("Unthunking error 'vm/types/b'.", b.tok)
				cp.Emit(vm.UntE, cp.That())
				cp.Emit(vm.Asgm, b.outLoc, cp.That())
				return AltType(values.ERROR)
			}
			cp.cmP("Going across branch consuming any value.", b.tok)
			typesFromGoingAcross = cp.generateMoveAlongBranchViaSingleOrTupleValue(&newBindle)
		default:
			skipElse := bkGoto(DUMMY)
			cp.cmP("Mix of any and tuple types.", b.tok)
			if acceptedTypes.Contains(values.TUPLE) {
				cp.cmP("Type is tuple. Generating branch to consume tuple.", b.tok)
				typesFromTuples = cp.generateMoveAlongBranchViaSingleOrTupleValue(&newBindle)
			} else {
				singleCheck := cp.vmIf(vm.Qsnq, b.valLocs[b.argNo])
				cp.cmP("Generating branch to check out any values.", b.tok)
				typesFromSingles = cp.generateMoveAlongBranchViaSingleOrTupleValue(&newBindle)
				skipElse = cp.vmGoTo()
				cp.vmComeFrom(singleCheck)
				cp.cmP("Generating branch to move along one element of a tuple.", b.tok)
				typesFromTuples = cp.generateMoveAlongBranchViaTupleElement(&newBindle)
			}

			cp.vmComeFrom(skipElse)
			typesFromGoingAcross = typesFromSingles.Union(typesFromTuples)
		}
	}

	cp.cmP("Types from going across are "+typesFromGoingAcross.describe(cp.Vm), b.tok)

	// And now we need to do the 'else' branch if there is one.
	if needsLowerBranch && !acceptingTuple {
		skipElse := cp.vmGoTo()
		cp.vmComeFrom(singleTypeCheck, elementOfTupleTypeCheck, varargsSlurpingTupleTypeCheck)
		// We recurse on the next branch down.
		cp.cmP("Function generateBranch calls generateNextBranchDown.", b.tok)
		typesFromGoingDown = cp.generateNextBranchDown(&newBindle)
		cp.vmComeFrom(skipElse)
	}
	cp.cmP("We return from generateBranch.", b.tok)
	cp.cmP("Types from going down are "+typesFromGoingDown.describe(cp.Vm), b.tok)
	return typesFromGoingAcross.Union(typesFromGoingDown)
}

var TYPE_COMPARISONS = map[string]vm.Opcode{
	"any":      vm.Qsng,
	"any?":     vm.Qsnq,
	"struct":   vm.Qstr,
	"struct?":  vm.Qstq,
}

// The reason why this and the following two functions exist is that we need to be able to emit restrictions on what values we
// can assign to things, but these restrictions may be given by the user as 'int', 'struct' or whatever, or they can have been inferred
// for a variable, and so be an AlternateType. At this point maybe we could translate everything to an AlternateType except that the
// extra burden of this conversion and the subsequent check for whether it is a built-in abstract type is  more than my conscience could
// reasonably bear. Hence a mass of pernickety little interfaces and functions try to conceal the fact that, again, I have like three-and-
// a-half ways to represent types, in the parser, in the compiler, and in the VM.
func (cp *Compiler) emitTypeComparison(typeRepresentation any, mem uint32, tok *token.Token) bkGoto {
	switch typeRepresentation := typeRepresentation.(type) {
	case string:
		return cp.emitTypeComparisonFromTypeName(typeRepresentation, mem, tok)
	case AlternateType:
		return cp.emitTypeComparisonFromAltType(typeRepresentation, mem, tok)
	case values.AbstractType:
		return cp.emitTypeComparisonFromAbstractType(typeRepresentation, mem, tok)
	}
	panic("Now this was not meant to happen.")
}

func (cp *Compiler) emitVarargsTypeComparisonOfTupleFromTypeName(typeAsString string, mem uint32, index int) bkGoto { // TODO --- more of this.
	ty := cp.GetAlternateTypeFromTypeName(typeAsString)
	args := []uint32{mem, uint32(index)}
	for _, t := range ty {
		args = append(args, uint32(t.(SimpleType)))
	}
	args = append(args, DUMMY)
	cp.Emit(vm.Qtpt, args...)
	return bkGoto(cp.CodeTop() - 1)
}

func (cp *Compiler) emitTypeComparisonFromTypeName(typeAsString string, mem uint32, tok *token.Token) bkGoto {
	cp.Cm("Emitting type comparison from typename "+text.Emph(typeAsString), tok)
	// We may have a 'varchar'.
	if len(typeAsString) >= 8 && typeAsString[0:8] == "varchar(" {
		if typeAsString[len(typeAsString)-1] == '?' {
			vChar, _ := strconv.Atoi(typeAsString[8 : len(typeAsString)-2])
			cp.Emit(vm.Qvcq, mem, uint32(vChar), DUMMY)
			return bkGoto(cp.CodeTop() - 1)
		} else {
			vChar, _ := strconv.Atoi(typeAsString[8 : len(typeAsString)-1])
			cp.Emit(vm.Qvch, mem, uint32(vChar), DUMMY)
			return bkGoto(cp.CodeTop() - 1)
		}
	}
	// It may be a plain old concrete type.
	ty := cp.GetAlternateTypeFromTypeName(typeAsString)
	if len(ty) == 1 {
		cp.Emit(vm.Qtyp, mem, uint32(ty[0].(SimpleType)), DUMMY)
		return bkGoto(cp.CodeTop() - 1)
	}
	// It may be a tuple. TODO --- I'm not sure whether I can instead safely address this case just by adding "tuple" to the cp.TypeNameToTypeList.
	if typeAsString == "tuple" {
		cp.Emit(vm.Qtyp, mem, uint32(values.TUPLE), DUMMY)
		return bkGoto(cp.CodeTop() - 1)
	}
	// It may be one of the built-in abstract types, 'struct', 'snippet', etc.
	op, ok := TYPE_COMPARISONS[typeAsString]
	if ok {
		cp.Emit(op, mem, DUMMY)
		return bkGoto(cp.CodeTop() - 1)
	}
	// It may be a user-defined abstract type.
	var abType values.AbstractType
	for _, aT := range cp.Vm.AbstractTypes { // TODO --- the lookup here and in the VM could be much faster, this by a map, that by a slice of booleans.
		if aT.Name == typeAsString {
			abType = aT.AT
			break
		}
	}
	// It may be a clone group:
	if group, ok := cp.Common.SharedTypenameToTypeList[typeAsString]; ok {
		abType = group.ToAbstractType()
	}
	if abType.Types != nil {
		args := []uint32{mem, abType.Varchar}
		for _, t := range abType.Types {
			args = append(args, uint32(t))
		}
		args = append(args, DUMMY)
		cp.Emit(vm.Qabt, args...)
		return bkGoto(cp.CodeTop() - 1)
	}
	panic("Unknown type: " + typeAsString)
}

func (cp *Compiler) emitTypeComparisonFromAltType(typeAsAlt AlternateType, mem uint32, tok *token.Token) bkGoto { // TODO --- more of this.
	cp.Cm("Emitting type comparison from alternate type "+text.Emph(typeAsAlt.describe(cp.Vm)), tok)
	if len(typeAsAlt) == 1 {
		cp.Emit(vm.Qtyp, mem, uint32(typeAsAlt[0].(SimpleType)), DUMMY)
		return bkGoto(cp.CodeTop() - 1)
	}
	args := []uint32{DUMMY} // Qabt can use this to check for varchars but (TODO) I'd need to know what to pass it.
	for _, t := range typeAsAlt {
		args = append(args, uint32(t.(SimpleType)))
	}
	args = append(args, DUMMY)
	cp.Emit(vm.Qabt, args...)
	return bkGoto(cp.CodeTop() - 1)
}

func (cp *Compiler) emitTypeComparisonFromAbstractType(abType values.AbstractType, mem uint32, tok *token.Token) bkGoto { // TODO --- more of this.
	cp.Cm("Emitting type comparison from abstract type "+text.Emph(abType.String()), tok)
	if len(abType.Types) == 1 {
		cp.Emit(vm.Qtyp, mem, uint32(abType.Types[0]), DUMMY)
		return bkGoto(cp.CodeTop() - 1)
	}
	args := []uint32{mem, DUMMY} // Qabt can use this to check for varchars but (TODO) I'd need to know what to pass it.
	for _, t := range abType.Types {
		args = append(args, uint32(t))
	}
	args = append(args, DUMMY)
	cp.Emit(vm.Qabt, args...)
	return bkGoto(cp.CodeTop() - 1)
	// TODO --- this no longer special-cases things like "any" or "struct".
}

func (cp *Compiler) emitVarargsTypeComparisonOfTupleFromAbstractType(abType values.AbstractType, mem uint32, index int) bkGoto {
	args := []uint32{mem, uint32(index)}
	for _, t := range abType.Types {
		args = append(args, uint32(t))
	}
	args = append(args, DUMMY)
	cp.Emit(vm.Qtpt, args...)
	return bkGoto(cp.CodeTop() - 1)
}

func (cp *Compiler) generateMoveAlongBranchViaTupleElement(b *bindle) AlternateType {
	cp.cmP("Called generateMoveAlongBranchViaTupleElement.", b.tok)
	// We may definitely have run off the end of all the potential tuple elements.
	if b.index == b.maxLength {
		cp.cmP("Reached the end of the tuple.", b.tok)
		newBindle := *b
		newBindle.argNo++
		return cp.generateNewArgument(&newBindle)
	}
	newBindle := *b
	newBindle.index++
	newBindle.treePosition = newBindle.treePosition.Branch[newBindle.branchNo].Node
	// We may have to generate an if-then-else to do a length check on the tuple.
	var typesFromNextArgument AlternateType
	needsConditional := b.maxLength == -1 || // Then there's a non-finite tuple
		b.lengths.Contains(newBindle.index) // Then we may have run off the end of a finite tuple.
	var skipElse bkGoto
	if needsConditional {
		cp.cmP("Generating a conditional to see if we move to the next argument or move along the tuple.", b.tok)
		lengthCheck := cp.vmIf(vm.QlnT, b.valLocs[newBindle.argNo], uint32(newBindle.index))
		newArgumentBindle := newBindle
		newArgumentBindle.argNo++
		typesFromNextArgument = cp.generateNewArgument(&newArgumentBindle)
		skipElse = cp.vmGoTo()
		cp.vmComeFrom(lengthCheck)
	}

	cp.cmP("Function generateMoveAlongBranchViaTupleElement calls generateFromTopBranchDown.", b.tok)
	typesFromContinuingInTuple := cp.generateFromTopBranchDown(&newBindle)

	if needsConditional {
		cp.vmComeFrom(skipElse)
	}

	return typesFromContinuingInTuple.Union(typesFromNextArgument)
}

func (cp *Compiler) generateMoveAlongBranchViaSingleOrTupleValue(b *bindle) AlternateType {
	cp.cmP("Called generateMoveAlongBranchViaSingleValue.", b.tok)
	newBindle := *b
	newBindle.treePosition = b.treePosition.Branch[b.branchNo].Node
	newBindle.argNo++
	newBindle.branchNo = 0
	return cp.generateNewArgument(&newBindle)
}

func (cp *Compiler) generateNextBranchDown(b *bindle) AlternateType {
	cp.cmP("Called generateNextBranchDown.", b.tok)
	newBindle := *b
	newBindle.branchNo++
	return cp.generateBranch(&newBindle)
}

func (cp *Compiler) seekFunctionCall(b *bindle) AlternateType {
	cp.cmP("Called seekFunctionCall.", b.tok)
	// This outer loop is to deal with the possibility that we're not at the leaf of
	// a tree, but there's a varargs as the next parameter, in which case we can move
	// along that branch and try again.
	var finished bool
	for !finished { 
		for _, branch := range b.treePosition.Branch {
			if branch.Node.Fn != nil {
				resolvingCompiler := branch.Node.Fn.Compiler.(*Compiler)
				fNo := branch.Node.Fn.Number
				if resolvingCompiler != cp && fNo == DUMMY {
					cp.cmP("Emitting interface backtracks", b.tok)
					cp.P.Common.InterfaceBacktracks = append(cp.P.Common.InterfaceBacktracks, parser.BkInterface{branch.Node.Fn, cp.CodeTop()}) // So we can come back and doctor all the dummy variables.
					cp.cmP("Emitting call opcode with dummy operands.", b.tok)
					args := append([]uint32{DUMMY, DUMMY, DUMMY}, b.valLocs...)
					cp.Emit(vm.Call, args...) // TODO --- find out from the sig whether this should be CalT.args := append([]uint32{DUMMY, DUMMY, DUMMY}, valLocs...)
					cp.Emit(vm.Asgm, b.outLoc, DUMMY)
					b.override = true
					return cp.rtnTypesToTypeScheme(cp.P.MakeAbstractSigFromStringSig(branch.Node.Fn.NameRets))
				}
				if fNo >= uint32(len(resolvingCompiler.Fns)) && cp == resolvingCompiler {
					cp.cmP("Undefined function. We're doing recursion!", b.tok)
					cp.Emit(vm.Rpsh, b.lowMem, cp.MemTop())
					cp.RecursionStore = append(cp.RecursionStore, BkRecursion{fNo, cp.CodeTop()}) // So we can come back and doctor all the dummy variables.
					cp.cmP("Emitting call opcode with dummy operands.", b.tok)
					cp.emitCallOpcode(fNo, b.valLocs) // As the fNo doesn't exist this will just fill in dummy values for addresses and locations.
					cp.Emit(vm.Rpop)
					cp.Emit(vm.Asgm, b.outLoc, DUMMY) // We don't know where the function's output will be yet.
					b.override = true              // We can't do constant folding on a dummy function call.
					return cp.rtnTypesToTypeScheme(cp.P.MakeAbstractSigFromStringSig(branch.Node.Fn.NameRets))
				}
				F := resolvingCompiler.Fns[fNo]
				if (b.access == REPL || b.libcall) && F.Private {
					cp.cmP("REPL trying to access private function. Returning error.", b.tok)
					cp.P.Throw("comp/private", b.tok)
					return AltType(values.COMPILE_TIME_ERROR)
				}
				// Deal with the case where the function is a builtin.
				builtinTag := F.Builtin
				functionAndType, ok := BUILTINS[builtinTag]
				if ok {
					cp.cmP("Emitting builtin.", b.tok)
					switch builtinTag { // Then for these we need to special-case their return types.
					case "get_from_sql":
						functionAndType.T = cp.Common.AnyTypeScheme
					case "cast":
						cp.Cm("Builtin is cast", b.tok)
						functionAndType.T = altType(values.ERROR)
						for _, ty := range typesAtIndex(b.types[0], 0) {
							st := values.ValueType(ty.(SimpleType))
							cp.Cm("Simple type is "+cp.Vm.DescribeType(values.ValueType(st), vm.LITERAL), b.tok)
							cp.Cm("Clone group is "+cp.TypeToCloneGroup[st].describe(cp.Vm), b.tok)
							functionAndType.T = functionAndType.T.Union(cp.TypeToCloneGroup[st])
						}
					case "first_in_tuple":
						if len(b.types) == 0 {
							functionAndType.T = altType(values.COMPILE_TIME_ERROR)
						} else {
							functionAndType.T = typesAtIndex(b.types[0], 0)
						}
					case "last_in_tuple":
						if len(b.types) == 0 {
							functionAndType.T = altType(values.COMPILE_TIME_ERROR)
						} else {
							functionAndType.T = typesAtIndex(b.types[0], len(b.types)-1)
						}
					case "tuple_of_varargs":
						if len(b.types) == 0 {
							functionAndType.T = AlternateType{FiniteTupleType{}}
						} else {
							functionAndType.T = AlternateType{FiniteTupleType{b.types[0]}}
						}
					case "tuple_of_tuple":
						functionAndType.T = b.doneList
					case "type_with":
						functionAndType.T = AlternateType{cp.GetAlternateTypeFromTypeName("struct")}.Union(AltType(values.ERROR))
					case "struct_with":
						functionAndType.T = AlternateType{cp.GetAlternateTypeFromTypeName("struct")}.Union(AltType(values.ERROR))
					}
					functionAndType.f(cp, b.tok, b.outLoc, b.valLocs)
					return functionAndType.T
				}
				typeNumber, ok := cp.GetConcreteType(builtinTag)
				// It might be a short-form struct constructor.
				if ok && cp.IsStruct(builtinTag) {
					cp.cmP("Emitting short form constructor.", b.tok)
					args := append([]uint32{b.outLoc, uint32(typeNumber)}, b.valLocs...)
					cp.Emit(vm.Strc, args...)
					return AltType(typeNumber)
				}
				// It might be a clone type constructor.
				if ok && cp.topRCompiler().isClone(builtinTag) {
					cp.cmP("Emitting clone constructor.", b.tok)
					cp.Emit(vm.Cast, b.outLoc, b.valLocs[0], uint32(typeNumber))
					return AltType(typeNumber)
				}
				// It could have a Golang body.
				if F.HasGo {
					cp.cmP("Emitting Go function call.", b.tok)
					convErrorLoc := cp.reserveError("golang/conv", b.tok)
					args := append([]uint32{b.outLoc, convErrorLoc, F.GoNumber}, b.valLocs...)
					cp.Emit(vm.Gofn, args...)
					if len(branch.Node.Fn.NameRets) == 0 {
						if F.Command {
							return AltType(values.SUCCESSFUL_VALUE, values.ERROR)
						} else {
							return cp.Common.AnyTypeScheme
						}
					}
					if len(branch.Node.Fn.NameRets) == 1 {
						return cp.GetAlternateTypeFromTypeName(branch.Node.Fn.NameRets[0].VarType)
					}
					// Otherwise it's a tuple.
					tt := make(AlternateType, 0, len(branch.Node.Fn.NameRets))
					for _, v := range branch.Node.Fn.NameRets {
						tt = append(tt, cp.GetAlternateTypeFromTypeName(v.VarType))
					}
					return AlternateType{FiniteTupleType{tt}}
				}
				// It could be a call to an external service.
				if F.Xcall != nil {
					cp.cmP("Emitting xcall.", b.tok)
					var remainingNamespace string
					vmArgs := make([]uint32, 0, len(b.valLocs)+5)
					vmArgs = append(vmArgs, b.outLoc, F.Xcall.ExternalServiceOrdinal, F.Xcall.Position)
					cp.Reserve(values.STRING, remainingNamespace, branch.Node.Fn.Body.GetToken())
					vmArgs = append(vmArgs, cp.That())
					cp.Reserve(values.STRING, F.Xcall.FunctionName, branch.Node.Fn.Body.GetToken())
					vmArgs = append(vmArgs, cp.That())
					vmArgs = append(vmArgs, b.valLocs...)
					cp.Emit(vm.Extn, vmArgs...)
					return F.RtnTypes
				}
				// Otherwise it's a regular old function call, which we do like this:
				cp.cmP("Emitting call opcode.", b.tok)
				resolvingCompiler.emitCallOpcode(fNo, b.valLocs)
				cp.Emit(vm.Asgm, b.outLoc, F.OutReg) // Because the different implementations of the function will have their own out register.
				return F.RtnTypes
			}
		}
		// We haven't found a function to call, but we may have a varargs parameter
		// in which case we can move along that branch.
		finished = true
		for _, branch := range b.treePosition.Branch {
			if branch.IsVararg {
				finished = false
				b.treePosition = branch.Node 
				break
			}
		}
	}
	cp.cmP("Returning error.", b.tok)
	cp.reserveError("vm/types/c", b.tok)
	for _, loc := range b.valLocs {
		cp.Vm.Mem[cp.That()].V.(*err.Error).Args = append(cp.Vm.Mem[cp.That()].V.(*err.Error).Args, loc)
	}
	cp.cmP("Unthunking error 'vm/types/c'.", b.tok)
	cp.Emit(vm.UntE, cp.That())
	cp.Emit(vm.Asgm, b.outLoc, cp.That())
	return AltType(values.ERROR)
}
