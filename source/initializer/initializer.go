package initializer

import (
	"embed"
	"os"
	"path/filepath"
	"sort"
	"testing"

	"strings"

	"github.com/tim-hardcastle/Pipefish/source/ast"
	"github.com/tim-hardcastle/Pipefish/source/compiler"
	"github.com/tim-hardcastle/Pipefish/source/dtypes"
	"github.com/tim-hardcastle/Pipefish/source/parser"
	"github.com/tim-hardcastle/Pipefish/source/settings"
	"github.com/tim-hardcastle/Pipefish/source/text"
	"github.com/tim-hardcastle/Pipefish/source/token"
	"github.com/tim-hardcastle/Pipefish/source/values"
	"github.com/tim-hardcastle/Pipefish/source/vm"

	"src.elv.sh/pkg/persistent/vector"
)

//go:embed rsc-pf/*
var folder embed.FS

// The initializer contains the methods and data which are only needed when parsing and compiling a script.
// It returns a compiler which has a parser which are capable between them of parsing, compiling, and running
// a single block/AST of Pipefish code, and which can therefore cope with everything demanded of them via
// the REPL at runtime.

// There is one initializer per module, just as there is one compiler and one parser. The others are constructed
// recursively as made necessary by the `import` and `external` declarations.

// Definition of the Initializer type.
type Initializer struct {
	cp                                  *compiler.Compiler           // The compiler for the module being intitialized.
	P                                   *parser.Parser               // The parser for the module being initialized.
	initializers                        map[string]*Initializer      // The child initializers of this one, to initialize imports and external stubs.
	tokenizedCode                       [][]tokenizedCode            // Code arranged by declaration type and lightly chunked and validated.
	parsedCode                          [][]parsedCode               // What you get by parsing that.
	localConcreteTypes                  dtypes.Set[values.ValueType] // All the struct, enum, and clone types defined in a given module.
	goBucket                            *GoBucket                    // Where the initializer keeps information gathered during parsing the script that will be needed to compile the Go modules.
	snippets                            []string                     // Names of snippet types visible to the module.
	Common                              *CommonInitializerBindle     // The information all the initializers have in Common.
	structDeclarationNumberToTypeNumber map[int]values.ValueType     // Maps the order of the declaration of the struct in the script to its type number in the VM. TODO --- there must be something better than this.
	unserializableTypes                 dtypes.Set[string]           // Keeps track of which abstract types are mandatory imports/singletons of a concrete type so we don't try to serialize them.

	functionTable functionTable // Intermediate step towards constructing the FunctinTree used by the compiler.

	// Holds the definitions of parameterized types.
	parameterizedTypes map[string][]ParameterInfo
	// Stores information we need to compile the runtime typechecks on parameterized type instances.
	parameterizedInstanceMap map[string]parameterizedTypeInstance
}

// Makes a new initializer.
func NewInitializer() *Initializer {
	iz := Initializer{
		initializers:             make(map[string]*Initializer),
		localConcreteTypes:       make(dtypes.Set[values.ValueType]),
		unserializableTypes:      make(dtypes.Set[string]),
		tokenizedCode:            make([][]tokenizedCode, 14),
		functionTable:            make(functionTable),
		parameterizedTypes:       make(map[string][]ParameterInfo),
		parameterizedInstanceMap: make(map[string]parameterizedTypeInstance),
	}
	iz.newGoBucket()
	return &iz
}

// The CommonInitializerBindle contains information that all the initializers need to share.
type CommonInitializerBindle struct {
	Functions      map[funcSource]*parsedFunction // This is to ensure that the same function (i.e. from the same place in source code) isn't parsed more than once.
	DeclarationMap map[decKey]any                 // This prevents redeclaration of types in the same sort of way.
	HubCompilers   map[string]*compiler.Compiler  // This is a map of the compilers of all the (potential) external services on the same hub.
	HubStore       *values.Map
}

// Initializes the `CommonInitializerBindle`
func NewCommonInitializerBindle(store *values.Map) *CommonInitializerBindle {
	b := CommonInitializerBindle{
		Functions:      make(map[funcSource]*parsedFunction),
		DeclarationMap: make(map[decKey]any),
		HubCompilers:   make(map[string]*compiler.Compiler),
		HubStore:       store,
	}
	return &b
}

// After hooking it up with the parameterized type definitions.
type parameterizedTypeInstance struct {
	astType   ast.TypeNode
	env       *compiler.Environment
	typeCheck *token.TokenizedCodeChunk
	fields    ast.AstSig
	vals      []values.Value
}

type typeOperatorInfo struct {
	constructorSig ast.AstSig // The signature of the constructor, before we prepend the secret-sauce `+t type` parameter.
	isClone        bool
	returnTypes    compiler.AlternateType
	definedAt      []*token.Token
}

// Initializes a compiler.
func newCompiler(Common *parser.CommonParserBindle, ccb *compiler.CommonCompilerBindle, scriptFilepath, sourcecode string, mc *vm.Vm, namespacePath string) *compiler.Compiler {
	p := parser.New(Common, scriptFilepath, sourcecode, namespacePath)
	cp := compiler.NewCompiler(p, ccb)
	cp.ScriptFilepath = scriptFilepath
	cp.Vm = mc
	cp.TupleType = cp.Reserve(values.TYPE, values.AbstractType{[]values.ValueType{values.TUPLE}}, &token.Token{Source: "Builtin constant"})
	return cp
}

// The public function serving as a way in.
func StartCompilerFromFilepath(filepath string, svs map[string]*compiler.Compiler, store *values.Map) (*compiler.Compiler, error) {
	sourcecode, e := compiler.GetSourceCode(filepath)
	if e != nil {
		return nil, e
	}
	return StartCompiler(filepath, sourcecode, svs, store), nil
}

// We begin by manufacturing a blank VM, a `CommonParserBindle` for all the parsers to share, and a
// `CommonInitializerBindle` for the initializers to share. These Common bindles are then passed down to the
// "children" of the intitializer and the parser when new modules are created.
func StartCompiler(scriptFilepath, sourcecode string, hubServices map[string]*compiler.Compiler, store *values.Map) *compiler.Compiler {
	iz := NewInitializer()
	iz.Common = NewCommonInitializerBindle(store)
	iz.Common.HubCompilers = hubServices
	// We then carry out eleven phases of initialization each of which is performed recursively on all of the
	// modules in the dependency tree before moving on to the next. (The need to do this is in fact what
	// defines the phases, so you shouldn't bother looking for some deeper logic in that.)
	iz.cmI("Parsing everything.")
	result := iz.ParseEverythingFromSourcecode(vm.BlankVm(), parser.NewCommonParserBindle(), compiler.NewCommonCompilerBindle(), scriptFilepath, sourcecode, "")
	if iz.errorsExist() {
		iz.cp.P.Common.IsBroken = true
		return result
	}
	iz.cmI("Finding shareable functions.")
	iz.findAllShareableFunctions()
	if iz.errorsExist() {
		iz.cp.P.Common.IsBroken = true
		return result
	}
	iz.cmI("Populating interface types.")
	iz.populateInterfaceTypes()
	if iz.errorsExist() {
		iz.cp.P.Common.IsBroken = true
		return result
	}
	iz.cmI("Populating abstract types and creating alternate types.")
	iz.populateAbstractTypes()
	if iz.errorsExist() {
		iz.cp.P.Common.IsBroken = true
		return result
	}

	iz.cmI("Making function tables.")
	iz.MakeFunctionTables()
	if iz.errorsExist() {
		iz.cp.P.Common.IsBroken = true
		return result
	}

	iz.cmI("Making function forest.")
	iz.MakeFunctionForests()
	if iz.errorsExist() {
		iz.cp.P.Common.IsBroken = true
		return result
	}
	iz.cmI("Adding abstract types to struct fields.")
	iz.addFieldsToStructsAndCheckForConsistency()
	if iz.errorsExist() {
		iz.cp.P.Common.IsBroken = true
		return result
	}
	iz.cmI("Adding abstract types to parameterized types.")
	iz.tweakParameterizedTypes()
	if iz.errorsExist() {
		iz.cp.P.Common.IsBroken = true
		return result
	}

	iz.cmI("Adding parameterized types to VM.")
	iz.addParameterizedTypesToVm()
	if iz.errorsExist() {
		iz.cp.P.Common.IsBroken = true
		return result
	}

	iz.cmI("Compiling Go.")
	iz.compileGoModules()
	if iz.errorsExist() {
		iz.cp.P.Common.IsBroken = true
		return result
	}
	iz.cmI("Compiling everything else.")
	iz.compileEverythingElse()
	if iz.errorsExist() {
		iz.cp.P.Common.IsBroken = true
		return result
	}
	iz.cmI("Resolving interface backtracks.")
	iz.resolveInterfaceBacktracks()

	iz.cmI("Serializing API")
	iz.cp.API = iz.SerializeApi()

	return result

}

// This initializes the initializer's compiler (which initializes its parser), and
// extracts the source code from the given file, and then calls the `parseEverything“
// method, below.
func (iz *Initializer) ParseEverythingFromSourcecode(mc *vm.Vm, cpb *parser.CommonParserBindle, ccb *compiler.CommonCompilerBindle, scriptFilepath, sourcecode, namespacePath string) *compiler.Compiler {
	iz.cp = newCompiler(cpb, ccb, scriptFilepath, sourcecode, mc, namespacePath)
	iz.P = iz.cp.P
	iz.parseEverything(scriptFilepath, sourcecode)
	iz.cp.ScriptFilepath = scriptFilepath
	if !(scriptFilepath == "" || scriptFilepath == "InitializeFromCode" ||
		(len(scriptFilepath) >= 5 && scriptFilepath[0:5] == "http:")) &&
		!testing.Testing() && !(len(scriptFilepath) >= 11 && scriptFilepath[:11] == "test-files/") {
		file, err := os.Stat(text.MakeFilepath(scriptFilepath))
		if err != nil {
			iz.throw("init/source", LINKING_TOKEN, scriptFilepath)
			return nil
		}
		iz.cp.Timestamp = file.ModTime().UnixMilli()
	}
	iz.P.Common.Sources[scriptFilepath] = strings.Split(sourcecode, "\n")
	return iz.cp
}

// We find the shareable functions.
func (iz *Initializer) findAllShareableFunctions() {
	// First we recursively call the method on all the dependencies of the module.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.findAllShareableFunctions()
	}
	// If at least one of the parameters of a function in the module has a parameter which
	// can only accept locally defined concrete types, then in principle this function might
	// fulfil an interface and be shared with the module that defines the interface.
	iz.findShareableFunctions()
}

// If a function in the module must have at least one of its parameters some type
// defined in the module, then we add a parsedFunction representation of it
// to the list of functions in the common initializer bindle.
func (iz *Initializer) findShareableFunctions() {
	for j := functionDeclaration; j <= commandDeclaration; j++ {
		for i := 0; i < len(iz.parsedCode[j]); i++ {
			fn := iz.parsedCode[j][i].(*parsedFunction)
			tok := &fn.op
			if iz.shareable(fn) || settings.MandatoryImportSet().Contains(tok.Source) {
				iz.Common.Functions[funcSource{tok.Source, tok.Line, fn.op.Literal, uint32(fn.pos)}] = fn
			}
		}
	}
}

// Function auxiliary to the above. A function is shareable if at least one of its parameters must be of a type
// declared in the same module.
func (iz *Initializer) shareable(f *parsedFunction) bool {
	for _, pair := range f.sig {
		ty := pair.VarType
		if _, ok := ty.(*ast.Bling); ok {
			continue
		}
		if t, ok := ty.(*ast.TypeDotDotDot); ok {
			ty = t.Right
		}
		if t, ok := ty.(*ast.TypeWithName); ok &&
			(t.Name == "struct" || t.Name == "enum") {
			continue
		}
		abType := iz.P.GetAbstractType(ty)
		ok := true
		for _, concType := range abType.Types {
			if !iz.localConcreteTypes.Contains(concType) {
				ok = false
			}
		}
		if ok {
			return true
		}
	}
	return false
}

func (iz *Initializer) populateInterfaceTypes() {
	// First we recursively call the method on all the dependencies of the module.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.populateInterfaceTypes()
	}
	iz.addAbstractTypesToVm() // TODO --- this is the first of two times we're going to do this when what we really need is another topological sort.
	// We pull in all the shared functions that fulfill the interface types, populating the types as we go.
	for _, tc := range iz.tokenizedCode[interfaceDeclaration] {
		dec := tc.(*tokenizedInterfaceDeclaration)
		typeInfo, _ := iz.getDeclaration(decINTERFACE, ixPtr(dec), DUMMY)
		types := values.MakeAbstractType()
		funcsToAdd := map[values.ValueType][]*parsedFunction{}
		for i, sigToMatch := range typeInfo.(interfaceInfo).sigs {
			typesMatched := values.MakeAbstractType()
			for key, fnToTry := range iz.Common.Functions {
				if key.functionName == sigToMatch.name {
					matches := iz.getMatches(sigToMatch, fnToTry, ixPtr(dec))
					typesMatched = typesMatched.Union(matches)
					if !settings.MandatoryImportSet().Contains(fnToTry.op.Source) {
						for _, ty := range matches.Types {
							if _, ok := funcsToAdd[ty]; ok {
								funcsToAdd[ty] = append(funcsToAdd[ty], fnToTry)
							} else {
								funcsToAdd[ty] = []*parsedFunction{fnToTry}
							}
						}
					}
				}
			}
			if i == 0 {
				types = typesMatched
			} else {
				types = types.Intersect(typesMatched)
			}
		}
		// We have created an abstract type from our interface! We put it in the type map.
		iz.P.TypeMap[dec.op.Literal] = types
		iz.addTypeToVm(values.AbstractTypeInfo{dec.op.Literal, iz.P.NamespacePath, types, settings.MandatoryImportSet().Contains(dec.op.Source)})
		// And we add all the implicated functions to the function table.
		for _, ty := range types.Types {
			for _, fn := range funcsToAdd[ty] {
				conflictingFunction := iz.Add(fn.op.Literal, fn)
				if conflictingFunction != nil && conflictingFunction != fn {
					iz.P.Throw("init/overload/b", &fn.op, fn.op.Literal, conflictingFunction.op)
				}
			}
		}
	}
}

func (iz *Initializer) populateAbstractTypes() {
	// First we recursively call the method on all the dependencies of the module.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.populateAbstractTypes()
	}
	// The vm needs to know how to describe the abstract types in words.
	iz.addAbstractTypesToVm()
	if iz.errorsExist() {
		return
	}

	// The compiler uses a somewhat richer type representation than the one used by the compiler and the
	// runtime.
	iz.makeAlternateTypesFromAbstractTypes()
}

// We add the abstract types to the VM.
//
// The VM doesn't *use* abstract types, but they are what values of type TYPE contain, and so it needs to
// be able to describe them.
func (iz *Initializer) addAbstractTypesToVm() {
	// For consistent results for tests, it is desirable that the types should be listed in a fixed order.
	keys := []string{}
	for typeName, _ := range iz.P.TypeMap {
		keys = append(keys, typeName)
	}
	for typeName, _ := range iz.P.Common.Types {
		keys = append(keys, typeName)
	}
	sort.Slice(keys, func(i, j int) bool { return keys[i] < keys[j] })
	for _, typeName := range keys {
		if text.Head(typeName, "clones{") && len(iz.P.GetAbstractTypeFromTypeSys(typeName).Types) == 1 {
			continue
		}
		iz.addTypeToVm(values.AbstractTypeInfo{Name: typeName, Path: iz.P.NamespacePath,
			AT: iz.P.GetAbstractTypeFromTypeSys(typeName), IsMI: iz.unserializableTypes.Contains(typeName)})
	}
	for _, v := range parser.ClonableTypes { // Clonable types are clones of themselves.
		selfInfo := iz.cp.Vm.ConcreteTypeInfo[v].(vm.BuiltinType)
		selfInfo = selfInfo.AddClone(values.ValueType(v))
		iz.cp.Vm.ConcreteTypeInfo[v] = selfInfo
	}
	for i, v := range iz.cp.Vm.ConcreteTypeInfo {
		if v.IsClone() {
			parentType := v.(vm.CloneType).Parent
			parentInfo := iz.cp.Vm.ConcreteTypeInfo[parentType].(vm.BuiltinType)
			parentInfo = parentInfo.AddClone(values.ValueType(i))
			iz.cp.Vm.ConcreteTypeInfo[parentType] = parentInfo
		}
	}
}

// We make the alternate types from the abstract types, because the compiler
// is shortly going to need them.
//
// OTOH, we want the type information spread across the parsers and shared in the Common parser bindle to
// collectively be the any source of truth for our type system.
// But it can't be the only *representation* of the truth, becase that would slow things down 'cos the compiler
// would have to keep converting abstract types to alternate types to build the type schemes with.
// The solution is to build the alternate type schemes once and for all from the alternate types, after we've
// entirely finished generating the data in the parsers.
func (iz *Initializer) makeAlternateTypesFromAbstractTypes() {
	iz.cp.TypeNameToTypeScheme = make(map[string]compiler.AlternateType)
	for typename, abType := range iz.P.TypeMap {
		iz.cp.TypeNameToTypeScheme[typename] = compiler.AbstractTypeToAlternateType(abType)
	}
	for typename, abType := range iz.P.Common.Types {
		iz.cp.TypeNameToTypeScheme[typename] = compiler.AbstractTypeToAlternateType(abType)
	}
}

// At this point we have our functions as `parsedCode`. We want to read their signatures
// and order them according to specificity for the purposes of implementing overloading.
func (iz *Initializer) MakeFunctionTables() {
	// First we recursively call the method on all the dependencies of the module.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.MakeFunctionTables()
	}
	iz.makeFunctionTable()
	if settings.FUNCTION_TO_PEEK != "" {
		println("In namespace", iz.P.NamespacePath)
		println(iz.functionTable.Describe(settings.FUNCTION_TO_PEEK))
	}
}

// Function auxillary to the above for making one function table.
func (iz *Initializer) makeFunctionTable() {
	for j := functionDeclaration; j <= commandDeclaration; j++ {
		for i, dec := range iz.parsedCode[j] {
			fn := dec.(*parsedFunction)
			tok := fn.op
			functionName := fn.op.Literal
			var (
				ok            bool
				functionToAdd *parsedFunction
			)
			if functionToAdd, ok = iz.Common.Functions[funcSource{tok.Source, tok.Line, functionName, uint32(fn.pos)}]; ok {
			} else {
				functionToAdd = fn
			}
			iz.parsedCode[j][i].(*parsedFunction).callInfo = functionToAdd.callInfo
			conflictingFunction := iz.Add(functionName, functionToAdd)
			if conflictingFunction != nil && conflictingFunction != functionToAdd {
				iz.P.Throw("init/overload/a", &fn.op, functionName, conflictingFunction.op)
				return
			}
		}
	}
}

func (iz *Initializer) MakeFunctionForests() {
	// First we recurse.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.MakeFunctionForests()
	}

	// Now we turn the function tables into a different data structure, a "function tree" with its branches labeled
	// with types. Following it tells us which version of an overloaded function to use.
	iz.MakeFunctionTrees()
	if tree, ok := iz.cp.FunctionForest[settings.FUNCTION_TO_PEEK]; ok && settings.FUNCTION_TO_PEEK != "" {
		println("In namespace", iz.P.NamespacePath, "function tree for "+settings.FUNCTION_TO_PEEK)
		println(tree.Tree.IndentString("") + "\n")
	}
}

// Function auxiliary to the above. Having made the parsers FunctionTable, each function name is associated with a
// (partially) ordered list of associated functions such that a more specific type signature comes before a less
// specific one. We will now re-represent this as a tree.
func (iz *Initializer) MakeFunctionTrees() {
	iz.cp.FunctionForest = map[string]*compiler.FunctionTree{}
	rc := 0
	for k, v := range iz.functionTable {
		tree := &compiler.FnTreeNode{CallInfo: nil, Branch: []*compiler.TypeNodePair{}}
		for i := range v {
			tree = iz.addSigToTree(tree, v[i], 0)

			refs := 0 // Overloaded functions must have the same number of reference variables, which go at the start.
			for ; refs < len(v[i].sig) && ast.IsRef(v[i].sig[refs].VarType); refs++ {
			}
			if i == 0 {
				rc = refs
			} else {
				if refs != rc {
					iz.P.Throw("init/overload/ref", &v[i].op)
					break
				}
			}
		}
		iz.cp.FunctionForest[k] = &compiler.FunctionTree{Tree: tree, RefCount: rc}
	}
}

// Note that the sigs have already been sorted on their specificity.
func (iz *Initializer) addSigToTree(tree *compiler.FnTreeNode, fn *parsedFunction, pos int) *compiler.FnTreeNode {
	nameSig := fn.sig // TODO --- do we really need both of these?
	sig := fn.callInfo.Compiler.P.MakeAbstractSigFromStringSig(nameSig)
	if pos < len(sig) {
		var currentTypeName string
		currentAbstractType := sig[pos].VarType
		if _, ok := nameSig[pos].VarType.(*ast.Bling); ok {
			currentTypeName = nameSig[pos].VarName
		} else {
			currentTypeName = nameSig[pos].VarType.String()
		}
		isVararg := len(currentTypeName) >= 3 && currentTypeName[:3] == "..."
		if isVararg {
			currentTypeName = currentTypeName[3:]
		}
		isPresent := false
		for _, v := range tree.Branch {
			if currentAbstractType.Equals(v.Type) {
				isPresent = true
				break
			}
		}
		if !isPresent {
			tree.Branch = append(tree.Branch, &compiler.TypeNodePair{Type: currentAbstractType, IsVararg: isVararg, Node: &compiler.FnTreeNode{CallInfo: nil, Branch: []*compiler.TypeNodePair{}}})
		}
		for _, branch := range tree.Branch {
			if branch.Type.IsSubtypeOf(currentAbstractType) {
				branch.Node = iz.addSigToTree(branch.Node, fn, pos+1)
				if (currentTypeName == "tuple") && !(branch.Type.Contains(values.TUPLE)) {
					iz.addSigToTree(branch.Node, fn, pos)
				}
				if isVararg && !branch.IsVararg {
					iz.addSigToTree(branch.Node, fn, pos)
				}
			}
		}
	} else {
		if tree.CallInfo == nil { // If it is non-nil then a sig of greater specificity has already led us here and we're good.
			tree.Branch = append(tree.Branch, &compiler.TypeNodePair{Type: values.MakeAbstractType(), Node: &compiler.FnTreeNode{CallInfo: fn.callInfo, Branch: []*compiler.TypeNodePair{}}})
		}
	}
	return tree
}

// We assign abstract types to the fields of the structs, and chek for consistency of
// private types, i.e. a struct type declared public can't have field types declared private.
func (iz *Initializer) addFieldsToStructsAndCheckForConsistency() {
	// First we recurse.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.addFieldsToStructsAndCheckForConsistency()
	}
	iz.cmI("Adding abstract types of fields to structs.")
	iz.addFieldsToStructs()
	if iz.errorsExist() {
		return
	}

	iz.cmI("Adding abstract types of fields to perameterized structs.")
	iz.addFieldsToParameterizedStructs()
	if iz.errorsExist() {
		return
	}

	// We want to ensure that no public type (whether a struct or abstract type) contains a private type.
	iz.cmI("Checking types for consistency of encapsulation.")
	iz.checkTypesForConsistency()
	if iz.errorsExist() {
		return
	}
}

// Adds field types to structs.
func (iz *Initializer) addFieldsToStructs() {
	for i, tc := range iz.tokenizedCode[structDeclaration] {
		izTypeInfo, _ := iz.getDeclaration(decSTRUCT, ixPtr(tc), DUMMY)
		if izTypeInfo == nil { // This will happen if it's a parameterized declaration.
			continue
		}
		izStructInfo := izTypeInfo.(structInfo)
		typeNumber := iz.structDeclarationNumberToTypeNumber[i]
		iz.addFields(typeNumber, izStructInfo.sig)
	}
}

func (iz *Initializer) addFieldsToParameterizedStructs() {
	for _, parTypeInfo := range iz.parameterizedInstanceMap {
		typeNo := iz.cp.ConcreteTypeNow(parTypeInfo.astType.String())
		if iz.cp.Vm.ConcreteTypeInfo[typeNo].IsStruct() {
			iz.addFields(typeNo, parTypeInfo.fields)
		}
	}
}

func (iz *Initializer) addFields(typeNumber values.ValueType, sig ast.AstSig) {
	structInfo := iz.cp.Vm.ConcreteTypeInfo[typeNumber].(vm.StructType)
	structTypes := make([]values.AbstractType, 0, len(sig))
	for _, labelNameAndType := range sig {
		typeAst := labelNameAndType.VarType
		abType := iz.P.GetAbstractType(typeAst)
		structTypes = append(structTypes, abType)
	}
	structInfo.AbstractStructFields = structTypes
	iz.cp.Vm.ConcreteTypeInfo[typeNumber] = structInfo
}

func (iz *Initializer) tweakParameterizedTypes() {
	// We replace the astTypes in the environment for typechecking a parameterized type with AbstractTypes.
	for _, pti := range iz.parameterizedInstanceMap {
		for _, v := range pti.env.Data {
			if iz.cp.Vm.Mem[v.MLoc].T == values.TYPE {
				iz.cp.Vm.Mem[v.MLoc].V = iz.P.GetAbstractType(iz.cp.Vm.Mem[v.MLoc].V.(ast.TypeNode))
			}
		}
	}
	for typename, pti := range iz.parameterizedInstanceMap {
		typeNo := iz.cp.ConcreteTypeNow(typename)
		typeInfo := iz.cp.Vm.ConcreteTypeInfo[typeNo]
		vals := make([]values.Value, len(pti.vals))
		for i, v := range pti.vals {
			vals[i] = iz.tweakValue(v)
		}
		switch typeInfo := typeInfo.(type) {
		case vm.CloneType:
			typeInfo.TypeArguments = vals
			iz.cp.Vm.ConcreteTypeInfo[typeNo] = typeInfo
		case vm.StructType:
			typeInfo.TypeArguments = vals
			iz.cp.Vm.ConcreteTypeInfo[typeNo] = typeInfo
		default:
			panic("unhandled case")
		}
	}
}

// This adds information about the parameterized types to the VM.
// It also tweaks the arguments to convert the payload of TYPE from the improper AstType to the correct AbstractType.
func (iz *Initializer) addParameterizedTypesToVm() {
	for _, ty := range iz.parameterizedInstanceMap { // TODO --- is there a reason why there aren't all *ast.TypeWithArguments?
		name := ty.astType.(*ast.TypeWithArguments).Name
		typeArgs := []values.Value{}
		for _, v := range ty.astType.(*ast.TypeWithArguments).Arguments {
			if v.Type == values.TYPE {
				typeArgs = append(typeArgs, values.Value{values.TYPE, iz.P.GetAbstractType(v.Value.(ast.TypeNode))})
			} else {
				typeArgs = append(typeArgs, values.Value{v.Type, v.Value})
			}
		}
		concreteType := iz.cp.ConcreteTypeNow(ty.astType.String())
		concreteTypeInfo := iz.cp.Vm.ConcreteTypeInfo[concreteType]
		if info, ok := iz.P.ParTypes2[name]; ok {
			iz.P.ParTypes2[name] = parser.TypeExpressionInfo{info.VmTypeInfo, concreteTypeInfo.IsClone(), iz.P.ParTypes2[name].PossibleReturnTypes.Union(values.MakeAbstractType(concreteType))}
		} else {
			iz.P.ParTypes2[name] = parser.TypeExpressionInfo{uint32(len(iz.cp.Vm.ParameterizedTypeInfo)), concreteTypeInfo.IsClone(), values.MakeAbstractType(values.ERROR, concreteType)}
			iz.cp.Vm.ParameterizedTypeInfo = append(iz.cp.Vm.ParameterizedTypeInfo, &values.Map{})
		}
		iz.cp.Vm.ParameterizedTypeInfo[iz.P.ParTypes2[name].VmTypeInfo] = iz.cp.Vm.ParameterizedTypeInfo[iz.P.ParTypes2[name].VmTypeInfo].Set(values.Value{values.TUPLE, typeArgs}, values.Value{values.TYPE, values.AbstractType{[]values.ValueType{concreteType}}})
	}
}

func (iz *Initializer) tweakValue(v values.Value) values.Value {
	if v.T == values.TYPE {
		v.V = iz.P.GetAbstractType(v.V.(ast.TypeNode))
	}
	return v
}

// We check that if a struct type is public, so are its fields.
func (iz *Initializer) checkTypesForConsistency() {
	for typeNumber := int(values.FIRST_DEFINED_TYPE); typeNumber < len(iz.cp.Vm.ConcreteTypeInfo); typeNumber++ {
		if !iz.cp.Vm.ConcreteTypeInfo[typeNumber].IsStruct() {
			continue
		}
		if !iz.cp.Vm.ConcreteTypeInfo[typeNumber].IsPrivate() {
			for _, ty := range iz.cp.Vm.ConcreteTypeInfo[typeNumber].(vm.StructType).AbstractStructFields {
				if iz.cp.IsPrivate(ty) {
					iz.throw("init/private/struct", &token.Token{}, iz.cp.Vm.ConcreteTypeInfo[typeNumber], iz.cp.Vm.DescribeAbstractType(ty, vm.LITERAL))
				}
			}
		}
	}
	for _, tc := range iz.tokenizedCode[abstractDeclaration] {
		dec := tc.(*tokenizedAbstractDeclaration)
		if dec.private {
			continue
		}
		abType := iz.P.GetAbstractTypeFromTypeSys(dec.op.Literal)
		for _, w := range abType.Types {
			if iz.cp.Vm.ConcreteTypeInfo[w].IsPrivate() {
				iz.throw("init/private/abstract", ixPtr(dec), dec.op.Literal)
			}
		}
	}
}

// We slurp the functions and converters out of the .so files, if necessary building or rebuilding
// the .so files first.

func (iz *Initializer) compileGoModules() {
	// First of all, the recursion.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.compileGoModules()
	}

	iz.compileGo() // This is in 'gohandler.go' in this package.
}

// We compile the constants, variables, functions, and commands.
func (iz *Initializer) compileEverythingElse() [][]labeledParsedCodeChunk { // TODO --- do we do anything with the return type?
	// First of all, the recursion.
	for _, dependencyIz := range iz.initializers {
		dependencyIz.compileEverythingElse()
	}
	// And now we compile the module.
	//
	// First we need to do a big topological sort on everything, according to the following rules:
	// * A function, variable or constant can't depend on a command.
	// * A constant can't depend on a variable.
	// * A variable or constant can't depend on itself.
	iz.cmI("Mapping variable names to the parsed code chunks in which they occur.")
	iz.cp.GlobalVars.Ext = iz.cp.GlobalConsts
	namesToDeclarations := map[string][]labeledParsedCodeChunk{}
	result := [][]labeledParsedCodeChunk{}
	for dT := constantDeclaration; dT <= variableDeclaration; dT++ {
		for i, pc := range iz.parsedCode[dT] {
			parsedDec := pc.(*parsedAssignment)
			names := iz.P.GetVariablesFromAstSig(parsedDec.sig)
			for _, name := range names {
				existingName, alreadyExists := namesToDeclarations[name]
				if alreadyExists {
					iz.P.Throw("init/name/exists/a", parsedDec.indexTok, existingName[0].chunk.getToken(), name)
					return nil
				}
				namesToDeclarations[name] = []labeledParsedCodeChunk{{parsedDec, dT, i, name, parsedDec.indexTok}}
			}
		}
	}
	iz.cmI("Mapping names of functions to their declarations.")
	for dT := functionDeclaration; dT <= commandDeclaration; dT++ {
		for i, pc := range iz.parsedCode[dT] {
			izFn := pc.(*parsedFunction)
			name := izFn.op.Literal
			_, alreadyExists := namesToDeclarations[name]
			if alreadyExists {
				names := namesToDeclarations[name]
				for _, existingName := range names {
					if existingName.decType == variableDeclaration || existingName.decType == constantDeclaration { // We can't redeclare variables or constants.
						iz.P.Throw("init/name/exists/b", &izFn.op, ixPtr(iz.tokenizedCode[existingName.decType][existingName.decNumber]), name)
					}
					if existingName.decType == functionDeclaration && dT == commandDeclaration { // We don't want to overload anything so it can be both a command and a function 'cos that would be weird.
						iz.P.Throw("init/name/exists/c", &izFn.op, ixPtr(iz.tokenizedCode[existingName.decType][existingName.decNumber]), name)
					}
				}
				namesToDeclarations[name] = append(names, labeledParsedCodeChunk{izFn, dT, i, name, ixPtr(iz.tokenizedCode[dT][i])})
			} else {
				namesToDeclarations[name] = []labeledParsedCodeChunk{{izFn, dT, i, name, ixPtr(iz.tokenizedCode[dT][i])}}
			}
		}
	}
	iz.cmI("Adding struct typechecks to declarations.")
	// Since the name of a type will appear already in the map as the name of the function
	// constructing it, we'll mangle the names by adding a `*` to the front of each.
	for i, pc := range iz.parsedCode[structDeclaration] {
		dec := pc.(*parsedTypecheck)
		if dec.body != nil {
			name := "*" + dec.indexTok.Literal
			namesToDeclarations[name] = []labeledParsedCodeChunk{{dec, structDeclaration, i, dec.indexTok.Literal, dec.indexTok}}
		}
		if iz.errorsExist() {
			return nil
		}
	}
	iz.cmI("Adding clone typechecks to declarations.")
	// Since the name of a type will appear already in the map as the name of the function
	// constructing it, we'll mangle the names by adding a `*` to the front of each.
	for i, pc := range iz.parsedCode[cloneDeclaration] {
		dec := pc.(*parsedTypecheck)
		if dec.body != nil {
			name := "*" + dec.indexTok.Literal
			namesToDeclarations[name] = []labeledParsedCodeChunk{{dec, cloneDeclaration, i, dec.indexTok.Literal, dec.indexTok}}
		}
		if iz.errorsExist() {
			return nil
		}
	}
	i := 0
	for _, v := range iz.parameterizedInstanceMap {
		if v.typeCheck.Length() == 0 {
			continue
		}
		name := "*" + v.astType.String() // Again we mangle the name with a '*' to distinguish is from the constructor.
		v.typeCheck.ToStart()
		iz.P.TokenizedCode = v.typeCheck
		node := iz.P.ParseTokenizedChunk()
		pc := &parsedTypeInstance{
			typeCheck:      node,
			instantiatedAt: v.typeCheck.IndexToken(), // TODO --- no it isn't.
			env:            nil,
		}
		namesToDeclarations[name] = []labeledParsedCodeChunk{{pc, makeDeclaration, i, name[1:], v.typeCheck.IndexToken()}}
		i++
	}

	iz.cmI("Building digraph of dependencies.")
	// We build a digraph of the dependencies between the constant/variable/function/command declarations.
	graph := dtypes.Digraph[string]{}
	for name, decs := range namesToDeclarations { // The same name may be used for different overloaded functions.
		graph.Add(name, []string{})
		for _, dec := range decs {
			rhsNames := iz.extractNamesFromCodeChunk(dec)
			// IMPORTANT NOTE. 'extractNamesFromCodeChunk' will also slurp up a lot of cruft: type names, for example; bling; local true variables in cmds.
			// So we do nothing to throw an error if a name doesn't exist. That will happen when we try to compile the function. What we're trying to
			// do here is establish the relationship between the comds/defs/vars/consts that *do* exist.
			for rhsName := range rhsNames {
				rhsDecs, ok := namesToDeclarations[rhsName]
				if ok { // Again, we don't care if 'ok' is 'false', just about the relationships between the declarations if it's true.
					if dec.decType != commandDeclaration {
						// We check for forbidden relationships.
						for _, rhsDec := range rhsDecs {
							if rhsDec.decType == commandDeclaration {
								iz.P.Throw("init/depend/cmd", dec.chunk.getToken())
								return nil
							}
							if rhsDec.decType == variableDeclaration && dec.decType == constantDeclaration {
								iz.P.Throw("init/depend/var", dec.chunk.getToken())
								return nil
							}
						}
					}
					// And if there are no forbidden relationships we can add the dependency to the graph.
					graph.AddTransitiveArrow(name, rhsName)
				}
			}
		}
	}
	iz.cmI("Initializing service variables.")
	// $We need a few bits and pieeces to assemble the types and content of the variables.
	loggingOptionsType, _ := iz.cp.GetConcreteType("$Logging")
	outputOptionsType, _ := iz.cp.GetConcreteType("$OutputAs")
	outputStructType, _ := iz.cp.GetConcreteType("Output")
	terminalStructType, _ := iz.cp.GetConcreteType("Terminal")
	fileStructType, _ := iz.cp.GetConcreteType("File")
	logToTypes := altType(outputStructType, terminalStructType, fileStructType)
	dir, _ := os.Getwd()
	cliArgs := vector.Empty
	if len(os.Args) >= 2 {
		firstArg := 2
		if os.Args[1] == "run" {
			firstArg = 3
		}
		if len(os.Args) > firstArg {
			for _, v := range os.Args[firstArg:] {
				cliArgs = cliArgs.Conj(val(values.STRING, v))
			}
		}
	}

	serviceVariables := map[string]serviceVariableData{
		"$logging":         {loggingOptionsType, 1, altType(loggingOptionsType)},
		"$logTo":           {terminalStructType, []values.Value{}, logToTypes},
		"$outputAs":        {outputOptionsType, 1, altType(outputOptionsType)},
		"$cliDirectory":    {values.STRING, dir, altType(values.STRING)},
		"$cliArguments":    {values.LIST, cliArgs, altType(values.LIST)},
		"$moduleDirectory": {values.STRING, filepath.Dir(iz.cp.ScriptFilepath), altType(values.STRING)},
		"$hub":             {values.MAP, iz.Common.HubStore, altType(values.MAP)},
	}
	// Service variables which tell the compiler how to compile things must be
	// set before we compile the functions, and so can't be calculated but must
	// be literal.
	compilerDirectives := dtypes.MakeFromSlice([]string{"$logging", "$logTo"})
	// Add variables to environment.
	for svName, svData := range serviceVariables {
		rhs, ok := graph[svName]
		if ok && compilerDirectives.Contains(svName) { // Then we've declared a service variable which is also a compiler directive, and must compile the declaration.
			tok := namesToDeclarations[svName][0].chunk.getToken()
			decType := namesToDeclarations[svName][0].decType
			decNumber := namesToDeclarations[svName][0].decNumber
			if len(rhs) > 0 {
				iz.P.Throw("init/service/depends", tok, svName)
				return nil
			}
			iz.compileGlobalConstantOrVariable(decType, decNumber)
			if !svData.alt.Contains(iz.cp.Vm.Mem[iz.cp.That()].T) {
				iz.P.Throw("init/service/type", tok, svName, compiler.Describe(svData.alt, iz.cp.Vm))
				return nil
			}
			delete(graph, svName)
		} else if !ok { // Then the service variable isn't declared, and we need to stick in a default value.
			dummyTok := token.Token{}
			iz.cp.Reserve(svData.t, svData.v, &dummyTok)
			iz.cp.AddVariable(iz.cp.GlobalVars, svName, compiler.GLOBAL_VARIABLE_PRIVATE, svData.alt, &dummyTok)
		}
		// The third possibility here is that we've declared a service variable which isn't
		// a compiler directive. In that case, it can be compiled in the usual way.
	}
	iz.cp.Vm.UsefulValues.OutputAs = iz.cp.GlobalVars.Data["$outputAs"].MLoc
	iz.cmI("Performing sort on digraph.")
	order := graph.Tarjan()

	// We now have a list of lists of names to declare. We're off to the races!
	iz.cmI("Compiling the variables/functions in the order given by the sort.")
	for _, namesToDeclare := range order { // 'namesToDeclare' is one Tarjan partition.
		groupOfDeclarations := []labeledParsedCodeChunk{}
		for _, nameToDeclare := range namesToDeclare {
			groupOfDeclarations = append(groupOfDeclarations, namesToDeclarations[nameToDeclare]...)
		}
		// If the declaration type is constant or variable it must be the only member of its Tarjan partion and there must only be one thing of that name.
		if groupOfDeclarations[0].decType == constantDeclaration || groupOfDeclarations[0].decType == variableDeclaration {
			iz.compileGlobalConstantOrVariable(groupOfDeclarations[0].decType, groupOfDeclarations[0].decNumber)
			continue
		}
		// So we have a group of functions/commands (but not both) which need to be declared together because either they have the same name or they
		// have a recursive relationship, or both.
		// We can't tell before we compile the group whether there is a recursive relationship in there, because we don't know how the dispatch is going to
		// shake out. E.g. suppose we have a type 'Money = struct(dollars, cents int)' and we wish to implement '+'. We will of course do it using '+' for ints.
		// This will not be recursion, but before we get that far we won't be able to tell whether it is or not.
		iz.cp.RecursionStore = []compiler.BkRecursion{} // The compiler will put all the places it needs to backtrack for recursion here.
		fCount := uint32(len(iz.cp.Fns))                // We can give the function data in the parser the right numbers for the group of functions in the parser before compiling them, since we know what order they come in.
		for _, dec := range groupOfDeclarations {
			if dec.decType == functionDeclaration || dec.decType == commandDeclaration {
				iz.parsedCode[dec.decType][dec.decNumber].(*parsedFunction).callInfo.Number = fCount
				iz.parsedCode[dec.decType][dec.decNumber].(*parsedFunction).callInfo.Compiler = iz.cp
				fCount++
			}
		}
	loop:
		for _, dec := range groupOfDeclarations {
			switch parsedCode := dec.chunk.(type) {
			case *parsedTypecheck:
				tok := parsedCode.getToken()
				if _, ok := iz.getDeclaration(decPARAMETERIZED, tok, DUMMY); ok {
					continue loop
				}
				iz.compileTypecheck(dec.name, parsedCode.body, compiler.NewEnvironment())
				continue
			case *parsedTypeInstance:
				iz.compileTypecheck(dec.name, parsedCode.typeCheck, iz.parameterizedInstanceMap[dec.name].env)
				continue
			case *parsedFunction:
				switch parsedCode.decType {
				case functionDeclaration:
					iz.compileFunction(functionDeclaration, dec.decNumber, iz.cp.GlobalConsts)
				case commandDeclaration:
					iz.compileFunction(commandDeclaration, dec.decNumber, iz.cp.GlobalVars)
				}
			}
		}
		// We've reached the end of the group and can go back and put the recursion in.

		if iz.errorsExist() {
			continue
		}

		for _, rDat := range iz.cp.RecursionStore {
			funcNumber := rDat.FunctionNumber
			addr := rDat.Address
			iz.cp.Vm.Code[addr].Args[0] = iz.cp.Fns[funcNumber].CallTo
			iz.cp.Vm.Code[addr].Args[1] = iz.cp.Fns[funcNumber].LoReg
			iz.cp.Vm.Code[addr].Args[2] = iz.cp.Fns[funcNumber].HiReg
			iz.cp.Vm.Code[addr+2].Args[1] = iz.cp.Fns[funcNumber].OutReg
		}
	}
	iz.cmI("Calling 'init' if it exists.")
	iz.cp.CallIfExists("init")
	return result
}

type serviceVariableData struct {
	t   values.ValueType
	v   any
	alt compiler.AlternateType
}

// Function auxiliary to the above for compiling constant and variable declarations.
func (iz *Initializer) compileGlobalConstantOrVariable(declarations declarationType, v int) {
	// dec := iz.ParsedDeclarations[declarations][v]
	asgn := iz.parsedCode[declarations][v].(*parsedAssignment)
	iz.cp.Cm("Compiling assignment", asgn.indexTok)
	// lhs := dec.(*ast.AssignmentExpression).Left
	rhs := asgn.body
	sig := asgn.sig
	if iz.errorsExist() {
		return
	}
	rollbackTo := iz.cp.GetState() // Unless the assignment generates code, i.e. we're creating a lambda function or a snippet, then we can roll back the declarations afterwards.
	ctxt := compiler.Context{Env: iz.cp.GlobalVars, Access: compiler.INIT, LowMem: DUMMY, TrackingFlavor: compiler.LF_INIT}
	iz.cp.CompileNode(rhs, ctxt)
	if iz.errorsExist() {
		return
	}
	iz.cp.Emit(vm.Ret)
	iz.cp.Cm("Calling Run from initializer's compileGlobalConstantOrVariable method.", asgn.indexTok)
	iz.cp.Vm.Run(uint32(rollbackTo.Code))
	result := iz.cp.Vm.Mem[iz.cp.That()]
	if !iz.cp.Common.CodeGeneratingTypes.Contains(result.T) { // We don't want to roll back the code generated when we make a lambda or a snippet.
		iz.cp.Rollback(rollbackTo, asgn.indexTok)
	}

	envToAddTo, vAcc := iz.getEnvAndAccessForConstOrVarDeclaration(declarations, v)

	last := len(sig) - 1
	t, ok := sig[last].VarType.(*ast.TypeWithName)
	lastIsTuple := ok && t.Name == "tuple"
	rhsIsTuple := result.T == values.TUPLE
	tupleLen := 1
	if rhsIsTuple {
		tupleLen = len(result.V.([]values.Value))
	}
	if !lastIsTuple && tupleLen > len(sig) {
		iz.P.Throw("comp/assign/a", asgn.indexTok, tupleLen, len(sig))
		return
	}
	if !lastIsTuple && tupleLen < len(sig) {
		iz.P.Throw("comp/assign/b", asgn.indexTok, tupleLen, len(sig))
		return
	}
	if lastIsTuple && tupleLen < len(sig)-1 {
		iz.P.Throw("comp/assign/c", asgn.indexTok, tupleLen, len(sig))
		return
	}
	loopTop := len(sig)
	head := []values.Value{result}
	if lastIsTuple {
		loopTop = last
		if rhsIsTuple {
			head = result.V.([]values.Value)[:last]
			iz.cp.Reserve(values.TUPLE, result.V.([]values.Value)[last:], rhs.GetToken())
		} else {
			if tupleLen == len(sig)-1 {
				iz.cp.Reserve(values.TUPLE, []values.Value{}, rhs.GetToken())
			} else {
				iz.cp.Reserve(values.TUPLE, result.V, rhs.GetToken())
			}
		}
		iz.cp.AddVariable(envToAddTo, sig[last].VarName, vAcc, altType(values.TUPLE), rhs.GetToken())
	} else {
		if rhsIsTuple {
			head = result.V.([]values.Value)
		}
	}
	for i := 0; i < loopTop; i++ {
		iz.cp.Reserve(head[i].T, head[i].V, rhs.GetToken())
		if varType, ok := sig[i].VarType.(*ast.TypeWithName); ok && varType.Name == "*inferred*" {
			iz.cp.AddVariable(envToAddTo, sig[i].VarName, vAcc, altType(head[i].T), rhs.GetToken())
		} else {
			allowedTypes := iz.cp.GetAlternateTypeFromTypeAst(sig[i].VarType)
			if allowedTypes.IsNoneOf(head[i].T) {
				iz.P.Throw("comp/assign/type/a", asgn.indexTok, sig[i].VarName, iz.cp.GetTypeNameFromNumber(head[i].T))
				return
			} else {
				iz.cp.AddVariable(envToAddTo, sig[i].VarName, vAcc, allowedTypes, rhs.GetToken())
			}
		}
	}
}

// Function auxiliary to the above.
func (iz *Initializer) getEnvAndAccessForConstOrVarDeclaration(dT declarationType, i int) (*compiler.Environment, compiler.VarAccess) {
	IsPrivate := iz.tokenizedCode[dT][i].(*tokenizedConstOrVarDeclaration).private
	var vAcc compiler.VarAccess
	envToAddTo := iz.cp.GlobalConsts
	if dT == constantDeclaration {
		if IsPrivate {
			vAcc = compiler.GLOBAL_CONSTANT_PRIVATE
		} else {
			vAcc = compiler.GLOBAL_CONSTANT_PUBLIC
		}
	} else {
		envToAddTo = iz.cp.GlobalVars
		if IsPrivate {
			vAcc = compiler.GLOBAL_VARIABLE_PRIVATE
		} else {
			vAcc = compiler.GLOBAL_VARIABLE_PUBLIC
		}
	}
	return envToAddTo, vAcc
}

// Method for compiling the runtime typechecks on structs and clones
func (iz *Initializer) compileTypecheck(name string, node ast.Node, newEnv *compiler.Environment) {
	typeNumber, _ := iz.cp.GetConcreteType(name)
	typeInfo := iz.cp.TypeInfoNow(name)
	thatType := typeNumber
	if typeInfo.IsClone() {
		thatType = typeInfo.(vm.CloneType).Parent
	}
	iz.cmI("Compiling typecheck for '" + name + "'")
	callAddress := iz.cp.CodeTop()
	info := iz.cp.Vm.ConcreteTypeInfo[typeNumber]
	resultLoc := iz.cp.Reserve(values.UNDEFINED_TYPE, nil, node.GetToken())
	tokNumberLoc := iz.cp.Reserve(values.UNDEFINED_TYPE, nil, node.GetToken())
	newEnv.Ext = iz.cp.GlobalConsts
	inLoc := iz.cp.Reserve(values.UNDEFINED_TYPE, nil, node.GetToken())
	iz.cp.AddVariable(newEnv, "that", compiler.VERY_LOCAL_VARIABLE, altType(thatType), node.GetToken())
	chunks := iz.cp.SplitOnNewlines(node)
	for _, chunk := range chunks {
		context := compiler.Context{Env: newEnv}
		rTypes, _ := iz.cp.CompileNode(chunk, context)
		if !rTypes.Contains(values.BOOL) {
			iz.throw("init/typecheck/bool", chunk.GetToken(), iz.cp.P.PrettyPrint(chunk), name)
		}
		errNo := iz.cp.ReserveTypeCheckError(chunk, name, inLoc)
		iz.cp.Emit(vm.Chck, resultLoc, iz.cp.That(), tokNumberLoc, errNo) // This will do its own early return from the typecheck.
	}
	iz.cp.Emit(vm.Ret)
	typeCheck := &vm.TypeCheck{CallAddress: callAddress, InLoc: inLoc, ResultLoc: resultLoc, TokNumberLoc: tokNumberLoc}
	if info.IsClone() {
		info = info.(vm.CloneType).AddTypeCheck(typeCheck)
	} else {
		info = info.(vm.StructType).AddTypeCheck(typeCheck)
	}
	iz.cp.Vm.ConcreteTypeInfo[typeNumber] = info
}

// Method for compiling a top-level function.
func (iz *Initializer) compileFunction(dec declarationType, decNo int, outerEnv *compiler.Environment) *compiler.CpFunc {
	izFn := iz.parsedCode[dec][decNo].(*parsedFunction)
	if info, functionExists := iz.getDeclaration(decFUNCTION, &izFn.op, DUMMY); functionExists {
		iz.cp.Fns = append(iz.cp.Fns, info.(*compiler.CpFunc))
		return info.(*compiler.CpFunc)
	}
	cpFn := compiler.CpFunc{}
	var ac compiler.CpAccess
	if dec == functionDeclaration {
		ac = compiler.DEF
	} else {
		ac = compiler.CMD
		cpFn.Command = true
	}
	cpFn.Private = izFn.private
	functionName := izFn.op.Literal

	iz.cp.Cm("Compiling function '"+functionName+"' with sig "+izFn.sig.String()+".", &izFn.op)
	if iz.errorsExist() {
		return nil
	}
	if izFn.body.GetToken().Type == token.XCALL {
		Xargs := izFn.body.(*ast.PrefixExpression).Args
		cpFn.Xcall = &compiler.XBindle{ExternalServiceOrdinal: uint32(Xargs[0].(*ast.IntegerLiteral).Value),
			FunctionName: Xargs[1].(*ast.StringLiteral).Value, Position: uint32(Xargs[2].(*ast.IntegerLiteral).Value)}
		serializedTypescheme := Xargs[3].(*ast.StringLiteral).Value
		cpFn.RtnTypes = iz.deserializeTypescheme(serializedTypescheme)
	}
	fnenv := compiler.NewEnvironment()
	fnenv.Ext = outerEnv
	cpFn.LoReg = iz.cp.MemTop()
	// First we do the local variables that are in the signature of the struct.
	for _, pair := range izFn.sig {
		iz.cp.Reserve(values.UNDEFINED_TYPE, DUMMY, &izFn.op)
		if ast.IsRef(pair.VarType) {
			iz.cp.AddVariable(fnenv, pair.VarName, compiler.REFERENCE_VARIABLE, iz.cp.Common.AnyTypeScheme, &izFn.op)
			continue
		}
		_, isVarargs := pair.VarType.(*ast.TypeDotDotDot)
		if isVarargs {
			iz.cp.AddVariable(fnenv, pair.VarName, compiler.FUNCTION_ARGUMENT, compiler.AlternateType{compiler.TypedTupleType{iz.cp.GetAlternateTypeFromTypeAst(pair.VarType)}}, &izFn.op)
		} else {
			if !ast.IsAstBling(pair.VarType) {
				iz.cp.AddVariable(fnenv, pair.VarName, compiler.FUNCTION_ARGUMENT, iz.cp.GetAlternateTypeFromTypeAst(pair.VarType), &izFn.op)
			}
		}
	}
	cpFn.HiReg = iz.cp.MemTop()
	cpFn.CallTo = iz.cp.CodeTop()

	// And then we take care of the arguments of a parameterized type.
	vmap := map[string][]uint32{}
	for _, pair := range izFn.sig {
		if twp, ok := pair.VarType.(*ast.TypeWithParameters); ok {
			yeetTo := iz.cp.That() + 1
			// We range over the parameters of the type.
			for _, param := range twp.Parameters {
				// We may already have declared it.
				variable, ok := fnenv.GetVar(param.Name)
				// If we have, we check that it's also a type parameter, and of compatible
				// type. If not, we skip the rest of the loop ...
				if ok {
					if variable.Access != compiler.TYPE_ARGUMENT {
						iz.throw("init/param/var", &izFn.op, param.Name)
						continue
					}
					if !compiler.Equals(variable.Types, iz.cp.GetAlternateTypeFromConcreteTypeName(param.Type)) {
						iz.throw("init/param/var", &izFn.op, param.Name)
						continue
					}
				}
				// We use the 'vmap' to keep track of what type parameters have been declared
				// and when, including duplicates.
				iz.cp.Reserve(values.UNDEFINED_TYPE, DUMMY, &izFn.op)
				if _, ok := vmap[param.Name]; !ok {
					vmap[param.Name] = []uint32{iz.cp.That()}
				} else {
					vmap[param.Name] = append(vmap[param.Name], iz.cp.That())
				}
				iz.cp.AddVariable(fnenv, param.Name, compiler.TYPE_ARGUMENT, iz.cp.GetAlternateTypeFromConcreteTypeName(param.Type), &izFn.op)
			}
			variable, _ := fnenv.GetVar(pair.VarName)
			iz.cp.Emit(vm.Yeet, yeetTo, variable.MLoc)
		}
	}
	paramChecks := []any{} // Though they will in fact all be early returns.
	for k, locs := range vmap {
		if len(locs) > 1 {
			for _, v := range locs[1:] {
				errLoc := iz.cp.ReserveError("vm/type/conflict", &izFn.op, k)
				iz.cp.Put(vm.Eqxx, locs[0], v, errLoc) // TODO: you have specialized versions of this for speed.
				paramChecks = append(paramChecks, iz.cp.VmConditionalEarlyReturn(vm.Qfls, iz.cp.That(), errLoc))
			}
		}
	}

	tupleData := make([]uint32, 0, len(izFn.sig))
	var foundTupleOrVarArgs bool
	for _, param := range izFn.sig {
		switch {
		case ast.IsVarargs(param.VarType):
			tupleData = append(tupleData, 1)
			foundTupleOrVarArgs = true
		case ast.Is(param.VarType, "tuple"):
			tupleData = append(tupleData, 2)
			foundTupleOrVarArgs = true
		default:
			tupleData = append(tupleData, 0)
		}
	}
	if foundTupleOrVarArgs {
		cpFn.LocOfTupleAndVarargData = iz.cp.Reserve(values.INT_ARRAY, tupleData, &izFn.op)
	} else {
		cpFn.LocOfTupleAndVarargData = DUMMY
	}
	switch izFn.body.GetToken().Type {
	case token.BUILTIN:
		name := izFn.body.(*ast.BuiltInExpression).Name
		types, ok := compiler.BUILTINS[name]
		if ok {
			cpFn.RtnTypes = types.T
		} else {
			typeNumber, ok := iz.cp.GetConcreteType(name) // We treat the clone constructors and short struct constructors as builtins. TODO --- todon't.
			if ok {
				cpFn.RtnTypes = altType(typeNumber)
			}
		}
		cpFn.Builtin = name
	case token.GOCODE:
		cpFn.GoNumber = uint32(len(iz.cp.Vm.GoFns))
		cpFn.HasGo = true
		iz.cp.Vm.GoFns = append(iz.cp.Vm.GoFns, vm.GoFn{Code: izFn.body.(*ast.GolangExpression).GoFunction})
	case token.XCALL:
	default:
		areWeTracking := compiler.LF_NONE
		if iz.cp.GetTrackingScope() == 2 {
			areWeTracking = compiler.LF_TRACK
		}
		if izFn.given != nil {
			iz.cp.ThunkList = []compiler.ThunkData{}
			givenContext := compiler.Context{fnenv, functionName, compiler.DEF, false, nil, cpFn.LoReg, areWeTracking, compiler.LF_NONE, altType()}
			iz.cp.CompileGivenBlock(izFn.given, givenContext)
			cpFn.CallTo = iz.cp.CodeTop()
			if len(iz.cp.ThunkList) > 0 {
				iz.cp.Cm("Initializing thunks for outer function.", &izFn.op)
			}
			for _, thunks := range iz.cp.ThunkList {
				iz.cp.Emit(vm.Thnk, thunks.Dest, thunks.Value.MLoc, thunks.Value.CAddr)
			}
		}
		// Logging the function call, if we do it, goes here.
		// 'stringify' is secret sauce, users aren't meant to know it exists. TODO --- conceal it better.

		trackingOn := areWeTracking == compiler.LF_TRACK && (functionName != "stringify")
		log, nodeHasLog := izFn.body.(*ast.LogExpression)
		autoOn := nodeHasLog && log.Token.Type == token.PRELOG && log.Value == ""
		if trackingOn || autoOn {
			iz.cp.TrackOrLog(vm.TR_FNCALL, trackingOn, autoOn, &izFn.op, functionName, izFn.sig, cpFn.LoReg)
		}
		if nodeHasLog && log.Token.Type == token.PRELOG && log.Value != "" {

		}
		bodyContext := compiler.Context{fnenv, functionName, ac, true, iz.cp.ReturnSigToAlternateType(izFn.callInfo.ReturnTypes), cpFn.LoReg, areWeTracking, compiler.LF_NONE, altType()}
		cpFn.RtnTypes, _ = iz.cp.CompileNode(izFn.body, bodyContext) // TODO --- could we in fact do anything useful if we knew it was a constant?
		if len(paramChecks) > 0 {
			cpFn.RtnTypes = cpFn.RtnTypes.Union(altType(values.ERROR))
		}
		cpFn.OutReg = iz.cp.That()

		if izFn.callInfo.ReturnTypes != nil && !(izFn.body.GetToken().Type == token.GOCODE) {
			iz.cp.EmitTypeChecks(cpFn.OutReg, cpFn.RtnTypes, fnenv, izFn.callInfo.ReturnTypes, ac, &izFn.op, compiler.CHECK_RETURN_TYPES, bodyContext)
		}
		iz.cp.VmComeFrom(paramChecks...)
		iz.cp.Emit(vm.Ret)
	}
	iz.cp.Fns = append(iz.cp.Fns, &cpFn)
	if ac == compiler.DEF && !cpFn.RtnTypes.IsLegalDefReturn() {
		iz.P.Throw("comp/return/def", &izFn.op)
	}
	if ac == compiler.CMD && !cpFn.RtnTypes.IsLegalCmdReturn() {
		iz.P.Throw("comp/return/cmd", &izFn.op)
	}
	iz.setDeclaration(decFUNCTION, &izFn.op, DUMMY, &cpFn)

	// We capture the 'stringify' function for use by the VM. TODO --- somewhere else altogether.

	if functionName == "stringify" {
		iz.cp.Vm.StringifyLoReg = cpFn.LoReg
		iz.cp.Vm.StringifyCallTo = cpFn.CallTo
		iz.cp.Vm.StringifyOutReg = cpFn.OutReg
	}

	return &cpFn
}

// We left DUMMY values in the code for where we'd call a function which fits the interface but
// hasn't been defined yet. Now we go back and fill in the gaps.
// TODO --- why are these stored in the common parser bindle and not the common initializer
// bindle?
func (iz *Initializer) resolveInterfaceBacktracks() {
	for _, rDat := range iz.P.Common.InterfaceBacktracks {
		callInfo := rDat.Fn.(*compiler.CallInfo)
		resolvingCompiler := callInfo.Compiler
		CpFunction := resolvingCompiler.Fns[callInfo.Number]
		addr := rDat.Addr
		iz.cp.Vm.Code[addr].Args[0] = CpFunction.CallTo
		iz.cp.Vm.Code[addr].Args[1] = CpFunction.LoReg
		iz.cp.Vm.Code[addr].Args[2] = CpFunction.HiReg
		iz.cp.Vm.Code[addr+1].Args[1] = CpFunction.OutReg
	}
}

// Various miscellaneous types and functions supporting compilation.

// Adds a concrete type to the parser, and to the common types it falls under (at least `any` and `any?`).
func (iz *Initializer) addType(name, supertype string, typeNo values.ValueType) {
	iz.localConcreteTypes = iz.localConcreteTypes.Add(typeNo)
	iz.P.TypeMap[name] = values.MakeAbstractType(typeNo)
	types := []string{supertype}
	iz.cp.Common.AddTypeNumberToSharedAlternateTypes(typeNo, types...)
	types = append(types, "any")
	for _, sT := range types {
		iz.P.Common.Types[sT] = iz.P.Common.Types[sT].Insert(typeNo)
	}
}

// Adds type information to the VM.
//
// For reasons, it's a good idea to have the type info stored as an ordered list rather than a set or hashmap.
// So we need to do insertion by hand to avoid duplication.
func (iz *Initializer) addTypeToVm(typeInfo values.AbstractTypeInfo) {
	for i, existingTypeInfo := range iz.cp.Vm.AbstractTypes {
		if typeInfo.Name == existingTypeInfo.Name {
			if typeInfo.Path == existingTypeInfo.Path {
				return
			}
			if strings.Count(typeInfo.Path, ".") < strings.Count(existingTypeInfo.Path, ".") {
				iz.cp.Vm.AbstractTypes[i] = typeInfo
				return
			}
			if len(typeInfo.Path) < len(existingTypeInfo.Path) {
				iz.cp.Vm.AbstractTypes[i] = typeInfo
				return
			}
		}
	}
	iz.cp.Vm.AbstractTypes = append(iz.cp.Vm.AbstractTypes, typeInfo)
}

func altType(t ...values.ValueType) compiler.AlternateType {
	return compiler.AltType(t...)
}

// For indexing the functions in the common function map, to prevent duplication.
type funcSource struct {
	filename     string
	lineNo       int
	functionName string
	pos          uint32 // Exists to distinguish '-' as a prefix from '-' as an infix when defining clone types
}

// You may wonder why the declarationMap is stored in the initializer and copied from one to the other rather than held
// in the Common initializer and shared. So do I, but we get all sorts of weird bugs if we try. TODO --- investigate.
type decKey struct {
	dOf declarationOf // A struct, a label, a function ...
	src string        // The filepath to the source code.
	lNo int           // Line number of the declaration.
	chS int
	ix  int // If it's an element of an enum, the index of the element in its type.
}

func makeKey(dOf declarationOf, tok *token.Token, ix int) decKey {
	return decKey{dOf: dOf, src: tok.Source, lNo: tok.Line, chS: tok.ChStart, ix: ix}
}

func (iz *Initializer) getDeclaration(dOf declarationOf, tok *token.Token, ix int) (any, bool) {
	result, ok := iz.Common.DeclarationMap[makeKey(dOf, tok, ix)]
	return result, ok
}

func (iz *Initializer) setDeclaration(dOf declarationOf, tok *token.Token, ix int, v any) {
	iz.Common.DeclarationMap[makeKey(dOf, tok, ix)] = v
}

// Tokens to return when no token is available.
var LINKING_TOKEN = &token.Token{Source: "Pipefish linker"}

// This is used to label things of type tokenizedCode and parsedCode, and also to index those
// and other data structures in arrays.
//
// When it's used as an array index we can iterate from e.g. constantDeclatation to
// variableDeclaration in the same way.
//
// For this and other reasons some aspects of the initialization process are dependent on the
// order of the constants, which are therefore not mere labels and should not be re-ordered
// without some care and forethought.
type declarationType int

const ( // Most of these names are self-explanatory.
	importDeclaration    declarationType = iota
	externalDeclaration                  //
	enumDeclaration                      //
	structDeclaration                    //
	abstractDeclaration                  //
	interfaceDeclaration                 //
	cloneDeclaration                     //
	constantDeclaration                  //
	variableDeclaration                  //
	functionDeclaration                  //
	commandDeclaration                   //
	golangDeclaration                    // Pure golang in a block; the Pipefish functions with golang bodies don't go here but under function or command as they were declared.
	makeDeclarations                     // Instantiates parameterized types.
	makeDeclaration                      // We break the makeDeclarations chunks down into this for convenience.
)

type labeledParsedCodeChunk struct {
	chunk     parsedCode
	decType   declarationType
	decNumber int
	name      string
	indexTok  *token.Token
}

// Types and functions to help with housekeeping. The initializer stores the declarations of types and functions
// in a map keyed by their source and line number. This is to prevent the same source code being compiled twice onto
// the same VM, which only needs it once.
type declarationOf int

const (
	decSTRUCT declarationOf = iota
	decLABEL
	decENUM
	decCLONE
	decABSTRACT
	decINTERFACE
	decFUNCTION
	decPARAMETERIZED // A placeholder/
)

type labelInfo struct {
	loc     uint32 // The location in the VM where we store a value {LABEL, n}.
	private bool
}

type structInfo struct {
	structNumber values.ValueType
	private      bool
	sig          ast.AstSig
}

type fnSigInfo struct {
	name   string
	sig    ast.AstSig
	rtnSig ast.AstSig
}

type interfaceInfo struct {
	sigs []fnSigInfo
}

// The maximum value of a `uint32`. Used as a dummy/sentinel value when `0` is not appropriate.
const DUMMY = 4294967295

// Manufactures a value.
func val(T values.ValueType, V any) values.Value {
	return values.Value{T: T, V: V}
}

// Function for commenting on what the initializer is doing. Only mentions the largest steps, hence
// the lack of a Token parameter in its sig. For more detail, turn on the SHOW_COMPILER flag.
func (iz *Initializer) cmI(s string) {
	if settings.SHOW_INITIALIZER {
		if iz.cp != nil && iz.cp.P != nil {
			println(text.UNDERLINE + s + text.RESET + " (" + iz.cp.P.NamespacePath + ")")
		}
	}
}

// Like everything else, the initializer sends its errors to the Common parser bindle via the parser.
func (iz *Initializer) throw(errorID string, tok *token.Token, args ...any) {
	iz.P.Throw(errorID, tok, args...)
}

// Return whether the initializer has encountered errors.
func (iz *Initializer) errorsExist() bool {
	return iz.P.ErrorsExist()
}
