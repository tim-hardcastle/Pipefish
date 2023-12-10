package vm

import (
	"charm/source/initializer"
	"charm/source/token"

	"database/sql"
)

// Just as the initializer directs the tokenizer and the parser in the construction of the parsed code
// chunks from the tokens, so the vmMaker directs the initializer and compiler in the construction of the vm.

type VmMaker struct {
	cp *Compiler
	uP *initializer.Initializer
}

func NewVmMaker(scriptFilepath, sourcecode string, db *sql.DB) *VmMaker {
	uP := initializer.New(scriptFilepath, sourcecode, db)
	vmm := &VmMaker{
		cp: NewCompiler(uP.Parser),
		uP: uP,
	}
	vmm.uP.GetSource(scriptFilepath)
	return vmm
}

func (vmm *VmMaker) Make() {
	vmm.uP.MakeParserAndTokenizedProgram()
	if vmm.uP.ErrorsExist() {
		return
	}

	// vmm.uP.addToNameSpace([]string{"rsc/charm/builtins.ch", "rsc/charm/world.ch"})
	// vmm.uP.ParseImports()
	// if vmm.uP.ErrorsExist() {
	// 	return newService, init
	// }
	// unnamespacedImports := vmm.uP.InitializeNamespacedImportsAndReturnUnnamespacedImports(root, namePath)

	// if vmm.uP.ErrorsExist() {
	// 	return newService, init
	// }
	// vmm.uP.addToNameSpace(unnamespacedImports)

	env := newEnvironment()
	vmm.createEnums(env)
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

// On the one hand, the vm must know the names of the enums and their elements so it can describe them.
// Otoh, the compiler needs to know how to turn enum literals into values.
func (vmm *VmMaker) createEnums(env *environment) {
	for chunk := 0; chunk < len(vmm.uP.Parser.TokenizedDeclarations[enumDeclaration]); chunk++ {
		vmm.uP.Parser.TokenizedDeclarations[enumDeclaration][chunk].ToStart()
		tok1 := vmm.uP.Parser.TokenizedDeclarations[enumDeclaration][chunk].NextToken()
		tok2 := vmm.uP.Parser.TokenizedDeclarations[enumDeclaration][chunk].NextToken()
		if !(tok1.Type == token.IDENT && tok2.Type == token.DEF_ASSIGN) {
			vmm.uP.Throw("init/enum/lhs", tok1)
		}

		vmm.cp.vm.typeNames = append(vmm.cp.vm.typeNames, tok1.Literal)
		vmm.cp.vm.enums = append(vmm.cp.vm.enums, []string{})
		vmm.cp.vm.ub_enums++

		vmm.uP.Parser.TokenizedDeclarations[enumDeclaration][chunk].NextToken() // This says "enum" or we wouldn't be here.
		for tok := vmm.uP.Parser.TokenizedDeclarations[enumDeclaration][chunk].NextToken(); tok.Type != token.EOF; {
			if tok.Type != token.IDENT {
				vmm.uP.Throw("init/enum/ident", tok)
			}
			// TODO --- needs different check now.
			// if env.exists(tok.Literal) {
			// 	vmm.uP.Throw("init/enum/free", tok)
			// }
			vmm.cp.enums[tok.Literal] = enumOrdinates{uint32(chunk) + LB_ENUMS, len(vmm.cp.vm.enums[chunk])}
			vmm.cp.vm.enums[chunk] = append(vmm.cp.vm.enums[chunk], tok.Literal)

			tok = vmm.uP.Parser.TokenizedDeclarations[enumDeclaration][chunk].NextToken()
			if tok.Type != token.COMMA && tok.Type != token.WEAK_COMMA && tok.Type != token.EOF {
				vmm.uP.Throw("init/enum/comma", tok)
			}
			tok = vmm.uP.Parser.TokenizedDeclarations[enumDeclaration][chunk].NextToken()
			vmm.uP.Parser.Suffixes.Add(tok1.Literal)
		}
	}
}
