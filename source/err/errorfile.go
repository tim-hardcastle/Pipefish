package err

import (
	"strings"

	"github.com/tim-hardcastle/Pipefish/source/text"
	"github.com/tim-hardcastle/Pipefish/source/token"
	"github.com/tim-hardcastle/Pipefish/source/values"

	"fmt"
	"strconv"
)

// A map from error identifiers to functions that supply the corresponding error messages and explanations.
//
// Errors in the map are in alphabetical order of their identifers.
//
// Major categories are built, err, init, lex, parse, repl, serve, and vm.
//
// Two otherwise identical errors thrown in different places in the Go code must be assigned
// different identifiers, if only by suffixing /a, /b, etc to the identifier, with the following exception:
// for the purposes of information-hiding, the same error should be shown when either a private or
// a non-existent variable/constant/function is referenced from the REPL, even if these errors are
// generated in slightly different places in the code.

var ErrorCreatorMap = map[string]ErrorCreator{

	// TEMPLATE
	"": {
		Message: func(tok *token.Token, args ...any) string {
			return ""
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return ""
		},
	},

	"comp/assign": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed assignment"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish assumes this is an assignment because it contains an " + emph("=") + " but is having a hard time making sense of it."
		},
	},

	"comp/assign/const": {
		Message: func(tok *token.Token, args ...any) string {
			return "assigning to constant"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "When an identifier has been declared constant in the " + emph("const") + " section of your script, it can't be redefined later: that's what it means to be constant."
		},
	},

	"comp/assign/ident": {
		Message: func(tok *token.Token, args ...any) string {
			return "Pipefish was expecting an identifier, not " + emph(tok.Literal)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The left-hand side of an assignment should contain one or more variable names."
		},
	},

	"comp/assign/private": {
		Message: func(tok *token.Token, args ...any) string {
			return "assigning to private variable"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "When a variable has been declared private, it can be accessed from commands in your script but not directly from the REPL: this is what private means. For more information see " + emph(`hub help "private"`) + "."
		},
	},

	"comp/bling/wut": {
		Message: func(tok *token.Token, args ...any) string {
			return "Unknown bling " + emph(tok.Literal)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This error shouldn't occur."
		},
	},

	"comp/body/known": {
		Message: func(tok *token.Token, args ...any) string {
			return "unknown identifier " + emph(args[0].(string))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You don't seem to have declared that as a variable, function, constant, or anything else."
		},
	},

	"comp/bool/and/left": {
		Message: func(tok *token.Token, args ...any) string {
			return "left-hand side of " + emph("and") + " should be boolean expression."
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Unlike in some languages, the " + emph("or") + " operator is not overloaded, and so can only be applied to boolean values."
		},
	},

	"comp/bool/and/right": {
		Message: func(tok *token.Token, args ...any) string {
			return "right-hand side of " + emph("and") + " should be boolean expression."
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Unlike in some languages, the " + emph("or") + " operator is not overloaded, and so can only be applied to boolean values."
		},
	},

	"comp/bool/cond": {
		Message: func(tok *token.Token, args ...any) string {
			return "left-hand side of conditional should be boolean expression"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Unlike in some languages, Pipefish has no notion of \"truthiness\", and so the left-hand side of a conditional should be a boolean-valued expression."
		},
	},

	"comp/bool/not": {
		Message: func(tok *token.Token, args ...any) string {
			return "trying to apply " + emph("not") + " to something that isn't a boolean expression"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Unlike in some languages, Pipefish has no notion of \"truthiness\", and so the argument of " + emph("not") + " should be a boolean-valued expression."
		},
	},

	"comp/bool/or/left": {
		Message: func(tok *token.Token, args ...any) string {
			return "left-hand side of " + emph("or") + " should be boolean expression."
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Unlike in some languages, the " + emph("or") + " operator is not overloaded, and so can only be applied to boolean values."
		},
	},

	"comp/bool/or/right": {
		Message: func(tok *token.Token, args ...any) string {
			return "right-hand side of " + emph("or") + " should be boolean expression."
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Unlike in some languages, the " + emph("or") + " operator is not overloaded, and so can only be applied to boolean values."
		},
	},

	"comp/call": {
		Message: func(tok *token.Token, args ...any) string {
			return "No implementation of function " + emph(tok.Literal) + " exists for the given types"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You have supplied the function with arguments of types for which no function of that name is defined."
		},
	},

	"comp/command": {
		Message: func(tok *token.Token, args ...any) string {
			return "trying to access a command " + emph(tok.Literal) + " from a function"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Functions can only call other functions, whereas commands can call both functions and commands."
		},
	},

	"comp/error/arg": {
		Message: func(tok *token.Token, args ...any) string {
			return "expression can only return error"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "One of your function arguments can only return an error value, which means that the function itself will only ever return that error value. Pipefish assumes that this is a mistake."
		},
	},

	"comp/error/eq/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "left-hand side of comparison can only be error"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "One of the values you're trying to compare can only ever be an error value, meaning that the comparison itself will just return this error. Pipefish assumes that this is a mistake."
		},
	},

	"comp/error/eq/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "right-hand side of comparison can only be error"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "One of the values you're trying to compare can only ever be an error value, meaning that the comparison itself will just return this error. Pipefish assumes that this is a mistake."
		},
	},

	"comp/error/eq/c": {
		Message: func(tok *token.Token, args ...any) string {
			return "comparison can only return error"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Because of the types of the values you're trying to compare, the result, if compiled, could only ever produce a runtime error. Pipefish assumes that this is a mistake."
		},
	},

	"comp/error/return": {
		Message: func(tok *token.Token, args ...any) string {
			return "function can only return error"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This function is written so that it can never return anything but an error. Pipefish assumes that this is a mistake."
		},
	},

	"comp/eq/types": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't compare values of different types"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish doesn't let you compare values of different types, on the grounds that " +
				"this is more often a mistake than intentional."
		},
	},

	"comp/given/cycle": {
		Message: func(tok *token.Token, args ...any) string {
			cycle := args[0].([]string)
			var description string
			if len(cycle) == 1 {
				description = emph(cycle[0]) + " is defined in terms of itself"
			} else {
				sep := ""
				for _, name := range cycle {
					description = description + sep + emph(name)
					if sep == "" {
						sep = " is defined in terms of "
					} else {
						sep = ", which is defined in terms of "
					}
				}
				description = description + ", which is defined in terms of " + emph(cycle[0])
			}
			return description + " in " + emph("given") + " block"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "It isn't possible to define variables in terms of each other because how would that even work?"
		},
	},

	"comp/log/close": {
		Message: func(tok *token.Token, args ...any) string {
			return "unclosed " + emph("|") + " in logging expression"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish interprets " + emph("| <expression> |") + " in a logging expression as meaning that the expression should be evaluated and inserted into the string. It therefore expects the " + emph("|") + " symbols to come in matching pairs."
		},
	},

	"comp/fcis": {
		Message: func(tok *token.Token, args ...any) string {
			return "imperative code in " + emph("def") + " section"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Some of this carries out imperative operations, other parts try to return values."
		},
	},

	"comp/for/exists/key": {
		Message: func(tok *token.Token, args ...any) string {
			return "reassigning to variable " + emph(args[0])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The index variables of a 'for' loop cannot have already been declared in the scope."
		},
	},

	"comp/for/exists/value": {
		Message: func(tok *token.Token, args ...any) string {
			return "reassigning to variable " + emph(args[0])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The index variables of a 'for' loop cannot have already been declared in the scope."
		},
	},

	"comp/global/global": {
		Message: func(tok *token.Token, args ...any) string {
			return "identifier " + emph(tok.Literal) + " doesn't identify a global variable"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You're trying to bring a variable into the scope of the command by using the " + emph("global") + " keyword when it is not in fact a global variable."
		},
	},

	"comp/global/ident": {
		Message: func(tok *token.Token, args ...any) string {
			return "unexpected occurrence of " + emph(tok.Literal) + " after " + emph("global")
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The only thing Pipefish expects to find after the " + emph("global") + " keyword is the name of a global variable"
		},
	},

	"comp/ident/known": {
		Message: func(tok *token.Token, args ...any) string {
			return "unknown identifier " + emph(tok.Literal)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You don't seem to have declared that as a variable, function, constant, or anything else."
		},
	},

	"comp/ident/private": {
		Message: func(tok *token.Token, args ...any) string {
			return "attempt to access private identifier " + emph(tok.Literal)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Once a value has been declared private, it can only be accessed from within the service that owns it."
		},
	},

	"comp/index/list": {
		Message: func(tok *token.Token, args ...any) string {
			return "list index is not of type" + emph("int") + " or " + emph("pair")
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A list may be indexed by an integer, to select a particular value, or by a pair of integers, to take a slice."
		},
	},

	"comp/index/string": {
		Message: func(tok *token.Token, args ...any) string {
			return "string index is not of type" + emph("int") + " or " + emph("pair")
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A string may be indexed by an integer, to select a particular value, or by a pair of integers, to take a slice."
		},
	},

	"comp/index/pair": {
		Message: func(tok *token.Token, args ...any) string {
			return "pair index is not of type" + emph("int")
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A pair may be indexed  only by the integers " + emph("0") + " and " + emph("1")
		},
	},

	"comp/index/struct/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "label " + emph(args[0]) + " does not label any field of a struct of type " + emph(args[1])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A struct may only be indexed by the labels it was given in its definition."
		},
	},

	"comp/index/struct/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "attempting to index a struct by something other than a label"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A struct may only be indexed by the labels it was given in its definition."
		},
	},

	"comp/index/struct/c": {
		Message: func(tok *token.Token, args ...any) string {
			return "struct cannot be indexed by given label expression"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "If this was compiled, it would always produce an error at runtime because the label would never match the struct type."
		},
	},

	"comp/index/tuple": {
		Message: func(tok *token.Token, args ...any) string {
			return "tuple index is not of type" + emph("int") + " or " + emph("pair")
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A tuple may be indexed by an integer, to select a particular value, or by a pair of integers, to take a slice."
		},
	},

	"comp/index/type": {
		Message: func(tok *token.Token, args ...any) string {
			return "enum index is not of type" + emph("int")
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "An enum type may be indexed  only by the integers " + emph("0") + " and " + emph("1")
		},
	},

	"comp/known/infix": {
		Message: func(tok *token.Token, args ...any) string {
			return "unknown infix " + emph(tok.Literal)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You are using " + emph(tok.Literal) + " as though it was an infix, but you haven't declared it as such in the 'cmd' or 'def' section."
		},
	},

	"comp/known/prefix": {
		Message: func(tok *token.Token, args ...any) string {
			return "unknown prefix " + emph(tok.Literal)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You are using " + emph(tok.Literal) + " as though it was a prefix, but you haven't declared it as such in the 'cmd' or 'def' section."
		},
	},

	"comp/known/suffix": {
		Message: func(tok *token.Token, args ...any) string {
			return "unknown suffix " + emph(tok.Literal)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You are using " + emph(tok.Literal) + " as though it was a suffix, but you haven't declared it as such in the 'cmd' or 'def' section."
		},
	},

	"comp/known/unfix": {
		Message: func(tok *token.Token, args ...any) string {
			return "unknown unfix " + emph(tok.Literal)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You are using " + emph(tok.Literal) + " as though it was an unfix, but you haven't declared it as such in the 'cmd' or 'def' section."
		},
	},

	"comp/list/err": {
		Message: func(tok *token.Token, args ...any) string {
			return "trying to wrap an error in a list"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The expression you are trying to wrap in a list must evaluate to an error, and so trying to wrap it in a list would return an error rather than a list. Pipefish assumes that this is not what you want to do."
		},
	},

	"comp/loop/body": {
		Message: func(tok *token.Token, args ...any) string {
			return "trying to return a value from an imperative loop"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The body of a loop should consist only of imperative instructions. It cannot return any value except an error."
		},
	},

	"comp/loop/infinite": {
		Message: func(tok *token.Token, args ...any) string {
			return "look cannot terminate"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This loop can neither return an error nor encounter a " + emph("break") + " statement, and so can never halt."
		},
	},

	"comp/namespace/exist": {
		Message: func(tok *token.Token, args ...any) string {
			return "unknown namespace " + emph(args[0])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You are using " + emph(args[0]) + " as though it was a namespace, but you haven't declared it as such in the 'import' or 'external' section."
		},
	},

	"comp/namespace/private": {
		Message: func(tok *token.Token, args ...any) string {
			return "trying to use private namespace " + emph(args[0])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "If an import or an external service has been declared private, you can only access it from the code that imports it, not from either the REPL or from code that imports the importing code."
		},
	},

	"comp/pipe/filter/bool": {
		Message: func(tok *token.Token, args ...any) string {
			return "right-hand side of filter expression cannot return boolean"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish expects the right-hand side of a filter operator to be a function returning a boolean, or an boolean-valued expression containing" + emph("that") + "."
		},
	},

	"comp/pipe/mf/ident": {
		Message: func(tok *token.Token, args ...any) string {
			return "unexpected occurrence of " + emph(tok.Literal)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish was expecting an expression that it could pipe the left-hand side of the piping operator into."
		},
	},

	"comp/pipe/mf/func": {
		Message: func(tok *token.Token, args ...any) string {
			return "trying to use " + emph(tok.Literal) + " as a function"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish was expecting an expression that it could pipe the left-hand side of the piping operator into."
		},
	},

	"comp/pipe/pipe/ident": {
		Message: func(tok *token.Token, args ...any) string {
			return "unexpected occurrence of " + emph(tok.Literal)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish was expecting an expression that it could pipe the left-hand side of the piping operator into."
		},
	},

	"comp/pipe/pipe/func": {
		Message: func(tok *token.Token, args ...any) string {
			return "trying to use " + emph(tok.Literal) + " as a function"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish was expecting an expression that it could pipe the left-hand side of the piping operator into."
		},
	},

	"comp/private": {
		Message: func(tok *token.Token, args ...any) string {
			return "trying to access a private function or command " + emph(tok.Literal)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "When a service declares something as private, that means that this is for its own use and cannot be used by a client service or from the REPL."
		},
	},

	"comp/private/enum": {
		Message: func(tok *token.Token, args ...any) string {
			return "trying to access an enum of private type " + emph(args[0])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "When an enumerated type is declared private, so are all its elements."
		},
	},

	"comp/private/label": {
		Message: func(tok *token.Token, args ...any) string {
			return "trying to access an label " + emph(tok.Literal) + " that only belongs to private struct types"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A struct field label is not public unless at least one of the types it belongs to is public."
		},
	},

	"comp/private/type": {
		Message: func(tok *token.Token, args ...any) string {
			return "trying to access private type " + emph(tok.Literal)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A type declared private can only be accessed by the service that owns it."
		},
	},

	"comp/return/cmd": {
		Message: func(tok *token.Token, args ...any) string {
			return "mixture of imperative and functional code"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Despite this being defined as a command, it tries to return values."
		},
	},

	"comp/return/def": {
		Message: func(tok *token.Token, args ...any) string {
			return "mixture of imperative and functional code"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Despite this being defined as a function, it carries out some imperative behavior."
		},
	},

	"comp/return/types": {
		Message: func(tok *token.Token, args ...any) string {
			return "return " + redescribeType(args[0].(string)) + " cannot satisfy specifed return " + redescribeType(args[1].(string))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You have given a return type to your function and at least one branch of this function cannot under any circumstances actually return anything that fits this return type. Pipefish assumes that this is an error."
		},
	},

	"comp/ref/ident": {
		Message: func(tok *token.Token, args ...any) string {
			return "passing non-variable to reference parameter"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The only thing you can ever pass to a reference parameter of a command is the name of a variable."
		},
	},

	"comp/ref/var": {
		Message: func(tok *token.Token, args ...any) string {
			return "trying to create variable " + emph(tok.Literal) + " in REPL"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "It is not possible to create new variables in the REPL."
		},
	},

	"comp/sanity": {
		Message: func(tok *token.Token, args ...any) string {
			return "mixture of imperative and functional code"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Some of this carries out imperative operations, other parts try to return values."
		},
	},

	"comp/snippet/form/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "unmatched " + emph("|") + " in snippet constructor"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish interprets " + emph("| <expression> |") + " in a snippet constructor as meaning that the expression should be evaluated. It therefore expects the " + emph("|") + " symbols to come in matching pairs."
		},
	},

	"comp/snippet/form/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "unmatched " + emph("|") + " in snippet constructor"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish interprets " + emph("| <expression> |") + " in a snippet constructor as meaning that the expression should be evaluated. It therefore expects the " + emph("|") + " symbols to come in matching pairs."
		},
	},

	"comp/snippet/sig": {
		Message: func(tok *token.Token, args ...any) string {
			return "type " + emph(args[0]) + "in struct definition is incompatible with SQL"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "In order for a struct type to be used as the basis for creating a SQL table, all its field types must have corresponding types in SQL."
		},
	},

	"comp/snippet/tuple": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't inject tuple of indeterminate length into SQL"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "To inject a tuple into SQL, the compiler needs to be able to figure out in advance how many elements the tuple will have, and in this case it can't."
		},
	},

	"comp/snippet/type": {
		Message: func(tok *token.Token, args ...any) string {
			return "left hand side of snippet constructor " + emph("---") + " should be snippet type literal"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You declare a snippet of type " + emph("Foo") + " with " + emph("Foo ---") + "."
		},
	},

	"comp/try/return": {
		Message: func(tok *token.Token, args ...any) string {
			return "trying to return value from " + emph("try") + " expression"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A " + emph("try") + " expression is imperative and can return only success or failure"
		},
	},

	"comp/try/var": {
		Message: func(tok *token.Token, args ...any) string {
			return "using global " + emph(tok.Literal) + " in " + emph("try") + "expression"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The variables that capture the error of a " + emph("try") + " expression can only be local variables."
		},
	},

	"comp/tuple/err/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "concatenation must produce error"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You are trying to concatenate together two values one of which is certainly of type " + emph("error") + ". As this will only return the error, Pipefish assumes this is a mistake."
		},
	},

	"comp/tuple/err/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "concatenation must produce error"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You are trying to concatenate together two values one of which is certainly of type " + emph("error") + ". As this will only return the error, Pipefish assumes this is a mistake."
		},
	},

	"comp/types": {
		Message: func(tok *token.Token, args ...any) string {
			// Note that emphasis of args[2] is done at source.
			return "function " + emph(args[0]) + " cannot accept arguments of type " + args[1].(string)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "While this function or command does exist, there is no version of it accepting the types you have supplied."
		},
	},

	"comp/unreachable": {
		Message: func(tok *token.Token, args ...any) string {
			return "unreachable code"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You have written code which can't be reached because one of the previous branches " +
				"must be taken. As there is never any point in doing this, Pipefish assumes that this is a mistake."
		},
	},

	"comp/var/exist": {
		Message: func(tok *token.Token, args ...any) string {
			return "unknown identifier " + emph(args[0])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You don't seem to have declared that as a variable, function, constant, or anything else."
		},
	},

	"comp/var/var": {
		Message: func(tok *token.Token, args ...any) string {
			return "trying to change value of constant " + emph(args[0])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Having declared that value as constant, you are not allowed to change it. If you want to, define it in the " + emph("var") + " section instead."
		},
	},

	"comp/varargs": {
		Message: func(tok *token.Token, args ...any) string {
			return "unexpected occurrence of " + emph("...")
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The " + emph("...") + " token is meaningful only in the signature of a function, command, or assignment, where '<parameterName> <typename>...' indicates " +
				"that the parameter can accept any number of arguments of the given type, e.g. " + emph("foo(numbers int...)") + ". It is meaningless in an ordinary statement or expression."
		},
	},

	"err/misdirect": {
		Message: func(tok *token.Token, args ...any) string {
			return "Pipefish is trying and failing to raise an error with reference " + emph(args[0])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The author of Pipefish, being a silly goose, has managed to throw an error with a code " +
				"that doesn't actually correspond to an error. This should be reported to him as an issue."
		},
	},

	"ext/broken": {
		Message: func(tok *token.Token, args ...any) string {
			return "external service is broken"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish has been able to contact the service but it is currently in a non-functional state."
		},
	},

	"ext/deserialize/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "unable to deserialize message from external service"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This condition should never actually arise. Please contact the author of Pipefish and tell him how it occurred."
		},
	},

	"ext/deserialize/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "unable to deserialize message from external service"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This condition should never actually arise. Please contact the author of Pipefish and tell him how it occurred."
		},
	},

	"ext/deserialize/c": {
		Message: func(tok *token.Token, args ...any) string {
			return "unable to deserialize message from external service"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This condition should never actually arise. Please contact the author of Pipefish and tell him how it occurred."
		},
	},

	"ext/deserialize/d": {
		Message: func(tok *token.Token, args ...any) string {
			return "unable to deserialize message from external service"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This condition should never actually arise. Please contact the author of Pipefish and tell him how it occurred."
		},
	},

	"ext/deserialize/e": {
		Message: func(tok *token.Token, args ...any) string {
			return "unable to deserialize message from external service"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This condition should never actually arise. Please contact the author of Pipefish and tell him how it occurred."
		},
	},

	"ext/deserialize/f": {
		Message: func(tok *token.Token, args ...any) string {
			return "unable to deserialize message from external service"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This condition should never actually arise. Please contact the author of Pipefish and tell him how it occurred."
		},
	},

	"ext/deserialize/g": {
		Message: func(tok *token.Token, args ...any) string {
			return "unable to deserialize message from external service"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This condition should never actually arise. Please contact the author of Pipefish and tell him how it occurred."
		},
	},

	"golang/build": {
		Message: func(tok *token.Token, args ...any) string {
			return "failed to compile Go\n\nError was '" + args[0].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A Pipefish function written in Go has failed to compile at initialization time."
		},
	},

	"golang/conv/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "failed to convert golang values."
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This error should not occur."
		},
	},

	"golang/conv/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "failed to convert golang values."
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This error should not occur."
		},
	},

	"golang/conv/c": {
		Message: func(tok *token.Token, args ...any) string {
			return "failed to convert golang values."
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This error should not occur."
		},
	},

	"golang/open": {
		Message: func(tok *token.Token, args ...any) string {
			return "failed to open Go\n\nError was '" + args[0].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A Pipefish function written in Go has failed to open at initialization time."
		},
	},

	"golang/file": {
		Message: func(tok *token.Token, args ...any) string {
			return "failed to open file " + emph(args[0]) + " with error message " + emph(args[1])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A Pipefish function written in Go has failed to compile at initialization time."
		},
	},

	"golang/found": {
		Message: func(tok *token.Token, args ...any) string {
			return "couldn't find Go function '" + args[0].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish's system for handling functions written in Go has broken down.\n\n" +
				"There are no circumstances under which you should actually see this error: if you ever " +
				"do, please err it to the author of Pipefish as an issue."
		},
	},

	"golang/namespace": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't find namespace " + emph(args[0])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The Golang type converter can't find the specified namespace."
		},
	},

	"golang/type": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't pass value of type " + emph(args[0]) + " to Go as value"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This is because the author of Pipefish hasn't gotten around to it yet."
		},
	},

	"golang/type/c": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't pass value of type " + emph(args[0]) + " to Go"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This is because the author of Pipefish hasn't gotten around to it yet."
		},
	},

	"init/assign/ident": {
		Message: func(tok *token.Token, args ...any) string {
			return "left-hand side of assignment should be identifier"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The left-hand side of an assignment operation names the things to be assigned to."
		},
	},

	"init/close": {
		Message: func(tok *token.Token, args ...any) string {
			return "'(' unclosed by outdent"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Just as an expression of the form '(foo] bar' would be considered a syntax error, " +
				"because it violates nesting rules, so too is something of the form" +
				"\n\n|-----------------------------------------------\n\n" +
				"zort :\n" +
				"    (foo\n" +
				"bar\n\n|-\n\n" + "This violates the nesting rules in just the same way " +
				"as the ']' in '(foo] bar'."
		},
	},

	"init/enum/comma": {
		Message: func(tok *token.Token, args ...any) string {
			return "expected comma, got " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish expects an enum declaration to take the form:" +
				"\n\n|-----------------------------------------------\n\n" +
				"def\n\nMyEnumName = enum FOO, BAR, TROZ\n\n|-\n\n" + "The right hand side of the expression that " +
				"you have supplied is defective because it has a " + text.DescribeTok(tok) + " where " +
				"Pipefish was expecting to find a comma between elements of the enum."
		},
	},

	"init/enum/element": {
		Message: func(tok *token.Token, args ...any) string {
			return "element '" + tok.Literal + "' has already been declared"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The elements of two enums should always be distinct. So for example the following " +
				"will produce an error because 'CLUBS' appears in both enums:" +
				"\n\n|-----------------------------------------------\n\n" +
				"def\n\nSuits = enum CLUBS, HEARTS, SPADES, DIAMONDS\n\n" +
				"MedievalWeaponry = enum CLUBS, SWORDS, MACES\n\n|-\n\n" +
				"And, of course, two elements of the same enum must also always be distinct."

		},
	},

	"init/enum/ident": {
		Message: func(tok *token.Token, args ...any) string {
			return "expected identifier, got " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish expects an enum declaration to take the form:" +
				"\n\n|-----------------------------------------------\n\n" +
				"def\n\nMyEnumName = enum FOO, BAR, TROZ\n\n|-\n\n" + "The right hand side of the expression that " +
				"you have supplied is defective because it has a " + text.DescribeTok(tok) + " where " +
				"Pipefish was expecting one of the elements of the enum."
		},
	},

	"init/external/exist/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "service " + emph(tok.Literal) + " does not exist"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return `If you try to initialize an external service just by giving the name of the service, then this will only work if a service of that name is already running on the hub.`
		},
	},

	"init/external/exist/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "source conflict for external service " + emph(tok.Literal)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return `The service you specified already exists on the hub and keeps its sourcecode in a different file from the one you specified.`
		},
	},

	"init/external/path/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed path to external service"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return `Pipefish expects the path to an http service to be of the form <url>:<port>/<hostname>/<name of external service>.`
		},
	},

	"init/external/path/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed path to external service"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return `Pipefish expects the path to an http service to be of the form <url>:<port>/<hostname>/<name of external service>.`
		},
	},

	"init/external/source": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't get source " + emph(args[0]) + "for external service"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return `Pipefish can't open the specified file.`
		},
	},

	"init/func/body": {
		Message: func(tok *token.Token, args ...any) string {
			return "function definition has no body"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return `This error pretty much speaks for itself. You have gotten as far as writing the signature of a function or command, but have neglected to supply it with a body saying what it actually does.`
		},
	},

	"init/head": {
		Message: func(tok *token.Token, args ...any) string {
			return "unexpected occurrence of " + text.DescribeTok(tok) + " without a headword"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish expects every part of your script to go in a section headed by one of the headwords " +
				"'var', 'def', 'cmd', etc. Instead, you've started your script with something other than " +
				"a headword, and so Pipefish doesn't know which section that thing should belong to." +
				"\n\nFor more information about headwords see 'hub help \"headwords\"'."
		},
	},

	"init/import/file": {
		Message: func(tok *token.Token, args ...any) string {
			return "couldn't open file " + emph(args[0]) + " with OS error " + emph(args[1])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "What it says. Pipefish is trying to import something from the given filepath and failing."
		},
	},

	"init/import/first": {
		Message: func(tok *token.Token, args ...any) string {
			return "if it occurs, 'import' must be the first headword"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish expects every part of your script to go in a section headed by one of the headwords " +
				"'import', 'var', 'def', 'cmd', etc. If your script is going to have an 'import' headword, " +
				"it must be the first headword of the first section, and so it must be the first thing in your " +
				"script apart from comments and whitespace.\n\nYou're seeing this error because you used 'import' " +
				"further down in the script." +
				"\n\nFor more information about the 'import' section see 'hub help \"import\"'."
		},
	},

	"init/import/found": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't find file " + emph(args[0])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "What it says. Pipefish is trying to import something from the given filepath and can't find it."
		},
	},

	"init/import/ident": {
		Message: func(tok *token.Token, args ...any) string {
			return "identifier expected"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Entries in the import section should consist either of filenames or of things of the form <namespace>::<\"filename.\">."
		},
	},

	"init/import/infix": {
		Message: func(tok *token.Token, args ...any) string {
			return "unexpected occurrence of " + text.DescribeTok(tok) +
				" in the 'import' section"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The only thing you should be doing in the 'import' section is specifying " +
				"files to import and, optionally, the namespaces to put them in." +
				"\n\nFor more information about the 'import' section see 'hub help \"import\"'."
		},
	},

	"init/import/string": {
		Message: func(tok *token.Token, args ...any) string {
			return "identifier expected"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Entries in the import or external section should consist either of paths to a service or of things of the form <namespace>::<path>."
		},
	},

	"init/import/weird": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed import or external declaration"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Entries in the import or external section should consist either of paths to a service or of things of the form <namespace>::<path>."
		},
	},

	"init/inexplicable": {
		Message: func(tok *token.Token, args ...any) string {
			return "inexplicable occurrence of " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You've used " + text.DescribeTok(tok) +
				" in a context where Pipefish can't make sense of it. This error is returned when parsing a " +
				"malformed function declaration, though it may also be returned when the surrounding code is so weird " +
				"that Pipefish thinks you're trying to declare a function even though you're not." + blame(errors, pos, "lex/comma") +
				"\n\nFor more information about function declarations, see 'hub help \"functions\"'."
		},
	},

	"init/overload/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "too much overloading: function '" + args[0].(string) + "' defined at@line " + strconv.Itoa(args[1].(int)) + "@conflicts with another version of the same function defined at"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish allows for multiple dispatch, i.e. you could write two functions like this and the result " +
				"would work as intended\n\n" +
				"|-------------------------------------\n\n" +
				"def\n\nadd(a, b string) : a + b\n\nadd(a, b bool) : \n    \"adding booleans is silly\"\n\n\n" +
				"|-\n\nCalled on two strings, add will do one thing, called on two booleans, it will do " +
				"another. However, suppose you wrote two more functions like this: \n\n" +
				"|-------------------------------------\n\n" +
				"def\n\nadd(a any, b int) : a + b\n\nadd(a int, b any) : a + b\n\n\n" +
				"|-\n\nNow, how does the interpreter decide which version of the function it should use " +
				"when 'add' is passed two integers? It can't and doesn't: Pipefish throws this error instead.\n\nSo you're seeing " +
				"this error because you've done something similar with your function/command/operator.\n\nIf this is something you've done deliberately, we would suggest that " +
				"this is probably a bad practise anyway, which will tend to produce unreadable and unmaintainable code, " +
				"and that you should try to do whatever it is you're doing some other way.\n\n" +
				"For more information about overloading, see 'hub help \"overloading\"'; for a more basic introduction to functions " +
				"see 'hub help \"functions\"'."
		},
	},

	"init/overload/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "too much overloading: function '" + args[0].(string) + "' defined at@line " + strconv.Itoa(args[2].(int)) + "@conflicts with another version of the same function defined at"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish allows for multiple dispatch, i.e. you could write two functions like this and the result " +
				"would work as intended\n\n" +
				"|-------------------------------------\n\n" +
				"def\n\nadd(a, b string) : a + b\n\nadd(a, b bool) : \n    \"adding booleans is silly\"\n\n\n" +
				"|-\n\nCalled on two strings, add will do one thing, called on two booleans, it will do " +
				"another. However, suppose you wrote two more functions like this: \n\n" +
				"|-------------------------------------\n\n" +
				"def\n\nadd(a any, b int) : a + b\n\nadd(a int, b any) : a + b\n\n\n" +
				"|-\n\nNow, how does the interpreter decide which version of the function it should use " +
				"when 'add' is passed two integers? It can't and doesn't: Pipefish throws this error instead.\n\nSo you're seeing " +
				"this error because you've done something similar with your function/command/operator.\n\nIf this is something you've done deliberately, we would suggest that " +
				"this is probably a bad practise anyway, which will tend to produce unreadable and unmaintainable code, " +
				"and that you should try to do whatever it is you're doing some other way.\n\n" +
				"For more information about overloading, see 'hub help \"overloading\"'; for a more basic introduction to functions " +
				"see 'hub help \"functions\"'."
		},
	},

	"init/overload/c": {
		Message: func(tok *token.Token, args ...any) string {
			return "too much overloading: function '" + args[0].(string) + "' defined at@line " + strconv.Itoa(args[2].(int)) + "@conflicts with another version of the same function defined at"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish allows for multiple dispatch, i.e. you could write two functions like this and the result " +
				"would work as intended\n\n" +
				"|-------------------------------------\n\n" +
				"def\n\nadd(a, b string) : a + b\n\nadd(a, b bool) : \n    \"adding booleans is silly\"\n\n\n" +
				"|-\n\nCalled on two strings, add will do one thing, called on two booleans, it will do " +
				"another. However, suppose you wrote two more functions like this: \n\n" +
				"|-------------------------------------\n\n" +
				"def\n\nadd(a any, b int) : a + b\n\nadd(a int, b any) : a + b\n\n\n" +
				"|-\n\nNow, how does the interpreter decide which version of the function it should use " +
				"when 'add' is passed two integers? It can't and doesn't: Pipefish throws this error instead.\n\nSo you're seeing " +
				"this error because you've done something similar with your function/command/operator.\n\nIf this is something you've done deliberately, we would suggest that " +
				"this is probably a bad practise anyway, which will tend to produce unreadable and unmaintainable code, " +
				"and that you should try to do whatever it is you're doing some other way.\n\n" +
				"For more information about overloading, see 'hub help \"overloading\"'; for a more basic introduction to functions " +
				"see 'hub help \"functions\"'."
		},
	},

	"init/overload/ref": {
		Message: func(tok *token.Token, args ...any) string {
			return "mismatching reference parameters in overloaded function"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "if you overload a function with reference parameters, then they must have the same number of reference parameters, at the start of the parameter list, or you will see this error."
		},
	},

	"init/private": {
		Message: func(tok *token.Token, args ...any) string {
			return "redeclaration of 'private'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "In blocks of the script where things can be declared private, " +
				"the 'private' modifier can only be used once after each headword: things before the 'private' " +
				"modifier are private, things after it are public.\n\nYou're seeing this error because you used " +
				"the 'private' modifier twice after the same headword." +
				"\n\nFor more information about the 'private' modifier see 'hub help \"private\"'."
		},
	},

	"init/private/struct": {
		Message: func(tok *token.Token, args ...any) string {
			return "public struct type " + emph(tok.Literal) + " cannot contain private type " + emph(args[0])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A public struct cannot contain a field which has a private type."
		},
	},

	"init/request/float": {
		Message: func(tok *token.Token, args ...any) string {
			return "clone of type " + emph("float") + " cannot request operation " + emph(args[0])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A clone of " + emph("int") + " can only request the native operations suitable to that type, i.e. " +
				emph("+") + ", " + emph("-") + ", " + emph("*") + ", and " + emph("/") + "."
		},
	},

	"init/request/int": {
		Message: func(tok *token.Token, args ...any) string {
			return "clone of type " + emph("int") + " cannot request operation " + emph(args[0])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A clone of " + emph("int") + " can only request the native operations suitable to that type, i.e. " +
				emph("+") + ", " + emph("-") + ", " + emph("*") + ", " + emph("/") + ", and " + emph("%") + "."
		},
	},

	"init/private/abstract": {
		Message: func(tok *token.Token, args ...any) string {
			return "public abstract type " + emph(tok.Literal) + " cannot contain private type " + emph(args[0])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A public abstract type cannot contain a private type."
		},
	},

	"init/source/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "unable to get source " + emph(args[0]) + " with error " + emph(args[1])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The initializer can't retrieve the source code for the given file. Check that it exists."
		},
	},

	"init/source/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "unable to get source " + emph(args[0])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The initializer can't retrieve the source code for the given file. Check that it exists."
		},
	},

	"init/source/c": {
		Message: func(tok *token.Token, args ...any) string {
			return "unable to get source " + emph(args[0])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The initializer can't retrieve the source code for the given file. Check that it exists."
		},
	},

	"init/type/exists": {
		Message: func(tok *token.Token, args ...any) string {
			return "type " + emph(tok.Literal) + " already exists"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You can't declare a type more than once.."
		},
	},

	"init/type/form/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed type declaration"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The type should begin with the name of the type being defined, followed by an " + emph("=") + "."
		},
	},

	"init/type/form/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed abstract type declaration"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "An abstract type declaration should consist of types joined by the " + emph("/") + "operator."
		},
	},

	"init/type/form/c": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed abstract type declaration"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "An abstract type declaration should consist of types joined by the " + emph("/") + "operator."
		},
	},

	"init/type/known": {
		Message: func(tok *token.Token, args ...any) string {
			return "unknown type " + emph(tok.Literal)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "An abstract type declaration should consist of types joined by the " + emph("/") + "operator, but you're using something that isn't the name of a type."
		},
	},

	"init/type/null": {
		Message: func(tok *token.Token, args ...any) string {
			return "type name ends in " + emph("?")
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Only an abstract type name can end in " + emph("?") + ", and even then only if the type contains " + emph("NULL") + "."
		},
	},

	"init/unfinished": {
		Message: func(tok *token.Token, args ...any) string {
			return "unfinished business at end of script"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "It looks like you started a function definition but didn't finish it."
		},
	},

	"lex/bin": {
		Message: func(tok *token.Token, args ...any) string {
			return "invalid binary token"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish expects a token begining with '0b' to represent a number in binary notation, so " +
				"that the remaining characters should all be 0s and 1s. You are seeing this error " +
				"because Pipefish was unable to parse the token in this way."
		},
	},

	"lex/comma": {
		Message: func(tok *token.Token, args ...any) string {
			return "a line ending in ',' must be followed by a line beginning with '..'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish allows two forms of continuation, and one is to end a line in a comma " +
				"in positions where a comma would be correct anyway, and to begin the next line with '..'\n\n" +
				"For example: \n\n" +
				"|-----------------------------------------------\n\n" +
				"someList = [1, 2, 4,\n         .. 8, 16, 32]\n\n" +
				"|-\n\nNote that such continuations are not bound by the whitespace rules and can be positioned as you like " +
				"for readability.\n\nThe error you're seeing is because you have ended a line with a ',' indicating " +
				"that you want a continuation, but then haven't followed it up with a '..' at the start of the next line." +
				"\n\nFor more information about continuations, see 'hub help \"continuations\"'."
		},
	},

	"lex/cont/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "a line ending in '..' must be followed by a line beginning with '..'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish allows two forms of continuation, and one is to end a line in a continuation sign " +
				"'..', and to begin the next line with another '..'\n\n" +
				"For example: \n\n" +
				"|-----------------------------------------------\n\n" +
				"someString = \"Hello\" + .. \n          .. \"world!\"\n\n" +
				"|-\n\nNote that such continuations are not bound by the whitespace rules and can be positioned as you like " +
				"for readability.\n\nThe error you are seeing is because you have put an isolated '..' at a line " +
				"boundary, rather than one at the end of one line and another at the start of the next.."
		},
	},

	"lex/cont/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "a line can't begin with a continuation unless it follows a line ending with either a continuation or a comma"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish allows two forms of continuation, One is to end a line in a continuation sign " +
				"'..', and to begin the next line with another '..'\n\n" +
				"For example: \n\n" +
				"|-----------------------------------------------\n\n" +
				"someString = \"Hello\" + .. \n          .. \"world!\"\n\n" + "|-\n\nThe other is to end a line in a comma " +
				"in positions where a comma would be correct anyway, and to begin the next line with '..'\n\n" +
				"For example: \n\n" +
				"|-----------------------------------------------\n\n" +
				"someList = [1, 2, 4,\n         .. 8, 16, 32]\n\n" +
				"|-\n\nNote that such continuations are not bound by the whitespace rules and can be positioned as you like " +
				"for readability.\n\n The error you are seeing is because you have put an isolated '..' at " +
				"the start of a line, rather than one at the end of one line and another at the start of the next."
		},
	},

	"lex/emdash/indent/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "bad indentation in snippet"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The '---' operator, followed by a snippet, obeys the same sort of whitespace rules as a ':' followed by a block of code. Either the snippet is all one line, in which case it may go on the same line as the '---'; or it is a block defined by indentation."
		},
	},

	"lex/emdash/indent/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "bad indentation in snippet"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The '---' operator, followed by a snippet, obeys the same sort of whitespace rules as a ':' followed by a block of code. Either the snippet is all one line, in which case it may go on the same line as the '---'; or it is a block defined by indentation."
		},
	},

	"lex/emdash/indent/c": {
		Message: func(tok *token.Token, args ...any) string {
			return "bad indentation in snippet"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The '---' operator, followed by a snippet, obeys the same sort of whitespace rules as a ':' followed by a block of code. Either the snippet is all one line, in which case it may go on the same line as the '---'; or it is a block defined by indentation."
		},
	},

	"lex/golang": {
		Message: func(tok *token.Token, args ...any) string {
			return "no '{' after 'golang"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish expects the 'golang' token to be followed by '{' to introduce the Go code, to be terminated by a matching '}'."
		},
	},

	"lex/oct": {
		Message: func(tok *token.Token, args ...any) string {
			return "invalid octal token"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish expects a token begining with '0o' to represent a number in octal notation, so " +
				"that the remaining characters should all be digits between 0 and 7. You are seeing this error " +
				"because Pipefish was unable to parse the token in this way."
		},
	},

	"lex/hex": {
		Message: func(tok *token.Token, args ...any) string {
			return "invalid hexadecimal token"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish expects a token begining with '0x' to represent a number in binary notation, so " +
				"that the remaining characters should all be digits or letters (of either case) between A and F inclusive. " +
				"You are seeing this error because Pipefish was unable to parse the token in this way."
		},
	},

	"lex/ill": {
		Message: func(tok *token.Token, args ...any) string {
			return "illegal character"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You have used an illegal character or combination of characters in an identifier."
		},
	},

	"lex/num": {
		Message: func(tok *token.Token, args ...any) string {
			return "invalid number"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish expects a token begining with a digit to represent a number. " +
				"You are seeing this error because Pipefish was unable to parse the token in this way."
		},
	},

	"lex/quote/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "string unterminated by end of line"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Having begun a string literal with an opening quote, you haven't concluded it with a matching " +
				"closing quote before the end of your line of code."
		},
	},

	"lex/quote/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "string unterminated by end of line"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Having begun a string literal with an opening quote, you haven't concluded it with a matching " +
				"closing quote before the end of your line of code."
		},
	},

	"lex/quote/c": {
		Message: func(tok *token.Token, args ...any) string {
			return "string unterminated by end of line"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Having begun a string literal with an opening quote, you haven't concluded it with a matching " +
				"closing quote before the end of your line of code."
		},
	},

	"lex/quote/d": {
		Message: func(tok *token.Token, args ...any) string {
			return "string unterminated by end of line"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Having begun a string literal with an opening quote, you haven't concluded it with a matching " +
				"closing quote before the end of your line of code."
		},
	},

	"lex/wsp": {
		Message: func(tok *token.Token, args ...any) string {
			return "whitespace is inconsistent with previous indentation levels"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "When you outdent your code, it should end up with the same indentation " +
				"as some previous line of code, otherwise Pipefish can't infer what nesting level you're trying to indicate."
		},
	},

	"parse/before/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't put " + text.DescribeTok(tok) + " before " + text.DescribeTok(args[0].(*token.Token))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This error occurs when you put one thing after another where they just make no sense in " +
				"sequence, e.g. '8 8' or 'false \"Wombat\"' or '0.5 ('."
		},
	},

	"parse/before/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't put " + text.DescribeTok(tok) + " before " + text.DescribeTok(args[0].(*token.Token))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This error occurs when you put one thing after another where they just make no sense in " +
				"sequence, e.g. '8 8' or 'false \"Wombat\"' or '0.5 ('."
		},
	},

	"parse/builtin": {
		Message: func(tok *token.Token, args ...any) string {
			return "the 'builtin' keyword should be followed by a string literal"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You shouldn't be seeing this error, nor using the 'builtin' keyword. However, if you are, " +
				"the reason for the error is that 'builtin' is followed by a string literal to say which builtin " +
				"function you mean."
		},
	},

	"parse/colon": {
		Message: func(tok *token.Token, args ...any) string {
			return "'func' declaration should have a colon"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A 'func' declaration looks much like a normal function declaration, and like a normal " +
				"function it requires a ':' between the parameters and the function body."
		},
	},

	"parse/close": {
		Message: func(tok *token.Token, args ...any) string {
			return "Pipefish was expecting an expression before closure by " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This error is produced when an outdent, ')', ']', or '}' occurs when " +
				"an expression is incomplete: for example if you type '(2 + )' into the REPL."
		},
	},

	"parse/eol": {
		Message: func(tok *token.Token, args ...any) string {
			return text.DescribeTok(args[0].(*token.Token)) + text.DescribePos(args[0].(*token.Token)) +
				" unclosed by " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You've reached the end of an expression and the " +
				text.DescribeTok(args[0].(*token.Token)) + text.DescribePos(args[0].(*token.Token)) +
				"hasn't been supplied with a corresponding " + text.DescribeOpposite(args[0].(*token.Token))
		},
	},

	"parse/expected": {
		Message: func(tok *token.Token, args ...any) string {
			return "Pipefish wasn't expecting " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This error occurs when an otherwise well-formed expression " +
				"is followed by some stray bit of code that shouldn't be there, e.g. '(2 + 2) \"wombat\"'." + blame(errors, pos, "lex/comma")
		},
	},

	"parse/float64": {
		Message: func(tok *token.Token, args ...any) string {
			return "Couldn't parse " + emph(tok.Literal) + " as float"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Something about the form of '" + tok.Literal + "' has persuaded Pipefish to try and parse it " +
				"as a floating-point number, and yet it is not a floating-point number so Pipefish has failed."
		},
	},

	"parse/follow": {
		Message: func(tok *token.Token, args ...any) string {
			return "Pipefish expected something to follow " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This error occurs when something which only makes sense as a prefix, such as 'not', is " +
				"then not followed by anything."
		},
	},

	"parse/from": {
		Message: func(tok *token.Token, args ...any) string {
			return "Prefix " + emph("from") + " without " + emph("for")
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "When " + emph("from") + " appears in prefix position, Pipefish expects it to be binding variable to a succeeding " + emph("for") + " loop, and it can't find one in this case."
		},
	},

	"parse/inner/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed inner function declaration: unexpected occurrence of " + emph(tok.Literal)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish thinks you're trying to declare an inner function here but is unable to parse it as such."
		},
	},

	"parse/inner/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed inner function declaration: unexpected occurrence of " + emph(tok.Literal)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish thinks you're trying to declare an inner function here but is unable to parse it as such."
		},
	},

	"parse/int": {
		Message: func(tok *token.Token, args ...any) string {
			return "Couldn't parse '" + tok.Literal + "' as integer"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Something about the form of '" + tok.Literal + "' has persuaded Pipefish to try and parse it " +
				"as an integer, and yet it is not an integer so Pipefish has failed."
		},
	},

	"parse/line": {
		Message: func(tok *token.Token, args ...any) string {
			return "Pipefish expected an expression before the end of the line"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This error is produced when a line stops with something clearly missing, e.g. if you put '2 + '" +
				" into the REPL."
		},
	},

	"parse/malfunc": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed 'func' expression"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This error pretty much speaks for itself: whatever you put after 'func' was in fact just " +
				"too plain weird for Pipefish to interpret as a function definition."
		},
	},

	"parse/match": {
		Message: func(tok *token.Token, args ...any) string {
			return text.DescribeTok(tok) + " doesn't close anything"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The " + text.DescribeTok(tok) + " at " + text.DescribePos(tok) + " doesn't " +
				"correspond to  any " + text.DescribeOpposite(tok) + "that needs closing."
		},
	},

	"parse/missing": {
		Message: func(tok *token.Token, args ...any) string {
			return "Pipefish expected an expression on either side of " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This error is typically returned when a binary operator is missing its left-hand side, e.g. if " +
				"you ask the REPL to evaluate 'and false'."
		},
	},

	"parse/namespace/exist": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't find namespace " + emph(args[0])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You haven't declared that namespace in the " + emph("import") + " or " + emph("external") + " section"
		},
	},

	"parse/namespace/lhs": {
		Message: func(tok *token.Token, args ...any) string {
			return "was expecting namespace, not " + emph(tok.Literal)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish expects to find a namespace to the left of the " + emph(".") + " operator."
		},
	},

	"parse/namespace/rhs": {
		Message: func(tok *token.Token, args ...any) string {
			return "was expecting identifier"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish expects to find something that can live in a namespace to the right of the " + emph(".") + " operator."
		},
	},

	"parse/nesting": {
		Message: func(tok *token.Token, args ...any) string {
			return text.DescribeTok(args[0].(*token.Token)) + text.DescribePos(args[0].(*token.Token)) +
				" unclosed by " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The " + text.DescribeTok(args[0].(*token.Token)) + text.DescribePos(args[0].(*token.Token)) +
				" hasn't been supplied with a corresponding " + text.DescribeOpposite(args[0].(*token.Token)) +
				" by the time the parser reaches the unmatching nesting closure " + text.DescribeTok(tok) +
				text.DescribePos(tok) + "."
		},
	},

	"parse/prefix": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't parse " + text.DescribeTok(tok) + " as a prefix"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You've put " + text.DescribeTok(tok) + " in such a position that it looks like you want it to " +
				"function as a prefix, but it isn't one."
		},
	},

	"parse/raw/form": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed signature"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Something's gone wrong with your syntax around the word 'raw', so that Pipefish isn't sure what you want to do."
		},
	},

	"parse/raw/ident": {
		Message: func(tok *token.Token, args ...any) string {
			return "expected identifier, not " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish was expecting the name of a parameter at that point in the function declaration."
		},
	},

	"parse/raw/type": {
		Message: func(tok *token.Token, args ...any) string {
			return "no such type as " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The 'raw' suffix must be attached either directly to the name of a parameter, or to a Pipefish type."
		},
	},

	"parse/ret/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "unexpected occurrence of " + text.DescribeTok(tok) + " in return types"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish expected your return types to the right of the '->' to be one or more types separated by " +
				"commas, and so it is puzzled to find " + text.DescribeTok(tok) + " instead."
		},
	},

	"parse/ret/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "unexpected occurrence of " + text.DescribeTok(tok) + " in return types"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish expected your return types to the right of the '->' to be one or more types separated by " +
				"commas, and so it is puzzled to find " + text.DescribeTok(tok) + " instead."
		},
	},

	"parse/sig/ident/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "expected identifier, found " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "At this point in the function declaration Pipefish was expecting the name of a parameter."
		},
	},

	"parse/sig/ident/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "expected identifier, found " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "At this point in the function declaration Pipefish was expecting the name of a parameter."
		},
	},

	"parse/sig/ident/c": {
		Message: func(tok *token.Token, args ...any) string {
			return "expected identifier, found " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "At this point in the function declaration Pipefish was expecting the name of a parameter."
		},
	},

	"parse/sig/ident/d": {
		Message: func(tok *token.Token, args ...any) string {
			return "expected identifier, found " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "At this point in the function declaration Pipefish was expecting the name of a parameter."
		},
	},

	"parse/sig/infix": {
		Message: func(tok *token.Token, args ...any) string {
			return "unexpected occurrence of " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish is trying to interpret this as a function or assignment signature, and " +
				"the " + text.DescribeTok(tok) + " doesn't belong in such a context."
		},
	},

	"parse/sig/malformed/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed function declaration"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Charm is trying to interpret this as a function or command declaration, " +
				"but is having trouble working out which bits are the signature and which bits " +
				"are the body."
		},
	},

	"parse/sig/malformed/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed function declaration"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish is trying to interpret this as a function or command declaration, " +
				"but is having trouble working out which bits are the signature and which bits " +
				"are the body."
		},
	},

	"parse/sig/malformed/c": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed function declaration"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish is trying to interpret this as a function or command declaration, " +
				"but is having trouble working out which bits are the signature and which bits " +
				"are the body."
		},
	},

	"parse/sig/malformed/d": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed function declaration"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish is trying to interpret this as a function or command declaration, " +
				"but is having trouble working out which bits are the signature and which bits " +
				"are the body."
		},
	},

	"parse/sig/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "unexpected occurrence of " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish is trying to interpret this as a function or assignment signature, and " +
				"the " + text.DescribeTok(tok) + " doesn't belong in such a context."
		},
	},

	"parse/sig/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "unexpected occurrence of " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish is trying to interpret this as a function or assignment signature, and " +
				"the " + text.DescribeTok(tok) + " doesn't belong in such a context."
		},
	},

	"parse/sig/c": {
		Message: func(tok *token.Token, args ...any) string {
			return "unexpected occurrence of " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish is trying to interpret this as a function or assignment signature, and " +
				"the " + text.DescribeTok(tok) + " doesn't belong in such a context."
		},
	},

	"parse/sig/suffix/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "unexpected occurrence of " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish is trying to interpret this as a function or assignment signature, and " +
				"the " + text.DescribeTok(tok) + " doesn't belong in such a context."
		},
	},

	"parse/sig/suffix/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "unexpected occurrence of " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish is trying to interpret this as a function or assignment signature, and " +
				"the " + text.DescribeTok(tok) + " doesn't belong in such a context."
		},
	},

	"parse/sig/type/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "no such type as " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish thinks that you're trying to specify a type in a function or assignment signature, but it doesn't know the name of the type."
		},
	},

	"parse/sig/type/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "no such type as " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish thinks that you're trying to specify a type in a function or assignment signature, but it doesn't know the name of the type."
		},
	},

	"parse/sig/varchar/int/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "expected integer literal, found " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "At this point in the function declaration Pipefish was expecting an integer literal specifying the size of the 'varchar' type."
		},
	},

	"parse/sig/varchar/int/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "expected integer literal, found " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "At this point in the function declaration Pipefish was expecting an integer literal specifying the size of the 'varchar' type."
		},
	},

	"parse/try/colon": {
		Message: func(tok *token.Token, args ...any) string {
			return "found " + text.DescribeTok(tok) + " in 'try' expression"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "'try' should be followed either by a colon, or by the name of a variable to capture the error, followed by a colon."
		},
	},

	"parse/try/ident": {
		Message: func(tok *token.Token, args ...any) string {
			return "found " + text.DescribeTok(tok) + " in 'try' expression"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "'try' should be followed either by a colon, or by the name of a variable to capture the error, followed by a colon."
		},
	},

	"relex/indent": {
		Message: func(tok *token.Token, args ...any) string {
			return "detatched indent"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The indentation marking the beginning of a code block should follow a line ending in a colon."
		},
	},

	"relex/log": {
		Message: func(tok *token.Token, args ...any) string {
			return "logging following comma or '..'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "While the logging syntax looks a lot like a comment, the rules governing its " +
				"use are slightly more strict: it cannot follow a continuaton as indicated by a comma or '..'."
		},
	},

	"sql/": {
		Message: func(tok *token.Token, args ...any) string {
			return "unable to parse Pipefish in SQL snippet"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Your SQL contains an expression of the form '|...|', which Pipefish is attempting to interpret as being in the Pipefish language. However, the parser isn't able to parse it."
		},
	},

	"sql/conv": {
		Message: func(tok *token.Token, args ...any) string {
			return "unable to perform type conversion"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "At present Pipefish only works with the varchar, int, and bool types. This is because the author has had other things to do."
		},
	},

	"sql/exists": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't find SQL database"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "It seems like you haven't done 'hub config db', and so Pipefish doesn't know which database you want to talk to."
		},
	},

	"sql/in/read": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't read from SQL database; error was \"" + args[0].(string) + "\""
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Hopefully the error returned from SQL makes it clear why this has happened."
		},
	},

	"sql/in/scan": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't scan data from SQL database; error was \"" + args[0].(string) + "\""
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Hopefully the error returned from SQL makes it clear why this has happened."
		},
	},

	"sql/in/type/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't read from database into type" + emph(args[0])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish expects a subtype of 'struct' here that matches the data you're trying to fetch from the database."
		},
	},

	"sql/in/type/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "failed type conversion"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "At present Pipefish only works with the varchar, int, and bool types. This is because the author has had other things to do."
		},
	},

	"sql/out": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't write to SQL database; error was \"" + args[0].(string) + "\""
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Hopefully the error returned from SQL makes it clear why this has happened."
		},
	},

	"sql/sig": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't convert Pipefish struct to SQL table"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Recall that only a limited number of types have been implemented for demonstration purposes."
		},
	},

	"sql/parse/type/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "failed type conversion"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "At present Pipefish only works with the varchar, int, and bool types. This is because the author has had other things to do."
		},
	},

	"sql/parse/type/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "failed type conversion"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "At present Pipefish only works with the varchar, int, and bool types. This is because the author has had other things to do."
		},
	},

	"sql/struct": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't convert type " + text.DescribeTok(tok) + " to SQL table"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Only concrete subtypes of 'struct' can be converted to the signature of a SQL table."
		},
	},

	"vm/div/float": {
		Message: func(tok *token.Token, args ...any) string {
			return "division by zero"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "As it is not possible to divide a number by zero, Pipefish considers this a runtime error."
		},
	},

	"vm/div/int": {
		Message: func(tok *token.Token, args ...any) string {
			return "division by zero"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "As it is not possible to divide a number by zero, Pipefish considers this a runtime error."
		},
	},

	"vm/go/type": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't convert Go value of type " + emph(args[0]) + " to Pipefish"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Not every Go value can or should be converted into Pipefish."
		},
	},

	"vm/pipefish/type": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't convert Pipefish value of type " + emph(args[0]) + " to Go"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Not every Pipefish value can or should be converted into Go."
		},
	},

	"vm/index/list": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("index %v is out of range 0::%v", emph(args[0]), args[1])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A list is indexed over a range from and including 0 up to and excluding the length of the list.")
		},
	},

	"vm/index/pair": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("index %v cannot be used to index something of type %v", emph(args[0]), emph("pair"))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("As a pair contains only two elements, they are indexed by %v and %v.", emph(0), emph(1))
		},
	},

	"vm/index/string": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("index %v is out of range 0::%v", emph(args[0]), args[1])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A string is indexed over a range from and including 0 up to and excluding the length of the string.")
		},
	},

	"vm/index/tuple": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("index %v is out of range 0::%v", emph(args[0]), args[1])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A tuple is indexed over a range from and including 0 up to and excluding the arity of the tuple.")
		},
	},

	"vm/index/type/a": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("can't index type %v", emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("The only types you can index are enum types.")
		},
	},

	"vm/index/type/b": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("index %v out of range for type %v", emph(args[1]), emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("An enum type is indexed over a range from and including 0 up to and excluding the length of the type.")
		},
	},

	"vm/index/a": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("lower bound of slice should be of type %v, not type %v", emph("int"), emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A slice ranges from and including a lower bound which is an integer, up to and excluding an upper bound which must also be an integer.")
		},
	},

	"vm/index/b": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("lower bound of slice should be of type %v, not type %v", emph("int"), emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A slice ranges from and including a lower bound which is an integer, up to and excluding an upper bound which must also be an integer.")
		},
	},

	"vm/index/c": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("lower bound of slice %v is less than %v", emph(args[0]), emph(0))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("Nothing can be indexed by a negative number.")
		},
	},

	"vm/index/d": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("upper bound of slice %v is less than lower bound %v", emph(args[1]), emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A slice ranges from and including a lower bound which is an integer, up to and excluding an upper bound which must also be an integer.")
		},
	},

	"vm/index/e": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("upper bound %v of list slice is strictly greater than list length %v", emph(args[0]), args[1])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("The greatest value the upper bound of a slice can have is the length of the thing being sliced.")
		},
	},

	"vm/index/f": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("upper bound %v of string slice is strictly greater than string length %v", emph(args[0]), args[1])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("The greatest value the upper bound of a slice can have is the length of the thing being sliced.")
		},
	},

	"vm/index/g": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("upper bound %v of tuple slice is strictly greater than the arity %v of the tuple", emph(args[0]), args[1])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("The greatest value the upper bound of a slice can have is the length of the thing being sliced.")
		},
	},

	"vm/index/h": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("key not found in map")
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("It is an error to try and get a value from a map using a key that is not in fact in the map.")
		},
	},

	"vm/index/i": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("can't index value of type %v by value of type %v", emph(args[0]), emph(args[1]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("The %v, %v, %v, and %v types can be indexed by integers; the %v type by its field labels, and a %v by any of its keys. This fits none of those cases.", emph("list"), emph("pair"), emph("string"), emph("tuple"), emph("struct"), emph("map"))
		},
	},

	"vm/index/j": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("index %v is out of range 0::%v", emph(args[0]), args[1])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A list is indexed over a range from and including 0 up to and excluding the length of the list.")
		},
	},

	"vm/index/k": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("index %v cannot be used to index something of type %v", emph(args[0]), emph("pair"))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("As a pair contains only two elements, they are indexed by %v and %v.", emph(0), emph(1))
		},
	},

	"vm/index/l": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("index %v is out of range 0::%v", emph(args[0]), args[1])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A string is indexed over a range from and including 0 up to and excluding the length of the string.")
		},
	},

	"vm/index/m": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("index %v is out of range 0::%v", emph(args[0]), args[1])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A tuple is indexed over a range from and including 0 up to and excluding the arity of the tuple.")
		},
	},

	"vm/index/n": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("can't index type %v", emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("The only types you can index are enum types.")
		},
	},

	"vm/index/o": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("index %v out of range for type %v", emph(args[1]), emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("An enum type is indexed over a range from and including 0 up to and excluding the length of the type.")
		},
	},

	"vm/index/p": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("can't index value of type %v by an integer", emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("Types that can be indexed by integers are %v, %v, %v, %v, and %v if it has integer keys.", emph("list"), emph("pair"), emph("string"), emph("tuple"), emph("map"))
		},
	},

	"vm/label/exist": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("can't convert string %v to a label", emphStr(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("In order to convert a string value to a label, the label must have been defined as the label of a field of a struct.")
		},
	},

	"vm/map/pair": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("can't use value of type %v as a key-value pair", emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A map is constructed from a tuple of key-value pairs.")
		},
	},

	"vm/map/key": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("can't use value of type %v as the key in a key-value pair", emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("To be the key of a key-value pair in a map, a value must have a type which is not a struct.")
		},
	},

	"vm/mod/int": {
		Message: func(tok *token.Token, args ...any) string {
			return "taking the modulus of a number by zero"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "As it is not possible to divide a number by zero, Pipefish considers this a runtime error."
		},
	},

	"vm/pipe/filter/bool": {
		Message: func(tok *token.Token, args ...any) string {
			return "right-hand side of filter expression cannot return boolean"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish expects the right-hand side of a filter operator to be a function returning a boolean, or an boolean-valued expression containing" + emph("that") + "."
		},
	},

	"vm/pipe/mf/func": {
		Message: func(tok *token.Token, args ...any) string {
			return "trying to use " + emph(tok.Literal) + " as a function"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish was expecting an expression that it could pipe the left-hand side of the piping operator into."
		},
	},

	"vm/pipe/pipe/func": {
		Message: func(tok *token.Token, args ...any) string {
			return "trying to use " + emph(tok.Literal) + " as a function"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish was expecting an expression that it could pipe the left-hand side of the piping operator into."
		},
	},

	"vm/set": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("can't put value of type %v in a set.", emphStr(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("To be an element of a set, a value must have a type which is not a struct.")
		},
	},

	"vm/slice/list/a": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("lower bound of slice should be of type %v, not type %v", emph("int"), emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A slice ranges from and including a lower bound which is an integer, up to and excluding an upper bound which must also be an integer.")
		},
	},

	"vm/slice/list/b": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("lower bound of slice should be of type %v, not type %v", emph("int"), emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A slice ranges from and including a lower bound which is an integer, up to and excluding an upper bound which must also be an integer.")
		},
	},

	"vm/slice/list/c": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("lower bound of slice %v is less than %v", emph(args[0]), emph(0))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("Nothing can be indexed by a negative number.")
		},
	},

	"vm/slice/list/d": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("upper bound of slice %v is less than lower bound %v", emph(args[1]), emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A slice ranges from and including a lower bound which is an integer, up to and excluding an upper bound which must also be an integer.")
		},
	},

	"vm/slice/list/e": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("upper bound %v of list slice is strictly greater than list length %v", emph(args[0]), args[1])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("The greatest value the upper bound of a slice can have is the length of the thing being sliced.")
		},
	},

	"vm/slice/string/a": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("lower bound of slice should be of type %v, not type %v", emph("int"), emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A slice ranges from and including a lower bound which is an integer, up to and excluding an upper bound which must also be an integer.")
		},
	},

	"vm/slice/string/b": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("lower bound of slice should be of type %v, not type %v", emph("int"), emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A slice ranges from and including a lower bound which is an integer, up to and excluding an upper bound which must also be an integer.")
		},
	},

	"vm/slice/string/c": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("lower bound of slice %v is less than %v", emph(args[0]), emph(0))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("Nothing can be indexed by a negative number.")
		},
	},

	"vm/slice/string/d": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("upper bound of slice %v is less than lower bound %v", emph(args[1]), emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A slice ranges from and including a lower bound which is an integer, up to and excluding an upper bound which must also be an integer.")
		},
	},

	"vm/slice/string/e": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("upper bound %v of string slice is strictly greater than list length %v", emph(args[0]), args[1])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("The greatest value the upper bound of a slice can have is the length of the thing being sliced.")
		},
	},

	"vm/slice/tuple/a": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("lower bound of slice should be of type %v, not type %v", emph("int"), emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A slice ranges from and including a lower bound which is an integer, up to and excluding an upper bound which must also be an integer.")
		},
	},

	"vm/slice/tuple/b": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("lower bound of slice should be of type %v, not type %v", emph("int"), emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A slice ranges from and including a lower bound which is an integer, up to and excluding an upper bound which must also be an integer.")
		},
	},

	"vm/slice/tuple/c": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("lower bound of slice %v is less than %v", emph(args[0]), emph(0))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("Nothing can be indexed by a negative number.")
		},
	},

	"vm/slice/tuple/d": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("upper bound of slice %v is less than lower bound %v", emph(args[1]), emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A slice ranges from and including a lower bound which is an integer, up to and excluding an upper bound which must also be an integer.")
		},
	},

	"vm/types/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "No implementation of function " + emph(tok.Literal) + " exists for the given types"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You have supplied the function with arguments of types for which no function of that name is defined."
		},
	},

	"vm/types/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "No implementation of function " + emph(tok.Literal) + " exists for the given types"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You have supplied the function with arguments of types for which no function of that name is defined."
		},
	},

	"vm/types/c": {
		Message: func(tok *token.Token, args ...any) string {
			return "No implementation of function " + emph(tok.Literal) + " exists for the given types"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You have supplied the function with arguments of types for which no function of that name is defined."
		},
	},

	"vm/slice/tuple/e": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("upper bound %v of tuple slice is strictly greater than list length %v", emph(args[0]), args[1])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("The greatest value the upper bound of a slice can have is the length of the thing being sliced.")
		},
	},

	"vm/with/a": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("type %v found indexing list in %v expression where %v was expected", emph(args[0]), emph("with"), emph("int"))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A list must be indexed by an integer.")
		},
	},

	"vm/with/b": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("index %v in %v expression out of bounds for list of length %v", emph(args[0]), emph("with"), emph(args[1]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("When you copy-and-mutate a list using a %v expression, the greatest possible value of the index is the length of the list.", emph("with"))
		},
	},

	"vm/with/c": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("trying to index a map with a value of type %v in %v expression", emph(args[0]), emph("with"))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A map can only be indexed by things of type %v, %v, %v, %v, %v, %v or %v", emph("bool"), emph("enum"), emph("float"), emph("int"), emph("label"), emph("rune"), emph("string"))
		},
	},

	"vm/with/d": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("type %v found indexing struct in %v expression where %v was expected", emph(args[0]), emph("with"), emph("label"))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A struct must be indexed by one of the field labels given in its definition.")
		},
	},

	"vm/with/e": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("%v is not a field label of structs of type %v", emph(args[0]), emph(args[1]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A struct is indexed only by its own labels as specified in the type definition.")
		},
	},

	"vm/with/f": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("assignment in %v expression of value of type %v to field %v of struct of type %v, which requires %v", emph("with"), emph(args[0]), emph(args[1]), emph(args[2]), emph(args[3]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("The values that can be put into a struct are constrained by the types in its definition.")
		},
	},

	"vm/with/list/a": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("type %v found in %v expression where %v was expected", emph(args[0]), emph("with"), emph("pair"))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("The right-hand side of a %v expression should be a tuple of key-value pairs.", emph("with"))
		},
	},

	"vm/with/list/b": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("empty list found as key in %v expression", emph("with"))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A %v expression of the form %v is malformed.", emph("with"), emph("x with []::y"))
		},
	},

	"vm/with/map/a": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("type %v found in %v expression where %v was expected", emph(args[0]), emph("with"), emph("pair"))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("The right-hand side of a %v expression should be a tuple of key-value pairs.", emph("with"))
		},
	},

	"vm/with/map/b": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("empty list found as key in %v expression", emph("with"))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A %v expression of the form %v is malformed.", emph("with"), emph("x with []::y"))
		},
	},

	"vm/with/type/a": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("cannot instantiate abstract type %v", emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A long-form constructor must be used on a concrete struct type.")
		},
	},

	"vm/with/type/b": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("cannot instantiate non-struct type %v", emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A long-form constructor must be used on a concrete struct type.")
		},
	},

	"vm/with/type/c": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("type %v found in long-form constructor where %v was expected", emph(args[0]), emph("pair"))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("The right-hand side of a long-form constructor should be a tuple of key-value pairs.")
		},
	},

	"vm/with/type/d": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("left-hand side of key-value pair in struct constructor should be of type %v, not %v", "label", emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A struct is indexed by its labels.")
		},
	},

	"vm/with/type/e": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("%v is not a field label of structs of type %v", emph(args[0]), emph(args[1]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A struct is indexed only by its own labels as specified in the type definition.")
		},
	},

	"vm/with/type/f": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("redefinition of %v in long-form constructor", emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A long-form constructor can specify the value of each field only once.")
		},
	},

	"vm/with/type/g": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("no assignment to field %v in long-form constructor", emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("Unless a field of a struct is nullable, it must be specified in a long-form constructor for the struct.")
		},
	},

	"vm/with/type/h": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("assignment of value of type %v to field %v of struct of type %v, which requires %v", emph(args[0]), emph(args[1]), emph(args[2]), emph(args[3]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("The values that can be put into a struct are constrained by the types in its definition.")
		},
	},

	"vm/with/struct/a": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("type %v found in %v expression where %v was expected", emph(args[0]), emph("with"), emph("pair"))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("The right-hand side of a %v expression should be a tuple of key-value pairs.", emph("with"))
		},
	},

	"vm/with/struct/b": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("empty list found as key in %v expression", emph("with"))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("A %v expression of the form %v is malformed.", emph("with"), emph("x with []::y"))
		},
	},

	"vm/without": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("can't use value of type %v as key of map", emph(args[0]))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return fmt.Sprintf("The key of a map cannot be of a struct type.")
		},
	},

	"vm/varchar": {
		Message: func(tok *token.Token, args ...any) string {
			return "varchar limit exceeded"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The function you are trying to pass the string to has a parameter with a varchar type which restricts how long " +
				"a string you can pass to it, and you have exceeded that limit."
		},
	},
}

func blame(errors Errors, pos int, args ...string) string {
	if pos == 0 {
		return ""
	}
	for _, v := range args {
		if errors[pos-1].ErrorId == v {
			very := ""
			if ((*errors[pos]).Token.Line - errors[pos-1].Token.Line) <= 1 {
				very = "very "
			}
			return "\n\nIn this case the problem is " + very + "likely a knock-on effect of the previous error ([" +
				strconv.Itoa(pos-1) + "] " + errors[pos-1].Message + ".)"
		}
	}
	return ""
}

func emph(s any) string {
	return fmt.Sprintf("'%v'", s)
}

func emphStr(s any) string {
	return fmt.Sprintf("\"%v\"", s)
}

func EmphType(v values.Value) string {
	return "'" + strconv.Itoa(int(v.T)) + "'" // TODO.
}

func DescribeParams(vL []values.Value) string {
	result := ""
	sep := ""
	for _, v := range vL {
		result = result + sep + EmphType(v)
		sep = ", "
	}
	return result
}

func DescribeSomeParams(vL []values.Value, unfinished bool) string {
	result := DescribeParams(vL)
	if unfinished {
		result = result + " ..."
	}
	return result
}

func redescribeType(s string) string {
	result := "type"
	if strings.ContainsAny(s, ",/") {
		result = result + "s"
	}
	result = result + " '" + s + "'"
	return result
}
