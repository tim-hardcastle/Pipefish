package report

import (
	"pipefish/source/text"
	"pipefish/source/token"
	"pipefish/source/values"

	"fmt"
	"strconv"
)

// A map from error identifiers to functions that supply the corresponding error messages and explanations.
//
// Errors in the map are in alphabetical order of their identifers.
//
// Major categories are built, err, eval, init, lex, parse, repl, and serve.
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

	"built/codepoint": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("codepoint applied to string of length %v", args[0].(int))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The builtin 'codepoint' function can only be applied to a string consisting of just one rune."
		},
	},

	"built/div/float64": {
		Message: func(tok *token.Token, args ...any) string {
			return "division by zero"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Because 'x * 0.0 == y * 0.0' for any floating-point numbers 'x' and 'y', mathematicians consider the result of " +
				"dividing by 0.0 to be undefined: there is no right answer — rather, it's the wrong question. So Pipefish throws " +
				"this error when you ask it."
		},
	},

	"built/div/int": {
		Message: func(tok *token.Token, args ...any) string {
			return "division by zero"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Because 'x * 0 == y * 0' for any integers 'x' and 'y', mathematicians consider the result of " +
				"dividing by zero to be undefined: there is no right answer — rather, it's the wrong question. So Pipefish throws " +
				"this error when you ask it."
		},
	},

	"built/hash/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "objects of type " + EmphType(args[0].(values.Value)) + " cannot be used as hashkeys"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "In Pipefish as presently implemented, only some types can be used as hashkeys, including " +
				"<int>, <string>, <float64>, <label> and <bool> — but not " + EmphType(args[0].(values.Value))
		},
	},

	"built/hash/c": {
		Message: func(tok *token.Token, args ...any) string {
			return "objects of type " + EmphType(args[0].(values.Value)) + " cannot be used as hashkeys"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "In Pipefish as presently implemented, only some types can be used as hashkeys, including " +
				"<int>, <string>, <float64>, <label> and <bool> — but not " + EmphType(args[0].(values.Value))
		},
	},

	"built/hash/d": {
		Message: func(tok *token.Token, args ...any) string {
			return "objects of type " + EmphType(args[0].(values.Value)) + " cannot be used as hashkeys"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "In Pipefish as presently implemented, only some types can be used as hashkeys, including " +
				"<int>, <string>, <float64>, <label> and <bool> — but not" + EmphType(args[0].(values.Value))
		},
	},

	"built/hash/pairs/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "a map must be constructed from things of type 'pair', not of type " + EmphType(args[0].(values.Value))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A map in Pipefish consists of '<key>::<value>' pairs, and if you try to make a map out of anything else this will fail."
		},
	},

	"built/hash/pairs/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "a map must be constructed from things of type <pair>, not of type " + EmphType(args[0].(values.Value))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A map in Pipefish consists of '<key>::<value>' pairs, and if you try to make a map out of anything else this will fail."
		},
	},

	"built/int": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't parse string \"" + args[0].(string) + "\" as integer"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "In order to parse a string as an integer, it must actually represent one, and this string doesn't."
		},
	},

	"built/keys/type": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't take the keys of type <" + args[0].(string) + ">"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The only types that have keys are struct types, which have the same keys as the structs that inhabit them."
		},
	},

	"built/list/int": {
		Message: func(tok *token.Token, args ...any) string {
			return "a list can only be indexed by something of type <integer>, not of type " + EmphType(args[0].(values.Value))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A list L can be indexed by an expression of the form L[n], where n is an integer from 0 up to " +
				"but not including the length of the list."
		},
	},

	"built/list/neg": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("index %v is out of bounds", args[0])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Recall that in Pipefish, lists are zero-indexed: a negative number cannot be the index of a list."
		},
	},

	"built/mod": {
		Message: func(tok *token.Token, args ...any) string {
			return "taking the remainder on division by zero"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Because 'x * 0 == y * 0' for any integers 'x' and 'y', mathematicians consider the result of " +
				"dividing by zero to be undefined: there is no right answer — rather, it's the wrong question. So Pipefish throws " +
				"an error when you ask it./n/nIn this particular case you were using the '%%' operator, which returns the remainder " +
				"on division by zero, but in order to figure that out, Pipefish would have to know how to divide by zero, and, as " +
				"just explained, no-one can do that."
		},
	},

	"built/map/hashable": {
		Message: func(tok *token.Token, args ...any) string {
			return "using a value of type '" + args[0].(string) + "' as a key"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Not all types can be used as the keys to a map. These include 'int', 'string', 'float', and 'bool', but not '" + args[0].(string) + "'."
		},
	},

	"built/map/key": {
		Message: func(tok *token.Token, args ...any) string {
			return "object not in keys of map"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "If you try to retrieve a value from a map by indexing the map with a key that isn't in the map " +
				"then this is the error you will see."
		},
	},

	"built/pair/empty/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed pair in 'with' expression"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You have written something along the lines of 'x with []::y', and the empty list makes this expression meaningless."
		},
	},

	"built/pair/empty/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed pair in 'with' expression"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You have written something along the lines of 'x with []::y', and the empty list makes this expression meaningless."
		},
	},

	"built/pair/empty/c": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed pair in 'with' expression"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You have written something along the lines of 'x with []::y', and the empty list makes this expression meaningless."
		},
	},

	"built/range/list/a": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("index %v is out of bounds: list has length %v", args[0].(int), args[1].(int))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Recall that in Pipefish, lists are zero-indexed, which means that the first element has index 0 and the " +
				"last element has index 1 less than the length of the list.\n\nAnything outside of these bounds will cause this " +
				"error if used as an index."
		},
	},

	"built/range/list/b": {
		Message: func(tok *token.Token, args ...any) string {
			return fmt.Sprintf("index %v is out of bounds: list has length %v", args[0].(int), args[1].(int))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Recall that in Pipefish, lists are zero-indexed, which means that the first element has index 0 and the " +
				"last element has index 1 less than the length of the list.\n\nAnything outside of these bounds will cause this " +
				"error if used as an index."
		},
	},

	"built/slice/int/range": {
		Message: func(tok *token.Token, args ...any) string {
			return "ranges are defined by pairs of type <int>::<int>, not of type " +
				EmphType(args[0].(values.Value)) + "::" + EmphType(args[1].(values.Value))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A range is a list of numbers given from one integer up to (but not including) another, " +
				"and so the 'range' function takes pairs of the form  <int>::<int> as input."
		},
	},

	"built/struct/field/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "value doesn't label a field of structs of type <" + args[1].(string) + ">"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "When you create a struct, the names you give to the parameters in the constructor are automatically " +
				"converted to a set of labels for the fields of the struct. These are the only things you can index the struct by.\n\n" +
				"While you are using a label as an index, it's not a label of this particular type of struct."
		},
	},

	"built/struct/field/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "value doesn't label a field of structs of type '" + args[1].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "When you create a struct, the names you give to the parameters in the constructor are automatically " +
				"converted to a set of labels for the fields of the struct. These are the only things you can index the struct by.\n\n" +
				"While you are using a label as an index, it's not a label of this particular type of struct."
		},
	},

	"built/struct/pair": {
		Message: func(tok *token.Token, args ...any) string {
			return "Pipefish was expecting a pair, not something of type " + text.EmphType(args[0].(string))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "It looks like you're trying to use `with` to return an updated copy of a struct, but you've " +
				"gotten the syntax wrong."
		},
	},

	"comp/assign/ident": {
		Message: func(tok *token.Token, args ...any) string {
			return "Pipefish was expecting an identifier, not " + text.Emph(tok.Literal)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The left-hand side of an assignment should contain one or more variable names."
		},
	},

	"comp/bling/wut": {
		Message: func(tok *token.Token, args ...any) string {
			return "Unknown bling " + text.Emph(tok.Literal)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This error shouldn't occur."
		},
	},

	"comp/call": {
		Message: func(tok *token.Token, args ...any) string {
			return "No implementation of function " + text.Emph(tok.Literal) + " exists for the given types"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You have supplied the function with arguments of types for which no function of that name is defined."
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

	"comp/ident/known": {
		Message: func(tok *token.Token, args ...any) string {
			return "unknown identifier " + text.Emph(tok.Literal)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You don't seem to have declared that as a variable, function, constant, or anything else."
		},
	},

	"comp/namespace/exist": {
		Message: func(tok *token.Token, args ...any) string {
			return "unknown namespace " + text.Emph(tok.Literal)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You are using " + text.Emph(tok.Literal) + " as though it was a namespace, but you haven't declared it as such in the 'import' section."
		},
	},

	"comp/not/bool": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't apply 'not' to non-boolean type"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Charm has no concept of \"truthiness\", so the 'not' operator can only be applied " +
				"to one of the two values 'true' and 'false' in the type 'bool'."
		},
	},

	"comp/prefix/known": {
		Message: func(tok *token.Token, args ...any) string {
			return "unknown prefix " + text.Emph(tok.Literal)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You don't seem to have declared that as a function, but you'ree trying to use it as one."
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

	"err/misdirect": {
		Message: func(tok *token.Token, args ...any) string {
			return "Pipefish is trying and failing to raise an error with reference '" + args[0].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The author of Pipefish, being a silly goose, has managed to throw an error with a code " +
				"that doesn't actually correspond to an error. This should be reported to him as an issue."
		},
	},

	"eval/apply": {
		Message: func(tok *token.Token, args ...any) string {
			return "Can't apply " + EmphType(args[0].(values.Value)) + " as a function"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "In this line you have written something of the form '(<expression 1>) (<expression 2>)'. " +
				"The only way Pipefish can evaluate something like that is if expression 1 evaluates to something of " +
				" type <func>, in which case this will be applied to expression 2. However, in this case the " +
				"first expression evaluated to something of type " + EmphType(args[0].(values.Value)) + "."
		},
	},

	"eval/args/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't find implementation of function " + text.DescribeTok(tok) + " accepting parameters of the given types " +
				DescribeSomeParams(args[0].([]values.Value), args[1].(bool))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "While the function or operator you have used is defined, it is not defined for the " +
				"data type or types to which you're trying to apply it."
		},
	},

	"eval/args/b": {
		Message: func(tok *token.Token, args ...any) string {
			if len(args[0].([]values.Value)) == 0 {
				return "can't find implementation of function " + text.DescribeTok(tok) + " requiring no parameters"
			}
			return "can't find implementation of function " + text.DescribeTok(tok) + " accepting parameters of the given types " +
				DescribeSomeParams(args[0].([]values.Value), args[1].(bool))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "While the function or operator you have used is defined, it takes parameters, you can't just " +
				"say " + text.DescribeTok(tok) + " on its own and expect it to be meaningful."
		},
	},

	"eval/args/c": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't find implementation of function " + text.DescribeTok(tok) + " accepting parameters of the given types " +
				DescribeSomeParams(args[0].([]values.Value), args[1].(bool))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "While the function or operator you have used is defined, it is not defined for the " +
				"data type or types to which you're trying to apply it."
		},
	},

	"eval/bool/iflog": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't apply " + emph(":") + " to things of type " + EmphType(args[0].(values.Value))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish has no concept of \"truthiness\": the only valid left-hand side of the " +
				emph(":") + " operator is a boolean value."
		},
	},

	"eval/bool/left": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't apply " + text.DescribeTok(tok) + " to things of type " + EmphType(args[0].(values.Value))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish has no concept of \"truthiness\": the only valid left-hand side of the " +
				text.DescribeTok(tok) + " operator is a boolean value."
		},
	},

	"eval/bool/not": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't apply " + text.DescribeTok(tok) + " to things of type " + EmphType(args[0].(values.Value))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish has no concept of \"truthiness\": the only valid argument of the " +
				text.DescribeTok(tok) + " operator is a boolean value."
		},
	},

	"eval/bool/right": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't apply " + text.DescribeTok(tok) + " to things of type " + EmphType(args[0].(values.Value))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish has no concept of \"truthiness\": the only valid right-hand side of the " +
				text.DescribeTok(tok) + " operator is a boolean value."
		},
	},

	"eval/break": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't 'break' outside of a command"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The 'break' keyword breaks out of a loop started by 'loop'. As such loops are only permitted inside the 'cmd' section, 'break' is meaningless outside of it."
		},
	},

	"eval/cmd/assign": {
		Message: func(tok *token.Token, args ...any) string {
			return "Variables cannot be assigned types in the 'cmd' section"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A variable can only be given a type when it is created in the 'var' section of " +
				"the script. In the 'cmd' section you can reassign its value but not its type."
		},
	},

	"eval/cmd/const": {
		Message: func(tok *token.Token, args ...any) string {
			return "reassigning constant '" + args[0].(string) + "' in the 'var' section"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Because you have defined '" + args[0].(string) + "' as a constant in the 'def' " +
				"section of the script, you can't also declare it as a variable in the 'def' section." +
				"\n\nFor more information about constants see 'hub help \"constants\"'."
		},
	},

	"eval/cmd/function": {
		Message: func(tok *token.Token, args ...any) string {
			return "calling a command from a function"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "It is one of the central tenets of Pipefish that a command can call functions but " +
				"functions can't call commands. You've tried to break that rule."
		},
	},

	"eval/cmd/global/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "global variable out of scope"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A global variable needs to be brought into the scope of a command with 'global <variable name>' before you can assign to it."
		},
	},

	"eval/cmd/global/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "global variable out of scope"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A global variable needs to be brought into the scope of a command with 'global <variable name>' before you can access its contents."
		},
	},

	"eval/cmd/global/c": {
		Message: func(tok *token.Token, args ...any) string {
			return "global variable out of scope"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A global variable needs to be brought into the scope of a command with 'global <variable name>' before you can access its contents."
		},
	},

	"eval/const/assign": {
		Message: func(tok *token.Token, args ...any) string {
			return "reassigning to a constant in the 'def' section."
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The value of a constant can only be declared once." +
				"\n\nFor more information about constants see 'hub help \"constants\"'."
		},
	},

	"eval/const/type": {
		Message: func(tok *token.Token, args ...any) string {
			return "attempting to assign object of type " + EmphType(args[0].(values.Value)) + " to constant of type '" +
				args[1].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Once you've declared a constant as being of a given type, the only things you can assign " +
				"to it are objects of that type or of one of its subtypes." +
				"\n\nFor more information about constants see 'hub help \"constants\"'."
		},
	},

	"eval/external/": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't evaluate " + emph(args[0].(string))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You have used '|...|' to embed a piece of Pipefish into a call to a external service, " +
				"this snippet being evaluated by the calling service. In this case, however, the calling service was unable " +
				"even to parse the code: it is syntactically incorrect."
		},
	},

	"eval/external/response": {
		Message: func(tok *token.Token, args ...any) string {
			return "called service didn't post to output"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "When you use 'get' to get a value from a external service, the external service must 'post' something to 'Output()' in response."
		},
	},

	"eval/external/return": {
		Message: func(tok *token.Token, args ...any) string {
			return "called service didn't post to output"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "When you use 'post', 'put', or 'delete' to get a value from a external service, the external service mustn't 'post' anything to 'Output()' in response, " +
				"but only emit a Successful values.Value (the little green 'ok')."
		},
	},

	"eval/external/service": {
		Message: func(tok *token.Token, args ...any) string {
			return "service " + emph(args[0].(string)) + "doesn't exist"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Either you forgot to start it up, or it shut itself down."
		},
	},

	"eval/enum/len": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't apply 'len' to type '" + args[0].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "An enum type has a length, i.e. the number of elements in the type. But you're trying to " +
				"apply the 'len' function to the type '" + args[0].(string) + "', and Pipefish doesn't know what you mean by this."
		},
	},

	"eval/equal/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "comparing values of type " + emph(args[0].(string)) + " and " + emph(args[1].(string))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You're using the equality operator '==' to compare to values of different concrete types. But Pipefish considers this a type error."
		},
	},

	"eval/equal/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "comparing values of type " + emph(args[0].(string)) + " and " + emph(args[1].(string))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You're using the inequality operator '!=' to compare to values of different concrete types. But Pipefish considers this a type error."
		},
	},

	"eval/eval": {
		Message: func(tok *token.Token, args ...any) string {
			return "'eval' takes a string or code object as a parameter"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "'eval' takes a string or code object as a parameter and returns the result of " +
				"evaluating that string or code object as an " +
				"expression in Pipefish: e.g. 'eval \"2 + 2\"' would return '4'."
		},
	},

	"eval/filter/list": {
		Message: func(tok *token.Token, args ...any) string {
			return "'?>' operates on a list, not an object of type " + EmphType(args[0].(values.Value))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "the filter operator '?>' takes a list as its left-hand parameter, and" +
				"on the right, and expression to filter by."
		},
	},

	"eval/field/struct": {
		Message: func(tok *token.Token, args ...any) string {
			return "'" + args[0].(string) + " does not name a field of '" + args[1].(string) + "'."
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You will see this error when you try to index a struct by a label which exists " +
				"but doesn't label a field of that particular type of struct." +
				"\n\nFor more information about structs see 'hub help \"structs\"'."
		},
	},

	"eval/field/type": {
		Message: func(tok *token.Token, args ...any) string {
			return "field '" + args[0].(string) + "' of variable '" + args[1].(string) +
				"' should have type <" + args[2].(string) + ">, not " + EmphType(args[3].(values.Value))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You will see this error when you try to assign a value to a field of a struct " +
				"and the value is not compatible with the type of the field in the struct declaration." +
				"\n\nFor more information about structs see 'hub help \"structs\"'."
		},
	},

	"eval/filter/bool/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "filter operator requires a boolean value"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "When you have something of the form 'aList ?> function(that)' then the function " +
				"must return a boolean value on which the list elements can be filtered."
		},
	},

	"eval/filter/bool/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "filter operator requires a boolean value"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "When you have something of the form 'aList ?> function(that)' then the function " +
				"must return a boolean value on which the list elements can be filtered."
		},
	},

	"eval/for/arg": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed range of 'for' expression"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish was expecting the range of the 'for' expression to be something one can iterate over: a list, an enum, a pair of integers 'a::b' representing a range, or a single integer representing the upper bound of a range bounded below by zero."
		},
	},

	"eval/for/pair": {
		Message: func(tok *token.Token, args ...any) string {
			return "incorrectly typed pair in range of 'for'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "One way in which you can specify the range of a 'for' expression is using a pair 'a::b', which you have done; but it must be a pair of integers, and it isn't."
		},
	},

	"eval/for/type": {
		Message: func(tok *token.Token, args ...any) string {
			return "unexpected type in range of 'for'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish was expecting the range of the 'for' expression to be something one can iterate over: a list, an enum, a pair of integers 'a::b' representing a range, or a single integer representing the upper bound of a range bounded below by zero."
		},
	},

	"eval/given/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "inexplicable use of 'given'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A 'given' block should follow a command, function, or inner function. Either " +
				"this doesn't follow one at all, or it's so malformed that Pipefish doesn't recognize it as one."
		},
	},

	"eval/given/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "inexplicable use of 'given'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A 'given' block should follow a command, function, or inner function. Either " +
				"this doesn't follow one at all, or it's so malformed that Pipefish doesn't recognize it as one."
		},
	},

	"eval/given/return": {
		Message: func(tok *token.Token, args ...any) string {
			return "attempt to return value in 'given' block"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The only thing you're allowed to do in the 'given' block of a function is assign values " +
				"to local variables."
		},
	},

	"eval/global/exists": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't find global " + emph(args[0].(string))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You have tried to use the 'global' keyword to import a global variable into the scope of a command, but no global variable of that name exists."
		},
	},

	"eval/global/ident": {
		Message: func(tok *token.Token, args ...any) string {
			return "not an identifier"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The only thing the 'global' keyword can take as its argument is an identifier, the name of the global variable to be brought into scope."
		},
	},

	"eval/golang": {
		Message: func(tok *token.Token, args ...any) string {
			return args[0].(string)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This is a runtime error returned by a function written in Go."
		},
	},

	"eval/ident/context": {
		Message: func(tok *token.Token, args ...any) string {
			return "Pipefish can't make any sense of '" + tok.Literal + "' in that context"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You're tring to treat '" + tok.Literal + "' as though it was the name " +
				"of a variable, but it isn't."
		},
	},

	"eval/ident/found": {
		Message: func(tok *token.Token, args ...any) string {
			return "identifier not found '" + tok.Literal + "'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You haven't defined anything with the name '" + tok.Literal + "'."
		},
	},

	"eval/index/type": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't index type " + emph(args[0].(string))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Only enum types can be indexed. (Of course, *members* of other types can be indexed: lists, maps, etc. But the types themselves cannot be: 'list[4]' is meaningless.)"
		},
	},

	"eval/index/types": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't index " + EmphType(args[0].(values.Value)) + " by " + EmphType(args[1].(values.Value))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You can index a list or a string or a tuple by an integer, or by a pair of integers to make a slice; or you can index a map by a key, or a struct by the label of one of its fields. But you have done none of these things and Pipefish is puzzled."
		},
	},

	"eval/label": {
		Message: func(tok *token.Token, args ...any) string {
			return "not the label of a field"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You're using a long-form constructor to create a new instance of a struct, " +
				"and have given it a parameter which is not of the form '<field name>::<value>'."
		},
	},

	"eval/log/append": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't append to log file"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This is a file-handling error: the logging statement is trying to write to a file and failing."
		},
	},

	"eval/log/file": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't open log file"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This is a file-handling error: the logging statement is trying to write to a file and failing even to open it."
		},
	},

	"eval/loop/value": {
		Message: func(tok *token.Token, args ...any) string {
			return "loop returning value"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The 'loop' construct in Pipefish is pureply imperative: it iterates over instructions; it cannot return a value. If you want to do that sort of thing, write a function."
		},
	},

	"eval/malret": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed 'respond' expression"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The 'respond' statement you used makes no sense in the place where " +
				"you used it."
		},
	},

	"eval/map/hashable": {
		Message: func(tok *token.Token, args ...any) string {
			return "using a value of type '" + args[0].(string) + "' as a key"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Not all types can be used as the keys to a map. These include 'int', 'string', 'float', and 'bool', but not '" + args[0].(string) + "'."
		},
	},

	"eval/map/key": {
		Message: func(tok *token.Token, args ...any) string {
			return "object not in keys of map"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "If you try to retrieve a value from a map by indexing the map with a key that isn't in the map " +
				"then this is the error you will see."
		},
	},

	"eval/mapping/list": {
		Message: func(tok *token.Token, args ...any) string {
			return "'>>' expects a list on the left-hand side"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The piping operator '>>' transforms a list, on the left, into another list, according to a rule specified on the right."
		},
	},

	"eval/namespace/args": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed namespace"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The namespace operator '.' shouldn't take multiple values on either side."
		},
	},

	"eval/namespace/known/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "unknown namespace " + emph(args[0].(string))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish is unable to identify a namespace with the given name."
		},
	},

	"eval/namespace/known/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "unknown namespace " + emph(args[0].(string))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish is unable to identify a namespace with the given name."
		},
	},

	"eval/namespace/ident": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed namespace"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A namespace should be an identifier."
		},
	},

	"eval/oops": {
		Message: func(tok *token.Token, args ...any) string {
			return "something too weird has happened for Pipefish to supply a meaningful error"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You shouldn't be seeing this error, nor should Pipefish be emitting it. If " +
				"you are seeing it, it means that some erroneous nil value has snuck through all " +
				"the other checks for nil values, and returning this error is Pipefish's last line of " +
				"defense against actually crashing.\n\nHence if you see this error, it should be considered " +
				"an issue, and you should notify the author of Pipefish, who will try to fix it."
		},
	},

	"eval/pair": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed constructor"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You're trying to construct an instance of a struct, but your syntax has gotten Pipefish so " +
				"confused that it's not sure what you mean to do."
		},
	},

	"eval/pair/int/left": {
		Message: func(tok *token.Token, args ...any) string {
			return "unexpected type in slice expression"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The pair 'a::b' in a slice expression should be two integers."
		},
	},

	"eval/pair/int/right": {
		Message: func(tok *token.Token, args ...any) string {
			return "unexpected type in slice expression"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The pair 'a::b' in a slice expression should be two integers."
		},
	},

	"eval/prefix/var": {
		Message: func(tok *token.Token, args ...any) string {
			return "variable " + emphText(tok.Literal) + " doesn't contain a function"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You are trying to apply the variable " + emphText(tok.Literal) + " as though it was a function, which is only valid if it does in fact contain a variable at runtime"
		},
	},

	"eval/range/index/enum": {
		Message: func(tok *token.Token, args ...any) string {
			return "index " + emphNum(args[0]) + " is out of bounds for enum " + emph(args[1].(string))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "An enum type can be indexed by an integer from 0 up to but not including the length of the type, but you have supplied an integer outside of this range."
		},
	},

	"eval/range/index/list": {
		Message: func(tok *token.Token, args ...any) string {
			return "index " + emphNum(args[0]) + " is out of bounds for a list of length " + emphNum(args[1])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A list is indexed from 0 up to but not including the length of the list."
		},
	},

	"eval/range/index/pair": {
		Message: func(tok *token.Token, args ...any) string {
			return "index " + emphNum(args[0]) + " is out of bounds for a pair"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A pair is indexed by 0 or 1."
		},
	},

	"eval/range/index/string": {
		Message: func(tok *token.Token, args ...any) string {
			return "index " + emphNum(args[0]) + " is out of bounds for a string of length " + emphNum(args[1])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A string is indexed from 0 up to but not including the length of the string."
		},
	},

	"eval/range/index/tuple": {
		Message: func(tok *token.Token, args ...any) string {
			return "index " + emphNum(args[0]) + " is out of bounds for a tuple of length " + emphNum(args[1])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A tuple is indexed from 0 up to but not including the arity of the tuple."
		},
	},

	"eval/range/slice/list": {
		Message: func(tok *token.Token, args ...any) string {
			return "slice " + emph("["+strconv.Itoa(args[0].(int))+"::"+strconv.Itoa(args[1].(int))+"]") + " is out of bounds for a list of length " + emphNum(args[2])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A slice has the form 'x[a::b]', where 'a' and 'b' are integers and a negative value of 'b' counts backwards from the end of the list to be sliced. The bounds of the slice are from-including-to-excluding, like everything else in Pipefish. The slice must lie entirely inside the list being sliced."
		},
	},

	"eval/range/slice/string": {
		Message: func(tok *token.Token, args ...any) string {
			return "slice " + emph("["+strconv.Itoa(args[0].(int))+"::"+strconv.Itoa(args[1].(int))+"]") + " is out of bounds for a string of length " + emphNum(args[2])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A slice has the form 'x[a::b]', where 'a' and 'b' are integers and a negative value of 'b' counts backwards from the end of the string to be sliced. The bounds of the slice are from-including-to-excluding, like everything else in Pipefish. The slice must lie entirely inside the string being sliced."
		},
	},

	"eval/range/slice/tuple": {
		Message: func(tok *token.Token, args ...any) string {
			return "slice " + emph("["+strconv.Itoa(args[0].(int))+"::"+strconv.Itoa(args[1].(int))+"]") + " is out of bounds for a tuple of length " + emphNum(args[2])
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A slice has the form 'x[a::b]', where 'a' and 'b' are integers and a negative value of 'b' counts backwards from the end of the tuple to be sliced. The bounds of the slice are from-including-to-excluding, like everything else in Pipefish. The slice must lie entirely inside the tuple being sliced."
		},
	},

	"eval/repl/assign": {
		Message: func(tok *token.Token, args ...any) string {
			return "attempt to assign the value of a private or non-existent variable or constant " + emph(args[0].(string))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You're referring in the REPL to an identifier " + emph(args[0].(string)) + " as though it " +
				"was a variable or constant. Either you didn't mean to refer to it that way, or you forgot to " +
				"declare it as a variable or constant in the script, or perhaps you declared it private." +
				"\n\nFor more information about the 'private' modifier see 'hub help \"private\"'."
		},
	},

	"eval/repl/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't find function " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Either there is no function/command matching the given parameters, or it has been declared " +
				"private and cannot be accessed through the REPL."
		},
	},

	"eval/repl/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't find suffix " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Either there is no function/command matching the given parameters, or it has been declared " +
				"private and cannot be accessed through the REPL."
		},
	},

	"eval/repl/const": {
		Message: func(tok *token.Token, args ...any) string {
			return "reassigning to a constant '" + args[0].(string) + "' in the REPL"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The value of a constant can only be declared once."
		},
	},

	"eval/repl/private": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't access private function or command from the REPL"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Private *means* private from the REPL."
		},
	},

	"eval/repl/type": {
		Message: func(tok *token.Token, args ...any) string {
			return "attempting to assign object of type " + EmphType(args[0].(values.Value)) + " to a variable of type " +
				text.Emph(args[1].(string))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Once you've declared a variable as being of a given type, the only things you can assign " +
				"to it are objects of that type or of one of its subtypes."
		},
	},

	"eval/repl/var": {
		Message: func(tok *token.Token, args ...any) string {
			return "attempt to access the value of a private or non-existent variable or constant " + emph(args[0].(string))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You're referring in the REPL to an identifier " + emph(args[0].(string)) + " as though it " +
				"was a variable or constant. Either you didn't mean to refer to it that way, or you forgot to " +
				"declare it as a variable or constant in the script, or perhaps you declared it private." +
				"\n\nFor more information about the 'private' modifier see 'hub help \"private\"'."
		},
	},

	"eval/rets/match": {
		Message: func(tok *token.Token, args ...any) string {
			return "return value " + DescribeParams(args[0].([]values.Value)) + " doesn't match function definition "
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The function in question has a return type, given after the '->' operator, and what you have " +
				"returned violates this constraint."
		},
	},

	"eval/ref/ident": {
		Message: func(tok *token.Token, args ...any) string {
			return "expected an identifier, not " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "When a command has 'ref' as the type of its parameter, it expects to be given the name of a variable as an argument."
		},
	},

	"eval/repl/exists": {
		Message: func(tok *token.Token, args ...any) string {
			return "attempt to assign the value of a private or non-existent variable or constant " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You're referring in the REPL to an identifier " + text.DescribeTok(tok) + " as though it " +
				"was a variable or constant. Either you didn't mean to refer to it that way, or you forgot to " +
				"declare it as a variable or constant in the script, or perhaps you declared it private." +
				"\n\nFor more information about the 'private' modifier see 'hub help \"private\"'."
		},
	},

	"eval/repl/ref": {
		Message: func(tok *token.Token, args ...any) string {
			return "attempt to assign the value of a private or non-existent variable or constant " + emph(args[0].(string)) + " dereferecend from " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You're referring in the REPL to an identifier " + emph(args[0].(string)) + " as though it " +
				"was a variable or constant. Either you didn't mean to refer to it that way, or you forgot to " +
				"declare it as a variable or constant in the script, or perhaps you declared it private." +
				"\n\nFor more information about the 'private' modifier see 'hub help \"private\"'."
		},
	},

	"eval/return": {
		Message: func(tok *token.Token, args ...any) string {
			return "unexpected return"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This error occurs when you try to do something with a return statement as though " +
				"it was a value, for example '1 + return \"foo\"'."
		},
	},

	"eval/return/return": {
		Message: func(tok *token.Token, args ...any) string {
			return "trying to respond with a 'respond' statement"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This error is almost self-explanatory: it's what you get if for some reason " +
				"your code includes something of the form 'respond respond <expression>'."
		},
	},

	"eval/sig/lambda": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't apply the supplied anonymous function to " + DescribeParams(args[0].([]values.Value))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "There is a mismatch between the parameters of the anonymous function you defined " +
				"and the arguments you tried to pass to it."
		},
	},

	"eval/snippet/params": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed snippet constructor"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A snippet constructor has a type on the left of the `---` symbol and the text of the snippet on the right"
		},
	},

	"eval/snippet/snippet": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed snippet constructor"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A snippet constructor has a type on the left of the `---` symbol, and the text of the snippet on the right"
		},
	},

	"eval/snippet/type": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed snippet constructor"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A snippet constructor has a type on the left of the `---` symbol and the text of the snippet on the right"
		},
	},

	"eval/stop": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't 'stop' outside of a command"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The 'stop' keyword shuts down the service and so is confined to the 'cmd' section of the script."
		},
	},

	"eval/struct/enum": {
		Message: func(tok *token.Token, args ...any) string {
			return "Field name '" + tok.Literal + "' has already been used as the name of an element in the " +
				"enum '" + args[0].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Once a label, in this case '" + tok.Literal + "' has been used as an element of an enum, " +
				"in this case '" + args[0].(string) + "', it can't be declared as the field name of a struct."
		},
	},

	"eval/struct/field": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't index " + emph(args[1].(string)) + " by field " + emph(args[0].(string))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "That's just not a label of a struc of that type"
		},
	},

	"eval/sv/exists": {
		Message: func(tok *token.Token, args ...any) string {
			return "service variable " + emph(args[0].(string)) + " doesn't exist"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Variables beigining with '$' are resered for service variables, and Pipefish has no service variable called " + emph(args[0].(string)) + "."
		},
	},

	"eval/try/global": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't use a global variable as the argument for 'try'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This is because you're never allowed to store an error in a global variable."
		},
	},

	"eval/unknown/prefix": {
		Message: func(tok *token.Token, args ...any) string {
			return "unknown function: " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You're using " + text.DescribeTok(tok) + " as though it was a prefix operator, but " +
				"it isn't."
		},
	},

	"eval/unknown/operator": {
		Message: func(tok *token.Token, args ...any) string {
			return "unknown operator: " + EmphType(args[0].(values.Value)) + " " + text.DescribeTok(tok) + " " + EmphType(args[1].(values.Value))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You're using " + text.DescribeTok(tok) + " as though it was an infix operator, but " +
				"it isn't."
		},
	},

	"eval/unsatisfied/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "unsatisfied conditional"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A conditional expression in which no condition is met, for example " +
				"'x == 1 : \"one\"; x == 2 : \"two\"' would return this error if called when 'x' " +
				"is in fact 47.\n\nIt's considered good practice to avoid this error by terminating " +
				"every condional with an 'else :' clause.\n\nFor more information " +
				"about conditionals, see 'hub help \"conditionals\"'."
		},
	},

	"eval/unsatisfied/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "unsatisfied conditional"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A conditional expression in which no condition is met, for example " +
				"'x == 1 : \"one\"; x == 2 : \"two\"' would return this error if called when 'x' " +
				"is in fact 47.\n\nIt's considered good practice to avoid this error by terminating " +
				"every condional with an 'else :' clause.\n\nFor more information " +
				"about conditionals, see 'hub help \"conditionals\"'."
		},
	},

	"eval/unsatisfied/e": {
		Message: func(tok *token.Token, args ...any) string {
			return "unsatisfied conditional"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A conditional expression in which no condition is met, for example " +
				"'x == 1 : \"one\"; x == 2 : \"two\"' would return this error if called when 'x' " +
				"is in fact 47.\n\nIt's considered good practice to avoid this error by terminating " +
				"every condional with an 'else :' clause.\n\nFor more information " +
				"about conditionals, see 'hub help \"conditionals\"'."
		},
	},

	"eval/unsatisfied/f": {
		Message: func(tok *token.Token, args ...any) string {
			return "unsatisfied conditional"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A conditional expression in which no condition is met, for example " +
				"'x == 1 : \"one\"; x == 2 : \"two\"' would return this error if called when 'x' " +
				"is in fact 47.\n\nIt's considered good practice to avoid this error by terminating " +
				"every condional with an 'else :' clause.\n\nFor more information " +
				"about conditionals, see 'hub help \"conditionals\"'."
		},
	},

	"eval/unsatisfied/g": {
		Message: func(tok *token.Token, args ...any) string {
			return "unsatisfied conditional"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A conditional expression in which no condition is met, for example " +
				"'x == 1 : \"one\"; x == 2 : \"two\"' would return this error if called when 'x' " +
				"is in fact 47.\n\nIt's considered good practice to avoid this error by terminating " +
				"every condional with an 'else :' clause.\n\nFor more information " +
				"about conditionals, see 'hub help \"conditionals\"'."
		},
	},

	"eval/unsatisfied/h": {
		Message: func(tok *token.Token, args ...any) string {
			return "unsatisfied conditional"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A conditional expression in which no condition is met, for example " +
				"'x == 1 : \"one\"; x == 2 : \"two\"' would return this error if called when 'x' " +
				"is in fact 47.\n\nIt's considered good practice to avoid this error by terminating " +
				"every condional with an 'else :' clause.\n\nFor more information " +
				"about conditionals, see 'hub help \"conditionals\"'."
		},
	},

	"eval/unsatisfied/j": {
		Message: func(tok *token.Token, args ...any) string {
			return "unsatisfied conditional"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A conditional expression in which no condition is met, for example " +
				"'x == 1 : \"one\"; x == 2 : \"two\"' would return this error if called when 'x' " +
				"is in fact 47.\n\nIt's considered good practice to avoid this error by terminating " +
				"every condional with an 'else :' clause.\n\nFor more information " +
				"about conditionals, see 'hub help \"conditionals\"'."
		},
	},

	"eval/unsatisfied/k": {
		Message: func(tok *token.Token, args ...any) string {
			return "unsatisfied conditional"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A conditional expression in which no condition is met, for example " +
				"'x == 1 : \"one\"; x == 2 : \"two\"' would return this error if called when 'x' " +
				"is in fact 47.\n\nIt's considered good practice to avoid this error by terminating " +
				"every condional with an 'else :' clause.\n\nFor more information " +
				"about conditionals, see 'hub help \"conditionals\"'."
		},
	},

	"eval/unsatisfied/l": {
		Message: func(tok *token.Token, args ...any) string {
			return "unsatisfied conditional"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A conditional expression in which no condition is met, for example " +
				"'x == 1 : \"one\"; x == 2 : \"two\"' would return this error if called when 'x' " +
				"is in fact 47.\n\nIt's considered good practice to avoid this error by terminating " +
				"every condional with an 'else :' clause.\n\nFor more information " +
				"about conditionals, see 'hub help \"conditionals\"'."
		},
	},

	// Protected kludge, DNE
	"eval/user": {
		Message: func(tok *token.Token, args ...any) string {
			return "if you see this message, something has gone wrong with the error-handling.\n\nPlease contact the " +
				"author of Pipefish and notify them of the circumstances under which you saw it"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This is a user-defined error."
		},
	},

	"eval/values": {
		Message: func(tok *token.Token, args ...any) string {
			return "not enough values on right-hand side of assignment"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This is the sort of error you'd get if for example you had variables 'x' and 'y' " +
				"and you tried to assign 'x, y = 1'."
		},
	},

	"eval/var/const/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "reassigning constant '" + args[0].(string) + "' in the 'var' section."
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Because you have defined '" + args[0].(string) + "' as a constant in the 'def' " +
				"section of the script, you can't also declare it as a variable in the 'def' section."
		},
	},

	"eval/var/exists/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "reassigning variable '" + args[0].(string) + "' in the 'var' section."
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A variable can only be initialized once, but you've tried to intitialize '" + args[0].(string) +
				"' more than once."
		},
	},

	"eval/var/exists/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "reassigning variable '" + args[0].(string) + "' in the 'var' section."
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A variable can only be initialized once, but you've tried to intitialize '" + args[0].(string) +
				"' more than once."
		},
	},

	"eval/var/type/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "attempting to assign object of type " + EmphType(args[0].(values.Value)) + " to variable of type '" +
				args[1].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Once you've declared a variable as being of a given type, the only things you can assign " +
				"to it are objects of that type or of one of its subtypes."
		},
	},

	"eval/var/type/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "attempting to assign object of type " + EmphType(args[0].(values.Value)) + " to variable of type '" +
				args[1].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Once you've declared a variable as being of a given type, the only things you can assign " +
				"to it are objects of that type or of one of its subtypes."
		},
	},

	"eval/var/type/c": {
		Message: func(tok *token.Token, args ...any) string {
			return "attempting to assign object of type " + EmphType(args[0].(values.Value)) + " to variable of type '" +
				args[1].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Once you've declared a variable as being of a given type, the only things you can assign " +
				"to it are objects of that type or of one of its subtypes."
		},
	},

	"eval/with/type": {
		Message: func(tok *token.Token, args ...any) string {
			return "attempting to initialize object of type <" + args[0].(string) + "> using 'with'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You can only use 'with' to initialize objects that are a subtype of <struct>."
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

	"golang/open": {
		Message: func(tok *token.Token, args ...any) string {
			return "failed to open Go\n\nError was '" + args[0].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A Pipefish function written in Go has failed to open at initialization time."
		},
	},

	"golang/found": {
		Message: func(tok *token.Token, args ...any) string {
			return "couldn't find Go function '" + args[0].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish's system for handling functions written in Go has broken down.\n\n" +
				"There are no circumstances under which you should actually see this error: if you ever " +
				"do, please report it to the author of Pipefish as an issue."
		},
	},

	"golang/return": {
		Message: func(tok *token.Token, args ...any) string {
			return "bad return value from golang"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This is because the author of Pipefish hasn't gotten around to it yet."
		},
	},

	"golang/type/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't pass value of type " + emph(args[0].(string)) + " to Go as raw value"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This is because the author of Pipefish hasn't gotten around to it yet."
		},
	},

	"golang/type/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "can't pass value of type " + emph(args[0].(string)) + " to Go"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "This is because the author of Pipefish hasn't gotten around to it yet."
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

	"init/cmd/assign": {
		Message: func(tok *token.Token, args ...any) string {
			return "attempt to declare a variable or constant in the 'cmd' section"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The only thing you should be doing in the 'cmd' section is defining commands."
		},
	},

	"init/external/assign": {
		Message: func(tok *token.Token, args ...any) string {
			return "attempt to declare a variable or constant in the 'external' section"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return `A line in the 'external' section should consist either of a string representing a filepath, or of an expression of the form <service name>::"<file path>".`
		},
	},

	"init/code/a": {
		Message: func(tok *token.Token, args ...any) string {
			return "unable to open " + emph(tok.Literal) + "; error was " + emph(args[0].(string))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return `A line in the 'external' section should consist either of the name of a service, or of an expression of the form  <service name>::"<file path>".`
		},
	},

	"init/code/b": {
		Message: func(tok *token.Token, args ...any) string {
			return "unable to open " + emph(tok.Literal) + "; error was " + emph(args[0].(string))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return `A line in the 'external' section should consist either of the name of a service, or of an expression of the form  <service name>::"<file path>".`
		},
	},

	"init/external/exist": {
		Message: func(tok *token.Token, args ...any) string {
			return "service " + emph(tok.Literal) + " does not exist on this hub"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return `When a declaration in the 'external' section consists only of an identifier, this will work only if the hub is already running a service with that name.`
		},
	},

	"init/external/form": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed entry in 'external' section"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return `A line in the 'external' section should consist either of the name of a service, or of an expression of the form  <service name>::"<file path>".`
		},
	},

	"init/external/ident": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed entry in 'external' section"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return `A line in the 'external' section should consist either of a string representing a filepath, or of an expression of the form <service name>::"<file path>".`
		},
	},

	"init/external/infix": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed entry in 'external' section"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return `A line in the 'external' section should consist either of a string representing a filepath, or of an expression of the form <service name>::"<file path>".`
		},
	},

	"init/external/string": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed entry in 'external' section"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return `A line in the 'external' section should consist either of a string representing a filepath, or of an expression of the form "<file path>" -> <service name>.`
		},
	},

	"init/def/assign": {
		Message: func(tok *token.Token, args ...any) string {
			return "attempted assignment in the main body of a function"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You can assign to local constants in the 'given' block of a function, if it has one, but " +
				"apart from that, assignment in a function is a syntax error."
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

	"init/enum/free": {
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

	"init/enum/lhs": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed left-hand side of enum definition"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "When you declare an enum, the left hand side should consist of a single identifier followed by a '='."
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

	"init/import/assign": {
		Message: func(tok *token.Token, args ...any) string {
			return "attempted assignment in 'import' section"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The only thing you should be doing in the 'import' section is specifying " +
				"files to import and, optionally, the namespaces to put them in." +
				"\n\nFor more information about the 'import' section see 'hub help \"import\"'."
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

	"init/import/pair": {
		Message: func(tok *token.Token, args ...any) string {
			return "unexpected occurrence of " + text.DescribeTok(tok) +
				" in the 'import' section"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The only function or operator that Pipefish expects to find in the 'import' section is " +
				"the pair operator '::' associating namespaces with files." +
				"\n\nFor more information about the 'import' section see 'hub help \"import\"'."
		},
	},

	"init/import/string": {
		Message: func(tok *token.Token, args ...any) string {
			return "identifier expected"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Entries in the import section should consist either of filenames or of things of the form <namespace>::<\"filename.\">."
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

	"init/lang/assign": {
		Message: func(tok *token.Token, args ...any) string {
			return "attempt to declare a variable or constant in the 'languages' section"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return `A line in the 'languages' section should consist only of the name of a language.`
		},
	},

	"init/lang/form": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed entry in 'languages' section"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return `A line in the 'languages' section should consist only of the name of a language.`
		},
	},

	"init/lang/infix": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed entry in 'languages' section"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return `A line in the 'languages' section should consist only of the name of a language.`
		},
	},

	"init/overload": {
		Message: func(tok *token.Token, args ...any) string {
			return "too much overloading of function '" + args[0].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish allows for multiple dispatch, i.e. you could write two functions like this and the result " +
				"would work as intended\n\n" +
				"|-------------------------------------\n\n" +
				"def\n\nadd(a, b string) : a + b\n\nadd(a, b bool) : \n    \"adding booleans is silly\"\n\n\n" +
				"|-\n\nCalled on two strings, add will do one thing, called on two booleans, it will do " +
				"another. However, suppose you wrote two more functions like this: \n\n" +
				"|-------------------------------------\n\n" +
				"def\n\nadd(a single, b int) : a + b\n\nadd(a int, b single) : a + b\n\n\n" +
				"|-\n\nNow, how does the interpreter decide which version of the function it should use " +
				"when 'add' is passed two integers? It can't and doesn't: Pipefish throws this error instead.\n\nSo you're seeing " +
				"this error because you've done something similar with your function/command/operator.\n\nIf this is something you've done deliberately, we would suggest that " +
				"this is probably a bad practise anyway, which will tend to produce unreadable and unmaintainable code, " +
				"and that you should try to do whatever it is you're doing some other way.\n\n" +
				"For more information about overloading, see 'hub help \"overloading\"'; for a more basic introduction to functions " +
				"see 'hub help \"functions\"'."
		},
	},

	"init/struct/private": {
		Message: func(tok *token.Token, args ...any) string {
			return "public struct type " + emphText(args[0].(string)) + " cannot contain private type " + emphText(args[1].(string))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "A public struct cannot contain a field which has a private type."
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

	"init/source/open": {
		Message: func(tok *token.Token, args ...any) string {
			return "unable to get source '" + args[0].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "The initializer can't retrieve the source code for the given file. Check that it exists."
		},
	},

	"init/struct": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed struct definition"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "On the one hand, the fact that you're using the word 'struct' here makes it " +
				"look like you're declaring a struct, but on the other hand the rest of the line is too " +
				"odd to parse as a struct definition."
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
			return "unmatched or misplaced continuation sign '..'"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish allows two forms of continuation, and one is to end a line in a continuation sign " +
				"'..', and to begin the next line with another '..'\n\n" +
				"For example: \n\n" +
				"|-----------------------------------------------\n\n" +
				"someList = [1, 2, 4,\n         .. 8, 16, 32]\n\n" +
				"|-\n\nNote that such continuations are not bound by the whitespace rules and can be positioned as you like " +
				"for readability.\n\nThe error you are seeing is because you have put an isolated '..' at a line " +
				"boundary, rather than one at the end of one line and another at the start of the next.."
		},
	},

	"lex/cont/c": {
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

	"lex/gocode": {
		Message: func(tok *token.Token, args ...any) string {
			return "no '{' after 'gocode"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "Pipefish expects the 'gocode' token to be followed by '{' to introduce the Go code, to be terminated by a matching '}'."
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

	"init/var/function": {
		Message: func(tok *token.Token, args ...any) string {
			return "declaration of function in 'var' section"
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "In the 'var' section, as the name suggests, you are only supposed to declare the inttial " +
				"values of variables. You've convinced Pipefish that you're trying to declare a function or operator or " +
				"command in the 'var' section instead, probably because the line of code it's complaining about either " +
				"contains a ':' or doesn't contain a '='."
		},
	},

	"parse/before": {
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
			return "Couldn't parse '" + tok.Literal + "' as float64"
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

	"parse/inner": {
		Message: func(tok *token.Token, args ...any) string {
			return "malformed inner function declaration"
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
			return "Can't find namespace " + text.Emph(args[0].(string))
		},
		Explanation: func(errors Errors, pos int, tok *token.Token, args ...any) string {
			return "You haven't declared that namespace in the " + text.Emph("import") + " section"
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

	"parse/sig/d": {
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
			return "can't read from database into type" + emph(args[0].(string))
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

func emph(s string) string {
	return "'" + s + "'"
}

func emphNum(i any) string {
	return "'" + (strconv.Itoa(i.(int))) + "'"
}

func emphText(s any) string {
	return "'" + s.(string) + "'"
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
