package object

import (
	"charm/text"
	"charm/token"

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

var ErrorCreatorMap = map[string] ErrorCreator {

	// TEMPLATE
	"" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return ""
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return ""
		},
	},

	"built/codepoint" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return fmt.Sprintf("codepoint applied to string of length %v", args[0].(int))
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The builtin 'codepoint' function can only be applied to a string consisting of just one rune."
		},
	},

	"built/div/float" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "division by zero"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Because 'x * 0.0 == y * 0.0' for any floating-point numbers 'x' and 'y', mathematicians consider the result of " +
			"dividing by 0.0 to be undefined: there is no right answer — rather, it's the wrong question. So Charm throws " +
			"this error when you ask it."
		},
	},

	"built/div/int" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "division by zero"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Because 'x * 0 == y * 0' for any integers 'x' and 'y', mathematicians consider the result of " +
			"dividing by zero to be undefined: there is no right answer — rather, it's the wrong question. So Charm throws " +
			"this error when you ask it."
		},
	},

	"built/file" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "os returned \"" + "\" when trying to open file '" + args[0].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "This is an error the os of your computer returned when you tried to access a file. If you " +
			"aren't sure what it means, you should consult the coumentation of your os."
		},
	},

	"built/hash/a" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "objects of type <" + args[0].(string) + "> cannot be used as hashkeys"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "In Charm as presently implemented, only some types can be used as hashkeys, including " +
			"<int>, <string>, <float>, <label> and <bool> — but not <" + args[0].(string) + ">."
		},
	},

	"built/hash/b" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "objects of type <" + args[0].(string) + "> cannot be used as hashkeys"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "In Charm as presently implemented, only some types can be used as hashkeys, including " +
			"<int>, <string>, <float>, <label> and <bool> — but not <" + args[0].(string) + ">."
		},
	},

	"built/hash/c" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "objects of type <" + args[0].(string) + "> cannot be used as hashkeys"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "In Charm as presently implemented, only some types can be used as hashkeys, including " +
			"<int>, <string>, <float>, <label> and <bool> — but not <" + args[0].(string) + ">."
		},
	},

	"built/hash/d" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "objects of type <" + args[0].(string) + "> cannot be used as hashkeys"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "In Charm as presently implemented, only some types can be used as hashkeys, including " +
			"<int>, <string>, <float>, <label> and <bool> — but not <" + args[0].(string) + ">."
		},
	},

	"built/hash/key" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "object '" + args[0].(string) + "' is not in the keys of map"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "If you try to retrieve a value from a map by indexing the map with a key that isn't in the map " +
			"then this is the error you will see."
		},
	},

	"built/hash/pairs/a" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "a map must be constructed from things of type <pair>, not of type <" + args[0].(string) + ">"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A map in Charm consists of '<key> :: <value>' pairs, and if you try to make a map out of anything else this will fail."
		},
	},

	"built/hash/pairs/b" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "a map must be constructed from things of type <pair>, not of type <" + args[0].(string) + ">"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A map in Charm consists of '<key> :: <value>' pairs, and if you try to make a map out of anything else this will fail."
		},
	},

	"built/index/int/list" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "a list must be indexed by an integer, not by something of type <" + args[0].(string) + ">"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "An error that very much means what it says. You tried to index a list with something of type <" + 
			    args[0].(string) + ">, and Charm has no idea what you mean by that."
		},
	},

	"built/index/int/string" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "a string must be indexed by an integer, not by something of type <" + args[0].(string) + ">"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "An error that very much means what it says. You tried to index a string with something of type <" + 
			    args[0].(string) + ">, and Charm has no idea what you mean by that."
		},
	},

	"built/index/int/tuple" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "a tuple must be indexed by an integer, not by something of type <" + args[0].(string) + ">"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "An error that very much means what it says. You tried to index a tuple with something of type <" + 
			    args[0].(string) + ">, and Charm has no idea what you mean by that."
		},
	},

	"built/index/tuple/tuple" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "can't index object of type <" + args[0].(string) + ">"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Unless the index operator has been overloaded, you can only index things of type <list>, <pair>, <string>, or <tuple>."
		},
	},

	"built/int" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "can't parse string \"" + args[0].(string) + "\" as integer"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "In order to parse a string as an integer, it must actually represent one, and this string doesn't."
		},
	},

	"built/list/int" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "a list can only be indexed by something of type <integer>"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A list L can be indexed by an expression of the form L[n], where n is a number from 0 up to " +
			"but not including the length of the list."
		},
	},

	"built/mod" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "taking the remainder on division by zero"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Because 'x * 0 == y * 0' for any integers 'x' and 'y', mathematicians consider the result of " +
			"dividing by zero to be undefined: there is no right answer — rather, it's the wrong question. So Charm throws " +
			"an error when you ask it./n/nIn this particular case you were using the '%%' operator, which returns the remainder " +
			"on division by zero, but in order to figure that out, Charm would have to know how to divide by zero, and, as " +
			"just explained, no-one can do that."
		},
	},

	"built/pair" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "objects of type <pair> can't be indexed by " + args[0].(string)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The only things you can index a pair by are 0 for the left-hand element of the pair and 1 for the right-hand element."
		},
	},

	"built/pos/list" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "A list cannot be indexed by a negative value."
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "In Charm the first index of a list is always 0."
		},
	},

	"built/range/list/a" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return fmt.Sprintf("index %v is out of bounds: list has length %v", args[0].(int64), args[1].(int))
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Recall that in Charm, lists are zero-indexed, which means that the first element has index 0 and the " +
			"last element has index 1 less than the length of the list.\n\nAnything outside of these bounds will cause this " +
			"error if used as an index."
		},
	},

	"built/range/list/b" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return fmt.Sprintf("index %v is out of bounds: list has length %v", args[0].(int64), args[1].(int))
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Recall that in Charm, lists are zero-indexed, which means that the first element has index 0 and the " +
			"last element has index 1 less than the length of the list.\n\nAnything outside of these bounds will cause this " +
			"error if used as an index."
		},
	},


	"built/range/tuple" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return fmt.Sprintf("index %v is out of bounds: tuple has arity %v", args[0].(int64), args[1].(int))
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Recall that in Charm, tuples are zero-indexed, which means that the first element has index 0 and the " +
			"last element has index 1 less than the length of the list.\n\nAnything outside of these bounds will cause this " +
			"error if used as an index."
		},
	},

	"built/range/string" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return fmt.Sprintf("index %v is out of bounds: string has length %v", args[0].(int64), args[1].(int))
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Recall that in Charm, strings are zero-indexed, which means that the first element has index 0 and the " +
			"last element has index 1 less than the length of the list.\n\nAnything outside of these bounds will cause this " +
			"error if used as an index."
		},
	},

	"built/slice/int/list" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "slice indexed by '<" + args[0].(string) + 
			"> :: <" + args[1].(string)+ ">'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A slice needs an index of the form '<int> :: <int>' but you have supplied '<" + args[0].(string) + 
			"> :: <" + args[1].(string)+ ">'."
		},
	},

	"built/slice/int/string" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "slice indexed by '<" + args[0].(string) + 
			"> :: <" + args[1].(string)+ ">'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A slice needs an index of the form '<int> :: <int>' but you have supplied '<" + args[0].(string) + 
			"> :: <" + args[1].(string)+ ">'."
		},
	},

	"built/slice/int/tuple" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "slice indexed by '<" + args[0].(string) + 
			"> :: <" + args[1].(string)+ ">'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A slice needs an index of the form '<int> :: <int>' but you have supplied '<" + args[0].(string) + 
			"> :: <" + args[1].(string)+ ">'."
		},
	},

	"built/slice/range/list" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return fmt.Sprintf("slice index '[%v::%v]' out of bounds (list has length %v)", args[0].(int64), args[1].(int64), args[2].(int64))
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Unlike in Python, it is currently considered a runtime error in Charm to try and take a slice beyond the actual bounds of a list."
		},
	},

	"built/slice/range/string" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return fmt.Sprintf("slice index '[%v::%v]' out of bounds (string has length %v)", args[0].(int64), args[1].(int64), args[2].(int64))
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Unlike in Python, it is currently considered a runtime error in Charm to try and take a slice beyond the actual bounds of a string."
		},
	},

	"built/slice/range/tuple" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return fmt.Sprintf("slice index '[%v::%v]' out of bounds (tuple has arity %v)", args[0].(int64), args[1].(int64), args[2].(int))
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Unlike in Python, it is currently considered a runtime error in Charm to try and take a slice beyond the actual bounds of a tuple."
		},
	},

	"built/slice/tuple/tuple" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "can't slice object of type <" + args[0].(string) + ">"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Unless the slice operator has been overloaded, you can only slice things of type <list>, <string> or <tuple>."
		},
	},

	"built/struct/label" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "structs are indexed by things of type <label>, not of type <" + args[0].(string) + ">"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "When you create a struct, the names you give to the parameters in the constructor are automatically " +
			"converted to a set of labels of type 'field' for the fields of the struct. These are the only things you can index the struct by."
		},
	},

	"built/struct/field/a" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "'" + args[0].(string) + "' doesn't label a field of structs of type <" + args[1].(string) +">"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "When you create a struct, the names you give to the parameters in the constructor are automatically " +
			"converted to a set of labels for the fields of the struct. These are the only things you can index the struct by.\n\n" +
			"While you are using a label as an index, it's not a label of this particular type of struct."
		},
	},

	"built/struct/field/b" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "'" + args[0].(string) + "' doesn't label a field of structs of type <" + args[1].(string) +">"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "When you create a struct, the names you give to the parameters in the constructor are automatically " +
			"converted to a set of labels for the fields of the struct. These are the only things you can index the struct by.\n\n" +
			"While you are using a label as an index, it's not a label of this particular type of struct."
		},
	},

	"err/misdirect" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "Charm is trying and failing to raise an error with reference '" + args[0].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The author of Charm, being a silly goose, has managed to throw an error with a code " +
			"that doesn't actually correspond to an error. This should be reported to him as an issue."
		},
	},

	"eval/apply" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "Can't apply <" + args[0].(string) + "> as a function" 
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "In this line you have written something of the form '(<expression 1>) (<expression 2>)'. " +
			"The only way Charm can evaluate something like that is if expression 1 evaluates to something of " + 
			" type <func>, in which case this will be applied to expression 2. However, in this case the " +
			"first expression evaluated to something of type <" + args[0].(string) + ">." 
		},
	},

	"eval/cmd/assign" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "Variables cannot be assigned types in the 'cmd' section." 
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A variable can only be given a type when it is created in the 'var' section of " +
			"the script. In the 'cmd' section you can reassign its value but not its type."
		},
	},

	"eval/cmd/const" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "reassigning constant '" + args[0].(string) + "' in the 'var' section." 
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Because you have defined '" + args[0].(string) + "' as a constant in the 'def' " +
			"section of the script, you can't also declare it as a variable in the 'def' section." +
			"\n\nFor more information about constants see 'hub help constants'."
		},
	},

	"eval/cmd/type" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
				return "attempting to assign object of type <" + args[0].(string) + "> to variable of type <" +
			args[1].(string) + ">"},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Once you've declared a variable as being of a given type, the only things you can assign " +
			"to it are objects of that type or of one of its subtypes.\n\nFor more about variable declarations " +
			"see 'hub help var'."
		},
	},

	"eval/const/assign" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "reassigning to a constant in the 'def' section."
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The value of a constant can only be declared once." +
			"\n\nFor more information about constants see 'hub help constants'."
		},
	},

	"eval/const/type" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
				return "attempting to assign object of type <" + args[0].(string) + "> to constant of type <" +
			args[1].(string) + ">"},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Once you've declared a constant as being of a given type, the only things you can assign " +
			"to it are objects of that type or of one of its subtypes." +
			"\n\nFor more information about constants see 'hub help constants'."
		},
	},

	"eval/enum/index" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "can't index type '" + args[0].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Only enum types can be indexed." 
		},
	},

	"eval/enum/len" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "can't apply 'len' to type '" + args[0].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "An enum type has a length, i.e. the number of elements in the type. But you're trying to " +
			"apply the 'len' function to the type '" + args[0].(string) + "', and Charm doesn't know what you mean by this." 
		},
	},

	"eval/enum/range" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "index of " + fmt.Sprint(args[0].(int64)) + " is out of range"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "An enum type is indexed by a number from and including 0, up to and not ncluding the length of the enum." 
		},
	},

	"eval/eval" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
				return "'eval' takes a string as a parameter"},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "'eval' takes a string as a parameter and returns the result of interpreting that string as an " +
			    "expression in Charm: e.g. 'eval \"2 + 2\"' would return 4."
		},
	},

	"eval/field/struct" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "'" + args[0].(string) + " does not name a field of '" + args[1].(string) +"'."
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You will see this error when you try to index a struct by a label which exists " +
			"but doesn't label a field of that particular type of struct." +
			"\n\nFor more information about structs see 'hub help structs'."
		},
	},

	"eval/field/type" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "field '" + args[0].(string) + "' of variable '" + args[1].(string) +
			 "' should have type <" + args[2].(string) + "> not <" + args[3].(string) + ">"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You will see this error when you try to assign a value to a field of a struct " +
			"and the value is not compatible with the type of the field in the struct declaration." +
			"\n\nFor more information about structs see 'hub help structs'."
		},
	},

	"eval/given/a" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "inexplicable use of 'given'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A 'given' block should follow a command, function, or inner function. Either " +
			"this doesn't follow one at all, or it's so malformed that Charm doesn't recognize it as one."
		},
	},

	"eval/given/b" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "inexplicable use of 'given'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A 'given' block should follow a command, function, or inner function. Either " +
			"this doesn't follow one at all, or it's so malformed that Charm doesn't recognize it as one."
		},
	},

	"eval/given/return" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "attempt to return value in 'given' block"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The only thing you're allowed to do in the 'given' block of a function is assign values " +
			"to local variables."
		},
	},

	"eval/golang" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return args[0].(string)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "This is a runtime error returned by a function written in Go."
		},
	},

	"eval/ident/context" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "Charm can't make any sense of '" + args[0].(string) + "' in that context"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You're tring to treat '" + args[0].(string) + "' as though it was the name " +
			"of a variable, but it isn't."
		},
	},

	"eval/ident/found" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "identifier not found '" + args[0].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You haven't defined anything with the name '" + args[0].(string) + "'."
		},
	},

	"eval/infix/a" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return args[0].(string)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You're trying to treat " + text.DescribeTok(tok) + " as an infix operator but it isn't one."
		},
	},

	"eval/label" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "not the label of a field"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You're using a long-form constructor to create a new instance of a struct, " +
			"and have given it a parameter which is not of the form '<field name> :: <value>'."
		},
	},

	"eval/keyword/a" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "undefined function " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You're tring to use " + text.DescribeTok(tok) + " as a function when it isn't one."
		},
	},

	"eval/keyword/b" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "undefined function " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You're tring to use " + text.DescribeTok(tok) + " as a suffix when it isn't one."
		},
	},

	"eval/keyword/c" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "undefined function " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You're tring to use " + text.DescribeTok(tok) + " as a function when it isn't one."
		},
	},

	"eval/keyword/d" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "undefined function " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You're tring to use " + text.DescribeTok(tok) + " as an infix when it isn't one."
		},
	},

	"eval/malret" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "malformed 'return' expression"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The 'return' statement you used makes no sense in the place where " +
			"you used it."
		},
	},

	"eval/oops" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "something too weird has happened for Charm to supply a meaningful error"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You shouldn't be seeing this error, nor should Charm be emitting it. If " +
			"you are seeing it, it means that some erroneous nil value has snuck through all " +
			"the other checks for nil values, and returning this error is Charm's last line of " +
			"defense against actually crashing.\n\nHence if you see this error, it should be considered " +
			"an issue, and you should notify the author of Charm, who will try to fix it."
		},
	},

	"eval/pair" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "malformed constructor"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You're trying to construct an instance of a struct, but your syntax has gotten Charm so " +
			    "confused that it's not sure what you mean to do."
		},
	},



	"eval/params/infix" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return args[0].(string)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The parameters you've supplied for " + text.DescribeTok(tok) + 
			"don't match its type signature."
		},
	},

	"eval/params/function" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return args[0].(string)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The parameters you've supplied for " + text.DescribeTok(tok) + 
			"don't match its type signature."
		},
	},

	"eval/params/suffix" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return args[0].(string)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The parameters you've supplied for " + text.DescribeTok(tok) + 
			"don't match its type signature."
		},
	},

	"eval/repl/assign" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "attempt to assign the value of a private or non-existent variable or constant " + Emph(args[0].(string))
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You're referring in the REPL to an identifier " + Emph(args[0].(string)) + " as though it " +
			"was a variable or constant. Either you didn't mean to refer to it that way, or you forgot to " +
			"declare it as a variable or constant in the script, or perhaps you declared it private." +
			"\n\nFor more information about the 'private' modifier see 'hub help private'."
		},
	},

	"eval/repl/a" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "can't find function " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Either there is no function/command matching the given parameters, or it has been declared " +
			    "private and cannot be accessed through the REPL."
		},
	},

	"eval/repl/b" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "can't find suffix " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Either there is no function/command matching the given parameters, or it has been declared " +
			    "private and cannot be accessed through the REPL."
		},
	},

	"eval/repl/const" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "reassigning to a constant '" + args[0].(string) + "'in the REPL."
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The value of a constant can only be declared once."
		},
	},

	"eval/repl/d" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "can't find infix " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Either there is no function/command matching the given parameters, or it has been declared " +
			    "private and cannot be accessed through the REPL."
		},
	},

	"eval/repl/type" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
				return "attempting to assign object of type <" + args[0].(string) + "> to constant of type <" +
			args[1].(string) + ">"},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Once you've declared a constant as being of a given type, the only things you can assign " +
			"to it are objects of that type or of one of its subtypes."
		},
	},

	"eval/repl/var" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "attempt to access the value of a private or non-existent variable or constant " + Emph(args[0].(string))
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You're referring in the REPL to an identifier " + Emph(args[0].(string)) + " as though it " +
			"was a variable or constant. Either you didn't mean to refer to it that way, or you forgot to " +
			"declare it as a variable or constant in the script, or perhaps you declared it private." +
			"\n\nFor more information about the 'private' modifier see 'hub help private'."
		},
	},

	"eval/rets/match" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "return value doesn't match function definition"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The function in question has a return type, given after the '->' operator, and what you have " +
			"returned violates this constraint."
		},
	},


	"eval/return" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unexpected return"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "This error occurs when you try to do something with a return statement as though " +
			"it was a value, for example '1 + return \"foo\"'."
		},
	},


	"eval/return/return" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "trying to return a 'return' statement"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "This error is almost self-explanatory: it's what you get if for some reason " +
			"your code includes something of the form 'return return <expression>'."
		},
	},

	"eval/sig/a" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "can't apply " + text.DescribeTok(tok) + " to " + DescribeParams(args[0].([]Object))
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "There is a function "  + text.DescribeTok(tok) + ", but its type signature doesn't " +
			"match the supplied parameters."
		},
	},

	"eval/sig/b" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "can't apply " + text.DescribeTok(tok) + " to " + DescribeParams(args[0].([]Object))
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "There is a function "  + text.DescribeTok(tok) + ", but its type signature doesn't " +
			"match the supplied parameters."
		},
	},

	"eval/sig/c" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "can't apply " + text.DescribeTok(tok) + " to " + DescribeParams(args[0].([]Object))
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "There is a function "  + text.DescribeTok(tok) + ", but its type signature doesn't " +
			"match the supplied parameters."
		},
	},

	"eval/sig/d" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "can't apply " + text.DescribeTok(tok) + " in context " + DescribeParams(args[0].([]Object))
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "There is an infix operator "  + text.DescribeTok(tok) + ", but its type signature doesn't " +
			"match the supplied parameters."
		},
	},

	"eval/sig/lambda" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "can't apply the supplied anonymous function to " + DescribeParams(args[0].([]Object))
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "There is a mismatch between the parametera of the anonymous function you defined " +
			"and the arguments you tried to pass to it."
		},
	},

	"eval/struct/def" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "Structs must be defined in the 'def' section."
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "This error very much means what it says. You can only define new struct types in the 'def' " +
			"section of the code, along with functions, constants, and enums."
		},
	},

	"eval/struct/enum" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "Field name '" + tok.Literal + "' has already been used as the name of an element in the " +
			"enum '" + args[0].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Once a label, in this case '" + tok.Literal + "' has been used as an element of an enum, " +
			"in this case '" + args[0].(string) + "', it can't be declared as the field name of a struct."
		},
	},

	"eval/unknown/suffix" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unknown operator: <" + TrueType(args[0].(Object)) + "> " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You're using " + text.DescribeTok(tok) + " as though it was a suffix operator, but " +
			"it isn't."
		},
	}, 

	"eval/unknown/prefix" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unknown function: " + text.DescribeTok(tok) + " " + EmphType(args[0].(Object))
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You're using " + text.DescribeTok(tok) + " as though it was a prefix operator, but " +
			"it isn't."
		},
	}, 
	
	"eval/unknown/operator" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unknown operator: " + EmphType(args[0].(Object)) + " " + text.DescribeTok(tok) + " " + EmphType(args[0].(Object))
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You're using " + text.DescribeTok(tok) + " as though it was an infix operator, but " +
			"it isn't."
		},
	},

	"eval/unknown/unfix" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unknown command or function"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You're using " + text.DescribeTok(tok) + " as though it was a function or command taking no parameters, but " +
			"it isn't."
		},
	},

	"eval/unsatisfied/a" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unsatisfied conditional"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A conditional expression in which no condition is met, for example " +
			"'x == 1 : \"one\"; x == 2 : \"two\"' would return this error if called when 'x' " +
			"is in fact 47.\n\nIt's considered good practice to avoid this error by terminating " +
			"every condional with an 'else :' clause.\n\nFor more information " +
			"about conditionals, see 'hub help conditionals'."
		},
	},

	"eval/unsatisfied/b" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unsatisfied conditional"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A conditional expression in which no condition is met, for example " +
			"'x == 1 : \"one\"; x == 2 : \"two\"' would return this error if called when 'x' " +
			"is in fact 47.\n\nIt's considered good practice to avoid this error by terminating " +
			"every condional with an 'else :' clause.\n\nFor more information " +
			"about conditionals, see 'hub help conditionals'."
		},
	},

	"eval/unsatisfied/c" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unsatisfied conditional"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A conditional expression in which no condition is met, for example " +
			"'x == 1 : \"one\"; x == 2 : \"two\"' would return this error if called when 'x' " +
			"is in fact 47.\n\nIt's considered good practice to avoid this error by terminating " +
			"every condional with an 'else :' clause.\n\nFor more information " +
			"about conditionals, see 'hub help conditionals'."
		},
	},

	"eval/unsatisfied/d" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unsatisfied conditional"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A conditional expression in which no condition is met, for example " +
			"'x == 1 : \"one\"; x == 2 : \"two\"' would return this error if called when 'x' " +
			"is in fact 47.\n\nIt's considered good practice to avoid this error by terminating " +
			"every condional with an 'else :' clause.\n\nFor more information " +
			"about conditionals, see 'hub help conditionals'."
		},
	},

	"eval/unsatisfied/e" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unsatisfied conditional"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A conditional expression in which no condition is met, for example " +
			"'x == 1 : \"one\"; x == 2 : \"two\"' would return this error if called when 'x' " +
			"is in fact 47.\n\nIt's considered good practice to avoid this error by terminating " +
			"every condional with an 'else :' clause.\n\nFor more information " +
			"about conditionals, see 'hub help conditionals'."
		},
	},

	"eval/unsatisfied/f" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unsatisfied conditional"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A conditional expression in which no condition is met, for example " +
			"'x == 1 : \"one\"; x == 2 : \"two\"' would return this error if called when 'x' " +
			"is in fact 47.\n\nIt's considered good practice to avoid this error by terminating " +
			"every condional with an 'else :' clause.\n\nFor more information " +
			"about conditionals, see 'hub help conditionals'."
		},
	},

	"eval/unsatisfied/g" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unsatisfied conditional"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A conditional expression in which no condition is met, for example " +
			"'x == 1 : \"one\"; x == 2 : \"two\"' would return this error if called when 'x' " +
			"is in fact 47.\n\nIt's considered good practice to avoid this error by terminating " +
			"every condional with an 'else :' clause.\n\nFor more information " +
			"about conditionals, see 'hub help conditionals'."
		},
	},

	"eval/unsatisfied/h" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unsatisfied conditional"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A conditional expression in which no condition is met, for example " +
			"'x == 1 : \"one\"; x == 2 : \"two\"' would return this error if called when 'x' " +
			"is in fact 47.\n\nIt's considered good practice to avoid this error by terminating " +
			"every condional with an 'else :' clause.\n\nFor more information " +
			"about conditionals, see 'hub help conditionals'."
		},
	},

	"eval/unsatisfied/i" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unsatisfied conditional"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A conditional expression in which no condition is met, for example " +
			"'x == 1 : \"one\"; x == 2 : \"two\"' would return this error if called when 'x' " +
			"is in fact 47.\n\nIt's considered good practice to avoid this error by terminating " +
			"every condional with an 'else :' clause.\n\nFor more information " +
			"about conditionals, see 'hub help conditionals'."
		},
	},

	"eval/unsatisfied/j" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unsatisfied conditional"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A conditional expression in which no condition is met, for example " +
			"'x == 1 : \"one\"; x == 2 : \"two\"' would return this error if called when 'x' " +
			"is in fact 47.\n\nIt's considered good practice to avoid this error by terminating " +
			"every condional with an 'else :' clause.\n\nFor more information " +
			"about conditionals, see 'hub help conditionals'."
		},
	},

	"eval/unsatisfied/k" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unsatisfied conditional"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A conditional expression in which no condition is met, for example " +
			"'x == 1 : \"one\"; x == 2 : \"two\"' would return this error if called when 'x' " +
			"is in fact 47.\n\nIt's considered good practice to avoid this error by terminating " +
			"every condional with an 'else :' clause.\n\nFor more information " +
			"about conditionals, see 'hub help conditionals'."
		},
	},

	"eval/user" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "if you see this message, something has gone wrong with the error-handling.\n\nPlease contact the " +
			"author of Charm and notify them of the circumstances under which you saw it"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "This is a user-defined error."
		},
	},

	"eval/values" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "not enough values on right-hand side of assignment"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "This is the sort of error you'd get if for example you had variables 'x' and 'y' " +
			"and you tried to assign 'x, y = 1'."
		},
	},

	"eval/var/const/a" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "reassigning constant '" + args[0].(string) + "' in the 'var' section." 
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Because you have defined '" + args[0].(string) + "' as a constant in the 'def' " +
			"section of the script, you can't also declare it as a variable in the 'def' section."
		},
	},

	"eval/var/const/b" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "reassigning constant '" + args[0].(string) + "' in the 'var' section." 
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Because you have defined '" + args[0].(string) + "' as a constant in the 'def' " +
			"section of the script, you can't also declare it as a variable in the 'def' section."
		},
	},

	"eval/var/exists/a" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "reassigning variable '" + args[0].(string) + "' in the 'var' section." 
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A variable can only be initialized once, but you've tried to intitialize '" + args[0].(string) + 
			"' more than once."
		},
	},

	"eval/var/exists/b" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "reassigning variable '" + args[0].(string) + "' in the 'var' section." 
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A variable can only be initialized once, but you've tried to intitialize '" + args[0].(string) + 
			"' more than once."
		},
	},

	"eval/var/type/a" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
				return "attempting to assign object of type <" + args[0].(string) + "> to variable of type <" +
			args[1].(string) + ">"},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Once you've declared a variable as being of a given type, the only things you can assign " +
			"to it are objects of that type or of one of its subtypes."
		},
	},

	"eval/var/type/b" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
				return "attempting to assign object of type <" + args[0].(string) + "> to variable of type <" +
			args[1].(string) + ">"},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Once you've declared a variable as being of a given type, the only things you can assign " +
			"to it are objects of that type or of one of its subtypes."
		},
	},

	"eval/var/type/c" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
				return "attempting to assign object of type <" + args[0].(string) + "> to variable of type <" +
			args[1].(string) + ">"},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Once you've declared a variable as being of a given type, the only things you can assign " +
			"to it are objects of that type or of one of its subtypes."
		},
	},

	"eval/with/type" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
				return "attempting to initialize or object of type <" + args[0].(string) + "> using 'with'"},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You can only use 'with' to initialize objects that are a subtype of <struct>."
		},
	},

	"golang/build" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
				return "failed to compile Go"},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A Charm function written in Go has failed to compile at initialization time."
		},
	},

	"golang/found" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
				return "couldn't find Go function '" + args[0].(string) + "'"},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm's system for handling functions written in Go has broken down.\n\n" +
			"There are no circumstances under which you should actually see this error: if you ever " +
			"do, please report it to the author of Charm as an issue."
		},
	},

	"golang/type" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
				return "golang type conversion failed for type <" + args[0].(string) + ">"},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A Charm function written in Go has failed to compile at initialization time."
		},
	},

	"init/close" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "'(' unclosed by outdent"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Just as an expression of the form '(foo] bar' would be considered a syntax error, " + 
			"because it violates nesting rules, so too is something of the form"+
			"\n\n|-----------------------------------------------\n\n" +
			"zort :\n" + 
			"    (foo\n" +
			"bar\n\n|-\n\n" + "This violates the nesting rules in just the same way " +
			"as the ']' in '(foo] bar'."
		},
	},

	"init/cmd/assign" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "attempt to declare a variable or constant in the 'cmd' section"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The only thing you should be doing in the 'cmd' section is defining commands."
		},
	},

	"init/def/assign" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "attempted assignment in the main body of a function"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You can assign to local constants in the 'given' block of a function, if it has one, but " +
			"apart from that, assignment in a function is a syntax error."
		},
	},

	"init/enum/comma" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "expected comma, got " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm expects an enum declaration to take the form:"+
			"\n\n|-----------------------------------------------\n\n" +
			"def\n\nMyEnumName = enum FOO, BAR, TROZ\n\n|-\n\n" + "The right hand side of the expression that " +
			"you have supplied is defective because it has a " + text.DescribeTok(tok) + " where " +
			"Charm was expecting to find a comma between elements of the enum."
		},
	},

	"init/enum/free" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "element '" + tok.Literal + "' has already been declare"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The elements of two enums should always be distinct. So for example the following " +
			"will produce an error because 'CLUBS' appears in both enums:" +
			"\n\n|-----------------------------------------------\n\n" +
			"def\n\nSuits = enum CLUBS, HEARTS, SPADES, DIAMONDS\n\n" +
			"MedievalWeaponry = enum CLUBS, SWORDS, MACES\n\n|-\n\n" +
			"And, of course, two elements of the same enum must also always be distinct."

		},
	},

	"init/enum/ident" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "expected identifier, got " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm expects an enum declaration to take the form:"+
			"\n\n|-----------------------------------------------\n\n" +
			"def\n\nMyEnumName = enum FOO, BAR, TROZ\n\n|-\n\n" + "The right hand side of the expression that " +
			"you have supplied is defective because it has a " + text.DescribeTok(tok) + " where " +
			"Charm was expecting one of the elements of the enum."
		},
	},

	"init/enum/lhs" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "malformed left-hand side of enum definition"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "When you declare an enum, the left hand side should consist of a single identifier followed by a '='."
		},
	},

	"init/head" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unexpected occurence of " + text.DescribeTok(tok) + " without a headword"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm expects every part of your script to go in a section headed by one of the headwords " + 
			"'var', 'def', 'cmd', etc. Instead, you've started your script with something other than " +
			"a headword, and so Charm doesn't know which section that thing should belong to."  +
			"\n\nFor more information about headwords see 'hub help headwords'."
		},
	},

	"init/import/assign" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "attempted assignment in 'import' section"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The only thing you should be doing in the 'import' section is specifying " +
			"files to import and, optionally, the namespaces to put them in." +
			"\n\nFor more information about the 'import' section see 'hub help import'."
		},
	},

	"init/import/first" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "if it occurs, 'import' must be the first headword"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm expects every part of your script to go in a section headed by one of the headwords " + 
			"'import', 'var', 'def', 'cmd', etc. If your script is going to have an 'import' headword, " +
			"it must be the first headword of the first section, and so it must be the first thing in your " +
			"script apart from comments and whitespace.\n\nYou're seeing this error because you used 'import' " + 
			"further down in the script." +
			"\n\nFor more information about the 'import' section see 'hub help import'."
		},
	},

	"init/import/infix" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unexpected occurrence of " + text.DescribeTok(tok) +
			     " in the 'import' section"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The only thing you should be doing in the 'import' section is specifying " +
			"files to import and, optionally, the namespaces to put them in." +
			"\n\nFor more information about the 'import' section see 'hub help import'."
		},
	},

	"init/import/file" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "couldn't find file '" + args[0].(string) +"'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm expects imports to be specified by a filename relative to the Charm executable. You told " +
			"Charm to import a file '" + args[0].(string) + "' and the file couldn't be found. Check that you've " +
			"provided the right filename and spelled everything correctly." +
			"\n\nFor more information about the 'import' section see 'hub help import'."
		},
	},

	"init/import/pair" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unexpected occurrence of " + text.DescribeTok(tok) +
			     " in the 'import' section"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The only function or operator that Charm expects to find in the 'import' section is " +
			"the pair operator '::' associating files with their namespaces." +
			"\n\nFor more information about the 'import' section see 'hub help import'."
		},
	},

	"init/import/string/a" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unexpected occurrence of " + text.DescribeTok(tok) +
			     " in the 'import' section"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The only thing you should be doing in the 'import' section is specifying " +
			"files to import and, optionally, the namespaces to put them in. Both should be in " + 
			"the form of string literals." +
			"\n\nFor more information about the 'import' section see 'hub help import'."
		},
	},

	"init/import/string/b" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unexpected occurrence of " + text.DescribeTok(tok) +
			     " in the 'import' section"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The only thing you should be doing in the 'import' section is specifying " +
			"files to import and, optionally, the namespaces to put them in. Both should be in " + 
			"the form of string literals." +
			"\n\nFor more information about the 'import' section see 'hub help import'."
		},
	},

	"init/inexplicable" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "inexplicable occurrence of " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You've used " + text.DescribeTok(tok) + 
			" in a context where Charm can't make sense of it. This error is returned when parsing a " +
			"malformed function declaration, though it may also be returned when the surrounding code is so weird " +
			"that Charm thinks you're trying to declare a function even though you're not." + blame(errors, pos, "lex/comma") +
			"\n\nFor more information about function declarations, see 'hub help functions'." 
		},
	},

	"init/overload" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "too much overloading of keyword '" + args[0].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm allows for multiple dispatch, i.e. you could write two functions like this and the result " +
			"would work as intended\n\n" + 
			"|-------------------------------------\n\n" +
			"def\nadd(a, b string) : a + b\n\nadd(a, b bool) : \"adding booleans makes no sense\"\n\n\n" +
			"|-\n\nCalled on two strings, add will do one thing, called on two booleans, it will do " +
			"another. However, suppose you wrote two more functions like this: \n\n" + 
			"|-------------------------------------\n\n" +
			"def\nadd(a any, b int) : a + b\n\nadd(a int, b any) : a + b\n\n\n" +
			"|-\n\nNow, how does the interpreter decide which version of the function it should use " +
			"when 'add' is passed two integers? It can't and doesn't: Charm throws this error instead.\n\nSo you're seeing " +
			"this error because you've done something similar with your function/command/operator ''" + 
			args[0].(string) + "'.\n\nIf this is something you've done deliberately, we would suggest that " +
			"this is probably a bad practise anyway, which will tend to produce unreadable and unmaintainable code, " + 
			"and that you should try to do whatever it is you're doing some other way.\n\n" +
			"For more information about overloading, see 'hub help overloading'; for a more basic introduction to functions " +
			"see 'hub help functions'."
		},
	},

	"init/private" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "redeclaration of 'private'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "In blocks of the script where things can be declared private (at present only 'var' " +
			"blocks), the 'private' modifier can only be used once after each headword: things before the 'private' " +
			"modifier are private, things after it are public.\n\nYou're seeing this error because you used " +
			"the 'private' modifier twice after the same headword." +
			"\n\nFor more information about the 'private' modifier see 'hub help private'."
		},	

	},

	"init/sig" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return args[0].(string)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Basically, the problem is that the signature of the function you're trying to " +
			"declare is too weird for Charm to understand it. For general information about how to " +
			"declare functions, see 'hub help functions'."
		},
	},

	"init/source/open" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unable to get source '" + args[0].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The initializer can't retrieve the source code for the given file. Check that it exists."
		},
	},

	"init/struct" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "malformed struct definition"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "On the one hand, the fact that you're using the word 'struct' here makes it " +
			"look like you're declaring a struct, but on the other hand the rest of the line is too " +
			"odd to parse as a struct definition."
		},
	},

	"lex/bin" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "invalid binary token"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm expects a token begining with '0b' to represent a number in binary notation, so " +
			"that the remaining characters should all be 0s and 1s. You are seeing this error " +
			"because Charm was unable to parse the token in this way."
		},
	},

	"lex/comma" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "a line ending in ',' must be followed by a line beginning with '..'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm allows two forms of continuation, and one is to end a line in a comma " +
			"in positions where a comma would be correct anyway, and to begin the next line with '..'\n\n" +
			"For example: \n\n" + 
			"|-----------------------------------------------\n\n" +
			"someList = [1, 2, 4,\n         .. 8, 16, 32]\n\n" +
			"|-\n\nNote that such continuations are not bound by the whitespace rules and can be positioned as you like " +
			"for readability.\n\nThe error you're seeing is because you have ended a line with a ',' indicating " +
			"that you want a continuation, but then haven't followed it up with a '..' at the start of the next line." +
			"\n\nFor more information about continuations, see 'hub help continuations'."
		},
	},

	"lex/cont/a" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "a line ending in '..' must be followed by a line beginning with '..'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm allows two forms of continuation, and one is to end a line in a continuation sign " +
			"'..', and to begin the next line with another '..'\n\n" +
			"For example: \n\n" + 
			"var\n\nsum = 1 + 2 + 3 ..    .. + 4 + 5 + 6\n\n" +
			"Note that such continuations are not bound by the whitespace rules and can be positioned as you like " +
			"for readability.\n\nThe error you are seeing is because you have put an isolated '..' at a line " +
			"boundary, rather than one at the end of one line and another at the start of the next.."
		},
	},

	"lex/cont/b" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unmatched or misplaced continuation sign '..'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm allows two forms of continuation, and one is to end a line in a continuation sign " +
			"'..', and to begin the next line with another '..'\n\n" +
			"For example: \n\n" + 
			"var\n\nsum = 1 + 2 + 3 ..    .. + 4 + 5 + 6\n\n" +
			"Note that such continuations are not bound by the whitespace rules and can be positioned as you like " +
			"for readability.\n\nThe error you are seeing is because you have put an isolated '..' at a line " +
			"boundary, rather than one at the end of one line and another at the start of the next.."
		},
	},

	"lex/cont/c" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "a line can't begin with a continuation unless it follows a line ending with either a continuation or a comma"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm allows two forms of continuation, One is to end a line in a continuation sign " +
			"'..', and to begin the next line with another '..'\n\n" +
			"For example: \n\n" + 
			"var\n\nsum = 1 + 2 + 3 ..     .. + 4 + 5 + 6\n\n" + "The other is to end a line in a comma " +
			"in positions where a comma would be correct anyway, and to begin the next line with '..'\n\n" +
			"For example: \n\n" + 
			"var\n\nsomeList = [1, 2, 4,\n         .. 8, 16, 32]\n\n" +
			"Note that such continuations are not bound by the whitespace rules and can be positioned as you like " +
			"for readability.\n\n The error you are seeing is because you have put an isolated '..' at " +
			"the start of a line, rather than one at the end of one line and another at the start of the next."
		},
	},

	"lex/oct" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "invalid octal token"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm expects a token begining with '0o' to represent a number in octal notation, so " +
			"that the remaining characters should all be digits between 0 and 7. You are seeing this error " +
			"because Charm was unable to parse the token in this way."
		},
	},

	"lex/hex" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "invalid hexadecimal token"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm expects a token begining with '0x' to represent a number in binary notation, so " +
			"that the remaining characters should all be digits or letters (of either case) between A and F inclusive. " +
			"You are seeing this error because Charm was unable to parse the token in this way."
		},
	},

	"lex/ident/underscore" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "invalid ident: ends with '_'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm places few restrictions on the forms of identifiers, but one is that they must " +
			"not end in an underscore character '_', and you have broken this rule."
		},
	},

	"lex/ill" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "illegal character"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You have used an illegal character or combination of characters in an identifier."
		},
	},

	"lex/num" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "invalid number"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm expects a token begining with a digit to represent a number. " +
			"You are seeing this error because Charm was unable to parse the token in this way."
		},
	},

	"lex/quote/a" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "string unterminated by end of line"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Having begun a string literal with an opening quote, you haven't concluded it with a matching " +
			    "closing quote before the end of your line of code."
		},
	},

	"lex/quote/b" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "string unterminated by end of line"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Having begun a string literal with an opening quote, you haven't concluded it with a matching " +
			    "closing quote before the end of your line of code."
		},
	},

	"lex/wsp" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "whitespace is inconsistent with previous indentation levels"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "When you outdent your code, it should end up with the same indentation " +
			"as some previous line of code, otherwise Charm can't infer what nesting level you're trying to indicate."
		},
	},

	"init/var/function" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "declaration of function in 'var' section"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "In the 'var' section, as the name suggests, you are only supposed to declare the inttial " +
			"values of variables. You've convinced Charm that you're trying to declare a function or operator or "+
			"command in the 'var' section instead, probably because the line of code it's complaining about either " +
			"contains a ':' or doesn't contain a '='."
		},
	},
	
	"parse/before" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "can't put " + text.DescribeTok(tok) + " before " + text.DescribeTok(args[0].(token.Token))
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "This error occurs when you put one thing after another where they just make no sense in " +
			"sequence, e.g. '8 8' or 'false \"Wombat\"' or '0.5 ('."
		},
	},

	

	"parse/builtin" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "the 'builtin' keyword should be followed by a string literal"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You shouldn't be seeing this error, nor using the 'builtin' keyword. However, if you are, " +
			"the reason for the error is that 'builtin' is followed by a string literal to say which builtin " +
			"function you mean."
		},
	},

	"parse/colon" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "'func' declaration should have a colon"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "A 'func' declaration looks much like a normal function declaration, and like a normal " +
			"function it requires a ':' between the parameters and the function body."
		},
	},

	"parse/close" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "Charm was expecting an expression before closure by " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "This error is produced when an outdent, ')', ']', or '}' occurs when " +
			"an expression is incomplete: for example if you type '(2 + )' into the REPL."
		},
	},

	"parse/eol" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return text.DescribeTok(args[0].(token.Token)) + text.DescribePos(args[0].(token.Token)) +
			    " unclosed by " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You've reached the end of an expression and the " + 
			text.DescribeTok(args[0].(token.Token)) + text.DescribePos(args[0].(token.Token)) +
			"hasn't been supplied with a corresponding " + text.DescribeOpposite(args[0].(token.Token))
		},
	},

	"parse/exec/found" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "couldn't find service '" + args[0].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The 'exec' keyword is supposed to be preceded by the name of a service running on " +
			"the hub. Instead you supplied '" + args[0].(string) + "'"
		},
	},

	"parse/exec/name" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "the keyword 'exec' should be preceded by the name of a service running on " +
			"the hub"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The 'exec' keyword is supposed to be preceded by the name of a service running on " +
			"the hub. Note that you cannot supply an expression: a command 'bar' of service 'FOO' must " +
			"be invoked in the form 'FOO exec bar': there is no way to set 'x' equal to 'FOO' and 'y' equal to " +
			"'bar', and then invoke 'x exec y'."
		},
	},

	"parse/expect" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "expected token " + text.DescribeTok(tok) + ", got token " + 
			    text.DescribeTok(args[0].(token.Token)) + " instead."
		},

		// I can improve this when I see which tokens are actually affected. We already have a lot of
		// bracket-checking, by now it may literally never come up.

		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm expected to find " + text.DescribeTok(tok) + 
			"completing an expression or subexpression, and didn't."
		},
	},

	"parse/expected" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "Charm wasn't expecting " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "This error occurs when an otherwise well-formed expression " +
			"is followed by some stray bit of code that shouldn't be there, e.g. '(2 + 2) \"wombat\"'." + blame(errors, pos, "lex/comma")
		},
	},

	"parse/float" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "Couldn't parse '" + tok.Literal + "' as float"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Something about the form of '" + tok.Literal + "' has persuaded Charm to try and parse it " +
			"as a floating-point number, and yet it is not a floating-point number so Charm has failed."
		},
	},

	"parse/follow" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "Charm expected something to follow " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "This error occurs when something which only makes sense as a prefix, such as 'not', is " +
			     "then not followed by anything."
		},
	},

	"parse/int" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "Couldn't parse '" + tok.Literal + "' as integer"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Something about the form of '" + tok.Literal + "' has persuaded Charm to try and parse it " +
			"as an integer, and yet it is not an integer so Charm has failed."
		},
	},

	"parse/line" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "Charm expected an expression before the end of the line"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "This error is produced when a line stops with something clearly missing, e.g. if you put '2 + '" +
			" into the REPL."
		},
	},

	"parse/malfunc" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "malformed 'func' expression"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "This error pretty much speaks for itself: whatever you put after 'func' was in fact just " +
			"too plain weird for Charm to interpret as a function definition."
		},
	},

	"parse/match" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return text.DescribeTok(tok) + " doesn't close anything" 
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The " + text.DescribeTok(tok) + " at " + text.DescribePos(tok) + " doesn't " +
			    "correspond to  any " + text.DescribeOpposite(tok) + "that needs closing."
		},
	},

	"parse/missing" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "Charm expected an expression on either side of " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "This error is typically returned when a binary operator is missing its left-hand side, e.g. if " +
			"you ask the REPL to evaluate 'and false'."
		},
	},

	"parse/nesting" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return text.DescribeTok(args[0].(token.Token)) + text.DescribePos(args[0].(token.Token)) +
			    " unclosed by " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The " + text.DescribeTok(args[0].(token.Token)) + text.DescribePos(args[0].(token.Token)) +
			" hasn't been supplied with a corresponding " + text.DescribeOpposite(args[0].(token.Token)) +
			" by the time the parser reaches the unmatching nesting closure " +  text.DescribeTok(tok) +
			text.DescribePos(tok) + "."
		},
	},

	// This is another error I'm having some difficulty triggering. Is it reachable code?
	"parse/prefix" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "can't parse " + text.DescribeTok(tok) + " as a prefix."
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You've put "  + text.DescribeTok(tok) + " in such a position that it looks like you want it to " +
			"function as a prefix, but it isn't one."
		},
	},

	"parse/ret/a" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unexpected occurence of " + text.DescribeTok(tok) + " in return types"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm expected your return typesto the left of the '->' to be one or more types separated by " +
			"commas, and so it is puzzled to find " + text.DescribeTok(tok) + " instead."
		},
	},

	"parse/ret/b" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unexpected occurence of " + text.DescribeTok(tok) + " in return types"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm expected your return typesto the left of the '->' to be one or more types separated by " +
			"commas, and so it is puzzled to find " + text.DescribeTok(tok) + " instead."
		},
	},

	"parse/sig/a" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unexpected occurence of " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm is trying to interpret this as a function or assignment signature, and " +
			"the " + text.DescribeTok(tok) + " doesn't belong in such a context.\n\n" +
			"Sometimes when this error occurs, the parser complains about the parameter name you've used " +
			"when from your point of view the problem is that the type you've tried to assign to it doesn't exist. If you're " +
			"finding this issue difficult to debug, you should check if you've done that."
		},
	},

	"parse/sig/b" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unexpected occurence of " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm is trying to interpret this as a function or assignment signature, and " +
			"the " + text.DescribeTok(tok) + " doesn't belong in such a context."
		},
	},

	"parse/sig/c" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unexpected occurence of " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm is trying to interpret this as a function or assignment signature, and " +
			"the " + text.DescribeTok(tok) + " doesn't belong in such a context."
		},
	},

	"parse/sig/d" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "unexpected occurence of " + text.DescribeTok(tok)
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Charm is trying to interpret this as a function or assignment signature, and " +
			"the " + text.DescribeTok(tok) + " doesn't belong in such a context."
		},
	},

	"relex/indent" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "detatched indent"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The indentation marking the beginning of a code block should follow a line ending in a colon."
		},
	},

	"repl/var" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "attempt to access the value of a private or non-existent variable or constant " + Emph(args[0].(string))
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "You're referring in the REPL to an identifier " + Emph(args[0].(string)) + " as though it " +
			"was a variable or constant. Either you didn't mean to refer to it that way, or you forgot to " +
			"declare it as a variable or constant in the script, or perhaps you declared it private." +
			"\n\nFor more information about the 'private' modifier see 'hub help private'."
		},
	},

	"serve/open/filename" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "no filename supplied"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "To open a data file you need to put 'open <dat file name>' or Charm doesn't know what to open."
		},
	},

	"serve/open/only" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "'open' has too many parameters"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Only one thing should follow 'open', the filename of the data file you want to open."
		},
	},

	"serve/save/current" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "no current file"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "In order for 'save' to work without any parameters, you need to first have done 'save <filename>' " +
			"to establish what your file is called, and you don't seem to have done that."
		},
	},

	"serve/save/only" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "'open' has too many parameters"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "Only one thing should follow 'open', the filename of the data file you want to open."
		},
	},

	"serve/save/file/a" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "os returns '" + args[0].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The main body of the error message was generated by the os of your computer when you tried to " +
			    "save the file. If you don't know what it means, you should consult the documentation of your os."
		},
	},

	"serve/save/file/b" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "os returns '" + args[0].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The main body of the error message was generated by the os of your computer when you tried to " +
			    "save the file. If you don't know what it means, you should consult the documentation of your os."
		},
	},

	"serve/open/file" : ErrorCreator {
		Message: func(tok token.Token, args ...any) string {
			return "os returns '" + args[0].(string) + "'"
		},
		Explanation: func(errors Errors, pos int, tok token.Token, args ...any) string {
			return "The main body of the error message was generated by the os of your computer when you tried to " +
			    "save the file. If you don't know what it means, you should consult the documentation of your os."
		},
	},
}

func blame(errors Errors, pos int, args ...string) string {
	if pos == 0 {return ""}
	for _, v := range args {
		if errors[pos - 1].ErrorId == v {
			very := ""
			if ((*errors[pos]).Token.Line - errors[pos - 1].Token.Line) <= 1 {
				very = "very "
			}
			return "\n\nIn this case the problem is " + very + "likely a knock-on effect of the previous error ([" +
				strconv.Itoa(pos - 1) + "] " + errors[pos - 1].Inspect(ViewStdOut) + ".)"
		}
	}
	return ""
}

