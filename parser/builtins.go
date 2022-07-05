package parser

import (
	"bufio"
	"fmt"
	"strconv"
	"os"
	"unicode/utf8"

	"charm/object"
)

var Builtins = map[string] func(args ...object.Object) object.Object {

	"init_file"  : func(args ...object.Object) object.Object {
		fname := args[0].(*object.String).Value
		file, err := os.Open(fname)
		if err != nil {
			return makeErr("built/file", fname, err.Error())
		}
		defer file.Close()

		result := &object.Struct{Name: "file",
		/**/Value: map[string]object.Object{"filename": &object.String{Value: fname}, 
		/**/"contents" : &object.List{Elements: []object.Object{}}}}
		scanner := bufio.NewScanner(file)
		for scanner.Scan() {
			result.Value["contents"].(*object.List).Elements = 
			/**/append(result.Value["contents"].(*object.List).Elements, &object.String{Value: scanner.Text()})
		}
		return result
	},

    "add_pair_to_list" : func(args ...object.Object) object.Object {
        index := args[2].(*object.Pair).Left
        if object.TrueType(index) != "int" {
            return makeErr("built/list/int", object.TrueType(index))
        }
        if index.(*object.Integer).Value < 0 {
            return makeErr("built/list/pos")
        }
		if index.(*object.Integer).Value >= int64(len(args[0].(*object.List).Elements)) {
            return makeErr("built/range/list/a", index.(*object.Integer).Value, len(args[0].(*object.List).Elements))
        }
		newElements := []object.Object{}
        for _, v := range(args[0].(*object.List).Elements) {
            newElements = append(newElements, v)
        }
		newElements[index.(*object.Integer).Value] = args[2].(*object.Pair).Right
        return &object.List{Elements: newElements}
    }, 

	"rune" : func(args ...object.Object) object.Object {
        return &object.String{Value: string(args[0].(*object.Integer).Value)}
    }, 

	"codepoint" : func(args ...object.Object) object.Object {
		slice := []rune(args[0].(*object.String).Value)
		if len(slice) != 1 {
			return makeErr("built/codepoint", len(slice))
		}
        return &object.Integer{Value: int64(slice[0])}
    }, 

    "add_pair_to_struct"    : func(args ...object.Object) object.Object {
        field := args[2].(*object.Pair).Left
        if object.TrueType(field) != "label" {
            return makeErr("built/struct/label", object.TrueType(field))
        }
        _, ok := args[0].(*object.Struct).Value[field.(*object.Label).Value]
        if ! ok {
            return makeErr("built/struct/field/a", field.(*object.Label).Value, args[0].(*object.Struct).Name)
        }

        newValue := make(map[string] object.Object)
        for k, v := range(args[0].(*object.Struct).Value) {
            newValue[k] = v
        }
        newValue[field.(*object.Label).Value] = args[2].(*object.Pair).Right
        return &object.Struct{Name : args[0].(*object.Struct).Name, Labels: args[0].(*object.Struct).Labels, Value: newValue}
    },  

	"charm_single" : func(args ...object.Object) object.Object {
		return &object.String{Value: args[0].Inspect(object.ViewCharmLiteral)}
	},

	"charm_tuple" : func(args ...object.Object) object.Object {
		s := ""
		for i := 0; i < len(args); i++ {
			s = s + args[i].Inspect(object.ViewCharmLiteral)
			if i < len(args) - 1 { s = s + ", " }
		}
		return &object.String{Value: args[0].Inspect(object.ViewCharmLiteral)}
	},

	"single_in_list"	: func(args ...object.Object) object.Object {
		for _, v := range args[2].(*object.List).Elements {
			if object.Equals(args[0], v) {return object.TRUE}
		}
		return object.FALSE
	},

	"single_in_set"	: func(args ...object.Object) object.Object {
		for _, v := range args[2].(*object.Set).Elements {
			if object.Equals(args[0], v) {return object.TRUE}
		}
		return object.FALSE
	},

	"single_in_tuple"	: func(args ...object.Object) object.Object {
		for i := 2 ; i < len(args); i++ {
			if object.Equals(args[0], args[i]) {return object.TRUE}
		}
		return object.FALSE
	},

	"tuple_to_map"	: func(args ...object.Object) object.Object {
		return tupleToMap(args)
	},

	"set_to_map"	: func(args ...object.Object) object.Object {
		return setToMap(args[0])
	},

	"add_pair_to_map"	: func(args ...object.Object) object.Object {
		hashKey, ok := args[2].(*object.Pair).Left.(object.Hashable)
		if !ok {
			return makeErr("built/hash", object.TrueType(args[1]))
		}

		hashed := hashKey.HashKey()
		pair := object.HashPair{Key: args[2].(*object.Pair).Left, Value: args[2].(*object.Pair).Right}
		newMap := make(map[object.HashKey]object.HashPair)

		for key, value := range args[0].(*object.Hash).Pairs {
  			newMap[key] = value
		}

		newMap[hashed] = pair

		return &object.Hash{Pairs: newMap}
	},

	"index_label_of_struct" : func(args ...object.Object) object.Object {
		result, ok := args[1].(*object.Struct).Value[args[0].(*object.Label).Value]
		if !ok {
			return makeErr("built/struct/field/b", args[0].(*object.Label).Value, args[1].(*object.Struct).Name)
		}
		return result
	},

	"index_int_of_list"	: func(args ...object.Object) object.Object {
		return evalArrayIndexExpression(args[1], args[0])
	},
	"index_pair_of_list"	: func(args ...object.Object) object.Object {
		return evalArraySliceExpression(args[1], args[0])
	},
	"index_pair_of_string"	: func(args ...object.Object) object.Object {
		return evalStringSliceExpression(args[1], args[0])
	},
	"index_pair_of_tuple"	: func(args ...object.Object) object.Object {
		return evalTupleSliceExpression(args[1], args[0])
	},
	"index_int_of_tuple"	: func(args ...object.Object) object.Object {
		return evalTupleIndexExpression(args[1], args[0])
	},
	"index_int_of_string"	: func(args ...object.Object) object.Object {
		return evalStringIndexExpression(args[1], args[0])
	},
	"index_int_of_pair"	: func(args ...object.Object) object.Object {
		return evalPairIndexExpression(args[1], args[0])
	},
	"index_any_of_map"	: func(args ...object.Object) object.Object {
		return evalHashIndexExpression(args[1], args[0])
	},
	"make_pair": func(args ...object.Object) object.Object {
        return &object.Pair{Left: args[0], Right: args[2]}
    },

    "add_strings": func(args ...object.Object) object.Object {
        return &object.String{Value: args[0].(*object.String).Value + args[2].(*object.String).Value}
    },

    "add_lists": func(args ...object.Object) object.Object {
        return &object.List{Elements: append(args[0].(*object.List).Elements, args[2].(*object.List).Elements...)}
    },

	"add_sets": func(args ...object.Object) object.Object {
        result := args[0].(*object.Set).Copy()
		for _, v := range args[2].(*object.Set).Elements {
			result.Elements = append(result.Elements, v)
		}
		return result
    },

	"add_element_to_set": func(args ...object.Object) object.Object {
        result := args[0].(*object.Set).Copy()
		result.AddElement(args[2])
		return result
	},

	"< int": func(args ...object.Object) object.Object {
        if args[0].(*object.Integer).Value < args[2].(*object.Integer).Value {
			return object.TRUE
		}
		return object.FALSE
    },
	"<= int": func(args ...object.Object) object.Object {
        if args[0].(*object.Integer).Value <= args[2].(*object.Integer).Value {
			return object.TRUE
		}
		return object.FALSE
    },
	"> int": func(args ...object.Object) object.Object {
        if args[0].(*object.Integer).Value > args[2].(*object.Integer).Value {
			return object.TRUE
		}
		return object.FALSE
    },
	">= int": func(args ...object.Object) object.Object {
        if args[0].(*object.Integer).Value >= args[2].(*object.Integer).Value {
			return object.TRUE
		}
		return object.FALSE
    },

    "add_integers": func(args ...object.Object) object.Object {
        return &object.Integer{Value: args[0].(*object.Integer).Value + args[2].(*object.Integer).Value}
    },

    "negate_integer": func(args ...object.Object) object.Object {
        return &object.Integer{Value: - args[0].(*object.Integer).Value}
    },


    "subtract_integers": func(args ...object.Object) object.Object {
        return &object.Integer{Value: args[0].(*object.Integer).Value - args[2].(*object.Integer).Value}
    },

	"multiply_integers": func(args ...object.Object) object.Object {
        return &object.Integer{Value: args[0].(*object.Integer).Value * args[2].(*object.Integer).Value}
    },

	"modulo_integers": func(args ...object.Object) object.Object {
        if args[2].(*object.Integer).Value == 0 {
            return makeErr("built/mod")
        }
        return &object.Integer{Value: args[0].(*object.Integer).Value % args[2].(*object.Integer).Value}
    },

	"divide_integers": func(args ...object.Object) object.Object {
        if args[2].(*object.Integer).Value == 0 {
            return makeErr("built/div/int")
        }
        return &object.Integer{Value: args[0].(*object.Integer).Value / args[2].(*object.Integer).Value}
    },

    "< float": func(args ...object.Object) object.Object {
        if args[0].(*object.Float).Value < args[2].(*object.Float).Value {
            return object.TRUE
        }
        return object.FALSE
    },
    "<= float": func(args ...object.Object) object.Object {
        if args[0].(*object.Float).Value <= args[2].(*object.Float).Value {
            return object.TRUE
        }
        return object.FALSE
    },
    "> float": func(args ...object.Object) object.Object {
        if args[0].(*object.Float).Value > args[2].(*object.Float).Value {
            return object.TRUE
        }
        return object.FALSE
    },
    ">= float": func(args ...object.Object) object.Object {
        if args[0].(*object.Float).Value >= args[2].(*object.Float).Value {
            return object.TRUE
        }
        return object.FALSE
    },

    "add_floats": func(args ...object.Object) object.Object {
        return &object.Float{Value: args[0].(*object.Float).Value + args[2].(*object.Float).Value}
    },

    "negate_float": func(args ...object.Object) object.Object {
        return &object.Float{Value: - args[0].(*object.Float).Value}
    },

    "subtract_floats": func(args ...object.Object) object.Object {
        return &object.Float{Value: args[0].(*object.Float).Value - args[2].(*object.Float).Value}
    },

    "multiply_floats": func(args ...object.Object) object.Object {
        return &object.Float{Value: args[0].(*object.Float).Value * args[2].(*object.Float).Value}
    },

    "divide_floats": func(args ...object.Object) object.Object {
        if args[2].(*object.Float).Value == 0 {
            return makeErr("built/div/float")
        }
        return &object.Float{Value: args[0].(*object.Float).Value / args[2].(*object.Float).Value}
    },

	"len_list": func(args ...object.Object) object.Object {
			return &object.Integer{Value: int64(len(args[0].(*object.List).Elements))}
	},

	"len_string": func(args ...object.Object) object.Object {
			return &object.Integer{Value: int64(len([]rune(args[0].(*object.String).Value)))}
	},

	"arity_tuple": func(args ...object.Object) object.Object {
		return &object.Integer{Value: int64(len(args))}
	},

	"int_to_string" : func(args ...object.Object) object.Object {
		return &object.String{Value: fmt.Sprint(args[0].(*object.Integer).Value)}
	},

	"float_to_string" : func(args ...object.Object) object.Object {
		return &object.String{Value: fmt.Sprint(args[0].(*object.Float).Value)}
	},

	"string_to_string" : func(args ...object.Object) object.Object {
		return args[0]
	},

	"type_to_string" : func(args ...object.Object) object.Object {
		return &object.String{Value: fmt.Sprint(args[0].(*object.Type).Value)}
	},

	"string_to_int" : func(args ...object.Object) object.Object {
		result, ok := strconv.ParseInt(args[0].(*object.String).Value, 0, 64)
		if ok != nil {
			return makeErr("built/int", args[0].(*object.String).Value)
		}
		return &object.Integer{Value: result}
	},

	"string_to_float" : func(args ...object.Object) object.Object {
		result, _ := strconv.ParseFloat(args[0].(*object.String).Value, 64)
		return &object.Float{Value: result}
	},

	"int_to_float" : func(args ...object.Object) object.Object {
		result := float64(args[0].(*object.Integer).Value)
		return &object.Float{Value: result}
	},

	"float_to_int" : func(args ...object.Object) object.Object {
		result := int64(args[0].(*object.Float).Value)
		return &object.Integer{Value: result}
	},

	"int_to_bool" : func(args ...object.Object) object.Object {
		if args[0].(*object.Integer).Value == 0 {
			return object.FALSE
		}
		return object.TRUE
	},

	"string_to_bool" : func(args ...object.Object) object.Object {
		if args[0].(*object.String).Value == "" {
			return object.FALSE
		}
		return object.TRUE
	},

	"list_to_bool" : func(args ...object.Object) object.Object {
		if len(args[0].(*object.List).Elements) == 0 {
			return object.FALSE
		}
		return object.TRUE
	},

	"set_to_bool" : func(args ...object.Object) object.Object {
		if len(args[0].(*object.Set).Elements) == 0 {
			return object.FALSE
		}
		return object.TRUE
	},

	"map_to_bool" : func(args ...object.Object) object.Object {
		if len(args[0].(*object.Hash).Pairs) == 0 {
			return object.FALSE
		}
		return object.TRUE
	},

	"spread_list" : func(args ...object.Object) object.Object {
		return &object.Tuple{Elements: args[0].(*object.List).Elements}
	},

	"spread_set" : func(args ...object.Object) object.Object {
		return &object.Tuple{Elements: args[0].(*object.Set).Elements}
	},

	"single_to_tuple" : func(args ...object.Object) object.Object {
		return &object.Tuple{Elements: []object.Object{args[0]}}
	},

	"tuple_to_tuple" : func(args ...object.Object) object.Object {
		return &object.Tuple{Elements: args}
	},

	"type_of_tuple" : func(args ...object.Object) object.Object {
		return &object.Type{Value: object.TUPLE_OBJ}
	},

	"type" : func(args ...object.Object) object.Object {
		return &object.Type{Value: object.TrueType(args[0])}
	},

	"make_error" : func(args ...object.Object) object.Object {
		return &object.Error{ErrorId: "eval/user", Message: args[0].(*object.String).Value}
	},

}


func evalArrayIndexExpression(array, index object.Object) object.Object {
	arrayObject := array.(*object.List)
	idx := index.(*object.Integer).Value
	max := int64(len(arrayObject.Elements) - 1)

	if idx < 0 || idx > max {
		return makeErr("built/range/list/b", idx, len(arrayObject.Elements))
	}

	return arrayObject.Elements[idx]
}

func evalPairIndexExpression(pair, index object.Object) object.Object {
	pairObject := pair.(*object.Pair)
	idx := index.(*object.Integer).Value

	if idx < 0 || idx > 1 {
		return makeErr("built/pair", strconv.FormatInt(idx, 10))
	}

	if idx == 0 { return pairObject.Left }
	return pairObject.Right
}

func evalArraySliceExpression(array, index object.Object) object.Object {
	arrayObject := array.(*object.List)
	if !((index.(*object.Pair).Left.Type() == object.INTEGER_OBJ) && (index.(*object.Pair).Right.Type() == object.INTEGER_OBJ)) {
		return makeErr("built/slice/int/list", index.(*object.Pair).Left.Type(), index.(*object.Pair).Right.Type())
	}
	idx := index.(*object.Pair).Left.(*object.Integer).Value
	idy := index.(*object.Pair).Right.(*object.Integer).Value
	max := int64(len(arrayObject.Elements) - 1)
	if idy < 0 { idy = max + idy + 1}

	if (idx < 0 || idx > max) || (idy < 0 || idy > max + 1) || (idy < idx) {
		return makeErr("built/slice/range/list", idx, idy, max + 1)
	}

	return &object.List{Elements: arrayObject.Elements[idx:idy]}
}

func evalTupleSliceExpression(array, index object.Object) object.Object {
	if array.Type() != object.TUPLE_OBJ {
		return makeErr("built/slice/tuple/tuple", object.TrueType(array))
	}
	arrayObject := array.(*object.Tuple)
	if !((index.(*object.Pair).Left.Type() == object.INTEGER_OBJ) && (index.(*object.Pair).Right.Type() == object.INTEGER_OBJ)) {
		return makeErr("built/slice/int/tuple", index.(*object.Pair).Left.Type(), index.(*object.Pair).Right.Type())
	}
	idx := index.(*object.Pair).Left.(*object.Integer).Value
	idy := index.(*object.Pair).Right.(*object.Integer).Value
	max := int64(len(arrayObject.Elements) - 1)
	if idy < 0 { idy = max + idy + 1}

	if (idx < 0 || idx > max) || (idy < 0 || idy > max + 1) {
		return makeErr("built/slice/range/tuple", idx, idy, max + 1)
	}

	return &object.Tuple{Elements: arrayObject.Elements[idx:idy]}
}

func evalStringSliceExpression(string, index object.Object) object.Object {
	stringObject := string.(*object.String)
	if !((index.(*object.Pair).Left.Type() == object.INTEGER_OBJ) && (index.(*object.Pair).Right.Type() == object.INTEGER_OBJ)) {
		return makeErr("built/slice/int/string", index.(*object.Pair).Left.Type(), index.(*object.Pair).Right.Type())
	}
	idx := index.(*object.Pair).Left.(*object.Integer).Value
	idy := index.(*object.Pair).Right.(*object.Integer).Value
	max := int64(len(stringObject.Value) - 1)
	if idy < 0 { idy = max + idy + 1}

	if (idx < 0 || idx > max) || (idy < 0 || idy > max + 1) {
		return makeErr("built/slice/range/string", idx, idy, max + 1)
	}

	return &object.String{Value: stringObject.Value[idx:idy]}
}

func evalTupleIndexExpression(tuple, index object.Object) object.Object {
	if tuple.Type() != object.TUPLE_OBJ {
		return makeErr("built/index/tuple/tuple", object.TrueType(tuple))
	}
	tupleObject := tuple.(*object.Tuple)
	idx := index.(*object.Integer).Value
	max := int64(len(tupleObject.Elements) - 1)

	if idx < 0 || idx > max {
		return makeErr("built/index/range/tuple")
	}

	return tupleObject.Elements[idx]
}

func evalStringIndexExpression(str, index object.Object) object.Object {
	stringObject := str.(*object.String)
	idx := index.(*object.Integer).Value
	max := int64(utf8.RuneCountInString((*stringObject).Value) - 1)

	if idx < 0 || idx > max {
		return makeErr("built/index/range/string")
	}
	result := object.String{Value : string([]rune((*stringObject).Value)[idx])}
	return &result
}

func evalHashIndexExpression(hash, index object.Object) object.Object {
	hashObject := hash.(*object.Hash)

	key, ok := index.(object.Hashable)
	if !ok {
		return makeErr("built/hash/b" + object.TrueType(index))
	}

	pair, ok := hashObject.Pairs[key.HashKey()]
	if !ok {
		return makeErr("built/hash/key", index.Inspect(object.ViewCharmLiteral))
	}

	return pair.Value
}

func tupleToMap(elements []object.Object) object.Object {
	pairs := make(map[object.HashKey]object.HashPair)
	for _, v := range(elements) {
		if v.Type() != object.PAIR_OBJ {
			return makeErr("built/hash/pairs/a", object.TrueType(v))
		}
		hashKey, ok := v.(*object.Pair).Left.(object.Hashable)
		if !ok {
			return makeErr("built/hash/c", object.TrueType(v))
		}

		hashed := hashKey.HashKey()
		pairs[hashed] = object.HashPair{Key: v.(*object.Pair).Left, Value: v.(*object.Pair).Right}
	}

	return &object.Hash{Pairs: pairs}
}

func setToMap(setObject object.Object) object.Object {
	pairs := make(map[object.HashKey]object.HashPair)
	elements := setObject.(*object.Set).Elements
	for _, v := range(elements) {
		if v.Type() != object.PAIR_OBJ {
			return makeErr("built/hash/pairs/b", object.TrueType(v))
		}
		hashKey, ok := v.(*object.Pair).Left.(object.Hashable)
		if !ok {
			return makeErr("built/hash/d", object.TrueType(v))
		}

		hashed := hashKey.HashKey()
		pairs[hashed] = object.HashPair{Key: v.(*object.Pair).Left, Value: v.(*object.Pair).Right}
	}

	return &object.Hash{Pairs: pairs}
}

func makeErr(s string, args ...any) *object.Error {
	return &object.Error{ErrorId: s, Info: args}
}

