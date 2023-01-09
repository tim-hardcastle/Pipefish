package parser

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"unicode/utf8"

	"charm/source/object"
)

var Builtins = map[string]func(p *Parser, args ...object.Object) object.Object{

	"index_int_of_type": func(p *Parser, args ...object.Object) object.Object {
		if p.TypeSystem.PointsTo(args[1].(*object.Type).Value, "enum") {
			ix := args[0].(*object.Integer).Value
			if ix < 0 || ix >= len(p.Enums[args[1].(*object.Type).Value]) {
				return makeErr("eval/enum/range")
			}
			return p.Enums[args[1].(*object.Type).Value][args[0].(*object.Integer).Value]
		} else {
			return makeErr("eval/enum/index")
		}
	},

	"len_of_type": func(p *Parser, args ...object.Object) object.Object {
		if p.TypeSystem.PointsTo(args[0].(*object.Type).Value, "enum") {
			return &object.Integer{Value: len(p.Enums[args[0].(*object.Type).Value])}
		} else {
			return makeErr("eval/enum/len")
		}
	},

	"init_file": func(p *Parser, args ...object.Object) object.Object {
		fname := args[0].(*object.String).Value
		file, err := os.Open(fname)
		if err != nil {
			return makeErr("built/file", fname, err.Error())
		}
		defer file.Close()

		result := &object.Struct{Name: "file",
			Value: map[string]object.Object{"filename": &object.String{Value: fname},
				"contents": &object.List{Elements: []object.Object{}}}}
		scanner := bufio.NewScanner(file)
		for scanner.Scan() {
			result.Value["contents"].(*object.List).Elements =
				append(result.Value["contents"].(*object.List).Elements, &object.String{Value: scanner.Text()})
		}
		return result
	},

	"add_pair_to_list": func(p *Parser, args ...object.Object) object.Object {
		return addPairToList(args...)
	},

	"add_pair_to_struct": func(p *Parser, args ...object.Object) object.Object {
		return addPairToStruct(args...)
	},

	"add_pair_to_map": func(p *Parser, args ...object.Object) object.Object {
		return addPairToMap(args...)
	},

	"add_tuple_to_list": func(p *Parser, args ...object.Object) object.Object {
		return addTupleToList(args...)
	},

	"add_tuple_to_struct": func(p *Parser, args ...object.Object) object.Object {
		return addTupleToStruct(args...)
	},

	"add_tuple_to_map": func(p *Parser, args ...object.Object) object.Object {
		return addTupleToMap(args...)
	},

	"rune": func(p *Parser, args ...object.Object) object.Object {
		return &object.String{Value: string(rune(args[0].(*object.Integer).Value))}
	},

	"codepoint": func(p *Parser, args ...object.Object) object.Object {
		slice := []rune(args[0].(*object.String).Value)
		if len(slice) != 1 {
			return makeErr("built/codepoint", len(slice))
		}
		return &object.Integer{Value: int(slice[0])}
	},

	"charm_single": func(p *Parser, args ...object.Object) object.Object {
		return &object.String{Value: args[0].Inspect(object.ViewCharmLiteral)}
	},

	"charm_tuple": func(p *Parser, args ...object.Object) object.Object {
		s := ""
		for i := 0; i < len(args); i++ {
			s = s + args[i].Inspect(object.ViewCharmLiteral)
			if i < len(args)-1 {
				s = s + ", "
			}
		}
		return &object.String{Value: args[0].Inspect(object.ViewCharmLiteral)}
	},

	"single_in_list": func(p *Parser, args ...object.Object) object.Object {
		for _, v := range args[2].(*object.List).Elements {
			if object.Equals(args[0], v) {
				return object.TRUE
			}
		}
		return object.FALSE
	},

	"single_in_set": func(p *Parser, args ...object.Object) object.Object {
		for _, v := range args[2].(*object.Set).Elements {
			if object.Equals(args[0], v) {
				return object.TRUE
			}
		}
		return object.FALSE
	},

	"single_in_tuple": func(p *Parser, args ...object.Object) object.Object {
		for i := 2; i < len(args); i++ {
			if object.Equals(args[0], args[i]) {
				return object.TRUE
			}
		}
		return object.FALSE
	},

	"tuple_to_map": func(p *Parser, args ...object.Object) object.Object {
		return tupleToMap(args)
	},

	"set_to_map": func(p *Parser, args ...object.Object) object.Object {
		return setToMap(args[0])
	},

	"index_label_of_struct": func(p *Parser, args ...object.Object) object.Object {
		return evalStructIndexExpression(args[1], args[0])
	},

	"index_int_of_list": func(p *Parser, args ...object.Object) object.Object {
		return evalArrayIndexExpression(args[1], args[0])
	},
	"index_pair_of_list": func(p *Parser, args ...object.Object) object.Object {
		return evalArraySliceExpression(args[1], args[0])
	},
	"index_pair_of_string": func(p *Parser, args ...object.Object) object.Object {
		return evalStringSliceExpression(args[1], args[0])
	},
	"index_pair_of_tuple": func(p *Parser, args ...object.Object) object.Object {
		return evalTupleSliceExpression(args[1], args[0])
	},
	"index_int_of_tuple": func(p *Parser, args ...object.Object) object.Object {
		return evalTupleIndexExpression(args[1], args[0])
	},
	"index_int_of_string": func(p *Parser, args ...object.Object) object.Object {
		return evalStringIndexExpression(args[1], args[0])
	},
	"index_int_of_pair": func(p *Parser, args ...object.Object) object.Object {
		return evalPairIndexExpression(args[1], args[0])
	},
	"index_any_of_map": func(p *Parser, args ...object.Object) object.Object {
		return evalHashIndexExpression(args[1], args[0])
	},
	"make_pair": func(p *Parser, args ...object.Object) object.Object {
		return &object.Pair{Left: args[0], Right: args[2]}
	},

	"add_strings": func(p *Parser, args ...object.Object) object.Object {
		return &object.String{Value: args[0].(*object.String).Value + args[2].(*object.String).Value}
	},

	"add_lists": func(p *Parser, args ...object.Object) object.Object {
		return &object.List{Elements: append(args[0].(*object.List).Elements, args[2].(*object.List).Elements...)}
	},

	"add_sets": func(p *Parser, args ...object.Object) object.Object {
		result := args[0].(*object.Set).Copy()
		for _, v := range args[2].(*object.Set).Elements {
			result.Elements = append(result.Elements, v)
		}
		return result
	},

	"add_element_to_set": func(p *Parser, args ...object.Object) object.Object {
		result := args[0].(*object.Set).Copy()
		result.AddElement(args[2])
		return result
	},

	"< int": func(p *Parser, args ...object.Object) object.Object {
		if args[0].(*object.Integer).Value < args[2].(*object.Integer).Value {
			return object.TRUE
		}
		return object.FALSE
	},
	"<= int": func(p *Parser, args ...object.Object) object.Object {
		if args[0].(*object.Integer).Value <= args[2].(*object.Integer).Value {
			return object.TRUE
		}
		return object.FALSE
	},
	"> int": func(p *Parser, args ...object.Object) object.Object {
		if args[0].(*object.Integer).Value > args[2].(*object.Integer).Value {
			return object.TRUE
		}
		return object.FALSE
	},
	">= int": func(p *Parser, args ...object.Object) object.Object {
		if args[0].(*object.Integer).Value >= args[2].(*object.Integer).Value {
			return object.TRUE
		}
		return object.FALSE
	},

	"add_integers": func(p *Parser, args ...object.Object) object.Object {
		return &object.Integer{Value: args[0].(*object.Integer).Value + args[2].(*object.Integer).Value}
	},

	"negate_integer": func(p *Parser, args ...object.Object) object.Object {
		return &object.Integer{Value: -args[0].(*object.Integer).Value}
	},

	"subtract_integers": func(p *Parser, args ...object.Object) object.Object {
		return &object.Integer{Value: args[0].(*object.Integer).Value - args[2].(*object.Integer).Value}
	},

	"multiply_integers": func(p *Parser, args ...object.Object) object.Object {
		return &object.Integer{Value: args[0].(*object.Integer).Value * args[2].(*object.Integer).Value}
	},

	"modulo_integers": func(p *Parser, args ...object.Object) object.Object {
		if args[2].(*object.Integer).Value == 0 {
			return makeErr("built/mod")
		}
		return &object.Integer{Value: args[0].(*object.Integer).Value % args[2].(*object.Integer).Value}
	},

	"divide_integers": func(p *Parser, args ...object.Object) object.Object {
		if args[2].(*object.Integer).Value == 0 {
			return makeErr("built/div/int")
		}
		return &object.Integer{Value: args[0].(*object.Integer).Value / args[2].(*object.Integer).Value}
	},

	"< float64": func(p *Parser, args ...object.Object) object.Object {
		if args[0].(*object.Float).Value < args[2].(*object.Float).Value {
			return object.TRUE
		}
		return object.FALSE
	},
	"<= float64": func(p *Parser, args ...object.Object) object.Object {
		if args[0].(*object.Float).Value <= args[2].(*object.Float).Value {
			return object.TRUE
		}
		return object.FALSE
	},
	"> float64": func(p *Parser, args ...object.Object) object.Object {
		if args[0].(*object.Float).Value > args[2].(*object.Float).Value {
			return object.TRUE
		}
		return object.FALSE
	},
	">= float64": func(p *Parser, args ...object.Object) object.Object {
		if args[0].(*object.Float).Value >= args[2].(*object.Float).Value {
			return object.TRUE
		}
		return object.FALSE
	},

	"add_floats": func(p *Parser, args ...object.Object) object.Object {
		return &object.Float{Value: args[0].(*object.Float).Value + args[2].(*object.Float).Value}
	},

	"negate_float": func(p *Parser, args ...object.Object) object.Object {
		return &object.Float{Value: -args[0].(*object.Float).Value}
	},

	"subtract_floats": func(p *Parser, args ...object.Object) object.Object {
		return &object.Float{Value: args[0].(*object.Float).Value - args[2].(*object.Float).Value}
	},

	"multiply_floats": func(p *Parser, args ...object.Object) object.Object {
		return &object.Float{Value: args[0].(*object.Float).Value * args[2].(*object.Float).Value}
	},

	"divide_floats": func(p *Parser, args ...object.Object) object.Object {
		if args[2].(*object.Float).Value == 0 {
			return makeErr("built/div/float64")
		}
		return &object.Float{Value: args[0].(*object.Float).Value / args[2].(*object.Float).Value}
	},

	"len_list": func(p *Parser, args ...object.Object) object.Object {
		return &object.Integer{Value: len(args[0].(*object.List).Elements)}
	},

	"len_string": func(p *Parser, args ...object.Object) object.Object {
		return &object.Integer{Value: (len([]rune(args[0].(*object.String).Value)))}
	},

	"arity_tuple": func(p *Parser, args ...object.Object) object.Object {
		return &object.Integer{Value: len(args)}
	},

	"int_to_string": func(p *Parser, args ...object.Object) object.Object {
		return &object.String{Value: fmt.Sprint(args[0].(*object.Integer).Value)}
	},

	"float_to_string": func(p *Parser, args ...object.Object) object.Object {
		return &object.String{Value: fmt.Sprint(args[0].(*object.Float).Value)}
	},

	"string_to_string": func(p *Parser, args ...object.Object) object.Object {
		return args[0]
	},

	"type_to_string": func(p *Parser, args ...object.Object) object.Object {
		return &object.String{Value: fmt.Sprint(args[0].(*object.Type).Value)}
	},

	"string_to_int": func(p *Parser, args ...object.Object) object.Object {
		result, ok := strconv.Atoi(args[0].(*object.String).Value)
		if ok != nil {
			return makeErr("built/int", args[0].(*object.String).Value)
		}
		return &object.Integer{Value: result}
	},

	"string_to_float": func(p *Parser, args ...object.Object) object.Object {
		result, _ := strconv.ParseFloat(args[0].(*object.String).Value, 64)
		return &object.Float{Value: result}
	},

	"int_to_float": func(p *Parser, args ...object.Object) object.Object {
		result := float64(args[0].(*object.Integer).Value)
		return &object.Float{Value: result}
	},

	"float_to_int": func(p *Parser, args ...object.Object) object.Object {
		result := int(args[0].(*object.Float).Value)
		return &object.Integer{Value: result}
	},

	"int_to_bool": func(p *Parser, args ...object.Object) object.Object {
		if args[0].(*object.Integer).Value == 0 {
			return object.FALSE
		}
		return object.TRUE
	},

	"string_to_bool": func(p *Parser, args ...object.Object) object.Object {
		if args[0].(*object.String).Value == "" {
			return object.FALSE
		}
		return object.TRUE
	},

	"list_to_bool": func(p *Parser, args ...object.Object) object.Object {
		if len(args[0].(*object.List).Elements) == 0 {
			return object.FALSE
		}
		return object.TRUE
	},

	"set_to_bool": func(p *Parser, args ...object.Object) object.Object {
		if len(args[0].(*object.Set).Elements) == 0 {
			return object.FALSE
		}
		return object.TRUE
	},

	"map_to_bool": func(p *Parser, args ...object.Object) object.Object {
		if len(args[0].(*object.Hash).Pairs) == 0 {
			return object.FALSE
		}
		return object.TRUE
	},

	"spread_list": func(p *Parser, args ...object.Object) object.Object {
		return &object.Tuple{Elements: args[0].(*object.List).Elements}
	},

	"spread_set": func(p *Parser, args ...object.Object) object.Object {
		return &object.Tuple{Elements: args[0].(*object.Set).Elements}
	},

	"single_to_tuple": func(p *Parser, args ...object.Object) object.Object {
		return &object.Tuple{Elements: []object.Object{args[0]}}
	},

	"tuple_to_tuple": func(p *Parser, args ...object.Object) object.Object {
		return &object.Tuple{Elements: args}
	},

	"type_of_tuple": func(p *Parser, args ...object.Object) object.Object {
		return &object.Type{Value: object.TUPLE_OBJ}
	},

	"type": func(p *Parser, args ...object.Object) object.Object {
		return &object.Type{Value: object.TrueType(args[0])}
	},

	"make_error": func(p *Parser, args ...object.Object) object.Object {
		return &object.Error{ErrorId: "eval/user", Message: args[0].(*object.String).Value}
	},
}

func evalArrayIndexExpression(array, index object.Object) object.Object {
	arrayObject := array.(*object.List)
	idx := index.(*object.Integer).Value
	max := len(arrayObject.Elements) - 1

	if idx < 0 || idx > max {
		return makeErr("built/range/list/b", idx, len(arrayObject.Elements))
	}

	return arrayObject.Elements[idx]
}

func evalStructIndexExpression(structure, index object.Object) object.Object {
	result, ok := structure.(*object.Struct).Value[index.(*object.Label).Value]
	if !ok {
		return makeErr("built/struct/field/b", index.(*object.Label), structure.(*object.Struct).Name)
	}
	return result
}

func evalPairIndexExpression(pair, index object.Object) object.Object {
	pairObject := pair.(*object.Pair)
	idx := index.(*object.Integer).Value

	if idx < 0 || idx > 1 {
		return makeErr("built/pair", strconv.Itoa(idx))
	}

	if idx == 0 {
		return pairObject.Left
	}
	return pairObject.Right
}

func evalArraySliceExpression(array, index object.Object) object.Object {
	arrayObject := array.(*object.List)
	if !((index.(*object.Pair).Left.Type() == object.INTEGER_OBJ) && (index.(*object.Pair).Right.Type() == object.INTEGER_OBJ)) {
		return makeErr("built/slice/int/list", index.(*object.Pair).Left, index.(*object.Pair).Right, index)
	}
	idx := index.(*object.Pair).Left.(*object.Integer).Value
	idy := index.(*object.Pair).Right.(*object.Integer).Value
	max := len(arrayObject.Elements) - 1
	if idy < 0 {
		idy = max + idy + 1
	}

	if (idx < 0 || idx > max) || (idy < 0 || idy > max+1) || (idy < idx) {
		return makeErr("built/slice/range/list", idx, idy, max+1)
	}

	return &object.List{Elements: arrayObject.Elements[idx:idy]}
}

func evalTupleSliceExpression(array, index object.Object) object.Object {
	arrayObject := array.(*object.Tuple)
	if !((index.(*object.Pair).Left.Type() == object.INTEGER_OBJ) && (index.(*object.Pair).Right.Type() == object.INTEGER_OBJ)) {
		return makeErr("built/slice/int/tuple", index.(*object.Pair).Left, index.(*object.Pair).Right, index)
	}
	idx := index.(*object.Pair).Left.(*object.Integer).Value
	idy := index.(*object.Pair).Right.(*object.Integer).Value
	max := len(arrayObject.Elements) - 1
	if idy < 0 {
		idy = max + idy + 1
	}

	if (idx < 0 || idx > max) || (idy < 0 || idy > max+1) {
		return makeErr("built/slice/range/tuple", idx, idy, max+1)
	}

	return &object.Tuple{Elements: arrayObject.Elements[idx:idy]}
}

func evalStringSliceExpression(string, index object.Object) object.Object {
	stringObject := string.(*object.String)
	if !((index.(*object.Pair).Left.Type() == object.INTEGER_OBJ) && (index.(*object.Pair).Right.Type() == object.INTEGER_OBJ)) {
		return makeErr("string", index.(*object.Pair).Left, index.(*object.Pair).Right, index)
	}
	idx := index.(*object.Pair).Left.(*object.Integer).Value
	idy := index.(*object.Pair).Right.(*object.Integer).Value
	max := len(stringObject.Value) - 1
	if idy < 0 {
		idy = max + idy + 1
	}

	if (idx < 0 || idx > max) || (idy < 0 || idy > max+1) {
		return makeErr("built/slice/range/string", idx, idy, max+1)
	}

	return &object.String{Value: stringObject.Value[idx:idy]}
}

func evalTupleIndexExpression(tuple, index object.Object) object.Object {
	tupleObject := tuple.(*object.Tuple)
	idx := index.(*object.Integer).Value
	max := len(tupleObject.Elements) - 1

	if idx < 0 || idx > max {
		return makeErr("built/index/range/tuple")
	}

	return tupleObject.Elements[idx]
}

func evalStringIndexExpression(str, index object.Object) object.Object {
	stringObject := str.(*object.String)
	idx := index.(*object.Integer).Value
	max := utf8.RuneCountInString((*stringObject).Value) - 1

	if idx < 0 || idx > max {
		return makeErr("built/index/range/string")
	}
	result := object.String{Value: string([]rune((*stringObject).Value)[idx])}
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
		return makeErr("built/hash/key", object.ViewCharmLiteral)
	}

	return pair.Value
}

func tupleToMap(elements []object.Object) object.Object {
	pairs := make(map[object.HashKey]object.HashPair)
	for _, v := range elements {
		if v.Type() != object.PAIR_OBJ {
			return makeErr("built/hash/pairs/a", v)
		}
		hashKey, ok := v.(*object.Pair).Left.(object.Hashable)
		if !ok {
			return makeErr("built/hash/c", v)
		}

		hashed := hashKey.HashKey()
		pairs[hashed] = object.HashPair{Key: v.(*object.Pair).Left, Value: v.(*object.Pair).Right}
	}

	return &object.Hash{Pairs: pairs}
}

func setToMap(setObject object.Object) object.Object {
	pairs := make(map[object.HashKey]object.HashPair)
	elements := setObject.(*object.Set).Elements
	for _, v := range elements {
		if v.Type() != object.PAIR_OBJ {
			return makeErr("built/hash/pairs/b", v)
		}
		hashKey, ok := v.(*object.Pair).Left.(object.Hashable)
		if !ok {
			return makeErr("built/hash/d", v)
		}

		hashed := hashKey.HashKey()
		pairs[hashed] = object.HashPair{Key: v.(*object.Pair).Left, Value: v.(*object.Pair).Right}
	}

	return &object.Hash{Pairs: pairs}
}

func makeErr(s string, args ...any) *object.Error {
	return &object.Error{ErrorId: s, Info: args}
}

func addTupleToList(args ...object.Object) object.Object {
	if len(args) == 2 {
		return args[0]
	}
	if len(args) == 3 {
		return addPairToList(args...)
	}
	newParams := []object.Object{addPairToList(args[0:3]...), &object.Bling{Value: "with"}}
	newParams = append(newParams, args[3:]...)
	return addTupleToList(newParams...)
}

func addTupleToStruct(args ...object.Object) object.Object {
	if len(args) == 2 {
		return args[0]
	}
	if len(args) == 3 {
		return addPairToStruct(args...)
	}
	newParams := []object.Object{addPairToStruct(args[0:3]...), &object.Bling{Value: "with"}}
	newParams = append(newParams, args[3:]...)
	return addTupleToStruct(newParams...)
}

func addTupleToMap(args ...object.Object) object.Object {
	if len(args) == 2 {
		return args[0]
	}
	if len(args) == 3 {
		return addPairToMap(args...)
	}
	newParams := []object.Object{addPairToMap(args[0:3]...), &object.Bling{Value: "with"}}
	newParams = append(newParams, args[3:]...)
	return addTupleToMap(newParams...)
}

func addPairToList(args ...object.Object) object.Object {
	index := args[2].(*object.Pair).Left
	if object.TrueType(index) == "list" {
		if len(index.(*object.List).Elements) == 0 {
			return makeErr("built/list/empty", object.TrueType(index))
		}
		if len(index.(*object.List).Elements) == 1 {
			return addPairToList(args[0], args[1], &object.Pair{
				Left:  index.(*object.List).Elements[0],
				Right: args[2].(*object.Pair).Right})
		}
		objectToChange := evalArrayIndexExpression(args[0], index.(*object.List).Elements[0])
		if objectToChange.Type() == "error" {
			return objectToChange
		}
		if objectToChange.Type() == "list" {
			return addPairToList(args[0], args[1], &object.Pair{
				Left: index.(*object.List).Elements[0],
				Right: addPairToList(objectToChange, args[1], &object.Pair{
					Left:  &object.List{Elements: index.(*object.List).Elements[1:len(index.(*object.List).Elements)]},
					Right: args[2].(*object.Pair).Right})})
		}
		if objectToChange.Type() == "struct" {
			return addPairToList(args[0], args[1], &object.Pair{
				Left: index.(*object.List).Elements[0],
				Right: addPairToStruct(objectToChange, args[1], &object.Pair{
					Left:  &object.List{Elements: index.(*object.List).Elements[1:len(index.(*object.List).Elements)]},
					Right: args[2].(*object.Pair).Right})})
		}
		if objectToChange.Type() == "map" {
			return addPairToList(args[0], args[1], &object.Pair{
				Left: index.(*object.List).Elements[0],
				Right: addPairToMap(objectToChange, args[1], &object.Pair{
					Left:  &object.List{Elements: index.(*object.List).Elements[1:len(index.(*object.List).Elements)]},
					Right: args[2].(*object.Pair).Right})})
		}
	}
	if object.TrueType(index) != "int" {
		return makeErr("built/list/int", index)
	}
	if index.(*object.Integer).Value < 0 {
		return makeErr("built/list/pos")
	}
	if index.(*object.Integer).Value >= len(args[0].(*object.List).Elements) {
		return makeErr("built/range/list/a", index.(*object.Integer).Value, len(args[0].(*object.List).Elements))
	}
	newElements := []object.Object{}
	for _, v := range args[0].(*object.List).Elements {
		newElements = append(newElements, v)
	}
	newElements[index.(*object.Integer).Value] = args[2].(*object.Pair).Right
	return &object.List{Elements: newElements}
}

func addPairToStruct(args ...object.Object) object.Object {
	index := args[2].(*object.Pair).Left
	if object.TrueType(index) == "list" {
		if len(index.(*object.List).Elements) == 0 {
			return makeErr("built/struct/empty", object.TrueType(index))
		}
		if len(index.(*object.List).Elements) == 1 {
			return addPairToStruct(args[0], args[1], &object.Pair{
				Left:  index.(*object.List).Elements[0],
				Right: args[2].(*object.Pair).Right})
		}
		objectToChange := evalStructIndexExpression(args[0], index.(*object.List).Elements[0])
		if objectToChange.Type() == "error" {
			return objectToChange
		}
		if objectToChange.Type() == "list" {
			return addPairToStruct(args[0], args[1], &object.Pair{
				Left: index.(*object.List).Elements[0],
				Right: addPairToList(objectToChange, args[1], &object.Pair{
					Left:  &object.List{Elements: index.(*object.List).Elements[1:len(index.(*object.List).Elements)]},
					Right: args[2].(*object.Pair).Right})})
		}
		if objectToChange.Type() == "struct" {
			return addPairToStruct(args[0], args[1], &object.Pair{
				Left: index.(*object.List).Elements[0],
				Right: addPairToStruct(objectToChange, args[1], &object.Pair{
					Left:  &object.List{Elements: index.(*object.List).Elements[1:len(index.(*object.List).Elements)]},
					Right: args[2].(*object.Pair).Right})})
		}
		if objectToChange.Type() == "map" {
			return addPairToStruct(args[0], args[1], &object.Pair{
				Left: index.(*object.List).Elements[0],
				Right: addPairToMap(objectToChange, args[1], &object.Pair{
					Left:  &object.List{Elements: index.(*object.List).Elements[1:len(index.(*object.List).Elements)]},
					Right: args[2].(*object.Pair).Right})})
		}
	}
	_, ok := args[0].(*object.Struct).Value[index.(*object.Label).Value]
	if !ok {
		return makeErr("built/struct/field/a", index.(*object.Label), args[0].(*object.Struct).Name)
	}

	newValue := make(map[string]object.Object)
	for k, v := range args[0].(*object.Struct).Value {
		newValue[k] = v
	}
	newValue[index.(*object.Label).Value] = args[2].(*object.Pair).Right
	return &object.Struct{Name: args[0].(*object.Struct).Name, Labels: args[0].(*object.Struct).Labels, Value: newValue}
}

func addPairToMap(args ...object.Object) object.Object {

	index := args[2].(*object.Pair).Left
	if object.TrueType(index) == "list" {
		if len(index.(*object.List).Elements) == 0 {
			return makeErr("built/map/empty", object.TrueType(index))
		}
		if len(index.(*object.List).Elements) == 1 {
			return addPairToMap(args[0], args[1], &object.Pair{
				Left:  index.(*object.List).Elements[0],
				Right: args[2].(*object.Pair).Right})
		}
		objectToChange := evalHashIndexExpression(args[0], index.(*object.List).Elements[0])
		if objectToChange.Type() == "error" {
			return objectToChange
		}
		if objectToChange.Type() == "list" {
			return addPairToMap(args[0], args[1], &object.Pair{
				Left: index.(*object.List).Elements[0],
				Right: addPairToList(objectToChange, args[1], &object.Pair{
					Left:  &object.List{Elements: index.(*object.List).Elements[1:len(index.(*object.List).Elements)]},
					Right: args[2].(*object.Pair).Right})})
		}
		if objectToChange.Type() == "struct" {
			return addPairToMap(args[0], args[1], &object.Pair{
				Left: index.(*object.List).Elements[0],
				Right: addPairToStruct(objectToChange, args[1], &object.Pair{
					Left:  &object.List{Elements: index.(*object.List).Elements[1:len(index.(*object.List).Elements)]},
					Right: args[2].(*object.Pair).Right})})
		}
		if objectToChange.Type() == "map" {
			return addPairToMap(args[0], args[1], &object.Pair{
				Left: index.(*object.List).Elements[0],
				Right: addPairToMap(objectToChange, args[1], &object.Pair{
					Left:  &object.List{Elements: index.(*object.List).Elements[1:len(index.(*object.List).Elements)]},
					Right: args[2].(*object.Pair).Right})})
		}
	}

	hashKey, ok := args[2].(*object.Pair).Left.(object.Hashable)
	if !ok {
		return makeErr("built/hash/a", args[1])
	}

	hashed := hashKey.HashKey()
	pair := object.HashPair{Key: args[2].(*object.Pair).Left, Value: args[2].(*object.Pair).Right}
	newMap := make(map[object.HashKey]object.HashPair)

	for key, value := range args[0].(*object.Hash).Pairs {
		newMap[key] = value
	}

	newMap[hashed] = pair

	return &object.Hash{Pairs: newMap}
}
