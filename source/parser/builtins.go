package parser

import (
	"strconv"
	"unicode/utf8"

	"charm/source/object"
	"charm/source/token"
)

var Builtins = map[string]func(p *Parser, tok token.Token, args ...object.Object) object.Object{

	"keys_of_map": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		returnList := &object.List{Elements: []object.Object{}}
		for _, v := range args[0].(*object.Hash).Pairs {
			returnList.Elements = append(returnList.Elements, v.Key)
		}
		return returnList
	},

	"range": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		index := args[0]
		if !((index.(*object.Pair).Left.Type() == object.INTEGER_OBJ) && (index.(*object.Pair).Right.Type() == object.INTEGER_OBJ)) {
			return newErrorWithVals("built/slice/int/range", tok, []object.Object{index}, index.(*object.Pair).Left, index.(*object.Pair).Right)
		}
		returnList := &object.List{Elements: []object.Object{}}
		for i := index.(*object.Pair).Left.(*object.Integer).Value; i < index.(*object.Pair).Right.(*object.Integer).Value; i++ {
			returnList.Elements = append(returnList.Elements, &object.Integer{Value: i})
		}
		return returnList
	},

	"index_int_of_type": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		if p.TypeSystem.PointsTo(args[1].(*object.Type).Value, "enum") {
			ix := args[0].(*object.Integer).Value
			if ix < 0 || ix >= len(p.Enums[args[1].(*object.Type).Value]) {
				return newError("eval/enum/range", tok)
			}
			return p.Enums[args[1].(*object.Type).Value][args[0].(*object.Integer).Value]
		} else {
			return newError("eval/enum/index", tok)
		}
	},

	"len_of_type": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		if p.TypeSystem.PointsTo(args[0].(*object.Type).Value, "enum") {
			return &object.Integer{Value: len(p.Enums[args[0].(*object.Type).Value])}
		} else {
			return newError("eval/enum/len", tok)
		}
	},

	"add_pair_to_list": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return addPairToList(tok, args...)
	},

	"add_pair_to_struct": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return addPairToStruct(tok, args...)
	},

	"add_pair_to_map": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return addPairToMap(tok, args...)
	},

	"add_tuple_to_list": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return addTupleToList(tok, args...)
	},

	"add_tuple_to_struct": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return addTupleToStruct(tok, args...)
	},

	"add_tuple_to_map": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return addTupleToMap(tok, args...)
	},

	"rune": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.String{Value: string(rune(args[0].(*object.Integer).Value))}
	},

	"codepoint": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		slice := []rune(args[0].(*object.String).Value)
		if len(slice) != 1 {
			return newError("built/codepoint", tok, len(slice))
		}
		return &object.Integer{Value: int(slice[0])}
	},

	"charm_literal": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.String{Value: args[0].Inspect(object.ViewCharmLiteral)}
	},

	"single_in_list": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		for _, v := range args[2].(*object.List).Elements {
			if object.Equals(args[0], v) {
				return object.TRUE
			}
		}
		return object.FALSE
	},

	"single_in_set": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		for _, v := range args[2].(*object.Set).Elements {
			if object.Equals(args[0], v) {
				return object.TRUE
			}
		}
		return object.FALSE
	},

	"single_in_tuple": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		for i := 2; i < len(args); i++ {
			if object.Equals(args[0], args[i]) {
				return object.TRUE
			}
		}
		return object.FALSE
	},

	"tuple_to_map": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		if len(args) == 0 {
			return &object.Hash{}
		}
		return tupleToMap(args[0].(*object.Tuple).Elements, tok)
	},

	"set_to_map": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return setToMap(args[0], tok)
	},

	"index_label_of_struct": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return evalStructIndexExpression(args[0], args[2], tok)
	},

	"index_int_of_list": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return evalArrayIndexExpression(args[0], args[2], tok)
	},
	"index_pair_of_list": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return evalArraySliceExpression(args[0], args[2], tok)
	},
	"index_pair_of_string": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return evalStringSliceExpression(args[0], args[2], tok)
	},
	"index_pair_of_tuple": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return evalTupleSliceExpression(args[0], args[2], tok)
	},
	"index_int_of_tuple": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return evalTupleIndexExpression(args[0], args[2], tok)
	},
	"index_int_of_string": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return evalStringIndexExpression(args[0], args[2], tok)
	},
	"index_int_of_pair": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return evalPairIndexExpression(args[0], args[2], tok)
	},
	"index_any_of_map": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return evalHashIndexExpression(args[0], args[2], tok)
	},
	"make_pair": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.Pair{Left: args[0], Right: args[2]}
	},

	"add_strings": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.String{Value: args[0].(*object.String).Value + args[2].(*object.String).Value}
	},

	"add_lists": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.List{Elements: append(args[0].(*object.List).Elements, args[2].(*object.List).Elements...)}
	},

	"add_sets": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		result := args[0].(*object.Set).Copy()
		result.Elements = append(result.Elements, args[2].(*object.Set).Elements...)

		return result
	},

	"add_element_to_set": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		result := args[0].(*object.Set).Copy()
		result.AddElement(args[2])
		return result
	},

	"< int": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		if args[0].(*object.Integer).Value < args[2].(*object.Integer).Value {
			return object.TRUE
		}
		return object.FALSE
	},
	"<= int": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		if args[0].(*object.Integer).Value <= args[2].(*object.Integer).Value {
			return object.TRUE
		}
		return object.FALSE
	},
	"> int": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		if args[0].(*object.Integer).Value > args[2].(*object.Integer).Value {
			return object.TRUE
		}
		return object.FALSE
	},
	">= int": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		if args[0].(*object.Integer).Value >= args[2].(*object.Integer).Value {
			return object.TRUE
		}
		return object.FALSE
	},

	"add_integers": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.Integer{Value: args[0].(*object.Integer).Value + args[2].(*object.Integer).Value}
	},

	"negate_integer": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.Integer{Value: -args[0].(*object.Integer).Value}
	},

	"subtract_integers": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.Integer{Value: args[0].(*object.Integer).Value - args[2].(*object.Integer).Value}
	},

	"multiply_integers": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.Integer{Value: args[0].(*object.Integer).Value * args[2].(*object.Integer).Value}
	},

	"modulo_integers": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		if args[2].(*object.Integer).Value == 0 {
			return newError("built/mod", tok)
		}
		return &object.Integer{Value: args[0].(*object.Integer).Value % args[2].(*object.Integer).Value}
	},

	"divide_integers": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		if args[2].(*object.Integer).Value == 0 {
			return newError("built/div/int", tok)
		}
		return &object.Integer{Value: args[0].(*object.Integer).Value / args[2].(*object.Integer).Value}
	},

	"< float64": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		if args[0].(*object.Float).Value < args[2].(*object.Float).Value {
			return object.TRUE
		}
		return object.FALSE
	},
	"<= float64": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		if args[0].(*object.Float).Value <= args[2].(*object.Float).Value {
			return object.TRUE
		}
		return object.FALSE
	},
	"> float64": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		if args[0].(*object.Float).Value > args[2].(*object.Float).Value {
			return object.TRUE
		}
		return object.FALSE
	},
	">= float64": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		if args[0].(*object.Float).Value >= args[2].(*object.Float).Value {
			return object.TRUE
		}
		return object.FALSE
	},

	"add_floats": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.Float{Value: args[0].(*object.Float).Value + args[2].(*object.Float).Value}
	},

	"negate_float": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.Float{Value: -args[0].(*object.Float).Value}
	},

	"subtract_floats": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.Float{Value: args[0].(*object.Float).Value - args[2].(*object.Float).Value}
	},

	"multiply_floats": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.Float{Value: args[0].(*object.Float).Value * args[2].(*object.Float).Value}
	},

	"divide_floats": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		if args[2].(*object.Float).Value == 0 {
			return newError("built/div/float64", tok)
		}
		return &object.Float{Value: args[0].(*object.Float).Value / args[2].(*object.Float).Value}
	},

	"len_list": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.Integer{Value: len(args[0].(*object.List).Elements)}
	},

	"len_string": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.Integer{Value: (len([]rune(args[0].(*object.String).Value)))}
	},

	"arity_tuple": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.Integer{Value: len(args[0].(*object.Tuple).Elements)}
	},

	"single_to_string": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.String{Value: args[0].Inspect(object.ViewStdOut)}
	},

	"string_to_int": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		result, ok := strconv.Atoi(args[0].(*object.String).Value)
		if ok != nil {
			return newError("built/int", tok, args[0].(*object.String).Value)
		}
		return &object.Integer{Value: result}
	},

	"string_to_float": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		result, _ := strconv.ParseFloat(args[0].(*object.String).Value, 64)
		return &object.Float{Value: result}
	},

	"int_to_float": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		result := float64(args[0].(*object.Integer).Value)
		return &object.Float{Value: result}
	},

	"float_to_int": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		result := int(args[0].(*object.Float).Value)
		return &object.Integer{Value: result}
	},

	"int_to_bool": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		if args[0].(*object.Integer).Value == 0 {
			return object.FALSE
		}
		return object.TRUE
	},

	"string_to_bool": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		if args[0].(*object.String).Value == "" {
			return object.FALSE
		}
		return object.TRUE
	},

	"list_to_bool": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		if len(args[0].(*object.List).Elements) == 0 {
			return object.FALSE
		}
		return object.TRUE
	},

	"set_to_bool": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		if len(args[0].(*object.Set).Elements) == 0 {
			return object.FALSE
		}
		return object.TRUE
	},

	"map_to_bool": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		if len(args[0].(*object.Hash).Pairs) == 0 {
			return object.FALSE
		}
		return object.TRUE
	},

	"spread_list": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.Tuple{Elements: args[0].(*object.List).Elements}
	},

	"spread_set": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.Tuple{Elements: args[0].(*object.Set).Elements}
	},

	"single_to_tuple": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.Tuple{Elements: []object.Object{args[0]}}
	},

	"tuple_to_tuple": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.Tuple{Elements: args}
	},

	"type_of_tuple": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.Type{Value: object.TUPLE_OBJ}
	},

	"type": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.Type{Value: object.ConcreteType(args[0])}
	},

	"make_error": func(p *Parser, tok token.Token, args ...object.Object) object.Object {
		return &object.Error{ErrorId: "eval/user", Message: args[0].(*object.String).Value}
	},
}

func evalArrayIndexExpression(array, index object.Object, tok token.Token) object.Object {
	arrayObject := array.(*object.List)
	idx := index.(*object.Integer).Value
	max := len(arrayObject.Elements) - 1

	if idx < 0 || idx > max {
		return newError("built/range/list/b", tok, idx, len(arrayObject.Elements))
	}

	return arrayObject.Elements[idx]
}

func evalStructIndexExpression(structure, index object.Object, tok token.Token) object.Object {
	result, ok := structure.(*object.Struct).Value[index.(*object.Label).Value]
	if !ok {
		return newError("built/struct/field/b", tok, index.(*object.Label), structure.(*object.Struct).Name)
	}
	return result
}

func evalPairIndexExpression(pair, index object.Object, tok token.Token) object.Object {
	pairObject := pair.(*object.Pair)
	idx := index.(*object.Integer).Value

	if idx < 0 || idx > 1 {
		return newError("built/pair", tok, strconv.Itoa(idx))
	}

	if idx == 0 {
		return pairObject.Left
	}
	return pairObject.Right
}

func evalArraySliceExpression(array, index object.Object, tok token.Token) object.Object {
	arrayObject := array.(*object.List)
	if !((index.(*object.Pair).Left.Type() == object.INTEGER_OBJ) && (index.(*object.Pair).Right.Type() == object.INTEGER_OBJ)) {
		return newErrorWithVals("built/slice/int/list", tok, []object.Object{index}, index.(*object.Pair).Left, index.(*object.Pair).Right, index)
	}
	idx := index.(*object.Pair).Left.(*object.Integer).Value
	idy := index.(*object.Pair).Right.(*object.Integer).Value
	max := len(arrayObject.Elements)
	if idy < 0 {
		idy = max + idy
	}

	if (idx < 0 || idx > max) || (idy < 0 || idy > max) || (idy < idx) {
		return newError("built/slice/range/list", tok, idx, idy, max)
	}
	return arrayObject.DeepSlice(idx, idy)
}

func evalTupleSliceExpression(array, index object.Object, tok token.Token) object.Object {
	arrayObject := array.(*object.Tuple)
	if !((index.(*object.Pair).Left.Type() == object.INTEGER_OBJ) && (index.(*object.Pair).Right.Type() == object.INTEGER_OBJ)) {
		return newErrorWithVals("built/slice/int/tuple", tok, []object.Object{index}, index.(*object.Pair).Left, index.(*object.Pair).Right, index)
	}
	idx := index.(*object.Pair).Left.(*object.Integer).Value
	idy := index.(*object.Pair).Right.(*object.Integer).Value
	max := len(arrayObject.Elements)
	if idy < 0 {
		idy = max + idy
	}

	if (idx < 0 || idx > max) || (idy < 0 || idy > max) {
		return newError("built/slice/range/tuple", tok, idx, idy, max)
	}
	return arrayObject.DeepSlice(idx, idy)
}

func evalStringSliceExpression(string, index object.Object, tok token.Token) object.Object {
	stringObject := string.(*object.String)
	if !((index.(*object.Pair).Left.Type() == object.INTEGER_OBJ) && (index.(*object.Pair).Right.Type() == object.INTEGER_OBJ)) {
		return newErrorWithVals("built/slice/int/string", tok, []object.Object{index}, index.(*object.Pair).Left, index.(*object.Pair).Right)
	}
	idx := index.(*object.Pair).Left.(*object.Integer).Value
	idy := index.(*object.Pair).Right.(*object.Integer).Value
	max := len(stringObject.Value)
	if idy < 0 {
		idy = max + idy
	}

	if (idx < 0 || idx > max) || (idy < 0 || idy > max) {
		return newError("built/slice/range/string", tok, idx, idy, max)
	}
	return &object.String{Value: stringObject.Value[idx:idy]}
}

func evalTupleIndexExpression(tuple, index object.Object, tok token.Token) object.Object {
	switch tupleObject := tuple.(type) {
	case *object.Tuple:
		idx := index.(*object.Integer).Value
		max := len(tupleObject.Elements) - 1
		if idx < 0 || idx > max {
			return newError("built/index/range/tuple", tok)
		}
		return tupleObject.Elements[idx]
	default:
		return newError("built/index/type", tok, tuple)
	}
}

func evalStringIndexExpression(str, index object.Object, tok token.Token) object.Object {
	stringObject := str.(*object.String)
	idx := index.(*object.Integer).Value
	max := utf8.RuneCountInString((*stringObject).Value) - 1

	if idx < 0 || idx > max {
		return newError("built/index/range/string", tok)
	}
	result := object.String{Value: string([]rune((*stringObject).Value)[idx])}
	return &result
}

func evalHashIndexExpression(hash, index object.Object, tok token.Token) object.Object {
	hashObject := hash.(*object.Hash)

	key, ok := index.(object.Hashable)
	if !ok {
		return newError("built/hash/b"+object.ConcreteType(index), tok)
	}

	pair, ok := hashObject.Pairs[key.HashKey()]
	if !ok {
		return newError("built/hash/key", tok, index)
	}

	return pair.Value
}

func tupleToMap(elements []object.Object, tok token.Token) object.Object {
	pairs := make(map[object.HashKey]object.HashPair)
	for _, v := range elements {
		if v.Type() != object.PAIR_OBJ {
			return newError("built/hash/pairs/a", tok, v)
		}
		hashKey, ok := v.(*object.Pair).Left.(object.Hashable)
		if !ok {
			return newError("built/hash/c", tok, v)
		}

		hashed := hashKey.HashKey()
		pairs[hashed] = object.HashPair{Key: v.(*object.Pair).Left, Value: v.(*object.Pair).Right}
	}

	return &object.Hash{Pairs: pairs}
}

func setToMap(setObject object.Object, tok token.Token) object.Object {
	pairs := make(map[object.HashKey]object.HashPair)
	elements := setObject.(*object.Set).Elements
	for _, v := range elements {
		if v.Type() != object.PAIR_OBJ {
			return newError("built/hash/pairs/b", tok, v)
		}
		hashKey, ok := v.(*object.Pair).Left.(object.Hashable)
		if !ok {
			return newError("built/hash/d", tok, v)
		}

		hashed := hashKey.HashKey()
		pairs[hashed] = object.HashPair{Key: v.(*object.Pair).Left, Value: v.(*object.Pair).Right}
	}

	return &object.Hash{Pairs: pairs}
}

func addTupleToList(tok token.Token, args ...object.Object) object.Object {
	if len(args) == 2 {
		return args[0]
	}
	outList := args[0].DeepCopy()
	for _, v := range args[2].(*object.Tuple).Elements {
		outList = addPairToList(tok, outList, &object.Bling{}, v)
	}
	return outList
}

func addTupleToStruct(tok token.Token, args ...object.Object) object.Object {
	if len(args) == 2 {
		return args[0]
	}
	outStruct := args[0].DeepCopy()
	for _, v := range args[2].(*object.Tuple).Elements {
		outStruct = addPairToStruct(tok, outStruct, &object.Bling{}, v)
	}
	return outStruct
}

func addTupleToMap(tok token.Token, args ...object.Object) object.Object {
	if len(args) == 2 {
		return args[0]
	}
	outMap := args[0].DeepCopy()
	for _, v := range args[2].(*object.Tuple).Elements {
		outMap = addPairToMap(tok, outMap, &object.Bling{}, v)
	}
	return outMap
}

func addPairToList(tok token.Token, args ...object.Object) object.Object {
	args[0] = args[0].DeepCopy()
	return unsafeAddPairToList(tok, args...)
}

func unsafeAddPairToList(tok token.Token, args ...object.Object) object.Object {
	index := args[2].(*object.Pair).Left
	if object.ConcreteType(index) == "list" {
		if len(index.(*object.List).Elements) == 0 {
			return newError("built/list/empty", tok, object.ConcreteType(index))
		}
		if len(index.(*object.List).Elements) == 1 {
			return addPairToList(tok, args[0], args[1], &object.Pair{
				Left:  index.(*object.List).Elements[0],
				Right: args[2].(*object.Pair).Right})
		}
		objectToChange := evalArrayIndexExpression(args[0], index.(*object.List).Elements[0], tok)
		if objectToChange.Type() == "error" {
			return objectToChange
		}
		if objectToChange.Type() == "list" {
			return addPairToList(tok, args[0], args[1], &object.Pair{
				Left: index.(*object.List).Elements[0],
				Right: addPairToList(tok, objectToChange, args[1], &object.Pair{
					Left:  &object.List{Elements: index.(*object.List).Elements[1:len(index.(*object.List).Elements)]},
					Right: args[2].(*object.Pair).Right})})
		}
		if objectToChange.Type() == "struct" {
			return addPairToList(tok, args[0], args[1], &object.Pair{
				Left: index.(*object.List).Elements[0],
				Right: addPairToStruct(tok, objectToChange, args[1], &object.Pair{
					Left:  &object.List{Elements: index.(*object.List).Elements[1:len(index.(*object.List).Elements)]},
					Right: args[2].(*object.Pair).Right})})
		}
		if objectToChange.Type() == "map" {
			return addPairToList(tok, args[0], args[1], &object.Pair{
				Left: index.(*object.List).Elements[0],
				Right: addPairToMap(tok, objectToChange, args[1], &object.Pair{
					Left:  &object.List{Elements: index.(*object.List).Elements[1:len(index.(*object.List).Elements)]},
					Right: args[2].(*object.Pair).Right})})
		}
	}
	if object.ConcreteType(index) != "int" {
		return newError("built/list/int", tok, index)
	}
	if index.(*object.Integer).Value < 0 {
		return newError("built/list/pos", tok)
	}
	if index.(*object.Integer).Value >= len(args[0].(*object.List).Elements) {
		return newError("built/range/list/a", tok, index.(*object.Integer).Value, len(args[0].(*object.List).Elements))
	}
	newElements := []object.Object{}
	newElements = append(newElements, args[0].(*object.List).Elements...)
	newElements[index.(*object.Integer).Value] = args[2].(*object.Pair).Right
	return &object.List{Elements: newElements}
}

func addPairToStruct(tok token.Token, args ...object.Object) object.Object {
	args[0] = args[0].DeepCopy()
	return unsafeAddPairToStruct(tok, args...)
}

func unsafeAddPairToStruct(tok token.Token, args ...object.Object) object.Object {
	if args[2].Type() != object.PAIR_OBJ {
		return newError("built/struct/pair", tok, object.ConcreteType(args[2]))
	}
	index := args[2].(*object.Pair).Left
	if object.ConcreteType(index) == "list" {
		if len(index.(*object.List).Elements) == 0 {
			return newError("built/struct/empty", tok, object.ConcreteType(index))
		}
		if len(index.(*object.List).Elements) == 1 {
			return unsafeAddPairToStruct(tok, args[0], args[1], &object.Pair{
				Left:  index.(*object.List).Elements[0],
				Right: args[2].(*object.Pair).Right})
		}
		objectToChange := evalStructIndexExpression(args[0], index.(*object.List).Elements[0], tok)
		if objectToChange.Type() == "error" {
			return objectToChange
		}
		if objectToChange.Type() == "list" {
			return unsafeAddPairToStruct(tok, args[0], args[1], &object.Pair{
				Left: index.(*object.List).Elements[0],
				Right: unsafeAddPairToList(tok, objectToChange, args[1], &object.Pair{
					Left:  &object.List{Elements: index.(*object.List).Elements[1:len(index.(*object.List).Elements)]},
					Right: args[2].(*object.Pair).Right})})
		}
		if objectToChange.Type() == "struct" {
			return unsafeAddPairToStruct(tok, args[0], args[1], &object.Pair{
				Left: index.(*object.List).Elements[0],
				Right: unsafeAddPairToStruct(tok, objectToChange, args[1], &object.Pair{
					Left:  &object.List{Elements: index.(*object.List).Elements[1:len(index.(*object.List).Elements)]},
					Right: args[2].(*object.Pair).Right})})
		}
		if objectToChange.Type() == "map" {
			return unsafeAddPairToStruct(tok, args[0], args[1], &object.Pair{
				Left: index.(*object.List).Elements[0],
				Right: unsafeAddPairToMap(tok, objectToChange, args[1], &object.Pair{
					Left:  &object.List{Elements: index.(*object.List).Elements[1:len(index.(*object.List).Elements)]},
					Right: args[2].(*object.Pair).Right})})
		}
	}
	_, ok := args[0].(*object.Struct).Value[index.(*object.Label).Value]
	if !ok {
		return newError("built/struct/field/a", tok, index.(*object.Label), args[0].(*object.Struct).Name)
	}

	newValue := make(map[string]object.Object)
	for k, v := range args[0].(*object.Struct).Value {
		newValue[k] = v
	}
	newValue[index.(*object.Label).Value] = args[2].(*object.Pair).Right
	return &object.Struct{Name: args[0].(*object.Struct).Name, Labels: args[0].(*object.Struct).Labels, Value: newValue}
}

func addPairToMap(tok token.Token, args ...object.Object) object.Object {
	args[0] = args[0].DeepCopy()
	return unsafeAddPairToMap(tok, args...)
}

func unsafeAddPairToMap(tok token.Token, args ...object.Object) object.Object {

	index := args[2].(*object.Pair).Left
	if object.ConcreteType(index) == "list" {
		if len(index.(*object.List).Elements) == 0 {
			return newError("built/map/empty", tok, object.ConcreteType(index))
		}
		if len(index.(*object.List).Elements) == 1 {
			return unsafeAddPairToMap(tok, args[0], args[1], &object.Pair{
				Left:  index.(*object.List).Elements[0],
				Right: args[2].(*object.Pair).Right})
		}
		objectToChange := evalHashIndexExpression(args[0], index.(*object.List).Elements[0], tok)
		if objectToChange.Type() == "error" {
			return objectToChange
		}
		if objectToChange.Type() == "list" {
			return unsafeAddPairToMap(tok, args[0], args[1], &object.Pair{
				Left: index.(*object.List).Elements[0],
				Right: unsafeAddPairToList(tok, objectToChange, args[1], &object.Pair{
					Left:  &object.List{Elements: index.(*object.List).Elements[1:len(index.(*object.List).Elements)]},
					Right: args[2].(*object.Pair).Right})})
		}
		if objectToChange.Type() == "struct" {
			return addPairToMap(tok, args[0], args[1], &object.Pair{
				Left: index.(*object.List).Elements[0],
				Right: unsafeAddPairToStruct(tok, objectToChange, args[1], &object.Pair{
					Left:  &object.List{Elements: index.(*object.List).Elements[1:len(index.(*object.List).Elements)]},
					Right: args[2].(*object.Pair).Right})})
		}
		if objectToChange.Type() == "map" {
			return unsafeAddPairToMap(tok, args[0], args[1], &object.Pair{
				Left: index.(*object.List).Elements[0],
				Right: unsafeAddPairToMap(tok, objectToChange, args[1], &object.Pair{
					Left:  &object.List{Elements: index.(*object.List).Elements[1:len(index.(*object.List).Elements)]},
					Right: args[2].(*object.Pair).Right})})
		}
	}

	hashKey, ok := args[2].(*object.Pair).Left.(object.Hashable)
	if !ok {
		return newError("built/hash/a", tok, args[1])
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

func newError(ident string, tok token.Token, args ...any) *object.Error {
	errorToReturn := object.CreateErr(ident, tok, args...)
	errorToReturn.Trace = []token.Token{tok}
	return errorToReturn
}

func newErrorWithVals(ident string, tok token.Token, vals []object.Object, args ...any) *object.Error {
	errorToReturn := object.CreateErrWithVals(ident, tok, vals, args...)
	errorToReturn.Trace = []token.Token{tok}
	return errorToReturn
}
