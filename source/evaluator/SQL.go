package evaluator

import (
	"strconv"

	"pipefish/source/object"
	"pipefish/source/parser"
	"pipefish/source/signature"
	"pipefish/source/token"
)

// While 'database.go' contains the means for the hub to use the database for RBAM purposes,
// this file contains the means for Charm scripts to interact with the database.

// So when this works properly we can write that bit of the hub entirely in Charm and dispense with the
// 'database.go' file.

func evalPostSQL(params []object.Object, tok token.Token, c *Context) object.Object {

	query, args, charmError := parseSQL(params[0].(*object.Struct), tok, c)
	if charmError != nil {
		return charmError
	}
	_, err := (c.prsr.Database).Exec(query, args...)
	if err != nil {
		return newError("sql/out", tok, err.Error())
	}
	return object.SUCCESS
}

func evalGetSQL(params []object.Object, tok token.Token, c *Context) object.Object {
	charmType := params[0].(*object.Type)
	if !parser.IsSameTypeOrSubtype(c.prsr.TypeSystem, charmType.Value, "struct") {
		return newError("sql/in/type/a", tok, charmType.Value)
	}
	query, args, charmError := parseSQL(params[1].(*object.Struct), tok, c)
	if charmError != nil {
		return charmError
	}
	rows, err := (c.prsr.Database).Query(query, args...)

	if err != nil {
		return newError("sql/in/read", tok, err.Error())
	}
	defer rows.Close()

	results := []object.Object{}

	targetStrings := []string{}
	targetInts := []int{}
	targetBools := []bool{}
	pointerList := []any{}
	for _, v := range c.prsr.StructSig[charmType.Value] {
		if parser.TypeIsStringlike(v.VarType) {
			targetStrings = append(targetStrings, "")
			pointerList = append(pointerList, &targetStrings[len(targetStrings)-1])
		} else {
			switch v.VarType {
			case "int", "int?":
				targetInts = append(targetInts, 0)
				pointerList = append(pointerList, &targetInts[len(targetInts)-1])
			case "bool", "bool?":
				targetBools = append(targetBools, false)
				pointerList = append(pointerList, &targetBools[len(targetBools)-1])
			default:
				return newError("sql/in/type/b", tok, v.VarType)
			}
		}
	}

	for rows.Next() {
		if err := rows.Scan(pointerList...); err != nil {
			return newError("sql/in/scan", tok, err.Error())
		}
		newStruct := makeStruct(charmType.Value, pointerList, tok, c)
		if newStruct.Type() == object.ERROR_OBJ {
			return newStruct
		}
		results = append(results, newStruct)
	}
	return &object.List{Elements: results}
}

func makeStruct(structName string, args []any, tok token.Token, c *Context) object.Object {
	newStruct := &object.Struct{Name: structName, Labels: []string{}, Value: make(map[string]object.Object), Namespace: c.prsr.NamespacePath}
	for i, v := range c.prsr.StructSig[structName] {
		var charmValue object.Object
		if args[i] == nil {
			charmValue = object.NULL
		} else {
			switch goValue := args[i].(type) {
			case *string:
				charmValue = &object.String{Value: *goValue}
			case *int:
				charmValue = &object.Integer{Value: *goValue}
			case *bool:
				if *goValue {
					charmValue = object.TRUE
				} else {
					charmValue = object.FALSE
				}
			default:
				return newError("sql/conv", tok)
			}
		}
		newStruct.Labels = append(newStruct.Labels, v.VarName)
		newStruct.Value[v.VarName] = charmValue
	}
	return newStruct
}

func parseSQL(snippet *object.Struct, tok token.Token, c *Context) (string, []any, *object.Error) {
	args := []any{}
	if c.prsr.Database == nil {
		return "", args, newError("sql/exists", tok)
	}
	goEnv := makeGoEnvFromCharmMap(snippet.Value["env"].(*object.Hash))
	context := NewContext(c.prsr, goEnv, CMD, false)
	text := snippet.Value["text"]
	outputText := ""
	charmToEvaluate := ""
	dollarNumber := 1
	readState := ' '
	for _, ch := range text.(*object.String).Value {
		if readState == ' ' {
			if ch == '`' || ch == '"' || ch == '\'' || ch == '|' {
				readState = ch
			}
			if ch != '|' {
				outputText = outputText + string(ch)
			}
			continue
		}
		if readState == '|' && ch != '|' {
			charmToEvaluate = charmToEvaluate + string(ch)
			continue
		}
		// Or we have some Charm to parse.
		parsedCharm := c.prsr.ParseLine(tok.Source, charmToEvaluate)
		if c.prsr.ErrorsExist() {
			c.prsr.ClearErrors()
			return "", args, newError("sql/", tok, charmToEvaluate)
		}
		charmValue := Eval(*parsedCharm, context)
		switch charmValue := charmValue.(type) {
		case *object.Type: // Then if it's a struct type we convert the type definition into a SQL table signature.
			if !parser.TypeSystem.PointsTo(c.prsr.TypeSystem, charmValue.Value, "struct") {
				return "", args, newError("sql/struct", tok, charmToEvaluate)
			}
			sqlSig, ok := getSqlSig(c.prsr.StructSig[charmValue.Value])
			if !ok {
				return "", args, newError("sql/sig", tok, charmToEvaluate)
			}
			outputText = outputText + sqlSig
		case *object.Error:
			return "", args, charmValue
		case *object.Tuple:
			outputText = outputText + "("
			for i := 0; i < len(charmValue.Elements); i++ {
				outputText = outputText + "$" + strconv.Itoa(dollarNumber)
				if i < len(charmValue.Elements)-1 {
					outputText = outputText + ", "
				}
				goValue := getGoValue(charmValue.Elements[i])
				if goValue == nil {
					return "", args, newError("sql/parse/type/a", tok)
				}
				args = append(args, goValue)
				dollarNumber++
			}
			outputText = outputText + ")"
		default:
			goValue := getGoValue(charmValue)
			if goValue == nil {
				return "", args, newError("sql/parse/type/b", tok)
			}
			outputText = outputText + "($" + strconv.Itoa(dollarNumber) + ")"
			args = append(args, goValue)
			dollarNumber++
		}
	}
	return outputText, args, nil
}

func getSqlSig(charmSig signature.Signature) (string, bool) {
	output := "("
	for i, v := range charmSig {
		sqlType := getSqlType(v.VarType)
		if sqlType == "" {
			return "", false
		}
		output = output + v.VarName + " " + sqlType
		if i < len(charmSig)-1 {
			output = output + ", "
		}
	}
	output = output + ")"
	return output, true
}

var SQL_TYPE_MAP = map[string]string{"int": "INTEGER NOT NULL", "string": "TEXT NOT NULL", "bool": "BOOLEAN NOT NULL",
	"int?": "INTEGER", "string?": "TEXT", "bool?": "BOOLEAN"}

func getSqlType(charmType string) string {
	nullable := parser.GetNullabilityFromType(charmType)
	if parser.TypeIsStringlike(charmType) {
		strLen, isVarchar := parser.GetLengthFromType(charmType)
		if isVarchar {
			result := "VARCHAR" + "(" + strconv.Itoa(strLen) + ")"
			if !nullable {
				result = result + " NOT NULL"
			}
			return result
		}
	}
	sqlType, ok := SQL_TYPE_MAP[charmType]
	if ok {
		return sqlType
	}
	return ""
}

func getGoValue(charmValue object.Object) any {
	switch charmValue := charmValue.(type) {
	case *object.String:
		return charmValue.Value
	case *object.Integer:
		return charmValue.Value
	case *object.Boolean:
		return charmValue.Value
	default:
		return nil
	}
}

func makeGoEnvFromCharmMap(h *object.Hash) *object.Environment {
	newEnv := object.NewEnvironment()
	for _, v := range h.Pairs {
		newEnv.InitializeConstant(v.Key.(*object.String).Value, v.Value)
	}
	return newEnv
}
