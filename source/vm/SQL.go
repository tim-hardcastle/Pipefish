package vm

import (
	"strconv"
	"strings"

	"github.com/tim-hardcastle/Pipefish/source/values"

	"src.elv.sh/pkg/persistent/vector"
)

// While 'database.go' contains the means for the hub to use the database for RBAM purposes,
// this file contains the means for Charm scripts to interact with the database.

// So when this works properly we can write that bit of the hub entirely in Charm and dispense with the
// 'database.go' file.

func (vm *Vm) evalPostSQL(query string, pfArgs []values.Value) values.Value {
	goArgs := pfToGoPointers(pfArgs)

	_, err := (vm.Database).Exec(query, goArgs...)
	if err != nil {
		return values.Value{values.ERROR, nil}
	}
	return values.Value{values.SUCCESSFUL_VALUE, nil}
}

func (vm *Vm) evalGetSQL(structTypeNumber values.ValueType, query string, pfArgs []values.Value) values.Value {
	goArgs := pfToGoPointers(pfArgs)
	rows, err := (vm.Database).Query(query, goArgs...)
	if err != nil {
		return values.Value{values.ERROR, nil}
	}
	defer rows.Close()

	targetStrings := []string{} // TODO --- can we set this up on initialization?
	targetInts := []int{}
	targetBools := []bool{}
	pointerList := []any{}
	for _, v := range vm.ConcreteTypeInfo[structTypeNumber].(StructType).AbstractStructFields {
		switch {
		case v.Contains(values.INT):
			targetInts = append(targetInts, 0)
			pointerList = append(pointerList, &targetInts[len(targetInts)-1])
		case v.Contains(values.BOOL):
			targetBools = append(targetBools, false)
			pointerList = append(pointerList, &targetBools[len(targetBools)-1])
		case v.Contains(values.STRING):
			targetStrings = append(targetStrings, "")
			pointerList = append(pointerList, &targetStrings[len(targetStrings)-1])
		default:
			return values.Value{values.ERROR, nil}
		}
	}

	vec := vector.Empty
	for rows.Next() {
		if err := rows.Scan(pointerList...); err != nil {
			return values.Value{values.ERROR, nil}
		}
		fields := make([]values.Value, 0, len(pointerList))
		for _, p := range pointerList {
			var pfVal values.Value
			if p == nil {
				pfVal = values.Value{values.NULL, nil}
			} else {
				switch goValue := p.(type) {
				case *string:
					pfVal = values.Value{values.STRING, *goValue}
				case *int:
					pfVal = values.Value{values.INT, *goValue}
				case *bool:
					pfVal = values.Value{values.BOOL, *goValue}
				default:
					return values.Value{values.ERROR, nil}
				}
			}
			fields = append(fields, pfVal)
		}
		vec = vec.Conj(values.Value{structTypeNumber, fields})
	}
	return values.Value{values.LIST, vec}
}

func (vm *Vm) GetSqlSig(pfStructType values.ValueType) (string, bool) {
	var buf strings.Builder
	buf.WriteString("(")
	sep := ""
	for i, v := range vm.ConcreteTypeInfo[pfStructType].(StructType).AbstractStructFields {
		sqlType := getSqlType(v)
		if sqlType == "" {
			return "", false
		}
		buf.WriteString(sep)
		buf.WriteString(vm.Labels[vm.ConcreteTypeInfo[pfStructType].(StructType).LabelNumbers[i]])
		buf.WriteString(" ")
		buf.WriteString(sqlType)
		sep = ", "
	}
	buf.WriteString(")")
	return buf.String(), true
}

func getSqlType(pfType values.AbstractType) string {
	switch {
	case pfType.Equals(values.AbstractType{[]values.ValueType{values.INT}, 0}):
		return "INTEGER NOT NULL"
	case pfType.Equals((values.AbstractType{[]values.ValueType{values.NULL, values.INT}, 0})):
		return "INTEGER"
	case pfType.Equals(values.AbstractType{[]values.ValueType{values.STRING}, DUMMY}):
		return "STRING NOT NULL"
	case pfType.Equals(values.AbstractType{[]values.ValueType{values.NULL, values.STRING}, DUMMY}):
		return "STRING"
	case pfType.IsVarchar():
		return "VARCHAR(" + strconv.Itoa(int(pfType.Varchar)) + ") NOT NULL"
	case pfType.IsVarcharOrNull():
		return "VARCHAR(" + strconv.Itoa(int(pfType.Varchar)) + ")"
	case pfType.Equals(values.AbstractType{[]values.ValueType{values.BOOL}, 0}):
		return "BOOL NOT NULL"
	case pfType.Equals(values.AbstractType{[]values.ValueType{values.NULL, values.BOOL}, 0}):
		return "BOOL"
	default:
		return ""
	}
}

func pfToGoPointers(pfValues []values.Value) []any {
	goValues := make([]any, 0, len(pfValues))
	for _, pfV := range pfValues {
		result := getGoValue(pfV)
		if result == nil {
			goValues = append(goValues, nil)
		} else {
			goValues = append(goValues, &result)
		}
	}
	return goValues
}

func getGoValue(pfValue values.Value) any {
	switch pfValue.T {
	case values.NULL:
		return nil
	case values.STRING:
		return pfValue.V.(string)
	case values.INT:
		return pfValue.V.(int)
	case values.BOOL:
		return pfValue.V.(bool)
	default:
		return nil
	}
}
