package vm

import (
	"database/sql"
	"strconv"
	"strings"

	"github.com/tim-hardcastle/Pipefish/source/text"
	"github.com/tim-hardcastle/Pipefish/source/values"

	"src.elv.sh/pkg/persistent/vector"
)

// While 'database.go' contains the means for the hub to use the database for RBAM purposes,
// this file contains the means for Pipefish scripts to interact with the database.

// So when this works properly we can write that bit of the hub entirely in Pipefish and dispense with the
// 'database.go' file.

func (vm *Vm) evalPostSQL(db *sql.DB, query string, pfArgs []values.Value, tok uint32) values.Value {
	goArgs := pfToGoPointers(pfArgs)
	_, err := (db).Exec(query, goArgs...)
	if err != nil {
		return vm.makeError("vm/sql/get", tok, err.Error())
	}
	return values.Value{values.SUCCESSFUL_VALUE, nil}
}

func (vm *Vm) evalGetSQL(db *sql.DB, structTypeNumber values.ValueType, query string, pfArgs []values.Value, tok uint32) values.Value {
	goArgs := pfToGoPointers(pfArgs)
	rows, err := (db).Query(query, goArgs...)
	if err != nil {
		return values.Value{values.ERROR, vm.makeError("vm/sql/get", tok, err.Error())}
	}
	defer rows.Close()

	targetStrings := []string{} 
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
		sqlType := vm.getSqlType(v)
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

func (vm *Vm) getSqlType(pfType values.AbstractType) string {
	// TODO --- this could be attached to the abstract type informtion.
	switch {
	case pfType.Equals(values.AbstractType{[]values.ValueType{values.INT}}):
		return "INTEGER NOT NULL"
	case pfType.Equals((values.AbstractType{[]values.ValueType{values.NULL, values.INT}})):
		return "INTEGER"
	case pfType.Equals(values.AbstractType{[]values.ValueType{values.STRING}}):
		return "STRING NOT NULL"
	case pfType.Equals(values.AbstractType{[]values.ValueType{values.NULL, values.STRING}}):
		return "STRING"
	case pfType.Equals(values.AbstractType{[]values.ValueType{values.BOOL}}):
		return "BOOL NOT NULL"
	case pfType.Equals(values.AbstractType{[]values.ValueType{values.NULL, values.BOOL}}):
		return "BOOL"
	}
	// Now we have to do something kludgy to find out if it's a varchar, and indeed one of 
	// our varchars.
	thingToCheck := values.UNDEFINED_TYPE
	if pfType.Len() == 1 {
		thingToCheck = pfType.Types[0]
	}
	if pfType.Len() == 2 && pfType.Types[0] == values.NULL {
		thingToCheck = pfType.Types[1]
	}
	if thingToCheck == values.UNDEFINED_TYPE {
		return ""
	}
	info, ok := vm.ConcreteTypeInfo[thingToCheck].(CloneType)
	if !ok || !text.Head(info.Name, "Varchar{") || info.Parent != values.STRING || len(info.TypeArguments) != 1 ||
			info.TypeArguments[0].T != values.INT {
		return ""
	}
	vNo := info.TypeArguments[0].V.(int)
	if pfType.Len() == 1 {
		return "VARCHAR(" + strconv.Itoa(vNo) + ") NOT NULL"
	}
	return "VARCHAR(" + strconv.Itoa(vNo) + ")"
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
