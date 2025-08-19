package vm

import (
	"database/sql"
	"reflect"
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
	goArgs, pfErr := vm.pfValuesToGoPtrValues(pfArgs, tok)
	if pfErr.T == values.ERROR {
		return pfErr
	}
	_, err := (db).Exec(query, goArgs...)
	if err != nil {
		println(query, err.Error())
		return vm.makeError("vm/sql/post", tok, err.Error())
	}
	return values.Value{values.SUCCESSFUL_VALUE, nil}
}

func (vm *Vm) evalGetSQL(db *sql.DB, structTypeNumber values.ValueType, query string, pfArgs []values.Value, tok uint32) values.Value {
	goArgs, pfErr := vm.pfValuesToGoPtrValues(pfArgs, tok)
	if pfErr.T == values.ERROR {
		return pfErr
	}
	rows, err := (db).Query(query, goArgs...)
	if err != nil {
		return vm.makeError("vm/sql/get", tok, err.Error())
	}
	defer rows.Close()

	pointerList, errorOrOK := vm.getPointers(values.MakeAbstractType(structTypeNumber), tok)
	if errorOrOK.T == values.ERROR {
		return errorOrOK
	}

	vec := vector.Empty
	for rows.Next() {
		if err := rows.Scan(pointerList...); err != nil {
			return vm.makeError("vm/sql/scan", tok, err.Error())
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
					return vm.makeError("vm/sql/goval", tok, reflect.TypeOf(goValue).String())
				}
			}
			fields = append(fields, pfVal)
		}
		vec = vec.Conj(values.Value{structTypeNumber, fields})
	}
	return values.Value{values.LIST, vec}
}

// Given a Pipefish type, we want to convert it into a flat list of pointers that we can try to cast
// a SQL row to.
func (vm *Vm) getPointers(abType values.AbstractType, tok uint32) ([]any, values.Value) {
	concreteType := values.ERROR
	switch abType.Len() {
	case 1 :
		concreteType = abType.Types[0]
	case 2 : 
		if abType.Types[0] == values.NULL {
			concreteType = abType.Types[1]
			break
		}
		fallthrough
	default :
		return nil, vm.makeError("vm/sql/abstract", tok, vm.DescribeAbstractType(abType, LITERAL))
	}
	info := vm.ConcreteTypeInfo[concreteType]
	// We special-case the built-in parameterized 'pair' type.
	if info, ok := vm.ConcreteTypeInfo[concreteType].(CloneType); ok && 
	        text.Head(info.Name, "pair{") && info.Parent == values.PAIR && 
			len(info.TypeArguments) == 2 && info.TypeArguments[0].T == values.TYPE && 
			info.TypeArguments[1].T == values.TYPE {
		result := []any{}
		for _, ty := range info.TypeArguments {
			ptrs, err := vm.getPointers(ty.V.(values.AbstractType), tok)
			if err.T == values.ERROR {
				return nil, err
			}
			result = append(result, ptrs...)
		}
		return result, values.OK
	}
	baseType := concreteType
	baseInfo := info
	if info, ok := info.(CloneType); ok {
		baseInfo = vm.ConcreteTypeInfo[info.Parent].(BuiltinType)
		baseType = info.Parent
	}
	switch baseInfo := baseInfo.(type) {
	case BuiltinType:
		switch baseType {
		case values.BOOL:
			var b bool
			return []any{&b}, values.OK
		case values.INT:
			var i int 
			return []any{&i}, values.OK
		case values.STRING:
			var s string 
			return []any{&s}, values.OK
		}
	case StructType:
		pointerList := []any{}
		for _, fieldType := range baseInfo.AbstractStructFields {
			pointers, err := vm.getPointers(fieldType, tok)
			if err.T == values.ERROR {
				return nil, err
			}
			pointerList = append(pointerList, pointers...)
		}
		return pointerList, values.OK
	case EnumType:
		var i int 
		return []any{&i}, values.OK
	}
	return nil, vm.makeError("vm/sql/type", tok, vm.DescribeAbstractType(abType, LITERAL))
}

// We want a struct type to be turned into the sig of an appropriate table. The entry point must
// be a struct type because the table needs names for the columns; and for the same reason the 
// fields of the struct must be either base types, clones of base types, their ullable twins, or 
// structs but *not* their nulllable twins because what would that even mean?
//
// We then join the results together and put parentheses around them before returning.
func (vm *Vm) getTableSigFromStructType(concreteType values.ValueType, tok uint32) (string, values.Value) { // As usual in this part of the program, the value is an ERROR or OK.
	if !vm.ConcreteTypeInfo[concreteType].IsStruct() {
		return "", vm.makeError("vm/sql/sig/b", tok, vm.DescribeType(concreteType, LITERAL))
	}
	sig, err := vm.getSQLSigFromStructType(concreteType, tok)
	if err.T == values.ERROR {
		return "", err
	}
	var buf strings.Builder
	buf.WriteString("(")
	buf.WriteString(strings.Join(sig, ", "))
	buf.WriteString(")")
	return buf.String(), values.OK
}

func (vm *Vm) getSQLSigFromStructType(concreteType values.ValueType, tok uint32) ([]string, values.Value) { // As usual in this part of the program, the value is an ERROR or OK.
	result := []string{} // We return a list of names seperated from types by spaces.
	info := vm.ConcreteTypeInfo[concreteType].(StructType)
	for i, labelNo := range(info.LabelNumbers) {
		abType := info.AbstractStructFields[i]
		if abType.Len() == 1 && vm.ConcreteTypeInfo[abType.Types[0]].IsStruct() {
			bitOfSig, err := vm.getSQLSigFromStructType(abType.Types[0], tok)
			if err.T == values.ERROR {
				return nil, err
			}
			result = append(result, bitOfSig...)
			continue
		}
		sqlType, err := vm.getSQLType(abType, tok)
		if err.T == values.ERROR {
			return nil, err
		}
		result = append(result, vm.Labels[labelNo] + " " + sqlType)
	}
	return result, values.OK
}

func (vm *Vm) getSQLType(abType values.AbstractType, tok uint32) (string, values.Value) { // As usual in this part of the program, the value is an ERROR or OK.
	nulled := false    // Whether the abstract type contains NULL.
	concreteType := values.ERROR
	switch abType.Len() {
	case 1 :
		concreteType = abType.Types[0]
	case 2 : 
		nulled = abType.Types[0] == values.NULL
		if nulled {
			concreteType = abType.Types[1]
			break
		}
		fallthrough
	default :
		return "", vm.makeError("vm/sql/abstract", tok, vm.DescribeAbstractType(abType, LITERAL))
	}
	info := vm.ConcreteTypeInfo[concreteType]
	// We special-case the varchars.
    if info, ok := vm.ConcreteTypeInfo[concreteType].(CloneType); ok && 
	        text.Head(info.Name, "Varchar{") && info.Parent == values.STRING && 
			len(info.TypeArguments) == 1 && info.TypeArguments[0].T == values.INT {
		vNo := info.TypeArguments[0].V.(int)
		if nulled {
			return "VARCHAR(" + strconv.Itoa(vNo) + ")", values.OK
		}
		return "VARCHAR(" + strconv.Itoa(vNo) + ") NOT NULL", values.OK
	}
	// Otherwise it's not a varchar and we move on.
	plainSQLType := "" // What we set it to before adding (or not adding) NOT NULL.
	baseType := concreteType
	if info, ok := info.(CloneType); ok {
		baseType = info.Parent
	}
	baseInfo := vm.ConcreteTypeInfo[baseType]
	// Now clone types will be treated like their base types, 
	switch baseInfo.(type) {
	case BuiltinType:
		switch baseType {
		case values.BOOL :
			plainSQLType = "BOOL"
		case values.INT :
			plainSQLType = "INTEGER"
		case values.STRING :
			plainSQLType = "TEXT"
		}
	case EnumType:
		plainSQLType = "INTEGER"
	}
	if plainSQLType == "" {
		return "", vm.makeError("sql/sig", tok, vm.DescribeAbstractType(abType, LITERAL))
	}
	if !nulled {
		return plainSQLType + " NOT NULL", values.OK
	}
	return plainSQLType, values.OK
}

// This takes a Pipefish value and turns it into a flattened list of pointers to Go values of the
// appropriate type.
func (vm *Vm) pfValueToGoPtrValues(v values.Value, tok uint32) ([]any, values.Value){ // The value being OK or an ERROR.
	switch typeInfo := vm.ConcreteTypeInfo[v.T].(type) {
	case BuiltinType:
		switch v.T {
		case values.BOOL:
			b := v.V.(bool)
			return []any{&b}, values.OK
		case values.INT:
			i := v.V.(int)
			return []any{&i}, values.OK
		case values.NULL:
			return []any{nil}, values.OK
		case values.STRING:
			i := v.V.(string)
			return []any{&i}, values.OK
		case values.TUPLE:
			return vm.pfValuesToGoPtrValues(v.V.([]values.Value), tok)
		default:
			return nil, vm.makeError("vm/conv/type", tok)
		}
	case CloneType:
		switch typeInfo.Parent {
		case values.BOOL:
			b := v.V.(bool)
			return []any{&b}, values.OK
		case values.INT:
			i := v.V.(int)
			return []any{&i}, values.OK
		case values.STRING:
			i := v.V.(string)
			return []any{&i}, values.OK
		case values.PAIR:
			// We check that this is the built-in parameterized 'pair' type.
			if text.Head(typeInfo.Name, "pair{") && 
					len(typeInfo.TypeArguments) == 2 && typeInfo.TypeArguments[0].T == values.TYPE && 
					typeInfo.TypeArguments[1].T == values.TYPE {
				return vm.pfValuesToGoPtrValues(v.V.([]values.Value), tok)
			}
		}
	case EnumType:
		i := v.V.(int)
		return []any{&i}, values.OK
	case StructType:
		return vm.pfValuesToGoPtrValues(v.V.([]values.Value), tok)
	}
	return nil, vm.makeError("vm/go/type", tok, vm.DescribeType(v.T, LITERAL))
} 

// This calls the preceding function on a list of Pipefish values such as the payload of a tuple
// or struc.
func (vm *Vm) pfValuesToGoPtrValues(vs []values.Value, tok uint32) ([]any, values.Value) { // The value being OK or an ERROR.
	result := []any{}
	for _, v := range vs {
		newGo, err := vm.pfValueToGoPtrValues(v, tok)
		if err.T == values.ERROR {
			return nil, err
		}
		result = append(result, newGo...)
	}
	return result, values.OK
}