package parser

import (
	"charm/source/object"
	"charm/source/text"

	"bytes"
	"fmt"
	"strings"
)

type Style int

const (
	PLAIN Style = iota
	LITERAL
)

func (p *Parser) Serialize(ob object.Object, style Style) string {
	switch ob := ob.(type) {
	case *object.Boolean:
		return fmt.Sprintf("%t", ob.Value)
	case *object.Effects:
		return ""
	case *object.Error:
		if style == PLAIN {
			if len(ob.Trace) == 0 {
				return text.ERROR + ob.Message + text.DescribePos(ob.Token)
			} else {
				return text.RT_ERROR + ob.Message + text.DescribePos(ob.Token)
			}
		}
		return "error " + text.ToEscapedText(ob.Message)
	case *object.Float:
		return fmt.Sprintf("%f", ob.Value)
	case *object.Func:
		result := "func " + ob.Sig.String() + " : " + ob.Body.String()
		if ob.Given != nil {
			result = "(" + result + ")" + " given : " + "(" + ob.Given.String() + ")"
		}
		return result
	case *object.Hash:
		var out bytes.Buffer
		pairs := []string{}
		out.WriteString("map(")
		for _, pair := range ob.Pairs {
			pairs = append(pairs, fmt.Sprintf("%s::%s",
				p.Serialize(pair.Key, style), p.Serialize(pair.Key, style)))
		}

		out.WriteString(strings.Join(pairs, ", "))

		out.WriteString(")")

		return out.String()
	case *object.Integer:
		return fmt.Sprintf("%d", ob.Value)
	case *object.Label:
		return ob.Value
	case *object.Lazy:
		return ob.Value.String() // TODO : find out what this does and make it do something else, probably? It doesn't look right.
	case *object.List:
		var out bytes.Buffer
		elements := []string{}
		for _, element := range ob.Elements {
			elements = append(elements, p.Serialize(element, style))
		}
		out.WriteString("[")
		out.WriteString(strings.Join(elements, ", "))
		out.WriteString("]")
		return out.String()
	case *object.Null:
		return "NULL"
	case *object.Pair:
		return fmt.Sprintf("%s::%s", p.Serialize(ob.Left, style), p.Serialize(ob.Right, style))
	case *object.OuterFunc:
		return "<Unserializable outer function>" // TODO --- is it really?
	case *object.Ref:
		return "<Unserializable refrence variable>" // TODO --- is it really?
	case *object.Set:
		var out bytes.Buffer
		elements := []string{}
		for _, element := range ob.Elements {
			elements = append(elements, p.Serialize(element, style))
		}
		out.WriteString("set (")
		out.WriteString(strings.Join(elements, ", "))
		out.WriteString(")")
		return out.String()
	case *object.String:
		if style == PLAIN {
			return ob.Value
		}
		return text.ToEscapedText(ob.Value)
	case *object.Struct:
		var out bytes.Buffer
		elements := []string{}
		for _, element := range ob.Labels {
			elements = append(elements, element+"::"+p.Serialize(ob.Value[element], style))
		}
		out.WriteString(ob.Name)
		out.WriteString(" with ")
		out.WriteString("(")
		out.WriteString(strings.Join(elements, ", "))
		out.WriteString(")")
		return out.String()
	case *object.SuccessfulAssignment:
		if style == LITERAL {
			return "ok"
		}
		return text.OK
	case *object.Tuple:
		var out bytes.Buffer
		elements := []string{}
		if len(ob.Elements) == 1 {
			out.WriteString("tuple ")
		}
		if len(ob.Elements) <= 1 {
			out.WriteString("(")
		}
		for _, element := range ob.Elements {
			elements = append(elements, p.Serialize(element, style))
		}
		out.WriteString(strings.Join(elements, ", "))
		if len(ob.Elements) <= 1 {
			out.WriteString(")")
		}
		return out.String()
	case *object.Type:
		return ob.Value
	case *object.UnsatisfiedConditional:
		return "<unsatisfied conditional>"

	}
	return "<unexpected serialization error>"
}
