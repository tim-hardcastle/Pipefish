package parser

import (
	"bytes"
	"reflect"
	"strconv"

	"pipefish/source/ast"
	"pipefish/source/token"
)

type printFlavor int

const (
	ppOUTER printFlavor = iota
	ppINLINE
)

type printContext struct {
	indent      string
	flavor      printFlavor
	mustBracket bool
}

var inlineCtxt = printContext{"", ppINLINE, false}
var prefixCtxt = printContext{"", ppINLINE, true}

func (ctxt printContext) in() printContext {
	ctxt.indent = ctxt.indent + "    "
	return ctxt
}

func (p *Parser) PrettyPrint(node ast.Node) string {
	return p.prettyPrint(node, printContext{"", ppOUTER, false})
}

func (p *Parser) prettyPrint(node ast.Node, ctxt printContext) string {
	var out bytes.Buffer
	out.WriteString(ctxt.indent)
	switch node := node.(type) {
	case *ast.AssignmentExpression:
		out.WriteString(p.prettyPrint(node.Left, inlineCtxt))
		out.WriteString(" = ")
		out.WriteString(p.prettyPrint(node.Right, inlineCtxt))
	case *ast.Bling:
		out.WriteString(node.Value)
	case *ast.BooleanLiteral:
		out.WriteString(node.Token.Literal)
	case *ast.BuiltInExpression:
		panic("Found secret 'builtin' keyword in prettyprinter")
	case *ast.FloatLiteral:
		out.WriteString(node.Token.Literal)
	case *ast.ForExpression:
		if node.BoundVariables != nil {
			out.WriteString("from ")
			out.WriteString(p.prettyPrint(node.BoundVariables, inlineCtxt))
			out.WriteString(" ")
		}
		out.WriteString("for ")
		if node.Initializer != nil {
			out.WriteString(p.prettyPrint(node.Initializer, inlineCtxt))
			out.WriteString("; ")
		}
		if node.ConditionOrRange != nil {
			out.WriteString(p.prettyPrint(node.ConditionOrRange, inlineCtxt))
			if node.Update != nil {
				out.WriteString("; ")
			}
		}
		if node.Update != nil {
			out.WriteString(p.prettyPrint(node.Update, inlineCtxt))
		}
		out.WriteString(" : ")
		switch ctxt.flavor {
		case ppOUTER:
			out.WriteString("\n")
			out.WriteString(p.prettyPrint(node.Body, ctxt.in()))
		case ppINLINE:
			out.WriteString(p.prettyPrint(node.Body, inlineCtxt))
		}
	case *ast.FuncExpression:
		out.WriteString("func ")
		out.WriteString(node.Sig.String() + " : ")
		switch ctxt.flavor {
		case ppOUTER:
			out.WriteString("\n")
			out.WriteString(p.prettyPrint(node.Body, ctxt.in()))
		case ppINLINE:
			out.WriteString(p.prettyPrint(node.Body, inlineCtxt))
		}
		if node.Given != nil {
			switch ctxt.flavor {
			case ppOUTER:
				out.WriteString("given :\n")
				out.WriteString(p.prettyPrint(node.Body, ctxt.in()))
				out.WriteString("\n")
			case ppINLINE:
				out.WriteString(p.prettyPrint(node.Body, inlineCtxt))
			}
		}
	case *ast.GolangExpression:
		panic("Found golang expression in prettyprinter.")
	case *ast.Identifier:
		out.WriteString(node.Value)
	case *ast.IndexExpression:
		if !isLeaf(node.Left) {
			out.WriteString("(")
		}
		out.WriteString(p.prettyPrint(node.Left, inlineCtxt))
		if !isLeaf(node.Left) {
			out.WriteString(")")
		}
		out.WriteString("[")
		out.WriteString(p.prettyPrint(node.Index, inlineCtxt))
		out.WriteString("]")
	case *ast.InfixExpression:
		pos := 1
		if len(node.Args) != 3 {
			for ; pos <= len(node.Args); pos++ {
				if blingNode, ok := node.Args[pos].(*ast.Bling); ok && blingNode.Value == node.Operator { // TODO --- record this in ast?
					break
				}
			}
		}
		leftNeedsBrackets := pos > 1 || p.hasLowerPrecedence(node.Args[0], node.Args[1]) && !isLeaf(node.Args[0])
		rightNeedsBrackets := (len(node.Args)-pos) > 2 || p.hasHigherOrEqualPrecedence(node.Args[pos], node.Args[pos+1]) && !isLeaf(node.Args[pos+1])
		if leftNeedsBrackets {
			out.WriteString("(")
		}
		sep := ""
		for i := 0; i < pos; i++ {
			out.WriteString(sep)
			out.WriteString(p.prettyPrint(node.Args[i], inlineCtxt))
			sep = ", "
		}
		if leftNeedsBrackets {
			out.WriteString(")")
		}
		out.WriteString(" ")
		out.WriteString(node.Operator)
		out.WriteString(" ")
		if rightNeedsBrackets {
			out.WriteString("(")
		}
		for i := pos + 1; i < len(node.Args); i++ {
			if ast.IsBling(node.Args[i-1]) || ast.IsBling(node.Args[i]) {
				if i != pos+1 {
					out.WriteString(" ")
				}
			} else {
				out.WriteString(", ")
			}
			out.WriteString(p.prettyPrint(node.Args[i], inlineCtxt))
		}
		if rightNeedsBrackets {
			out.WriteString(")")
		}
	case *ast.IntegerLiteral:
		out.WriteString(node.Token.Literal)
	case *ast.LazyInfixExpression:
		if node.Operator == "and" || node.Operator == "or" || ctxt.flavor == ppINLINE {
			if p.hasLowerPrecedence(node.Left, node) && !isLeaf(node.Left) {
				out.WriteString("(")
			}
			out.WriteString(p.prettyPrint(node.Left, inlineCtxt))
			if p.hasLowerPrecedence(node.Left, node) && !isLeaf(node.Left) {
				out.WriteString(")")
			}
			out.WriteString(" ")
			out.WriteString(node.Operator)
			out.WriteString(" ")
			if p.hasHigherOrEqualPrecedence(node, node.Right) && !isLeaf(node.Right) {
				out.WriteString("(")
			}
			out.WriteString(p.prettyPrint(node.Right, inlineCtxt))
			if p.hasHigherOrEqualPrecedence(node, node.Right) && !isLeaf(node.Right) {
				out.WriteString(")")
			}
		} else {
			if node.Token.Type == token.SEMICOLON {
				out.WriteString(p.prettyPrint(node.Left, ctxt))
				out.WriteString("\n")
				out.WriteString(p.prettyPrint(node.Right, ctxt))
			}
			if node.Token.Type == token.COLON {
				out.WriteString(p.prettyPrint(node.Left, inlineCtxt))
				out.WriteString(" : \n")
				out.WriteString(p.prettyPrint(node.Right, ctxt.in()))
			}
		}
	case *ast.ListExpression:
		out.WriteString("[")
		out.WriteString(p.prettyPrint(node.List, inlineCtxt))
		out.WriteString("]")
	case *ast.LogExpression:
		panic("Found log statement in prettyprinter.")
	case *ast.Nothing:
		out.WriteString("()")
	case *ast.PipingExpression:
		if p.hasLowerPrecedence(node.Left, node) && !isLeaf(node.Left) {
			out.WriteString("(")
		}
		out.WriteString(p.prettyPrint(node.Left, inlineCtxt))
		if p.hasLowerPrecedence(node.Left, node) && !isLeaf(node.Left) {
			out.WriteString(")")
		}
		out.WriteString(" ")
		out.WriteString(node.Operator)
		out.WriteString(" ")
		if p.hasHigherOrEqualPrecedence(node, node.Right) && !isLeaf(node.Right) {
			out.WriteString("(")
		}
		out.WriteString(p.prettyPrint(node.Right, inlineCtxt))
		if p.hasHigherOrEqualPrecedence(node, node.Right) && !isLeaf(node.Right) {
			out.WriteString(")")
		}
	case *ast.PrefixExpression:
		out.WriteString(node.Operator)
		if ctxt.mustBracket {
			out.WriteString("(")
		} else {
			out.WriteString(" ")
		}
		for i, arg := range node.Args {
			if i == 0 {
				if len(node.Args) > 1 {
					out.WriteString(p.prettyPrint(arg, prefixCtxt))
				} else {
					out.WriteString(p.prettyPrint(arg, inlineCtxt))
				}
				continue
			}
			if ast.IsBling(arg) {
				if ctxt.mustBracket && !ast.IsBling(node.Args[i-1]) {
					out.WriteString(")")
				}
				out.WriteString(" ")
				out.WriteString(p.prettyPrint(arg, inlineCtxt))
				out.WriteString(" ")
				if ctxt.mustBracket && i+1 < len(node.Args) && !ast.IsBling(node.Args[i+1]) {
					out.WriteString("(")
				}
			} else {
				if !ast.IsBling(node.Args[i-1]) {
					out.WriteString(", ")
				}
				if i+1 < len(node.Args) {
					out.WriteString(p.prettyPrint(arg, prefixCtxt))
				} else {
					out.WriteString(p.prettyPrint(arg, inlineCtxt))
				}
				if ctxt.mustBracket {
					out.WriteString(")")
				}
			}
		}
	case *ast.RuneLiteral:
		out.WriteString(strconv.QuoteRune(node.Value))
	case *ast.StringLiteral:
		out.WriteString(strconv.Quote(node.Value))
	case *ast.StructExpression:
		panic("Found struct definition in prettyprinter.")
	case *ast.SuffixExpression:
		if len(node.Args) > 1 || !isLeaf(node.Args[0]) {
			out.WriteString("(")
		}
		sep := ""
		for i := 0; i < len(node.Args); i++ {
			out.WriteString(sep)
			out.WriteString(p.prettyPrint(node.Args[i], inlineCtxt))
			sep = ", "
		}
		if len(node.Args) > 1 || !isLeaf(node.Args[0]) {
			out.WriteString(")")
		} else {
			out.WriteString(" ")
		}
		out.WriteString(node.Operator)
	case *ast.TryExpression:
		out.WriteString("try ")
		if node.VarName != "" {
			out.WriteString(node.VarName)
			out.WriteString(" ")
		}
		out.WriteString(": ")
		switch ctxt.flavor {
		case ppOUTER:
			out.WriteString("\n")
			out.WriteString(p.prettyPrint(node.Right, ctxt.in()))
		case ppINLINE:
			out.WriteString(p.prettyPrint(node.Right, inlineCtxt))
		}
	case *ast.UnfixExpression:
		out.WriteString(node.Operator)
	default:
		panic("Unhandled case in prettyprint: " + reflect.TypeOf(node).String())
	}
	return out.String()
}

func (p *Parser) hasLowerPrecedence(nodeA, nodeB ast.Node) bool {
	return p.leftPrecedence(*nodeA.GetToken()) < p.rightPrecedence(*nodeB.GetToken())
}

func (p *Parser) hasHigherOrEqualPrecedence(nodeA, nodeB ast.Node) bool {
	return p.leftPrecedence(*nodeA.GetToken()) >= p.rightPrecedence(*nodeB.GetToken())
}

func isLeaf(node ast.Node) bool {
	return len(node.Children()) == 0
}
