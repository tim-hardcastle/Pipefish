package initializer

import (
	"pipefish/source/ast"
	"pipefish/source/dtypes"
	"pipefish/source/settings"
	"pipefish/source/text"
	"pipefish/source/token"
	"pipefish/source/values"
)

// A miscellaneous collection of functions for extracting data from other data.

func (iz *initializer) getPartsOfImportOrExternalDeclaration(imp ast.Node) (string, string) {
	namespace := ""
	scriptFilepath := ""
	switch imp := (imp).(type) {
	case *ast.StringLiteral:
		scriptFilepath = imp.Value
		if settings.StandardLibraries.Contains(scriptFilepath) {
			scriptFilepath = settings.PipefishHomeDirectory + "lib/" + scriptFilepath + ".pf"
		}
		namespace = text.ExtractFileName(scriptFilepath)
		return namespace, scriptFilepath
	case *ast.Identifier:
		namespace = imp.Value
		return namespace, scriptFilepath
	case *ast.InfixExpression:
		if imp.GetToken().Literal != "::" {
			iz.Throw("init/import/infix", imp.GetToken())
		}
		lhs := imp.Args[0]
		rhs := imp.Args[2]
		switch rhs := rhs.(type) {
		case *ast.StringLiteral:
			scriptFilepath = rhs.Value
			if settings.StandardLibraries.Contains(scriptFilepath) {
				namespace = scriptFilepath
				scriptFilepath = settings.PipefishHomeDirectory + "lib/" + scriptFilepath + ".pf"
			}
			switch lhs := lhs.(type) {
			case *ast.Identifier:
				if lhs.Value != "NULL" {
					namespace = lhs.Value
				} else {
					namespace = ""
				}
				return namespace, scriptFilepath
			default:
				iz.Throw("init/import/ident", lhs.GetToken())
			}
		default:
			iz.Throw("init/import/string", lhs.GetToken())
		}
	}
	iz.Throw("init/import/weird", imp.GetToken())
	return "", ""
}

func (iz *initializer) getMatches(sigToMatch fnSigInfo, fnToTry *ast.PrsrFunction, tok *token.Token) values.AbstractType {
	result := values.MakeAbstractType()
	if sigToMatch.sig.Len() != len(fnToTry.Sig) {
		return result
	}
	if sigToMatch.rtnSig.Len() != 0 && sigToMatch.rtnSig.Len() != len(fnToTry.RtnSig) {
		return result
	}
	foundSelf := false
	for i := 0; i < len(sigToMatch.sig); i++ {
		if sigToMatch.sig.GetVarType(i).(string) == "self" {
			if foundSelf {
				result = result.Intersect(fnToTry.Sig[i].VarType)
				if len(result.Types) == 0 {
					break
				}
			} else {
				foundSelf = true
				result = fnToTry.Sig[i].VarType
			}
		} else {
			if !iz.p.GetAbstractType(sigToMatch.sig.GetVarType(i).(string)).IsSubtypeOf(fnToTry.Sig[i].VarType) ||
				sigToMatch.sig.GetVarType(i).(string) == "bling" && sigToMatch.sig.GetVarName(i) != fnToTry.Sig[i].VarName {
				return values.MakeAbstractType()
			}
		}
	}
	if !foundSelf {
		iz.p.Throw("init/interface/self", tok)
		return values.MakeAbstractType()
	}
	for i := 0; i < sigToMatch.rtnSig.Len(); i++ {
		if sigToMatch.rtnSig[i].VarType == "self" {
			result = result.Intersect(fnToTry.RtnSig[i].VarType)
		} else {
			if !fnToTry.RtnSig[i].VarType.IsSubtypeOf(iz.p.GetAbstractType(sigToMatch.rtnSig[i].VarType)) {
				return values.MakeAbstractType()
			}
		}
	}
	return result
}

// This is a fairly crude way of slurping the names of functions, commands, constants, and variables out of a declaration.
// It is crude in that it will slurp other things too: type names, for example; bling; local true variables in cmds. We can live
// with the false positives so long as there are no false negatives.
func (iz *initializer) extractNamesFromCodeChunk(dec labeledParsedCodeChunk) dtypes.Set[string] {
	if dec.decType == variableDeclaration || dec.decType == constantDeclaration {
		return ast.ExtractAllNames(dec.chunk.(*ast.AssignmentExpression).Right)
	}
	_, _, sig, _, body, given := iz.p.ExtractPartsOfFunction(iz.ParsedDeclarations[dec.decType][dec.decNumber])
	sigNames := dtypes.Set[string]{}
	for _, pair := range sig {
		if pair.VarType != "bling" {
			sigNames = sigNames.Add(pair.VarName)
		}
	}
	bodyNames := ast.ExtractAllNames(body)
	lhsG, rhsG := ast.ExtractNamesFromLhsAndRhsOfGivenBlock(given)
	bodyNames.AddSet(rhsG)
	bodyNames = bodyNames.SubtractSet(lhsG)
	return bodyNames.SubtractSet(sigNames)
}

