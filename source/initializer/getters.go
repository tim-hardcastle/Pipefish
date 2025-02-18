package initializer

import (
	"path/filepath"

	"github.com/tim-hardcastle/Pipefish/source/ast"
	"github.com/tim-hardcastle/Pipefish/source/dtypes"
	"github.com/tim-hardcastle/Pipefish/source/settings"
	"github.com/tim-hardcastle/Pipefish/source/text"
	"github.com/tim-hardcastle/Pipefish/source/token"
	"github.com/tim-hardcastle/Pipefish/source/values"
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
		if filepath.IsLocal(scriptFilepath) {
			scriptFilepath = filepath.Join(filepath.Dir(imp.GetToken().Source), scriptFilepath)
		}
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
			if filepath.IsLocal(scriptFilepath) {
				scriptFilepath = filepath.Join(filepath.Dir(imp.GetToken().Source), scriptFilepath)
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

func (iz *initializer) getMatches(sigToMatch fnSigInfo, abSig, abRets ast.AbstractSig, fnToTry *ast.PrsrFunction, tok *token.Token) values.AbstractType {
	result := values.MakeAbstractType()
	// Check that the sigs are the right length, the return sig being optional.
	if sigToMatch.sig.Len() != len(fnToTry.NameSig) {
		return result
	}
	if sigToMatch.rtnSig.Len() != 0 && sigToMatch.rtnSig.Len() != len(fnToTry.NameRets) {
		return result
	}
	// Once we have identified one set of types as being 'self' we need to fix that
	// as 'self' and take its intersection with the other things that appear in the
	// 'self' position.
	foundSelf := false
	for i := 0; i < len(sigToMatch.sig); i++ {
		if sigToMatch.sig.GetVarType(i).(string) == "self" {
			if foundSelf {
				result = result.Intersect(abSig[i].VarType)
				if len(result.Types) == 0 {
					break
				}
			} else {
				foundSelf = true
				result = abSig[i].VarType
			}
		} else {
			if !iz.p.GetAbstractType(sigToMatch.sig.GetVarType(i).(string)).IsSubtypeOf(abSig[i].VarType) ||
				sigToMatch.sig.GetVarType(i).(string) == "bling" && sigToMatch.sig.GetVarName(i) != abSig[i].VarName {
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
			result = result.Intersect(abRets[i].VarType)
		} else {
			if !abRets[i].VarType.IsSubtypeOf(iz.p.GetAbstractType(sigToMatch.rtnSig[i].VarType)) {
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
