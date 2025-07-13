package initializer

import (
	"github.com/tim-hardcastle/Pipefish/source/ast"
	"github.com/tim-hardcastle/Pipefish/source/compiler"
	"github.com/tim-hardcastle/Pipefish/source/dtypes"
	"github.com/tim-hardcastle/Pipefish/source/parser"
	"github.com/tim-hardcastle/Pipefish/source/token"
	"github.com/tim-hardcastle/Pipefish/source/values"
)

// A miscellaneous collection of functions for extracting data from other data.

func (iz *Initializer) getMatches(sigToMatch fnSigInfo, fnToTry *ast.PrsrFunction, tok *token.Token) values.AbstractType {
	result := values.MakeAbstractType()
	// Check that the sigs are the right length, the return sig being optional.
	if sigToMatch.sig.Len() != len(fnToTry.NameSig) {
		return result
	}
	if sigToMatch.rtnSig.Len() != 0 && sigToMatch.rtnSig.Len() != len(fnToTry.NameRets) {
		return result
	}
	abSig := fnToTry.Compiler.(*compiler.Compiler).P.MakeAbstractSigFromStringSig(fnToTry.NameSig)
	abRets := fnToTry.Compiler.(*compiler.Compiler).P.MakeAbstractSigFromStringSig(fnToTry.NameRets)
	// Once we have identified one set of types as being 'self' we need to fix that
	// as 'self' and take its intersection with the other things that appear in the
	// 'self' position.
	foundSelf := false
	// If it's a parameterized type, e.g. list{T type} then we need to match it against
	// the same parameterized type in the call types and a corresponding type calculation
	// (e.g. list{T}) in the return types.
	var paramType *ast.TypeWithParameters
	for i := 0; i < len(sigToMatch.sig); i++ {
		if maybeSelf, ok := sigToMatch.sig.GetVarType(i).(*ast.TypeWithName); ok && maybeSelf.Name == "self" {
			if foundSelf {
				result = result.Intersect(abSig[i].VarType)
				if len(result.Types) == 0 {
					break
				}
				if twp, ok := fnToTry.NameSig.GetVarType(i).(*ast.TypeWithParameters); ok {
					if paramType == nil || !paramType.Equals(twp) {
						return values.MakeAbstractType()
					}
				}
			} else {
				foundSelf = true
				result = abSig[i].VarType
				if twp, ok := fnToTry.NameSig.GetVarType(i).(*ast.TypeWithParameters); ok {
					paramType = twp
				}
			}
		} else {
			if !iz.P.GetAbstractType(sigToMatch.sig.GetVarType(i).(ast.TypeNode)).IsSubtypeOf(abSig[i].VarType) ||
				ast.IsAstBling(sigToMatch.sig.GetVarType(i).(ast.TypeNode)) && sigToMatch.sig.GetVarName(i) != abSig[i].VarName {
				return values.MakeAbstractType()
			}
		}
	}
	if !foundSelf {
		iz.P.Throw("init/interface/self", tok)
		return values.MakeAbstractType()
	}
	for i := 0; i < sigToMatch.rtnSig.Len(); i++ {
		if t, ok := sigToMatch.rtnSig[i].VarType.(*ast.TypeWithName); ok && t.Name == "self" {
			// First we deal with the possibility of a type expression matching a parameterized
			// type.
			te, ok := fnToTry.NameRets.GetVarType(i).(*ast.TypeExpression)
			if ok && paramType != nil {
				if paramType.Matches(te) {
					continue
				} else {
					return values.MakeAbstractType()
				}
			}
			// If not ...
			result = result.Intersect(abRets[i].VarType)
			if paramType == nil && result.Len() != 1 {
				// To explain. If we have types A and B which are subtypes of C, then having
				// a function defined (x C) + (y C) -> C doesn't guarantee that A is addable.
				return values.MakeAbstractType()
			}
		} else {
			if !abRets[i].VarType.IsSubtypeOf(iz.P.GetAbstractType(sigToMatch.rtnSig[i].VarType)) {
				return values.MakeAbstractType()
			}
		}
	}
	return result
}

func (iz *Initializer) makeTypeWithParameters(op token.Token, tokSig parser.TokSig) *ast.TypeWithParameters {
	params := []*ast.Parameter{}
	for _, pair := range tokSig {
		// TODO --- Checking on the well-formedness of types may have slipped through the
		// cracks. The typename should only be one token, but this hasn't been verified.
		newParam := &ast.Parameter{pair.Name.Literal, pair.Typename[0].Literal}
		params = append(params, newParam)
	}
	return &ast.TypeWithParameters{op, op.Literal, params}
}

func (iz *Initializer) makeAstSigFromTokenizedSig(ts parser.TokSig) ast.AstSig {
	as := ast.AstSig{}
	for _, pair := range ts {
		as = append(as, ast.NameTypeAstPair{pair.Name.Literal, iz.makeTypeAstFromTokens(pair.Typename)})
	}
	return as
}

func (iz *Initializer) makeRetsFromTokenizedReturns(ts parser.TokReturns) ast.AstSig {
	as := ast.AstSig{}
	for _, ty := range ts {
		as = append(as, ast.NameTypeAstPair{"", iz.makeTypeAstFromTokens(ty)})
	}
	return as
}

func (iz *Initializer) makeTypeAstFromTokens(toks []token.Token) ast.TypeNode {
	ts := token.MakeCodeChunk(toks, false)
	iz.P.PrimeWithTokenSupplier(ts)
	node := iz.P.ParseTypeFromCurTok(parser.LOWEST)
	return node
}

// This is a fairly crude way of slurping the names of functions, commands, constants, and variables out of a declaration.
// It is crude in that it will slurp other things too: type names, for example; bling; local true variables in cmds. We can live
// with the false positives so long as there are no false negatives.
func (iz *Initializer) extractNamesFromCodeChunk(dec labeledParsedCodeChunk) dtypes.Set[string] {
	if dec.decType == variableDeclaration || dec.decType == constantDeclaration {
		return ast.ExtractAllNames(dec.chunk.(*ast.AssignmentExpression).Right)
	}
	if dec.decType == structDeclaration || dec.decType == cloneDeclaration || dec.decType == makeDeclaration {
		return ast.ExtractAllNames(dec.chunk)
	}
	_, _, sig, _, body, given := iz.P.ExtractPartsOfFunction(iz.ParsedDeclarations[dec.decType][dec.decNumber])
	sigNames := dtypes.Set[string]{}
	for _, pair := range sig {
		if _, ok := pair.VarType.(*ast.Bling); !ok {
			sigNames = sigNames.Add(pair.VarName)
		}
	}
	bodyNames := ast.ExtractAllNames(body)
	lhsG, rhsG := ast.ExtractNamesFromLhsAndRhsOfGivenBlock(given)
	bodyNames.AddSet(rhsG)
	bodyNames = bodyNames.SubtractSet(lhsG)
	return bodyNames.SubtractSet(sigNames)
}
