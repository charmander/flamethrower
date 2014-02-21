{-# LANGUAGE NamedFieldPuns #-}

module Flamethrower where

import Data.Maybe (fromJust)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Flamethrower.Lexer as L
import qualified Flamethrower.Parser as P
import qualified Flamethrower.Compiler as C
import Flamethrower.Escape

data FunctionMap = FunctionMap {
	escapeContentName :: Name,
	escapeAttributeValueName :: Name,
	listConcatName :: Name,
	textConcatName :: Name
}

codeTreeToExpression :: FunctionMap -> C.CodeTree -> Exp
codeTreeToExpression functionMap tree = case tree of
	C.Text s -> ListE [LitE $ StringL s]
	C.Expression escaper exp -> ListE . replicate 1 $
		case escaper of
			None -> exp
			Content -> VarE (escapeContentName functionMap) `AppE` exp
			Attribute -> VarE (escapeAttributeValueName functionMap) `AppE` exp
	C.If condition truePart falsePart ->
		let cond = CondE condition (ListE $ map (codeTreeToExpression functionMap) truePart) (ListE $ map (codeTreeToExpression functionMap) falsePart)
		in VarE (listConcatName functionMap) `AppE` cond
	C.For bind loop children -> VarE (listConcatName functionMap) `AppE` CompE [BindS (VarP bind) loop, NoBindS $ VarE (listConcatName functionMap) `AppE` ListE (map (codeTreeToExpression functionMap) children)]

compileTemplate :: FunctionMap -> String -> [Exp]
compileTemplate functionMap = map (codeTreeToExpression functionMap) . C.compile . P.parse . L.lex

flamethrower' :: String -> Q Exp
flamethrower' template = do
	let
		get :: String -> Q Name
		get = fmap fromJust . lookupValueName

	[escapeContentName, escapeAttributeValueName, listConcatName, textConcatName] <-
		mapM get ["Flamethrower.Escape.escapeContent", "Flamethrower.Escape.escapeAttributeValue", "Prelude.concat", "Data.Text.concat"]

	let functionMap = FunctionMap { escapeContentName, escapeAttributeValueName, listConcatName, textConcatName }

	return $ VarE textConcatName `AppE` (VarE listConcatName `AppE` ListE (compileTemplate functionMap template))

flamethrower :: QuasiQuoter
flamethrower = QuasiQuoter {
	quoteExp = flamethrower',
	quotePat = error "Flamethrower templates are expressions, not patterns.",
	quoteDec = error "Flamethrower templates are expressions, not declarations.",
	quoteType = error "Flamethrower templates are expressions, not types."
}

flamef :: QuasiQuoter
flamef = quoteFile flamethrower
