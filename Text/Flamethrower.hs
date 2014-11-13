-- | Contains the main quasiquoters that convert Flamethrower templates into expressions.
module Text.Flamethrower (flamethrower, flamef) where

import Data.Maybe (fromMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Text.Flamethrower.Lexer as L
import qualified Text.Flamethrower.Parser as P
import qualified Text.Flamethrower.Compiler as C
import Text.Flamethrower.Escape

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

-- | Converts strings representing Flamethrower templates into expressions.
flamethrower' :: String -> Q Exp
flamethrower' template = do
	let
		get :: String -> Q Name
		get name = fmap (fromMaybe $ error $ "Couldn’t find name " ++ name ++ ".") $ lookupValueName name

	[escapeContentName, escapeAttributeValueName, listConcatName, textConcatName] <-
		mapM get ["Text.Flamethrower.Escape.escapeContent", "Text.Flamethrower.Escape.escapeAttributeValue", "Prelude.concat", "Data.Text.concat"]

	let functionMap = FunctionMap {
		escapeContentName = escapeContentName,
		escapeAttributeValueName = escapeAttributeValueName,
		listConcatName = listConcatName,
		textConcatName = textConcatName
	}

	return $ VarE textConcatName `AppE` (VarE listConcatName `AppE` ListE (compileTemplate functionMap template))

-- | A quasiquoter to convert Flamethrower templates into expressions.
--
-- > exampleTemplate :: Text -> Text
-- > exampleTemplate title = [flamethrower|
-- > doctype
-- >
-- > html
-- >     head
-- >         meta charset: "utf-8"
-- >
-- >         title "#{title}"
-- >
-- >     body
-- >         h1 "A page"
-- >
-- >         p "Hello, world!"
-- > |]
flamethrower :: QuasiQuoter
flamethrower = QuasiQuoter {
	quoteExp = flamethrower',
	quotePat = error "Flamethrower templates are expressions, not patterns.",
	quoteDec = error "Flamethrower templates are expressions, not declarations.",
	quoteType = error "Flamethrower templates are expressions, not types."
}

-- | A quasiquoter that reads Flamethrower template files.
--
-- > exampleTemplate :: Text -> Text
-- > exampleTemplate title = [flamef|example-template.flame|]
flamef :: QuasiQuoter
flamef = quoteFile flamethrower