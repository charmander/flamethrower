{-# LANGUAGE TemplateHaskell #-}

-- | Contains the main quasiquoters that convert Flamethrower templates into expressions.
module Text.Flamethrower (flamethrower, flamef) where

import Data.Maybe (fromMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Text.Flamethrower.Lexer as L
import qualified Text.Flamethrower.Parser as P
import qualified Text.Flamethrower.Compiler as C
import Text.Flamethrower.Escape

import qualified Data.Text

codeTreeToExpression :: C.CodeTree -> Exp
codeTreeToExpression tree = case tree of
	C.Text s -> ListE [LitE $ StringL s]
	C.Expression escaper exp -> ListE . replicate 1 $
		case escaper of
			None -> exp
			Content -> VarE 'escapeContent `AppE` exp
			Attribute -> VarE 'escapeAttributeValue `AppE` exp
	C.If condition truePart falsePart ->
		let cond = CondE condition (ListE $ map codeTreeToExpression truePart) (ListE $ map codeTreeToExpression falsePart)
		in VarE 'concat `AppE` cond
	C.For bind loop children -> VarE 'concat `AppE` CompE [BindS (VarP bind) loop, NoBindS $ VarE 'concat `AppE` ListE (map codeTreeToExpression children)]

compileTemplate :: String -> [Exp]
compileTemplate = map codeTreeToExpression . C.compile . P.parse . L.lex

-- | Converts strings representing Flamethrower templates into expressions.
flamethrower' :: String -> Q Exp
flamethrower' template = return $ VarE 'Data.Text.concat `AppE` (VarE 'concat `AppE` ListE (compileTemplate template))

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
