{-# LANGUAGE NamedFieldPuns #-}

module Flamethrower (escapeContent, escapeAttributeValue, flamethrower, flamef) where

import Data.Maybe (fromJust)
import Data.List (intercalate)
import Data.Char (toLower)

import qualified Flamethrower.Lexer as L
import Flamethrower.Parser
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.Meta.Parse.Careful (parseExp)

data Escaper = Escaper Name (Char -> String)
data FunctionMap = FunctionMap { escapeContentName :: Name, escapeAttributeValueName :: Name, concatName :: Name }

escapeContent' :: Char -> String
escapeContent' c = case c of
	'&' -> "&amp;"
	'<' -> "&lt;"
	'>' -> "&gt;"
	_ -> [c]

escapeContent :: String -> String
escapeContent = concatMap escapeContent'

escapeAttributeValue' :: Char -> String
escapeAttributeValue' c = case c of
	'&' -> "&amp;"
	'"' -> "&quot;"
	_ -> [c]

escapeAttributeValue :: String -> String
escapeAttributeValue = concatMap escapeAttributeValue'

compileStringPart :: Maybe Escaper -> L.StringPart -> Exp
compileStringPart Nothing part = case part of
	L.Character c -> LitE (StringL [c])
	L.Interpolation i -> case parseExp i of
		Left s -> error s
		Right e -> e
compileStringPart (Just (Escaper name escaper)) part = case part of
	L.Character c -> LitE $ StringL $ escaper c
	L.Interpolation i ->
		case parseExp i of
			Left s -> error s
			Right e -> VarE name `AppE` e

compileString :: FunctionMap -> Maybe Escaper -> [L.StringPart] -> Exp
compileString FunctionMap { concatName } escaper parts = AppE (VarE concatName) $ ListE $ map (compileStringPart escaper) parts

voidTags :: [String]
voidTags = ["area", "base", "br", "col", "command", "embed", "hr", "img", "input", "keygen", "link", "meta", "param", "source", "track", "wbr"]

isVoid :: String -> Bool
isVoid = flip elem voidTags . map toLower

compile :: FunctionMap -> Node -> Exp
compile functionMap @ FunctionMap { escapeContentName, escapeAttributeValueName, concatName } node = do
	let
		separate :: [Node] -> ([Node], [Node], [Node])
		separate [] = ([], [], [])
		separate (x:xs) =
			let (a, b, c) = separate xs
			in case x of
				ClassNode _ -> (x:a, b, c)
				AttributeNode _ _ -> (a, x:b, c)
				_ -> (a, b, x:c)

		classToString :: Node -> String
		classToString (ClassNode name) = name

		contentEscaper = Just (Escaper escapeContentName escapeContent')
		attributeEscaper = Just (Escaper escapeAttributeValueName escapeAttributeValue')

		compile' = compile functionMap

	case node of
		ElementNode name children ->
			case separate children of
				(classes, attributes, content) | not $ isVoid name ->
					AppE (VarE concatName) $ ListE $
					(LitE $ StringL $ '<' : name) :
					(case classes of
						[] -> []
						_  -> LitE (StringL " class=\"") : LitE (StringL (unwords (map classToString classes))) : [LitE (StringL "\"")]
					) ++
					map compile' attributes ++
					LitE (StringL ">") :
					map compile' content ++
					[LitE $ StringL $ "</" ++ name ++ ">"]
				(classes, attributes, []) ->
					AppE (VarE concatName) $ ListE $
					(LitE $ StringL $ '<' : name) :
					(case classes of
						[] -> []
						_  -> LitE (StringL " class=\"") : LitE (StringL (unwords (map classToString classes))) : [LitE (StringL "\"")]
					) ++
					map compile' attributes ++
					[LitE (StringL ">")]
				_ -> error $ "The void element <" ++ name ++ "> cannot have content."
		StringNode value ->
			case value of
				String parts -> compileString functionMap contentEscaper parts
				Raw parts -> compileString functionMap Nothing parts
		AttributeNode name value ->
			case value of
				Nothing -> LitE $ StringL $ ' ' : name
				Just (StringNode (String parts)) -> AppE (VarE concatName) $ ListE $ (LitE $ StringL $ ' ' : name ++ "=\"") : compileString functionMap attributeEscaper parts : [LitE (StringL "\"")]

compileTemplate :: FunctionMap -> String -> Exp
compileTemplate functionMap = ListE . map (compile functionMap) . parse . L.lex

flamethrower' :: String -> Q Exp
flamethrower' template = do
	escapeContentName <- fmap fromJust $ lookupValueName "Flamethrower.escapeContent"
	escapeAttributeValueName <- fmap fromJust $ lookupValueName "Flamethrower.escapeAttributeValue"
	concatName <- fmap fromJust $ lookupValueName "Prelude.concat"

	return . AppE (VarE concatName) $ compileTemplate FunctionMap { escapeContentName, escapeAttributeValueName, concatName } template

flamethrower :: QuasiQuoter
flamethrower = QuasiQuoter { quoteExp = flamethrower' }

flamef :: QuasiQuoter
flamef = quoteFile flamethrower
