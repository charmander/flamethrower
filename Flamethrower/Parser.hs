module Flamethrower.Parser where

import Control.Arrow (first)
import Data.List (isPrefixOf)
import qualified Flamethrower.Lexer as L
import Flamethrower.Lexer (StringPart)
import Language.Haskell.TH (Exp, stringE)

data Node =
	  ElementNode String [Node]
	| StringNode StringNode
	| AttributeNode String (Maybe StringNode)
	| ClassNode String
	| DoctypeNode
	| IfNode String [Node] [Node]
	deriving (Show, Eq)

data StringNode = String [StringPart] | Raw [StringPart]
	deriving (Show, Eq)

isIndent :: L.Token -> Bool
isIndent L.Indent = True
isIndent _ = False

parseInside :: Int -> [L.Token] -> ([Node], [L.Token])
parseInside _ [] = ([], [])
parseInside parentIndent tokens =
	case first length $ span isIndent tokens of
		(_, L.Newline:rest) -> parseInside parentIndent rest
		(indent, rest)
			| indent == parentIndent + 1 ->
				let
					(content, rest2) = parseContent indent rest
					(continuedParse, rest3) = parseInside parentIndent rest2
				in (content ++ continuedParse, rest3)
			| indent <= parentIndent -> ([], tokens)
			| otherwise -> error "Excessive indentation."

parseContent :: Int -> [L.Token] -> ([Node], [L.Token])
parseContent indent tokens =
	let
		continueParse = parseContent indent
		addToParse node = first (node:) . continueParse
	in case tokens of
		[] -> ([], [])
		L.Newline : rest -> parseInside indent rest
		L.Element name : rest -> first (replicate 1 . ElementNode name) $ continueParse rest
		L.Attribute name : L.String parts : rest -> addToParse (AttributeNode name . Just $ String parts) rest
		L.Attribute name : rest -> addToParse (AttributeNode name Nothing) rest
		L.Raw : L.String parts : rest -> addToParse (StringNode (Raw parts)) rest
		L.String parts : rest -> addToParse (StringNode (String parts)) rest
		L.Class name : rest -> addToParse (ClassNode name) rest
		L.Doctype : rest -> addToParse DoctypeNode rest
		L.If condition : rest ->
			let (inside, after) = continueParse rest
			in
				if isPrefixOf (replicate indent L.Indent ++ [L.Else]) after then
					first (replicate 1 . IfNode condition inside) . continueParse $ drop (indent + 1) after
				else
					([IfNode condition inside []], after)
		x : _ -> error $ "Unexpected " ++ show x

parseRoot :: [L.Token] -> ([Node], [L.Token])
parseRoot tokens = case parseInside (-1) tokens of
	x@(_, []) -> x
	(tree, rest) -> first (tree++) $ parseRoot rest

parse :: [L.Token] -> [Node]
parse = fst . parseRoot
