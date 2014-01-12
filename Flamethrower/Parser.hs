module Flamethrower.Parser where

import qualified Flamethrower.Lexer as L
import Flamethrower.Lexer (StringPart)
import Language.Haskell.TH (Exp, stringE)

data Node =
	  ElementNode String [Node]
	| StringNode StringNode
	| AttributeNode String (Maybe Node)
	| ClassNode String
	deriving Show

data StringNode = String [StringPart] | Raw [StringPart]
	deriving Show

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)

isIndent :: L.Token -> Bool
isIndent L.Indent = True
isIndent _ = False

parseInside :: Int -> [L.Token] -> ([Node], [L.Token])
parseInside _ [] = ([], [])
parseInside parentIndent tokens =
	case mapFst length $ span isIndent tokens of
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
		addToParse node = mapFst (node:) . continueParse
	in case tokens of
		[] -> ([], [])
		L.Newline : rest -> parseInside indent rest
		L.Element name : rest -> mapFst (replicate 1 . ElementNode name) $ continueParse rest
		L.Attribute name : L.String parts : rest -> addToParse (AttributeNode name . Just . StringNode . String $ parts) rest
		L.Attribute name : rest -> addToParse (AttributeNode name Nothing) rest
		L.Raw : L.String parts : rest -> addToParse (StringNode (Raw parts)) rest
		L.String parts : rest -> addToParse (StringNode (String parts)) rest
		L.Class name : rest -> addToParse (ClassNode name) rest

parseRoot :: [L.Token] -> ([Node], [L.Token])
parseRoot tokens = case parseInside (-1) tokens of
	x@(_, []) -> x
	(tree, rest) -> mapFst (tree++) $ parseRoot rest

parse :: [L.Token] -> [Node]
parse = fst . parseRoot
