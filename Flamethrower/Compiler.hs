module Flamethrower.Compiler where

import Language.Haskell.TH
import qualified Flamethrower.Lexer as L
import Flamethrower.Parser
import Flamethrower.Escape
import Language.Haskell.Meta.Parse.Careful (parseExp)

data CodeTree =
	  Text String
	| Expression Escaper Exp
	| If Exp [CodeTree] [CodeTree]
	| For Name Exp [CodeTree]
	deriving Show

data Compiled = Compiled {
	attributes :: [CodeTree],
	classes :: [CodeTree],
	content :: [CodeTree]
}

fromClasses :: [CodeTree] -> Compiled
fromClasses classes = Compiled { classes = classes, attributes = [], content = [] }

fromAttributes :: [CodeTree] -> Compiled
fromAttributes attributes = Compiled { classes = [], attributes = attributes, content = [] }

fromContent :: [CodeTree] -> Compiled
fromContent content = Compiled { classes = [], attributes = [], content = content }

stringPartToCode :: Escaper -> L.StringPart -> CodeTree
stringPartToCode escaper part = case part of
	L.Character c -> Text $ escapeCharacter escaper c
	L.Interpolation i -> either error (Expression escaper) $ parseExp i

stringPartsToCode :: Escaper -> [L.StringPart] -> [CodeTree]
stringPartsToCode = map . stringPartToCode

voidTags :: [String]
voidTags = ["area", "base", "br", "col", "command", "embed", "hr", "img", "input", "keygen", "link", "meta", "param", "source", "track", "wbr"]

isVoid :: String -> Bool
isVoid = flip elem voidTags

compileNode :: Node -> Compiled
compileNode node = case node of
	ElementNode name children ->
		let
			compiledChildren = map compileNode children
			childAttributes = concatMap attributes compiledChildren
			childClasses = concatMap classes compiledChildren
			childContent = concatMap content compiledChildren

			classContent =
				if null childClasses then
					[]
				else
					Text " class=\"" : childClasses ++ [Text "\""]

			allContent =
				if isVoid name then
					case childContent of
						[] -> (Text $ '<' : name) : classContent ++ childAttributes ++ [Text ">"]
						_ -> error $ "Void element " ++ name ++ " cannot have content."
				else
					(Text $ '<' : name) : classContent ++ childAttributes ++ [Text ">"]
					++ childContent ++ [Text $ "</" ++ name ++ ">"]
		in fromContent allContent
	StringNode (String parts) -> fromContent $ stringPartsToCode Content parts
	StringNode (Raw parts) -> fromContent $ stringPartsToCode None parts
	AttributeNode name (Just (String parts)) -> fromAttributes (Text (' ':name ++ "=\"") : stringPartsToCode Attribute parts ++ [Text "\""])
	AttributeNode name Nothing -> fromAttributes [Text (' ':name)]
	ClassNode name -> fromClasses [Text (' ':name)]
	DoctypeNode -> fromContent [Text "<!DOCTYPE html>"]
	IfNode condition trueChildren falseChildren ->
		let
			trueCompiled = map compileNode trueChildren
			falseCompiled = map compileNode falseChildren

			trueAttributes = concatMap attributes trueCompiled
			trueClasses = concatMap classes trueCompiled
			trueContent = concatMap content trueCompiled

			falseAttributes = concatMap attributes falseCompiled
			falseClasses = concatMap classes falseCompiled
			falseContent = concatMap content falseCompiled

			wrap :: [CodeTree] -> [CodeTree] -> [CodeTree]
			wrap [] [] = []
			wrap truePart falsePart = case parseExp condition of
				Left e -> error e
				Right e -> [If e truePart falsePart]
		in Compiled {
			attributes = wrap trueAttributes falseAttributes,
			classes = wrap trueClasses falseClasses,
			content = wrap trueContent falseContent
		}
	ForNode identifier list children -> case parseExp list of
		Left e -> error e
		Right e -> fromContent [For (mkName identifier) e $ concatMap (contentOnly . compileNode) children]

contentOnly :: Compiled -> [CodeTree]
contentOnly Compiled { classes = [], attributes = [], content = content } = content

optimized :: [CodeTree] -> [CodeTree]
optimized tree = case tree of
	Text a : Text b : rest -> optimized (Text (a ++ b) : rest)
	If cond true false : rest -> If cond (optimized true) (optimized false) : optimized rest
	a : rest -> a : optimized rest
	[] -> []

compile :: [Node] -> [CodeTree]
compile = optimized . concatMap (contentOnly . compileNode)
