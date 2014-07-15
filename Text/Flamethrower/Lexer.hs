module Text.Flamethrower.Lexer where

import Data.Char
import Control.Arrow (first, second)
import Data.List (isPrefixOf)

data Context = Context { indentType :: Maybe Indent }
data Indent = Tab | Spaces Int

data Token =
	  Indent
	| Newline
	| Element String
	| Class String
	| Attribute String
	| String [StringPart]
	| Raw
	| Doctype
	| If String
	| Else
	| For String String
	deriving (Show, Eq)

data StringPart = Character Char | Interpolation String
	deriving (Show, Eq)

isIdentifierCharacter :: Char -> Bool
isIdentifierCharacter c = case c of
	'-' -> True
	'_' -> True
	_   -> isLetter c || isDigit c

lexIndent :: Context -> String -> [Token]
lexIndent context template =
	let
		(spaceIndent, afterSpaceIndent) =
			let (a, b) = span (== ' ') template in (length a, b)
		(tabIndent, afterTabIndent) =
			let (a, b) = span (== '\t') template in (length a, b)
	in case context of
		Context { indentType = Nothing } ->
			case tabIndent of
				0 -> case spaceIndent of
					0 -> lexContent context template
					n -> Indent : lexContent context { indentType = Just (Spaces n) } afterSpaceIndent
				n -> replicate n Indent ++ lexContent context { indentType = Just Tab } afterTabIndent
		Context { indentType = Just Tab } ->
			replicate tabIndent Indent ++ lexContent context afterTabIndent
		Context { indentType = Just (Spaces n) } ->
			case tabIndent of
				0 -> case spaceIndent `quotRem` n of
					(d, 0) -> replicate d Indent ++ lexContent context afterSpaceIndent
					_ -> error $ "Expected an indent of a number of spaces divisible by " ++ show n ++ "."
				_ -> error "Unexpected tab."

lexContent :: Context -> String -> [Token]
lexContent context template =
	let newline rest = Newline : lexIndent context rest
	in case template of
		[] -> []
		'\n':rest -> newline rest
		'\r':'\n':rest -> newline rest
		'\r':rest -> newline rest
		' ':rest -> lexContent context rest
		'#':rest -> lexComment context rest
		'"':rest -> lexString context rest
		'!':'"':rest -> Raw : lexString context rest
		'.':rest -> lexClass context rest
		c:_
			| isLetter c -> lexIdentifier context template
			| otherwise -> error $ "Unexpected " ++ show c ++ "."

readIdentifier :: String -> (String, String)
readIdentifier input =
	let (identifier, rest) = span isIdentifierCharacter input
	in case rest of
		':':c:rest | isLetter c ->
			let (a, b) = readIdentifier (c:rest)
			in (identifier ++ ":" ++ a, b)
		_ -> (identifier, rest)

lexIdentifier :: Context -> String -> [Token]
lexIdentifier context template = case readIdentifier template of
	(identifier, ':':rest) -> Attribute identifier : lexContent context rest
	(identifier, rest) -> case identifier of
		"doctype" -> Doctype : lexContent context rest
		"if" -> lexIf context rest
		"else" -> Else : lexContent context rest
		"for" -> lexFor context rest
		_ -> Element identifier : lexContent context rest

readLine :: String -> (String, String)
readLine input = case input of
	'\r':'\n':rest -> ("", rest)
	'\n':rest -> ("", rest)
	'\r':rest -> ("", rest)
	c:rest -> first (c:) $ readLine rest

lexIf :: Context -> String -> [Token]
lexIf context template =
	let (condition, rest) = readLine (dropWhile (== ' ') template)
	in If condition : Newline : lexIndent context rest

isHaskellVarIdCharacter :: Char -> Bool
isHaskellVarIdCharacter = flip elem [UppercaseLetter, LowercaseLetter, TitlecaseLetter] . generalCategory

readHaskellVarId :: String -> (String, String)
readHaskellVarId input = case input of
	c:rest | generalCategory c == LowercaseLetter -> first (c:) $ span isHaskellVarIdCharacter rest
	_ -> ("", input)

lexFor :: Context -> String -> [Token]
lexFor context template = case second (dropWhile (== ' ')) . readHaskellVarId . dropWhile (== ' ') $ template of
	("", _) -> error "Expected Haskell identifier."
	(identifier, rest)
		| "in " `isPrefixOf` rest ->
			let (list, rest') = readLine $ dropWhile (== ' ') $ drop 3 rest
			in For identifier list : Newline : lexIndent context rest'
		| otherwise -> error "Expected “in”."

lexComment :: Context -> String -> [Token]
lexComment context template = case template of
	[] -> []
	'\r':'\n':_ -> lexContent context template
	'\n':_ -> lexContent context template
	'\r':_ -> lexContent context template
	c:rest -> lexComment context rest

readString :: String -> ([StringPart], String)
readString template = case template of
	'\\':rest -> readEscape rest
	'"':rest -> ([], rest)
	'#':'{':rest -> readInterpolation rest
	c:cs ->
		let (parts, rest) = readString cs
		in (Character c : parts, rest)
	[] -> error "Expected end of string before end of input"

readEscape :: String -> ([StringPart], String)
readEscape s@(c:cs) =
	case c of
		'#' -> first (Character '#':) $ readString cs
		'&' -> readString cs
		_
			| isSpace c -> case span isSpace cs of
				(_, '\\':rest) -> readString rest
				_ -> error "Gap must end with a backslash."
			| otherwise -> case readLitChar ('\\':s) of
				[(c, rest)] -> first (Character c:) $ readString rest
				_ -> error "Unrecognized escape sequence."

readInterpolation :: String -> ([StringPart], String)
readInterpolation template =
	let
		(interpolation, cs) = span (/= '}') template
		(parts, rest) = readString $ tail cs
	in (Interpolation interpolation : parts, rest)

lexString :: Context -> String -> [Token]
lexString context template =
	let (parts, rest) = readString template
	in String parts : lexContent context rest

lexClass :: Context -> String -> [Token]
lexClass context template =
	let (className, rest) = span isIdentifierCharacter template
	in Class className : lexContent context rest

lex :: String -> [Token]
lex = lexIndent Context { indentType = Nothing }
