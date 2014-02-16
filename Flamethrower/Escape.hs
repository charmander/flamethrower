{-# LANGUAGE OverloadedStrings #-}

module Flamethrower.Escape where

import qualified Data.Text as Text

data Escaper = None | Content | Attribute
	deriving Show

escapeAttributeCharacter :: Char -> String
escapeAttributeCharacter c = case c of
	'&' -> "&amp;"
	'"' -> "&quot;"
	_ -> [c]

escapeContentCharacter :: Char -> String
escapeContentCharacter c = case c of
	'&' -> "&amp;"
	'<' -> "&lt;"
	'>' -> "&gt;"
	_ -> [c]

escapeAttributeValue :: Text.Text -> Text.Text
escapeAttributeValue = Text.concatMap $ Text.pack . escapeAttributeCharacter

escapeContent :: Text.Text -> Text.Text
escapeContent = Text.concatMap $ Text.pack . escapeContentCharacter

escapeCharacter :: Escaper -> Char -> String
escapeCharacter None = replicate 1
escapeCharacter Content = escapeContentCharacter
escapeCharacter Attribute = escapeAttributeCharacter

escape :: Escaper -> Text.Text -> Text.Text
escape None = id
escape Content = escapeContent
escape Attribute = escapeAttributeValue
