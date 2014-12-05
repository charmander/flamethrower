{-# LANGUAGE OverloadedStrings #-}

-- | Contains functions to escape text for inclusion in HTML.
module Text.Flamethrower.Escape where

import qualified Data.Text as Text

-- | Represents a type of escaping to be applied to text.
data Escaper =
	-- | Perform no escaping.
	None |
	-- | Escape for inclusion as content in HTML – that is, for use between tags.
	-- This escapes @&@, @<@, and @>@.
	Content |
	-- | Escape for inclusion in a double-quoted HTML attribute value.
	-- This escapes @&@ and @"@.
	Attribute
	deriving (Eq, Show)

-- | Converts a character into an equivalent HTML character reference if necessary for it to be correctly included in a double-quoted HTML attribute value.
-- This escapes @&@ and @"@.
escapeAttributeCharacter :: Char -> String
escapeAttributeCharacter c = case c of
	'&' -> "&amp;"
	'"' -> "&quot;"
	_ -> [c]

-- | Converts a character into an equivalent HTML character reference if necessary for it to be correctly included as content – that is, between HTML tags.
-- This escapes @&@, @<@, and @>@.
escapeContentCharacter :: Char -> String
escapeContentCharacter c = case c of
	'&' -> "&amp;"
	'<' -> "&lt;"
	'>' -> "&gt;"
	_ -> [c]

-- | Converts characters in text into equivalent HTML character references when necessary for the entire string to be correctly included in a double-quoted HTML attribute value.
-- This escapes @&@ and @"@.
escapeAttributeValue :: Text.Text -> Text.Text
escapeAttributeValue = Text.concatMap $ Text.pack . escapeAttributeCharacter

-- | Converts characters in text into equivalent HTML character references when necessary for the entire string to be correctly included as content – that is, between HTML tags.
-- This escapes @&@, @<@, and @>@.
escapeContent :: Text.Text -> Text.Text
escapeContent = Text.concatMap $ Text.pack . escapeContentCharacter

-- | Converts a character into an equivalent HTML character reference as defined by the given 'Escaper'.
escapeCharacter :: Escaper -> Char -> String
escapeCharacter None = replicate 1
escapeCharacter Content = escapeContentCharacter
escapeCharacter Attribute = escapeAttributeCharacter
