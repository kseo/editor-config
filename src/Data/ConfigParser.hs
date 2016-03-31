{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Clean configuration files in the INI format.
--
-- Format rules and recommendations:
--
--  * The @: @ syntax is space-sensitive.
--
--  * Keys are case-sensitive.
--
--  * Lower-case is recommended.
--
--  * Values can be empty.
--
--  * Keys cannot key separators, section delimiters, or comment markers.
--
--  * Comments must start at the beginning of the line and start with @;@ or @#@.
--
-- An example configuration file:
--
-- @
-- # Some comment.
-- [SERVER]
-- port=6667
-- hostname=localhost
-- ; another comment here
-- [AUTH]
-- user: hello
-- pass: world
-- salt:
-- @
--
-- Parsing example:
--
-- >>> parseIni "[SERVER]\nport: 6667\nhostname: localhost"
-- Right (Ini {unIni = fromList [("SERVER",fromList [("hostname","localhost"),("port","6667")])]})
--

module Data.ConfigParser
  (-- * Reading
   readIniFile
  ,parseIni
  ,lookupValue
  ,readValue
  ,parseValue
  ,sections
  ,keys
   -- * Types
  ,Ini(..)
   -- * Parsers
  ,iniParser
  ,sectionParser
  ,keyValueParser
  )
  where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import           Data.Char
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as M
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Prelude                    hiding (takeWhile)

-- | An INI configuration.
newtype Ini = Ini { unIni :: HashMap Text (HashMap Text Text) }
  deriving (Show, Monoid)

-- | Parse an INI file.
readIniFile :: FilePath -> IO (Either String Ini)
readIniFile = fmap parseIni . T.readFile

-- | Parse an INI config.
parseIni :: Text -> Either String Ini
parseIni = parseOnly iniParser

-- | Lookup values in the config.
lookupValue :: Text -> Text -> Ini -> Either String Text
lookupValue name key (Ini ini) =
  case M.lookup name ini of
    Nothing -> Left ("Couldn't find section: " ++ T.unpack name)
    Just section ->
      case M.lookup key section of
        Nothing -> Left ("Couldn't find key: " ++ T.unpack key)
        Just value -> return value

-- | Get the sections in the config.
sections :: Ini -> [Text]
sections (Ini ini) = M.keys ini

-- | Get the keys in a section.
keys :: Text -> Ini -> Either String [Text]
keys name (Ini ini) =
  case M.lookup name ini of
    Nothing -> Left ("Couldn't find section: " ++ T.unpack name)
    Just section -> Right (M.keys section)

-- | Read a value using a reader from "Data.Text.Read".
readValue :: Text -> Text -> (Text -> Either String (a, Text))
          -> Ini
          -> Either String a
readValue section key f ini =
  fmap fst (lookupValue section key ini >>= f)

-- | Parse a value using a reader from "Data.Attoparsec.Text".
parseValue :: Text -> Text -> Parser a
           -> Ini
           -> Either String a
parseValue section key f ini =
  lookupValue section key ini >>= parseOnly (f <* (skipSpace >> endOfInput))

-- | Parser for an INI.
iniParser :: Parser Ini
iniParser =
  do x <- sectionlessParser
     xs <- many sectionParser
     return (Ini . M.fromList $ (x:xs))

sectionlessParser :: Parser (Text,HashMap Text Text)
sectionlessParser =
  do values <- many keyValueParser
     return ("[]", M.fromList values)

-- | A section. Format: @[foo]@. Conventionally, @[FOO]@.
sectionParser :: Parser (Text,HashMap Text Text)
sectionParser =
  do skipComments
     _ <- char '['
     name <- takeWhile (\c -> c /=']' && c /= '[')
     _ <- char ']'
     skipEndOfLine
     values <- many keyValueParser
     return (T.strip name, M.fromList values)

-- | A key-value pair. Either @foo: bar@ or @foo=bar@.
keyValueParser :: Parser (Text,Text)
keyValueParser =
  do skipComments
     key <- takeWhile1 (\c -> not (isDelim c || c == '[' || c == ']'))
     delim <- satisfy isDelim
     value <- fmap (clean delim) (takeWhile (not . isEndOfLine))
     skipEndOfLine
     return (T.strip key, T.strip value)
  where clean ':' = T.drop 1
        clean _   = id

-- | Is the given character a delimiter?
isDelim :: Char -> Bool
isDelim x = x == '=' || x == ':'

-- | Skip end of line and whitespace beyond.
skipEndOfLine :: Parser ()
skipEndOfLine = skipWhile isEndOfLine

-- | Skip comments starting at the beginning of the line.
skipComments :: Parser ()
skipComments =
  skipMany (do _ <- satisfy (\c -> c == ';' || c == '#')
               skipWhile (not . isEndOfLine)
               skipEndOfLine)
