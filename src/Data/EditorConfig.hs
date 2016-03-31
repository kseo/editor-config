{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2016 Kwang Yul Seo
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Kwang Yul Seo <kwangyul.seo@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- EditorConfig makes it easy to maintain the correct coding style when
-- switching between different text editors and between different projects.
-- The EditorConfig project maintains a file format and plugins for various
-- text editors which allow this file format to be read and used by those
-- editors.
----------------------------------------------------------------------------
module Data.EditorConfig
    ( EditorConfigError(..)
    , EndOfLine(..)
    , getProperties
    , IndentStyle(..)
    , Properties(..)
    ) where

import           Control.Applicative ((<|>), (<$>))
import           Control.Monad ((>=>), (=<<), filterM, unless)
import           Control.Monad.Except (ExceptT, liftIO, runExceptT, throwError)
import           Data.Attoparsec.Text (string, many1, digit, Parser, parseOnly)
import           Data.Foldable (fold, foldMap)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List (findIndex, find, inits, intercalate)
import           Data.Monoid (First(..), (<>))
import           Data.Text (Text, unpack)
import qualified Data.Text.IO as T
import           System.Directory (doesFileExist, doesDirectoryExist)
import           System.FilePath (joinPath, splitPath, (</>), takeDirectory, takeFileName)
import           System.FilePath.Glob (Pattern)
import qualified System.FilePath.Glob as Glob
import           System.IO (TextEncoding, latin1, utf8, utf8_bom, utf16le, utf16be)

import Data.ConfigParser (parseIni, Ini(..))

data EditorConfigError = ParseError String
  deriving (Eq, Show)

data IndentStyle = Tab | Space deriving (Eq, Show)

data EndOfLine = LF | CR | CRLF deriving (Eq, Show)

data Properties =
  Properties { indentStyle            :: Maybe IndentStyle
                -- ^ Sets to 'Tab' or 'Space' to use hard tabs or soft tabs
                -- respectively.
             , indentSize             :: Maybe Int
                -- ^ A whole number defining the number of columns used for
                -- each indentation level and the width of soft tabs (when
                -- supported). WHen set to 'Tab', the value of 'tabWidth' (if
                -- specified) will be used.
             , tabWidth               :: Maybe Int
                -- ^ A whole number defining the number of columns used to
                -- represent a tab character. This defaults to the value of
                -- 'indentSize' and doesn't usually need to be specified.
             , endOfLine              :: Maybe EndOfLine
                -- ^ Sets to 'LF', 'CR' or 'CRLF' to control how line breaks
                -- are represented.
             , charset                :: Maybe String
                -- ^ Sets to 'LATIN1', 'UTF8', 'UTF8-BOM', 'UTF16BE',
                -- or 'UTF16LE' to control the character set. Use of
                -- 'UTF8-BOM' is discouraged.
             , trimTrailingWhitespace :: Maybe Bool
                -- ^ Sets to 'True' to remove any whitespace characters
                -- preceding newline characters and 'False' to ensure it
                -- doesn't.
             , insertFinalNewline     :: Maybe Bool
                -- ^ Sets to 'True' to ensure file ends with a newline when
                -- saving and 'False' to ensure it doesn't.
             , root                   :: Maybe Bool
                -- ^ Special property that should be specified at the top of
                -- the file outside of any sections. Sets to 'True' to stop
                -- .editorconfig files search on current file.
             } deriving (Eq)

emptyProperties :: Properties
emptyProperties =
  Properties { indentStyle=Nothing
             , indentSize=Nothing
             , tabWidth=Nothing
             , endOfLine=Nothing
             , charset=Nothing
             , trimTrailingWhitespace=Nothing
             , insertFinalNewline=Nothing
             , root=Nothing
             }

instance Monoid Properties where
  mempty = emptyProperties
  p1 `mappend` p2 =
    Properties { indentStyle=getFirst $ First (indentStyle p1) <> First (indentStyle p2)
               , indentSize=getFirst $ First (indentSize p1) <> First (indentSize p2)
               , tabWidth=getFirst $ First (tabWidth p1) <> First (tabWidth p2)
               , endOfLine=getFirst $ First (endOfLine p1) <> First (endOfLine p2)
               , charset=getFirst $ First (charset p1) <> First (charset p2)
               , trimTrailingWhitespace=getFirst $ First (trimTrailingWhitespace p1) <> First (trimTrailingWhitespace p2)
               , insertFinalNewline=getFirst $ First (insertFinalNewline p1) <> First (insertFinalNewline p2)
               , root=getFirst $ First (root p1) <> First (root p2)
               }

instance Show Properties where
  show Properties {..} = intercalate "\n" $ filter (not . null) $
    [ showProp "indent_style" indentStyle
    , showProp "indent_size" indentSize
    , showProp "tab_width" tabWidth
    , showProp "end_of_line" endOfLine
    , showProp "charset" charset
    , showProp "trim_trailing_whitespace" trimTrailingWhitespace
    , showProp "insert_final_newline" insertFinalNewline
    , showProp "root" root
    ]
    where
      showProp :: Show a => String -> Maybe a -> String
      showProp field (Just x) = field ++ ": " ++ show x
      showProp _ Nothing = ""

data EditorConfigSection =
  EditorConfigSection { sectionName :: Text
                      , pattern :: Pattern
                      , properties :: Properties
                      } deriving (Show)

newtype EditorConfig =
  EditorConfig { sections :: [EditorConfigSection] }
  deriving (Show)

emptyEditorConfig :: EditorConfig
emptyEditorConfig = EditorConfig { sections = [] }

instance Monoid EditorConfig where
  mempty = emptyEditorConfig
  (EditorConfig {sections=s1}) `mappend` (EditorConfig {sections=s2}) = EditorConfig {sections=s1 `mappend` s2}

mkProperties :: HashMap Text Text -> Properties
mkProperties map =
  let indentStyle            = HashMap.lookup "indent_style" map             >>= parseMaybe indentStyleParser
      indentSize             = HashMap.lookup "indent_size" map              >>= parseMaybe intParser
      tabWidth               = HashMap.lookup "tab_width" map                >>= parseMaybe intParser
      endOfLine              = HashMap.lookup "end_of_line" map              >>= parseMaybe endOfLineParser
      charset                = HashMap.lookup "charset" map                  >>= parseMaybe charsetParser
      trimTrailingWhitespace = HashMap.lookup "trim_trailing_whitespace" map >>= parseMaybe boolParser
      insertFinalNewline     = HashMap.lookup "insert_final_newline" map     >>= parseMaybe boolParser
      root                   = HashMap.lookup "root" map                     >>= parseMaybe boolParser
  in Properties{..}
  where
    parseMaybe :: Parser a -> Text -> Maybe a
    parseMaybe m s = either (const Nothing) Just $ parseOnly m s

    indentStyleParser :: Parser IndentStyle
    indentStyleParser =
          (string "tab" >> return Tab)
      <|> (string "space" >> return Space)

    endOfLineParser :: Parser EndOfLine
    endOfLineParser =
          (string "crlf" >> return CRLF)
      <|> (string "cr" >> return CR)
      <|> (string "lf" >> return LF)

    charsetParser :: Parser String
    charsetParser =
          (string "latin1" >> return "LATIN1")
      <|> (string "utf-8-bom" >> return "UTF8-BOM")
      <|> (string "utf-8" >> return "UTF8")
      <|> (string "utf-16be" >> return "UTF16BE")
      <|> (string "utf-16le" >> return "UTF16LE")

    intParser :: Parser Int
    intParser = read <$> many1 digit

    boolParser :: Parser Bool
    boolParser =
          (string "true" >> return True)
      <|> (string "false" >> return False)

isRootEditorConfig :: EditorConfig -> Bool
isRootEditorConfig (EditorConfig {sections=sections}) =
  let Just rootSection = find isRootSection sections
   in root (properties rootSection) == Just True
  where
    isRootSection (EditorConfigSection {sectionName=name}) = name == "[]"

readEditorConfig :: Ini -> EditorConfig
readEditorConfig (Ini iniSections) = EditorConfig $ HashMap.foldrWithKey f [] iniSections
  where
    f :: Text
      -> HashMap Text Text
      -> [EditorConfigSection]
      -> [EditorConfigSection]
    f key section acc = EditorConfigSection
      { sectionName = key
      , pattern = Glob.compile $ unpack key
      , properties = mkProperties section} : acc

parseEditorConfig :: Text -> Either String EditorConfig
parseEditorConfig = parseIni >=> (return . readEditorConfig)

readProperties :: EditorConfig -> FilePath -> Properties
readProperties (EditorConfig sections) path =
  let fileName = takeFileName path
      matchingSections = filter (match fileName) sections
   in foldMap properties matchingSections
  where
    match fileName EditorConfigSection{..} = sectionName == "[]" || Glob.match pattern fileName

loadEditorConfig :: FilePath -> ExceptT EditorConfigError IO EditorConfig
loadEditorConfig dirPath = do
  configPaths <- liftIO (editorConfigPaths dirPath)
  configs <- traverse loadSingleEditorConfig configPaths
  let mIndex = findIndex isRootEditorConfig configs
  case mIndex of
    Just index -> do let configs' = take (index + 1) configs
                     return $ fold configs'
    Nothing -> return $ fold configs
  where
    editorConfigFile :: FilePath
    editorConfigFile = ".editorconfig"

    editorConfigPaths :: FilePath -> IO [FilePath]
    editorConfigPaths dirPath = do
      let dirs  = reverse $ map joinPath (inits (splitPath dirPath))
          paths = map (</> editorConfigFile) dirs
      filterM doesFileExist paths

    loadSingleEditorConfig :: FilePath -> ExceptT EditorConfigError IO EditorConfig
    loadSingleEditorConfig path = do
      contents <- liftIO $ T.readFile path
      case parseEditorConfig contents of
        Left e   -> throwError (ParseError e)
        Right ec -> return ec

-- | Parse editorconfig files corresponding to the file path,
-- and return the parsing result.
getProperties :: FilePath -> IO (Either EditorConfigError Properties)
getProperties path = runExceptT (getProperties' path)
  where
    getProperties' :: FilePath -> ExceptT EditorConfigError IO Properties
    getProperties' path = do
      let dirPath = takeDirectory path
      editorConfig <- loadEditorConfig dirPath
      return $ readProperties editorConfig path

