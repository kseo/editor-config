import Test.Hspec

import           Data.Either
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Directory
import           System.FilePath

import Data.EditorConfig

main :: IO ()
main = hspec $ do
  describe "getProperties" $ do
    it "looks for a file named .editorconfig in the directory of the opened file and in every parent directory" $ do
      Right props <- getProperties "test/ec/foo/bar/a.py"
      props `shouldBe` Properties
        { indentStyle = Just Space
        , indentSize = Just 4
        , tabWidth = Nothing
        , endOfLine = Just LF
        , charset = Just "UTF8"
        , trimTrailingWhitespace = Nothing
        , insertFinalNewline = Just True
        , root = Just True
        }
