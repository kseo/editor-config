module Main where

import qualified Data.Text.IO as T
import Options.Applicative

import Data.EditorConfig

opts = info (helper <*> cmdOption)
  ( fullDesc
  <> progDesc "editor-config"
  <> header "a command line tool to test EditorConfig library" )
  where
    cmdOption = argument str (metavar "FILE")

main :: IO ()
main = do
  path <- execParser opts
  res <- getProperties path
  case res of
    Left e   -> putStrLn ("Error: " ++ show e)
    Right prop -> print prop
