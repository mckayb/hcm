{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Cmds.Add (run)
import qualified Cmds.Remove (run)
import qualified Cmds.Lock (run)
import qualified Cmds.Run (run)
import Data.Bool (not)
import Data.Eq (Eq, (==))
import Data.Foldable (foldl)
import Data.Function (($), (.))
import Data.List (filter, head, length, null, tail, take)
import Data.Semigroup ((<>))
import Data.String (String)
import Prelude ()
import System.Environment (getArgs)
import System.IO (IO, print, putStrLn)
import Text.Show (Show)

main :: IO ()
main = do
  cmdLineArgs <- getArgs
  print cmdLineArgs
  case parseArgs cmdLineArgs of
    Help -> showHelp
    Version -> showVersion
    Cmd Options {cmd, args, flags} ->
      case cmd of
        "add" -> Cmds.Add.run args flags
        "install" -> Cmds.Add.run args flags
        "remove" -> Cmds.Remove.run args flags
        "uninstall" -> Cmds.Remove.run args flags
        "lock" -> Cmds.Lock.run args flags
        "run" -> Cmds.Run.run args flags
        _ -> do
          showError cmdLineArgs
          showHelp
    Error -> do
      showError cmdLineArgs
      showHelp

parseArgs :: [String] -> ParseResult
parseArgs args =
  case args of
    [] -> Help
    ["--help"] -> Help
    ["--version"] -> Version
    _ ->
      if null cmds
        then Error
        else Cmd Options {cmd = head cmds, args = tail cmds, flags}
  where
    isPrefix pre str = take (length pre) str == pre
    cmds = filter (not . isPrefix "-") args
    flags = filter (isPrefix "-") args

showHelp :: IO ()
showHelp = putStrLn "Help String"

showVersion :: IO ()
showVersion = putStrLn "Version String"

showError :: [String] -> IO ()
showError badArgs = putStrLn $ "Error: " <> foldl concatWithSpace "" badArgs

concatWithSpace :: String -> String -> String
concatWithSpace "" b = b
concatWithSpace a b = a <> " " <> b

data Options = Options
  { cmd :: String
  , args :: [String]
  , flags :: [String]
  } deriving (Show, Eq)

data ParseResult
  = Help
  | Version
  | Cmd Options
  | Error