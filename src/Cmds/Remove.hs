module Cmds.Remove where

import Data.String (String)
import Prelude ()
import System.IO (IO, print, putStrLn)

run :: [String] -> [String] -> IO ()
run args flags = do
  print args
  print flags
  putStrLn "Running"
