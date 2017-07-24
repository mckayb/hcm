module Cmds.Remove where

import Data.String (String)
import Prelude ()
import System.IO (IO, putStrLn)

run :: [String] -> [String] -> IO ()
run args flags = putStrLn "Running"
