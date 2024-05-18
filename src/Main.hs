module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  content <- readFile $ unwords args

  print content
