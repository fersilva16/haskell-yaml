module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment (getArgs)

data YamlValue
  = YamlNull
  | YamlBool Bool
  | YamlNumber Integer
  | YamlString String
  | YamlList [YamlValue]
  | YamlScalar [(String, YamlValue)]
  deriving (Show)

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p1) =
    Parser $ \y -> do
      (y', x) <- p1 y
      return (y', f x)

instance Applicative Parser where
  pure x = Parser (\y -> Just (y, x))
  (Parser p1) <*> (Parser p2) =
    Parser $ \y -> do
      (y', f) <- p1 y
      (y'', a) <- p2 y'
      return (y'', f a)

charP :: Char -> Parser Char
charP x = Parser f
  where
    f y
      | head y == x = Just (tail y, x)
      | otherwise = Nothing

stringP :: String -> Parser String
stringP = traverse charP

main :: IO ()
main = do
  args <- getArgs
  content <- readFile $ unwords args

  print content
