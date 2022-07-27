module Main where

import Control.Applicative
import Control.Monad (MonadPlus (mzero))
import Data.Char (isAlpha)
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (satisfy)

data YamlValue
  = YamlNull
  | YamlBool Bool
  | YamlInteger Integer
  | YamlFloat Float
  | YamlString String
  | YamlList [YamlValue]
  | YamlMapping (String, YamlValue)
  | YamlMap [YamlMapping]
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

instance Alternative Parser where
  empty = Parser (const Nothing)
  (Parser p1) <|> (Parser p2) = Parser $ \y -> p1 y <|> p2 y

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys) = if y == x then Just (ys, y) else Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

alternativeP :: String -> Parser Char
alternativeP = foldr ((<|>) . charP) empty

digitP :: Parser Char
digitP = alternativeP ['0' .. '9']

intP :: Parser Int
intP = read <$> some digitP

satisfyP :: (Char -> Bool) -> Parser Char
satisfyP f = Parser fp
  where
    fp (y : ys) = if f y then Just (ys, y) else Nothing
    fp [] = Nothing

alphaP :: Parser String
alphaP = some . satisfyP $ isAlpha

yamlStringP :: Parser YamlValue
yamlStringP = YamlString <$> alphaP

yamlMappingP :: Parser YamlValue
yamlMappingP = YamlMapping <$> liftA2 (,) (alphaP <* stringP ": ") yamlStringP

main :: IO ()
main = do
  args <- getArgs
  content <- readFile $ unwords args

  print content
