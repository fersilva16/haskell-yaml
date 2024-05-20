module HaskellYaml where

import Control.Applicative
import Control.Monad (MonadPlus (mzero))
import Data.Char (isAlpha, isSpace)
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as Map

data YamlScalar
  = YamlNull
  | YamlBool Bool
  | YamlInt Int
  | YamlFloat Float
  | YamlString String
  deriving (Eq, Show)

data Yaml
  = YamlScalar YamlScalar
  | YamlSequence [Yaml]
  | YamlStructure [Yaml]
  | YamlMap [(YamlScalar, Yaml)]
  deriving (Eq, Show)

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

(?>) :: Parser a -> Parser b -> Parser b
p1 ?> p2 = optional p1 *> p2

(<?) :: Parser a -> Parser b -> Parser a
p1 <? p2 = p1 <* optional p2

(+?>) :: Parser [a] -> Parser [a] -> Parser [a]
p1 +?> p2 = liftA2 f (optional p1) p2
  where
    f (Just x) y = x ++ y
    f Nothing y = y

(<?+) :: Parser [a] -> Parser [a] -> Parser [a]
p1 <?+ p2 = liftA2 f p1 (optional p2)
  where
    f x (Just y) = x ++ y
    f x Nothing = x

char :: Char -> Parser Char
char x = Parser f
  where
    f (y : ys) = if y == x then Just (ys, y) else Nothing
    f [] = Nothing

string :: String -> Parser String
string = traverse char

alternative :: String -> Parser Char
alternative = foldr ((<|>) . char) empty

digit :: Parser Char
digit = alternative ['0' .. '9']

decimal :: Parser Char
decimal = some (char '_') ?> digit <? some (char '_')

sign :: Parser String -> Parser String
sign p = (string "+" ?> p) <|> (string "-" +?> p)

int :: Parser Int
int = read <$> sign (some decimal)

scientificNotation :: Parser String
scientificNotation = string "e" +?> sign (some digit)

float :: Parser Float
float = read <$> sign (liftA3 (\x y z -> x ++ y ++ z) (some decimal) (string ".") (some decimal <?+ scientificNotation))

true :: Parser Bool
true = True <$ (string "true" <|> string "True" <|> string "TRUE")

false :: Parser Bool
false = False <$ (string "false" <|> string "False" <|> string "FALSE")

-- satisfyP :: (Char -> Bool) -> Parser Char
-- satisfyP f = Parser fp
--   where
--     fp (y : ys) = if f y then Just (ys, y) else Nothing
--     fp [] = Nothing

-- alphaP :: Parser String
-- alphaP = some . satisfyP $ isAlpha

yamlNull :: Parser YamlScalar
yamlNull = YamlNull <$ (string "null" <|> string "Null" <|> string "NULL" <|> string "~")

yamlBool :: Parser YamlScalar
yamlBool = YamlBool <$> (true <|> false)

yamlInt :: Parser YamlScalar
yamlInt = YamlInt <$> int

yamlFloat :: Parser YamlScalar
yamlFloat = YamlFloat <$> float

yamlScalar :: Parser Yaml
yamlScalar = YamlScalar <$> (yamlNull <|> yamlBool <|> yamlFloat <|> yamlInt)

yaml :: Parser Yaml
yaml = yamlScalar
