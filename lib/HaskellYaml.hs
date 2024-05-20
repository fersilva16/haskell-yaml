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

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys) = if y == x then Just (ys, y) else Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

alternativeP :: String -> Parser Char
alternativeP = foldr ((<|>) . charP) empty

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

digitP :: Parser Char
digitP = alternativeP ['0' .. '9']

decimalP :: Parser Char
decimalP = some (charP '_') ?> digitP <? some (charP '_')

signP :: Parser String -> Parser String
signP p = (stringP "+" ?> p) <|> (stringP "-" +?> p)

intP :: Parser Int
intP = read <$> signP (some decimalP)

scientificNotationP :: Parser String
scientificNotationP = stringP "e" +?> signP (some digitP)

floatP :: Parser Float
floatP = read <$> signP (liftA3 (\x y z -> x ++ y ++ z) (some decimalP) (stringP ".") (some decimalP <?+ scientificNotationP))

trueP :: Parser Bool
trueP = True <$ (stringP "true" <|> stringP "True" <|> stringP "TRUE")

falseP :: Parser Bool
falseP = False <$ (stringP "false" <|> stringP "False" <|> stringP "FALSE")

-- satisfyP :: (Char -> Bool) -> Parser Char
-- satisfyP f = Parser fp
--   where
--     fp (y : ys) = if f y then Just (ys, y) else Nothing
--     fp [] = Nothing

-- alphaP :: Parser String
-- alphaP = some . satisfyP $ isAlpha

yamlNullP :: Parser YamlScalar
yamlNullP = YamlNull <$ (stringP "null" <|> stringP "Null" <|> stringP "NULL" <|> stringP "~")

yamlBoolP :: Parser YamlScalar
yamlBoolP = YamlBool <$> (trueP <|> falseP)

yamlIntP :: Parser YamlScalar
yamlIntP = YamlInt <$> intP

yamlFloatP :: Parser YamlScalar
yamlFloatP = YamlFloat <$> floatP

yamlScalarP :: Parser Yaml
yamlScalarP = YamlScalar <$> (yamlNullP <|> yamlBoolP <|> yamlFloatP <|> yamlIntP)

yamlP :: Parser Yaml
yamlP = yamlScalarP
