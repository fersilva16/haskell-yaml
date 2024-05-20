module HaskellYaml where

import Control.Applicative
import Control.Monad (MonadPlus (mzero))
import Data.Char (isAlpha)
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

digitP :: Parser Char
digitP = alternativeP ['0' .. '9']

intCanonicalP :: Parser Int
intCanonicalP = read <$> some digitP

intDecimalP :: Parser Int
intDecimalP = read <$> some (digitP <|> (some (charP '_') *> digitP))

intP :: Parser Int
intP = intDecimalP <|> intCanonicalP 

intPositiveP :: Parser Int
intPositiveP = charP '+' *> intP

intNegativeP :: Parser Int
intNegativeP = ((-1) *) <$> (charP '-' *> intP)

floatCanonicalP :: Parser Float
floatCanonicalP = read <$> liftA3 (\x y z -> x ++ y ++ z) (some digitP) (stringP ".") (some digitP)

floatDecimalP :: Parser Float
floatDecimalP = let
  decimalP = digitP <|> (some (charP '_') *> digitP)
  in read <$> liftA3 (\x y z -> x ++ y ++ z) (some decimalP) (stringP ".") (some decimalP)

floatP :: Parser Float
floatP = floatCanonicalP <|> floatDecimalP

floatPositiveP :: Parser Float
floatPositiveP = charP '+' *> floatP

floatNegativeP :: Parser Float
floatNegativeP = ((-1) *) <$> (charP '-' *> floatP)

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
yamlIntP = YamlInt <$> (intP <|> intPositiveP <|> intNegativeP)

yamlFloatP :: Parser YamlScalar
yamlFloatP = YamlFloat <$> (floatP <|> floatPositiveP <|> floatNegativeP)

yamlScalarP :: Parser Yaml
yamlScalarP = YamlScalar <$> (yamlNullP <|> yamlBoolP <|> yamlFloatP <|> yamlIntP)

yamlP :: Parser Yaml
yamlP = yamlScalarP
