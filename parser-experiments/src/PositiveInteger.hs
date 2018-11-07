module PositiveInteger where

import Control.Applicative ((<|>))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

parseDigit :: Parser Char
parseDigit =
      char '0' 
  <|> char '1' 
  <|> char '2' 
  <|> char '3' 
  <|> char '4' 
  <|> char '5' 
  <|> char '6' 
  <|> char '7' 
  <|> char '8' 
  <|> char '9' 

base10Integer :: Parser Integer
base10Integer = do 
  digits <- some parseDigit
  let integer = foldr toInteger 0 (read <$> digits)
  return integer
  where 
    toInteger :: Integer -> Integer -> Integer
    toInteger integer digit = (integer * 10) + digit
