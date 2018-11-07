module Digit where

import Control.Applicative ((<|>))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

positiveInteger :: Parser Integer
positiveInteger = do
  digits <- some parseDigit
  return $ read digits

negativeInteger :: Parser Integer
negativeInteger =  hyphen >> (negate <$> positiveInteger)
  where hyphen = char '-'

integerParser :: Parser Integer
integerParser = negativeInteger <|> positiveInteger
