module IpV4 where

import Data.Word

import Control.Applicative ((<|>))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

data IpAddressV4 = IpAddressV4 Word32 deriving (Eq, Ord, Show)

type Parser = Parsec Void String

ipAddressV4Parser :: Parser IpAddressV4
ipAddressV4Parser = do
  digit1 <- integerParser
  dotParser
  digit2 <- integerParser
  dotParser
  digit3 <- integerParser
  dotParser
  digit4 <- integerParser
  return $ IpAddressV4 ((digit1 * 16777216) + (digit2 * 65536) + (digit3 * 256) + digit4)
  where 
    integerParser = (read <$> some digitChar)
    dotParser = char '.'
