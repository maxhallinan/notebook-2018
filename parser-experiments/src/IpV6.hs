module IpV6 where

import Data.Word

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data IpAddressV6 = IpAddressV6 Word32 Word32 deriving (Eq, Ord, Show)

ipAddressV6Parser :: Parser IpAddressV6
ipAddressV6Parser = undefined
