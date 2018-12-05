module PhoneNumber where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

newtype AreaCode = AreaCode Int deriving (Eq, Show)
newtype Exchange = Exchange Int deriving (Eq, Show)
newtype Line = Line Int deriving (Eq, Show)

data PhoneNumber = PhoneNumber 
  { areaCode :: AreaCode
  , exchange :: Exchange
  , line :: Line
  } 
  deriving (Eq, Show)

parseNDigits :: Int -> Parser Int
parseNDigits n = read <$> count n digitChar

parseThreeDigits :: Parser Int
parseThreeDigits = parseNDigits 3

parseFourDigits :: Parser Int
parseFourDigits = parseNDigits 4

parseAreaCode :: Parser AreaCode
parseAreaCode = AreaCode <$> parseThreeDigits

parseExchange :: Parser Exchange
parseExchange = Exchange <$> parseThreeDigits

parseLine :: Parser Line
parseLine = Line <$> parseFourDigits

-- 1234567890
parsePhoneNumber1 :: Parser PhoneNumber
parsePhoneNumber1 = 
  PhoneNumber <$> parseAreaCode <*> parseExchange <*> parseLine

-- 123-456-7890
parsePhoneNumber2 :: Parser PhoneNumber
parsePhoneNumber2 = do
  areaCode <- parseAreaCode 
  char '-'
  exchange <- parseExchange 
  char '-'
  line <- parseLine
  return $ PhoneNumber areaCode exchange line

-- 1-123-456-7890
parsePhoneNumber3 :: Parser PhoneNumber
parsePhoneNumber3 = string "1-" >> parsePhoneNumber2

-- (123) 456-7890
parsePhoneNumber4 :: Parser PhoneNumber
parsePhoneNumber4 = do
  char '('
  areaCode <- parseAreaCode
  char ')'
  space
  exchange <- parseExchange 
  char '-'
  line <- parseLine
  return $ PhoneNumber areaCode exchange line

parsePhoneNumber :: Parser PhoneNumber
parsePhoneNumber = 
      (try parsePhoneNumber1)
  <|> (try parsePhoneNumber2)
  <|> (try parsePhoneNumber3)
  <|> parsePhoneNumber4

testPhoneNumberParser :: IO ()
testPhoneNumberParser = do
  parseTest parsePhoneNumber "1234567890"
  parseTest parsePhoneNumber "123-456-7890"
  parseTest parsePhoneNumber "1-123-456-7890"
  parseTest parsePhoneNumber "(123) 456-7890"
