module Log where

import Data.Time
import Data.Map
import Control.Applicative ((<|>))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

newtype LogMessage = LogMessage String deriving (Eq, Show)

newtype LogTime = LogTime UTCTime deriving (Eq, Show)

type Log = Map LogTime LogMessage

parseTime :: Day -> Parser LogTime
parseTime day = do 
  hour <- read <$> doubleDigit
  char ':' 
  minute <- read <$> doubleDigit
  return $ LogTime (UTCTime day (hour*60*60 + minute*60 + 0))
  where doubleDigit = count 2 charDigit

{-
Log = LineComment | 

LineComment = --


-}

parseComment :: Parser ()
parseComment = skipLineComment $ string '--'

{-
  log = optionalSpace, optionalSpace, logHeader, logList, optionalSpace 
  logDay = logHeader, logList
  logList = logLine, logList
  logHeader = "#", space, logDate, eol | "#", logDate, eol
  logDate = digit, digit, digit, digit, "-", digit, digit, "-", digit, digit
  logLine = time, space, text, endOfLine
  time = digit, digit, ":", digit, digit
  endOfLine = eol | lineComment
  text = "" | character, text

  commentBlock = lineComment | lineComment, commentBlock
  lineComment  = "--", text, eol
-}
logParser :: Parser Log
logParser = undefined
