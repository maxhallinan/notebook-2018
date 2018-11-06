module Semver where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

newtype Patch = Patch Integer deriving (Eq, Ord, Show)

newtype Minor = Minor Integer deriving (Eq, Ord, Show)
 
newtype Major = Major Integer deriving (Eq, Ord, Show)

data Version = Version Major Minor Patch deriving (Eq, Show)

instance Ord Version where
  (<=) (Version major1 minor1 patch1) (Version major2 minor2 patch2) =
    (major1 <= major2) && (minor1 <= minor2) && (patch1 <= patch2)

type Parser = Parsec Void String
  
integerParser :: Parser Integer
integerParser = read <$> some digitChar

patchParser :: Parser Patch
patchParser = Patch <$> integerParser

minorParser :: Parser Minor
minorParser = Minor <$> integerParser

majorParser :: Parser Major
majorParser = Major <$> integerParser

dotParser :: Parser Char
dotParser = char '.' 

versionParser :: Parser Version 
versionParser = do 
  major <- majorParser
  dotParser
  minor <- minorParser
  dotParser
  patch <- patchParser
  eof
  return $ Version major minor patch

testParser :: IO ()
testParser = do
  parseTest versionParser "1.0.0"
  parseTest versionParser ".1.0.0."
  parseTest versionParser "1.0.0."
