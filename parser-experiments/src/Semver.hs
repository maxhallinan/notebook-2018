module Semver where

import Control.Applicative ((<|>))
import Data.Foldable
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

data Identifier = IntegerId Integer | StringId String deriving (Eq, Ord, Show)

data PreRelease = PreRelease [Identifier] deriving (Eq, Ord, Show)

newtype Patch = Patch Integer deriving (Eq, Ord, Show)

newtype Minor = Minor Integer deriving (Eq, Ord, Show)
 
newtype Major = Major Integer deriving (Eq, Ord, Show)

data Version = Version Major Minor Patch deriving (Eq, Show)

instance Ord Version where
  (<=) (Version major1 minor1 patch1) (Version major2 minor2 patch2) =
    (major1 <= major2) && (minor1 <= minor2) && (patch1 <= patch2)

type Parser = Parsec Void String

-- A pre-release version MAY be denoted by appending a hyphen and a series of 
-- dot separated identifiers immediately following the patch version. 
-- Identifiers MUST comprise only ASCII alphanumerics and hyphen [0-9A-Za-z-]. 
-- Identifiers MUST NOT be empty. Numeric identifiers MUST NOT include leading 
-- zeroes. Pre-release versions have a lower precedence than the associated 
-- normal version. A pre-release version indicates that the version is unstable 
-- and might not satisfy the intended compatibility requirements as denoted by 
-- its associated normal version. Examples: 1.0.0-alpha, 1.0.0-alpha.1, 
-- 1.0.0-0.3.7, 1.0.0-x.7.z.92.

identifierParser :: Parser Identifier
identifierParser = integerIdParser <|> stringIdParser
  where
    integerIdParser = (IntegerId . read) <$> some digitChar
    stringIdParser  = StringId <$> some alphaNumChar

segmentParser :: Parser [Identifier]
segmentParser = some identifierParser

-- Question:
-- 1. See if there is a '-' character immediately after the patch version.
-- 2. If there is, then parse what follows until I hit a '+'.
-- 3. If '+' is found, use the metadata parser.
-- 4. Don't fail if neither are found.
-- 5. Don't fail if only one is found.

-- Question:
-- 1. How do I allow for an unbounded pattern of identifier followed by dot where
--    there is no dot at the beginning or the end of the segment?

preReleaseParser :: Parser PreRelease
preReleaseParser = do
  char '-'
  segments <- many (dotParser >> segmentParser)
  return $ PreRelease (fold segments)

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
  preRelease <- preReleaseParser
  eof
  return $ Version major minor patch

testParser :: IO ()
testParser = do
  parseTest versionParser "1.0.0"
  parseTest versionParser ".1.0.0."
  parseTest versionParser "1.0.0."
