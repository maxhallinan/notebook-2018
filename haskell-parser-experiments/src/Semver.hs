module Semver where

import Control.Applicative ((<|>))
import Data.Foldable
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

data Identifier = IntegerId Integer | StringId String deriving (Eq, Ord, Show)

data MetaData = MetaData [Identifier] deriving (Eq, Ord, Show)

data PreRelease = PreRelease [Identifier] deriving (Eq, Ord, Show)

newtype Patch = Patch Integer deriving (Eq, Ord, Show)

newtype Minor = Minor Integer deriving (Eq, Ord, Show)
 
newtype Major = Major Integer deriving (Eq, Ord, Show)

data Version = Version 
  { majorVersion :: Major
  , minorVersion :: Minor
  , patchVersion :: Patch
  , preReleaseVersion :: PreRelease 
  , versionMetaData :: MetaData 
  } deriving (Eq, Show)

instance Ord Version where
  (<=) (Version major1 minor1 patch1 _ _) (Version major2 minor2 patch2 _ _) =
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
-- 1.0.0-0.3.7, 1.0.0-x.7.z.92

identifierParser :: Parser Identifier
identifierParser = integerIdParser <|> stringIdParser
  where
    integerIdParser = (IntegerId . read) <$> (someTill digitChar (char '.' <|> eof :: Parser Char))
    stringIdParser  = StringId <$> (someTill (alphaNumChar <|> char '-') (char '.' <|> char ' '))

segmentParser :: Parser [Identifier]
segmentParser = identifierParser `sepBy` dotParser

-- Build metadata MAY be denoted by appending a plus sign and a series of dot 
-- separated identifiers immediately following the patch or pre-release version. 
-- Identifiers MUST comprise only ASCII alphanumerics and hyphen [0-9A-Za-z-]. 
-- Identifiers MUST NOT be empty. Build metadata SHOULD be ignored when 
-- determining version precedence. Thus two versions that differ only in the 
-- build metadata, have the same precedence. Examples: 1.0.0-alpha+001, 
-- 1.0.0+20130313144700, 1.0.0-beta+exp.sha.5114f85.

metaDataParser :: Parser MetaData
metaDataParser = MetaData <$> metaDataParser'
  where metaDataParser' = option [] (char '+' >> segmentParser)

-- Question:
-- 1. See if there is a '-' character immediately after the patch version.
-- 2. If there is, then parse what follows until I hit a '+'.
-- 3. If '+' is found, use the metadata parser.
-- 4. Don't fail if neither are found.
-- 5. Don't fail if only one is found.

-- Question:
-- How do I allow for an unbounded pattern of identifier followed by dot where
-- there is no dot at the beginning or the end of the segment?
-- Answer:
-- Use `skipMany p`: "applies the parser p zero or more times, skipping its 
-- result."
-- Or use the `sepBy` combinator.

preReleaseParser :: Parser PreRelease
preReleaseParser = PreRelease <$> preReleaseParser'
  where preReleaseParser' = option [] (char '-' >> segmentParser)

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
  metaData <- metaDataParser
  eof
  return $ Version major minor patch preRelease metaData

testParser :: IO ()
testParser = do
  parseTest versionParser "1.0.0"
  parseTest versionParser ".1.0.0."
  parseTest versionParser "1.0.0-alpha"
  parseTest versionParser "1.0.0-alpha.beta.1"
