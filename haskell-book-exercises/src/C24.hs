{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module C24 where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta
import Text.Parser.Combinators

-- Parsing transforms a serialized data format (string or binary) into a -- structured data format like a Tree.  -- Parsers conform to rules that are specified in a grammar.
-- A parser combinator takes multiple parsers and makes them into a single
-- parser.
-- One of the hardest problems in parsing is writing expressive _and_ performant
-- parsers.

stop :: Parser a
-- `unexpected` throws an error with the error message "stop"
stop = unexpected "stop"

one = char '1'

one' = one >> stop

-- type Parser a = String -> Maybe (a, String)
-- Parser a is an alias for a function
-- The function takes a String
-- Returns the result of parsing that string `a` and whatever is left of the
-- input string once the parsing is finished.
-- `Nothing` means that parsing failed.
-- newtype Reader r a = Reader { runReader :: r -> a }
-- newtype State s a = State { runState :: s -> (a, s) }
-- type Parser a = String -> Maybe (a, String)
-- Parser is similar to State
-- State expands on the idea of Reader.
-- Parser is a bit like State.
-- Parser awaits a String input and produces a new state as a result of parsing it.
-- put :: Monad m => s -> StateT s m ()
-- put returns a unit value `()`. it's just used to modify the state.
-- the char parser doesn't have a real return value. it just modifies the state of
-- the parser.
-- the state of a parser is its location in the input stream.

-- char' :: Char -> Parser Char
-- char' c =
--   Parser $ \ s ->
--     case s of
--       (x:xs) -> if c == x
--                 then Just (c, xs)
--                 else Nothing
--       _ -> Nothing
--


-- reads in two characters if they are 1 and 2
oneTwo = char '1' >> char '2'

-- read characters '1' and '2', then die with an error
oneTwo' = oneTwo >> stop

testParser :: Show a => Parser a -> String -> IO ()
testParser parser s =
  print $ parseString parser mempty s

-- Exercises: Practice Parsing

-- 1.

failEof1 = (one >> eof)

-- fails because it expects to find EOF after "1"
test1 = do testParser failEof1

failEof2 = (oneTwo >> eof)

-- fails because it expects to find EOF after "12"
test2 = do testParser failEof2

parseStrings1 :: Parser String
parseStrings1 = string "1"

parseStrings12 :: Parser String
parseStrings12 = string "12"

parseStrings123 :: Parser String
parseStrings123 = string "123"

parseStrings2 :: Parser String
parseStrings2 = parseStrings1 >> parseStrings12 >> parseStrings123

-- parseStrings :: Parser String
-- parseStrings = string "1" <|> string "12" <|> string "123"

allThree :: Parser String
allThree = do
  c <- (show <$> integer)
  -- c <- char '1'
  -- c <- char '2'
  -- c <- char '3'
  _ <- eof
  return c

-- Prerequisite knowledge
-- - Parser has an instance of Monad
-- - Parser has an intance of Monoid
-- string' :: Parser String
-- string' s = go s mempty
  -- iterate over each character in the string
  -- the second argument to go is the parser result
  -- start with the mempty, in this case ""
  -- where
    -- what does char x do?
    -- create a Parser Char for each character in the string
    -- run that Parser, append the char to the parsed string
    -- keep doing this until you hit the base case
    -- go (x:xs) parsed = char x >>= (\x' -> go xs (parsed ++ [x']))
    -- the base case
    -- this lifts the result of the parsing into the Parser monad
    -- parsed :: String
    -- return parsed :: Parser String
    -- go [] parsed     = return parsed

-- describes a string of integers
-- but this parser returns the result of eof, which is unit.
parseFoo :: Parser ()
parseFoo = integer >> eof

-- to keep the integers but still enfoce eof, then use this:
parseFoo' :: Parser Integer
parseFoo' = do
  n <- integer
  _ <- eof
  return n

-- parseFoo' is equivalent to this:
parseFoo'' :: Parser Integer
parseFoo'' = integer >>= (\ n -> eof >> return n)

foo :: IO ()
foo = do
  print $ parseString (parseFoo'') mempty "123"
  print $ parseString (parseFoo'') mempty "123abc"

headerExample :: ByteString
headerExample = "[foo]"

newtype Header = Header String deriving (Eq, Ord, Show)

betweenBrackets :: Parser a -> Parser a
betweenBrackets p =
  openBracket *> p <* closeBracket 
  where openBracket   = char '['
        closeBracket  = char ']'

headerParser :: Parser Header
headerParser =
  betweenBrackets headerName
  where headerName = Header <$> some letter

assignmentExample1 :: ByteString
assignmentExample1 = "foo=one"

assignmentExample2 :: ByteString
assignmentExample2 = "foo=one\nbar=two\nbaz=three"

type Name = String
type Value = String
type Assignments = Map Name Value

assignmentParser :: Parser (Name, Value)
assignmentParser = do
  name <- some letter
  _ <- char '='
  value <- some letter
  skipEOL
  return (name, value)
  
skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

testHeaderParser :: IO ()
testHeaderParser = do
  print $ parseByteString headerParser mempty headerExample

testAssignmentParser :: IO ()
testAssignmentParser = do
  print $ parseByteString assignmentParser mempty assignmentExample1
  print $ parseByteString (some assignmentParser) mempty assignmentExample2

commentExample = "; this is a single line of comments\n;this is another line of comments.\nfoo=bar"

skipComment :: Parser ()
skipComment = skipMany $ do
  char ';' <|> char '#'
  skipMany (noneOf "\n")
  skipEOL

testSkipComment :: IO ()
testSkipComment = do
  print $ parseByteString (some skipComment >> assignmentParser) mempty commentExample

sectionExample = [r|
; this is a comment
[sectionone]
foo=one
bar=two
baz=three

[sectiontwo]
qux=four
|]

data Section = Section Header Assignments deriving (Eq, Show)

newtype Config = Config (Map Header Assignments) deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace = do
  skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComment
  header <- headerParser
  skipEOL
  assignments <- some assignmentParser
  return $ Section header (M.fromList assignments)

testParseSection :: IO ()
testParseSection = do
  print $ parseByteString parseSection mempty sectionExample

rollupSections :: Section -> Map Header Assignments -> Map Header Assignments
rollupSections (Section header assignments) sections =
  M.insert header assignments sections

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let sectionsMap = foldr rollupSections M.empty sections   
  return (Config sectionsMap)

testParseIni :: IO ()
testParseIni = do
  print $ parseByteString parseIni mempty sectionExample

-- Tokenizing parsers

-- parseString digit mempty "123"
-- Success "1"

-- parseString (some digit) mempty "123 456"
-- Success "123"

-- parseString (some (some digit)) mempty "123 456"
-- Success ["123"]

-- `digit` stops at whitepspace

-- parseString integer mempty "123 456"
-- Success 123

-- parseString (some integer) mempty "123 456"
-- Success [123, 456]

-- parseString (some integer) mempty "123\n \n456"
-- Success [123, 456]

-- `integer` does not stop at whitespace.
-- `integer` is a tokenizer.
-- `digit` is not a tokenizer.

-- applying `token` to `some digit` does have the expected result
-- parseString (token (some digit)) mempty "1\n2\n3"
-- Success "123"

-- parseString (some (token (some digit))) mempty "1\n2\n3"
-- Success ["1", "2", "3"]

tokenizedDigit' :: Parser [Integer]
tokenizedDigit' = some $ do
  -- Token parses trailing whitespace first and then applies the given parser.
  -- In this case, that is `some digit`
  -- `parseString (some (token digit)) mempty "\n1\n2\n3"` fails because the 
  -- whitespace is at the beginning.
  -- That's why the docs say: The only point where the whiteSpace parser should 
  -- be called explicitly is the start of the main parser in order to skip any 
  -- leading white space."
  -- `parseString (some (token digit)) mempty "1\n2\n3"` results in Success "123"
  -- The reason is that some is Alternative f => f a -> f [a]
  -- digit :: CharParsing m => m Char
  -- so `m Char` becomes `m [Char]`
  -- [Char] is String
  -- and `token digit` parses one digit character followed by any whitespace
  -- so `some (token digit)` consumes n digit characters interspersed with 
  -- whitespace.
  -- digits <- token digit will only return one digit Char
  -- digits <- some (token digit) will not chunk each set of digits separated 
  -- by whitespace as separate strings.
  -- this says give me a list of 1 or more digit characters but stop at whitespace
  digits <- token (some digit)
  -- read is like the reverse of show 
  -- read parses a string into a Haskell value
  -- `digit` parses a Char in the digit range
  -- `some digit` parses one or more digits, so [Char], i.e. String
  -- It seems like `some` takes an `a` and makes it an [a].
  -- Each new `a` is appended to the list.
  -- In this case, [a] is [Char], also known as String.
  -- That's why you get "123" from "1\n2\n3".
  -- So this takes a String and makes it a 
  return $ read digits
