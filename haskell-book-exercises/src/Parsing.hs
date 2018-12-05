module Parsing where

import Control.Applicative
import Text.Trifecta

-- define a simple language for arithmetic expressions

data Expr 
  = Add Expr Expr
  | Lit Integer
  deriving (Show)

-- <|> returns the result of the first parser to succeed
parseExpr :: Parser Expr
parseExpr = parseAdd <|> parseLit
  where 
    parseAdd = parens $ do
      x <- parseExpr
      -- `symbolic` is a "tokenizing" function that parses a Char and then skips
      -- trailing whitespace.
      -- it is useful to use tokenizing functions to that the parser isn't 
      -- cluttered with `skipWhitespace` calls.
      -- `char` would parse a char without skipping whitespace.
      _ <- symbolic '+'
      y <- parseExpr
      pure (Add x y)
    parseLit = Lit <$> integer

testExprParser :: IO ()
testExprParser = print $ parseString parseExpr mempty "(1 + (2 + (3 + 4)))"
-- Success (Add (Lit 1) (Add (Lit 2) (Add (Lit 3) (Lit 4))))

data Fraction 
  = Div Fraction Fraction
  | Lit' Integer
  deriving (Show)

parseFraction :: Parser Fraction
parseFraction = parseDiv <|> parseLit'
  where
    parseDiv = parens $ do
      x <- parseFraction
      _ <- symbolic '/'
      y <- parseFraction
      pure (Div x y)
    parseLit' = Lit' <$> integer

testFractionParser :: IO ()
testFractionParser = do 
  print $ parseString parseFraction mempty "(1 / (1 / 2))"

type NumberOrString = Either Integer String

parseNos :: Parser NumberOrString
parseNos = do (Left <$> integer) <|> (Right <$> some letter)

testNosParser :: IO ()
testNosParser = do
  let testParser parser string = parseString parser mempty string
  print $ testParser parseNos "blah" 
  -- Success (Right "blah")
  print $ testParser parseNos "123"
  -- Success (Left 123)
  print $ testParser parseNos "123blah789"
  -- Success (Left 123)
  -- many is a 0 or more Alternative operation
  print $ testParser (many parseNos) "123blah789"
  -- Success [Left 123,Right "blah",Left 789]
  print $ testParser (many parseNos) ""
  -- Success []
  -- some is a 1 or more Alternative operation
  print $ testParser (some parseNos) "123blah789"
  -- Success [Left 123,Right "blah",Left 789]
  print $ testParser (some parseNos) ""
  -- why does some not work here
  -- Failure (ErrInfo {_errDoc = (interactive):1:1: error: unexpected
  --     EOF, expected: integer, letter
  -- <EOF>

newlineInteger :: String
newlineInteger = "\n123\n456\n789\n"

newlineIntegerParser1 :: Parser NumberOrString
newlineIntegerParser1 = skipMany (oneOf "\n") >> (Left <$> integer)

newlineIntegerParser2 :: Parser NumberOrString
newlineIntegerParser2 = do
  skipNewline
  v <- (Left <$> integer) <|> (Right <$> some letter)
  skipNewline
  return v
  where skipNewline = skipMany (oneOf "\n")

testIntegerWithNewlineParser :: IO ()
testIntegerWithNewlineParser = do
  print $ parseString newlineIntegerParser1 mempty newlineInteger
  -- Success (Left 123)
  print $ parseString (some newlineIntegerParser2) mempty newlineInteger
  -- Success [Left 123,Left 456,Left 789]

