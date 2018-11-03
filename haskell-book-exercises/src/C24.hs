module C24 where

import Control.Applicative
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
