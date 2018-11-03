{-# LANGUAGE OverloadedStrings #-}

module C24Backtrack where

import Control.Applicative
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Text.Trifecta hiding (parseTest)
import Text.Parsec (Parsec, parseTest)

runTrifectaParser :: Show a => Parser a -> String -> IO ()
runTrifectaParser parser input = print $ parseString parser mempty input

runParsecParser :: Show a => Parsec String () a -> String -> IO ()
runParsecParser parser input = parseTest parser input

runAttoparsecParser :: Show a => A.Parser a -> ByteString -> IO ()
runAttoparsecParser parser input = print $ parseOnly parser input

withoutBacktrackParser :: (Monad f, CharParsing f) => f Char
withoutBacktrackParser = (char '1' >> char '2') <|> char '3'

-- this parser returns the cursor to point where the parser failed
withBacktrackParser :: (Monad f, CharParsing f) => f Char
withBacktrackParser = try (char '1' >> char '2') <|> char '3'

annotatedBacktraceParser :: (Monad f, CharParsing f) => f Char
annotatedBacktraceParser = 
  parse12 <|> parse3
  where 
    parse12 = (try (char '1' >> char '2')) <?> "I was expecting something like 12"
    parse3  = (char '3') <?> "I was expecting something like 3"

testParsers :: IO ()
testParsers = do
  print "-- Trifecta ---"
  runTrifectaParser withoutBacktrackParser "13"
  runTrifectaParser withBacktrackParser "13"
  runTrifectaParser annotatedBacktraceParser "13"
  putStrLn ""
  
  print "-- Parsec ---"
  runParsecParser withoutBacktrackParser "13"
  runParsecParser withBacktrackParser "13"
  runParsecParser annotatedBacktraceParser "13"
  putStrLn ""

  print "-- Attoparsec ---"
  runAttoparsecParser withoutBacktrackParser "13"
  runAttoparsecParser withBacktrackParser "13"
  runAttoparsecParser annotatedBacktraceParser "13"
  putStrLn ""
