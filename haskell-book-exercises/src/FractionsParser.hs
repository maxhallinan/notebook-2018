{-# LANGUAGE OverloadedStrings #-}

module FractionsParser where

import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Data.Ratio ((%))
import Data.String (IsString)
import Text.Trifecta

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: (Monad m, TokenParsing m) => m Rational
parseFraction = do
  -- decimal :: Integral a => Parser a
  numerator <- decimal
  -- char :: Char => Parser Char
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be 0"
    _ -> return (numerator % denominator)

main :: IO ()
main = do
  -- polymorphic parsing: the same parser can be run by both Trifecta and 
  -- Attoparsec and any library that has an instance of TokenParsing
  let attoParse = parseOnly parseFraction
  print $ attoParse shouldWork
  print $ attoParse shouldAlsoWork
  print $ attoParse badFraction
  print $ attoParse alsoBad

  let parseFraction' = parseString parseFraction mempty
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' badFraction
  print $ parseFraction' alsoBad
