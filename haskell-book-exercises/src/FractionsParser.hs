{-# LANGUAGE OverloadedStrings #-}

module FractionsParser where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
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
  let parseFraction' = parseString parseFraction mempty
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' badFraction
  print $ parseFraction' alsoBad
