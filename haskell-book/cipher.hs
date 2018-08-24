module Cipher where

import qualified Data.Char

chars = ['A'..'Z'] ++ ['a'..'z']

charOrds = fmap Data.Char.ord chars

shiftWith :: (Int -> Int) -> Char -> Char
shiftWith f = Data.Char.chr . f . Data.Char.ord
      
leftShift :: Int -> Char -> Char
leftShift 0 char = char
leftShift place 'A' = leftShift (place - 1) 'Z'
leftShift place 'a' = leftShift (place - 1) 'z'
leftShift place char 
  | not (elem char chars) = char
  | otherwise = leftShift (place - 1) (shiftWith (subtract 1) char)

leftCaesar :: Int -> String -> String
leftCaesar shift source = fmap (leftShift shift) source

rightShift :: Int -> Char -> Char
rightShift 0 char = char
rightShift place 'Z' = rightShift (place - 1) 'A'
rightShift place 'z' = rightShift (place - 1) 'a'
rightShift place char 
  | not (elem char chars) = char
  | otherwise = rightShift (place - 1) (shiftWith (+ 1) char)

rightCaesar :: Int -> String -> String
rightCaesar shift source = fmap (rightShift shift) source
