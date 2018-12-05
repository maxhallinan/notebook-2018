module CipherA where

import qualified Data.Char

-- better solution: https://codereview.stackexchange.com/questions/158663/caesar-and-vigen%C3%A8re-ciphers-in-haskell-the-simple-way
caesar :: Int -> String -> String
caesar n = map helper
  where
    helper ' ' = ' ' 
    -- 1. lower case character
    -- 2. get ord of character
    -- 3. reduce ord of character by "base" + number of characters to shift
    -- 4. modulo the result by 26
    -- 5. add the base
    -- 6. transform number to character to get shifted character
    helper c   = Data.Char.chr $ (Data.Char.ord (Data.Char.toLower c) - base + n) `mod` 26  + base
    base       = Data.Char.ord 'a'

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
