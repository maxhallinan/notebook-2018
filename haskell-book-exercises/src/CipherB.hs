module CipherB where

import qualified Data.Char

offset :: Int -> Char -> Char 
offset n c = helper c
  where
    helper ' ' = ' '
    helper c   = Data.Char.chr $ (Data.Char.ord (Data.Char.toLower c) - base + n) `mod` 26 + base
    base       = Data.Char.ord 'a'

replaceString :: [Char] -> [Char] -> [Char]
replaceString as bs = go as bs ""
  where 
    go "" _ result = result
    go _ "" result = result    
    go xs ys result
      | head ys == ' ' = ' ' : go xs (tail ys) result
      | otherwise = head xs : go (tail xs) (tail ys) result

vign :: [Char] -> [Char] -> [Char]
vign keyword cleartext = cipherText
  where 
    kw          = foldr (++) "" (repeat keyword)
    replaced    = replaceString kw cleartext 
    ords        = fmap Data.Char.ord replaced
    zipped      = zip ords cleartext
    cipherText  = fmap (\(ord, char) -> offset ord char) zipped
