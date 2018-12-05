module C12A where

import Control.Monad
import qualified Data.Char
import qualified System.Exit

palindrome :: IO ()
palindrome = forever $ do
  line <- getLine
  case (line == reverse line)  of
    True -> do
      putStrLn "It's a palindrome!"
    False -> do 
      putStrLn "Not a palindrome."
      System.Exit.exitSuccess

type Id' a = a -> a
type R' a f = a -> f a

-- kind: * -> *
id' :: a -> a
id' = undefined

-- kind: * -> *
r' :: a -> f a
r' = undefined

replaceWith :: [Char] -> [Char] -> [Char] -> [Char]
replaceWith pattern sub = unwords . fmap replaceWord . words
  where 
    replaceWord word = if word == pattern then sub else word

replaceThe = replaceWith "the" "a"

notThe :: String -> Maybe String
notThe word = if word == "the" then Nothing else Just word

replaceNotThe :: Maybe String -> String
replaceNotThe Nothing = "a"
replaceNotThe (Just word) = word

replaceThe2 :: String -> String
replaceThe2 = unwords . replaceThe . words 
  where replaceThe = fmap (replaceNotThe . notThe)
