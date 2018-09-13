module C12 where

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
