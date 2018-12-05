module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/words.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in     l >= minWordLength
              && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wordList) = do
  randomIndex <- randomRIO (0, listLength - 1)
  return $ wordList !! randomIndex
  where listLength = length wordList

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) = 
    discoveredStr ++ " Guessed so far: " ++ guessed
    where discoveredStr = intersperse ' ' $ fmap renderPuzzleChar discovered

freshPuzzle :: String -> Puzzle
freshPuzzle answer = Puzzle answer discovered guesses 
  where 
    discovered  = fmap (\_ -> Nothing) answer
    guesses     = []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle answer _ _) char = elem char answer

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) char = elem char guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just char) = char

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle answer discovered guesses) guess = 
  Puzzle answer newDiscovered newGuesses
  where 
    newGuesses = guess : guesses
    newDiscovered = zipWith testGuess answer discovered
    testGuess _ (Just c) = Just c
    testGuess answerChar _ 
      | guess == answerChar = Just guess
      | otherwise           = Nothing

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of 
    (_, True) -> do
      putStrLn "You already guess that character. Pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "Good guess!"
      return (fillInCharacter puzzle guess)    
    (False, _) -> do
      putStrLn "Sorry, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle answer _ guesses) =
  if (length incorrectGuesses) > 7 then
    do putStrLn "You lose :("
       putStrLn $ "The word was " ++ answer
       exitSuccess
  else 
      do 
         putStrLn $ "Remaining guesses: " ++ show (7 - (length incorrectGuesses))
         return ()
  where incorrectGuesses  = filter isIncorrect guesses
        isIncorrect       = not . (flip elem answer)

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of 
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character."

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
