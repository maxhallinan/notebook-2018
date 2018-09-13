module C10 where

foldList :: (a -> b -> b) -> b -> [a] -> b
foldList _ y [] = y
foldList f y (x : xs) = f x (foldList f y xs)

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Int -> [Integer]
fibsN n = take n fibs

fibsWhile :: (Integer -> Bool) -> [Integer]
fibsWhile f = takeWhile f fibs

fact :: Integer -> [Integer]
fact n = scanl (*) 1 [1..n]

stops = "pbtkdg"

vowels = "aeiou" 

toThruples :: [a] -> [b] -> [(a, b, a)]
toThruples xs ys =
  [ (x1, y1, x2) | x1 <- xs, y1 <- ys, x2 <- xs ]

stopVowelStops1 :: [Char] -> [Char] -> [(Char, Char, Char)]
stopVowelStops1 = toThruples

stopVowelStops2 :: [Char] -> [Char] -> [(Char, Char, Char)]
stopVowelStops2 stops vowels = foldr f [] $ toThruples stops vowels
    where 
      f ('p', x, y) acc = ('p', x, y) : acc
      f _ acc           = acc

nouns = 
  [ "boy"
  , "girl"
  , "bike"
  , "apple"
  , "squirrel"
  , "garage"
  ]

verbs = 
  [ "ate"
  , "threw"
  , "spoke"
  , "ran"
  ]

nounVerbNouns1 :: [String] -> [String] -> [(String, String, String)]
nounVerbNouns1 nouns verbs = toThruples nouns verbs

-- nounVerbNouns1 :: [String] -> [String] -> [(String, String, String)]
-- nounVerbNouns1 stops vowels = foldr f [] $ stopVowelStops1 stops vowels
--     where 
--       f ('p', x, y) acc = ('p', x, y) : acc
--       f _ acc           = acc

-- nouns = [
-- ]
--
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x : xs) = foldr (\x y -> if f x y == GT then x else y) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x : xs) = foldr (\x y -> if f x y == LT then x else y) x xs
    
