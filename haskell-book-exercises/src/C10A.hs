module C10A where

avgWordLength :: String -> Double
avgWordLength x = fromIntegral lengthSum / fromIntegral wordCount
  where
    ws = words x
    lengthSum = sum (map length ws)
    wordCount = length ws

and' :: [Bool] -> Bool
and' = foldr (&&) True

or' :: [Bool] -> Bool
or' = foldr (||) False

any1 :: (a -> Bool) -> [a] -> Bool
any1 _ [] = False
any1 f (x: xs)
  | (f x) == True = True
  | otherwise = any1 f xs

any2 :: (a -> Bool) -> [a] -> Bool
any2 f = foldr (\x y -> f x) False

elem1 :: Eq a => a -> [a] -> Bool
elem1 a = foldr (\x acc -> if a == x then True else acc) False

elem2 :: Eq a => a -> [a] -> Bool
elem2 a = any (a ==)

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x : xs) = reverse1 xs ++ [x]

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x: xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x : xs)
  | f x         = x : filter' f xs
  | otherwise   = filter' f xs

squish' :: [[a]] -> [a]
squish' = foldr (++) []

squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' f = foldr (\x acc -> (f x) ++ acc) []

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
