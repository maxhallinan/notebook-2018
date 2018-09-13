module C10 where

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
