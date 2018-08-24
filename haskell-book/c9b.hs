module C9b where

and' :: [Bool] -> Bool
and' [] = True
and' (x : xs) = x && and' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x : xs) = x || or' xs

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (x : xs) = (f x) || any' f xs

elem' :: Eq a => a -> [a] -> Bool
elem' x = any' (\y -> x == y)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' _ [] = []
squishMap' f (x : xs) = f x ++ squishMap' f xs

squish' :: [[a]] -> [a]
squish' = squishMap' id

maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' f (x : xs) = go x xs
  where 
    go greatest [] = greatest
    go greatest (x : rest) 
      | f greatest x == LT = go x rest
      | f greatest x == GT = go greatest rest
      | f greatest x == EQ = go greatest rest

minimumBy' :: (a -> a -> Ordering) -> [a] -> a
minimumBy' f = maximumBy' (flip f)

maximum' :: (Ord a) => [a] -> a
maximum' =  maximumBy' compare

minimum' :: (Ord a) => [a] -> a
minimum' = minimumBy' compare
