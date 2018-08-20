module C9 where
import Prelude

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x

tail' :: [a] -> Maybe [a]
tail' [] = Nothing
tail' (_ : xs) = Just xs

--range syntax
-- `(..)` expects an instance of the Enum typeclass

oneToTen = [1..10]
aToZ = ['a'..'z']

evenToTen = enumFromThenTo 0 2 10
-- [0,2,4,6,8,10]

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool True True = [True]
eftBool False True = [True, False] 
eftBool True False = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT LT = [LT]
eftOrd EQ EQ = [EQ]
eftOrd GT GT = [GT]
eftOrd GT _ = []
eftOrd _ LT = []
eftOrd start stop = go (succ start) stop (start : [])
  where go next sp (x : xs)
          | next == x = (next : x : xs)
          | otherwise =
              go (succ next) sp (next : x : xs)

enumFromTo' :: (Ord a, Enum a) => a -> a -> [a]
enumFromTo' start stop = go stop start []
  where go current start result
          | current == start = current : result
          | otherwise =
              go (pred current) start (current : result)
