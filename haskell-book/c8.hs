module C8 where

import Data.List (concat, intersperse)

applyTimes 0 f b = b
applyTimes n f b = applyTimes (n-1) f (f b)

-- applyTimes 5 (+1) 5 = applyTimes (5 - 1) (+1) ((+1) 5)
-- applyTimes 4 (+1) 6 = applyTimes (4 - 1) (+1) ((+1) 6)
-- applyTimes 3 (+1) 7 = applyTimes (3 - 1) (+1) ((+1) 7)
-- applyTimes 2 (+1) 8 = applyTimes (2 - 1) (+1) ((+1) 8)
-- applyTimes 1 (+1) 9 = applyTimes (1 - 1) (+1) ((+1) 9)
-- applyTimes 0 (+1) 10 = 10
-- (applyTimes 0 (+1) (applyTimes 1 (+1) (applyTimes 2 (+1) (applyTimes 3 (+1) (applyTimes 4 (+1) 6)))))
--
fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n =
  fibonacci (n - 2) + fibonacci (n - 1)

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

dividedBy :: Numerator -> Denominator -> Quotient
dividedBy = div

intDividedBy :: Integral a => a -> a -> (a, a)
intDividedBy num denom = go num denom 0
  -- count keeps track of how many times the denominator is in the numerator
  -- for each iteration, reduce the numerator by the denominator and increment the counter
  -- the remainder is the value of the numerator when the numerator is less than the demoninator
  where go n d count
          | n < d = (count, n)
          | otherwise =
              go (n - d) d (count + 1)

isStr :: String -> String
isStr x = x

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appedConny = cattyConny "woops"
frappe = flippy "haha"

expectedAnswer8 = "pink mrow haha mrow green mrow woops mrow blue" 
actualAnswer8 = cattyConny (frappe "pink") (cattyConny "green" (appedConny "blue"))
answer8 = expectedAnswer8 == actualAnswer8

expected9 = cattyConny (flippy "Pugs" "are") "awesome"
actual9 = "are mrow Pugs mrow awesome"
answer9 = expected9 == actual9

sumN :: (Eq a, Num a) => a -> a
sumN n = go n 0
  where go n sum
          | n == 0 = sum
          | otherwise =
              go (n - 1) (sum + n)

multN :: Integral a => a -> a -> a
multN x y = go x y 0
  where go x y prod
          | y < 1 = prod
          | otherwise =
              go x (y - 1) (x + prod)

data DividedByResult = 
    Result Integer
  | DividedByZero
  deriving Show

dividedBy' :: Integral a => a -> a -> DividedByResult
dividedBy' _ 0 = DividedByZero
dividedBy' num denom 
  | (num < 0) && (denom < 0) =
      Result (go (num * (-1)) (denom * (-1)) 0)
  | num < 0 =
      Result ((go (num * (-1)) denom 0) * (-1))
  | denom < 0 =
      Result ((go num (denom * (-1)) 0) * (-1))
  | otherwise =
      Result  (go num denom 0)
  where go n d quotient 
          | n < d = quotient
          | otherwise =
              go (n - d) d (quotient + 1)

mc91 :: Integral a => a -> a
mc91 n
    | n > 100 = n - 10
    | otherwise = mc91 (mc91 (n + 11))

digitToWord :: Int -> String
digitToWord n = 
  case n of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    _ -> ""

digits :: Int -> [Int]
digits n = go n []
  where go n digits 
          | n < 1 = digits
          | otherwise =
              go (div n 10) ([mod n 10] ++ digits)
    
wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . (map digitToWord) . digits
