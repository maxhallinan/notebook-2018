module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' key [] = Nothing
lookup' key (pair : pairs) = go pair
  where go (k, v)
          | key == k  = Just v
          | otherwise = lookup' key pairs

xs :: Maybe Integer
xs = lookup' 3 $ zip x y 

ys :: Maybe Integer
ys = lookup' 6 $ zip y z

zs :: Maybe Integer
zs = lookup' 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup' n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> Maybe (Integer, Integer)
x3 n = liftA2 (,) (z' n) (z' n)

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

fromMaybe' :: a -> Maybe a -> a
fromMaybe' x Nothing   = x
fromMaybe' _ (Just x)  = x

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ foldr (&&) True $ sequA 6
  print $ sequA $ fromMaybe' 1 s'
  print $ bolt $ fromMaybe' 1 ys
  -- print $ sequenceA [Just 3, Just 2, Just 1]
  -- print $ sequenceA [x, y]
  -- print $ sequenceA [xs ,ys]
  -- print $ summed <$> ((,) <$> xs <*> ys)
  -- print $ fmap summed ((,) <$> xs <*> zs)
  -- print $ bolt 7
  -- print $ fmap bolt z
  -- print $ sequA 7
  -- print $ s'

