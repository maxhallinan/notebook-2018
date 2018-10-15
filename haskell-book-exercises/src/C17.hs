module C17 where

import Control.Applicative
import Data.List (elemIndex)
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Exercises: Lookups

-- 1.
added :: Maybe Integer
added = 
  Just (+ 3) <*> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2.
x1 :: Maybe Integer
x1 = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

y1 :: Maybe Integer
y1 = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = liftA2 (,) x1 y1

-- 3.
x2 :: Maybe Int
x2 = elemIndex 3 [1, 2, 3, 4, 5]

y2 :: Maybe Int
y2 = elemIndex 4 [1, 2, 3, 4, 5]

maxed :: Maybe Int
maxed = liftA2 max x2 y2

-- 4. 
xs = [1, 2, 3]

ys = [4, 5, 6]

x :: Maybe Integer
x = lookup 3 $ zip xs ys

y :: Maybe Integer
y = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ liftA2 (,) x y

-- Exercise: Identity Instance

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure x = Identity x
  (<*>) (Identity f) (Identity x) = Identity (f x)

-- Exercise: Constant Instance

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  -- mempty is used because we don't know what concrete type `a` is but the 
  -- argument to the Constant data constructor must be of type `a`.
  pure _ = Constant mempty
  (<*>) _ (Constant x) = Constant x

-- Exercise: Fixer Upper

-- 1.
a1 = const <$> Just "Hello" <*> pure "World"

a2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]

-- List Applicative Exercise

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  -- apply every f in fs to every x in xs 
  -- [(+1), (+2)] [1,2]
  -- [2, 3, 3, 4]
  (<*>) (Cons f fs) (Cons x xs) = Cons (f x) 

instance Arbitrary List where
  arbitrary = frequency ()
