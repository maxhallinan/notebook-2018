module C20 where

import Data.Foldable as Foldable
import Data.Monoid as Monoid
import Test.QuickCheck as QuickCheck
import Test.QuickCheck.Checkers as Checkers
import Test.QuickCheck.Classes as Classes
-- Foldable
-- folding functions are almost always dependent on a Monoid instance
-- Foldable is a "class of data structures that can be folded to a summary value"
-- the minimal definition of Foldable is foldMap or foldr

-- class Foldable (t :: * -> *) where
--   -- summarize the value inside of the Foldable using the Monoid of that value
--   fold :: Monoid m => t m -> m
--   -- first map any value inside the Foldable to a Monoid, then combine using 
--   -- that Monoid instance
--   foldMap :: Monoid m => (a -> m) -> t a -> m
--   -- this doesn't require a Monoid because the folding is done by the function
--   -- as long as the function can map a and b to b, then a Monoid is not required.
--   -- This might be useful when a in t a has more than one Monoid and you need a 
--   -- way to specify which append-like operation to use.
--   foldr :: (a -> b -> b) -> b -> t a -> b

-- fold ([1,2,3] :: [Int])
-- No instance for (Monoid Int) arising from a use of ‘fold’
-- Int doesn't implement Monoid because there is more than one possible instance
-- for the type.
-- Data.Monoid exposed the Sum and Product newtypes that both have Monoid instances.
-- These newtypes are used as a way to indicate which Monoid instance you want:
-- (getSum . fold) $ fmap Data.Monoid.Sum [1,2,3] 
-- 6
-- fold ["Hello", " ", "World!"]
-- "Hello World!"

-- Foldable class operations
-- 1. `fold` requires a Monoid m => (Foldable m) and creates the summary value 
-- by using that Monoid instance.
-- 2. `foldMap` enables you to map `a` in `Foldable a` to a `Monoid b => b` and 
-- then creates the summary value by using that Monoid instance.
-- 3. `foldr` is the most flexible. It just takes two types of any type and 
-- transforms a Foldable of the first type into the second type.

-- Bool does not implement Monoid because it has no empty value
-- Data.Semigroup exposes a newtype `All` that can be used to use a Semigroup 
-- instance for Bool.
-- `All` is a Semigroup of `Bool`
-- (getAll . foldMap All) [True, False, True]
-- False

-- A catamorphism on a single value like `foldMap id (Identity 1)` is about
-- using 1, or getting 1 out of Identity, and not about transforming many values
-- into one value, like the catamorphism of a list.
data Identity a = Identity a deriving (Eq, Show)

instance Foldable.Foldable Identity where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f empty (Identity x) = f x empty
  -- Monoid t => foldr :: (a -> m b) -> t a -> b
  foldMap f (Identity x) = f x

data Maybe' a = Nothing' | Just' a

instance Foldable.Foldable Maybe' where
  foldr _ empty Nothing'  = empty
  foldr f empty (Just' a) = f a empty

  foldMap _ Nothing'  = mempty
  foldMap f (Just' x) = f x

-- Basic derived operations
-- `toList`
-- toList $ Just 1 
-- [1]

-- foldMap :: (Foldable t, Monoid a) => (a -> m) -> t a -> m
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr (\y _ -> x == y) False

-- 
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr (\x y -> compare' (Just x) y) Nothing
  where 
    compare' Nothing Nothing    = Nothing
    compare' Nothing (Just x)   = Just x
    compare' (Just x) Nothing   = Just x
    compare' (Just x) (Just y)  = if x < y then Just x else Just y

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (\x y -> compare' (Just x) y) Nothing
  where 
    compare' Nothing Nothing    = Nothing
    compare' Nothing (Just x)   = Just x
    compare' (Just x) Nothing   = Just x
    compare' (Just x) (Just y)  = if x > y then Just x else Just y

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) [] 

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x y -> f x <> y) mempty

data Constant' b = Constant' b

instance Foldable Constant' where
  -- (Foldable t) => (a -> b -> b) -> t a -> b
  foldr f y (Constant' x) = f x y

data Two a b = Two a b

instance Foldable (Two a) where
  foldr f y (Two _ x) = f x y

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldr f y (Three _ _ x) = f x y

data Four a b c d = Four a b c d

instance Foldable (Four a b c) where
  foldr f y (Four _ _ _ x) = f x y

-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
filterF :: (Foldable t, Applicative f, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF predicate = foldMap (\x -> if predicate x then pure x else mempty)
-- constraints are saying that f must have an instance of Appliciative
-- that t must be Foldable
-- and that the result must have an instance of Monoid
-- result must be an Applicative and a Monoid
-- input must be a Foldable
-- filterF f = foldMap (\x -> if f x then x else mempty)

-- t a is Foldable 
-- foldMap returns a monoid
-- foldMap 
