module C18 where

import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Checkers as Checkers
import qualified Test.QuickCheck.Classes as QuickCheck.Classes
import Control.Monad ((>=>), join)

-- Chapter exercises

-- 2.

-- This is trick question.
-- Normally, this is written as `Result a b = Err a | Ok b` 
-- The key is that the first parameter of the type constructor _is not_ 
-- transformed by typeclass operations.
-- That's why the type constructor is partially applied in the instance 
-- definitions.
data Result b a = Ok a | Err b deriving (Eq, Show)

-- When it's an `Err`, just return the `Err`.
instance Functor (Result a) where
  fmap _ (Err x)  = Err x
  fmap f (Ok x)   = Ok $ f x

-- An `Err` on the left side has highest precedence.
-- If only the right side is an `Err`, return that.
-- If both are `Ok`, then apply the function in the left side to the value in 
-- the right side.
instance Applicative (Result a) where
  pure x = Ok x
  (<*>) (Err y) _       = Err y
  (<*>) _ (Err y)       = Err y
  (<*>) (Ok f) (Ok x)   = Ok $ f x

-- This follows the same logic as Functor:
-- When `Err`, return `Err`.
-- Else, return `f x`.
instance Monad (Result a) where
  (>>=) (Ok x) f  = f x
  (>>=) (Err x) _ = Err x

instance (QuickCheck.Arbitrary a, Eq a) => QuickCheck.Arbitrary (Result b a) where
  arbitrary = do 
    x <- QuickCheck.arbitrary 
    return $ Ok x

instance (Eq a, Eq b) => Checkers.EqProp (Result b a) where
  (=-=) = Checkers.eq


-- 3.

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure x = Identity x
  (<*>) (Identity f) (Identity x) = Identity $ f x

instance Monad Identity where
  (>>=) (Identity x) f = f x

instance (QuickCheck.Arbitrary a, Eq a) => QuickCheck.Arbitrary (Identity a) where
  arbitrary = do
    x <- QuickCheck.arbitrary
    return $ Identity x

instance (Eq a) => Checkers.EqProp (Identity a) where
  (=-=) = Checkers.eq

-- 4.

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x tail) = Cons (f x) (fmap f tail)

appendList :: List a -> List a -> List a
appendList xs Nil = xs
appendList Nil ys = ys
appendList (Cons x xs) ys = 
  Cons x $ xs `appendList` ys

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) (Cons x xs) = 
    Cons (f x) $ (f <$> xs) `appendList` (fs <*> xs)

foo = (Cons 1 (Cons 2 (Cons 3 Nil)))

bar = (Cons (+1) (Cons (+2) (Cons (+3) Nil)))

baz = bar <*> foo

instance Monad List where
  (>>=) Nil _ = Nil
  (>>=) xs f = Control.Monad.join $ fmap f xs

instance (QuickCheck.Arbitrary a) => QuickCheck.Arbitrary (List a) where
  arbitrary = do
    x <- QuickCheck.arbitrary
    return $ Cons x Nil

instance Eq a => Checkers.EqProp (List a) where
  (=-=) = Checkers.eq

j :: Monad m => m (m a) -> m a
j x = x >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x1 x2 = pure f <*> x1 <*> x2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x : xs) f = do
  -- expose b in m b
  y <- f x
  -- meh xs f returns m [b]
  -- to get at [b], use fmap
  -- fmap preserves the structure m
  -- while allowing you to add `y` to the front of the list
  fmap ((:) y) (meh xs f)
  -- the above is equivalent to this: 
  -- f x >>= (\y -> fmap ((:) y) $ meh xs f)
  -- (f x :: m b) >>= (\(y :: b) -> fmap (\(xs :: [b]) -> y : xs) (meh xs f :: m [b]))
  -- First solution:
  -- return (<>) <*> y <*> rest
  -- where
  --   y = (f x) >>= (\y -> return [y])
  --   rest = meh xs f

flipType :: Monad m => [m a] -> m [a]
flipType = (flip meh) id
-- First solution:
-- flipType [] = return []
-- flipType (x : xs) = 
--   return (<>) <*> y <*> rest
--   where
--     toY = (\x' -> return [x']) >=> (\xs' -> meh xs' return)
--     y = x >>= toY
--     rest = flipType xs

-- Tests

result = undefined :: Result Int (Int, Int, Int)
identity' = undefined :: Identity (Int, Int, Int)
list' = undefined :: List (Int, Int, Int)

runTests :: IO ()
runTests = do
  Checkers.quickBatch (QuickCheck.Classes.monad result)
  Checkers.quickBatch (QuickCheck.Classes.monad identity')
  Checkers.quickBatch (QuickCheck.Classes.applicative list')
  Checkers.quickBatch (QuickCheck.Classes.monad list')
