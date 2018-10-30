module C21 where

import Control.Applicative
import Test.QuickCheck as QuickCheck
import Test.QuickCheck.Checkers as Checkers
import Test.QuickCheck.Classes as Classes

-- class (Functor t, Foldable t) => Traversable t where
--   -- `traverse` maps each value in a structure to an action, evaluates the 
--   -- action from left to right, and collects the results
--   traverse :: (Applicative f) => (a -> f b) -> t a -> t (f b)
--   traverse f = sequenceA . fmap f

--   -- `sequenceA [return ()]` returns `Monad f => f [()]`
--  -- the effect is flipping two contexts or structures
--  -- sequenceA does not apply any user-defined function to the value inside the
--  -- inner structure, it just flips the structures.
--   sequenceA :: (Applicative f) => t (f a) -> f (t a)
--   sequenceA = traverse id

-- sum :: (Foldable t, Num a) => t a -> a
-- sum is a catamorphism that returns the summary value of a Foldable
-- `sum [1,2,3] == 6` 
-- `sum [Just 1, Just 2, Just 3] == [1,2,3]`
-- `sum`
-- `sum $ Just 1` returns 1


-- foo = sequenceA $ Just [1, 2, 3] == [Just 1, Just 2, Just 3]

-- foo' = sequenceA $ Just (Right 1) == Right (Just 1)

-- foo'' = sequenceA $ Just (Left 1) == Left 1

-- first apply a function `Applicative f => (a -> f b)`
-- fmap f returns a `(Traversable t, Applicative f) => t (f a)`
-- that value is exactly the type of the first argument to `sequenceA`
-- then sequenceA flips the structure, that's how you get the f (t a)
-- anytime you first map and then sequenceA, you can use traverse
-- traverse f = sequenceA . fmap f

-- Elm's Cmd.batch is `[ Cmd a ] -> Cmd a`
-- that seems like Foldable's `asum`
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Foldable.html#v:asum
-- take a `[ Cmd a ]` and get a `Cmd a`
-- No, that's more like a fold I guess.

-- `[Foo a] -> Foo a` 


-- "anytime you need to flip two type constructors
-- around, or map something and then flip them around, that’s probably
-- Traversable"

-- foo''' = sequenceA [Just 1, Nothing] == Nothing

-- data Query = Query 
-- data SomeObj = SomeObj
-- data IoOnlyObj = IoOnlyObj
-- data Err = Err

-- -- transforms a String into Err or SomeObj
-- decodeFn :: String -> Either Err SomeObj
-- decodeFn = undefined

-- -- takes a Query that's run against a database and returns a list of strings
-- fetchFn :: Query -> IO [String]
-- fetchFn = undefined

-- -- a "context" initializer"?
-- makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
-- makeIoOnlyObj = undefined

-- pipelineFn :: Query -> IO (Either Err (SomeObj, IoOnlyObj))
-- pipelineFn query = do 
--   -- `a` is `[String]`
--   a <- fetchFn query
--   -- `map decodeFn a` is `[Either Err SomeObj]`
--   -- `sequence (map decodeFn a)` is `Either Err [SomeObj]`
--   case sequence (map decodeFn a) of
--     -- err is Err
--     (Left err) -> return $ Left err
--     -- res is [SomeObj]
--     (Right res) -> do
--       -- `a` is [(SomeObj, IoOnlyObj)]
--       a <- makeIoOnlyObj res
--       return $ Right a

-- pipelineFn' :: Query -> IO (Either Err (SomeObj, IoOnlyObj))
-- pipelineFn' query = do
--   -- `a` is [String]
--   a <- fetchFn query
--   traverse makeIoOnlyObj foo -- IO (Either Err (SomeObj, IoOnlyObj))
--   -- (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
--   where foo = mapM decodeFn a -- foo is `Either Err [SomeObj]`


-- Chapter Exercises
-- Traversable Instances
-- 1. Identity

newtype Identity' a = Identity' a deriving (Eq, Ord, Show)

instance Functor Identity' where
  fmap f (Identity' x) = Identity' $ f x

instance Applicative Identity' where
  pure = Identity'
  (<*>) (Identity' f) (Identity' x) = Identity' $ f x

instance Foldable Identity' where
  foldr f empty (Identity' x) = f x empty

instance Traversable Identity' where
  traverse f (Identity' x) = fmap Identity' $ f x

instance QuickCheck.Arbitrary a => QuickCheck.Arbitrary (Identity' a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity' a

instance Eq a => Checkers.EqProp (Identity' a) where
  (=-=) = eq

-- Traversable Instances
-- 2. Constant

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

-- instance Applicative (Constant a) where
--   pure = Constant
--   (<*>) _ x = x

-- 3. Maybe
--
data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep $ f x

instance Applicative Optional where
  pure x = Yep x
  (<*>) _ Nada = Nada
  (<*>) Nada _ = Nada
  (<*>) (Yep f) (Yep x) = Yep $ f x

instance Foldable Optional where
  foldr _ y Nada = y
  foldr f y (Yep x) = f x y

instance Traversable Optional where
  traverse f (Yep x) = fmap Yep $ f x 

instance Arbitrary a => QuickCheck.Arbitrary (Optional a) where
  arbitrary = do
    x <- QuickCheck.arbitrary
    return $ Yep x

instance Eq a => Checkers.EqProp (Optional a) where
  (=-=) = eq

-- 4. List

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Semigroup (List a) where
  (<>) xs Nil = xs
  (<>) Nil ys = ys
  (<>) (Cons x xs) ys = Cons x (xs <> ys)

instance Monoid (List a) where
  mempty = Nil

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) (Cons x xs) = 
    (Cons (f x) (fmap f xs)) <> (fs <*> (Cons x xs))

instance Foldable List where
  foldr f y Nil = y
  foldr f y (Cons x xs) = foldr f (f x y) xs

instance Traversable List where
  traverse _ Nil          = pure Nil
  traverse f (Cons x xs)  = liftA2 Cons (f x) (traverse f xs)
  -- pure (<>) <*> fmap pure (f x) <*> (traverse f xs)

instance QuickCheck.Arbitrary a => QuickCheck.Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    return $ pure a

instance Eq a => Checkers.EqProp (List a) where
  (=-=) = eq

-- Tests 

trigger1 :: Identity' (Int, Int, [Int])
trigger1 = undefined

trigger3 :: Optional (Int, Int, [Int])
trigger3 = undefined

trigger4 :: List (Int, Int, [Int])
trigger4 = undefined

runTests :: IO ()
runTests = do
  Checkers.quickBatch $ Classes.traversable trigger1
  Checkers.quickBatch $ Classes.traversable trigger3
  Checkers.quickBatch $ Classes.traversable trigger4
