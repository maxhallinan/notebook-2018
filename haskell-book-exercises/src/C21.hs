module C21 where

import Control.Applicative
import Test.QuickCheck as QuickCheck
import Test.QuickCheck.Checkers as Checkers
import Test.QuickCheck.Classes as Classes

-- 2. Constant

newtype Constant a b = Constant { getConstant :: a }

instance Functor (Constant a) where
  fmap f (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant x) = pure Constant <*> pure x

-- 3. Constant

data Optional a = Nada | Yep a deriving (Eq, Show)

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
  traverse _ Nada    = pure Nada
  traverse f (Yep x) = fmap Yep $ f x

instance QuickCheck.Arbitrary a => QuickCheck.Arbitrary (Optional a) where
  arbitrary = do
    x <- QuickCheck.arbitrary
    return $ Yep x

instance Eq a => EqProp (Optional a) where
  (=-=) = eq


-- 4. Three

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
  foldr f y (Three _ _ x) = f x y

instance Traversable (Three a b) where
  traverse f (Three x y z) = fmap (Three x y) $ f z

instance (QuickCheck.Arbitrary a, QuickCheck.Arbitrary b, QuickCheck.Arbitrary c) => QuickCheck.Arbitrary (Three a b c) where
  arbitrary = do
    x <- QuickCheck.arbitrary
    y <- QuickCheck.arbitrary
    z <- QuickCheck.arbitrary
    return $ Three x y z

instance (Eq a, Eq b, Eq c) => Checkers.EqProp (Three a b c) where
  (=-=) = eq


-- 5. Pair

data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x $ f y

instance Foldable (Pair a) where
  foldr f y (Pair _ x) = f x y

instance Traversable (Pair a) where
  traverse f (Pair x y) = fmap (Pair x) $ f y

instance (QuickCheck.Arbitrary a, QuickCheck.Arbitrary b) => QuickCheck.Arbitrary (Pair a b) where
  arbitrary = do
    x <- QuickCheck.arbitrary
    y <- QuickCheck.arbitrary
    return $ Pair x y

instance (Eq a, Eq b) => Checkers.EqProp (Pair a b) where
  (=-=) = eq

-- 6. Big

data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big x y1 y2) = Big x (f y1) (f y2)

instance Foldable (Big a) where
  foldr f x (Big _ z1 z2) = f z2 $ f z1 x

instance Traversable (Big a) where
  traverse f (Big x y1 y2) = liftA2 (Big x) (f y1) (f y2)

instance (QuickCheck.Arbitrary a, QuickCheck.Arbitrary b) => QuickCheck.Arbitrary (Big a b) where
  arbitrary = do
    x <- QuickCheck.arbitrary
    y <- QuickCheck.arbitrary
    return $ Big x y y

instance (Eq a, Eq b) => Checkers.EqProp (Big a b) where
  (=-=) = eq

-- 7. Bigger

data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger x y1 y2 y3) = Bigger x (f y1) (f y2) (f y3)

instance Foldable (Bigger a) where
  foldr f x (Bigger y z1 z2 z3) = 
    f z3 $ f z2 $ f z1 x

instance Traversable (Bigger a) where
  traverse f (Bigger x y1 y2 y3) = pure (Bigger x) <*> (f y1) <*> (f y2) <*> (f y3)


instance (QuickCheck.Arbitrary a, QuickCheck.Arbitrary b) => QuickCheck.Arbitrary (Bigger a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Bigger x y y y

instance (Eq a, Eq b) => Checkers.EqProp (Bigger a b) where
  (=-=) = eq

-- Tests

optionalTrigger :: Optional (Int, Int, [Int])
optionalTrigger = undefined

threeTrigger :: Three String String (Int, Int, [Int])
threeTrigger = undefined

pairTrigger :: Pair String (Int, Int, [Int])
pairTrigger = undefined

bigTrigger :: Big String (Int, Int, [Int])
bigTrigger = undefined

biggerTrigger :: Bigger String (Int, Int, [Int])
biggerTrigger = undefined

runTests :: IO ()
runTests = do
  Checkers.quickBatch $ Classes.traversable optionalTrigger
  Checkers.quickBatch $ Classes.traversable threeTrigger
  Checkers.quickBatch $ Classes.traversable pairTrigger
  Checkers.quickBatch $ Classes.traversable bigTrigger
  Checkers.quickBatch $ Classes.traversable biggerTrigger
