module SkiFree where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S x y) = S (fmap f x) (f y)

instance Foldable n => Foldable (S n) where
  -- foldMap f (S x y) = foldMap f x `mappend` f y
  foldr f x (S y z) = f z (foldr f x y)

instance Traversable n => Traversable (S n) where
  traverse f (S y z) = liftA2 S (traverse f y) (f z)
 
instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), EqProp a) => EqProp (S n a) where
  (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)

sTrigger :: S [] (Int, Int, [Int])
sTrigger = undefined

runTests = do
  quickBatch $ functor sTrigger
  -- quickBatch $ traversable sTrigger
