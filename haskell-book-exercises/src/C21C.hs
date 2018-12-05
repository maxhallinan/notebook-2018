module C21C where

import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Checkers as Checkers
import qualified Test.QuickCheck.Classes as Classes

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

-- instance Traversable Optional where
--   traverse (f x = fmap Yep $ f x 

instance QuickCheck.Arbitrary a => QuickCheck.Arbitrary (Optional a) where
  arbitrary = do
    x <- QuickCheck.arbitrary
    return $ Yep x

instance (Checkers.EqProp a, Eq a) => Checkers.EqProp (Optional a) where
  (=-=) = Checkers.eq

-- optionalTrigger :: Optional (Int, Int, [Int])
-- optionalTrigger = undefined

-- runTests :: IO ()
-- runTests = do
--   Checkers.quickBatch $ Classes.traversable optionalTrigger
