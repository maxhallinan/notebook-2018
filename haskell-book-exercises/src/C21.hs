module C21 where

import Test.QuickCheck as QuickCheck
import Test.Checkers as Checkers
import Test.Classes as Classes

data Optional a = Nada | Yep a

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
  traverse f x = fmap Yada $ f x 

instance QuickCheck.Arbitrary (Optional a) where
  arbitrary = do
    x <- QuickCheck.arbitrary
    return $ Yep x

instance EqProp a => EqProp (Optional a) where
  (=-=) = eq

optionalTrigger : Optional (Int, Int, [Int])
optionalTrigger = undefined

runTests : IO ()
runTests = do
  QuickCheck.quickBatch $ Classes.traversable optionalTrigger
