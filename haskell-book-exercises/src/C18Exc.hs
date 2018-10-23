module C18Exc where

import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Checkers as Checkers
import qualified Test.QuickCheck.Classes as QuickCheck.Classes

-- 1.

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _    = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  (>>=) _ _ = NopeDotJpg

instance QuickCheck.Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => Checkers.EqProp (Nope a) where
  (=-=) = Checkers.eq

-- 2.

-- data PhhhbbtttEither b a = Left a | Right b deriving (Eq, Show)

-- instance Functor (PhhhbbtttEither a) where
--   fmap _ (Left x) = Left x
--   fmap f (Right x) = Right $ f x

-- instance Applicative (PhhhbbtttEither a) where
--   pure x = Right x
--   (<*>) (Left x) _ = Left x
--   (<*>) _ (Left x) = Left x
--   (<*>) (Right f) (Right x) = Right $ f x

-- nopeTestValue = undefined :: Nope (Int, Int, Int)

-- runTests :: IO ()
-- runTests = do
--   Checkers.quickBatch $ QuickCheck.Classes.monad nopeTestValue
