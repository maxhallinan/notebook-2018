module C16B where

import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Function as QuickCheck.Function
import qualified Test.QuickCheck.Checkers as Checkers

prop_functorIdentity :: (Eq (f a), Functor f) => f a -> Bool
prop_functorIdentity x = fmap id x == x

prop_functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
prop_functorCompose f g x = fmap (g . f) x == (fmap g . fmap f) x

prop_functorCompose' :: (Eq (f c), Functor f) => f a -> QuickCheck.Function.Fun a b -> QuickCheck.Function.Fun b c -> Bool
prop_functorCompose' x (QuickCheck.Function.Fun _ f) (QuickCheck.Function.Fun _ g) =
  fmap (g . f) x == (fmap g . fmap f) x

-- functorBatch :: Checkers.TestBatch
-- functorBatch =
--   ("Functor", [ ("Law of Identity", prop_functorIdentity)
--               , ("Law of Composition", pro_functorCompose) ])

-- runTests :: IO ()
-- runTests = QuickCheck.quickCheck functorBatch

testFunctorId :: IO ()
testFunctorId = QuickCheck.quickCheck (prop_functorIdentity :: [Int] -> Bool)

testFunctorCompose :: IO ()
testFunctorCompose = QuickCheck.quickCheck (prop_functorCompose (+1) (+2) :: [Int] -> Bool)

type IntToInt = QuickCheck.Fun Int Int

testFunctorCompose' :: IO () 
testFunctorCompose' = QuickCheck.quickCheck (prop_functorCompose' :: [Int] -> IntToInt -> IntToInt -> Bool)

runTests :: IO ()
runTests = do
  testFunctorId
  testFunctorCompose
  testFunctorCompose'
