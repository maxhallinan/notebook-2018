module Laws where

import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck as QuickCheck.Function

-- Semigroup

-- law of associativity

prop_semigroupAssoc :: (Eq a, Semigroup a) => a -> a -> a -> Bool
prop_semigroupAssoc x y z =
  (x <> y) <> z == x <> (y <> z)
  

-- Monoid

-- law of left identity

prop_monoidLeftId :: (Eq a, Monoid a) => a -> Bool
prop_monoidLeftId x = mappend mempty x == x

-- law of right identity

prop_monoidRightId :: (Eq a, Monoid a) => a -> Bool
prop_monoidRightId x = mappend x mempty == x

-- Functor

-- law of identity

prop_functorId :: (Eq (f a), Functor f) => f a -> Bool
prop_functorId x = fmap id x == x

-- law of composition

prop_functorCompose :: (Eq (f c), Functor f) => f a -> QuickCheck.Function.Fun a b -> QuickCheck.Function.Fun b c -> Bool
prop_functorCompose x (QuickCheck.Function.Fun _ f) (QuickCheck.Function.Fun _ g) =
  fmap (g . f) x == (fmap g . fmap f) x

-- Applicative

-- law of identity

prop_applicativeId :: (Eq (f a), Applicative f) => f a -> Bool
prop_applicativeId x = ((pure id) <*> x) == x

-- law of composition

prop_applicativeCompose :: (Eq (f c), Applicative f) => f a -> QuickCheck.Function.Fun a b -> QuickCheck.Function.Fun b c -> Bool
prop_applicativeCompose x (QuickCheck.Function.Fun _ f) (QuickCheck.Function.Fun _ g) =
  (((pure (.) <*> pure g) <*> pure f) <*> x) == (pure g <*> (pure f <*> x))

-- law of homomorphism
-- prop_applicativeHomomorphism :: Eq b => a -> QuickCheck.Function.Fun a b -> Bool
prop_applicativeHomomorphism x (QuickCheck.Function.Fun _ f) =
  (pure f <*> pure x) == pure (f x)

-- law of interchange

-- Monad

-- Tests

type StringToInt = QuickCheck.Function.Fun String Int

type IntToBool = QuickCheck.Function.Fun Int Bool

runTests :: IO ()
runTests = do
  -- Semigroup
  QuickCheck.quickCheck (prop_semigroupAssoc :: String -> String -> String -> Bool)
  -- Monoid
  QuickCheck.quickCheck (prop_monoidLeftId :: String -> Bool)
  QuickCheck.quickCheck (prop_monoidRightId :: String -> Bool)
  -- Functor
  QuickCheck.quickCheck (prop_functorId :: Maybe String -> Bool)
  QuickCheck.quickCheck (prop_functorCompose :: Maybe String -> StringToInt -> IntToBool -> Bool)
  -- Applicative
  QuickCheck.quickCheck (prop_applicativeId :: Maybe String -> Bool)
  QuickCheck.quickCheck (prop_applicativeCompose :: Maybe String -> StringToInt -> IntToBool -> Bool)
 
