module ApplicativeLaws where

import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Function as QuickCheck.Function
import qualified Test.QuickCheck.Checkers as Checkers

-- identity

prop_applicativeId :: (Applicative f, Eq (f a)) => f a -> Bool
prop_applicativeId x = (pure id <*> x) == x

-- composition
-- composing first and then applying should be the same as applying and then composing
prop_applicativeCompose :: (Applicative f, Eq (f c)) => f a -> QuickCheck.Function.Fun a b -> QuickCheck.Function.Fun b c -> Bool
prop_applicativeCompose x (QuickCheck.Function.Fun _ f) (QuickCheck.Function.Fun _ g) =
  -- (pure (g . f) <*> x) == (pure g <*> (pure f <*> x))
  (pure (.) <*> pure g <*> pure f <*> x) == (pure g <*> (pure f <*> x))

-- homomorphism
-- interchange
type IntToStr = QuickCheck.Function.Fun Int String
type StrToBool = QuickCheck.Function.Fun String Bool

runTests :: IO ()
runTests = do
  QuickCheck.quickCheck (prop_applicativeId :: Maybe String -> Bool)
  QuickCheck.quickCheck (prop_applicativeCompose :: Maybe Int -> IntToStr -> StrToBool -> Bool)
