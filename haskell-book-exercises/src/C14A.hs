module C14A where

import qualified Test.Hspec
import qualified Test.QuickCheck
import qualified Test.QuickCheck.Checkers as Checkers
import qualified Test.QuickCheck.Classes as Classes
import qualified Test.QuickCheck.Function as QCF
import qualified Test.QuickCheck.Gen

prop_addition :: Int -> Int -> Bool
prop_addition x y = ((x + 1) > x) && (y + 1 > y)

runQuickCheckTests :: IO ()
runQuickCheckTests = Test.QuickCheck.quickCheck prop_addition

runTests :: IO ()
runTests = Test.Hspec.hspec $ do
  Test.Hspec.describe "Addition" $ do
    Test.Hspec.it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `Test.Hspec.shouldBe` True
    Test.Hspec.it "1 + n is always greater than 1" $ do
      Test.QuickCheck.property $ \x -> x + 1 > (x :: Int)

data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Test.QuickCheck.Gen Trivial
trivialGen = return Trivial

instance Test.QuickCheck.Arbitrary Trivial where
  arbitrary = trivialGen

data Bar a = Bar a deriving (Eq, Show)

barGen :: Test.QuickCheck.Arbitrary a => Test.QuickCheck.Gen (Bar a)
barGen = do
  a <- Test.QuickCheck.arbitrary
  return (Bar a)

instance Test.QuickCheck.Arbitrary a => Test.QuickCheck.Arbitrary (Bar a) where
  arbitrary = barGen

data Baz a b = Baz a b deriving (Eq, Show)

bazGen :: (Test.QuickCheck.Arbitrary a, Test.QuickCheck.Arbitrary b) => Test.QuickCheck.Gen (Baz a b)
bazGen = do
  a <- Test.QuickCheck.arbitrary
  b <- Test.QuickCheck.arbitrary
  return (Baz a b)

instance (Test.QuickCheck.Arbitrary a, Test.QuickCheck.Arbitrary b) => Test.QuickCheck.Arbitrary (Baz a b) where
   arbitrary = bazGen

data Sum a b = Foo' a | Bar' b deriving (Eq, Show)

genSum :: (Test.QuickCheck.Arbitrary a, Test.QuickCheck.Arbitrary b) => Test.QuickCheck.Gen (Sum a b)
genSum = do
  a <- Test.QuickCheck.arbitrary
  b <- Test.QuickCheck.arbitrary
  Test.QuickCheck.Gen.oneof [ return $ Foo' a
                            , return $ Bar' b]

instance (Test.QuickCheck.Arbitrary a, Test.QuickCheck.Arbitrary b) => Test.QuickCheck.Arbitrary (Sum a b) where
  arbitrary = genSum

-- to generate functions of type `Fun a b`, you must have an instance of `Function a`
-- `a` in `Fun a b` is a data-type representation of the function
-- `b` is a callable function
functorCompose' :: (Eq (f c), Functor f) => f a -> QCF.Fun a b -> QCF.Fun b c -> Bool
functorCompose' x (QCF.Fun _ f) (QCF.Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f) x

-- Using checkers to test typeclass instances

data Foo'' = Bar'' | Baz'' deriving (Eq, Show)

instance Test.QuickCheck.Arbitrary Foo'' where
  arbitrary = Test.QuickCheck.frequency [ (1, return Bar'')
                                        , (2, return Baz'') ]
instance Semigroup Foo'' where
  (<>) Bar'' Bar'' = Bar''
  (<>) Baz'' Baz'' = Baz''
  (<>) Bar'' x = x
  (<>) x Bar'' = x

instance Monoid Foo'' where
  mempty = Bar''

instance Checkers.EqProp Foo'' where
  (=-=) = Checkers.eq

runCheckersTests :: IO ()
runCheckersTests = Checkers.quickBatch (Classes.monoid Bar'')

