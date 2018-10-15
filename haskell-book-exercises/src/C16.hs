module C16 where

import Test.QuickCheck as QC
import Test.QuickCheck.Function as QCF

-- Property testing for a Functor instance

functorIdentity :: (Eq (f a), Functor f) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => QCF.Fun a b -> QCF.Fun b c -> f a -> Bool
functorCompose (QCF.Fun _ f1) (QCF.Fun _ f2) x = fmap (f2 . f1) x == (fmap f2 . fmap f1 $ x)

data Foo a b = Bar | Baz a | Qux b deriving (Eq, Show)

instance Functor (Foo a) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap _ Bar = Bar 
  fmap _ (Baz x) = Baz x
  fmap f (Qux x) = Qux (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Foo a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [Bar, Baz x, Qux y]

type FooId = Foo String Int -> Bool
type FooCompose = (QCF.Fun Int String) -> (QCF.Fun String Int) -> Foo String Int -> Bool

main = do 
  QC.quickCheck (functorIdentity :: FooId)
  QC.quickCheck (functorCompose :: FooCompose)
