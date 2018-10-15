module C17Scratch where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


validateLength :: Int -> String -> Maybe String
validateLength maxLen s = 
  if (length s) > maxLen 
  then Nothing
  else Just s

newtype Name = Name String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = 
  fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress s =
  fmap Address $ validateLength 100 s

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson name address =
  case mkName name of 
    Nothing -> Nothing
    Just n -> 
      case mkAddress address of
        Nothing -> Nothing
        Just a ->
          Just (Person n a)

n1 :: Maybe Name 
n1 = mkName "foo"

a1 :: Maybe Address
a1 = mkAddress "bar"

p1 = (Just Person <*> n1) <*> a1

p2 = Person <$> Just (Name "foo") <*> Just (Address "bar")
p3 = Person <$> Nothing <*> Just (Address "bar")

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Semigroup Bull where
  (<>) _ _ = Fools 

instance Monoid Bull where
  mempty = Fools
  mappend = (<>)

instance EqProp Bull where (=-=) = eq

main :: IO ()
main = quickBatch (monoid Twoo)
