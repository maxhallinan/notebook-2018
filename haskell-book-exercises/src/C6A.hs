module C6A where

import Data.List

data TisAnInteger = 
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn x') = x == x'
  
data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two x y) (Two x' y') = (x == x') && (y == y')

data StringOrInt 
  = TisAnInt Int
  | TisAString String

instance Eq StringOrInt where 
  (==) (TisAnInt x)  (TisAnInt x') = x == x'
  (==) (TisAString x) (TisAString x') = x == x'
  (==) (TisAnInt _) (TisAString _) = False
  (==) (TisAString _) (TisAnInt _) = False

data Pair a = 
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = (x == x') && (y == y')

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = (x == x') && (y == y')

data DayOfWeek =
  Sun | Mon | Tue | Wed | Thu | Fri | Sat
  deriving (Ord, Show)

instance Eq DayOfWeek where
  (==) Sun Sun = True
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) _ _     = False

data Mood 
  = Woot 
  | Blah deriving (Eq, Show)

settleDown :: Mood -> Mood
settleDown x = if x == Woot
  then  Blah
  else x

-- instance Show Mood where
--   show _ = "Foo"

data Person = Person Bool deriving (Show)

printPerson :: Person -> IO ()
printPerson = putStrLn . show

type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object

sentence1 :: Object -> Sentence
sentence1 = Sentence "foo" "bar"

sentence2 :: Sentence
sentence2 = Sentence "foo" "bar" "baz"

data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah deriving (Eq, Show)

phew :: Papu
phew = Papu (Rocks "chases") (Yeah True)

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' =
--   p > p'

-- i :: a
-- i = 1

f :: RealFrac a => a
f = 1.0

-- freud :: a -> a
-- freud x = x

freud :: Ord a => a -> a
freud x = x

myX :: Num a => a
myX = 1

-- sigmund  :: Int -> Int
-- sigmund  :: Num a => a -> a
-- sigmund x = myX

jung :: Ord a => [a] -> a
jung = head . sort

charSort :: [Char] -> [Char]
charSort = sort

charHead :: [Char] -> Char
charHead = head . charSort

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk fn x y = (fn x) == y

arith :: Num b
  => (a -> b)
  -> Integer
  -> a
  -> b
arith f' n a =
  (f' a) + (fromInteger n)
  
