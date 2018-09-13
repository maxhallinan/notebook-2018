{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module C11 where

-- Algebraic data structures are "algebraic" because each constructor is either
-- a sum or product of other types.
-- Cardinality is the number of possible values it represents.
-- The cardinality of `Bool` is 2 because `True` and `False` are the only
-- values that inhabit the type.

newtype Name = Name String deriving (Show)

class TooShort a where
  isTooShort :: a -> Bool

instance TooShort Name where
  isTooShort (Name name) = length name < 2

instance TooShort Int where
  isTooShort n = n < 2

instance TooShort (Int, String) where
  isTooShort (_, s) = length s < 2

instance TooShort (Int, Int) where
  isTooShort (n1, n2) = (n1 + n2) < 2

instance (Num a, TooShort a) => TooShort (a, a) where
  isTooShort (n1, n2) = isTooShort (n1 + n2)

-- To find the cardinality of a sum type, add the cardinalities of the data
-- constructors.
--
-- To find the cardinality of a product type, find the product of the
-- cardinalities of the data constructors.

-- data Foo = Bar String Int

-- Bar is the product of String and Int

-- A product carries multiple values around in a single data constructor.
-- Tuples are anonymous products.

data QuantumBool = QuantumTrue | QuantumFalse | QuantumBoth deriving (Eq, Show)

data TwoQs = TwoQs' QuantumBool QuantumBool deriving (Eq, Show)

-- TwoQs' has a cardinatlity of 9 because it is the product of QuantumBool and
-- QuantumBool.

-- Product types have no special syntax

-- Low cardinality is a good signal that the type is easy to reason about.
-- High cardinality is a good signal that the type is hard to reason about.

-- Record syntax

data Person1 = MakePerson1 String Int deriving (Eq, Show)

max = MakePerson1 "Max" 32

moom = MakePerson1 "Moomin Troll" 10

getName1 :: Person1 -> String
getName1 (MakePerson1 name _) = name

getAge1 :: Person1 -> Int
getAge1 (MakePerson1 _ age) = age

data Person2 = MakePerson2 { name :: String, age :: Int } deriving (Eq, Show)

max2 = MakePerson2 "max" 10
-- MakePerson2 { name = "max", age = 1 }


-- Normal Form

-- Algebraic rules apply to algebraic type systems.
-- Distributive property: a(b + c) = a(b) + a(c).
-- Multiplication is distributed over addition.
-- Product types distribute over sum types.

-- cardinality of 1
data Fiction = Fiction deriving Show

-- cardinality of 1
data NonFiction = NonFiction deriving Show

-- cardinality of 2
data BookType = NonFictionBook NonFiction | FictionBook Fiction deriving Show

type AuthorName = String

-- (AuthorName, BookType) is a product type
-- BookType is a sum type
data Author = Author (AuthorName, BookType)

-- to distribute the product over the sum type would look like this:
data Author2 = Fiction2 AuthorName | NonFiction2 AuthorName

-- when it's a product of sum types, then it's not in normal form?
-- when it's a sum of product types, then it is in normal form?

data Expr = 
    Number Int
  | Add Expr Expr
  | Minus Expr
  | Mult Expr Expr
  | Divide Expr Expr

-- "This is in normal form because it's the sum (type) of products."

-- A stricter representation of the normal form of "sum of products" is to 
-- represent products with tuples and sums with Either.
-- "This representation finds applications in problems where one is writing 
-- functions or folds over the representations of datatypes, such as with 
-- generics and metaprogramming."
type Number2 = Int
type Add2 = (Expr, Expr)
type Minus2 = Expr
type Mult2 = (Expr, Expr)
type Divide2 = (Expr, Expr)
-- type Expr = Either Number2 (Either Add2 (Either Minus2 (Either Mult2 Divide2)))

data FlowerType = Gardenia | Daisy | Rose | Lilac deriving Show

type Gardener = String

data Garden = Garden Gardener FlowerType deriving Show

-- the sum-of-products normal form of Garden:

data FlowerType' = 
    Gardenia' Gardener 
  | Daisy' Gardener 
  | Rose' Gardener 
  | Lilac' Gardener 
  deriving Show

data GuessWhat = Chickenbutt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a | Second b deriving (Eq, Show)

data RecordProduct a b = 
  RecordProduct { pfirst :: a
                , psecond :: b } 
                deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)

newtype NumPig = NumPig Int deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig

-- the above can also be represented as:
type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving (Eq, Show)

data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type Name1 = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int

data CowInfo = CowInfo Name1 Age deriving (Eq, Show)
data PigInfo = PigInfo Name1 Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name1 Age PoundsOfWool deriving (Eq, Show)

data Animal = Cow CowInfo | Pig PigInfo | Sheep SheepInfo deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

-- Higher-Kinded Types

-- a kind is the type of a type constructor

-- Foo has kind *
data Foo = Foo

-- Bar has kind * -> *
data Bar a = Bar a

-- Baz has kind * -> * -> *
-- Baz is called a "higher-kinded type"
data Baz a b = Baz a b

-- giving a non-alphanumeric name to an operator makes it infix by default
-- any operator that starts with a colon must be infix operator or data constructor

-- values that are LT a should go to the left side of the tree
-- values that are GT a should go to the right side of the tree
data BinaryTree a = 
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)


twice :: (a -> a) -> a -> a
twice f x = f (f x)

mapBinaryTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapBinaryTree _ Leaf = Leaf 
mapBinaryTree f (Node left a right) = 
  Node (mapBinaryTree f left) (f a) (mapBinaryTree f right)

preorder :: BinaryTree a -> [a]
preorder Leaf = [] 
preorder (Node left a right) = (a : preorder left) ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ (a : inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "preorder passes"
  else putStrLn "preorder fails"

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "inorder passes"
  else putStrLn "inorder fails"

testPostorder :: IO  ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "postorder passes"
  else putStrLn "postorder fails"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) = f a (foldTree f (foldTree f b left) right) a
=======
module C11 where

import qualified Data.Char

data Price = Price Integer deriving (Show)

data Size = Size Integer deriving (Show)

data Manufacturer = Mini | Mazda | Tata deriving (Show)

data Airline = PapuAir | Catapault | TakeYourChance deriving (Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Show)

car1 = Car Mini (Price 14000)

car2 = Car Mazda (Price 20000)

car3 = Car Tata (Price 7000)

plane1 = Plane PapuAir (Size 10000)

isCar :: Vehicle -> Bool
isCar (Plane _ _) = False
isCar (Car _ _) = True

isPlane :: Vehicle -> Bool 
isPlane = not . isCar

areCars :: [Vehicle] -> Bool
areCars = all isCar

getManufacturer :: Vehicle -> Manufacturer
getManufacturer (Car m _) = m

data Example1 = Example1

data Example2 = Example2 Int

data Foo = Foo deriving (Eq, Show)

data Bar = Bar deriving (Eq, Show)

data Baz = Baz deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = 
    First a b 
  | Second a b 
  deriving (Eq, Show)

data RecordProduct a b = 
  RecordProduct { first :: a
                , second :: b } 
                deriving (Eq, Show)


data OperatingSystem = 
    GnuPlusLinux
  | OpenBSD
  | Mac
  | Windows
  deriving (Eq, Show) 

data ProgrammingLanguage =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgrammingLanguage }
             deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = 
  [ GnuPlusLinux
  , OpenBSD
  , Mac
  , Windows
  ]

allProgrammingLanguages :: [ProgrammingLanguage]
allProgrammingLanguages = 
  [ Haskell
  , Agda
  , Idris
  , PureScript
  ]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer { os = os, lang = lang } | os <- allOperatingSystems, lang <- allProgrammingLanguages ]

isSubseqOf :: [Char] -> [Char] -> Bool
isSubseqOf sub seq = sub == (foldr helper "" seq)
  where helper = (\x acc -> if elem x sub then x : acc else acc)

capitWords :: String -> [(String, String)]
capitWords = fmap helper . words
  where helper word@(first : rest) = (word, Data.Char.toUpper first : rest)

capitWord :: String -> String
capitWord (first : rest) = Data.Char.toUpper first : rest

-- split :: (Char -> Bool) -> [Char] -> [[Char]]
-- split f = flip helper []
--   where helper string@(char : rest) result
--     | string == "" = result
--     | f char = helper string : result
--     | otherwise = helper string result

-- split :: (Char -> Bool) -> [Char] -> [[Char]]
-- split _ string = []
-- split f string@(c : rest) =
-- foldr helper []
  -- where helper sentence@(char : rest) sentences =
    

-- capitSentences :: String -> String
-- capitSentences = foldr (++) "" . fmap (capitWord) . Data.Text.split ('.' ==)

-- capitSentences :: String -> String
-- capitSentences s = helper s
--   where helper (c : rest) =
--     | c == '.'  = 
--     | otherwise = result ++ [c] ++ 
--     where result = ""
-- split :: (a -> Bool) -> [a] -> [[a]]
-- split _ [] = []
-- split predicate xs@(x : tail) 
--    | tail == []   = [[x]]
--    | predicate x  = [x] ++ [split predicate tail]
--    | otherwise    = x : split predicate tail

-- split :: (Char -> Bool) -> [Char] -> [[Char]]
-- split p [] = []
-- split p s@(c : []) = [[c]]
-- split p t = loop t
--   where go s@(c : tail) =
--     | tail == [] = [[c]]
--     | otherwise = l : split s

-- split :: (Char -> Bool) -> Text -> [Text]
-- split _ t@(Text _off _arr 0) = [t]
-- split p t = loop t
--     where loop s | null s'   = [l]
--                  | otherwise = l : loop (unsafeTail s')
--               where (# l, s' #) = span_ (not . p) s
--
-- fuck i am a useless ignoramous
-- why can't i get this shit correct
-- fuck me fuck me fuck me fuck me

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split x ys = chunk : split x rest
  where (chunk, rest) = break (== x) ys
