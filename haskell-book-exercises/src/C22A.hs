{-# LANGUAGE InstanceSigs #-}

module C22A where

import Control.Applicative
import Control.Monad
import Data.Char

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

-- Composition is the Functor of functions
-- fmap boop doop = (boop . doop)
-- implemented as `fmap = (.)`
-- the structure here is a partially applied function
bloop :: Integer -> Integer
bloop = fmap boop doop

-- bbop 2 = (+) (boop 2) (doop 2)
-- bbop 2 = (+) ((*) 2 2) ((+) 10 2)
-- bbop 2 = (+) 4 12
-- bbop 2 = 16
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

-- use this when two functions require the same input, and the output of those 
-- functions will be used as an input to a third function
-- this is a different way of writing the above
duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

boopDoop :: Integer -> Integer
boopDoop = do
  x <- boop
  y <- doop
  return (x + y)

foo :: Integer -> Integer
foo = (*2)

bar :: Integer -> Integer
bar = (+10)

-- >>= :: Monad m => m a -> (a -> m b) -> m b
-- >> :: Monad m => m a -> m b -> m b
-- return :: Monad m => a -> m a

-- f >>= k = \ r -> k (f r) r
-- f1 >>= f2 = \ r -> f2 (f1 r) r
-- f1 >>= f2 = \ x -> f2 (f1 x) x
fooBar :: Integer -> Integer
fooBar = do
  x <- foo
  -- y <- bar
  return (x + 1)
-- fooBar 2 == 5
-- fooBar 2 == (2 + 2) + 1

-- fooBar2 :: Integer -> Integer
-- fooBar2 x = 
  -- foo >>= (\y -> (\z -> y + 1)) $ x
  -- (\x -> x + 2) >>= (\y -> (\z -> y + 2)) $ x
  -- fooBar2 2 -> 6
  -- (>>=) (\x -> x + 2) (\y -> (\z -> y + 2)) = (\2 -> (2 + 2)) 2
 
-- fooBar2 2
-- f >>= k = \ r -> k (f r) r
-- (*2) >>= (\x -> \y -> x + 1) = (4 -> 2 -> 4 + 1) (2 *2) (2)

-- The >>= implementation for functions takes a 1 argument function as its 
-- first argument
-- It takes a two argument function as its second argument
-- It returns a one argument function
-- That function calls the first function with its argument, passes the result
-- to the second function as its first argument, and its argument as its second
-- argument
-- FunctionOne has one argument
-- FunctionTwo has two arguments
-- FunctionThree is (Arg -> FunctionTwo(FunctionOne(Arg), Arg)
-- >>= takes a two argument function and makes it into a one argument function
-- but the original two argument function still gets both of its arguments.
-- how?

-- "Reader is a way of stringing functions together when all functions are 
-- awaiting input from a shared environment"
-- "A way of abstracting function application" - (e.g. a function of 2-arity becomes
-- a function of 1-arity"
-- Often use when a bunch of functions depend on a constant value from a shared
-- environment, like configuration values.





-- instance Monand ((->) r) where
--   f >>= k = \ r -> k (f r) r

-- (\x -> x * 2) >>= (\y -> (\x -> x + 10) >>= (\z -> y + z))

a1 :: IO ()
a1 = do
  putStrLn "foo"
  b <- getLine
  putStrLn $ "bar" ++ b

-- a2 :: IO ()
-- a2 = do
--   -- putStrLn :: IO ()
--   putStrLn "foo" >>
--   -- getLine :: IO String
--   getLine >>= 
--   -- String -> IO ()
--   \b ->
--     -- putStrLn :: IO ()
--     putStrLn $ "bar" ++ b

-- (>>) :: m a -> m b -> m b
-- (>>=) :: m a -> (a -> m b) -> m b
-- (putStrLn "foo") >> (getLine >>= \b -> putStrLn $ "bar" ++ b)

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- m is a partially applied function. m == ((->) x). m a == ((->) x) a
-- left side is a partially applied function
-- the right side is a function that takes a partially applied function ((->) x)
-- and returns a function (a -> b)
-- m b is the function a -> b 

-- m a == (\x -> x * 2) 
-- (a -> m b) == 

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) rev cap

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  x <- rev
  y <- cap
  return (x, y)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = rev >>= (\x -> fmap (\y -> (x,y)) cap)

-- "The Applicative and Monad instances for the function type give us a way to 
-- map a function waiting an `a` argument over another function waiting an `a`
-- argument."
-- In the functor of (->), the first argument to (->) is part of the structure
-- being mapped over.
-- The second argument to the type constructor, the function's output type, is 
-- the type that is mapped over.
{-
  a -> b
  instance Functor ((->) a) where
    -- g transforms the type b in a -> b
    fmap g f = g . f

(.) :: (b -> c) -> (a -> b) -> a -> c
fmap :: (b -> c) -> f b -> f c
where f is (a ->)
fmap for functions
-}
{-

-}
newtype Reader r b = Reader { runReader :: r -> b }

instance Functor (Reader r) where
  -- fmap :: (a -> b) -> Reader r a -> Reader r b
  -- ra :: r -> b
  -- fmap f (Reader ra) = Reader $ \r -> f (ra r)
  fmap f (Reader ra) = Reader $ (f . ra)

ask :: Reader a a
ask = Reader id

instance Applicative (Reader r) where
  pure = Reader . const
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) (Reader f) (Reader g) = Reader (\x -> f x (g x))

newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person { humanName :: HumanName, dogName :: DogName, address :: Address } deriving (Eq, Show)

data Dog = Dog { dogsName :: DogName, dogsAddress :: Address } deriving (Eq, Show)

person :: Person
person = Person (HumanName "Bill") (DogName "Bob") (Address "332 Chatham Dr.")

-- dog :: Dog
-- dog = Dog (DogName "Bob") (Address "332 Chatham Dr.")

getDog :: Person -> Dog
getDog p = 
  Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR =
  liftA2 Dog dogName address
  -- Dog <$> dogName <*> address

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = pure f <*> x <*> y

-- myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- myLiftA2 f x y = f <$> x <*> y

asks' :: (r -> a) -> Reader r a
asks' f = Reader f

-- increments the value inside a structure

foo' :: (Functor f, Num a) => f a -> f a 
foo' r = fmap (+1) r

-- tells us the length of the value inside the structure

bar' :: Foldable f => t -> f a -> (t, Int)
bar' r t = (r, length t)

-- one function to increment the value and record the length

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

barOne :: Foldable t => t a -> (t a, Int)
barOne x = (x , length x)

-- barTwo :: Foldable t => t a -> (t a, Int)
-- barTwo x = (foo' x, length x)

frooty :: Num a => [a] -> ([a], Int)
frooty x = bar' (foo' x) x

-- fooBind m k = \r -> k (m r) r

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- bind for functions
-- (r ->) a is like m a
-- (r ->) b is like m b
-- (>>=) ::  (r -> a) -> (a -> (r -> b)) -> (r -> b)

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) (Reader f) g = Reader $ \r -> runReader (g (f r)) r

  -- (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  -- (<*>) (Reader f) (Reader g) = Reader (\x -> f x (g x))
