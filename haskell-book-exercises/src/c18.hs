module C18 where

import Control.Monad as Control.Monad

-- a functor maps a function over some structure
-- an applicative maps a function contained in some structure over some other structure
-- and combines the result of structure
-- monads are a another way to apply functions over structure
-- a monad is an applicative functor with some other features

-- class Functor (f :: * -> *) where
--   fmap :: (a -> b) -> f a -> f b
--   (<$) :: a -> f b -> f a

-- class Functor f => Applicative (f :: * -> *) where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b
--   (*>) :: f a -> f b -> f b
--   (<*) :: f a -> f b -> f a
--   liftA2 :: (a -> b -> c) -> f a -> f b -> f c

-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a

-- Applicative and Functor can be derived in terms of Monad
-- Functor can be derived in terms of Applicative
-- whenever you've implemented an instance of Monad for a type, you necessarily
-- have an Applicative and a Functor as well

-- fmap defined with monadic operations

fmap' f xs = xs >>= return . f

-- only need to define >>= for a minimally complete Monad instance

-- Monadic operations:
-- (>>=) - (also called bind)
-- (>>) - sequences two actions, discards the result of the first action
-- return - is the same as Applicative's pure. Takes a value and wraps it in structure.

-- fmap' :: Functor f => (a -> b) -> f a -> f b
-- <*> :: Applicative f => f (a -> b) -> f a -> f b
-- >>= :: Monad f => f a -> (a -> f b) -> f b

-- The problem: mapping a structure-returning function over a structure while 
-- maintaining one depth of structure.
-- e.g. fmap (\x -> [1, x]) [1,2,3] returns [[1,1], [1,2], [1,3]], and we want to return [1,1,1,2,1,3]
-- bind is a way to map a structure-returning function over structure without generating another depth of structure
-- With lists, you can compose fmap and concat to do this.
-- Monad is, in a sense, a generalization of concat.
-- Control.Monad.join :: Monad m => m (m a) -> m a
-- Control.Monad.join [[1,2], [2,3], [3,4]]
-- Monads make it possible to flatten structure
bind :: Monad m => (a -> m b) -> m a -> m b
bind f = Control.Monad.join . fmap f

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []

data Cow = Cow {
    name    :: String
  , age     :: Int
  , weight  :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0     = Just n
             | otherwise  = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
    then Nothing
    else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              weightCheck (Cow nammy agey weighty)

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>=
    \nammy ->
      noNegative age' >>=
        \agey ->
          noNegative weight' >>=
            \weighty ->
              weightCheck (Cow nammy agey weighty)

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i =
  if even i
  then Just (i + 1)
  else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

doSomething' n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a, b, c)

-- years ago 
type Founded = Int

-- number of programmers
type Coders = Int

data SoftwareShop = Shop { founded :: Founded, programmers :: Coders } deriving (Eq, Show)

data FoundedError = 
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n 
  | n < 0     = Left $ NegativeYears n
  | n > 500   = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0     = Left $ NegativeCoders n
  | n > 5000  = Left $ TooManyCoders n 
  | otherwise = Right n

-- The difference between using Maybe and Either in this way is that Maybe will
-- return a Nothing when it short circuits.
-- Either will return information about what went wrong.
-- Either short-circuits on the first thing to fail
mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded     <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
    then Left $ TooManyCodersForYears founded programmers
    else Right $ Shop founded programmers

data Sum a b  = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x)   = First x
  fmap f (Second x)  = Second $ f x

instance Applicative (Sum a) where
  pure = Second 
  (<*>) (First e) _           = First e
  (<*>) (Second f) x = fmap f x

instance Monad (Sum a) where
  return  = pure
  -- >>= :: m a -> (a -> m b) -> m b
  (>>=) (First e) _  = First e
  (>>=) (Second x) f = f x

s1 = Second (\x -> x ++ "foo")
s2 = First "bar"
s3 = s1 <*> s2

e1 = Right (\x -> x ++ "foo")
e2 = Left "bar"
e3 = e1 <*> e2
