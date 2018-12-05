{-# Language InstanceSigs #-}

module C23A where

import Control.Applicative (liftA2, liftA3)
import Control.Monad.Trans.State
import Control.Monad
import System.Random

-- State :: (s -> (a, s)) -> State s a
-- 
--

data Die 
  = SideOne
  | SideTwo
  | SideThree
  | SideFour
  | SideFive
  | SideSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 ->
      SideOne
    2 ->
      SideTwo
    3 ->
      SideThree
    4 ->
      SideFour
    5 ->
      SideFive
    6 ->
      SideSix
    _ ->
      error $ "must be a number 1 - 6"

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let gen1 = mkStdGen 0
      (d1, gen2) = randomR (1,6) gen1
      (d2, gen3) = randomR (1,6) gen2
      (d3, _)    = randomR (1,6) gen3
  (intToDie d1, intToDie d2, intToDie d3)

-- here the `do` block is a function waiting a StdGen argument that will be 
-- passed to randomR (1, 6)
-- this is very implicit
-- i don't like this way of writing it
rollDie1 :: State StdGen Die
rollDie1 = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

-- this is a nicer way to write it
-- clearer that state is passed a partially applied function
rollDie2 :: State StdGen Die
rollDie2 = intToDie <$> state (randomR (1,6))
-- rollDie2 :: StdGen -> (Die, StdGen)
-- rollDie2 = do
--   (n, s) <- randomR (1, 6)
--   return (intToDie n, s)

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie2

-- (rollsToGetTwenty . mkStdGen) <$> randomIO
rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
          let (die, nextGen) = randomR (1,6) gen
          in go (sum + die) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN limit g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= limit = count
      | otherwise =
          let (die, nextGen) = randomR (1,6) gen
          in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> [Die]
rollsCountLogged limit g = go 0 [] g
  where
    go :: Int -> [Die] -> StdGen -> [Die]
    go sum rolls gen
      | sum >= limit = rolls
      | otherwise =
          let (die, nextGen) = randomR (1,6) gen
          in go (sum + die) (intToDie die : rolls) nextGen


-- foo :: Int -> Int -> Maybe Int
-- foo a b =
--   Just b

-- bar :: Int -> Maybe Int
-- bar = do
--   x <- foo 1
--   return x 


-- newtype State s a = State { runState :: s -> (a,s) }
-- State wraps a function that takes an input state `s`, returns a product of an 
-- output value `a` and a new state value `s`.

-- `State state value` is a "state-passing computation" - a function that must be
-- run
-- `evalState` takes that State monad and an initial state, calls the state-passing
-- computation, and returns the result.
-- `evalState` discards the final state value.
-- so none of this state exists until evalState is called
-- the evaluation is lazy 
-- evalState :: State state value -> state -> value
-- evalState m s = fst (runState m s)

-- StdGen is (Int32, Int32)
-- mkStdGen :: Int -> StdGen
-- next :: g -> (Int, g) where g is a value of type StdGen
--
-- random :: (RandomGen g, Random a) => g -> (a, g)
--
-- System.Random has two ways of generating a random value:
-- - provide a seed value
-- - use the system-initialized generator
-- StdGen is a datatype from the library
-- StdGen is a product of two Int32 values
-- data StdGen = StdGen !Int32 !Int32
-- These Int32 values are seed values used to generate the next random number
-- `mkStdGen :: Int -> StdGen`
-- used to create an initial generator
-- maps an Int to a generator
-- `next :: g -> (Int, g)` 
-- Int in (Int, g) is a pseudorandom number
-- g is a value of type `StdGen`.
-- why doesn't the type signature of `next` include a type constraint?
-- `random :: (RandomGen g, Random a) => g -> (a, g)`
-- A way to generate random values that aren't numbers.
-- The range generated is determined by the type.

fooGen :: StdGen
fooGen = mkStdGen 0

-- the random number generation here is deterministic
-- given the same generator, we'll generate the same random number every time
barRandom1 :: (Int, StdGen)
barRandom1 = next fooGen
-- (2147482884,40014 40692)

barRandom2 :: (Int, StdGen)
barRandom2 = next fooGen
-- (2147482884,40014 40692)

-- `random` is used to generate values of different types
bazRandom1 :: (Char, StdGen)
bazRandom1 = random fooGen

bazRandom2 :: (Char, StdGen)
bazRandom2 = random fooGen

-- randomR is used to generate a random number within a range
-- (RandomGen g, Random a) => (a, a) -> g -> (a, g)

quxRandom1 :: (Int, StdGen)
quxRandom1 = randomR (0, 3) fooGen
-- (3,40014 40692)

-- to get a new random value, you have to pass to use the new StdGen returned 
-- with the last random value

-- The State newtype can help with this

-- newtype State s a = State { runState :: s -> (a, s) }

-- newtype Reader r a = Reader { runReader :: r -> a }

-- a newtyp emust have the same underlying representation as the type they wrap
-- a function contained in the newtype must be isomorphic to the type it wraps
-- an isomorphism is the ability to go from the newtype to the value it wraps
-- and back to the newtype without losing information

type Iso a b = (a -> b, b -> a)

newtype Sum a = Sum { getSum :: a }

sumIsIsomorphicWithItsContents :: Iso a (Sum a)
sumIsIsomorphicWithItsContents = (Sum, getSum)
-- the Sum data constructor is (a -> Sum a)
-- the getSum function is (Sum a -> a)
-- that is an isomorphism

-- Is not an isomorphism: (a -> Maybe b, b -> Maybe a)
-- if you go from a -> Nothing, there is no way to get back to a. the information
-- in a is lost


newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap f (Moi g) = Moi $ do
    (x, s) <- g
    return (f x, s)

instance Applicative (Moi s) where
  pure x = Moi (\s -> (x, s))

  (<*>) (Moi a1) (Moi a2) = Moi $ \s ->
    let (f, _) = a1 s -- a1 :: s -> ((a -> b), s)
        (x, _) = a2 s -- a2 :: s -> (a, s)
    in (f x, s)

instance Monad (Moi s) where
  return = pure
  
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  -- (>>=) f g = join $ pure g <*> f
  -- (>>=) f g = join $ g <$> f
  -- create a function that runs the two state dependent computations in sequence
  (>>=) (Moi f) g = Moi $ \s ->
    -- f :: s -> (a, s)
    -- run the first state-dependent computation
    let (x, s) = f s 
    -- produce a second Moi with (g x)
    -- run the computation in that second Moi
    in runMoi (g x) s
 
  -- (>>=) (Moi f1) f2 = Moi $ \ s ->
  --   let x = fst $ f1 s
  --       (Moi g) = f2 x
  --   in g
-- Moi $ \s ->
--     let x = fst $ f s
--         (Moi y) = g x
--     in (y, s)


get :: Moi s s
get = Moi (\s -> (s, s))

put :: s -> Moi s ()
put s = Moi (\_ -> ((), s))

exec :: Moi s a  -> s -> s
exec (Moi sa) s = snd $ sa s

eval :: Moi s a -> s -> a
eval (Moi sa) s = fst $ sa s

modify :: (s -> s) -> Moi s ()
modify f = Moi $ \ s -> ((), f s)
