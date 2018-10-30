module C23 where

import Control.Applicative (liftA3)
import Control.Monad.Trans.State
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
