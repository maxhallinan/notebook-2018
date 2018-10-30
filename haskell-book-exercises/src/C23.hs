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
evalState :: State state value -> state -> value
evalState m s = fst (runState m s)
