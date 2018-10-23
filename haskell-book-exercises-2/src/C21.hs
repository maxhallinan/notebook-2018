module C21 where

-- class (Functor t, Foldable t) => Traversable t where
--   -- `traverse` maps each value in a structure to an action, evaluates the 
--   -- action from left to right, and collects the results
--   traverse :: (Applicative f) => (a -> f b) -> t a -> t (f b)
--   traverse f = sequenceA . fmap f

--   -- `sequenceA [return ()]` returns `Monad f => f [()]`
--  -- the effect is flipping two contexts or structures
--  -- sequenceA does not apply any user-defined function to the value inside the
--  -- inner structure, it just flips the structures.
--   sequenceA :: (Applicative f) => t (f a) -> f (t a)
--   sequenceA = traverse id

-- sum :: (Foldable t, Num a) => t a -> a
-- sum is a catamorphism that returns the summary value of a Foldable
-- `sum [1,2,3] == 6` 
-- `sum [Just 1, Just 2, Just 3] == [1,2,3]`
-- `sum`
-- `sum $ Just 1` returns 1


-- foo = sequenceA $ Just [1, 2, 3] == [Just 1, Just 2, Just 3]

-- foo' = sequenceA $ Just (Right 1) == Right (Just 1)

-- foo'' = sequenceA $ Just (Left 1) == Left 1

-- first apply a function `Applicative f => (a -> f b)`
-- fmap f returns a `(Traversable t, Applicative f) => t (f a)`
-- that value is exactly the type of the first argument to `sequenceA`
-- then sequenceA flips the structure, that's how you get the f (t a)
-- anytime you first map and then sequenceA, you can use traverse
-- traverse f = sequenceA . fmap f

-- Elm's Cmd.batch is `[ Cmd a ] -> Cmd a`
-- that seems like Foldable's `asum`
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Foldable.html#v:asum
-- take a `[ Cmd a ]` and get a `Cmd a`
-- No, that's more like a fold I guess.

-- `[Foo a] -> Foo a` 


-- "anytime you need to flip two type constructors
-- around, or map something and then flip them around, that’s probably
-- Traversable"

-- foo''' = sequenceA [Just 1, Nothing] == Nothing

-- data Query = Query 
-- data SomeObj = SomeObj
-- data IoOnlyObj = IoOnlyObj
-- data Err = Err

-- -- transforms a String into Err or SomeObj
-- decodeFn :: String -> Either Err SomeObj
-- decodeFn = undefined

-- -- takes a Query that's run against a database and returns a list of strings
-- fetchFn :: Query -> IO [String]
-- fetchFn = undefined

-- -- a "context" initializer"?
-- makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
-- makeIoOnlyObj = undefined

-- pipelineFn :: Query -> IO (Either Err (SomeObj, IoOnlyObj))
-- pipelineFn query = do 
--   -- `a` is `[String]`
--   a <- fetchFn query
--   -- `map decodeFn a` is `[Either Err SomeObj]`
--   -- `sequence (map decodeFn a)` is `Either Err [SomeObj]`
--   case sequence (map decodeFn a) of
--     -- err is Err
--     (Left err) -> return $ Left err
--     -- res is [SomeObj]
--     (Right res) -> do
--       -- `a` is [(SomeObj, IoOnlyObj)]
--       a <- makeIoOnlyObj res
--       return $ Right a

-- pipelineFn' :: Query -> IO (Either Err (SomeObj, IoOnlyObj))
-- pipelineFn' query = do
--   -- `a` is [String]
--   a <- fetchFn query
--   traverse makeIoOnlyObj foo -- IO (Either Err (SomeObj, IoOnlyObj))
--   -- (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
--   where foo = mapM decodeFn a -- foo is `Either Err [SomeObj]`


-- Chapter Exercises
-- Traversable Instances
-- 1. Identity

newtype Identity' a = Identity' a deriving (Eq, Ord, Show)

instance Functor Identity' where
  fmap f (Identity' x) = Identity' $ f x

instance Applicative Identity' where
  pure = Identity'
  (<*>) (Identity' f) (Identity' x) = Identity' $ f x

instance Foldable Identity' where
  foldr f empty (Identity' x) = f x empty

instance Traversable Identity' where
  traverse f (Identity' x) = fmap Identity' $ f x

instance Arbitrary a => Arbitrary (Identity' a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity' a

instance Eq a => EqProp (Identity' a) where
  (=-=) = eq

trigger1 :: Identity' (Int, Int, [Int])
trigger1 = undefined

runTests :: IO ()
runTests = do
  Checkers.quickBatch $ Classes.traversable trigger1

-- Traversable Instances
-- 2. Constant

newtype Constant a b = Constant { getConstant :: a }

instance Functor (Constant a) where
  fmap _ x = x 

instance Applicative (Constant a) where
  pure = Constant
  (<*>) _ x = x

