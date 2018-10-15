module C16B where

import Test.QuickCheck qualified QuickCheck

-- Outline
-- 1. Functor instances must have kind * -> *.
-- 2. Functor instances must obey the Functor laws.
-- 2a. Functors must obey the law of identity: fmap id (f a) == id (f a)
-- 2b. Functors must obey the law of composition: (fmap f . fmap g) (F a) == fmap (f . g) (F a)
-- 3. Functors must preserve the structure, transforming only the value.
-- 4. Goes through some examples of Functor instances that follow/break these rules.
-- 5. Explains some commonly used Functors
-- 6. Functors can be nested and fmap can be composed.
-- 7. Transforming the structure instead of the value ("natural transformation")
-- 8. What to do with type arguments that aren't the innermost type argument.

-- A functor is a way to apply a function over or around some structure without
-- altering the strucutre.

-- class Functor f where
--   fmap :: (a -> b) f a -> f b

-- Any instance of Functor must have kind * -> *.
-- The argument to a function must have kind *.
-- `(a -> b)` is applied to `f a`.
-- Because a must have kind *, then `f a` must have kind `* -> *`.

class Foo a where
  -- * -> *
  foo :: a -> a

class Bar a where
  -- a is * 
  -- f is * -> * 
  -- g is * -> * -> * -> *
  bar :: a -> f (g a b c)

class Baz a where
  -- e is * -> * -> *
  -- a is *
  -- b is *
  -- c is *
  -- d is *
  baz :: e a b -> (a -> c) -> (b -> d) -> e c d

-- These fail:
-- a is kind * -> * and must be kind *.
-- class Foo2 a where
--   foo2 :: a -> a b

-- class Bar2 a where
--   bar2 :: a b -> a

-- Exercises: Be Kind
-- What are the kinds of:
-- 1. What is the kind of `a` in a -> a
-- *
-- 2. What is the kind of `b` and `T` in `a -> b a -> T (b a)`
-- b: * -> *
-- T: * -> *
-- 3. What is the kind of `c` in `c a b -> c b a`.
-- c: * -> * -> *

-- <$> is the infix for `fmap`

-- A Functor must have kind * -> *
-- This makes sense because the type of fmap is `(a -> b) -> f a -> f b`.
-- `f` has kind `* -> *`.
-- If it has kind `*`, then the type of fmap doesn't make sense.
-- `(a -> b) -> a -> b` is just function application.
-- Likewise, f can't be kind `* -> * -> *`, because `f a` is kind `* -> *`.

data Foo2 a = Bar2 | Baz2 a deriving (Eq, Show)

instance Functor Foo2 where 
  fmap _ Bar2 = Bar2
  fmap f (Baz2 a) = Baz2 (f a)

data Foo3 a b = Bar3 a | Baz3 b deriving (Eq, Show)

instance Functor (Foo3 a) where
  fmap _ (Bar3 x) = Bar3 x
  fmap f (Baz3 x) = Baz3 (f x)

-- Law of identity

data Foo4 a = Bar4 | Baz4 a deriving (Eq, Show)

instance Functor Foo4 where
  fmap _ Bar4 = Bar4
  fmap f (Baz4 x) = Baz4 (f x)

lawOfId = id (Baz4 1) == fmap id (Baz4 1)

-- Law of composition

lawOfComposition = fmap (not . not) (Baz4 True) == (fmap not . fmap not) (Baz4 True)


-- Exercises: Heavy Lifting
-- Fix the following:

-- 1. a = (+1) $ read "[1]" :: [Int]
heavyLifting1 = fmap (+1) $ read "[1]" :: [Int]

-- 2. b = (++ "lol") (Just ["Hi,", "Hello"])
heavyLifting2 = fmap (fmap (++ "lol")) (Just ["Hi", "Hello"])

-- 3. c = (*2) (\x -> x - 2)
heavyLifting3 = fmap (*2) (\x -> x - 2)

-- 4. d = ((return '1' ++) . show) (\x -> [x, 1..3])
heavyLifting4 = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

--5. 
heavyLifting5 :: IO Integer
heavyLifting5 = let ioi = readIO "1" :: IO Integer
                    changed = fmap (read . ("123" ++) . show) ioi 
                in fmap (*3) changed

data Two a b = Two a b deriving (Eq, Show)

data Or a b = First a | Second b deriving (Eq, Show)

-- `a` here is part of the Functorial structure. It must be preserved just like
-- `Two`.
instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Functor (Or a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

-- QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity x = fmap id x == x

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f1 f2 x = (fmap f2 (fmap f1 x)) == (fmap (f2 . f1) x)
