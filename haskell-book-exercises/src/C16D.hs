{-# LANGUAGE RankNTypes #-}

module C16D where

-- data Foo = Foo | Bar | Baz deriving (Eq, Show)

-- instance Functor Foo where
--   fmap = undefined

data FixMePls a =
    FixMe
  | Pls a
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

data WhoCares a =
    ItDoesnt
  | Matter a
  | WhatThisIsCalled
  deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a) = Matter (f a)

replaceWithP :: a -> Char
replaceWithP = const 'p'

newtype Identity a = Identity a

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a 

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

data Four a b c d = Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

data Possibly a = Nope | Yep a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ Nope = Nope
  fmap f (Yep a) = Yep $ f a

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant v) = Constant v

data Wrap a b = Wrap (a b) deriving (Eq, Show)

instance Functor a => Functor (Wrap a) where
  fmap f (Wrap aB) = Wrap (fmap f aB)

wrapOne = Wrap (Just 1) 

wrapTwo = fmap (+ 1) wrapOne

nat :: (f a -> g a) -> f a -> g a
nat f x = f x

data Foo a = Foo' a deriving (Eq, Show)

data Bar a = Bar' a deriving (Eq, Show)

fooToBar :: Foo a -> Bar a
fooToBar (Foo' a) = Bar' a

type Nat1 f g = forall a . f a -> g a

-- this compiles
maybeToList :: Nat1 Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- this doesn't compile because requires `a` to be more specific
-- degenerateMtl :: Nat Maybe []
-- degenerateMtl Nothing = []
-- degenerateMtl (Just a) = [a + 1]

-- type Nat2 f g a = f a -> g a

-- this also works
-- maybeToList :: Nat Maybe [] a
-- maybeToList Nothing = []
-- maybeToList (Just a) = [a]

-- this works if we constrain `a`
-- but then this isn't a natural transformation
-- degenerateMtl :: Num a => Nat Maybe [] a
-- degenerateMtl Nothing = []
-- degenerateMtl (Just a) = [a + 1]

data Tuple a b = Tuple a b deriving (Eq, Show)

instance Functor (Tuple a) where
  fmap f (Tuple x y) = Tuple x (f y)

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

-- instance Functor (Flip Tuple a) where
--   fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b

data Sum' b a = First' a | Second' b deriving (Eq, Show)

instance Functor (Sum' a) where
  fmap f (First' a) = First' (f a)
  fmap f (Second' b) = Second' b
