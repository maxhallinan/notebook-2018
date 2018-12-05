{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module C25A where

import Control.Applicative

-- Composing types is like composing functions, but on the type level.
-- Type constructors are like functions, and like functions, type constructors 
-- can be composed: `f (g a)`.
-- Functors are closed under composition - can compose two and return a third
-- functor.
-- The same is true of Applicatives.
-- Monads are _not_ closed under composition. Composing two monads does not 
-- necessarily result in another monad.
-- Stacking monads is a way of combining multiple effects.
-- Stacking Maybe and IO can express IO actions that have the possibility of 
-- failure.
-- A Monad Transformer is an ordinary type that takes an additional type argument.
-- The additional type argument is assumed to have a Monad instance.
-- `MaybeT` is the transformer variant of the Maybe type.
-- The transformer variant binds over both bits of structure.

-- The Compose newtype is a way to Compose type constructors instead of functions
newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

-- example:

foo :: Compose [] Maybe Integer
foo = Compose [Just 1, Nothing]

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)

  (<*>) (IdentityT fab) (IdentityT fa) = IdentityT (fab <*> fa)

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  -- (>>=) :: Monad m => Monad m => IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  -- must call runIdentityT and then put it back into IdentityT because otherwise
  -- IdentityT and the inner Monad `m` are joined, and the result is `m a`.
  (>>=) :: forall m a b. Monad m => IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (>>=) (IdentityT ma) f = IdentityT $ ma >>= (runIdentityT . f)
    where 
      -- Just playing around with types here:
      --
      -- ma :: m a
      -- f :: a -> IdentityT m b
      
      -- - Can't just `ma >>= f` because the type doesn't work out.
      -- - >>= expects f to be `a -> m b`. 
      -- - But f is `a -> IdentityT m b`.
      -- - The result is something (m (Identity (m b)))
      -- - THe same thing happens when you use fmap.
      -- - Can't just fmap f ma because the result is `m (IdentityT (m b))` and
      --   the desired result is `IdentityT m b`.
      -- - Need a way to join the two layers of `m`.
      -- - (runIdentityT . f) gets rid of the IdentityT layer between the two 
      --  layers of m m.
      --  Then those two layers are joined.
      
      foo1 :: m b 
      foo1 = ma >>= (runIdentityT . f)

      -- This doesn't work. >>= is expected to be `m a -> (a -> m b) -> m b`.
      -- The actual type here is `m a -> (a -> IdentityT m b) -> IdentityT m b`.
      -- aimb :: IdentityT m b
      -- aimb = ma >>= f
    
      -- fmap :: Functor f -> (a -> b) -> f a -> f b
      foo2 :: m (IdentityT m b)
      foo2 = fmap f ma

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = Compose $ (pure . pure) x
  -- (<*>) (Compose f1) (Compose x1) = Compose $ liftA2 (\f2 x2 -> f2 <*> x2) f1 x1
  (<*>) (Compose f1) (Compose x1) = Compose $ fmap (<*>) f1 <*> x1

-- Composing two Monads does not give you a Monad
-- return :: Monad m :: a -> m a
-- >>= :: Monad m :: m a -> (a -> m b) -> m b
-- >> :: Monad m :: m a -> m b
-- this is impossible

-- instance (Monad f, Monad g) => Monad (f g) where
--   return = pure
  -- >>= :: Monad m :: (a -> m b) -> m a -> m b
  -- >>= :: Compose f g a -> (a -> Compose f g b) -> Compose f g b
  -- (>>=) f (Monad x) = Compose $ 
  -- Monad f :: (a -> f b) -> f a -> f b
  -- Monad g :: (a -> g b) -> f a -> f b
  -- (Monad f, Monad g) => f (g a) -> (g a -> f (g b)) -> f (g b)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  -- foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap f (Compose x) = (foldMap . foldMap) f x

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  -- traverse :: Applicative f => (a -> f b) -> t (f a) -> f (t b)
  traverse f (Compose x) = Compose <$> (traverse . traverse) f x

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (c -> d) -> p a c -> p a d
  second f = bimap id f

data Deux a b = Deux a b deriving (Show)

instance Bifunctor Deux where
  bimap f g (Deux x y) = Deux (f x) (g y)

data Const' a b = Const' a deriving (Show)

instance Bifunctor Const' where
  bimap f g (Const' x) = Const' $ f x

data Drei a b c = Drei a b c deriving (Show)

instance Bifunctor (Drei a) where
  bimap f g (Drei x y z) = Drei x (f y) (g z)

data SuperDrei a b c = SuperDrei a b deriving (Show)

instance Bifunctor (SuperDrei a) where
  bimap f g (SuperDrei x y) = SuperDrei x $ f y

data SemiDrei a b c = SemiDrei a deriving (Show)

instance Bifunctor (SemiDrei a) where
  bimap f g (SemiDrei x) = SemiDrei x

data Quadriceps a b c d = Quadzzz a b c d 

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

data Either' a b = Left' a | Right' b

instance Bifunctor Either' where
  bimap f _ (Left' x) = Left' $ f x
  bimap _ g (Right' y) = Right' $ g y

-- when you compose two Monads, you do not get a Monad instance because you
-- can't implement join
-- Monad transformers are a way to compose two Monads.
-- A Monad transformer is a type constructor that takes a Monad as an argument
-- and returns a Monad as a result.
-- The two levels of polymorphism in bind are the problem. 
-- Monad transformers reduce the polymorphism by providing information about what
-- kind of Monad one of the instances is.
-- Motivation: bind over a type `IO (Reader String [a])`, for the monad instances
-- of IO, Reader, and []
-- Could make one-off types for each combination of Monad that we create:
-- newtype MaybeIO a = MaybeIO { runMaybeIO :: Maybe (IO a) }
-- *Can get a Monad from two Monads if we know the type of one of the monads*.
