{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module C25 where

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

newtype Identity a = Identity { runIdentity :: a }

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

