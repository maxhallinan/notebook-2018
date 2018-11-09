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
