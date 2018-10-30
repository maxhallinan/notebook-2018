{-# LANGUAGE RankNTypes #-}

module Comonad where

-- class Functor w => Comonad w where
--   extract :: w a -> a
--   duplicate :: w a -> w (w a)

-- instance Comonad (Store s) where
--   extract (Store state view) = view state
--   duplicate (Store state view) = Store state (\next -> Store next view)

-- -- instance Functor (Store s) where
-- --   fmap f (Store s a

-- data Store s a = Store
--   { here :: s
--   , view :: s -> a
--   }

-- move :: s -> Store s a -> Store s a
-- move s store = view (duplicate store) s


-- newtype Co w a = Co { runCo :: forall r. w (a -> r) -> r }

-- instance Comonad w => Monad (Co w) where

-- select :: Comonad w => Co w (a -> b) -> w a -> w b
-- select co w = runCo co (extend dist w) 
--   where dist fs f = fmap (f $) fs

