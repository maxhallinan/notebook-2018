{-# LANGUAGE InstanceSigs #-}
module C26 where

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor f => Functor (MaybeT f) where
  fmap f (MaybeT x) = MaybeT $ (fmap . fmap) f x

instance Applicative f => Applicative (MaybeT f) where
  pure x = MaybeT (pure (pure x))
  -- (<*>) (MaybeT f) (MaybeT x) = MaybeT $ fmap (<*>) (f <*> x)
  (<*>) (MaybeT f) (MaybeT x) = MaybeT $ ((<*>) <$> f <*> x)
  -- f :: m (Maybe (a -> b))
  -- x :: m (Maybe a)
  -- (<*>) :: Applicative f => f (Maybe (a -> b)) -> f (Maybe a) -> f (Maybe b)
  -- (<*>) (MaybeT f) (MaybeT x)

innerMost :: [Maybe (Identity (a -> b))] -> [Maybe (Identity a -> Identity b)]
innerMost = (fmap . fmap) (<*>)
-- so now you have a structure of a structure containing a function that will call apply on a third structure

second' :: [Maybe (Identity a -> Identity b)] -> [Maybe (Identity a) -> Maybe (Identity a)]
second' = fmap (<*>)

final' :: [Maybe (Identity a) -> Maybe (Identity b)] -> [Maybe (Identity a)] -> [Maybe (Identity b)]
final' = (<*>)
