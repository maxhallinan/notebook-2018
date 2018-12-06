module C26B where

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT m) = MaybeT $ (fmap . fmap) f m

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT $ (pure (pure x))
  (<*>) (MaybeT f) (MaybeT x) = MaybeT $ (<*>) <$> f <*> x

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  -- MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  -- f must be (a -> MaybeT m b) because this is the Monad instance of MaybeT
  -- and (>>=) has the signature m a -> (a -> m b) -> m b
  (>>=) (MaybeT m1) f = MaybeT $ do
    -- m here is the unknown Monad type 
    -- get the Maybe out of that Monad
    m2 <- m1
    case m2 of
      -- if the Maybe is Just, call (a -> MaybeT m b) on it
      -- but then discard the `MaybeT` layer
      Just x -> runMaybeT (f x)
      -- lift the Nothing into the unknown monadic context
      Nothing -> return Nothing

foo :: Maybe [String]
foo = Just $ do 
  x <- [""]
  return x
  -- x <- (Right (Right "foo"))
  -- case x of
  --   Left _ -> ""
  --   Right x -> ""
