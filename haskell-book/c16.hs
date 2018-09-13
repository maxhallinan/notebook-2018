module C16 where


data Foo = Foo | Bar | Baz deriving (Eq, Show)

instance Functor Foo where
  fmap = undefined
