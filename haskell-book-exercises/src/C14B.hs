module C14B where

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada x = x
  (<>) x Nada = x
  (<>) (Only x1) (Only x2) = Only (x1 <> x2)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada opt = opt
  mappend opt Nada = opt 
  mappend (Only opt1) (Only opt2) = Only (mappend opt1 opt2)
