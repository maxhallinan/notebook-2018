module C14 where

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada opt = opt
  mappend opt Nada = opt 
  mappend (Only opt1) (Only opt2) = Only (mappend opt1 opt2)
