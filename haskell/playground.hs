bindExp :: Integer -> String
bindExp x = let y = 10 
                z = 11
            in
            (show y) ++ (show z)
  
newtype Foo a = Foo a deriving Show

fromFoo :: Show a => Foo a -> a
fromFoo (Foo a) = a

factorial' :: Integer -> Integer
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)
