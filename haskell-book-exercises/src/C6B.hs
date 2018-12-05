module C6B where

fib :: Integral a => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2) 

divideBy :: Integer -> Integer -> (Integer, Integer)
divideBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise =
              go (n - d) d (count + 1)

data Foo a = Empty | Cell a (Foo a) deriving Show

singleton :: Foo Integer
singleton = Cell 1 Empty

twoFoo :: Foo Integer
twoFoo = Cell 2 singleton

threeFoo :: Foo Integer
threeFoo = Cell 3 singleton

-- `:` or "cons" is a product type
-- how is it possible to use `[] a` as `[a]`?
-- how is it possible to use infix operators in a type declaration?
-- how do you know if you can use a data constructor infix?
