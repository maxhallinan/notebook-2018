{-# LANGUAGE NoMonomorphismRestriction #-}
module C4 where

f1 :: a -> a
f1 x = x

f2 :: a -> a -> a
f2 x y = x

f3 :: a -> a -> a
f3 x y = y

f4 :: a -> b -> b
f4 x y =
  y

f5 :: a -> b -> b
f5 x y =
  y

-- fromIntegral :: (Num b, Integral a) => a -> b
-- fromIntegral x = x

example = 1

-- Num a => a
x1 = (* 9) 6

-- Num a => (a, [Char])
x2 = head [(0, "doge"), (1, "kitteh")]

-- (Integer, [Char])
x3 = head [(0 :: Integer, "doge"), (1, "kitteh")]

-- Bool
x4 = if False then True else False

-- Int
x5 = length [1,2,3,4,5]

x6 = 5

x7 = x6 + 5

-- Num a => a
x8 = x7 * 10

-- Num a => a -> a
f6 x = x * 10

-- Fractional a => a -> a
f7 x = x / 10

bigNum = (^) 5 $ 10
-- wahoo = bigNum $ 10

f8 = (+)
x9 = 1
f9 = f8 x9
x10 = f9 x9

functionC :: Ord a => a -> a -> Bool
functionC x y =
  if (x > y) then True else False

functionD :: (a, b) -> b
functionD (x, y) = y

id :: a -> a
id x = x

fd1 :: a -> b -> a
fd1 x y = x

co :: (b -> c) -> (a -> b) -> a -> c
co = (.)

a :: (a -> c) -> a -> a
a f x = x

a' :: (a -> b) -> a -> b
a' f x = f x

main :: IO ()
main = do
  print $ 1 + 3
  print 10
  print $ negate (-1)
  print $ 0 + blah
  where blah = negate 1

data Woot
data Blah

d :: Woot -> Blah
d = undefined

e :: (Blah, Woot) -> (Blah, Blah)
e (x, y) = (x, x)

d1 :: Int -> String
d1 = undefined

e1 :: String -> Char
e1 = undefined

u1 :: Int -> Char
u1 = e1 . d1

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

t :: A -> C
t = w . q

data D
data E
data F

qq :: D -> F
qq = undefined

ww :: E -> F
ww = undefined

zz :: (D, E) -> (F, F)
zz (x, y) = (qq x, ww y)

munge :: (a -> b) -> (b -> (c, d)) -> a -> c
munge f g =
  fst . g . f

