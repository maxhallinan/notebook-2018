module C3 where

data Mood = Blah | Woot deriving (Eq, Ord, Show)

changeMood :: Mood -> Mood
changeMood Blah = Woot 
changeMood Woot = Blah

isPalindrome :: String -> Bool
isPalindrome x =
  x == reversed 
  where
    reversed = reverse x

myAbs :: Integer -> Integer
myAbs x = 
  if x < 0 then
    x * (-1)
  else
    x

f1 :: (a, b) -> (c, d) -> ((b,d), (a,c))
f1 (a, b) (c, d) =
  ((b, d), (a, c))

f2 :: (a, b) -> (c, d) -> ((b,d), (a,c))
f2 x y =
  ((snd x, snd y), (fst x, fst y))

add = (+)

f3 :: String -> Int
f3 xs = 
  l `add` 1
  where
    l = length xs
