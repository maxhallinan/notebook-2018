module E2 where

tail' :: String -> String
tail' x = tail x

exclaim :: String -> String
exclaim x = (++) x "!" 

at4 :: String -> String
at4 = (: "") . (!! 4) 

after9 :: String -> String
after9 = drop 9

at3 :: String -> Char
at3 = (!! 3)

charFromStr :: Int -> Char
charFromStr = ("Curry is awesome!" !!)

rvrs :: String -> String
rvrs x =
  concat [awesome, empty, is, empty, curry ]
  where 
    awesome = drop 9 $ take 16 x
    curry = take 5 x 
    empty = " "
    is = drop 6 $ take 8 x
