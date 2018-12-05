module Mathh where

showSteps :: String -> String -> String
showSteps x y = concat ["(", x, "+", y, ")"]

multiply :: Int -> Int -> String
multiply 0 _ = "0"
multiply _ 0 = "0"
multiply n1 n2 = foldr showSteps "0" (map show (take n1 (repeat n2)))
