module Print3Flipped where

hello :: String
hello = "Hello"

world :: String
world = "World!"

helloWorld :: String
helloWorld = hello ++ " " ++ world

main :: IO ()
main = do
  putStrLn helloWorld
  putStrLn helloWorld2
  where helloWorld2 =
            (++) hello $ (++) " " world



