module C7A where

mth :: Num a => a -> a -> a -> a
mth x y z = x * y * z

newtype Username =  Username String

newtype AccountNumber = AccountNumber Integer 

data User 
  = UnregisteredUser
  | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser =
  putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name) (AccountNumber number)) =
  putStrLn $ name ++ " " ++ show number

k :: (a, b) -> a
k (x, y) = x

k1 = k ((4 - 1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

f1 :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f1 (a, _, c) (d, _, f) = ((a, d), (c, f))

fC :: Ord a => a -> a -> a
fC x y = 
  case x > y of 
    True -> x
    False -> y

evenAdd2 :: Integer -> Integer
evenAdd2 x =
  case even x of
    True -> x + 2
    False -> x

nums :: Integer -> Integer
nums x = 
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

data Employee = Coder
  | Manager
  | Veep
  | CEO
  deriving (Eq, Ord, Show)  

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ boss ++ " is the boss of " ++ employee
  where
    boss = show e'
    employee = show e

employeeRank1 :: Employee -> Employee -> IO ()
employeeRank1 e e' =
  case compare e e' of
    LT -> reportBoss e e'
    GT -> reportBoss e' e
    EQ -> putStrLn "These employees are colleagues"

employeeRank2 :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
employeeRank2 compare' e e' =
  case compare' e e' of
    LT -> reportBoss e e'
    GT -> reportBoss e' e
    EQ -> putStrLn "These employees are colleagues"
  
compareEmployees :: Employee -> Employee -> Ordering
compareEmployees Coder Coder = EQ
compareEmployees Coder _ = GT
compareEmployees _ Coder = LT
compareEmployees e e' = compare e e'

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne = dodgy 1
oneIsTwo = flip dodgy $ 2

-- dodgy 1 1 == 20
-- dodgy 2 2 == 22
-- dodgy 1 2 == 21
-- dodgy 2 1 == 12
-- oneIsOne 1 == 11
-- oneIsOne 2 == 12
-- oneIsTwo 1 == 21
-- oneIsTwo 2 == 22
-- oneIsOne 3 == 31
-- oneIsTwo 3 == 23

-- guard syntax
myAbs :: Integer -> Integer
myAbs x
  | x < 0 = (-x)
  | otherwise = x

fooBarBaz1 :: Integer -> String
fooBarBaz1 x 
  | x > 0 = "foo"
  | x < 0 = "bar"
  | x == 0 = "baz"

data FooBarBaz 
  = Foo
  | Bar
  | Baz

fooBarBaz2 :: (Fractional a, Ord a) => a -> FooBarBaz 
fooBarBaz2 x 
  | y > 0   = Foo
  | y < 0   = Bar
  | otherwise  = Baz
  where y = x / 100

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.6 = 'D'
  | otherwise = 'F'
  where y = x / 100

pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs  = True
  | otherwise         = False

-- numbers :: (Ord a, Int a, Int b) => a -> b
-- numbers x
--   | x > 0   = 1
--   | x < 0   = -1
--   | x == 0  = 0

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

main = do
  print (tensDigit 1 :: Integer)
