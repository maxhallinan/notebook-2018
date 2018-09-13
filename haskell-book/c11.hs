module C11 where

import qualified Data.Char

data Price = Price Integer deriving (Show)

data Size = Size Integer deriving (Show)

data Manufacturer = Mini | Mazda | Tata deriving (Show)

data Airline = PapuAir | Catapault | TakeYourChance deriving (Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Show)

car1 = Car Mini (Price 14000)

car2 = Car Mazda (Price 20000)

car3 = Car Tata (Price 7000)

plane1 = Plane PapuAir (Size 10000)

isCar :: Vehicle -> Bool
isCar (Plane _ _) = False
isCar (Car _ _) = True

isPlane :: Vehicle -> Bool 
isPlane = not . isCar

areCars :: [Vehicle] -> Bool
areCars = all isCar

getManufacturer :: Vehicle -> Manufacturer
getManufacturer (Car m _) = m

data Example1 = Example1

data Example2 = Example2 Int

data Foo = Foo deriving (Eq, Show)

data Bar = Bar deriving (Eq, Show)

data Baz = Baz deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = 
    First a b 
  | Second a b 
  deriving (Eq, Show)

data RecordProduct a b = 
  RecordProduct { first :: a
                , second :: b } 
                deriving (Eq, Show)


data OperatingSystem = 
    GnuPlusLinux
  | OpenBSD
  | Mac
  | Windows
  deriving (Eq, Show) 

data ProgrammingLanguage =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgrammingLanguage }
             deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = 
  [ GnuPlusLinux
  , OpenBSD
  , Mac
  , Windows
  ]

allProgrammingLanguages :: [ProgrammingLanguage]
allProgrammingLanguages = 
  [ Haskell
  , Agda
  , Idris
  , PureScript
  ]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer { os = os, lang = lang } | os <- allOperatingSystems, lang <- allProgrammingLanguages ]

isSubseqOf :: [Char] -> [Char] -> Bool
isSubseqOf sub seq = sub == (foldr helper "" seq)
  where helper = (\x acc -> if elem x sub then x : acc else acc)

capitWords :: String -> [(String, String)]
capitWords = fmap helper . words
  where helper word@(first : rest) = (word, Data.Char.toUpper first : rest)

capitWord :: String -> String
capitWord (first : rest) = Data.Char.toUpper first : rest

-- split :: (Char -> Bool) -> [Char] -> [[Char]]
-- split f = flip helper []
--   where helper string@(char : rest) result
--     | string == "" = result
--     | f char = helper string : result
--     | otherwise = helper string result

-- split :: (Char -> Bool) -> [Char] -> [[Char]]
-- split _ string = []
-- split f string@(c : rest) =
-- foldr helper []
  -- where helper sentence@(char : rest) sentences =
    

-- capitSentences :: String -> String
-- capitSentences = foldr (++) "" . fmap (capitWord) . Data.Text.split ('.' ==)

-- capitSentences :: String -> String
-- capitSentences s = helper s
--   where helper (c : rest) =
--     | c == '.'  = 
--     | otherwise = result ++ [c] ++ 
--     where result = ""
-- split :: (a -> Bool) -> [a] -> [[a]]
-- split _ [] = []
-- split predicate xs@(x : tail) 
--    | tail == []   = [[x]]
--    | predicate x  = [x] ++ [split predicate tail]
--    | otherwise    = x : split predicate tail

-- split :: (Char -> Bool) -> [Char] -> [[Char]]
-- split p [] = []
-- split p s@(c : []) = [[c]]
-- split p t = loop t
--   where go s@(c : tail) =
--     | tail == [] = [[c]]
--     | otherwise = l : split s

-- split :: (Char -> Bool) -> Text -> [Text]
-- split _ t@(Text _off _arr 0) = [t]
-- split p t = loop t
--     where loop s | null s'   = [l]
--                  | otherwise = l : loop (unsafeTail s')
--               where (# l, s' #) = span_ (not . p) s
--
-- fuck i am a useless ignoramous
-- why can't i get this shit correct
-- fuck me fuck me fuck me fuck me

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split x ys = chunk : split x rest
  where (chunk, rest) = break (== x) ys
