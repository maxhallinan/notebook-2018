module C9B where
import qualified Data.Bool
import qualified Data.Char

eftBool :: Bool -> Bool -> [Bool]
eftBool True True = [True]
eftBool True False = []
eftBool False True = [False, True]
eftBool False False = [False]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd GT _ = []
eftOrd LT LT = [LT]
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ LT = []
eftOrd EQ EQ = [EQ]
eftOrd EQ GT = [EQ, GT]

eftInt :: Int -> Int -> [Int]
eftInt start stop
  | stop < start = []
  | otherwise    = go stop start []
      where go current start result
              | current == start = current : result
              | otherwise        = go (pred current) start (current : result)

eftChar :: Char -> Char -> [Char]
eftChar start stop
  | stop < start = []
  | otherwise    = go stop start []
      where go current end result
              | current == end = current : result
              | otherwise      = go (pred current) end (current : result)

eftList :: (Ord a, Enum a) => a -> a -> [a]
eftList start stop
  | stop < start = []
  | otherwise    = go stop start []
      where go current end result
              | current == end = current : result
              | otherwise      = go (pred current) end (current : result)

-- why does:
-- eftChar 'a' 'z' work
-- eftChar 'A' 'Z' work
-- eftChar 'a' 'Z' throws an error: Exception: Prelude.Enum.Char.pred: bad argument
-- eftChar 'Z' 'a' returns "Z[\\]^_`a"
-- possible blog topics: use lambda calculus to show how 1 * 5 and 5 * 1 are different operations


-- "foo bar baz"
-- dropWhile (/= ' ') "foo bar baz" -> " bar baz"
-- takeWhile (/= ' ') "foo bar baz" -> "foo"

dropNotSpace :: [Char] -> [Char]
dropNotSpace = dropWhile (/= ' ')

dropSpace :: [Char] -> [Char]
dropSpace = dropWhile (== ' ')

myWords :: [Char] -> [[Char]]
myWords str = go str []
  where
      go "" result = result
      go source result =
          go ((dropSpace . dropNotSpace)  source) (result ++ [takeWhile (/= ' ') source])

dropNotNewLine :: [Char] -> [Char]
dropNotNewLine = dropWhile (/= '\n')

dropNewLine :: [Char] -> [Char]
dropNewLine = dropWhile (== '\n')

myLines :: [Char] -> [[Char]]
myLines str = go str []
  where
      go "" result = result
      go source result =
          go ((dropNewLine . dropNotNewLine)  source) (result ++ [takeWhile (/= '\n') source])

dropNotChar :: Char -> [Char] -> [Char]
dropNotChar char = dropWhile (/= char)

dropChar :: Char -> [Char] -> [Char]
dropChar char = dropWhile (== char)

split' :: Char -> [Char] -> [[Char]]
split' char str = go str []
  where
      go "" result = result
      go source result =
          go
            ((dropChar char . dropNotChar char)  source)
            (result ++ [takeWhile (/= char) source])

myWords2 :: [Char] -> [[Char]]
myWords2 = split' ' '

myLines2 :: [Char] -> [[Char]]
myLines2 = split' '\n'

itIsMystery :: [Char] -> [Bool]
itIsMystery xs =
  fmap (\x -> elem x "aeiou") xs

negateThree :: Int -> Int
negateThree n =
  Data.Bool.bool n (-n) (n == 3)

negated :: [Int]
negated = fmap negateThree [1..10]

filterWords :: String -> [String]
filterWords =
  filter (\word -> not $ elem word ["the", "a", "am"]) . words

-- is there a better way to write this?
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' xs ys = go xs ys []
  where
    go [] _ zipped  = zipped
    go _ [] zipped  = zipped
    go xs ys zipped =
      go (tail xs) (tail ys) (zipped ++ [(head xs, head ys)])

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f xs ys = go xs ys []
  where
    go [] _ zipped = zipped
    go _ [] zipped = zipped
    go xs ys zipped =
      go (tail xs) (tail ys) (zipped ++ [f (head xs) (head ys)])

zip2 :: [a] -> [b] -> [(a, b)]
zip2 = zipWith' (,)

filterUpper :: String -> String
filterUpper =
  filter Data.Char.isUpper

capitalizeFirst :: String -> String
capitalizeFirst "" = ""
capitalizeFirst source =
  Data.Char.toUpper (head source) : tail source

capitalizeAll :: String -> String
capitalizeAll "" = ""
capitalizeAll source =
  Data.Char.toUpper (head source) : capitalizeAll (tail source)

first :: [Char] -> [Char]
first "" = ""
first source = ((: []) . Data.Char.toUpper . head) source

firstChar :: [Char] -> Char
firstChar = Data.Char.toUpper . head
