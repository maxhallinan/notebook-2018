module Database where

import qualified Data.Time

data DbItem = DbString String
            | DbNumber Integer
            | DbDate Data.Time.UTCTime
            deriving (Eq, Ord, Show)

theDatabase :: [DbItem]
theDatabase = 
  [ DbDate (
      Data.Time.UTCTime 
        (Data.Time.fromGregorian 1911 5 1) 
        (Data.Time.secondsToDiffTime 34123))
  , DbNumber 9001 
  , DbString "Hello, world!" 
  , DbDate (
      Data.Time.UTCTime 
        (Data.Time.fromGregorian 1921 5 1) 
        (Data.Time.secondsToDiffTime 34123))
  , DbNumber 9001 
  ]

filterDb :: (DbItem -> [a] -> [a]) -> [DbItem] -> [a]
filterDb f =  foldr f []

filterDbDate :: [DbItem] -> [Data.Time.UTCTime]
filterDbDate = filterDb f
  where 
    f (DbString _) acc = acc
    f (DbNumber _) acc = acc
    f (DbDate time) acc = time : acc

filterDbNumber :: [DbItem] -> [Integer]
filterDbNumber = filterDb f
  where 
    f (DbString _) acc = acc
    f (DbNumber n) acc = n : acc
    f (DbDate _) acc = acc

mostRecent :: [DbItem] -> Data.Time.UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DbItem] -> Integer
sumDb = (foldr (+) 0) . filterDbNumber

sum' :: Num a => [a] -> a
sum' = foldr (+) 0

avgDb :: [DbItem] -> Double
avgDb items = fromIntegral dbSum / fromIntegral dbLen
  where 
    dbSum = (sum' . filterDbNumber) items -- Integer
    dbLen = length items -- Int
