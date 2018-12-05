module C5A where

data DayOfWeek = 
  Sun | Mon | Tue | Wed | Thu | Fri | Sat

data Date =
  Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Sun Sun = True
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) _ _     = False


