module NonZero where

-- two examples of refinement types

-- represents the type of values `a` such taht the value is not equal to 0
-- cardinality: a - 1
newtype NonZero a = UnsafeNonZero { unNonZero :: a }

nonZero :: (Num a, Eq a) => a -> Maybe (NonZero a)
nonZero 0 = Nothing
nonZero i = Just (UnsafeNonZero i)

-- represents the type of lists of a that are not empty
-- cardinality [a] - 1
-- the cardinality of [a] is infinite
-- [a] - 1 is also infinite
data NonEmpty a = a :| [a]

nonEmpty :: [a] -> Maybe (NonEmpty a)
nonEmpty []     = Nothing
nonEmpty (x:xs) = Just (x :| xs)
