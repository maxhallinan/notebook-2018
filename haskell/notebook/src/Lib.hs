module Lib
    ( someFunc
    ) where

import Control.Applicative
import Data.Char
import Data.List

someFunc :: IO ()
someFunc = putStrLn "someFunc"

sequenceA :: (Applicative f) => [f a] -> f[a]
sequenceA [] = pure []
sequenceA x:xs = (:) <$> x <*> sequenceA xs
