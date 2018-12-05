module C20B where

import Data.ByteString.Lazy hiding (map)
import Network.Wreq

urls :: [String]
urls = [ "http://httpbin.org/ip"
       , "http://httpbin.org/bytes/5"
       ]

mappingGet :: [IO (Response ByteString)] 
mappingGet = map get urls

traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls

doThings :: IO ()
doThings = do
  traversedUrls
  return ()

-- traverse id $ Right "foo"
-- [Right 'f', Right 'o', Right 'o']
-- traverse f (Right y) = Right <$> f y
-- y == ['f', 'o', 'o']
-- id ['f','o','o'] == ['f','o','o']
-- Right <$> ['f', 'o', 'o'] == [Right 'f', Right 'o', Right 'o']
-- traverse f (Right x) = Right <$> f x
-- 1. f x should return an Applicative
-- 2. An Applicative is a Functor
-- 3. maps the Right constructor over this functor
-- The result is switching the outer structure (Either) with the inner structure,
-- the Applicative.

-- instance Traversable ((,) a) where
--   traverse f (x, y) = (,) x <$> f y


-- Traversable Laws
-- 1. Naturality
-- f produces the outer structure
--   if t takes that structure as an argument, then you should be able to compose (t . f)
-- t. t . traverse f = traverse (t . f)
-- 2. Identity
--    traverse Identity = Identity
-- 3. Composition


