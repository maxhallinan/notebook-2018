{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ParsingJson where

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Text.RawString.QQ

-- "One of the mistakes programmers make in writing programs handling text is
-- allowing their data to stay in textual format..." 
-- Sometimes the AST that results from parsing text is not enough.
-- Sometimes the AST does not encode enough information about the meaning of the
-- text - just its structure.
-- Then the AST is transformed into a second data structure that has more meaning.
-- This second step is often called "marshalling".
-- Marshalling is "the act of preparing data for serialization"

-- Marshalling JSON

sectionJson :: ByteString
sectionJson = [r|
{
  "foo": { "name": "Name" },
  "bar": { "one": "annotation" }
}
|]

data TestData = TestData 
  { foo :: Name
  , bar :: Which
  } 
  deriving (Eq, Show)

newtype Name = Name String deriving (Eq, Show)

type Annotation = String

data Which = 
    One Annotation 
  | Two Annotation
  | Three Annotation
  deriving (Eq, Show)

-- main = do
--   let d :: Maybe TestData
--       d = decode sectionJson
--   print d

instance FromJSON TestData where
  parseJSON (Object v) = 
    TestData 
      <$> v .: "foo" 
      <*> v .: "bar"
  parseJSON _ = fail "Expected an Object for TestData"

instance FromJSON Name where
  parseJSON (Object v) = Name <$> v .: "name"
  parseJSON _ = fail "Expected an Object for Name"

instance FromJSON Which where
  parseJSON (Object v) = 
        (One <$> v .: "one") 
    <|> (Two <$> v .: "two") 
    <|> (Three <$> v .: "three")
  parseJSON _ = fail "Expected an Object for Which"

testDecoder = do
  let d :: Either String TestData
      d = eitherDecode sectionJson
  print d
