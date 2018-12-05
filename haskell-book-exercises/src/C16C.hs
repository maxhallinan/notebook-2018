{-# LANGUAGE FlexibleInstances #-}

module C16C where

data Company a b c = DeepBlue a b | Something c deriving (Eq, Show)

instance Functor (Company a b) where
  fmap f (Something x) = Something (f x)
  fmap _ (DeepBlue x y) = DeepBlue x y

data More a b = L b a b | R a b a deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk x) = Desk x
  fmap f (Bloor x) = Bloor (f x)

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap f (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

-- Flip K Int Char == (K Char Int)
--
instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip $ K (f x)

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst (f x)

data LiftItOut f a = LiftItOut (f a)

data Parappa f g a = DaWrappa (f a) (g a)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
