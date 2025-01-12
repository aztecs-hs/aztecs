{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aztecs.Entity where

import Data.Kind (Type)

data Entity (ts :: [Type]) where
  ENil :: Entity '[]
  ECons :: t -> Entity ts -> Entity (t ': ts)

instance Show (Entity '[]) where
  show ENil = "[]"

instance (Show t, ShowEntity (Entity ts)) => Show (Entity (t ': ts)) where
  show (ECons x xs) = "[ " ++ show x ++ showEntity xs

class ShowEntity a where
  showEntity :: a -> String

instance ShowEntity (Entity '[]) where
  showEntity ENil = "]"

instance (Show t, ShowEntity (Entity ts)) => ShowEntity (Entity (t ': ts)) where
  showEntity (ECons x xs) = ", " ++ show x ++ showEntity xs

class Has a l where
  component :: l -> a

instance {-# OVERLAPPING #-} Has a (Entity (a ': ts)) where
  component (ECons x _) = x

instance {-# OVERLAPPING #-} (Has a (Entity ts)) => Has a (Entity (b ': ts)) where
  component (ECons _ xs) = component xs

entity :: t -> Entity '[t]
entity t = ECons t ENil

(<&>) :: Entity ts -> t -> Entity (t ': ts)
(<&>) = flip ECons

data (:&) a b = (:&) a b

class Component a

type family FromEntityType a where
  FromEntityType (a :& b) = a ': FromEntityType b
  FromEntityType (Entity ts) = ts
  FromEntityType a = '[a]

class FromEntity a where
  fromEntity :: Entity (FromEntityType a) -> a

instance {-# OVERLAPS #-} (FromEntityType a ~ '[a]) => FromEntity a where
  fromEntity (ECons a ENil) = a

instance FromEntity (Entity ts) where
  fromEntity = id

instance (FromEntity a, FromEntity b, FromEntityType (a :& b) ~ (a ': FromEntityType b)) => FromEntity (a :& b) where
  fromEntity (ECons a rest) = a :& fromEntity rest

type family ToEntityType a where
  ToEntityType (a :& b) = a ': ToEntityType b
  ToEntityType (Entity ts) = ts
  ToEntityType a = '[a]

class ToEntity a where
  toEntity :: a -> Entity (ToEntityType a)

instance {-# OVERLAPS #-} (ToEntityType a ~ '[a]) => ToEntity a where
  toEntity a = ECons a ENil

instance ToEntity (Entity ts) where
  toEntity = id

instance (ToEntity a, ToEntity b, ToEntityType (a :& b) ~ (a ': ToEntityType b)) => ToEntity (a :& b) where
  toEntity (a :& b) = ECons a (toEntity b)