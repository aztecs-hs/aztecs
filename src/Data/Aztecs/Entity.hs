{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aztecs.Entity where

import Data.Kind (Type)
import Prelude hiding (concat)

data Entity (ts :: [Type]) where
  ENil :: Entity '[]
  ECons :: t -> Entity ts -> Entity (t ': ts)

instance Show (Entity '[]) where
  show ENil = "[]"

instance (Show a, Show' (Entity as)) => Show (Entity (a ': as)) where
  show (ECons x xs) = "[" ++ show x ++ showRow xs

class Show' a where
  showRow :: a -> String

instance Show' (Entity '[]) where
  showRow ENil = "]"

instance (Show a, Show' (Entity as)) => Show' (Entity (a ': as)) where
  showRow (ECons x xs) = ", " ++ show x ++ showRow xs

entity :: a -> Entity '[a]
entity = flip ECons ENil

(<&>) :: Entity as -> a -> Entity (a : as)
(<&>) es c = ECons c es

type family ConcatT (a :: [Type]) (b :: [Type]) where
  ConcatT '[] b = b
  ConcatT (a ': as) b = a ': ConcatT as b

concat :: Entity as -> Entity bs -> Entity (ConcatT as bs)
concat ENil ys = ys
concat (ECons x xs) ys = ECons x (concat xs ys)

type family SplitT (a :: [Type]) (b :: [Type]) :: [Type] where
  SplitT '[] bs = bs
  SplitT (a ': as) (a ': bs) = SplitT as bs

class Split (a :: [Type]) (b :: [Type]) where
  split :: Entity b -> (Entity a, Entity (SplitT a b))

instance Split '[] bs where
  split e = (ENil, e)

instance forall a as bs. (Split as bs) => Split (a ': as) (a ': bs) where
  split (ECons x xs) =
    let (as, bs) = split @as xs
     in (ECons x as, bs)

data (:&) a b = (:&) a b

type family EntityT a where
  EntityT (a :& b) = a ': EntityT b
  EntityT (Entity ts) = ts
  EntityT a = '[a]

class FromEntity a where
  fromEntity :: Entity (EntityT a) -> a

instance {-# OVERLAPS #-} (EntityT a ~ '[a]) => FromEntity a where
  fromEntity (ECons a ENil) = a

instance FromEntity (Entity ts) where
  fromEntity = id

instance (FromEntity a, FromEntity b, EntityT (a :& b) ~ (a ': EntityT b)) => FromEntity (a :& b) where
  fromEntity (ECons a rest) = a :& fromEntity rest

class ToEntity a where
  toEntity :: a -> Entity (EntityT a)

instance {-# OVERLAPS #-} (EntityT a ~ '[a]) => ToEntity a where
  toEntity a = ECons a ENil

instance ToEntity (Entity ts) where
  toEntity = id

instance (ToEntity a, ToEntity b, EntityT (a :& b) ~ (a ': EntityT b)) => ToEntity (a :& b) where
  toEntity (a :& b) = ECons a (toEntity b)