{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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

data a :& b = a :& b

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

type family ElemT (a :: Type) (b :: [Type]) :: Bool where
  ElemT a '[] = 'False
  ElemT a (a ': as) = 'True
  ElemT a (b ': as) = ElemT a as

type family If (cond :: Bool) (true :: [Type]) (false :: [Type]) :: [Type] where
  If 'True true false = true
  If 'False true false = false

type family IntersectT (a :: [Type]) (b :: [Type]) :: [Type] where
  IntersectT '[] b = '[]
  IntersectT (a ': as) b = If (ElemT a b) (a ': IntersectT as b) (IntersectT as b)

class Intersect' (flag :: Bool) (a :: [Type]) (b :: [Type]) where
  intersect' :: Entity a -> Entity b -> Entity (IntersectT a b)

instance (Intersect as b, ElemT a b ~ 'True) => Intersect' 'True (a ': as) b where
  intersect' (ECons x xs) ys = ECons x (xs `intersect` ys)

instance (Intersect as (b ': bs), ElemT a (b ': bs) ~ 'False) => Intersect' 'False (a ': as) (b ': bs) where
  intersect' (ECons _ xs) = intersect xs

class Intersect (a :: [Type]) (b :: [Type]) where
  intersect :: Entity a -> Entity b -> Entity (IntersectT a b)

instance Intersect '[] b where
  intersect _ _ = ENil

instance (Intersect' (ElemT a bs) (a ': as) bs) => Intersect (a ': as) bs where
  intersect = intersect' @(ElemT a bs)

type family DifferenceT (a :: [Type]) (b :: [Type]) :: [Type] where
  DifferenceT '[] b = '[]
  DifferenceT (a ': as) b = If (ElemT a b) (DifferenceT as b) (a ': DifferenceT as b)

class Difference' (flag :: Bool) (a :: [Type]) (b :: [Type]) where
  difference' :: Entity a -> Entity b -> Entity (DifferenceT a b)

instance (Difference as b, DifferenceT (a ': as) b ~ DifferenceT as b) => Difference' 'True (a ': as) b where
  difference' (ECons _ xs) ys = xs `difference` ys

instance (Difference as (b ': bs), DifferenceT (a ': as) (b ': bs) ~ (a ': DifferenceT as (b ': bs))) => Difference' 'False (a ': as) (b ': bs) where
  difference' (ECons x xs) ys = ECons x (xs `difference` ys)

class Difference (a :: [Type]) (b :: [Type]) where
  difference :: Entity a -> Entity b -> Entity (DifferenceT a b)

instance Difference '[] b where
  difference _ _ = ENil

instance (Difference' (ElemT a bs) (a ': as) bs) => Difference (a ': as) bs where
  difference = difference' @(ElemT a bs)
