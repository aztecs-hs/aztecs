{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS.HSet
  ( HSet (..),
    Lookup (..),
    AdjustM (..),
    Subset (..),
  )
where

import Data.Kind
import Prelude hiding (lookup)

data HSet ts where
  HEmpty :: HSet '[]
  HCons :: t -> HSet ts -> HSet (t ': ts)

instance (ShowHSet ts) => Show (HSet ts) where
  show = showHSet
  {-# INLINE show #-}

class ShowHSet ts where
  showHSet :: HSet ts -> String

instance ShowHSet '[] where
  showHSet _ = "HEmpty"
  {-# INLINE showHSet #-}

instance (Show t, ShowHSet ts) => ShowHSet (t ': ts) where
  showHSet (HCons x xs) = "HCons " ++ show x ++ " (" ++ showHSet xs ++ ")"
  {-# INLINE showHSet #-}

type family Elem (t :: k) (ts :: [k]) :: Bool where
  Elem t '[] = 'False
  Elem t (t ': xs) = 'True
  Elem t (_ ': xs) = Elem t xs

class Lookup (t :: Type) (ts :: [Type]) where
  lookup :: HSet ts -> t

instance {-# OVERLAPPING #-} Lookup t (t ': ts) where
  lookup (HCons x _) = x
  {-# INLINE lookup #-}

instance {-# OVERLAPPABLE #-} (Lookup t ts) => Lookup t (u ': ts) where
  lookup (HCons _ xs) = lookup xs
  {-# INLINE lookup #-}

class AdjustM m t ts where
  adjustM :: (t -> m t) -> HSet ts -> m (HSet ts)

instance {-# OVERLAPPING #-} (Applicative m) => AdjustM m t (t ': ts) where
  adjustM f (HCons x xs) = HCons <$> f x <*> pure xs
  {-# INLINE adjustM #-}

instance {-# OVERLAPPABLE #-} (Functor m, AdjustM m t ts) => AdjustM m t (u ': ts) where
  adjustM f (HCons y xs) = HCons y <$> adjustM f xs
  {-# INLINE adjustM #-}

class Subset (subset :: [Type]) (superset :: [Type]) where
  subset :: HSet superset -> HSet subset

instance Subset '[] superset where
  subset _ = HEmpty
  {-# INLINE subset #-}

instance (Lookup t superset, Subset ts superset) => Subset (t ': ts) superset where
  subset hset = HCons (lookup hset) (subset @ts hset)
  {-# INLINE subset #-}
