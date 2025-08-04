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
    Run (..),
    UnwrapSystem,
    GetConstraints,
    Before,
    After,
    Lookup (..),
    AdjustM (..),
    Subset (..),
  )
where

import Control.Monad.Identity
import Data.Kind
import Data.SparseSet.Strict.Mutable (MSparseSet, PrimMonad (..))
import qualified Data.SparseSet.Strict.Mutable as MS
import Data.Word
import Prelude hiding (lookup)

data HSet ts where
  HEmpty :: HSet '[]
  HCons :: t -> HSet ts -> HSet (t ': ts)

data Before (sys :: Type)

data After (sys :: Type)

data Run (constraints :: [Type]) (sys :: Type) where
  Run :: sys -> Run constraints sys

type family UnwrapSystem (runSys :: Type) :: Type where
  UnwrapSystem (Run constraints sys) = sys
  UnwrapSystem sys = sys

type family GetConstraints (runSys :: Type) :: [Type] where
  GetConstraints (Run constraints sys) = constraints
  GetConstraints sys = '[]

instance (Show sys) => Show (Run constraints sys) where
  show (Run sys) = "Run " ++ show sys

instance (ShowHSet  ts) => Show (HSet ts) where
  show = showHSet

class ShowHSet  ts where
  showHSet :: HSet ts -> String

instance ShowHSet '[] where
  showHSet _ = "HEmpty"

instance (Show t, ShowHSet ts) => ShowHSet (t ': ts) where
  showHSet (HCons x xs) = "HCons " ++ show x ++ " (" ++ showHSet xs ++ ")"

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

class AdjustM m  t ts where
  adjustM :: (t-> m t) -> HSet ts -> m (HSet ts)

instance {-# OVERLAPPING #-} (Applicative m) => AdjustM m  t (t ': ts) where
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

