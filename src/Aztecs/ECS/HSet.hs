{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS.HSet
  ( HSet,
    HSetT (..),
    Run (..),
    UnwrapSystem,
    GetConstraints,
    Before,
    After,
    EmptyStorage (..),
    Empty (..),
    Lookup (..),
    AdjustM (..),
    Subset (..),
    hcons,
    hempty,
  )
where

import Control.Monad.Identity
import Data.Kind
import Data.SparseSet.Strict.Mutable (MSparseSet, PrimMonad (..))
import qualified Data.SparseSet.Strict.Mutable as MS
import Data.Word
import Prelude hiding (lookup)

type HSet = HSetT Identity

data HSetT f ts where
  HEmpty :: HSetT f '[]
  HCons :: f t -> HSetT f ts -> HSetT f (t ': ts)

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

instance (ShowHSet f ts) => Show (HSetT f ts) where
  show = showHSet

class ShowHSet f ts where
  showHSet :: HSetT f ts -> String

instance ShowHSet f '[] where
  showHSet _ = "HEmpty"

instance (Show (f t), ShowHSet f ts) => ShowHSet f (t ': ts) where
  showHSet (HCons x xs) = "HCons " ++ show x ++ " (" ++ showHSet xs ++ ")"

class EmptyStorage m a where
  emptyStorage :: m a

instance (PrimMonad m, PrimState m ~ s) => EmptyStorage m (MSparseSet s Word32 a) where
  emptyStorage = MS.empty

class Empty m a where
  empty :: m a

instance (Applicative m) => Empty m (HSetT f '[]) where
  empty = pure HEmpty

instance (Monad m, EmptyStorage m (f t), Empty m (HSetT f ts)) => Empty m (HSetT f (t ': ts)) where
  empty = do
    xs <- emptyStorage
    rest <- empty
    pure (HCons xs rest)

type family Elem (t :: k) (ts :: [k]) :: Bool where
  Elem t '[] = 'False
  Elem t (t ': xs) = 'True
  Elem t (_ ': xs) = Elem t xs

class Lookup (t :: Type) (ts :: [Type]) where
  lookup :: HSetT f ts -> f t

instance {-# OVERLAPPING #-} Lookup t (t ': ts) where
  lookup (HCons x _) = x
  {-# INLINE lookup #-}

instance {-# OVERLAPPABLE #-} (Lookup t ts) => Lookup t (u ': ts) where
  lookup (HCons _ xs) = lookup xs
  {-# INLINE lookup #-}

class AdjustM m f t ts where
  adjustM :: (f t -> m (f t)) -> HSetT f ts -> m (HSetT f ts)

instance {-# OVERLAPPING #-} (Applicative m) => AdjustM m f t (t ': ts) where
  adjustM f (HCons x xs) = HCons <$> f x <*> pure xs
  {-# INLINE adjustM #-}

instance {-# OVERLAPPABLE #-} (Functor m, AdjustM m f t ts) => AdjustM m f t (u ': ts) where
  adjustM f (HCons y xs) = HCons y <$> adjustM f xs
  {-# INLINE adjustM #-}

class Subset (subset :: [Type]) (superset :: [Type]) where
  subset :: HSetT f superset -> HSetT f subset

instance Subset '[] superset where
  subset _ = HEmpty
  {-# INLINE subset #-}

instance
  ( Lookup t superset,
    Subset ts superset
  ) =>
  Subset (t ': ts) superset
  where
  subset hset = HCons (lookup hset) (subset @ts hset)
  {-# INLINE subset #-}

hcons :: (Applicative f) => t -> HSetT f ts -> HSetT f (t ': ts)
hcons x xs = HCons (pure x) xs

hempty :: HSetT f '[]
hempty = HEmpty
