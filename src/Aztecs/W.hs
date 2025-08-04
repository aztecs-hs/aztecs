{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.W
  ( W (..),
    MkW (..),
    readW,
    writeW,
    modifyW,
  )
where

import Aztecs.ECS.HSet (Lookup (..))
import Aztecs.ECS.Query
import Aztecs.ECS.Queryable
import Aztecs.ECS.Queryable.Internal
import Aztecs.Internal
import Aztecs.World (ComponentStorage)
import qualified Aztecs.World as W
import Control.Monad.Primitive
import Control.Monad.State.Strict
import qualified Data.SparseSet.Strict.Mutable as MS
import Data.Word
import Prelude hiding (Read, lookup)

-- | See `MkW`.
type W m a = MkW (PrimState m) a

-- | Read-write 'Queryable' component access.
data MkW s c = W
  { wIndex :: {-# UNPACK #-} !Word32,
    wSparseSet :: {-# UNPACK #-} !(ComponentStorage s c)
  }

-- | Read a component referenced in `W`.
readW :: (PrimMonad m) => W m c -> m c
readW r = MS.unsafeRead (wSparseSet r) (fromIntegral $ wIndex r)
{-# INLINE readW #-}

-- | Write to a component referenced in `W`.
writeW :: (PrimMonad m) => W m c -> c -> m ()
writeW r = MS.unsafeWrite (wSparseSet r) (fromIntegral $ wIndex r)
{-# INLINE writeW #-}

-- | Modify a component referenced in `W`.
modifyW :: (PrimMonad m) => W m c -> (c -> c) -> m ()
modifyW r = MS.unsafeModify (wSparseSet r) (fromIntegral $ wIndex r)
{-# INLINE modifyW #-}

instance (PrimMonad m, PrimState m ~ s, Lookup a cs) => Queryable (AztecsT cs m) (MkW s a) where
  type QueryableAccess (MkW s a) = '[Write a]
  queryable = AztecsT $ do
    w <- get
    let s = lookup @a $ W.worldComponents w
    !as <- MS.toList s
    let go (i, _) = W i s
    return . Query $ map (fmap go) as
  {-# INLINE queryable #-}
