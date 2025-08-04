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

import Aztecs.Component (Component (..))
import Aztecs.ECS.HSet (Lookup (..))
import Aztecs.ECS.Query
import Aztecs.ECS.Queryable
import Aztecs.ECS.Queryable.Internal
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.SparseSet.Strict.Mutable (MSparseSet)
import qualified Data.SparseSet.Strict.Mutable as MS
import Data.Word
import Prelude hiding (Read, lookup)

-- | See `MkW`.
type W m a = MkW (PrimState m) a

-- | Read-write 'Queryable' component access.
data MkW s c = W
  { wIndex :: {-# UNPACK #-} !Word32,
    wSparseSet :: {-# UNPACK #-} !(MSparseSet s Word32 c)
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
