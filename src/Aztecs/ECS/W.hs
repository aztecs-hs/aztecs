{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS.W where

import Aztecs.ECS.HSet
import Aztecs.ECS.Query (Query (..))
import Aztecs.ECS.Queryable
import Aztecs.ECS.Queryable.Internal
import Control.Monad.Primitive
import qualified Data.SparseSet.Strict.Mutable as MS
import Data.Word
import Prelude hiding (Read, lookup)

type W m a = MkW (PrimState m) a

data MkW s c = W
  { wIndex :: {-# UNPACK #-} !Word32,
    wSparseSet :: {-# UNPACK #-} !(ComponentStorage s c)
  }

readW :: (PrimMonad m) => W m c -> m c
readW r = MS.unsafeRead (wSparseSet r) (fromIntegral $ wIndex r)
{-# INLINE readW #-}

writeW :: (PrimMonad m) => W m c -> c -> m ()
writeW r = MS.unsafeWrite (wSparseSet r) (fromIntegral $ wIndex r)
{-# INLINE writeW #-}

modifyW :: (PrimMonad m) => W m c -> (c -> c) -> m ()
modifyW r f = MS.unsafeModify (wSparseSet r) (fromIntegral $ wIndex r) f
{-# INLINE modifyW #-}

instance (PrimMonad m, PrimState m ~ s, Functor m, Lookup a cs) => Queryable cs m (MkW s a) where
  type QueryableAccess (MkW s a) = '[Write a]
  queryable cs _ = Query $ do
    let s = lookup @a cs
    !as <- MS.toList s
    let go (i, _) = W i s
    return $ map (fmap go) as
  {-# INLINE queryable #-}
