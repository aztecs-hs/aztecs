{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.Storage (Storage (..), Empty (..)) where

import Aztecs.ECS.HSet hiding (empty)
import qualified Aztecs.ECS.HSet as HS
import Aztecs.ECS.Query
import Aztecs.Entities
import Aztecs.R
import Aztecs.W
import Control.Monad.Identity
import Control.Monad.Primitive
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Kind
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import qualified Data.SparseSet.Strict as S
import Data.SparseSet.Strict.Mutable (MSparseSet)
import qualified Data.SparseSet.Strict.Mutable as MS
import Data.Word
import Prelude hiding (lookup)

class Storage m s where
  type StorageR m s a :: Type
  type StorageW m s a :: Type

  emptyStorage :: m (s a)

  insertStorage :: Entity -> a -> s a -> m (s a)

  removeStorage :: Entity -> s a -> m (s a)

  queryStorageR :: s a -> m (Query (StorageR m s a))

  queryStorageW :: s a -> m (Query (StorageW m s a))

instance (PrimMonad m, PrimState m ~ s) => Storage m (MSparseSet s Word32) where
  type StorageR m (MSparseSet s Word32) a = R a
  type StorageW m (MSparseSet s Word32) a = MkW s a

  emptyStorage = MS.empty

  insertStorage entity a s = do
    s' <- S.freeze s
    S.thaw $ S.insert (entityIndex entity) a s'
  {-# INLINE insertStorage #-}

  removeStorage entity s = do
    s' <- S.freeze s
    S.thaw $ S.delete (entityIndex entity) s'
  {-# INLINE removeStorage #-}

  queryStorageR s = do
    s' <- S.freeze s
    return . Query . fmap (fmap R) $ S.toList s'
  {-# INLINE queryStorageR #-}

  queryStorageW s = do
    !as <- MS.toList s
    let go (i, _) = W i s
    return . Query $ map (fmap go) as
  {-# INLINE queryStorageW #-}

class Empty m a where
  empty :: m a

instance (Applicative m) => Empty m (HSet '[]) where
  empty = pure HEmpty

instance (Monad m, Storage m s, Empty m (HSet ts)) => Empty m (HSet (s a ': ts)) where
  empty = do
    xs <- emptyStorage @m @s
    res <- empty
    return $ HCons xs res
