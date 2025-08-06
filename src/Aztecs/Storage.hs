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

import Aztecs.ECS.HSet
import qualified Aztecs.ECS.HSet as HS
import Aztecs.ECS.Query
import Aztecs.ECS.R
import Aztecs.ECS.W
import Aztecs.Entity
import Control.Monad.Primitive
import qualified Data.SparseSet.Strict as S
import Data.SparseSet.Strict.Mutable (MSparseSet)
import qualified Data.SparseSet.Strict.Mutable as MS
import Data.Word
import Prelude hiding (lookup)

class Storage m s where
  emptyStorage :: m (s a)

  insertStorage :: Entity -> a -> s a -> m (s a)

  removeStorage :: Entity -> s a -> m (s a)

  queryStorageR :: s a -> m (Query (R a))

  queryStorageW :: s a -> m (Query (W m a))

instance (PrimMonad m, PrimState m ~ s) => Storage m (MSparseSet s Word32) where
  emptyStorage = MS.empty
  {-# INLINE emptyStorage #-}

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
    let go (i, _) =
          W
            { readW = MS.unsafeRead s (fromIntegral i),
              writeW = MS.unsafeWrite s (fromIntegral i),
              modifyW = MS.unsafeModify s (fromIntegral i)
            }
    return . Query $ map (fmap go) as
  {-# INLINE queryStorageW #-}

class Empty m a where
  empty :: m a

instance (Applicative m) => Empty m (HSet '[]) where
  empty = pure HEmpty
  {-# INLINE empty #-}

instance (Monad m, Storage m s, Empty m (HSet ts)) => Empty m (HSet (s a ': ts)) where
  empty = do
    xs <- emptyStorage @m @s
    HCons xs <$> empty
  {-# INLINE empty #-}
