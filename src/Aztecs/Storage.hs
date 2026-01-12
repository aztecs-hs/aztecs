{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
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
import Data.SparseSet.Strict.Mutable (MSparseSet (..))
import qualified Data.SparseSet.Strict.Mutable as MS
import qualified Data.SparseVector.Strict.Mutable as MSV
import Data.Vector.Fusion.Stream.Monadic (Step (..), Stream (..))
import qualified Data.Vector.Strict.Mutable as MV
import Data.Word
import Prelude hiding (lookup)

class Storage m s where
  emptyStorage :: m (s a)

  insertStorage :: Entity -> a -> s a -> m (s a)

  removeStorage :: Entity -> s a -> m (s a)

  -- | Query storage for read access. The result works for any scope.
  queryStorageR :: s a -> m (Query m scope (R scope a))

  -- | Query storage for write access. The result works for any scope.
  queryStorageW :: s a -> m (Query m scope (W scope m a))

instance (PrimMonad m, PrimState m ~ st) => Storage m (MSparseSet st Word32) where
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
    let sparseVec = MSV.unMSparseVector (sparse s)
        denseVec = dense s
        !len = MV.length sparseVec
        stream = Stream step 0
        step !i
          | i >= len = return Done
          | otherwise = do
              (!present, !denseIdx) <- MV.unsafeRead sparseVec i
              if present
                then do
                  !val <- MV.unsafeRead denseVec (fromIntegral denseIdx)
                  return $ Yield (Just (R val)) (i + 1)
                else return $ Yield Nothing (i + 1)
    return $ Query len stream
  {-# INLINE queryStorageR #-}

  queryStorageW s = do
    let sparseVec = MSV.unMSparseVector (sparse s)
        !len = MV.length sparseVec
        stream = Stream step 0
        step !i
          | i >= len = return Done
          | otherwise = do
              (!present, !denseIdx) <- MV.unsafeRead sparseVec i
              if present
                then do
                  let w =
                        W
                          { readW = Runner $ MS.unsafeRead s i,
                            writeW = Runner . MS.unsafeWrite s i,
                            modifyW = Runner . MS.unsafeModify s i
                          }
                  return $ Yield (Just w) (i + 1)
                else return $ Yield Nothing (i + 1)
    return $ Query len stream
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
