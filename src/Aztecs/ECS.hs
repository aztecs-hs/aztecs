{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS where

import Control.Monad.State.Strict
import Data.Bits
import Data.SparseSet.Strict
import qualified Data.SparseSet.Strict as S
import Data.Word

newtype Entity = Entity {unEntity :: Word64}
  deriving (Eq, Ord, Show)

mkEntity :: Word32 -> Word32 -> Entity
mkEntity index generation = Entity $ (fromIntegral generation `shiftL` 32) .|. fromIntegral index

entityIndex :: Entity -> Word32
entityIndex (Entity e) = fromIntegral (e .&. 0xFFFFFFFF)

entityGeneration :: Entity -> Word32
entityGeneration (Entity e) = fromIntegral ((e `shiftR` 32) .&. 0xFFFFFFFF)

class (Monad m) => MonadAccess c m | m -> c where
  insert :: Entity -> c -> m ()

newtype AccessT c m a = AccessT {unAccessT :: StateT (SparseSet Word32 c) m a}
  deriving (Functor, Applicative, Monad)

instance (Monad m) => MonadAccess c (AccessT c m) where
  insert e = AccessT . modify . S.insert (entityIndex e)

runAccessT :: (Monad m) => AccessT c m a -> SparseSet Word32 c -> m (a, SparseSet Word32 c)
runAccessT (AccessT m) = runStateT m
{-# INLINE runAccessT #-}
