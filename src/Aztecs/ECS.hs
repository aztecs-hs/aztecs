{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

class (Monad m) => MonadEntities m where
  spawn :: m Entity

data EntityCounter = EntityCounter
  { entitiesNextGeneration :: Word32,
    entitiesNextIndex :: Word32,
    entitiesFreeIndicies :: [Word32]
  }

emptyEntityCounter :: EntityCounter
emptyEntityCounter = EntityCounter 0 0 []

newtype EntitiesT m a = EntitiesT {unEntitiesT :: StateT EntityCounter m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance (Monad m) => MonadEntities (EntitiesT m) where
  spawn = EntitiesT $ do
    EntityCounter gen index free <- get
    let (i, nextIndex, free') = case free of
          (i' : rest) -> (i', index, rest)
          [] -> (index, index + 1, [])
        nextGeneration = if null free' then gen else gen + 1
    put $ EntityCounter nextGeneration nextIndex free'
    return $ mkEntity i gen

runEntitiesT :: (Monad m) => EntitiesT m a -> EntityCounter -> m (a, EntityCounter)
runEntitiesT (EntitiesT m) = runStateT m
{-# INLINE runEntitiesT #-}

class (Monad m) => MonadAccess c m | m -> c where
  insert :: Entity -> c -> m ()

newtype AccessT c m a = AccessT {unAccessT :: StateT (SparseSet Word32 c) m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance (Monad m) => MonadAccess c (AccessT c m) where
  insert e = AccessT . modify . S.insert (entityIndex e)

instance (MonadEntities m) => MonadEntities (AccessT c m) where
  spawn = lift spawn

runAccessT :: (Monad m) => AccessT c m a -> SparseSet Word32 c -> m (a, SparseSet Word32 c)
runAccessT (AccessT m) = runStateT m
{-# INLINE runAccessT #-}
