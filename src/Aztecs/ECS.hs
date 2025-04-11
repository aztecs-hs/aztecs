{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS where

import Control.Monad.State.Strict
import Data.Bits
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.SparseSet.Strict (SparseSet)
import qualified Data.SparseSet.Strict as S
import Data.Word
import Prelude hiding (lookup)

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
  entities :: Query m Entity

data EntityCounter = EntityCounter
  { entitiesNextGeneration :: Word32,
    entitiesGenerations :: IntMap Word32,
    entitiesNextIndex :: Word32,
    entitiesFreeIndicies :: [Word32]
  }

emptyEntityCounter :: EntityCounter
emptyEntityCounter = EntityCounter 0 IntMap.empty 0 []

newtype EntitiesT m a = EntitiesT {unEntitiesT :: StateT EntityCounter m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance (Monad m) => MonadEntities (EntitiesT m) where
  spawn = EntitiesT $ do
    EntityCounter gen gens index free <- get
    let (i, nextIndex, free') = case free of
          (i' : rest) -> (i', index, rest)
          [] -> (index, index + 1, [])
        nextGeneration = gen + 1
        gens' = IntMap.insert (fromIntegral nextGeneration) i gens
    put $ EntityCounter nextGeneration gens' nextIndex free'
    return $ mkEntity i gen
  entities = Query $
    EntitiesT $ do
      EntityCounter _ gens _ _ <- get
      let go (i, g) = Just $ mkEntity (fromIntegral i) g
      return . map go $ IntMap.toList gens

runEntitiesT :: (Monad m) => EntitiesT m a -> EntityCounter -> m (a, EntityCounter)
runEntitiesT (EntitiesT m) = runStateT m
{-# INLINE runEntitiesT #-}

class (Monad m) => MonadAccess c m | m -> c where
  insert :: Entity -> c -> m ()
  lookup :: Entity -> m (Maybe c)
  query :: Query m c

newtype AccessT c m a = AccessT {unAccessT :: StateT (SparseSet Word32 c) m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance {-# OVERLAPPING #-} (Monad m) => MonadAccess c (AccessT c m) where
  insert e = AccessT . modify . S.insert (entityIndex e)
  lookup e = AccessT $ do
    s <- get
    return $ S.lookup s (entityIndex e)
  query = Query . AccessT $ S.toList <$> get

instance (MonadAccess c2 m) => MonadAccess c2 (AccessT c m) where
  insert e = lift . insert e
  lookup = lift . lookup
  query = Query . lift $ runQuery query

instance (MonadEntities m) => MonadEntities (AccessT c m) where
  spawn = lift spawn
  entities = Query . lift $ runQuery entities

runAccessT :: (Monad m) => AccessT c m a -> SparseSet Word32 c -> m (a, SparseSet Word32 c)
runAccessT (AccessT m) = runStateT m
{-# INLINE runAccessT #-}

newtype Query m a = Query {runQuery :: m [Maybe a]}
  deriving (Functor)

instance (Monad m) => Applicative (Query m) where
  pure x = Query $ return [Just x]
  Query f <*> Query x = Query $ do
    fs <- f
    zipWith (<*>) fs <$> x
