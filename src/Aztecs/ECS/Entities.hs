{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.ECS.Entities
  ( MonadEntities (..),
    EntitiesT (..),
    runEntitiesT,
  )
where

import Aztecs.ECS
import Aztecs.ECS.Entities.Class
import Aztecs.ECS.Query
import Control.Monad.Primitive
import Control.Monad.State
import qualified Data.IntMap as IntMap

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

instance (PrimMonad m) => PrimMonad (EntitiesT m) where
  type PrimState (EntitiesT m) = PrimState m
  primitive = EntitiesT . lift . primitive
  {-# INLINE primitive #-}

runEntitiesT :: (Monad m) => EntitiesT m a -> EntityCounter -> m (a, EntityCounter)
runEntitiesT (EntitiesT m) = runStateT m
{-# INLINE runEntitiesT #-}
