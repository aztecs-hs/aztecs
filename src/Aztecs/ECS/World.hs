{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS.World
  ( World (..),
    empty,
    spawn,
    insert,
    removeComponent,
    remove,
    query,
  )
where

import Aztecs.ECS.Entities
import Aztecs.ECS.HSet hiding (empty)
import qualified Aztecs.ECS.HSet as HS
import Aztecs.ECS.Query
import Control.Monad
import Control.Monad.Primitive
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.SparseSet.Strict as S
import Data.SparseSet.Strict.Mutable (MSparseSet)
import Data.Typeable
import Data.Word
import Prelude hiding (Read, lookup)

data World m cs = World
  { worldComponents :: Components (PrimState m) cs,
    worldEntities :: Entities,
    worldEntityComponents :: IntMap (Map TypeRep (Components (PrimState m) cs -> m (Components (PrimState m) cs)))
  }

empty :: (Monad m, Empty m (Components (PrimState m) cs)) => m (World m cs)
empty = do
  cs <- HS.empty
  return $ World cs emptyEntities IntMap.empty

spawn :: forall m c cs. (AdjustM m (MSparseSet (PrimState m) Word32) c cs, PrimMonad m, Typeable c) => c -> World m cs -> m (Entity, World m cs)
spawn c w = do
  let (newEntity, counter) = mkEntityWithCounter (worldEntities w)
      entityIdx = fromIntegral (entityIndex newEntity)
      componentType = typeOf c
      go s = do
        s' <- S.freeze s
        S.thaw $ S.insert (entityIndex newEntity) c s'
      removeFunc :: Components (PrimState m) cs -> m (Components (PrimState m) cs)
      removeFunc components = do
        let removeGo s = do
              s' <- S.freeze s
              S.thaw $ S.delete (entityIndex newEntity) s'
        HS.adjustM @m @(MSparseSet (PrimState m) Word32) @c removeGo components
  cs <- HS.adjustM go $ worldComponents w
  let entityComponents' = IntMap.insertWith Map.union entityIdx (Map.singleton componentType removeFunc) (worldEntityComponents w)
      world' = w {worldComponents = cs, worldEntities = counter, worldEntityComponents = entityComponents'}
  return (newEntity, world')

insert :: forall m c cs. (AdjustM m (MSparseSet (PrimState m) Word32) c cs, PrimMonad m, Typeable c) => Entity -> c -> World m cs -> m (World m cs)
insert entity c w = do
  let entityIdx = fromIntegral (entityIndex entity)
      componentType = typeOf c
      go s = do
        s' <- S.freeze s
        S.thaw $ S.insert (entityIndex entity) c s'
      removeFunc :: Components (PrimState m) cs -> m (Components (PrimState m) cs)
      removeFunc components = do
        let removeGo s = do
              s' <- S.freeze s
              S.thaw $ S.delete (entityIndex entity) s'
        HS.adjustM @m @(MSparseSet (PrimState m) Word32) @c removeGo components
  cs <- HS.adjustM go $ worldComponents w
  let entityComponents' = IntMap.insertWith Map.union entityIdx (Map.singleton componentType removeFunc) (worldEntityComponents w)
  return $ w {worldComponents = cs, worldEntityComponents = entityComponents'}

removeComponent ::
  forall m cs c.
  (AdjustM m (MSparseSet (PrimState m) Word32) c cs, PrimMonad m, Typeable c) =>
  Entity ->
  World m cs ->
  m (World m cs)
removeComponent entity w = do
  let entityIdx = fromIntegral (entityIndex entity)
      componentType = typeRep (Proxy :: Proxy c)
      removeGo s = do
        s' <- S.freeze s
        S.thaw $ S.delete (entityIndex entity) s'
  cs <- HS.adjustM @m @(MSparseSet (PrimState m) Word32) @c removeGo (worldComponents w)
  let entityComponents' = IntMap.adjust (Map.delete componentType) entityIdx (worldEntityComponents w)
  return $ w {worldComponents = cs, worldEntityComponents = entityComponents'}

remove :: (PrimMonad m) => Entity -> World m cs -> m (World m cs)
remove entity w = do
  let entityIdx = fromIntegral (entityIndex entity)
  case IntMap.lookup entityIdx (worldEntityComponents w) of
    Nothing -> return w
    Just componentRemovalFunctions -> do
      cs' <- foldr (<=<) return (Map.elems componentRemovalFunctions) (worldComponents w)
      let entityComponents' = IntMap.delete entityIdx (worldEntityComponents w)
      return $ w {worldComponents = cs', worldEntityComponents = entityComponents'}

query :: (QueryTo (Components (PrimState m) cs) m a, PrimMonad m) => World m cs -> Query m a
query w = queryTo (worldComponents w) (worldEntities w)
