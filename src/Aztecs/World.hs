{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Aztecs.World
  ( World (..),
    empty,
    removeComponent,
    removeComponent',
    remove,
    Components,
    ComponentStorage,
  )
where

import Aztecs.ECS.Bundle
import Aztecs.ECS.HSet hiding (empty)
import qualified Aztecs.ECS.HSet as HS
import Aztecs.ECS.Query
import Aztecs.ECS.Queryable
import Aztecs.ECS.Queryable.Internal
import Aztecs.Entities
import Control.Monad
import Control.Monad.Primitive
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Kind
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import qualified Data.SparseSet.Strict as S
import Data.SparseSet.Strict.Mutable (MSparseSet)
import Data.Typeable
import Data.Word
import Prelude hiding (Read, lookup)

type ComponentStorage s = MSparseSet s Word32

type Components s = HSetT (ComponentStorage s)

data World m cs = World
  { worldComponents :: !(Components (PrimState m) cs),
    worldEntities :: {-# UNPACK #-} !Entities,
    worldEntityComponents :: {-# UNPACK #-} !(IntMap (Map TypeRep (Components (PrimState m) cs -> m (Components (PrimState m) cs))))
  }

empty :: (Monad m, Empty m (Components (PrimState m) cs)) => m (World m cs)
empty = do
  cs <- HS.empty
  return $ World cs emptyEntities IntMap.empty

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

removeComponent' ::
  forall m (c :: Type) cs.
  (AdjustM m (MSparseSet (PrimState m) Word32) c cs, PrimMonad m) =>
  Entity ->
  Components (PrimState m) cs ->
  m (Components (PrimState m) cs)
removeComponent' e components = do
  let removeGo s = do
        s' <- S.freeze s
        S.thaw $ S.delete (entityIndex e) s'
  HS.adjustM @m @(MSparseSet (PrimState m) Word32) @c removeGo components

remove :: (Monad m) => Entity -> World m cs -> m (World m cs)
remove entity w = do
  let entityIdx = fromIntegral (entityIndex entity)
  case IntMap.lookup entityIdx (worldEntityComponents w) of
    Nothing -> return w
    Just componentRemovalFunctions -> do
      cs' <- foldr (<=<) return (Map.elems componentRemovalFunctions) (worldComponents w)
      let entityComponents' = IntMap.delete entityIdx (worldEntityComponents w)
      return $ w {worldComponents = cs', worldEntityComponents = entityComponents'}
