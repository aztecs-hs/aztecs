{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Aztecs.World
  ( World (..),
    WorldComponents (..),
    empty,
    removeComponent,
    removeComponent',
    remove,
    SparseStorage,
  )
where

import qualified Aztecs.ECS.Class as ECS
import Aztecs.ECS.Component
import Aztecs.ECS.HSet
import qualified Aztecs.ECS.HSet as HS
import Aztecs.Entity
import Aztecs.Storage hiding (empty)
import qualified Aztecs.Storage as Storage
import Aztecs.World.Entities
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

type SparseStorage m = MSparseSet (PrimState m) Word32

type family WorldComponents m (cs :: [Type]) :: [Type] where
  WorldComponents m '[] = '[]
  WorldComponents m (c ': cs) = ComponentStorage m c c ': WorldComponents m cs

type WorldComponentSet m cs =
  HSet (WorldComponents m cs)

type WorldEntityComponents m cs =
  IntMap (Map TypeRep (WorldComponentSet m cs -> m (WorldComponentSet m cs)))

data World m cs = World
  { worldComponents :: !(HSet (WorldComponents m cs)),
    worldEntities :: {-# UNPACK #-} !Entities,
    worldEntityComponents :: {-# UNPACK #-} !(WorldEntityComponents m cs)
  }

empty :: (Monad m, Empty m (HSet (WorldComponents m cs))) => m (World m cs)
empty = do
  cs <- Storage.empty
  return $ World cs emptyEntities IntMap.empty
{-# INLINE empty #-}

lookupStorage ::
  (Lookup (ComponentStorage m c c) (WorldComponents m cs)) =>
  World m cs ->
  ComponentStorage m c c
lookupStorage = HS.lookup . worldComponents
{-# INLINE lookupStorage #-}

adjustStorage ::
  forall m cs c.
  ( PrimMonad m,
    Typeable c,
    Component m c,
    AdjustM m (ComponentStorage m c c) (WorldComponents m cs),
    Storage m (ComponentStorage m c)
  ) =>
  (ComponentStorage m c c -> m (ComponentStorage m c c)) ->
  World m cs ->
  m (World m cs)
adjustStorage f w = do
  cs <- HS.adjustM @m @(ComponentStorage m c c) f (worldComponents w)
  return $ w {worldComponents = cs}
{-# INLINE adjustStorage #-}

removeComponent ::
  forall m cs c.
  ( AdjustM m (ComponentStorage m c c) (WorldComponents m cs),
    PrimMonad m,
    Component m c,
    ECS.Entity m ~ Entity,
    Typeable c,
    Storage m (ComponentStorage m c)
  ) =>
  Entity ->
  World m cs ->
  m (World m cs)
removeComponent entity w = do
  let entityIdx = fromIntegral (entityIndex entity)
      componentType = typeRep (Proxy :: Proxy c)
      hooks = componentHooks (Proxy :: Proxy c)
  -- Run the onRemove hook first
  onRemove hooks entity
  w' <- adjustStorage @_ @_ @c (removeStorage entity) w
  let entityComponents' = IntMap.adjust (Map.delete componentType) entityIdx (worldEntityComponents w)
  return $ w' {worldEntityComponents = entityComponents'}
{-# INLINE removeComponent #-}

removeComponent' ::
  forall m (c :: Type) cs.
  (AdjustM m (SparseStorage m c) cs, PrimMonad m) =>
  Entity ->
  HSet cs ->
  m (HSet cs)
removeComponent' e components = HS.adjustM @m @(SparseStorage m c) go components
  where
    go s = do
      s' <- S.freeze s
      S.thaw (S.delete (entityIndex e) s')
{-# INLINE removeComponent' #-}

remove :: (Monad m) => Entity -> World m cs -> m (World m cs)
remove entity w = do
  let entityIdx = fromIntegral (entityIndex entity)
  case IntMap.lookup entityIdx (worldEntityComponents w) of
    Nothing -> return w
    Just componentRemovalFunctions -> do
      cs' <- foldr (<=<) return (Map.elems componentRemovalFunctions) (worldComponents w)
      let entityComponents' = IntMap.delete entityIdx (worldEntityComponents w)
      return $ w {worldComponents = cs', worldEntityComponents = entityComponents'}
{-# INLINE remove #-}
