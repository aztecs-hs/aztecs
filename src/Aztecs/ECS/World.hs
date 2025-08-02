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
  ( Bundle (..),
    bundle,
    World (..),
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
import Aztecs.ECS.Queryable
import Aztecs.ECS.Queryable.Internal
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

newtype Bundle cs m = Bundle {runBundle :: Entity -> World m cs -> m (World m cs)}

instance (Monad m) => Semigroup (Bundle cs m) where
  Bundle f <> Bundle g = Bundle $ \entity w -> f entity w >>= g entity

instance (Monad m) => Monoid (Bundle cs m) where
  mempty = Bundle $ \_ w -> return w

bundle ::
  forall cs m c.
  (AdjustM m (MSparseSet (PrimState m) Word32) c cs, PrimMonad m, Typeable c) =>
  c ->
  Bundle cs m
bundle c = Bundle $ \entity w -> do
  let entityIdx = fromIntegral (entityIndex entity)
      componentType = typeOf c
      go s = do
        s' <- S.freeze s
        S.thaw $ S.insert (entityIndex entity) c s'
  cs <- HS.adjustM go $ worldComponents w
  let entityComponents' = IntMap.insertWith Map.union entityIdx (Map.singleton componentType (removeComponent' @m @c entity)) (worldEntityComponents w)
  return w {worldComponents = cs, worldEntityComponents = entityComponents'}

data World m cs = World
  { worldComponents :: !(Components (PrimState m) cs),
    worldEntities :: {-# UNPACK #-} !Entities,
    worldEntityComponents :: {-# UNPACK #-} !(IntMap (Map TypeRep (Components (PrimState m) cs -> m (Components (PrimState m) cs))))
  }

empty :: (Monad m, Empty m (Components (PrimState m) cs)) => m (World m cs)
empty = do
  cs <- HS.empty
  return $ World cs emptyEntities IntMap.empty

spawn :: (Monad m) => Bundle cs m -> World m cs -> m (Entity, World m cs)
spawn c w = do
  let (newEntity, counter) = mkEntityWithCounter (worldEntities w)
      world' = w {worldEntities = counter}
  world'' <- runBundle c newEntity world'
  return (newEntity, world'')

insert :: Entity -> Bundle cs m -> World m cs -> m (World m cs)
insert entity b = runBundle b entity

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

query :: (Queryable cs m a) => World m cs -> Query m a
query w = queryable (worldComponents w) (worldEntities w)
{-# INLINE query #-}
