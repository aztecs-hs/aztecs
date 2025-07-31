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

module Aztecs.ECS.World where

import Aztecs.ECS.Entities
import Aztecs.ECS.HSet
import qualified Aztecs.ECS.HSet as HS
import Aztecs.ECS.Query
import Control.Monad.Primitive
import qualified Data.SparseSet.Strict as S
import Data.SparseSet.Strict.Mutable (MSparseSet)
import Data.Word
import Prelude hiding (Read, lookup)

data World s cs = World
  { worldComponents :: Components s cs,
    worldEntities :: Entities
  }

empty :: (Monad m, Empty m (Components (PrimState m) cs)) => m (World (PrimState m) cs)
empty = do
  cs <- HS.empty
  return $ World cs emptyEntities

spawn :: (AdjustM m (MSparseSet (PrimState m) Word32) c cs, PrimMonad m) => c -> World (PrimState m) cs -> m (Entity, World (PrimState m) cs)
spawn c w = do
  let (newEntity, counter) = mkEntityWithCounter (worldEntities w)
      go s = do
        s' <- S.freeze s
        S.thaw $ S.insert (entityIndex newEntity) c s'
  cs <- HS.adjustM go $ worldComponents w
  let world' = w {worldComponents = cs, worldEntities = counter}
  return (newEntity, world')

query :: (QueryTo (Components s cs) m a) => World s cs -> Query m a
query w = queryTo (worldComponents w) (worldEntities w)
