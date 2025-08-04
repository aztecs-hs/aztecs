{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}

module Aztecs.Internal
  ( AztecsT (..),
    runAztecsT,
    runAztecsT_,
  )
where

import Aztecs.ECS.Bundle
import Aztecs.ECS.Class
import Aztecs.ECS.Commands
import Aztecs.ECS.HSet (AdjustM, HSetT (..), Lookup (..))
import qualified Aztecs.ECS.HSet as HS
import Aztecs.ECS.Query
import Aztecs.ECS.Queryable
import qualified Aztecs.ECS.Scheduler as Scheduler
import Aztecs.Entities
import qualified Aztecs.Entities as E
import Aztecs.World (ComponentStorage)
import qualified Aztecs.World as W
import Control.Monad.Primitive
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.SparseSet.Strict as S
import qualified Data.SparseSet.Strict.Mutable as MS
import Data.Typeable
import Prelude hiding (Read, lookup)

newtype AztecsT cs m a = AztecsT {unAztecsT :: StateT (W.World m cs) m a}
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

instance MonadTrans (AztecsT cs) where
  lift = AztecsT . lift

instance (PrimMonad m) => ECS (AztecsT cs m) where
  type Entity (AztecsT cs m) = E.Entity
  type Components (AztecsT cs m) = cs
  type Task (AztecsT cs m) = (Commands (AztecsT cs) m)

  spawn b = do
    w <- AztecsT $ get
    let (e, counter) = mkEntityWithCounter (W.worldEntities w)
    AztecsT $ put w {W.worldEntities = counter}
    runBundle b e
    return e
  {-# INLINE spawn #-}
  insert e b = runBundle b e
  {-# INLINE insert #-}
  remove e = AztecsT $ do
    w <- get
    w' <- lift $ W.remove e w
    put w'
  {-# INLINE remove #-}
  task = runCommands
  {-# INLINE task #-}

instance
  ( PrimMonad m,
    Typeable c,
    AdjustM m (ComponentStorage (PrimState m)) c cs
  ) =>
  Bundleable c (AztecsT cs m)
  where
  bundle c = Bundle $ \entity -> do
    w <- AztecsT $ get
    let entityIdx = fromIntegral (entityIndex entity)
        componentType = typeOf c
        go s = do
          s' <- S.freeze s
          S.thaw $ S.insert (entityIndex entity) c s'
    cs <- lift . HS.adjustM go $ W.worldComponents w
    let entityComponents' =
          IntMap.insertWith
            Map.union
            entityIdx
            (Map.singleton componentType (W.removeComponent' @m @c entity))
            (W.worldEntityComponents w)
    AztecsT $ put w {W.worldComponents = cs, W.worldEntityComponents = entityComponents'}

runAztecsT :: (Monad m) => AztecsT cs m a -> W.World m cs -> m (a, W.World m cs)
runAztecsT (AztecsT m) = runStateT m
{-# INLINE runAztecsT #-}

runAztecsT_ :: (Monad m) => AztecsT cs m a -> W.World m cs -> m a
runAztecsT_ (AztecsT m) = evalStateT m
{-# INLINE runAztecsT_ #-}

instance (PrimMonad m) => Queryable (AztecsT cs m) E.Entity where
  type QueryableAccess E.Entity = '[]
  queryable = AztecsT $ do
    w <- get
    return . Query . map pure . E.entities $ W.worldEntities w

instance (PrimMonad m, Lookup a cs) => Queryable (AztecsT cs m) (With a) where
  type QueryableAccess (With a) = '[With a]
  queryable = AztecsT $ do
    w <- get
    withComponent <- MS.toList $ HS.lookup @a $ W.worldComponents w
    let withComponentIndices = Set.fromList $ map fst $ catMaybes withComponent
        allEntities = entities $ W.worldEntities w
        result =
          map
            ( \e ->
                if Set.member (entityIndex e) withComponentIndices
                  then Just With
                  else Nothing
            )
            allEntities
    return $ Query result

instance (PrimMonad m, Lookup a cs) => Queryable (AztecsT cs m) (Without a) where
  type QueryableAccess (Without a) = '[Without a]
  queryable = AztecsT $ do
    w <- get
    withComponent <- MS.toList $ HS.lookup @a $ W.worldComponents w
    let withComponentIndices = Set.fromList $ map fst $ catMaybes withComponent
        allEntities = entities $ W.worldEntities w
        result =
          map
            ( \e ->
                if Set.member (entityIndex e) withComponentIndices
                  then Nothing
                  else Just Without
            )
            allEntities
    return $ Query result

runCommands :: (Monad m) => Commands (AztecsT cs) m a -> AztecsT cs m a
runCommands (Commands m) = AztecsT $ do
  w <- get
  (result, action) <- lift m
  unAztecsT action
  return result
