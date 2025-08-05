{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.Internal
  ( AztecsT (..),
    runAztecsT,
    runAztecsT_,
  )
where

import Aztecs.Component (Component (ComponentStorage))
import Aztecs.ECS.Bundle
import Aztecs.ECS.Class
import Aztecs.ECS.Commands
import Aztecs.ECS.HSet (AdjustM, HSet (..), Lookup (..))
import qualified Aztecs.ECS.HSet as HS
import Aztecs.ECS.Query
import Aztecs.ECS.Queryable.Internal
import qualified Aztecs.ECS.Scheduler as Scheduler
import Aztecs.Entities
import qualified Aztecs.Entities as E
import Aztecs.R
import Aztecs.Storage
import qualified Aztecs.Storage as S
import Aztecs.W
import Aztecs.World (SparseStorage, WorldComponents)
import qualified Aztecs.World as W
import Control.Monad.Primitive
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Typeable
import Prelude hiding (Read, lookup)

newtype AztecsT cs m a = AztecsT {unAztecsT :: StateT (W.World m cs) m a}
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

instance MonadTrans (AztecsT cs) where
  lift = AztecsT . lift
  {-# INLINE lift #-}

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
    AdjustM m (SparseStorage m c) (WorldComponents m cs)
  ) =>
  Bundleable c (AztecsT cs m)
  where
  bundle c = Bundle $ \entity -> do
    w <- AztecsT $ get
    let entityIdx = fromIntegral (entityIndex entity)
        componentType = typeOf c
        go s = S.insertStorage entity c s
    cs <- lift . HS.adjustM @_ @(SparseStorage m c) go $ W.worldComponents w
    let entityComponents' =
          IntMap.insertWith
            Map.union
            entityIdx
            (Map.singleton componentType (W.removeComponent' @m @c entity))
            (W.worldEntityComponents w)
    AztecsT $ put w {W.worldComponents = cs, W.worldEntityComponents = entityComponents'}
  {-# INLINE bundle #-}

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
  {-# INLINE queryable #-}

instance
  ( PrimMonad m,
    Lookup (ComponentStorage m a a) (WorldComponents m cs),
    Storage m (ComponentStorage m a)
  ) =>
  Queryable (AztecsT cs m) (With a)
  where
  type QueryableAccess (With a) = '[With a]
  queryable = AztecsT $ do
    w <- get
    withComponent <-
      lift
        . S.queryStorageR
        . HS.lookup @(ComponentStorage m a a)
        $ W.worldComponents w
    return . fmap (const With) $ withComponent
  {-# INLINE queryable #-}

instance
  ( PrimMonad m,
    Lookup (ComponentStorage m a a) (WorldComponents m cs),
    Storage m (ComponentStorage m a)
  ) =>
  Queryable (AztecsT cs m) (Without a)
  where
  type QueryableAccess (Without a) = '[Without a]
  queryable = AztecsT $ do
    w <- get
    (Query cs) <-
      lift
        . S.queryStorageR
        . HS.lookup @(ComponentStorage m a a)
        $ W.worldComponents w
    let go m = case m of
          Just v -> Nothing
          Nothing -> Just Without
    return . Query $ fmap go cs
  {-# INLINE queryable #-}


instance
  ( PrimMonad m,
    Lookup (ComponentStorage m a a) (WorldComponents m cs),
    Storage (AztecsT cs m) (ComponentStorage m a)
  ) =>
  Queryable (AztecsT cs m) (R a)
  where
  type QueryableAccess (R a) = '[Read a]
  queryable = do
    w <- AztecsT $ get
    S.queryStorageR . HS.lookup @(ComponentStorage m a a) $ W.worldComponents w
  {-# INLINE queryable #-}

instance
  ( PrimMonad m,
    PrimState m ~ s,
    Lookup (ComponentStorage m a a) (WorldComponents m cs),
    Storage m (ComponentStorage m a)
  ) =>
  Queryable (AztecsT cs m) (W (Commands (AztecsT cs) m) a)
  where
  type QueryableAccess (W (Commands (AztecsT cs) m) a) = '[Write a]
  queryable = AztecsT $ do
    w <- get
    Query results <-
      lift
        . S.queryStorageW
        . HS.lookup @(ComponentStorage m a a)
        $ W.worldComponents w
    let liftToCommands m = Commands $ (\x -> (x, pure ())) <$> m
        go (W r wf mf) =
          W
            (liftToCommands r)
            (liftToCommands . wf)
            (liftToCommands . mf)
    return . Query $ map (fmap go) results
  {-# INLINE queryable #-}

-- Additional instance for direct AztecsT usage in scheduler
instance
  ( PrimMonad m,
    PrimState m ~ s,
    Lookup (ComponentStorage m a a) (WorldComponents m cs),
    Storage m (ComponentStorage m a)
  ) =>
  Queryable (AztecsT cs m) (W (AztecsT cs m) a)
  where
  type QueryableAccess (W (AztecsT cs m) a) = '[Write a]
  queryable = AztecsT $ do
    w <- get
    Query results <-
      lift
        . S.queryStorageW
        . HS.lookup @(ComponentStorage m a a)
        $ W.worldComponents w
    let liftToAztecs (W r wf mf) =
          W
            (AztecsT $ lift r)
            (AztecsT . lift . wf)
            (AztecsT . lift . mf)
    return . Query $ map (fmap liftToAztecs) results
  {-# INLINE queryable #-}
