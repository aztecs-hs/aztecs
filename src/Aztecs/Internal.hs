{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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

import Aztecs.ECS.Access.Internal (Access (..))
import Aztecs.ECS.Bundle
import Aztecs.ECS.Bundle.Class
import Aztecs.ECS.Class
import Aztecs.ECS.Commands
import Aztecs.ECS.Component (Component (ComponentStorage, componentHooks), Hooks (..))
import Aztecs.ECS.HSet (AdjustM, HSet (..), Lookup (..))
import qualified Aztecs.ECS.HSet as HS
import Aztecs.ECS.Query
import Aztecs.ECS.Query.Internal
import Aztecs.ECS.R
import qualified Aztecs.ECS.Scheduler as Scheduler
import Aztecs.ECS.W
import qualified Aztecs.Entity as E
import Aztecs.Storage
import qualified Aztecs.Storage as S
import Aztecs.World (SparseStorage, WorldComponents)
import qualified Aztecs.World as W
import qualified Aztecs.World.Entities as E
import Control.Monad.Primitive
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Proxy
import qualified Data.Set as Set
import Data.SparseSet.Strict.Mutable (MSparseSet (..))
import qualified Data.SparseSet.Strict.Mutable as MS
import qualified Data.SparseVector.Strict.Mutable as MSV
import Data.Typeable
import qualified Data.Vector.Strict.Mutable as MV
import Data.Word (Word32)
import Prelude hiding (Read, lookup)

newtype AztecsT cs m a = AztecsT {unAztecsT :: StateT (W.World m cs) m a}
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

instance MonadTrans (AztecsT cs) where
  lift = AztecsT . lift
  {-# INLINE lift #-}

instance (PrimMonad m) => ECS (AztecsT cs m) where
  type Entity (AztecsT cs m) = E.Entity
  type Task (AztecsT cs m) = (Commands (AztecsT cs) m)

  spawn b = do
    w <- AztecsT get
    let (e, counter) = E.mkEntityWithCounter (W.worldEntities w)
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
    Component (AztecsT cs m) c,
    AdjustM m (SparseStorage m c) (WorldComponents m cs)
  ) =>
  Bundleable c (AztecsT cs m)
  where
  bundle c = Bundle $ \entity -> do
    w <- AztecsT get
    let entityIdx = fromIntegral $ E.entityIndex entity
        componentType = typeOf c
        go = S.insertStorage entity c
        hooks = componentHooks (Proxy :: Proxy c)
    cs <- lift . HS.adjustM @_ @(SparseStorage m c) go $ W.worldComponents w
    let entityComponents' =
          IntMap.insertWith
            Map.union
            entityIdx
            (Map.singleton componentType (W.removeComponent' @m @c entity))
            (W.worldEntityComponents w)
    AztecsT $ put w {W.worldComponents = cs, W.worldEntityComponents = entityComponents'}
    onInsert hooks entity
  {-# INLINE bundle #-}

runAztecsT :: (Monad m) => AztecsT cs m a -> W.World m cs -> m (a, W.World m cs)
runAztecsT (AztecsT m) = runStateT m
{-# INLINE runAztecsT #-}

runAztecsT_ :: (Monad m) => AztecsT cs m a -> W.World m cs -> m a
runAztecsT_ (AztecsT m) = evalStateT m
{-# INLINE runAztecsT_ #-}

instance (PrimMonad m) => Queryable (AztecsT cs m) scope E.Entity where
  type QueryableAccess E.Entity = '[]
  queryable = AztecsT $ do
    w <- get
    let es = E.entities $ W.worldEntities w
        entMap = IntMap.fromList [(fromIntegral $ E.entityIndex e, e) | e <- es]
        !len = if null es then 0 else fromIntegral (maximum (map E.entityIndex es)) + 1
        fetch i = return $ IntMap.lookup (fromIntegral i) entMap
    return $ Query len fetch
  {-# INLINE queryable #-}

instance
  ( PrimMonad m,
    Lookup (ComponentStorage m a a) (WorldComponents m cs),
    Storage m (ComponentStorage m a)
  ) =>
  Queryable (AztecsT cs m) scope (With a)
  where
  type QueryableAccess (With a) = '[With a]
  queryable = AztecsT $ do
    w <- get
    Query sz f <-
      lift
        . S.queryStorageR
        . HS.lookup @(ComponentStorage m a a)
        $ W.worldComponents w
    let fetch i = lift (f i) >>= \case
          Just _ -> return (Just With)
          Nothing -> return Nothing
    return $ Query sz fetch
  {-# INLINE queryable #-}

instance
  ( PrimMonad m,
    Lookup (ComponentStorage m a a) (WorldComponents m cs),
    Storage m (ComponentStorage m a)
  ) =>
  Queryable (AztecsT cs m) scope (Without a)
  where
  type QueryableAccess (Without a) = '[Without a]
  queryable = AztecsT $ do
    w <- get
    (Query sz f) <-
      lift
        . S.queryStorageR
        . HS.lookup @(ComponentStorage m a a)
        $ W.worldComponents w
    let fetch i = lift (f i) >>= \case
          Just _ -> return Nothing
          Nothing -> return (Just Without)
    return $ Query sz fetch
  {-# INLINE queryable #-}

instance
  ( PrimMonad m,
    PrimState m ~ s,
    Lookup (MSparseSet s Word32 a) (WorldComponents m cs)
  ) =>
  Queryable (AztecsT cs m) scope (R scope a)
  where
  type QueryableAccess (R scope a) = '[Read a]
  queryable = AztecsT $ do
    w <- get
    let storage = HS.lookup @(MSparseSet s Word32 a) $ W.worldComponents w
        sparseVec = MSV.unMSparseVector (sparse storage)
        denseVec = dense storage
        !len = MV.length sparseVec
        fetch !i
          | fromIntegral i >= len = return Nothing
          | otherwise = do
              (!present, !denseIdx) <- lift $ MV.unsafeRead sparseVec (fromIntegral i)
              if present
                then do
                  !val <- lift $ MV.unsafeRead denseVec (fromIntegral denseIdx)
                  return $ Just (R val)
                else return Nothing
    return $ Query len fetch
  {-# INLINE queryable #-}

instance
  ( PrimMonad m,
    PrimState m ~ st,
    Lookup (ComponentStorage m a a) (WorldComponents m cs),
    Storage m (ComponentStorage m a)
  ) =>
  Queryable (AztecsT cs m) scope (W scope (Commands (AztecsT cs) m) a)
  where
  type QueryableAccess (W scope (Commands (AztecsT cs) m) a) = '[Write a]
  queryable = AztecsT $ do
    w <- get
    Query sz f <-
      lift
        . S.queryStorageW
        . HS.lookup @(ComponentStorage m a a)
        $ W.worldComponents w
    let liftToCommands m = Commands $ (,pure ()) <$> unsafeRunRunner m
        go (W r wf mf) =
          W
            (Runner $ liftToCommands r)
            (Runner . liftToCommands . wf)
            (Runner . liftToCommands . mf)
        fetch i = lift (f i) >>= \case
          Just w' -> return (Just (go w'))
          Nothing -> return Nothing
    return $ Query sz fetch
  {-# INLINE queryable #-}

-- Additional instance for direct AztecsT usage in scheduler
instance
  ( PrimMonad m,
    PrimState m ~ st,
    Lookup (MSparseSet st Word32 a) (WorldComponents m cs)
  ) =>
  Queryable (AztecsT cs m) scope (W scope (AztecsT cs m) a)
  where
  type QueryableAccess (W scope (AztecsT cs m) a) = '[Write a]
  queryable = AztecsT $ do
    w <- get
    let storage = HS.lookup @(MSparseSet st Word32 a) $ W.worldComponents w
        sparseVec = MSV.unMSparseVector (sparse storage)
        !len = MV.length sparseVec
        fetch !i
          | fromIntegral i >= len = return Nothing
          | otherwise = do
              (!present, !_denseIdx) <- lift $ MV.unsafeRead sparseVec (fromIntegral i)
              if present
                then do
                  let !w' = W
                        { readW = Runner $ AztecsT $ lift $ MS.unsafeRead storage (fromIntegral i),
                          writeW = \val -> Runner $ AztecsT $ lift $ MS.unsafeWrite storage (fromIntegral i) val,
                          modifyW = \fn -> Runner $ AztecsT $ lift $ MS.unsafeModify storage (fromIntegral i) fn
                        }
                  return $ Just w'
                else return Nothing
    return $ Query len fetch
  {-# INLINE queryable #-}
