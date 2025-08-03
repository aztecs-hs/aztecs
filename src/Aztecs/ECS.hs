{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS
  ( module Aztecs.ECS.Queryable,
    module Aztecs.ECS.Schedule,
    module Aztecs.ECS.Scheduler,
    PrimMonad (..),
    bundle,
    Query (..),
    runQuery,
    System (..),
    system,
    ECS (..),
    AztecsT (..),
    runAztecsT_,
    R (..),
    W (..),
    MkW (..),
    readW,
    writeW,
    modifyW,
  )
where

import Aztecs.ECS.Access.Internal
import qualified Aztecs.ECS.Access.Internal as A
import Aztecs.ECS.Class
import Aztecs.ECS.Entities
import qualified Aztecs.ECS.Entities as E
import Aztecs.ECS.Executor
import Aztecs.ECS.HSet (HSet (..), Lookup (..))
import qualified Aztecs.ECS.HSet as HS
import Aztecs.ECS.Query
import Aztecs.ECS.Queryable
import Aztecs.ECS.Queryable.Internal
import Aztecs.ECS.Schedule
import Aztecs.ECS.Scheduler
import qualified Aztecs.ECS.Scheduler as Scheduler
import Aztecs.ECS.System
import Aztecs.ECS.World (ComponentStorage, bundle)
import qualified Aztecs.ECS.World as W
import Control.Monad.Identity
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.SparseSet.Strict.Mutable as MS
import Data.Word
import Prelude hiding (Read, lookup)

newtype Commands cs m a = Commands
  {unCommands :: m (a, AztecsT cs m ())}
  deriving (Functor)

instance (Monad m) => Applicative (Commands cs m) where
  pure x = Commands $ pure (x, AztecsT $ pure ())
  Commands mf <*> Commands mx = Commands $ do
    (f, w1) <- mf
    (x, w2) <- mx
    return (f x, w1 >> w2)

instance (Monad m) => Monad (Commands cs m) where
  Commands mx >>= f = Commands $ do
    (x, w1) <- mx
    (y, w2) <- unCommands (f x)
    return (y, w1 >> w2)

instance MonadTrans (Commands cs) where
  lift m = Commands $ do
    x <- m
    return (x, AztecsT $ pure ())

instance (MonadIO m) => MonadIO (Commands cs m) where
  liftIO io = Commands $ do
    x <- liftIO io
    return (x, AztecsT $ pure ())

instance (PrimMonad m) => PrimMonad (Commands cs m) where
  type PrimState (Commands cs m) = PrimState m
  primitive f = Commands $ do
    x <- primitive f
    return (x, AztecsT $ pure ())

queue :: (Applicative m) => AztecsT cs m () -> Commands cs m ()
queue action = Commands $ pure ((), action)

runCommands :: (Monad m) => Commands cs m a -> AztecsT cs m a
runCommands (Commands m) = AztecsT $ do
  w <- get
  !(result, action) <- lift m
  unAztecsT action
  return result

newtype AztecsT cs m a = AztecsT {unAztecsT :: StateT (W.World m cs) m a}
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

instance MonadTrans (AztecsT cs) where
  lift = AztecsT . lift

instance (PrimMonad m) => ECS (AztecsT cs m) where
  type Entity (AztecsT cs m) = E.Entity
  type Bundle (AztecsT cs m) = W.Bundle cs m
  type Components (AztecsT cs m) = cs
  type Task (AztecsT cs m) = (Commands cs m)

  spawn b = AztecsT $ do
    w <- get
    (e, w') <- lift $ W.spawn b w
    put w'
    return e
  {-# INLINE spawn #-}
  insert e b = AztecsT $ do
    w <- get
    w' <- lift $ W.insert e b w
    put w'
  {-# INLINE insert #-}
  remove e = AztecsT $ do
    w <- get
    w' <- lift $ W.remove e w
    put w'
  {-# INLINE remove #-}
  task = runCommands
  {-# INLINE task #-}

runAztecsT_ :: (Monad m) => AztecsT cs m a -> W.World m cs -> m a
runAztecsT_ (AztecsT m) = evalStateT m
{-# INLINE runAztecsT_ #-}

newtype R a = R {unR :: a}
  deriving (Show, Eq, Functor)

instance (PrimMonad m, Lookup a cs) => Queryable (AztecsT cs m) (R a) where
  type QueryableAccess (R a) = '[Read a]
  queryable = AztecsT $ do
    w <- get
    !as <- lift $ MS.toList . lookup $ W.worldComponents w
    let go (_, c) = R c
    return . Query $ map (fmap go) as
  {-# INLINE queryable #-}

type W m a = MkW (PrimState m) a

data MkW s c = W
  { wIndex :: {-# UNPACK #-} !Word32,
    wSparseSet :: {-# UNPACK #-} !(ComponentStorage s c)
  }

readW :: (PrimMonad m) => W m c -> m c
readW r = MS.unsafeRead (wSparseSet r) (fromIntegral $ wIndex r)
{-# INLINE readW #-}

writeW :: (PrimMonad m) => W m c -> c -> m ()
writeW r = MS.unsafeWrite (wSparseSet r) (fromIntegral $ wIndex r)
{-# INLINE writeW #-}

modifyW :: (PrimMonad m) => W m c -> (c -> c) -> m ()
modifyW r f = MS.unsafeModify (wSparseSet r) (fromIntegral $ wIndex r) f
{-# INLINE modifyW #-}

instance (PrimMonad m, PrimState m ~ s, Lookup a cs) => Queryable (AztecsT cs m) (MkW s a) where
  type QueryableAccess (MkW s a) = '[Write a]
  queryable = AztecsT $ do
    w <- get
    let s = lookup @a $ W.worldComponents w
    !as <- MS.toList s
    let go (i, _) = W i s
    return . Query $ map (fmap go) as
  {-# INLINE queryable #-}

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
                  then Just (With)
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
                  else Just (Without)
            )
            allEntities
    return $ Query result
