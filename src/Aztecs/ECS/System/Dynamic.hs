{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      : Aztecs.ECS.System
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.System.Dynamic
  ( -- * Dynamic systems
    DynamicSystem (..),
    runDynamicSystem,

    -- ** Queries
    runQuery,
    runQueryFiltered,
    runQuerySingle,
    runQuerySingleMaybe,
    readQuery,
    readQueryFiltered,
    readQuerySingle,
    readQuerySingleMaybe,
  )
where

import Aztecs.ECS.Access.Internal (Access)
import Aztecs.ECS.Component
import Aztecs.ECS.Query.Dynamic (DynamicQuery)
import qualified Aztecs.ECS.Query.Dynamic as DQ
import Aztecs.ECS.World.Archetypes (Node (..))
import Aztecs.ECS.World.Entities (Entities)
import Data.Set (Set)
import Data.Vector.Strict (Vector)
import Prelude hiding (all, filter, map, mapM)

-- | Query operation.
data Op m a where
  RunQuery :: DynamicQuery m a -> Op m (Vector a)
  RunFiltered :: (Node m -> Bool) -> DynamicQuery m a -> Op m (Vector a)
  RunQuerySingle :: DynamicQuery m a -> Op m a
  RunQuerySingleMaybe :: DynamicQuery m a -> Op m (Maybe a)
  ReadQuery :: DynamicQuery m a -> Op m (Vector a)
  ReadQueryFiltered :: DynamicQuery m a -> (Node m -> Bool) -> Op m (Vector a)
  ReadQuerySingle :: DynamicQuery m a -> Op m a
  ReadQuerySingleMaybe :: DynamicQuery m a -> Op m (Maybe a)

-- | Run a query operation on entities.
runOp :: (Monad m) => Set ComponentID -> Op m a -> Entities m -> m (a, Entities m, Access m ())
runOp cIds (RunQuery q) es = DQ.runQueryDyn cIds q es
runOp cIds (RunFiltered flt q) es = DQ.runQueryFilteredDyn cIds flt q es
runOp cIds (RunQuerySingle q) es = DQ.runQuerySingleDyn cIds q es
runOp cIds (RunQuerySingleMaybe q) es = DQ.runQuerySingleMaybeDyn cIds q es
runOp cIds (ReadQuery q) es = do
  as <- DQ.readQueryDyn cIds q es
  return (as, es, return ())
runOp cIds (ReadQueryFiltered q flt) es = do
  as <- DQ.readQueryFilteredDyn cIds flt q es
  return (as, es, return ())
runOp cIds (ReadQuerySingle q) es = do
  a <- DQ.readQuerySingleDyn cIds q es
  return (a, es, return ())
runOp cIds (ReadQuerySingleMaybe q) es = do
  a <- DQ.readQuerySingleMaybeDyn cIds q es
  return (a, es, return ())
{-# INLINE runOp #-}

-- | Dynamic system.
data DynamicSystem m a where
  -- | Pure value.
  Pure :: a -> DynamicSystem m a
  -- | Functor map.
  Map :: (b -> a) -> DynamicSystem m b -> DynamicSystem m a
  -- | Applicative apply.
  Ap :: DynamicSystem m (b -> a) -> DynamicSystem m b -> DynamicSystem m a
  -- | Query operation.
  Op :: Set ComponentID -> Op m a -> DynamicSystem m a

instance Functor (DynamicSystem m) where
  fmap f (Pure a) = Pure (f a)
  fmap f s = Map f s
  {-# INLINE fmap #-}

instance Applicative (DynamicSystem m) where
  pure = Pure
  {-# INLINE pure #-}

  Pure f <*> s = fmap f s
  f <*> Pure a = fmap ($ a) f
  f <*> s = Ap f s
  {-# INLINE (<*>) #-}

-- | Run a dynamic system on entities, returning results, updated entities, and hooks to run.
runDynamicSystem :: (Monad m) => DynamicSystem m a -> Entities m -> m (a, Entities m, Access m ())
runDynamicSystem (Pure a) es = return (a, es, return ())
runDynamicSystem (Map f s) es = do
  (b, es', hook) <- runDynamicSystem s es
  return (f b, es', hook)
runDynamicSystem (Ap sf sa) es = do
  (f, es', hook1) <- runDynamicSystem sf es
  (a, es'', hook2) <- runDynamicSystem sa es'
  return (f a, es'', hook1 >> hook2)
runDynamicSystem (Op cIds op) es = runOp cIds op es
{-# INLINE runDynamicSystem #-}

runQuery :: Set ComponentID -> DynamicQuery m a -> DynamicSystem m (Vector a)
runQuery cIds q = Op cIds (RunQuery q)
{-# INLINE runQuery #-}

runQueryFiltered :: Set ComponentID -> DynamicQuery m a -> (Node m -> Bool) -> DynamicSystem m (Vector a)
runQueryFiltered cIds q flt = Op cIds (RunFiltered flt q)
{-# INLINE runQueryFiltered #-}

runQuerySingle :: Set ComponentID -> DynamicQuery m a -> DynamicSystem m a
runQuerySingle cIds q = Op cIds (RunQuerySingle q)
{-# INLINE runQuerySingle #-}

runQuerySingleMaybe :: Set ComponentID -> DynamicQuery m a -> DynamicSystem m (Maybe a)
runQuerySingleMaybe cIds q = Op cIds (RunQuerySingleMaybe q)
{-# INLINE runQuerySingleMaybe #-}

readQuery :: Set ComponentID -> DynamicQuery m a -> DynamicSystem m (Vector a)
readQuery cIds q = Op cIds (ReadQuery q)
{-# INLINE readQuery #-}

readQueryFiltered :: Set ComponentID -> (Node m -> Bool) -> DynamicQuery m a -> DynamicSystem m (Vector a)
readQueryFiltered cIds flt q = Op cIds (ReadQueryFiltered q flt)
{-# INLINE readQueryFiltered #-}

readQuerySingle :: Set ComponentID -> DynamicQuery m a -> DynamicSystem m a
readQuerySingle cIds q = Op cIds (ReadQuerySingle q)

readQuerySingleMaybe :: Set ComponentID -> DynamicQuery m a -> DynamicSystem m (Maybe a)
readQuerySingleMaybe cIds q = Op cIds (ReadQuerySingleMaybe q)
{-# INLINE readQuerySingleMaybe #-}
