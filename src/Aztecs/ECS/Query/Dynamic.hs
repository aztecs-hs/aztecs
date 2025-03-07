{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Aztecs.ECS.Query.Dynamic
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Query.Dynamic
  ( -- * Dynamic queries
    DynamicQuery,
    DynamicQueryT (..),

    -- ** Operations
    entityDyn,
    fetchDyn,
    fetchMaybeDyn,
    zipFetchMapDyn,
    zipFetchMapAccumDyn,
    zipFetchMapDynM,
    zipFetchMapAccumDynM,

    -- ** Running
    allDyn,
    filterDyn,
    singleDyn,
    singleMaybeDyn,
    queryEntitiesDyn,
    mapDyn,
    filterMapDyn,
    mapSingleDyn,
    mapSingleMaybeDyn,
    readQueryEntitiesDyn,

    -- *** Internal
    runDynQuery,
    runDynQueryEntities,
    readDynQuery,
    readDynQueryEntities,

    -- * Dynamic query filters
    DynamicQueryFilter (..),

    -- * Reads and writes
    ReadsWrites (..),
    disjoint,
    readsWrites,
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.World.Archetype (Archetype)
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (ArchetypeID, Node (..))
import qualified Aztecs.ECS.World.Archetypes as AS
import Aztecs.ECS.World.Entities
import Control.Applicative
import Control.Monad.Identity
import Data.Bifunctor
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack
import Prelude hiding (reads)

data Operation f a where
  Entity :: Operation f EntityID
  Fetch :: (Component a) => !ComponentID -> Operation f a
  FetchMaybe :: (Component a) => !ComponentID -> Operation f (Maybe a)
  Adjust :: (Component a) => !(b -> a -> (c, a)) -> !ComponentID -> !(DynamicQueryT f b) -> Operation f (c, a)
  AdjustM :: (Monad f, Component a) => !(b -> a -> f (c, a)) -> !ComponentID -> !(DynamicQueryT f b) -> Operation f (c, a)

-- @since 0.9
type DynamicQuery = DynamicQueryT Identity

-- | Dynamic query for components by ID.
--
-- @since 0.11
data DynamicQueryT f a where
  Pure :: !a -> DynamicQueryT f a
  Map :: !(a -> b) -> !(DynamicQueryT f a) -> DynamicQueryT f b
  Ap :: !(DynamicQueryT f (a -> b)) -> !(DynamicQueryT f a) -> DynamicQueryT f b
  Op :: !(Operation f a) -> DynamicQueryT f a

instance Functor (DynamicQueryT f) where
  {-# INLINE fmap #-}
  fmap = Map

-- | @since 0.10
instance Applicative (DynamicQueryT f) where
  {-# INLINE pure #-}
  pure = Pure

  {-# INLINE (<*>) #-}
  (<*>) = Ap

{-# INLINE entityDyn #-}
entityDyn :: DynamicQueryT f EntityID
entityDyn = Op Entity

{-# INLINE fetchDyn #-}
fetchDyn :: (Component a) => ComponentID -> DynamicQueryT f a
fetchDyn = Op . Fetch

{-# INLINE fetchMaybeDyn #-}
fetchMaybeDyn :: (Component a) => ComponentID -> DynamicQueryT f (Maybe a)
fetchMaybeDyn = Op . FetchMaybe

{-# INLINE zipFetchMapDyn #-}
zipFetchMapDyn :: (Component a) => (b -> a -> a) -> ComponentID -> DynamicQueryT f b -> DynamicQueryT f a
zipFetchMapDyn f cId q = snd <$> Op (Adjust (\b a -> ((), f b a)) cId q)

{-# INLINE zipFetchMapAccumDyn #-}
zipFetchMapAccumDyn :: (Component a) => (b -> a -> (c, a)) -> ComponentID -> DynamicQueryT f b -> DynamicQueryT f (c, a)
zipFetchMapAccumDyn f cId q = Op $ Adjust f cId q

{-# INLINE zipFetchMapDynM #-}
zipFetchMapDynM :: (Monad f, Component a) => (b -> a -> f a) -> ComponentID -> DynamicQueryT f b -> DynamicQueryT f a
zipFetchMapDynM f cId q = snd <$> zipFetchMapAccumDynM (\b a -> ((),) <$> f b a) cId q

{-# INLINE zipFetchMapAccumDynM #-}
zipFetchMapAccumDynM :: (Monad f, Component a) => (b -> a -> f (c, a)) -> ComponentID -> DynamicQueryT f b -> DynamicQueryT f (c, a)
zipFetchMapAccumDynM f cId q = Op $ AdjustM f cId q

{-# INLINE opReadsWrites #-}
opReadsWrites :: Operation f a -> ReadsWrites
opReadsWrites Entity = mempty
opReadsWrites (Fetch cId) = mempty {reads = Set.singleton cId}
opReadsWrites (FetchMaybe cId) = mempty {reads = Set.singleton cId}
opReadsWrites (Adjust _ cId q) = readsWrites q <> mempty {writes = Set.singleton cId}
opReadsWrites (AdjustM _ cId q) = readsWrites q <> mempty {writes = Set.singleton cId}

{-# INLINE readsWrites #-}
readsWrites :: DynamicQueryT f a -> ReadsWrites
readsWrites (Pure _) = mempty
readsWrites (Map _ q) = readsWrites q
readsWrites (Ap f g) = readsWrites f <> readsWrites g
readsWrites (Op op) = opReadsWrites op

{-# INLINE runOp #-}
runOp :: (Applicative f) => Operation f a -> Archetype -> f ([a], Archetype)
runOp Entity arch = pure (Set.toList $ A.entities arch, mempty)
runOp (Fetch cId) arch = pure (A.lookupComponentsAsc cId arch, mempty)
runOp (FetchMaybe cId) arch =
  pure
    ( case A.lookupComponentsAscMaybe cId arch of
        Just as -> fmap Just as
        Nothing -> replicate (length $ A.entities arch) Nothing,
      mempty
    )
runOp (Adjust f cId q) arch = do
  res <- runDynQuery q arch
  return $
    let !(bs, arch') = res
        !(as, arch'') = A.zipWith bs f cId arch
     in (as, arch'' <> arch')
runOp (AdjustM f cId q) arch = do
  (as, arch') <- runDynQuery q arch
  (bs, arch'') <- A.zipWithM as f cId arch
  return (bs, arch'' <> arch')

{-# INLINE readOp #-}
readOp :: (Applicative f) => Operation f a -> Archetype -> f [a]
readOp Entity arch = pure $ Set.toList $ A.entities arch
readOp (Fetch cId) arch = pure $ A.lookupComponentsAsc cId arch
readOp (FetchMaybe cId) arch =
  pure $
    case A.lookupComponentsAscMaybe cId arch of
      Just as -> fmap Just as
      Nothing -> replicate (length $ A.entities arch) Nothing
readOp (Adjust f cId q) arch = do
  as <- readDynQuery q arch
  bs <- readOp (Fetch cId) arch
  return $ zipWith f as bs
readOp (AdjustM f cId q) arch = do
  as <- readDynQuery q arch
  bs <- readOp (Fetch cId) arch
  zipWithM f as bs

{-# INLINE queryEntitiesDyn #-}
queryEntitiesDyn ::
  (Monad m) =>
  [EntityID] ->
  DynamicQueryT m a ->
  Entities ->
  m ([a], Entities)
queryEntitiesDyn eIds q es =
  let ReadsWrites rs ws = readsWrites q
      cIds = rs <> ws
      go = runDynQueryEntities eIds q
   in if Set.null cIds
        then do
          (as, _) <- go A.empty {A.entities = Map.keysSet $ entities es}
          return (as, es)
        else
          let go' (acc, esAcc) (aId, n) = do
                (as', arch') <- go $ nodeArchetype n
                let n' = n {nodeArchetype = arch' <> nodeArchetype n}
                    !nodes = Map.insert aId n' . AS.nodes $ archetypes esAcc
                return (as' ++ acc, esAcc {archetypes = (archetypes esAcc) {AS.nodes = nodes}})
           in foldlM go' ([], es) $ Map.toList . AS.find cIds $ archetypes es

readQueryEntitiesDyn :: (Monad m) => [EntityID] -> DynamicQueryT m a -> Entities -> m [a]
readQueryEntitiesDyn eIds q es =
  let ReadsWrites rs ws = readsWrites q
      cIds = rs <> ws
   in if Set.null cIds
        then readDynQueryEntities eIds q A.empty {A.entities = Map.keysSet $ entities es}
        else
          let go n = readDynQuery q $ AS.nodeArchetype n
           in concat <$> mapM go (AS.find cIds $ archetypes es)

runOpEntities :: (Applicative f) => Operation f a -> [EntityID] -> Archetype -> f ([a], Archetype)
runOpEntities Entity es _ = pure (es, mempty)
runOpEntities (Fetch cId) es arch =
  pure
    ( map snd
        . filter (\(e, _) -> e `elem` es)
        . Map.toList
        $ A.lookupComponents cId arch,
      mempty
    )
runOpEntities (FetchMaybe cId) es arch =
  pure
    ( map (\(e, a) -> if e `elem` es then Just a else Nothing)
        . Map.toList
        $ A.lookupComponents cId arch,
      mempty
    )
runOpEntities (Adjust f cId q) es arch = do
  res <- runDynQuery q arch
  return $
    let go (e, b) a =
          if e `elem` es
            then let (x, y) = f b a in (Just x, y)
            else (Nothing, a)
        !(bs, arch') = res
        !(as, arch'') = A.zipWith (zip es bs) go cId arch
     in (mapMaybe (\(m, b) -> fmap (,b) m) as, arch'' <> arch')
runOpEntities (AdjustM f cId q) es arch = do
  (bs, arch') <- runDynQuery q arch
  let go (e, b) a =
        if e `elem` es
          then do
            (x, y) <- f b a
            return (Just x, y)
          else return (Nothing, a)
  (as, arch'') <- A.zipWithM (zip es bs) go cId arch
  return (mapMaybe (\(m, b) -> fmap (,b) m) as, arch'' <> arch')

runDynQueryEntities :: (Applicative f) => [EntityID] -> DynamicQueryT f a -> Archetype -> f ([a], Archetype)
runDynQueryEntities es (Pure a) _ = pure (replicate (length es) a, mempty)
runDynQueryEntities es (Map f q) arch = first (fmap f) <$> runDynQueryEntities es q arch
runDynQueryEntities es (Ap f g) arch = do
  res <- runDynQueryEntities es g arch
  res' <- runDynQueryEntities es f arch
  return $
    let (as, arch') = res
        (bs, arch'') = res'
     in (zipWith ($) bs as, arch'' <> arch')
runDynQueryEntities es (Op op) arch = runOpEntities op es arch

{-# INLINE readOpEntities #-}
readOpEntities :: (Applicative f) => Operation f a -> [EntityID] -> Archetype -> f [a]
readOpEntities Entity es _ = pure es
readOpEntities (Fetch cId) es arch =
  pure
    . map snd
    . filter (\(e, _) -> e `elem` es)
    . Map.toList
    $ A.lookupComponents cId arch
readOpEntities (FetchMaybe cId) es arch =
  pure
    . map (\(e, a) -> if e `elem` es then Just a else Nothing)
    . Map.toList
    $ A.lookupComponents cId arch
readOpEntities (Adjust f cId q) es arch = do
  a <- readDynQueryEntities es q arch
  b <- readOpEntities (Fetch cId) es arch
  pure $ zipWith f a b
readOpEntities (AdjustM f cId q) es arch = do
  a <- readDynQueryEntities es q arch
  b <- readOpEntities (Fetch cId) es arch
  zipWithM f a b

{-# INLINE readDynQueryEntities #-}
readDynQueryEntities :: (Applicative f) => [EntityID] -> DynamicQueryT f a -> Archetype -> f [a]
readDynQueryEntities es (Pure a) _ = pure $ replicate (length es) a
readDynQueryEntities es (Map f q) arch = fmap f <$> readDynQueryEntities es q arch
readDynQueryEntities es (Ap f g) arch = do
  a <- readDynQueryEntities es g arch
  b <- readDynQueryEntities es f arch
  pure $ b <*> a
readDynQueryEntities es (Op op) arch = readOpEntities op es arch

{-# INLINE runDynQuery #-}
runDynQuery :: (Applicative f) => DynamicQueryT f a -> Archetype -> f ([a], Archetype)
runDynQuery (Pure a) arch = pure (replicate (length $ A.entities arch) a, mempty)
runDynQuery (Map f q) arch = do
  res <- runDynQuery q arch
  return $ first (fmap f) res
runDynQuery (Ap f g) arch = do
  res <- runDynQuery g arch
  res' <- runDynQuery f arch
  return $
    let (as, arch') = res
        (bs, arch'') = res'
     in (zipWith ($) bs as, arch'' <> arch')
runDynQuery (Op op) arch = runOp op arch

{-# INLINE readDynQuery #-}
readDynQuery :: (Applicative f) => DynamicQueryT f a -> Archetype -> f [a]
readDynQuery (Pure a) arch = pure $ replicate (length $ A.entities arch) a
readDynQuery (Map f q) arch = fmap f <$> readDynQuery q arch
readDynQuery (Ap f g) arch = do
  as <- readDynQuery g arch
  bs <- readDynQuery f arch
  pure $ zipWith ($) bs as
readDynQuery (Op op) arch = readOp op arch

-- | Match all entities.
--
-- @since 0.10
allDyn :: (Monad m) => DynamicQueryT m a -> Entities -> m [a]
allDyn q es =
  let ReadsWrites rs ws = readsWrites q
      cIds = rs <> ws
   in if Set.null cIds
        then readDynQuery q A.empty {A.entities = Map.keysSet $ entities es}
        else
          let go n = readDynQuery q $ AS.nodeArchetype n
           in concat <$> mapM go (AS.find cIds $ archetypes es)

-- | Match all entities with a filter.
--
-- @since 0.10
filterDyn :: (Monad m) => (Node -> Bool) -> DynamicQueryT m a -> Entities -> m [a]
filterDyn f q es =
  let ReadsWrites rs ws = readsWrites q
      cIds = rs <> ws
   in if Set.null cIds
        then readDynQuery q $ A.empty {A.entities = Map.keysSet $ entities es}
        else
          let go n = readDynQuery q $ AS.nodeArchetype n
           in concat <$> mapM go (Map.filter f $ AS.find cIds $ archetypes es)

-- | Match a single entity.
--
-- @since 0.10
singleDyn :: (HasCallStack, Monad m) => DynamicQueryT m a -> Entities -> m a
singleDyn q es = do
  res <- singleMaybeDyn q es
  return $ case res of
    Just a -> a
    _ -> error "singleDyn: expected a single entity"

-- | Match a single entity, or `Nothing`.
--
-- @since 0.10
singleMaybeDyn :: (Monad m) => DynamicQueryT m a -> Entities -> m (Maybe a)
singleMaybeDyn q es =
  let ReadsWrites rs ws = readsWrites q
      cIds = rs <> ws
   in if Set.null cIds
        then case Map.keys $ entities es of
          [eId] -> do
            res <- readDynQuery q $ A.singleton eId
            return $ case res of
              [a] -> Just a
              _ -> Nothing
          _ -> return Nothing
        else case Map.elems $ AS.find cIds $ archetypes es of
          [n] -> do
            res <- readDynQuery q $ AS.nodeArchetype n
            return $ case res of
              [a] -> Just a
              _ -> Nothing
          _ -> return Nothing

-- | Map all matched entities.
--
-- @since 0.10
{-# INLINE mapDyn #-}
mapDyn :: (Monad m) => DynamicQueryT m a -> Entities -> m ([a], Entities)
mapDyn = mapDyn' id

-- | Map all matched entities with a filter.
--
-- @since 0.10
{-# INLINE filterMapDyn #-}
filterMapDyn ::
  (Monad m) =>
  (Node -> Bool) ->
  DynamicQueryT m a ->
  Entities ->
  m ([a], Entities)
filterMapDyn f = mapDyn' (Map.filter f)

{-# INLINE mapDyn' #-}
mapDyn' ::
  (Monad m) =>
  (Map ArchetypeID Node -> Map ArchetypeID Node) ->
  DynamicQueryT m a ->
  Entities ->
  m ([a], Entities)
mapDyn' f q es =
  let ReadsWrites rs ws = readsWrites q
      cIds = rs <> ws
      go = runDynQuery q
   in if Set.null cIds
        then do
          (as, _) <- go A.empty {A.entities = Map.keysSet $ entities es}
          return (as, es)
        else
          let go' (acc, esAcc) (aId, n) = do
                (as', arch') <- go $ nodeArchetype n
                let n' = n {nodeArchetype = arch' <> nodeArchetype n}
                    !nodes = Map.insert aId n' . AS.nodes $ archetypes esAcc
                return (as' ++ acc, esAcc {archetypes = (archetypes esAcc) {AS.nodes = nodes}})
           in foldlM go' ([], es) $ Map.toList . f . AS.find cIds $ archetypes es

-- | Map a single matched entity.
--
-- @since 0.10
mapSingleDyn :: (HasCallStack, Monad m) => DynamicQueryT m a -> Entities -> m (a, Entities)
mapSingleDyn q es = do
  (res, es') <- mapSingleMaybeDyn q es
  return $ case res of
    Just a -> (a, es')
    _ -> error "mapSingleDyn: expected single matching entity"

-- | Map a single matched entity, or @Nothing@.
--
-- @since 0.10
{-# INLINE mapSingleMaybeDyn #-}
mapSingleMaybeDyn :: (Monad m) => DynamicQueryT m a -> Entities -> m (Maybe a, Entities)
mapSingleMaybeDyn q es =
  let ReadsWrites rs ws = readsWrites q
      cIds = rs <> ws
   in if Set.null cIds
        then case Map.keys $ entities es of
          [eId] -> do
            res <- runDynQuery q $ A.singleton eId
            return $ case res of
              ([a], _) -> (Just a, es)
              _ -> (Nothing, es)
          _ -> pure (Nothing, es)
        else case Map.toList $ AS.find cIds $ archetypes es of
          [(aId, n)] -> do
            (as, arch') <- runDynQuery q $ AS.nodeArchetype n
            return $ case as of
              [a] ->
                let nodes = Map.insert aId n {nodeArchetype = arch' <> nodeArchetype n} . AS.nodes $ archetypes es
                 in (Just a, es {archetypes = (archetypes es) {AS.nodes = nodes}})
              _ -> (Nothing, es)
          _ -> pure (Nothing, es)

-- | Dynamic query for components by ID.
--
-- @since 0.9

-- | Dynamic query filter.
--
-- @since 0.9
data DynamicQueryFilter = DynamicQueryFilter
  { -- | `ComponentID`s to include.
    --
    -- @since 0.9
    filterWith :: !(Set ComponentID),
    -- | `ComponentID`s to exclude.
    --
    -- @since 0.9
    filterWithout :: !(Set ComponentID)
  }

-- | @since 0.9
instance Semigroup DynamicQueryFilter where
  DynamicQueryFilter withA withoutA <> DynamicQueryFilter withB withoutB =
    DynamicQueryFilter (withA <> withB) (withoutA <> withoutB)

-- | @since 0.9
instance Monoid DynamicQueryFilter where
  mempty = DynamicQueryFilter mempty mempty

-- | Reads and writes of a `Query`.
--
-- @since 0.9
data ReadsWrites = ReadsWrites
  { -- | Component IDs being read.
    --
    -- @since 0.9
    reads :: !(Set ComponentID),
    -- | Component IDs being written.
    --
    -- @since 0.9
    writes :: !(Set ComponentID)
  }
  deriving (Show)

-- | @since 0.9
instance Semigroup ReadsWrites where
  ReadsWrites r1 w1 <> ReadsWrites r2 w2 = ReadsWrites (r1 <> r2) (w1 <> w2)

-- | @since 0.9
instance Monoid ReadsWrites where
  mempty = ReadsWrites mempty mempty

-- | `True` if the reads and writes of two `Query`s overlap.
--
-- @since 0.9
disjoint :: ReadsWrites -> ReadsWrites -> Bool
disjoint a b =
  Set.disjoint (reads a) (writes b)
    || Set.disjoint (reads b) (writes a)
    || Set.disjoint (writes b) (writes a)
