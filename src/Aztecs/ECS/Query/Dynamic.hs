{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

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
    fetchMapDyn,
    fetchMapDynM,
    zipFetchMapDyn,
    zipFetchMapAccumDyn,
    zipFetchMapDynM,
    zipFetchMapAccumDynM,

    -- ** Filters
    withDyn,
    withoutDyn,

    -- ** Conversion
    liftQueryDyn,

    -- ** Running
    queryDyn,
    readQuerySingleDyn,
    readQuerySingleMaybeDyn,
    queryEntitiesDyn,
    readQueryDyn,
    querySingleDyn,
    querySingleMaybeDyn,
    readQueryEntitiesDyn,

    -- *** Internal
    QueryFilter (..),
    Operation (..),
    queryFilter,
    runDynQuery,
    runDynQueryEntities,
    readDynQuery,
    readDynQueryEntities,
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.World.Archetype (Archetype)
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node (..))
import qualified Aztecs.ECS.World.Archetypes as AS
import Aztecs.ECS.World.Entities
import Control.Applicative
import Control.Monad
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Identity
import Data.Bifunctor
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack
import Prelude hiding (reads)

-- @since 0.9
type DynamicQuery = DynamicQueryT Identity

-- | Dynamic query for components by ID.
--
-- @since 0.11
data DynamicQueryT f a where
  Entity :: DynamicQueryT f EntityID
  Pure :: a -> DynamicQueryT f a
  Map :: (a -> b) -> DynamicQueryT f a -> DynamicQueryT f b
  Ap :: DynamicQueryT f (a -> b) -> DynamicQueryT f a -> DynamicQueryT f b
  Lift :: (MonadTrans g, Monad (g f), Monad f) => DynamicQueryT f a -> DynamicQueryT (g f) a
  Op :: ComponentID -> Operation f a -> DynamicQueryT f a

instance Functor (DynamicQueryT f) where
  {-# INLINE fmap #-}
  fmap = Map

-- | @since 0.11
instance Applicative (DynamicQueryT f) where
  {-# INLINE pure #-}
  pure = Pure

  {-# INLINE (<*>) #-}
  (<*>) = Ap

{-# INLINE entityDyn #-}
entityDyn :: DynamicQueryT f EntityID
entityDyn = Entity

{-# INLINE fetchDyn #-}
fetchDyn :: (Component a) => ComponentID -> DynamicQueryT f a
fetchDyn cId = Op cId Fetch

{-# INLINE fetchMaybeDyn #-}
fetchMaybeDyn :: (Component a) => ComponentID -> DynamicQueryT f (Maybe a)
fetchMaybeDyn cId = Op cId FetchMaybe

{-# INLINE fetchMapDyn #-}
fetchMapDyn :: (Component a) => (a -> a) -> ComponentID -> DynamicQueryT f a
fetchMapDyn f cId = Op cId $ FetchMap f

{-# INLINE fetchMapDynM #-}
fetchMapDynM :: (Monad f, Component a) => (a -> f a) -> ComponentID -> DynamicQueryT f a
fetchMapDynM f cId = Op cId $ FetchMapM f

{-# INLINE zipFetchMapDyn #-}
zipFetchMapDyn ::
  (Component a) => (b -> a -> a) -> ComponentID -> DynamicQueryT f b -> DynamicQueryT f a
zipFetchMapDyn f cId q = snd <$> Op cId (ZipFetchMap (\b a -> ((), f b a)) q)

{-# INLINE zipFetchMapAccumDyn #-}
zipFetchMapAccumDyn ::
  (Component a) => (b -> a -> (c, a)) -> ComponentID -> DynamicQueryT f b -> DynamicQueryT f (c, a)
zipFetchMapAccumDyn f cId q = Op cId $ ZipFetchMap f q

{-# INLINE zipFetchMapDynM #-}
zipFetchMapDynM ::
  (Monad f, Component a) =>
  (b -> a -> f a) ->
  ComponentID ->
  DynamicQueryT f b ->
  DynamicQueryT f a
zipFetchMapDynM f cId q = snd <$> zipFetchMapAccumDynM (\b a -> ((),) <$> f b a) cId q

{-# INLINE zipFetchMapAccumDynM #-}
zipFetchMapAccumDynM ::
  (Monad f, Component a) =>
  (b -> a -> f (c, a)) ->
  ComponentID ->
  DynamicQueryT f b ->
  DynamicQueryT f (c, a)
zipFetchMapAccumDynM f cId q = Op cId $ ZipFetchMapM f q

{-# INLINE withDyn #-}
withDyn :: ComponentID -> DynamicQueryT f ()
withDyn cId = Op cId With

{-# INLINE withoutDyn #-}
withoutDyn :: ComponentID -> DynamicQueryT f ()
withoutDyn cId = Op cId Without

{-# INLINE liftQueryDyn #-}
liftQueryDyn :: (MonadTrans g, Monad (g f), Monad f) => DynamicQueryT f a -> DynamicQueryT (g f) a
liftQueryDyn = Lift

-- | Match all entities.
--
-- @since 0.11
readQueryDyn :: (Applicative f) => DynamicQueryT f a -> Entities -> f [a]
readQueryDyn q es =
  let qf = queryFilter q
   in if Set.null $ filterWith qf
        then readDynQuery q $ A.empty {A.entities = Map.keysSet $ entities es}
        else
          let go n = readDynQuery q $ AS.nodeArchetype n
           in concat <$> traverse go (AS.find (filterWith qf) (filterWithout qf) $ archetypes es)

-- | Match a single entity.
--
-- @since 0.11
readQuerySingleDyn :: (HasCallStack, Applicative f) => DynamicQueryT f a -> Entities -> f a
readQuerySingleDyn q es = do
  res <- readQuerySingleMaybeDyn q es
  return $ case res of
    Just a -> a
    _ -> error "singleDyn: expected a single entity"

-- | Match a single entity, or `Nothing`.
--
-- @since 0.11
readQuerySingleMaybeDyn :: (Applicative f) => DynamicQueryT f a -> Entities -> f (Maybe a)
readQuerySingleMaybeDyn q es =
  let qf = queryFilter q
   in if Set.null $ filterWith qf
        then case Map.keys $ entities es of
          [eId] -> do
            res <- readDynQuery q $ A.singleton eId
            return $ case res of
              [a] -> Just a
              _ -> Nothing
          _ -> pure Nothing
        else case Map.elems $ AS.find (filterWith qf) (filterWithout qf) $ archetypes es of
          [n] -> do
            res <- readDynQuery q $ AS.nodeArchetype n
            return $ case res of
              [a] -> Just a
              _ -> Nothing
          _ -> pure Nothing

readQueryEntitiesDyn :: (Applicative f) => [EntityID] -> DynamicQueryT f a -> Entities -> f [a]
readQueryEntitiesDyn eIds q es =
  let qf = queryFilter q
   in if Set.null $ filterWith qf
        then readDynQueryEntities eIds q A.empty {A.entities = Map.keysSet $ entities es}
        else
          let go n = readDynQuery q $ AS.nodeArchetype n
           in concat <$> traverse go (AS.find (filterWith qf) (filterWithout qf) $ archetypes es)

-- | Match and update all matched entities.
--
-- @since 0.11
{-# INLINE queryDyn #-}
queryDyn :: (Applicative f) => DynamicQueryT f a -> Entities -> f ([a], Entities)
queryDyn q es =
  let qf = queryFilter q
   in if Set.null $ filterWith qf
        then (,es) . fst <$> runDynQuery q A.empty {A.entities = Map.keysSet $ entities es}
        else
          let go (aId, n) = do
                res <- runDynQuery q $ nodeArchetype n
                return $
                  let (as', arch') = res
                   in (as', aId, n {nodeArchetype = arch' <> nodeArchetype n})
              matches = Map.toList . AS.find (filterWith qf) (filterWithout qf) $ archetypes es
              res' = traverse go matches
              folder (acc, esAcc) (as, aId, node) =
                let nodes = Map.insert aId node . AS.nodes $ archetypes esAcc
                 in (as ++ acc, esAcc {archetypes = (archetypes esAcc) {AS.nodes = nodes}})
           in fmap (foldl' folder ([], es)) res'

-- | Match and update a single entity.
--
-- @since 0.11
querySingleDyn :: (HasCallStack, Applicative m) => DynamicQueryT m a -> Entities -> m (a, Entities)
querySingleDyn q es = do
  res <- querySingleMaybeDyn q es
  return $ case res of
    (Just a, es') -> (a, es')
    _ -> error "mapSingleDyn: expected single matching entity"

-- | Match and update a single entity, or @Nothing@.
--
-- @since 0.11
{-# INLINE querySingleMaybeDyn #-}
querySingleMaybeDyn :: (Applicative f) => DynamicQueryT f a -> Entities -> f (Maybe a, Entities)
querySingleMaybeDyn q es =
  let qf = queryFilter q
   in if Set.null $ filterWith qf
        then case Map.keys $ entities es of
          [eId] -> do
            res <- runDynQuery q $ A.singleton eId
            return $ case res of
              ([a], _) -> (Just a, es)
              _ -> (Nothing, es)
          _ -> pure (Nothing, es)
        else case Map.toList $ AS.find (filterWith qf) (filterWithout qf) $ archetypes es of
          [(aId, n)] -> do
            res <- runDynQuery q $ AS.nodeArchetype n
            return $ case res of
              ([a], arch') ->
                let nodes = Map.insert aId n {nodeArchetype = arch' <> nodeArchetype n} . AS.nodes $ archetypes es
                 in (Just a, es {archetypes = (archetypes es) {AS.nodes = nodes}})
              _ -> (Nothing, es)
          _ -> pure (Nothing, es)

{-# INLINE queryEntitiesDyn #-}
queryEntitiesDyn ::
  (Monad m) =>
  [EntityID] ->
  DynamicQueryT m a ->
  Entities ->
  m ([a], Entities)
queryEntitiesDyn eIds q es =
  let qf = queryFilter q
      go = runDynQueryEntities eIds q
   in if Set.null $ filterWith qf
        then do
          (as, _) <- go A.empty {A.entities = Map.keysSet $ entities es}
          return (as, es)
        else
          let go' (acc, esAcc) (aId, n) = do
                (as', arch') <- go $ nodeArchetype n
                let n' = n {nodeArchetype = arch' <> nodeArchetype n}
                    nodes = Map.insert aId n' . AS.nodes $ archetypes esAcc
                return (as' ++ acc, esAcc {archetypes = (archetypes esAcc) {AS.nodes = nodes}})
           in foldlM go' ([], es) $ Map.toList . AS.find (filterWith qf) (filterWithout qf) $ archetypes es

{-# INLINE queryFilter #-}
queryFilter :: DynamicQueryT f a -> QueryFilter
queryFilter (Pure _) = mempty
queryFilter (Map _ q) = queryFilter q
queryFilter (Ap f g) = queryFilter f <> queryFilter g
queryFilter (Lift q) = queryFilter q
queryFilter Entity = mempty
queryFilter (Op cId op) = opFilter cId op

{-# INLINE readDynQuery #-}
readDynQuery :: (Applicative f) => DynamicQueryT f a -> Archetype -> f [a]
readDynQuery (Pure a) arch = pure $ replicate (length $ A.entities arch) a
readDynQuery (Map f q) arch = fmap f <$> readDynQuery q arch
readDynQuery (Ap f g) arch = do
  as <- readDynQuery g arch
  bs <- readDynQuery f arch
  pure $ zipWith ($) bs as
readDynQuery (Lift q) arch = lift $ readDynQuery q arch
readDynQuery Entity arch = pure $ Set.toList $ A.entities arch
readDynQuery (Op cId op) arch = readOp cId op arch

{-# INLINE readDynQueryEntities #-}
readDynQueryEntities :: (Applicative f) => [EntityID] -> DynamicQueryT f a -> Archetype -> f [a]
readDynQueryEntities es (Pure a) _ = pure $ replicate (length es) a
readDynQueryEntities es (Map f q) arch = fmap f <$> readDynQueryEntities es q arch
readDynQueryEntities es (Ap f g) arch = do
  a <- readDynQueryEntities es g arch
  b <- readDynQueryEntities es f arch
  pure $ b <*> a
readDynQueryEntities es (Lift q) arch = lift $ readDynQueryEntities es q arch
readDynQueryEntities es Entity _ = pure es
readDynQueryEntities es (Op cId op) arch = readOpEntities cId es op arch

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
runDynQuery (Lift q) arch = lift $ runDynQuery q arch
runDynQuery Entity arch = (,arch) <$> readDynQuery Entity arch
runDynQuery (Op cId op) arch = runOp cId op arch

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
runDynQueryEntities es (Lift q) arch = lift $ runDynQueryEntities es q arch
runDynQueryEntities es Entity _ = pure (es, mempty)
runDynQueryEntities es (Op cId op) arch = runOpEntities cId es op arch

data Operation f a where
  Fetch :: (Component a) => Operation f a
  FetchMaybe :: (Component a) => Operation f (Maybe a)
  FetchMap :: (Component a) => (a -> a) -> Operation f a
  FetchMapM :: (Monad f, Component a) => (a -> f a) -> Operation f a
  ZipFetchMap :: (Component a) => (b -> a -> (c, a)) -> (DynamicQueryT f b) -> Operation f (c, a)
  ZipFetchMapM :: (Monad f, Component a) => (b -> a -> f (c, a)) -> (DynamicQueryT f b) -> Operation f (c, a)
  With :: Operation f ()
  Without :: Operation f ()

{-# INLINE opFilter #-}
opFilter :: ComponentID -> Operation f a -> QueryFilter
opFilter cId Fetch = mempty {filterWith = Set.singleton cId}
opFilter cId FetchMaybe = mempty {filterWith = Set.singleton cId}
opFilter cId (FetchMap _) = mempty {filterWith = Set.singleton cId}
opFilter cId (FetchMapM _) = mempty {filterWith = Set.singleton cId}
opFilter cId (ZipFetchMap _ q) = queryFilter q <> mempty {filterWith = Set.singleton cId}
opFilter cId (ZipFetchMapM _ q) = queryFilter q <> mempty {filterWith = Set.singleton cId}
opFilter cId With = mempty {filterWith = Set.singleton cId}
opFilter cId Without = mempty {filterWithout = Set.singleton cId}

{-# INLINE readOp #-}
readOp :: (Applicative f) => ComponentID -> Operation f a -> Archetype -> f [a]
readOp cId Fetch arch = pure $ A.lookupComponentsAsc cId arch
readOp cId FetchMaybe arch =
  pure $
    case A.lookupComponentsAscMaybe cId arch of
      Just as -> fmap Just as
      Nothing -> replicate (length $ A.entities arch) Nothing
readOp cId (FetchMap f) arch = do
  bs <- readOp cId Fetch arch
  return $ map f bs
readOp cId (FetchMapM f) arch = do
  bs <- readOp cId Fetch arch
  mapM f bs
readOp cId (ZipFetchMap f q) arch = do
  as <- readDynQuery q arch
  bs <- readOp cId Fetch arch
  return $ zipWith f as bs
readOp cId (ZipFetchMapM f q) arch = do
  as <- readDynQuery q arch
  bs <- readOp cId Fetch arch
  zipWithM f as bs
readOp _ With _ = pure []
readOp _ Without _ = pure []

{-# INLINE runOp #-}
runOp :: (Applicative f) => ComponentID -> Operation f a -> Archetype -> f ([a], Archetype)
runOp cId (FetchMap f) arch = pure $ A.map f cId arch
runOp cId (FetchMapM f) arch = do
  (as, arch') <- A.mapM f cId arch
  return (as, arch')
runOp cId (ZipFetchMap f q) arch = do
  res <- runDynQuery q arch
  return $
    let (bs, arch') = res
        (as, arch'') = A.zipMap bs f cId arch
     in (as, arch'' <> arch')
runOp cId (ZipFetchMapM f q) arch = do
  (as, arch') <- runDynQuery q arch
  (bs, arch'') <- A.zipMapM as f cId arch
  return (bs, arch'' <> arch')
runOp cId op arch = (,mempty) <$> readOp cId op arch

{-# INLINE readOpEntities #-}
readOpEntities :: (Applicative f) => ComponentID -> [EntityID] -> Operation f a -> Archetype -> f [a]
readOpEntities cId es Fetch arch =
  pure
    . map snd
    . filter (\(e, _) -> e `elem` es)
    . Map.toList
    $ A.lookupComponents cId arch
readOpEntities cId es FetchMaybe arch =
  pure
    . map (\(e, a) -> if e `elem` es then Just a else Nothing)
    . Map.toList
    $ A.lookupComponents cId arch
readOpEntities cId es (FetchMap f) arch = do
  b <- readOpEntities cId es Fetch arch
  pure $ map f b
readOpEntities cId es (FetchMapM f) arch = do
  b <- readOpEntities cId es Fetch arch
  mapM f b
readOpEntities cId es (ZipFetchMap f q) arch = do
  a <- readDynQueryEntities es q arch
  b <- readOpEntities cId es Fetch arch
  pure $ zipWith f a b
readOpEntities cId es (ZipFetchMapM f q) arch = do
  a <- readDynQueryEntities es q arch
  b <- readOpEntities cId es Fetch arch
  zipWithM f a b
readOpEntities _ _ With _ = pure []
readOpEntities _ _ Without _ = pure []

runOpEntities :: (Applicative f) => ComponentID -> [EntityID] -> Operation f a -> Archetype -> f ([a], Archetype)
runOpEntities cId es (FetchMap f) arch =
  pure $
    let go e a =
          if e `elem` es
            then let a' = f a in (Just a', a')
            else (Nothing, a)
        (as, arch') = A.zipMap es go cId arch
     in (mapMaybe fst as, arch')
runOpEntities cId es (FetchMapM f) arch = do
  (as, arch') <- runOpEntities cId es (ZipFetchMapM (\() a -> (,a) <$> f a) (pure ())) arch
  return (map snd as, arch')
runOpEntities cId es (ZipFetchMap f q) arch = do
  res <- runDynQuery q arch
  return $
    let go (e, b) a =
          if e `elem` es
            then let (x, y) = f b a in (Just x, y)
            else (Nothing, a)
        (bs, arch') = res
        (as, arch'') = A.zipMap (zip es bs) go cId arch
     in (mapMaybe (\(m, b) -> fmap (,b) m) as, arch'' <> arch')
runOpEntities cId es (ZipFetchMapM f q) arch = do
  (bs, arch') <- runDynQuery q arch
  let go (e, b) a =
        if e `elem` es
          then do
            (x, y) <- f b a
            return (Just x, y)
          else return (Nothing, a)
  (as, arch'') <- A.zipMapM (zip es bs) go cId arch
  return (mapMaybe (\(m, b) -> fmap (,b) m) as, arch'' <> arch')
runOpEntities cId es op arch = (,arch) <$> readOpEntities cId es op arch

-- | `Query` filter.
--
-- @since 0.11
data QueryFilter = QueryFilter
  { filterWith :: !(Set ComponentID),
    filterWithout :: !(Set ComponentID)
  }
  deriving (Show)

-- | @since 0.9
instance Semigroup QueryFilter where
  QueryFilter r1 w1 <> QueryFilter r2 w2 = QueryFilter (r1 <> r2) (w1 <> w2)

-- | @since 0.9
instance Monoid QueryFilter where
  mempty = QueryFilter mempty mempty
