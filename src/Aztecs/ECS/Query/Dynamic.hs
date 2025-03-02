{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.ECS.Query.Dynamic
  ( -- * Dynamic queries
    DynamicQuery,
    DynamicQueryT (..),
    ArrowDynamicQueryReader (..),
    ArrowDynamicQuery (..),

    -- ** Conversion
    fromDynReader,
    toDynReader,

    -- ** Running
    mapDyn,
    mapSingleDyn,
    mapSingleMaybeDyn,

    -- * Dynamic query filters
    DynamicQueryFilter (..),
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.Query.Dynamic.Class
import Aztecs.ECS.Query.Dynamic.Reader
import Aztecs.ECS.World.Archetype (Archetype)
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node (..))
import qualified Aztecs.ECS.World.Archetypes as AS
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Arrow
import Control.Category
import Control.Monad.Identity
import Data.Either (partitionEithers)
import Data.Foldable
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Prelude hiding ((.))

type DynamicQuery = DynamicQueryT Identity

-- | Dynamic query for components by ID.
newtype DynamicQueryT m i o
  = DynamicQuery {runDynQuery :: [i] -> [EntityID] -> Archetype -> m ([o], Archetype)}
  deriving (Functor)

instance (Monad m) => Applicative (DynamicQueryT m i) where
  {-# INLINE pure #-}
  pure a = DynamicQuery $ \_ es arch -> pure (replicate (length es) a, arch)
  {-# INLINE (<*>) #-}
  f <*> g = DynamicQuery $ \i es arch -> do
    (as, arch') <- runDynQuery g i es arch
    (fs, arch'') <- runDynQuery f i es arch'
    return (zipWith ($) fs as, arch'')

instance (Monad m) => Category (DynamicQueryT m) where
  {-# INLINE id #-}
  id = DynamicQuery $ \as _ arch -> pure (as, arch)
  {-# INLINE (.) #-}
  f . g = DynamicQuery $ \i es arch -> do
    (as, arch') <- runDynQuery g i es arch
    runDynQuery f as es arch'

instance (Monad m) => Arrow (DynamicQueryT m) where
  {-# INLINE arr #-}
  arr f = DynamicQuery $ \bs _ arch -> pure (fmap f bs, arch)
  {-# INLINE first #-}
  first f = DynamicQuery $ \bds es arch -> do
    let !(bs, ds) = unzip bds
    (cs, arch') <- runDynQuery f bs es arch
    return (zip cs ds, arch')

instance (Monad m) => ArrowChoice (DynamicQueryT m) where
  {-# INLINE left #-}
  left f = DynamicQuery $ \eds es arch -> do
    let !(es', ds) = partitionEithers eds
    (cs, arch') <- runDynQuery f es' es arch
    return (fmap Left cs ++ fmap Right ds, arch')

instance (Monad m) => ArrowDynamicQueryReader (DynamicQueryT m) where
  {-# INLINE entity #-}
  entity = fromDynReader entity
  {-# INLINE fetchDyn #-}
  fetchDyn = fromDynReader . fetchDyn
  {-# INLINE fetchMaybeDyn #-}
  fetchMaybeDyn = fromDynReader . fetchMaybeDyn

instance (Monad m) => ArrowDynamicQuery m (DynamicQueryT m) where
  {-# INLINE adjustDyn #-}
  adjustDyn f cId = DynamicQuery $ \is _ arch -> pure $ A.zipWith is f cId arch

  {-# INLINE adjustDyn_ #-}
  adjustDyn_ f cId = DynamicQuery $ \is _ arch -> pure (repeat (), A.zipWith_ is f cId arch)

  {-# INLINE adjustDynM #-}
  adjustDynM f cId = DynamicQuery $ \is _ arch -> A.zipWithM is f cId arch

  {-# INLINE setDyn #-}
  setDyn cId = DynamicQuery $ \is _ arch -> pure (is, A.insertAscList cId is arch)

{-# INLINE fromDynReader #-}
fromDynReader :: (Monad m) => DynamicQueryReaderT m i o -> DynamicQueryT m i o
fromDynReader q = DynamicQuery $ \is es arch -> do
  !os <- runDynQueryReader' q is es arch
  return (os, arch)

{-# INLINE toDynReader #-}
toDynReader :: (Functor m) => DynamicQueryT m i o -> DynamicQueryReaderT m i o
toDynReader q = DynamicQueryReader $ \is es arch -> fst <$> runDynQuery q is es arch

-- | Map all matched entities.
{-# INLINE mapDyn #-}
mapDyn :: (Monad m) => Set ComponentID -> i -> DynamicQueryT m i a -> Entities -> m ([a], Entities)
mapDyn cIds i q es =
  let go = runDynQuery q (repeat i)
   in if Set.null cIds
        then do
          (as, _) <- go (Map.keys $ entities es) A.empty
          return (as, es)
        else
          let go' (acc, esAcc) (aId, n) = do
                (as', arch') <- go (Set.toList . A.entities $ nodeArchetype n) $ nodeArchetype n
                let !nodes = Map.insert aId n {nodeArchetype = arch'} . AS.nodes $ archetypes esAcc
                return (as' ++ acc, esAcc {archetypes = (archetypes esAcc) {AS.nodes = nodes}})
           in foldlM go' ([], es) $ Map.toList . AS.find cIds $ archetypes es

mapSingleDyn :: (HasCallStack, Monad m) => Set ComponentID -> i -> DynamicQueryT m i a -> Entities -> m (a, Entities)
mapSingleDyn cIds i q es = do
  res <- mapDyn cIds i q es
  return $ case res of
    ([a], es') -> (a, es')
    _ -> error "mapSingleDyn: expected single matching entity"

-- | Map a single matched entity.
{-# INLINE mapSingleMaybeDyn #-}
mapSingleMaybeDyn :: (Monad m) => Set ComponentID -> i -> DynamicQueryT m i a -> Entities -> m (Maybe a, Entities)
mapSingleMaybeDyn cIds i q es =
  if Set.null cIds
    then case Map.keys $ entities es of
      [eId] -> do
        res <- runDynQuery q [i] [eId] A.empty
        return $ case res of
          ([a], _) -> (Just a, es)
          _ -> (Nothing, es)
      _ -> pure (Nothing, es)
    else case Map.toList $ AS.find cIds $ archetypes es of
      [(aId, n)] -> do
        let !eIds = Set.toList $ A.entities $ AS.nodeArchetype n
        res <- runDynQuery q [i] eIds (AS.nodeArchetype n)
        return $ case res of
          ([a], arch') ->
            let nodes = Map.insert aId n {nodeArchetype = arch'} . AS.nodes $ archetypes es
             in (Just a, es {archetypes = (archetypes es) {AS.nodes = nodes}})
          _ -> (Nothing, es)
      _ -> pure (Nothing, es)
