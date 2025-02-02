{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aztecs.Query
  ( Query (..),
    (<?>),
    fetch,
    fetchId,
    fetchMaybe,
    all,
    queryAll,
    queryAll',
    map,
    Queryable (..),
    lookup,
    lookupQuery,
    alter,
    QueryState (..),
    IsEq,
    Map (..),
  )
where

import Data.Aztecs.Component
import Data.Aztecs.Entity (ConcatT, Difference, DifferenceT, Entity (..), EntityID, EntityT, FromEntity (..), Intersect, IntersectT, Sort, ToEntity (..))
import qualified Data.Aztecs.Entity as E
import Data.Aztecs.World (World (..))
import Data.Aztecs.World.Archetype (Archetype)
import qualified Data.Aztecs.World.Archetype as A
import Data.Aztecs.World.Archetypes (Archetypes, Node (nodeArchetype))
import qualified Data.Aztecs.World.Archetypes as AS
import Data.Aztecs.World.Components (Components)
import qualified Data.Aztecs.World.Components as CS
import Data.Data (Typeable)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (all, lookup, map)

data QueryState a = QueryState
  { queryStateComponentIds :: Set ComponentID,
    queryStateAll :: Archetype -> ([(EntityID, Entity a)], [Entity a] -> Archetype -> Archetype),
    queryStateLookup :: EntityID -> Archetype -> Maybe (Entity a),
    queryStateInsert :: EntityID -> Archetype -> Entity a -> Archetype
  }

-- | Query into the `World`.
newtype Query a = Query {runQuery' :: Components -> QueryState a}

(<?>) ::
  (E.Split a (ConcatT a b), E.SplitT a (ConcatT a b) ~ b) =>
  Query a ->
  Query b ->
  Query (ConcatT a b)
(Query a) <?> (Query b) = Query $ \cs ->
  let aQS = a cs
      bQS = b cs
   in QueryState
        { queryStateComponentIds = queryStateComponentIds aQS <> queryStateComponentIds bQS,
          queryStateAll = \arch ->
            let (as, aF) = queryStateAll aQS arch
                (bs, bF) = queryStateAll bQS arch
             in ( fmap (\((eId, aE), (_, bE)) -> (eId, E.concat aE bE)) (zip as bs),
                  \new newArch -> let (as', bs') = unzip $ fmap E.split new in bF bs' $ aF as' newArch
                ),
          queryStateLookup = \eId arch -> do
            aE <- queryStateLookup aQS eId arch
            bE <- queryStateLookup bQS eId arch
            return $ E.concat aE bE,
          queryStateInsert = \eId arch e ->
            let (aE, bE) = E.split e
                arch' = queryStateInsert aQS eId arch aE
             in queryStateInsert bQS eId arch' bE
        }

-- | Fetch a `Component` by its type.
fetch :: forall a. (Component a, Typeable (StorageT a)) => Query '[a]
fetch = Query $ \cs ->
  let cId = fromMaybe (error "TODO") (CS.lookup @a cs)
   in QueryState
        { queryStateComponentIds = Set.singleton cId,
          queryStateAll = \arch ->
            let as = A.all cId arch
             in ( fmap (\(eId, a) -> (eId, ECons a ENil)) as,
                  A.insertAscList cId . fmap (\((e, _), ECons a ENil) -> (e, a)) . zip as
                ),
          queryStateLookup = \eId arch -> do
            a <- A.lookupComponent eId cId arch
            return $ ECons a ENil,
          queryStateInsert = \eId arch e -> let (_, arch', _) = A.insert eId e arch cs in arch'
        }

-- | Fetch a `Component` by its `ComponentID`.
fetchId :: forall a. (Component a, Typeable (StorageT a)) => ComponentID -> Query '[a]
fetchId cId = fetch' @a (const cId)

fetch' :: forall a. (Component a, Typeable (StorageT a)) => (Components -> ComponentID) -> Query '[a]
fetch' f = Query $ \cs ->
  let cId = f cs
   in QueryState
        { queryStateComponentIds = Set.singleton cId,
          queryStateAll = \arch ->
            let as = A.all cId arch
             in ( fmap (\(eId, a) -> (eId, ECons a ENil)) as,
                  A.insertAscList cId . fmap (\((e, _), ECons a ENil) -> (e, a)) . zip as
                ),
          queryStateLookup = \eId arch -> do
            a <- A.lookupComponent eId cId arch
            return $ ECons a ENil,
          queryStateInsert = \eId arch e -> let (_, arch', _) = A.insert eId e arch cs in arch'
        }

fetchMaybe :: forall a. (Component a, Typeable (StorageT a)) => Query '[Maybe a]
fetchMaybe = fetchMaybe' @a (\cs -> (fromMaybe (error "TODO") (CS.lookup @a cs)))

fetchMaybe' :: forall a. (Component a, Typeable (StorageT a)) => (Components -> ComponentID) -> Query '[Maybe a]
fetchMaybe' f = Query $ \cs ->
  let cId = f cs
   in QueryState
        { queryStateComponentIds = Set.singleton cId,
          queryStateAll = \arch ->
            let as = A.allMaybe cId arch
             in ( fmap (\(eId, a) -> (eId, ECons a ENil)) as,
                  A.insertAscList cId
                    . fmap
                      ( \((e, a'), ECons a ENil) ->
                          (e, fromMaybe (fromMaybe (error "TODO") a') a)
                      )
                    . zip as
                ),
          queryStateLookup = \eId arch -> do
            a <- A.lookupComponent eId cId arch
            return $ ECons (Just a) ENil,
          queryStateInsert = \eId arch (ECons e ENil) ->
            let (cIds, arch', cs') = case e of
                  Just e' -> A.insert eId (ECons e' ENil) arch cs'
                  Nothing -> (cIds, arch, cs')
             in arch'
        }

all :: (ToEntity a, FromEntity a, Queryable (EntityT a)) => World -> [a]
all w = fmap snd (allWithId w)

allWithId :: forall a. (ToEntity a, FromEntity a, Queryable (EntityT a)) => World -> [(EntityID, a)]
allWithId = fmap (\(eId, e) -> (eId, fromEntity e)) . queryAll (query @(EntityT a))

queryAll :: (FromEntity a) => Query (EntityT a) -> World -> [(EntityID, a)]
queryAll q w = fromMaybe [] $ do
  let qS = runQuery' q (components w)
      es = concatMap (fst . queryStateAll qS) (AS.lookup (queryStateComponentIds qS) (archetypes w))
  return $ fmap (\(eId, e) -> (eId, fromEntity e)) es

queryAll' :: Query a -> Archetypes -> Components -> [(EntityID, Entity a)]
queryAll' q as cs = fromMaybe [] $ do
  let qS = runQuery' q cs
  return $ concatMap (fst . queryStateAll qS) (AS.lookup (queryStateComponentIds qS) as)

class QueryItem a where
  queryItem :: Query a

instance (Component c, Typeable (StorageT c)) => QueryItem '[c] where
  queryItem = fetch @c

instance  {-# OVERLAPPING #-} (Component c, Typeable (StorageT c)) => QueryItem '[Maybe c] where
  queryItem = fetchMaybe @c

class Queryable a where
  query :: Query a

instance {-# OVERLAPPING #-} (QueryItem '[a]) => Queryable '[a] where
  query = queryItem @'[a]

instance (QueryItem '[a], Queryable as) => Queryable (a ': as) where
  query = queryItem @'[a] <?> query @as

-- | Map over all entities that match this query,
-- storing the resulting components in the @World@.
map ::
  forall i o.
  (Map (IsEq (Entity (EntityT i)) (Entity (EntityT o))) i o) =>
  (i -> o) ->
  World ->
  ([o], World)
map f w =
  let (o, arches) = map' @(IsEq (Entity (EntityT i)) (Entity (EntityT o))) f (components w) AS.map (archetypes w)
   in (o, w {archetypes = arches})

-- Returns @True@ if @a@ and @b@ are the same type.
type family IsEq a b :: Bool where
  IsEq a a = 'True
  IsEq a b = 'False

-- Map over an entity's components.
--
-- @flag@ indicates if the input and output entities are the same,
-- in order to specialize at compile-time.
--
-- If the input and output entities are different, a reader and writer query are combined into one.
-- Otherwise, if the input and output entities are the same, we can use a more efficient implementation.
class Map (flag :: Bool) i o where
  map' ::
    (i -> o) ->
    Components ->
    (Set ComponentID -> (Archetype -> ([o], Archetype)) -> a -> ([[o]], a)) ->
    a ->
    ([o], a)
  mapM' ::
    (Monad m) =>
    (i -> m o) ->
    Components ->
    (Set ComponentID -> (Archetype -> m ([o], Archetype)) -> a -> m ([[o]], a)) ->
    a ->
    m ([o], a)
  mapSingleM' ::
    (Monad m) =>
    (i -> m o) ->
    Components ->
    (Set ComponentID -> (Archetype -> m ([o], Archetype)) -> a -> m ([[o]], a)) ->
    a ->
    m (Maybe o, a)

instance (FromEntity i, ToEntity o, EntityT i ~ a, EntityT o ~ a, Queryable a) => Map 'True i o where
  map' f cs mapArches arches =
    let qS = runQuery' (query @a) cs
        go arch =
          let (as, h) = queryStateAll qS arch
              as' = fmap (f . fromEntity) (fmap snd as)
              arch' = h (fmap toEntity as') arch
           in (as', arch')
        (es, arches') = mapArches (queryStateComponentIds qS) go arches
     in (concat es, arches')
  mapM' f cs mapArches arches = do
    let qS = runQuery' (query @a) cs
        go arch = do
          let (as, h) = queryStateAll qS arch
          as' <- mapM (f . fromEntity) (fmap snd as)
          let arch' = h (fmap toEntity as') arch
          return (as', arch')
    (es, arches') <- mapArches (queryStateComponentIds qS) go arches
    return (concat es, arches')
  mapSingleM' f cs mapArches arches = do
    let qS = runQuery' (query @a) cs
        go arch = do
          let (as, h) = queryStateAll qS arch
          case as of
            [(_, a)] -> do
              o <- f (fromEntity a)
              let arch' = h [toEntity o] arch
              return ([o], arch')
            _ -> return ([], arch)
    result <- mapArches (queryStateComponentIds qS) go arches
    case result of
      ([[o]], arches') -> return (Just o, arches')
      _ -> return (Nothing, arches)

instance
  ( FromEntity i,
    ToEntity o,
    Intersect (EntityT i) (EntityT o),
    Queryable (IntersectT (EntityT i) (EntityT o)),
    Difference (EntityT i) (EntityT o),
    Queryable (DifferenceT (EntityT i) (EntityT o)),
    Sort (ConcatT (DifferenceT (EntityT i) (EntityT o)) (IntersectT (EntityT i) (EntityT o))) (EntityT i),
    Sort (EntityT o) (IntersectT (EntityT i) (EntityT o))
  ) =>
  Map 'False i o
  where
  map' f cs mapArches arches =
    let i = query @(IntersectT (EntityT i) (EntityT o))
        o = query @(DifferenceT (EntityT i) (EntityT o))
        aQS = runQuery' i cs
        bQS = runQuery' o cs
        g arch =
          let (as, aH) = queryStateAll aQS arch
              (bs, _) = queryStateAll bQS arch
              es = fmap (\(aE, bE) -> f $ fromEntity @i (E.sort $ E.concat bE aE)) (zip (fmap snd as) (fmap snd bs))
              arch' = aH (fmap (\x -> let e = E.sort $ toEntity x in e) es) arch
           in (es, arch')
        (es', arches') = mapArches (queryStateComponentIds aQS <> queryStateComponentIds bQS) g arches
     in (concat es', arches')
  mapM' f cs mapArches arches = do
    let i = query @(IntersectT (EntityT i) (EntityT o))
        o = query @(DifferenceT (EntityT i) (EntityT o))
        aQS = runQuery' i cs
        bQS = runQuery' o cs
        g arch = do
          let (as, aH) = queryStateAll aQS arch
              (bs, _) = queryStateAll bQS arch
          es <- mapM (\(aE, bE) -> f $ fromEntity @i (E.sort $ E.concat bE aE)) (zip (fmap snd as) (fmap snd bs))
          let arch' = aH (fmap (\x -> let e = E.sort $ toEntity x in e) es) arch
          return (es, arch')
    (es', arches') <- mapArches (queryStateComponentIds aQS <> queryStateComponentIds bQS) g arches
    return (concat es', arches')
  mapSingleM' f cs mapArches arches = do
    let i = query @(IntersectT (EntityT i) (EntityT o))
        o = query @(DifferenceT (EntityT i) (EntityT o))
        aQS = runQuery' i cs
        bQS = runQuery' o cs
        g arch = do
          let (as, aH) = queryStateAll aQS arch
              (bs, _) = queryStateAll bQS arch
          es <- mapM (\(aE, bE) -> f $ fromEntity @i (E.sort $ E.concat bE aE)) (zip (fmap snd as) (fmap snd bs))
          let arch' = aH (fmap (\x -> let e = E.sort $ toEntity x in e) es) arch
          return (es, arch')
    result <- mapArches (queryStateComponentIds aQS <> queryStateComponentIds bQS) g arches
    case result of
      ([[o']], arches') -> return (Just o', arches')
      _ -> return (Nothing, arches)

lookup :: forall a. (FromEntity a, Queryable (EntityT a)) => EntityID -> World -> Maybe a
lookup eId w = fromEntity <$> lookupQuery eId (query @(EntityT a)) w

lookupQuery :: EntityID -> Query a -> World -> Maybe (Entity a)
lookupQuery eId q w = do
  let qS = runQuery' q (components w)
  aId <- Map.lookup eId (entities w)
  node <- AS.lookupNode aId (archetypes w)
  queryStateLookup qS eId (nodeArchetype node)

alter :: forall a. (FromEntity a, ToEntity a, Queryable (EntityT a)) => EntityID -> (a -> a) -> World -> World
alter eId f w = fromMaybe w $ do
  let qS = runQuery' (query @(EntityT a)) (components w)
  aId <- Map.lookup eId (entities w)
  node <- AS.lookupNode aId (archetypes w)
  e <- queryStateLookup qS eId (nodeArchetype node)
  let e' = f (fromEntity e)
      arch' = queryStateInsert qS eId (nodeArchetype node) (toEntity e')
  return $
    w
      { archetypes =
          (archetypes w) {AS.nodes = Map.insert aId node {nodeArchetype = arch'} (AS.nodes $ archetypes w)}
      }
