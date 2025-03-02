{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

module Aztecs.ECS.Query.Dynamic.Reader
  ( -- * Dynamic queries
    DynamicQueryReader,
    DynamicQueryReaderT (..),
    ArrowDynamicQueryReader (..),

    -- ** Running
    allDyn,
    singleDyn,
    singleMaybeDyn,
    runDynQueryReader,
    runDynQueryReaderT,

    -- * Dynamic query filters
    DynamicQueryFilter (..),
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.Query.Dynamic.Reader.Class
import Aztecs.ECS.World.Archetype (Archetype)
import qualified Aztecs.ECS.World.Archetype as A
import qualified Aztecs.ECS.World.Archetypes as AS
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Arrow
import Control.Category
import Control.Monad.Identity (Identity (runIdentity))
import Data.Either (partitionEithers)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)

type DynamicQueryReader = DynamicQueryReaderT Identity

-- | Dynamic query for components by ID.
newtype DynamicQueryReaderT m i o
  = DynamicQueryReader {runDynQueryReader' :: [i] -> [EntityID] -> Archetype -> m [o]}
  deriving (Functor)

instance (Monad m) => Applicative (DynamicQueryReaderT m i) where
  {-# INLINE pure #-}
  pure a = DynamicQueryReader $ \_ es _ -> pure $ replicate (length es) a
  {-# INLINE (<*>) #-}
  f <*> g =
    DynamicQueryReader $ \i es arch -> do
      !as <- runDynQueryReader' g i es arch
      !fs <- runDynQueryReader' f i es arch
      return $ zipWith ($) fs as

instance (Monad m) => Category (DynamicQueryReaderT m) where
  {-# INLINE id #-}
  id = DynamicQueryReader $ \as _ _ -> pure as
  {-# INLINE (.) #-}
  f . g = DynamicQueryReader $ \i es arch -> do
    !as <- runDynQueryReader' g i es arch
    runDynQueryReader' f as es arch

instance (Monad m) => Arrow (DynamicQueryReaderT m) where
  {-# INLINE arr #-}
  arr f = DynamicQueryReader $ \bs _ _ -> pure $ fmap f bs
  {-# INLINE first #-}
  first f = DynamicQueryReader $ \bds es arch -> do
    let !(bs, ds) = unzip bds
    !cs <- runDynQueryReader' f bs es arch
    return $ zip cs ds

instance (Monad m) => ArrowChoice (DynamicQueryReaderT m) where
  {-# INLINE left #-}
  left f = DynamicQueryReader $ \eds es arch -> do
    let !(es', ds) = partitionEithers eds
    !cs <- runDynQueryReader' f es' es arch
    return $ fmap Left cs ++ fmap Right ds

instance (Monad m) => ArrowDynamicQueryReader (DynamicQueryReaderT m) where
  {-# INLINE entity #-}
  entity = DynamicQueryReader $ \_ es _ -> pure es
  {-# INLINE fetchDyn #-}
  fetchDyn cId = DynamicQueryReader $ \_ _ arch -> pure $ A.lookupComponentsAsc cId arch
  {-# INLINE fetchMaybeDyn #-}
  fetchMaybeDyn cId = DynamicQueryReader $ \_ es arch -> pure $ case A.lookupComponentsAscMaybe cId arch of
    Just as -> fmap Just as
    Nothing -> map (const Nothing) es

data DynamicQueryFilter = DynamicQueryFilter
  { filterWith :: !(Set ComponentID),
    filterWithout :: !(Set ComponentID)
  }

instance Semigroup DynamicQueryFilter where
  DynamicQueryFilter withA withoutA <> DynamicQueryFilter withB withoutB =
    DynamicQueryFilter (withA <> withB) (withoutA <> withoutB)

instance Monoid DynamicQueryFilter where
  mempty = DynamicQueryFilter mempty mempty

{-# INLINE runDynQueryReaderT #-}
runDynQueryReaderT :: i -> DynamicQueryReaderT m i o -> [EntityID] -> Archetype -> m [o]
runDynQueryReaderT i q = runDynQueryReader' q (repeat i)

{-# INLINE runDynQueryReader #-}
runDynQueryReader :: i -> DynamicQueryReader i o -> [EntityID] -> Archetype -> [o]
runDynQueryReader i q es arch = runIdentity $ runDynQueryReaderT i q es arch

-- | Match all entities.
allDyn :: Set ComponentID -> i -> DynamicQueryReader i a -> Entities -> [a]
allDyn cIds i q es =
  if Set.null cIds
    then runDynQueryReader i q (Map.keys $ entities es) A.empty
    else
      let go n =
            let !eIds = Set.toList $ A.entities $ AS.nodeArchetype n
             in runDynQueryReader i q eIds (AS.nodeArchetype n)
       in concatMap go (AS.find cIds $ archetypes es)

singleDyn :: (HasCallStack) => Set ComponentID -> i -> DynamicQueryReader i a -> Entities -> a
singleDyn cIds i q es = case singleMaybeDyn cIds i q es of
  Just a -> a
  _ -> error "singleDyn: expected a single entity"

singleMaybeDyn :: Set ComponentID -> i -> DynamicQueryReader i a -> Entities -> Maybe a
singleMaybeDyn cIds i q es =
  if Set.null cIds
    then case Map.keys $ entities es of
      [eId] -> case runDynQueryReader i q [eId] A.empty of
        [a] -> Just a
        _ -> Nothing
      _ -> Nothing
    else case Map.elems $ AS.find cIds $ archetypes es of
      [n] ->
        let !eIds = Set.toList $ A.entities $ AS.nodeArchetype n
         in case runDynQueryReader i q eIds (AS.nodeArchetype n) of
              [a] -> Just a
              _ -> Nothing
      _ -> Nothing
