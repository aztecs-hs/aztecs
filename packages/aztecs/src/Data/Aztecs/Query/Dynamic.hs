{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aztecs.Query.Dynamic
  ( -- * Dynamic queries
    DynamicQuery (..),
    entityDyn,
    fetchDyn,
    fetchMaybeDyn,
    setDyn,
    -- * Dynamic query filters
    DynamicQueryFilter (..),
  )
where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Data.Aztecs.Component
import Data.Aztecs.Entity (EntityID)
import Data.Aztecs.Query.Dynamic.Class (ArrowDynamicQuery (..))
import Data.Aztecs.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader (..))
import Data.Aztecs.World.Archetype (Archetype)
import qualified Data.Aztecs.World.Archetype as A
import Data.Set (Set)
import Prelude hiding (all, any, id, lookup, map, mapM, reads, (.))

data DynamicQueryFilter = DynamicQueryFilter
  { filterWith :: !(Set ComponentID),
    filterWithout :: !(Set ComponentID)
  }

instance Semigroup DynamicQueryFilter where
  DynamicQueryFilter withA withoutA <> DynamicQueryFilter withB withoutB =
    DynamicQueryFilter (withA <> withB) (withoutA <> withoutB)

instance Monoid DynamicQueryFilter where
  mempty = DynamicQueryFilter mempty mempty

-- | Dynamic query for components by ID.
data DynamicQuery m i o = DynamicQuery
  { dynQueryAll :: !([i] -> [EntityID] -> Archetype -> m ([o], Archetype)),
    dynQueryLookup :: !(i -> EntityID -> Archetype -> m (Maybe o, Archetype))
  }

instance (Functor m) => Functor (DynamicQuery m i) where
  fmap f q =
    DynamicQuery
      { dynQueryAll =
          \i es arch -> fmap (\(a, arch') -> (fmap f a, arch')) $ dynQueryAll q i es arch,
        dynQueryLookup = \i eId arch -> fmap (first $ fmap f) $ dynQueryLookup q i eId arch
      }

instance (Monad m) => Applicative (DynamicQuery m i) where
  pure a =
    DynamicQuery
      { dynQueryAll = \_ es arch -> pure (take (length es) $ repeat a, arch),
        dynQueryLookup = \_ _ arch -> pure (Just a, arch)
      }
  f <*> g =
    DynamicQuery
      { dynQueryAll = \i es arch -> do
          (as, arch') <- dynQueryAll g i es arch
          (fs, arch'') <- dynQueryAll f i es arch'
          return (zipWith ($) fs as, arch''),
        dynQueryLookup = \i eId arch -> do
          (res, arch') <- dynQueryLookup g i eId arch
          case res of
            Just a -> do
              (res', arch'') <- dynQueryLookup f i eId arch'
              return (fmap ($) res' <*> Just a, arch'')
            Nothing -> pure (Nothing, arch')
      }

instance (Monad m) => Category (DynamicQuery m) where
  id =
    DynamicQuery
      { dynQueryAll = \as _ arch -> pure (as, arch),
        dynQueryLookup = \a _ arch -> pure (Just a, arch)
      }
  f . g =
    DynamicQuery
      { dynQueryAll = \i es arch -> do
          (as, arch') <- dynQueryAll g i es arch
          dynQueryAll f as es arch',
        dynQueryLookup = \i eId arch -> do
          (res, arch') <- dynQueryLookup g i eId arch
          case res of
            Just a -> dynQueryLookup f a eId arch'
            Nothing -> pure (Nothing, arch')
      }

instance (Monad m) => Arrow (DynamicQuery m) where
  arr f =
    DynamicQuery
      { dynQueryAll = \bs _ arch -> pure (fmap f bs, arch),
        dynQueryLookup = \b _ arch -> pure (Just (f b), arch)
      }
  first f =
    DynamicQuery
      { dynQueryAll = \bds es arch -> do
          let (bs, ds) = unzip bds
          (cs, arch') <- dynQueryAll f bs es arch
          return (zip cs ds, arch'),
        dynQueryLookup = \(b, d) eId arch -> do
          (res, arch') <- dynQueryLookup f b eId arch
          return
            ( case res of
                Just c -> Just (c, d)
                Nothing -> Nothing,
              arch'
            )
      }

instance (Monad m) => ArrowDynamicQueryReader (DynamicQuery m) where
  entityDyn =
    DynamicQuery
      { dynQueryAll = \_ es arch -> pure (es, arch),
        dynQueryLookup = \_ eId arch -> pure $ (Just eId, arch)
      }
  fetchDyn cId =
    DynamicQuery
      { dynQueryAll = \_ _ arch -> let !as = A.all cId arch in pure (fmap snd as, arch),
        dynQueryLookup = \_ eId arch -> pure $ (A.lookupComponent eId cId arch, arch)
      }
  fetchMaybeDyn cId =
    DynamicQuery
      { dynQueryAll = \_ _ arch -> let as = A.allMaybe cId arch in pure (fmap snd as, arch),
        dynQueryLookup = \_ eId arch -> pure $ (Just <$> A.lookupComponent eId cId arch, arch)
      }

instance (Monad m) => ArrowDynamicQuery (DynamicQuery m) where
  setDyn cId =
    DynamicQuery
      { dynQueryAll = \is _ arch -> let !arch' = A.withAscList cId is arch in pure (is, arch'),
        dynQueryLookup =
          \i eId arch -> pure (A.lookupComponent eId cId arch, A.insertComponent eId cId i arch)
      }
