{-# LANGUAGE BangPatterns #-}
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
newtype DynamicQuery m i o = DynamicQuery
  { dynQueryAll :: [i] -> [EntityID] -> Archetype -> m ([o], Archetype)
  }

instance (Functor m) => Functor (DynamicQuery m i) where
  fmap f q =
    DynamicQuery $ \i es arch -> fmap (\(a, arch') -> (fmap f a, arch')) $ dynQueryAll q i es arch

instance (Monad m) => Applicative (DynamicQuery m i) where
  pure a = DynamicQuery $ \_ es arch -> pure (take (length es) $ repeat a, arch)

  f <*> g =
    DynamicQuery
      { dynQueryAll = \i es arch -> do
          (as, arch') <- dynQueryAll g i es arch
          (fs, arch'') <- dynQueryAll f i es arch'
          return (zipWith ($) fs as, arch'')
      }

instance (Monad m) => Category (DynamicQuery m) where
  id = DynamicQuery $ \as _ arch -> pure (as, arch)

  f . g =
    DynamicQuery
      { dynQueryAll = \i es arch -> do
          (as, arch') <- dynQueryAll g i es arch
          dynQueryAll f as es arch'
      }

instance (Monad m) => Arrow (DynamicQuery m) where
  arr f = DynamicQuery $ \bs _ arch -> pure (fmap f bs, arch)
  first f =
    DynamicQuery
      { dynQueryAll = \bds es arch -> do
          let (bs, ds) = unzip bds
          (cs, arch') <- dynQueryAll f bs es arch
          return (zip cs ds, arch')
      }

instance (Monad m) => ArrowDynamicQueryReader (DynamicQuery m) where
  entityDyn = DynamicQuery $ \_ es arch -> pure (es, arch)

  fetchDyn cId =
    DynamicQuery $ \_ _ arch -> let !as = A.all cId arch in pure (fmap snd as, arch)

  fetchMaybeDyn cId =
    DynamicQuery $ \_ _ arch -> let as = A.allMaybe cId arch in pure (fmap snd as, arch)

instance (Monad m) => ArrowDynamicQuery (DynamicQuery m) where
  setDyn cId =
    DynamicQuery $ \is _ arch -> let !arch' = A.withAscList cId is arch in pure (is, arch')
