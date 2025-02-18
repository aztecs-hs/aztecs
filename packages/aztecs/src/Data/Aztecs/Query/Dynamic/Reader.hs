{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aztecs.Query.Dynamic.Reader
  ( -- * Dynamic queries
    DynamicQueryReader (..),
    DynamicQueryFilter (..),
  )
where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Data.Aztecs.Component
import Data.Aztecs.Entity (EntityID)
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
newtype DynamicQueryReader m i o
  = DynamicQueryReader {dynQueryReaderAll :: [i] -> [EntityID] -> Archetype -> m [o]}

instance (Functor m) => Functor (DynamicQueryReader m i) where
  fmap f q =
    DynamicQueryReader $ \i es arch -> fmap (fmap f) $ dynQueryReaderAll q i es arch

instance (Monad m) => Applicative (DynamicQueryReader m i) where
  pure a =
    DynamicQueryReader $ \_ es _ -> pure (take (length es) $ repeat a)

  f <*> g =
    DynamicQueryReader $ \i es arch -> do
      as <- dynQueryReaderAll g i es arch
      fs <- dynQueryReaderAll f i es arch
      return (zipWith ($) fs as)

instance (Monad m) => Category (DynamicQueryReader m) where
  id = DynamicQueryReader $ \as _ _ -> pure as
  f . g = DynamicQueryReader $ \i es arch -> do
    as <- dynQueryReaderAll g i es arch
    dynQueryReaderAll f as es arch

instance (Monad m) => Arrow (DynamicQueryReader m) where
  arr f = DynamicQueryReader $ \bs _ _ -> pure $ fmap f bs
  first f = DynamicQueryReader $ \bds es arch -> do
    let (bs, ds) = unzip bds
    cs <- dynQueryReaderAll f bs es arch
    return $ zip cs ds

instance (Monad m) => ArrowDynamicQueryReader (DynamicQueryReader m) where
  entityDyn = DynamicQueryReader $ \_ es _ -> pure es
  fetchDyn cId =
    DynamicQueryReader $ \_ _ arch -> let !as = A.all cId arch in pure $ fmap snd as
  fetchMaybeDyn cId =
    DynamicQueryReader $ \_ _ arch -> let as = A.allMaybe cId arch in pure $ fmap snd as
